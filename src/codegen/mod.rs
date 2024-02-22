
pub mod stencils;

use lazy_static::lazy_static;

use std::{cell::RefCell, collections::BTreeMap};
use libc::{c_void, stack_t};
use stencils::Stencil;

use crate::expr::{self, Atom, BuiltIn, Expr};

use self::stencils::compile_all_stencils;

// TODO: Once we go beyond basic arithmetic expressions we should have our own IR

pub struct GeneratedCode {
    stack: *mut u8,
    code: *const c_void,
    ghcc_code: *const c_void,
}

impl GeneratedCode {

    pub fn new(stack_size: usize, wrapper_stencil: &Stencil, code: &[u8]) -> Self {

        // mmap a memory region with read and execute permissions
        let mmap = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                code.len(),
                libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            )
        };

        let mut ghcc_code = wrapper_stencil.code.clone();

        let holes_values = vec![(mmap as u64).to_ne_bytes()];
        for (&ofs, val) in wrapper_stencil.holes.iter().zip(holes_values.iter()) {
            ghcc_code[ofs..ofs + 8].copy_from_slice(val);
        }

        let ghcc_fun = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                ghcc_code.len(),
                libc::PROT_READ | libc::PROT_WRITE | libc::PROT_EXEC,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            )
        };

        // Allocate stack space for our generated code
        let stack_space = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                0x1000, // 4kb
                libc::PROT_READ | libc::PROT_WRITE,
                libc::MAP_PRIVATE | libc::MAP_ANONYMOUS,
                -1,
                0,
            ) as *mut u8
        };
    
        unsafe {
            std::ptr::copy_nonoverlapping(ghcc_code.as_ptr(), ghcc_fun as *mut u8, ghcc_code.len());
        }
    
        // copy the code to the memory region
        unsafe {
            std::ptr::copy_nonoverlapping(code.as_ptr(), mmap as *mut u8, code.len());
        }

        Self {
            stack: stack_space,
            code: mmap,
            ghcc_code: ghcc_fun,
        }
    }
    
    pub fn call(&self, args: &[u64]) -> u64 {
            // cast the memory region to a function pointer
        let f: extern "C" fn(*mut u8) = unsafe { std::mem::transmute(self.ghcc_code) };

        // Copy args to the stack
        for (i, item) in args.iter().enumerate() {
            unsafe {
                std::ptr::write_unaligned((self.stack as *mut u64).offset(i as isize), *item);
            }
        }

        // call the function with the stack pointer in RAX
        // This should work, however it requires inline asm and it requires us to use 
        // a "root stencil" that unpacks all of our arguments from our custom stack into
        // the registers that the other stencils expect but that is only done once per 
        // call so it should be fine
        f(self.stack);
        // get the result from the stack;

        unsafe {
            let value = std::ptr::read_unaligned(self.stack as *const u64);
            value
        }
    }
}

impl Drop for GeneratedCode {
    fn drop(&mut self) {
        unsafe {
            libc::munmap(self.code as *mut libc::c_void, 0x1000);
            libc::munmap(self.ghcc_code as *mut libc::c_void, 0x1000);
            libc::munmap(self.stack as *mut libc::c_void, 0x1000);
        }
    }
}

lazy_static! {
    pub static ref STENCILS: BTreeMap<String, Stencil> = compile_all_stencils();
}

struct CodeGen {
    code: Vec<u8>,
    stack_size: usize
}

fn fold_constants(fun: &BuiltIn, l: u64, r: u64) -> u64 {
    match fun {
        BuiltIn::Plus => l + r,
        BuiltIn::Times => l * r,
    }
}

fn fold_all_constants(fun: &BuiltIn, args: &[Expr]) -> Vec<Expr> {
    let mut result = Vec::new();
    let mut folded_constants = None;

    // Add all the variables to result and fold all constants into one
    for arg in args {
        match arg {
            Expr::Constant(Atom::Num(n)) => {
                if let Some(folded_constants_n) = folded_constants {
                    folded_constants = Some(fold_constants(fun, folded_constants_n, *n));
                } else {
                    folded_constants = Some(*n);
                }
            },
            Expr::Constant(Atom::Variable(n)) => {
                result.push(Expr::Constant(Atom::Variable(*n)));
            },
            Expr::Application(fun2, s) => {
                let folded_s = fold_all_constants(fun2, s);
                if folded_s.len() == 1 {
                    match folded_s[0] {
                        Expr::Constant(Atom::Num(n)) => {
                            if let Some(folded_constants_n) = folded_constants {
                                folded_constants = Some(fold_constants(fun, folded_constants_n, n));
                            } else {
                                folded_constants = Some(n);
                            }
                        },
                        Expr::Constant(Atom::Variable(n)) => {
                            result.push(Expr::Constant(Atom::Variable(n)));
                        },
                        _ => unreachable!()
                    }
                } else {
                    result.push(Expr::Application(*fun2, folded_s));
                }
            },
        }
    }

    if let Some(folded_constants_n) = folded_constants {
        result.push(Expr::Constant(Atom::Num(folded_constants_n)));
    }

    result
}

impl CodeGen {
    fn new(args: usize) -> Self {
        Self {  
            code: Vec::new(),
            stack_size: args * 8,
        }
    }

    fn copy_and_patch(&mut self, stencil: &Stencil, holes_values: Vec<u64>) {
        let start_ofs = self.code.len();
        self.code.extend_from_slice(&stencil.code);
        let end_ofs = self.code.len();
        let stencil_slice = &mut self.code[start_ofs..end_ofs];
        let hole_lengths = if stencil.large { 8 } else { 4 };
        for (&ofs, val) in stencil.holes.iter().zip(holes_values.iter()) {
            if stencil.large {
                stencil_slice[ofs..ofs + hole_lengths].copy_from_slice(&val.to_ne_bytes());
            } else {
                stencil_slice[ofs..ofs + hole_lengths].copy_from_slice(&(*val as u32).to_ne_bytes());
            }
        }
    }

    fn generate_take_stack(&mut self, n: usize) {
        let stencil = STENCILS.get("i64_take-stack").unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_put_stack(&mut self, n: usize) {
        let stencil = STENCILS.get("i64_put-stack").unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_take_2_stack(&mut self, n: usize) {
        let stencil = STENCILS.get("i64_take-2-stack").unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_duplex(&mut self) {
        let stencil = STENCILS.get("duplex").unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_i64_add(&mut self) {
        let stencil = STENCILS.get("i64_add").unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_i64_add_const(&mut self, n: u64) {
        let stencil = STENCILS.get("i64_add_const").unwrap();
        let holes_values = vec![n];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_i64_mul(&mut self) {
        let stencil = STENCILS.get("i64_mul").unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_i64_mul_const(&mut self, n: u64) {
        let stencil = STENCILS.get("i64_mul_const").unwrap();
        let holes_values = vec![n];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_ret(&mut self) {
        let stencil = STENCILS.get("ret").unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_op(&mut self, fun: &BuiltIn) {
        match fun {
            BuiltIn::Plus => {
                self.generate_i64_add();
            },
            BuiltIn::Times => {
                self.generate_i64_mul();
            },
        }
    }

    fn generate_op_const(&mut self, fun: &BuiltIn, n: u64) {
        match fun {
            BuiltIn::Plus => {
                self.generate_i64_add_const(n);
            },
            BuiltIn::Times => {
                self.generate_i64_mul_const(n);
            },
        }
    }

    fn generate_code_application(&mut self, fun: &BuiltIn, args: &[Expr]) {
        let first_variable = &args[0];

        match first_variable {
            Expr::Constant(Atom::Variable(n)) => {
                self.generate_take_stack(n * 8);
            },
            Expr::Application(fun2, args2) => {
                self.generate_code_application(&fun2, &args2);
            },
            _ => unreachable!()
        }

        for arg in args.iter().skip(1) {
            match arg {
                Expr::Constant(Atom::Variable(n)) => {
                    self.generate_take_2_stack(n * 8);
                    self.generate_op(fun);
                },
                Expr::Constant(Atom::Num(n)) => {
                    self.generate_op_const(fun, *n);
                },
                Expr::Application(fun2, args2) => {
                    // Save the current result to the stack 
                    // TODO: this stack allocation is a hack and wasteful
                    let stack_top = self.stack_size;
                    self.stack_size += 8;
                    self.generate_put_stack(stack_top);
                    self.generate_code_application(&fun2, &args2);
                    self.generate_take_2_stack(stack_top);
                    self.generate_op(fun);
                },
            }
        }
    }
}

pub fn generate_code(expr: &Expr, args: usize) -> Result<GeneratedCode, u64> {

    let mut cg = CodeGen::new(args);

    let ghc_stencil = STENCILS.get("__GHC_CC-CONVERTER__").unwrap();

    match expr {
        Expr::Constant(e) => {
            panic!("Not implemented");
        },
        Expr::Application(fun, args) => {
            let folded_args = fold_all_constants(fun, args);
            if folded_args.len() == 1 {
                match folded_args[0] {
                    Expr::Constant(Atom::Num(n)) => {
                        return Err(n);
                    },
                    _ => {
                        cg.generate_code_application(fun, &folded_args);
                    },
                }
            }
            cg.generate_code_application(fun, &folded_args);
        },
    }

    // Put the result back on the stack and return
    cg.generate_put_stack(0);
    cg.generate_ret();

    Ok(GeneratedCode::new(cg.stack_size * 8, ghc_stencil, &cg.code))
}

