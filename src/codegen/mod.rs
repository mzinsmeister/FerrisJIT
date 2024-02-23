
pub mod stencils;
pub mod ir;

use goblin::mach::constants::S_16BYTE_LITERALS;
use lazy_static::lazy_static;

use std::collections::BTreeMap;
use libc::c_void;
use stencils::Stencil;

use crate::{codegen::{ir::DataType, stencils::StencilOperation}, expr::{Atom, BuiltIn, Expr}};

use self::stencils::{compile_all_stencils, StencilType};

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
        // TODO: We could (and maybe should) also use the actual stack for this
        let stack_space = unsafe {
            libc::mmap(
                std::ptr::null_mut(),
                stack_size,
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
    
    pub fn call(&self, args: &[i64]) -> i64 {
            // cast the memory region to a function pointer
        let f: extern "C" fn(*mut u8) = unsafe { std::mem::transmute(self.ghcc_code) };

        // Copy args to the stack
        for (i, item) in args.iter().enumerate() {
            unsafe {
                std::ptr::write_unaligned((self.stack as *mut i64).offset(i as isize), *item);
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
            let value = std::ptr::read_unaligned(self.stack as *const i64);
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
    pub static ref STENCILS: BTreeMap<StencilType, Stencil> = compile_all_stencils();
}

struct CodeGen {
    code: Vec<u8>,
    stack_ptr: usize,
    stack_size: usize,
}

fn fold_op(fun: &BuiltIn, l: i64, r: i64) -> i64 {
    match fun {
        BuiltIn::Plus => l + r,
        BuiltIn::Times => l * r,
        BuiltIn::Minus => l - r,
        BuiltIn::Divide => l / r,
    }
}

fn is_commutative(fun: &BuiltIn) -> bool {
    match fun {
        BuiltIn::Plus => true,
        BuiltIn::Times => true,
        _ => false,
    }
}

fn fold_constants(fun: &BuiltIn, args: &[Expr]) -> Vec<Expr> {
    if is_commutative(fun) {
        fold_all_constants_commutative(fun, args)
    } else {
        // We just merge all the constants that appear in the beginning of the list for now which should be safe to do
        // assuming that a list of arguments just means folding the arguments using the function
        let mut result = Vec::new();

        let mut args_iter = args.iter().peekable();
        
        if let Some(Expr::Constant(Atom::Num(n))) = args_iter.peek() {
            let mut folded_constants = *n;
            args_iter.next();
            while let Some(Expr::Constant(Atom::Num(n))) = args_iter.peek() {
                folded_constants = fold_op(fun, folded_constants, *n);
                args_iter.next();
            }
            result.push(Expr::Constant(Atom::Num(folded_constants)));
        }

        for arg in args_iter {
            result.push(arg.clone());
        }

        result
    }
}

fn fold_all_constants_commutative(fun: &BuiltIn, args: &[Expr]) -> Vec<Expr> {
    let mut applications = Vec::new();
    let mut variables = Vec::new();
    let mut folded_constants = None;

    // Add all the variables to result and fold all constants into one
    for arg in args {
        match arg {
            Expr::Constant(Atom::Num(n)) => {
                if let Some(folded_constants_n) = folded_constants {
                    folded_constants = Some(fold_op(fun, folded_constants_n, *n));
                } else {
                    folded_constants = Some(*n);
                }
            },
            Expr::Constant(Atom::Variable(n)) => {
                variables.push(Expr::Constant(Atom::Variable(*n)));
            },
            Expr::Application(fun2, s) => {
                let folded_s = fold_constants(fun2, s);
                if folded_s.len() == 1 {
                    match folded_s[0] {
                        Expr::Constant(Atom::Num(n)) => {
                            if let Some(folded_constants_n) = folded_constants {
                                folded_constants = Some(fold_op(fun, folded_constants_n, n));
                            } else {
                                folded_constants = Some(n);
                            }
                        },
                        Expr::Constant(Atom::Variable(n)) => {
                            variables.push(Expr::Constant(Atom::Variable(n)));
                        },
                        _ => unreachable!()
                    }
                } else {
                    applications.push(Expr::Application(*fun2, folded_s));
                }
            },
        }
    }

    // We make sure to execute applications first then variables so that
    // we minimize the number of times we need to take from the stack
    let mut result = Vec::new();
    result.extend(applications);
    result.extend(variables);

    if let Some(folded_constants_n) = folded_constants {
        result.push(Expr::Constant(Atom::Num(folded_constants_n)));
    }

    result
}


// TODO: Introduce the concept of "Values" like in LLVM IR
//       and then either do nothing if the values is in the correct
//       register or move it there, either from the stack or from a 
//       different register. This of course requires tracking which
//       values are in which registers at all times

impl CodeGen {
    fn new(args: usize) -> Self {
        Self {  
            code: Vec::new(),
            stack_ptr: args * 8,
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
        let s_type = StencilType::new(StencilOperation::Take, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_put_stack(&mut self, n: usize) {
        let s_type = StencilType::new(StencilOperation::Put, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_take_2_stack(&mut self, n: usize) {
        let s_type = StencilType::new(StencilOperation::Take2, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_take_const(&mut self, n: i64) {
        let s_type = StencilType::new(StencilOperation::TakeConst, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_i64_add(&mut self) {
        let s_type = StencilType::new(StencilOperation::Add, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_i64_add_const(&mut self, n: i64) {
        let s_type = StencilType::new(StencilOperation::AddConst, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_i64_mul(&mut self) {
        let s_type = StencilType::new(StencilOperation::Mul, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_i64_mul_const(&mut self, n: i64) {
        let s_type = StencilType::new(StencilOperation::MulConst, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_i64_sub(&mut self) {
        let s_type = StencilType::new(StencilOperation::Sub, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_i64_sub_const(&mut self, n: i64) {
        let s_type = StencilType::new(StencilOperation::SubConst, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_i64_div(&mut self) {
        let s_type = StencilType::new(StencilOperation::SDiv, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_i64_div_const(&mut self, n: i64) {
        let s_type = StencilType::new(StencilOperation::SDivConst, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_ret(&mut self) {
        let s_type = StencilType::new(StencilOperation::Ret, None);
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_atom(&mut self, atom: &Atom) {
        match atom {
            Atom::Num(n) => {
                self.generate_take_stack(0);
                self.generate_put_stack(*n as usize);
            },
            Atom::Variable(n) => {
                self.generate_take_stack(n * 8);
            },
        }
    }

    fn generate_op(&mut self, fun: &BuiltIn) {
        match fun {
            BuiltIn::Plus => {
                self.generate_i64_add();
            },
            BuiltIn::Times => {
                self.generate_i64_mul();
            },
            BuiltIn::Minus => {
                self.generate_i64_sub();
            },
            BuiltIn::Divide => {
                self.generate_i64_div();
            },
        }
    }

    fn generate_op_const(&mut self, fun: &BuiltIn, n: i64) {
        match fun {
            BuiltIn::Plus => {
                self.generate_i64_add_const(n);
            },
            BuiltIn::Times => {
                self.generate_i64_mul_const(n);
            },
            BuiltIn::Minus => {
                self.generate_i64_sub_const(n);
            },
            BuiltIn::Divide => {
                self.generate_i64_div_const(n);
            },
        }
    }

    fn generate_code_application(&mut self, fun: &BuiltIn, args: &[Expr]) {
        let first_variable = &args[0];

        match first_variable {
            Expr::Constant(Atom::Variable(n)) => {
                self.generate_take_stack(n * 8);
            },
            Expr::Constant(Atom::Num(n)) => {
                self.generate_take_const(*n);
            },
            Expr::Application(fun2, args2) => {
                self.generate_code_application(&fun2, &args2);
            },
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
                    let stack_top = self.stack_ptr;
                    self.stack_ptr += 8;
                    self.stack_size = self.stack_size.max(self.stack_ptr);
                    self.generate_put_stack(stack_top);
                    self.generate_code_application(&fun2, &args2);
                    self.generate_take_2_stack(stack_top);
                    self.stack_ptr -= 8;
                    self.generate_op(fun);
                },
            }
        }
    }
}

pub fn generate_code(expr: &Expr, args: usize) -> Result<GeneratedCode, i64> {

    let mut cg = CodeGen::new(args);

    let ghc_stencil = STENCILS.get(&StencilType::new(StencilOperation::GhcWrapper, None)).unwrap();

    match expr {
        Expr::Constant(e) => {
            cg.generate_atom(e);
        },
        Expr::Application(fun, args) => {
            let folded_args = fold_constants(fun, args);
            if folded_args.len() == 1 {
                match folded_args[0] {
                    // TODO: Once we introduce operators that also do something to
                    //       a single argument, this is no longer correct like this
                    //       but constant folding will need to be imroved anyway then
                    Expr::Constant(Atom::Num(n)) => {
                        return Err(n);
                    },
                    Expr::Constant(Atom::Variable(n)) => {
                        cg.generate_take_stack(n * 8);
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

