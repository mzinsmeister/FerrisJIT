
pub mod stencils;
pub mod ir;
mod disassemble;

use lazy_static::lazy_static;

use std::{collections::BTreeMap, fmt::Display};
use libc::c_void;
use stencils::Stencil;

use crate::{codegen::{ir::DataType, stencils::StencilOperation}, expr::{Atom, BuiltIn, Expr}};

use self::{ir::ConstValue, stencils::{compile_all_stencils, StencilType}};

// TODO: Once we go beyond basic arithmetic expressions we should have our own IR
//       We should also have a way to represent/address values so that we can insert
//       put/take instructions automatically and so that we can also map the same logic to LLVM IR

pub struct GeneratedCode<T> {
    pub stack: *mut u8,
    pub code: *const c_void,
    pub code_len: usize,
    pub ghcc_code: *const c_void,
    _phantom: std::marker::PhantomData<T>,
}

impl<T: Sized> GeneratedCode<T> {

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
            code_len: code.len(),
            ghcc_code: ghcc_fun,
            _phantom: std::marker::PhantomData,
        }
    }
    
    pub fn call(&self, args: &[i64]) -> T {
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
            let value = std::ptr::read_unaligned(self.stack as *const u64);
            std::mem::transmute_copy(&value)
        }
    }
}

impl<T> Drop for GeneratedCode<T> {
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
    fixup_holes: Vec<usize>,
    stack_ptr: usize,
    stack_size: usize,
}

fn fold_op(fun: &BuiltIn, l: Atom, r: Atom) -> Option<Atom> {
    match (fun, l, r) {
        (BuiltIn::Plus, Atom::Num(l), Atom::Num(r)) => Some(Atom::Num(l + r)),
        (BuiltIn::Times, Atom::Num(l), Atom::Num(r)) => Some(Atom::Num(l * r)),
        (BuiltIn::Minus, Atom::Num(l), Atom::Num(r)) => Some(Atom::Num(l - r)),
        (BuiltIn::Divide, Atom::Num(l), Atom::Num(r)) => Some(Atom::Num(l / r)),
        (BuiltIn::Equal, Atom::Num(l), Atom::Num(r)) => Some(Atom::Boolean(l == r)),
        (BuiltIn::Equal, Atom::Boolean(l), Atom::Boolean(r)) => Some(Atom::Boolean(l == r)),
        _ => None,
    }
}

fn is_commutative(fun: &BuiltIn) -> bool {
    match fun {
        BuiltIn::Plus | BuiltIn::Times | BuiltIn::Equal => true,
        _ => false,
    }
}

pub fn get_type(expr: &Expr) -> DataType {
    match expr {
        Expr::Constant(Atom::Num(_)) => DataType::I64,
        Expr::Constant(Atom::Boolean(_)) => DataType::Bool,
        Expr::Variable(_) => DataType::I64,
        Expr::Application(fun, _) => {
            match fun {
                BuiltIn::Plus | BuiltIn::Minus | BuiltIn::Times | BuiltIn::Divide => {
                    DataType::I64
                },
                BuiltIn::Equal => {
                    DataType::Bool
                }
            }
        },
    }
}

fn fold_constants(fun: &BuiltIn, args: &[Expr]) -> Option<Vec<Expr>> {
    if is_commutative(fun) {
        fold_all_constants_commutative(fun, args)
    } else {
        // We just merge all the constants that appear in the beginning of the list for now which should be safe to do
        // assuming that a list of arguments just means folding the arguments using the function
        let mut result = Vec::new();

        let mut args_iter = args.iter().peekable();

        let data_type = get_type(&args[0]);
        
        if let Some(Expr::Constant(n)) = args_iter.peek() {
            let mut folded_constants = *n;
            args_iter.next();
            while let Some(Expr::Constant(n)) = args_iter.peek() {
                folded_constants = fold_op(fun, folded_constants, *n)?;
                args_iter.next();
            }
            result.push(Expr::Constant(folded_constants));
        }

        for arg in args_iter {
            if get_type(&arg) != data_type {
                return None;
            }
            result.push(arg.clone());
        }

        Some(result)
    }
}

fn fold_all_constants_commutative(fun: &BuiltIn, args: &[Expr]) -> Option<Vec<Expr>> {
    let mut applications = Vec::new();
    let mut variables = Vec::new();
    let mut folded_constants = None;

    let data_type = get_type(&args[0]);

    // Add all the variables to result and fold all constants into one
    for arg in args {
        if get_type(&arg) != data_type {
            return None;
        }
        match arg {
            Expr::Constant(n) => {
                if let Some(folded_constants_n) = folded_constants {
                    folded_constants = Some(fold_op(fun, folded_constants_n, *n)?);
                } else {
                    folded_constants = Some(*n);
                }
            },
            Expr::Variable(n) => {
                variables.push(Expr::Variable(*n));
            },
            Expr::Application(fun2, s) => {
                let folded_s = fold_constants(fun2, s)?;
                if folded_s.len() == 1 {
                    match folded_s[0] {
                        Expr::Constant(n) => {
                            if let Some(folded_constants_n) = folded_constants {
                                folded_constants = Some(fold_op(fun, folded_constants_n, n)?);
                            } else {
                                folded_constants = Some(n);
                            }
                        },
                        Expr::Variable(n) => {
                            variables.push(Expr::Variable(n));
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
        result.push(Expr::Constant(folded_constants_n));
    }

    Some(result)
}

fn atom_to_const_value(atom: &Atom) -> ConstValue {
    match atom {
        Atom::Num(n) => ConstValue::I64(*n),
        Atom::Boolean(b) => ConstValue::Bool(*b),
    }
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
            fixup_holes: Vec::new(),
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

        for &ofs in &stencil.tail_holes {
            if ofs + hole_lengths > stencil_slice.len() {
                continue;
            }
            if stencil.large {
                stencil_slice[ofs..ofs + hole_lengths].copy_from_slice(&(end_ofs).to_ne_bytes());
                self.fixup_holes.push(start_ofs + ofs);
            } else {
                stencil_slice[ofs..ofs + hole_lengths].copy_from_slice(&((end_ofs - (ofs + 4)) as u32).to_ne_bytes());
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

    fn generate_take_const(&mut self, ir_const: ConstValue) {
        let s_type = StencilType::new(StencilOperation::TakeConst, Some(ir_const.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![ir_const.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_add(&mut self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Add, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_add_const(&mut self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::AddConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_mul(&mut self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Mul, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_mul_const(&mut self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::MulConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_sub(&mut self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Sub, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_sub_const(&mut self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::SubConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_div(&mut self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Div, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_div_const(&mut self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::DivConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_eq(&mut self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Eq, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    fn generate_duplex(&mut self) {
        let s_type = StencilType::new(StencilOperation::Duplex, None);
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
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
                self.generate_take_const(ConstValue::I64(*n));
            },
            Atom::Boolean(b) => {
                self.generate_take_const(ConstValue::Bool(*b));
            },
        }
    }

    fn generate_op(&mut self, fun: &BuiltIn, data_type: DataType) {
        match fun {
            BuiltIn::Plus => {
                self.generate_add(data_type);
            },
            BuiltIn::Times => {
                self.generate_mul(data_type);
            },
            BuiltIn::Minus => {
                self.generate_sub(data_type);
            },
            BuiltIn::Divide => {
                self.generate_div(data_type);
            },
            BuiltIn::Equal => {
                self.generate_eq(data_type);
            }
        }
    }

    fn generate_op_const(&mut self, fun: &BuiltIn, n: ConstValue) {
        match fun {
            BuiltIn::Plus => {
                self.generate_add_const(n);
            },
            BuiltIn::Times => {
                self.generate_mul_const(n);
            },
            BuiltIn::Minus => {
                self.generate_sub_const(n);
            },
            BuiltIn::Divide => {
                self.generate_div_const(n);
            },
            BuiltIn::Equal => {
                // TODO: This is not ideal but should do for now
                self.generate_duplex();
                self.generate_take_const(n);
                self.generate_eq(n.get_type());
            }
        }
    }

    fn generate_code_application(&mut self, fun: &BuiltIn, args: &[Expr]) -> DataType {
        let first_variable = &args[0];

        match first_variable {
            Expr::Variable(n) => {
                self.generate_take_stack(n * 8);
            },
            Expr::Constant(n) => {
                self.generate_take_const(atom_to_const_value(n));
            },
            Expr::Application(fun2, args2) => {
                self.generate_code_application(&fun2, &args2);
            },
        }

        for arg in args.iter().skip(1) {
            match arg {
                Expr::Variable(n) => {
                    self.generate_take_2_stack(n * 8);
                    self.generate_op(fun, DataType::I64);
                },
                Expr::Constant(n) => {
                    self.generate_op_const(fun, atom_to_const_value(n));
                },
                Expr::Application(fun2, args2) => {
                    // Save the current result to the stack 
                    let stack_top = self.stack_ptr;
                    self.stack_ptr += 8;
                    self.stack_size = self.stack_size.max(self.stack_ptr);
                    self.generate_put_stack(stack_top);
                    let ret_type = self.generate_code_application(&fun2, &args2);
                    self.generate_take_2_stack(stack_top);
                    self.stack_ptr -= 8;
                    self.generate_op(fun, ret_type);
                },
            }
        }
        match fun {
            BuiltIn::Plus | BuiltIn::Minus | BuiltIn::Times | BuiltIn::Divide => {
                DataType::I64
            },
            BuiltIn::Equal => {
                DataType::Bool
            }
        }
    }
}


#[derive(Debug)]
pub enum CodeGenError {
    Const(Atom),
    TypeError
}

impl Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodeGenError::Const(n) => {
                write!(f, "Result(const): {}", n)
            },
            CodeGenError::TypeError => {
                write!(f, "Type error")
            }
        }
    }
}

pub fn generate_code<T>(expr: &Expr, args: usize) -> Result<GeneratedCode<T>, CodeGenError> {

    let mut cg = CodeGen::new(args);

    let ghc_stencil = STENCILS.get(&StencilType::new(StencilOperation::GhcWrapper, None)).unwrap();

    match expr {
        Expr::Constant(e) => {
            cg.generate_atom(e);
        },
        Expr::Variable(n) => {
            cg.generate_take_stack(n * 8);
        },
        Expr::Application(fun, args) => {
            let folded_args = if let Some(fa) = fold_constants(fun, args) {
                fa
            } else {
                return Err(CodeGenError::TypeError);
            };
            if folded_args.len() == 1 {
                match &folded_args[0] {
                    // TODO: Once we introduce operators that also do something to
                    //       a single argument, this is no longer correct like this
                    //       but constant folding will need to be imroved anyway then
                    Expr::Constant(n) => {
                        return Err(CodeGenError::Const(n.clone()));
                    },
                    Expr::Variable(n) => {
                        cg.generate_take_stack(n * 8);
                    },
                    _ => {
                        cg.generate_code_application(fun, &folded_args);
                    },
                }
            } else {
                cg.generate_code_application(fun, &folded_args);
            }
        },
    }

    // Put the result back on the stack and return
    cg.generate_put_stack(0);
    cg.generate_ret();

    let gc = GeneratedCode::<T>::new(cg.stack_size * 8, ghc_stencil, &cg.code);

    // Fix up the holes that need an absolute address

    // TODO: Find a nicer solution for this
    for &ofs in cg.fixup_holes.iter() {
        unsafe {
            let addr = gc.code.byte_offset(ofs as isize) as *mut u64;
            let start_offset = addr.read_unaligned();
            let new_addr = gc.code as u64 + start_offset;
            addr.write_unaligned(new_addr);           
        }
    }

    #[cfg(feature = "print-asm")]
    {
        let gc_code_slice = unsafe { std::slice::from_raw_parts(gc.code as *const u8, cg.code.len()) };
        disassemble::disassemble(gc_code_slice);
    }

    Ok(gc)
}


