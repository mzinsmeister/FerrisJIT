use std::{cell::RefCell, collections::BTreeMap, hint::black_box};

use crate::codegen::stencils::{compile_all_stencils, Stencil, RelocType};

#[cfg(feature = "print-asm")]
use super::disassemble;

use super::{ir::{ConstValue, DataType}, stencils::{StencilOperation, StencilType}, GeneratedCode};
use lazy_static::lazy_static;
use libc::c_void;


lazy_static! {
    pub static ref STENCILS: BTreeMap<StencilType, Stencil> = compile_all_stencils();
}

/// This does the actual Copy and Patch compilation
pub struct CopyPatchBackend {
    code: RefCell<Vec<u8>>,
    fixup_holes: RefCell<Vec<usize>>,
}

// Example of how to emit control flow constructs
//#[cfg(not(test))]
/*
    cg.emit_take_stack(0);
    cg.emit_rem_const(ConstValue::I64(2));
    cg.emit_eq_const(ConstValue::I64(0));
    cg.emit_if_else(|cg: &mut CodeGen| {
        cg.emit_take_const(ConstValue::I64(123));
        cg.emit_put_stack(args * 8);
        cg.emit_get_stackptr(args * 8);
        cg.emit_call_c_func(get_fn_ptr(hello_world));
    }, |cg| {
        cg.emit_take_const(ConstValue::I64(321));
        cg.emit_put_stack(args * 8);
        cg.emit_get_stackptr(args * 8);
        cg.emit_call_c_func(get_fn_ptr(hello_world));
    });
}*/

#[allow(dead_code)]
impl CopyPatchBackend {
    pub fn new() -> Self {
        Self {
            code: RefCell::new(Vec::new()),
            fixup_holes: RefCell::new(Vec::new()),
        }
    }

    pub fn reset(&self) {
        self.code.borrow_mut().clear();
        self.fixup_holes.borrow_mut().clear();
    }   

    fn copy_and_patch(&self, stencil: &Stencil, holes_values: Vec<u64>) {
        black_box(self);
        black_box(stencil);
        black_box(holes_values);
    }

    pub fn emit_take_stack(&self, n: usize) {
        self.emit_take_1_stack(n);
    }

    pub fn emit_put_stack(&self, n: usize) {
        self.emit_put_1_stack(n);
    }

    pub fn emit_put_1_stack(&self, n: usize) {
        black_box(self);
        black_box(n);

    }

    pub fn emit_put_2_stack(&self, n: usize) {
        black_box(self);
        black_box(n);

    }

    pub fn emit_take_1_stack(&self, n: usize) {
        black_box(self);
        black_box(n);

    }

    pub fn emit_take_2_stack(&self, n: usize) {
        black_box(self);
        black_box(n);

    }

    pub fn emit_take_1_const(&self, ir_const: ConstValue) {
        black_box(self);
        black_box(ir_const);

    }

    pub fn emit_take_2_const(&self, ir_const: ConstValue) {
        black_box(self);
        black_box(ir_const);

    }

    pub fn emit_add(&self, data_type: DataType) {
        black_box(self);
        black_box(data_type);

    }

    pub fn emit_add_const(&self, n: ConstValue) {
        black_box(self);
        black_box(n);

    }

    pub fn emit_mul(&self, data_type: DataType) {
        black_box(self);
        black_box(data_type);

    }

    pub fn emit_mul_const(&self, n: ConstValue) {
        black_box(self);
        black_box(n);

    }

    pub fn emit_sub(&self, data_type: DataType) {
        black_box(self);
        black_box(data_type);

    }

    pub fn emit_sub_const(&self, n: ConstValue) {
        black_box(self);
        black_box(n);

    }

    pub fn emit_div(&self, data_type: DataType) {
        black_box(self);
        black_box(data_type);

    }

    pub fn emit_div_const(&self, n: ConstValue) {
        black_box(self);
        black_box(n);

    }

    pub fn emit_rem(&self, data_type: DataType) {
        black_box(self);
        black_box(data_type);

    }

    pub fn emit_rem_const(&self, n: ConstValue) {
        black_box(self);
        black_box(n);

    }

    pub fn emit_eq(&self, data_type: DataType) {
        black_box(self);
        black_box(data_type);

    }

    pub fn emit_eq_const(&self, n: ConstValue) {
         black_box(self);
         black_box(n);

    }

    pub fn emit_neq(&self, data_type: DataType) {
        black_box(self);
        black_box(data_type);

    }

    pub fn emit_neq_const(&self, n: ConstValue) {
        black_box(self);

    }

    pub fn emit_lt(&self, data_type: DataType) {
        black_box(self);
        black_box(data_type);

    }

    pub fn emit_lt_const(&self, n: ConstValue) {
        black_box(self);
        black_box(n);

    }

    pub fn emit_lte(&self, data_type: DataType) {
        black_box(self);
        black_box(data_type);

    }

    pub fn emit_lte_const(&self, n: ConstValue) {
        black_box(self);
        black_box(n);

    }         

    pub fn emit_gt(&self, data_type: DataType) {
        black_box(self);
        black_box(data_type);

    }

    pub fn emit_gt_const(&self, n: ConstValue) {
        black_box(self);
        black_box(n);

    }

    pub fn emit_gte(&self, data_type: DataType) {
        black_box(self);
        black_box(data_type);

    }

    pub fn emit_gte_const(&self, n: ConstValue) {
        black_box(self);
        black_box(n);

    }

    pub fn emit_and(&self, data_type: DataType) {
        black_box(self);
        black_box(data_type);

    }

    pub fn emit_and_const(&self, n: ConstValue) {
        black_box(self);
        black_box(n);

    }

    pub fn emit_or(&self, data_type: DataType) {
        black_box(self);
        black_box(data_type);

    }

    pub fn emit_or_const(&self, n: ConstValue) {
        black_box(self);
        black_box(n);

    }

    pub fn emit_not(&self, data_type: DataType) {
        black_box(self);
        black_box(data_type);

    }

    pub fn emit_duplex1(&self) {
        black_box(self);

    }

    pub fn emit_duplex2(&self) {
        black_box(self);

    }

    pub fn emit_swap12(&self) {
        black_box(self);
    }

    pub fn emit_ret(&self) {
        black_box(self);
    }

    pub fn emit_call_c_func(&self, func: *const c_void, state_stack_pos: usize) {
        black_box(func);
        black_box(state_stack_pos);
    }

    // TODO: Long term this should not notbe in the code generator but only used internally inside
    //       the C&P specific code generator to get pointers to stack values
    pub fn emit_get_stackptr(&self, offset: usize) {
        black_box(offset);
    }

    pub fn emit_load(&self, data_type: DataType) {
        black_box(data_type);
    }

    pub fn emit_store(&self, data_type: DataType) {
        black_box(data_type);
    }

    pub fn emit_if<E>(&self, then: impl FnOnce() -> Result<(), E>) -> Result<(), E> {
        black_box(then);
        Ok(())
    }

    /// The offset is the offset from the current code length
    pub fn emit_uncond_branch(&self, ofs: i32) -> usize {
        black_box(ofs);
        0
    }

    pub fn emit_if_else<THEN: FnOnce(), ELSE: FnOnce()>(&self, then_branch: THEN, else_branch: ELSE) {
        black_box(then_branch);
        black_box(else_branch);
    }

    /// Important!: don't assume anything about the state of the registers in either the condition or the body
    pub fn emit_loop<E>(&self, cond: impl FnOnce() -> Result<(), E>, body: impl FnOnce() -> Result<(), E>) -> Result<(), E> {
        black_box(body);
        black_box(cond);
        Ok(())
    }

    pub fn generate_code(&self, stack_size: usize) -> GeneratedCode {
    
        let ghc_stencil = STENCILS.get(&StencilType::new(StencilOperation::GhcWrapper, None)).unwrap();
    
        let gc = GeneratedCode::new(stack_size, ghc_stencil, &self.code.borrow());
    
        // Fix up the holes that need an absolute address
        // TODO: Find a nicer solution for this
        for &ofs in self.fixup_holes.borrow().iter() {
            unsafe {
                let addr = gc.code.byte_offset(ofs as isize) as *mut u64;
                let start_offset = addr.read_unaligned();
                let new_addr = gc.code as u64 + start_offset;
                addr.write_unaligned(new_addr);           
            }
        }
    
    #[cfg(feature = "print-asm")]
        {
            let gc_code_slice = unsafe { std::slice::from_raw_parts(gc.code as *const u8, self.code.borrow().len()) };
            disassemble::disassemble(gc_code_slice);
        }
    
        gc
    }
    
}


