use std::{cell::RefCell, collections::BTreeMap};

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

// Example of how to generate control flow constructs
//#[cfg(not(test))]
/*
    cg.generate_take_stack(0);
    cg.generate_rem_const(ConstValue::I64(2));
    cg.generate_eq_const(ConstValue::I64(0));
    cg.generate_if_else(|cg: &mut CodeGen| {
        cg.generate_take_const(ConstValue::I64(123));
        cg.generate_put_stack(args * 8);
        cg.generate_get_stackptr(args * 8);
        cg.generate_call_c_func(get_fn_ptr(hello_world));
    }, |cg| {
        cg.generate_take_const(ConstValue::I64(321));
        cg.generate_put_stack(args * 8);
        cg.generate_get_stackptr(args * 8);
        cg.generate_call_c_func(get_fn_ptr(hello_world));
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
        let code = &mut self.code.borrow_mut();
        let fixup_holes = &mut self.fixup_holes.borrow_mut();
        let start_ofs = code.len();
        code.extend_from_slice(&stencil.code);
        let end_ofs = code.len();
        let stencil_slice = &mut code[start_ofs..end_ofs];
        for (&reloc, val) in stencil.holes.iter().zip(holes_values.iter()) {
            let hole_len = reloc.reloc_type.get_hole_len();
            if reloc.offset + hole_len > stencil_slice.len() {
                continue;
            }
            match reloc.reloc_type {
                RelocType::Abs64 | RelocType::Abs64Fun => {
                    stencil_slice[reloc.offset..reloc.offset + hole_len].copy_from_slice(&val.to_ne_bytes());
                    if reloc.reloc_type == RelocType::Abs64Fun {
                        fixup_holes.push(start_ofs + reloc.offset);
                    }
                },
                RelocType::Abs32 => {
                    let val = *val as u32;
                    stencil_slice[reloc.offset..reloc.offset + hole_len].copy_from_slice(&val.to_ne_bytes());
                },
                RelocType::Rel32 => {
                    let val = (*val as i64 - (reloc.offset as i64 + 4)) as i32;
                    stencil_slice[reloc.offset..reloc.offset + hole_len].copy_from_slice(&val.to_ne_bytes());
                },
            }
        }

        for &reloc in &stencil.tail_holes {
            let hole_len = reloc.reloc_type.get_hole_len();
            if reloc.offset + hole_len > stencil_slice.len() {
                continue;
            }
            match reloc.reloc_type {
                RelocType::Abs64Fun => {
                    stencil_slice[reloc.offset..reloc.offset + hole_len].copy_from_slice(&end_ofs.to_ne_bytes());
                    fixup_holes.push(start_ofs + reloc.offset);
                },
                RelocType::Rel32 => {
                    let val = (end_ofs as i64 - (reloc.offset as i64 + 4)) as i32;
                    stencil_slice[reloc.offset..reloc.offset + hole_len].copy_from_slice(&val.to_ne_bytes());
                },
                _ => unreachable!("Function pointers should never have reloc type {:?}", reloc.reloc_type),
            }
        }
    }

    pub fn generate_take_stack(&self, n: usize) {
        self.generate_take_1_stack(n);
    }

    pub fn generate_put_stack(&self, n: usize) {
        self.generate_put_1_stack(n);
    }

    pub fn generate_put_1_stack(&self, n: usize) {
        let s_type = StencilType::new(StencilOperation::Put1, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_put_2_stack(&self, n: usize) {
        let s_type = StencilType::new(StencilOperation::Put2, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_take_1_stack(&self, n: usize) {
        let s_type = StencilType::new(StencilOperation::Take1, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_take_2_stack(&self, n: usize) {
        let s_type = StencilType::new(StencilOperation::Take2, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_take_1_const(&self, ir_const: ConstValue) {
        let s_type = StencilType::new(StencilOperation::Take1Const, Some(ir_const.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![ir_const.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_take_2_const(&self, ir_const: ConstValue) {
        let s_type = StencilType::new(StencilOperation::Take2Const, Some(ir_const.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![ir_const.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_add(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Add, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_add_const(&self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::AddConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_mul(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Mul, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_mul_const(&self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::MulConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_sub(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Sub, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_sub_const(&self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::SubConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_div(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Div, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_div_const(&self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::DivConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_rem(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Rem, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_rem_const(&self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::RemConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_eq(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Eq, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_eq_const(&self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::EqConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_neq(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Ne, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_neq_const(&self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::Ne, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_lt(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Lt, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_lt_const(&self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::LtConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_lte(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Lte, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_lte_const(&self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::LteConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_gt(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Gt, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_gt_const(&self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::GtConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_gte(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Gte, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_gte_const(&self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::GteConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_and(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::And, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_and_const(&self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::AndConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_or(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Or, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_or_const(&self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::OrConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_not(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Not, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_duplex1(&self) {
        let s_type = StencilType::new(StencilOperation::Duplex1, None);
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_duplex2(&self) {
        let s_type = StencilType::new(StencilOperation::Duplex2, None);
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_swap12(&self) {
        let s_type = StencilType::new(StencilOperation::Swap12, None);
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_ret(&self) {
        let s_type = StencilType::new(StencilOperation::Ret, None);
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_call_c_func(&self, func: *const c_void) {
        let s_type = StencilType::new(StencilOperation::CallCFunction, None);
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![func as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    // TODO: Long term this should not notbe in the code generator but only used internally inside
    //       the C&P specific code generator to get pointers to stack values
    pub fn generate_get_stackptr(&self, offset: usize) {
        let s_type = StencilType::new(StencilOperation::GetStackPtr, None);
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![offset as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_load(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Load, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_store(&self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Store, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_if<THEN: FnOnce()>(&self, then: THEN) {
        let cond_stencil = STENCILS.get(&StencilType::new(StencilOperation::CondBr, None)).unwrap();
        let then_hole = cond_stencil.holes[0];
        if then_hole.reloc_type == RelocType::Rel32 && then_hole.offset + 4 == cond_stencil.code.len() {
            let code_without_jump = cond_stencil.code_without_jump();
            let mut code = self.code.borrow_mut();
            let start_len = code.len();
            code.extend_from_slice(&code_without_jump);
            // drop the borrow so that the closure can borrow self again
            drop(code);
            then();
            let mut code = self.code.borrow_mut();
            let else_hole_ofs = start_len + cond_stencil.holes[1].offset;
            let end_ofs = code.len();
            let else_hole = &mut code[else_hole_ofs..else_hole_ofs + 4];
            else_hole.copy_from_slice(&((end_ofs as i32 - (else_hole_ofs as i32 + 4)) as u32).to_ne_bytes());
        } else {
            // TODO: Just implement this in case some other LLVM version produces some other code
            panic!("LLVM has produced some unexpected output it seems. Fix this case! {}:{}", file!(), line!())
        }
    }

    /// The offset is the offset from the current code length
    pub fn generate_uncond_branch(&self, ofs: i32) -> usize {
        let s_type = StencilType::new(StencilOperation::UncondBr, None);
        let stencil = STENCILS.get(&s_type).unwrap();
        debug_assert_eq!(stencil.tail_holes[0].reloc_type, RelocType::Rel32);
        debug_assert_eq!(stencil.tail_holes.len(), 1);
        let code = &mut self.code.borrow_mut();
        let start_len = code.len();
        code.extend_from_slice(&stencil.code);
        let inst_len = stencil.code.len();
        let ofs = ofs - (inst_len as i32);
        let tail_hole_ofs = start_len + stencil.tail_holes[0].offset;
        let ofs_hole = &mut code[tail_hole_ofs..tail_hole_ofs + 4];
        ofs_hole.copy_from_slice(&ofs.to_ne_bytes());
        tail_hole_ofs
    }

    pub fn generate_if_else<THEN: FnOnce(), ELSE: FnOnce()>(&self, then_branch: THEN, else_branch: ELSE) {
        let cond_stencil = STENCILS.get(&StencilType::new(StencilOperation::CondBr, None)).unwrap();
        debug_assert_eq!(cond_stencil.holes[0].reloc_type, RelocType::Rel32);
        debug_assert_eq!(cond_stencil.holes[1].reloc_type, RelocType::Rel32);
        let then_hole = cond_stencil.holes[0].offset;
        let else_hole = cond_stencil.holes[1].offset;
        if then_hole + 4 >= cond_stencil.code.len()  {
            let mut code = self.code.borrow_mut();
            let start_len = code.len();
            code.extend_from_slice(&cond_stencil.code_without_jump());
            // Drop the borrow so that the closure can borrow self again
            drop(code);
            then_branch();
            let mut code = self.code.borrow_mut();
            let tail_hole_ofs = self.generate_uncond_branch(0);
            let else_hole_ofs = start_len + else_hole;
            let end_ofs = code.len();
            let else_hole = &mut code[else_hole_ofs..else_hole_ofs + 4];
            else_hole.copy_from_slice(&((end_ofs - (else_hole_ofs + 4)) as u32).to_ne_bytes());
            // Drop the borrow so that the closure can borrow self again
            drop(code);
            else_branch();
            let code = &mut self.code.borrow_mut();
            let end_ofs = code.len();
            let then_tail_hole = &mut code[tail_hole_ofs..tail_hole_ofs + 4];
            then_tail_hole.copy_from_slice(&((end_ofs - (tail_hole_ofs + 4)) as u32).to_ne_bytes());
        } else {
            panic!("LLVM has produced some unexpected output it seems. Fix this case! {}:{}", file!(), line!())
        }
    }

    /// Important!: don't assume anything about the state of the registers in either the condition or the body
    pub fn generate_loop<COND: FnOnce(), BODY: FnOnce()>(&self, cond: COND, body: BODY) {
        let start_ofs = self.code.borrow().len();
        cond();
        self.generate_if(|| {
            body();
            self.generate_uncond_branch(self.code.borrow().len() as i32 - start_ofs as i32);
        });
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


