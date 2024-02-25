use std::collections::BTreeMap;

use crate::codegen::stencils::{compile_all_stencils, Stencil};

use super::{ir::{ConstValue, DataType}, stencils::{StencilOperation, StencilType}, GeneratedCode};
use lazy_static::lazy_static;
use libc::c_void;


lazy_static! {
    pub static ref STENCILS: BTreeMap<StencilType, Stencil> = compile_all_stencils();
}

/// This does the actual Copy and Patch compilation
pub struct CopyPatchBackend {
    code: Vec<u8>,
    fixup_holes: Vec<usize>,
}

// TODO: Introduce the concept of "Values" like in LLVM IR
//       and then either do nothing if the values is in the correct
//       register or move it there, either from the stack or from a 
//       different register. This of course requires tracking which
//       values are in which registers at all times
#[allow(dead_code)]
impl CopyPatchBackend {
    pub fn new(args: usize) -> Self {
        Self {  
            code: Vec::new(),
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

    pub fn generate_take_stack(&mut self, n: usize) {
        let s_type = StencilType::new(StencilOperation::Take, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_put_stack(&mut self, n: usize) {
        let s_type = StencilType::new(StencilOperation::Put, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_take_1_stack(&mut self, n: usize) {
        let s_type = StencilType::new(StencilOperation::Take1, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_take_2_stack(&mut self, n: usize) {
        let s_type = StencilType::new(StencilOperation::Take2, Some(DataType::I64));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_take_const(&mut self, ir_const: ConstValue) {
        let s_type = StencilType::new(StencilOperation::TakeConst, Some(ir_const.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![ir_const.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_add(&mut self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Add, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_add_const(&mut self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::AddConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_mul(&mut self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Mul, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_mul_const(&mut self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::MulConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_sub(&mut self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Sub, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_sub_const(&mut self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::SubConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_div(&mut self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Div, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_div_const(&mut self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::DivConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_rem(&mut self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Rem, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_rem_const(&mut self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::RemConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_eq(&mut self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Eq, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_eq_const(&mut self, n: ConstValue) {
        let s_type = StencilType::new(StencilOperation::EqConst, Some(n.get_type()));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![n.bitcast_to_u64()];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_duplex(&mut self) {
        let s_type = StencilType::new(StencilOperation::Duplex, None);
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_ret(&mut self) {
        let s_type = StencilType::new(StencilOperation::Ret, None);
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_call_c_func(&mut self, func: *const c_void) {
        let s_type = StencilType::new(StencilOperation::CallCFunction, None);
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![func as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    // TODO: Long term this should not be in the code generator but only used internally inside
    //       the C&P specific code generator to get pointers to stack values
    pub fn generate_get_stackptr(&mut self, offset: usize) {
        let s_type = StencilType::new(StencilOperation::GetStackPtr, None);
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![offset as u64];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_load(&mut self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Load, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_store(&mut self, data_type: DataType) {
        let s_type = StencilType::new(StencilOperation::Store, Some(data_type));
        let stencil = STENCILS.get(&s_type).unwrap();
        let holes_values = vec![];
        self.copy_and_patch(stencil, holes_values);
    }

    pub fn generate_if<THEN: Fn(&mut Self)>(&mut self, then: THEN) {
        let cond_stencil = STENCILS.get(&StencilType::new(StencilOperation::CondBr, None)).unwrap();
        assert_eq!(cond_stencil.get_holes_bytelen(), 4);
        let then_hole = cond_stencil.holes[0];
        if then_hole + 4 >= cond_stencil.code.len() {
            let code_without_jump = cond_stencil.code_without_jump();
            let start_len = self.code.len();
            self.code.extend_from_slice(&code_without_jump);
            then(self);
            let else_hole_ofs = start_len + cond_stencil.holes[1];
            let end_ofs = self.code.len();
            let else_hole = &mut self.code[else_hole_ofs..else_hole_ofs + 4];
            else_hole.copy_from_slice(&((end_ofs - (else_hole_ofs + 4)) as u32).to_ne_bytes());
        } else {
            // TODO: Just implement this in case some other LLVM version produces some other code
            panic!("LLVM has produced some unexpected output it seems. Fix this case! {}:{}", file!(), line!())
        }
    }

    /// The offset is the offset from the current code length
    pub fn generate_uncond_branch(&mut self, ofs: i32) -> usize {
        let s_type = StencilType::new(StencilOperation::UncondBr, None);
        let stencil = STENCILS.get(&s_type).unwrap();
        assert_eq!(stencil.get_holes_bytelen(), 4);
        assert_eq!(stencil.tail_holes.len(), 1);
        let start_len = self.code.len();
        self.code.extend_from_slice(&stencil.code);
        let inst_len = stencil.code.len();
        let ofs = ofs - (inst_len as i32);
        let tail_hole_ofs = start_len + stencil.tail_holes[0];
        let ofs_hole = &mut self.code[tail_hole_ofs..tail_hole_ofs + 4];
        ofs_hole.copy_from_slice(&ofs.to_ne_bytes());
        tail_hole_ofs
    }

    pub fn generate_if_else<THEN: Fn(&mut Self), ELSE: Fn(&mut Self)>(&mut self, then: THEN, else_: ELSE) {
        let cond_stencil = STENCILS.get(&StencilType::new(StencilOperation::CondBr, None)).unwrap();
        assert_eq!(cond_stencil.get_holes_bytelen(), 4);
        let then_hole = cond_stencil.holes[0];
        let else_hole = cond_stencil.holes[1];
        if then_hole + 4 >= cond_stencil.code.len()  {
            let start_len = self.code.len();
            self.code.extend_from_slice(&cond_stencil.code_without_jump());
            then(self);
            let tail_hole_ofs = self.generate_uncond_branch(0);
            let else_hole_ofs = start_len + else_hole;
            let end_ofs = self.code.len();
            let else_hole = &mut self.code[else_hole_ofs..else_hole_ofs + 4];
            else_hole.copy_from_slice(&((end_ofs - (else_hole_ofs + 4)) as u32).to_ne_bytes());
            else_(self);
            let end_ofs = self.code.len();
            let then_tail_hole = &mut self.code[tail_hole_ofs..tail_hole_ofs + 4];
            then_tail_hole.copy_from_slice(&((end_ofs - (tail_hole_ofs + 4)) as u32).to_ne_bytes());
        } else {
            panic!("LLVM has produced some unexpected output it seems. Fix this case! {}:{}", file!(), line!())
        }
    }

    /// Important!: don't assume anything about the state of the registers in either the condition or the body
    pub fn generate_loop<COND: Fn(&mut Self), BODY: Fn(&mut Self)>(&mut self, cond: COND, body: BODY) {
        let start_ofs = self.code.len();
        cond(self);
        self.generate_if(|cg| {
            body(cg);
            cg.generate_uncond_branch(cg.code.len() as i32 - start_ofs as i32);
        });
    }

    pub fn generate_code<T>(&self, stack_size: usize) -> GeneratedCode<T> {
    
        let ghc_stencil = STENCILS.get(&StencilType::new(StencilOperation::GhcWrapper, None)).unwrap();
    
        let gc = GeneratedCode::<T>::new(stack_size, ghc_stencil, &self.code);
    
        // Fix up the holes that need an absolute address
        // TODO: Find a nicer solution for this
        for &ofs in self.fixup_holes.iter() {
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
    
        gc
    }
    
}


