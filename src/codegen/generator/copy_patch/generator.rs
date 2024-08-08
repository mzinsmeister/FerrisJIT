use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

use crate::codegen::{generator::Generator, get_data_type_size, ir::{ConstValue, DataType}};

use super::{memory_management::{CGValue, MemoryManagement}, CopyPatchBackend};


struct CopyPatchGenerator {
    cp_backend: Rc<CopyPatchBackend>,
    memory_management: MemoryManagement,
    id_value_mapping: BTreeMap<usize, usize>
}

impl Generator for CopyPatchGenerator {
    fn map_arg(&mut self, arg_n: usize, n: usize) {
        let i = self.memory_management.clone_value(n);
        self.id_value_mapping.insert(arg_n, i);
    }

    fn new_var(&mut self, n: usize, data_type: DataType) {
        let i = self.memory_management.allocate_stack(data_type);
        self.id_value_mapping.insert(n, i);
    }

    fn init_var(&mut self, c: crate::codegen::ir::ConstValue, n: usize) {
        let i = self.id_value_mapping.get(&n).unwrap();
        self.memory_management.init(i, c);
    }

    fn free_value(&mut self, n: usize) {
        let i = self.id_value_mapping.get(&n).unwrap();
        self.memory_management.free_value(*i);
    }

    fn copy_value(&mut self, src: usize, dest: usize) {
        let src_i = self.id_value_mapping[&src];
        let dest_i = self.id_value_mapping[&dest];
        let dest_ptr = match &self.memory_management.values[dest_i] {
            CGValue::Variable{stack_pos,..} => *stack_pos,
            _ => unreachable!(),
        };
        self.memory_management.put_in_reg(0, src_i);
        self.cp_backend.emit_put_1_stack(dest_ptr);
        self.memory_management.put_in_reg(1, dest_i);
    }

    fn add(&mut self, n: usize, m: usize) {
        self.gen_arith(CopyPatchBackend::emit_add, n, m);
    }

    fn add_const(&mut self, n: usize, c: crate::codegen::ir::ConstValue) {
        self.gen_arith_const(CopyPatchBackend::emit_add_const, n, c);
    }

    fn sub(&mut self, n: usize, m: usize) {
        self.gen_arith(CopyPatchBackend::emit_sub, n, m);
    }

    fn sub_const(&mut self, n: usize, c: crate::codegen::ir::ConstValue) {
        self.gen_arith_const(CopyPatchBackend::emit_sub_const, n, c);
    }

    fn mul(&mut self, n: usize, m: usize) {
        self.gen_arith(CopyPatchBackend::emit_mul, n, m);
    }

    fn mul_const(&mut self, n: usize, c: crate::codegen::ir::ConstValue) {
        self.gen_arith_const(CopyPatchBackend::emit_mul_const, n, c);
    }

    fn div(&mut self, n: usize, m: usize) {
        self.gen_arith(CopyPatchBackend::emit_div, n, m);
    }

    fn div_const(&mut self, n: usize, c: crate::codegen::ir::ConstValue) {
        self.gen_arith_const(CopyPatchBackend::emit_div_const, n, c);
    }

    fn rem(&mut self, n: usize, m: usize) {
        self.gen_arith(CopyPatchBackend::emit_rem, n, m);
    }

    fn rem_const(&mut self, n: usize, c: crate::codegen::ir::ConstValue) {
        self.gen_arith_const(CopyPatchBackend::emit_rem_const, n, c);
    }

    fn eq(&mut self, n: usize, m: usize) {
        self.gen_arith(CopyPatchBackend::emit_eq, n, m);
    }

    fn neq(&mut self, n: usize, m: usize) {
        self.gen_arith(CopyPatchBackend::emit_neq, n, m);
    }

    fn lt(&mut self, n: usize, m: usize) {
        self.gen_arith(CopyPatchBackend::emit_lt, n, m);
    }

    fn lte(&mut self, n: usize, m: usize) {
        self.gen_arith(CopyPatchBackend::emit_lte, n, m);
    }

    fn gt(&mut self, n: usize, m: usize) {
        self.gen_arith(CopyPatchBackend::emit_gt, n, m);
    }

    fn gte(&mut self, n: usize, m: usize) {
        self.gen_arith(CopyPatchBackend::emit_gte, n, m);
    }

    fn and(&mut self, n: usize, m: usize) {
        self.gen_arith(CopyPatchBackend::emit_and, n, m);
    }

    fn or(&mut self, n: usize, m: usize) {
        self.gen_arith(CopyPatchBackend::emit_or, n, m);
    }

    fn not(&mut self, n: usize) {
        self.memory_management.put_in_reg(0, n);
        match &self.memory_management.values[n] {
            CGValue::Variable{data_type,..} => {
                self.cp_backend.emit_not(data_type.clone());
            },
            _ => unreachable!(),
        }
        self.memory_management.dirty_reg(0);
    }

    fn gep(&mut self, pointee_type: DataType, ptr: usize, idx: usize) {
        // Create intermediate value for idx to not overwrite it
        let idx_i = self.memory_management.clone_value(idx);
        self.memory_management.put_in_reg(0, idx_i);
        self.cp_backend.emit_mul_const(ConstValue::U64(get_data_type_size(&pointee_type) as u64));
        self.memory_management.dirty_reg(0);
        self.memory_management.put_in_regs(ptr, idx_i);
        self.cp_backend.emit_add(DataType::U64);
        self.memory_management.dirty_reg(0);
        self.free_value(idx_i);
    }

    fn gep_const(&mut self, pointee_type: DataType, ptr: usize, idx_const: isize) {
        self.memory_management.put_in_reg(0, ptr);
        self.cp_backend.emit_add_const(ConstValue::U64(idx_const as u64 * get_data_type_size(&pointee_type) as u64));
        self.memory_management.dirty_reg(0);
    }

    fn deref_ptr(&mut self, pointee_type: DataType, ptr_i: usize, target_i: usize) {
    }

    fn write_to_ptr(&mut self, ptr_i: usize, value_i: usize) {
        todo!()
    }

    fn ret(&mut self, n: Option<usize>) {
        todo!()
    }

    fn c_call(&mut self, func: *const std::os::raw::c_void, args_ptr: usize, result: usize) {
        todo!()
    }

    fn pre_if_then(&mut self, condition: usize) {
        todo!()
    }

    fn post_if_then(&mut self) {
        todo!()
    }

    fn pre_if(&mut self, condition: usize) {
        todo!()
    }

    fn pre_then(&mut self) {
        todo!()
    }

    fn post_then(&mut self) {
        todo!()
    }

    fn pre_else(&mut self) {
        todo!()
    }

    fn post_else(&mut self) {
        todo!()
    }

    fn post_if(&mut self) {
        todo!()
    }

    fn pre_loop_cond(&mut self) {
        todo!()
    }

    fn pre_loop_body(&mut self, condition: usize) {
        todo!()
    }

    fn post_loop_body(&mut self) {
        todo!()
    }
}

impl CopyPatchGenerator {

    fn gen_arith(&mut self, gen_op: fn(&CopyPatchBackend, DataType), l: usize, r: usize) {
        self.memory_management.put_in_regs(l, r);
        match &self.memory_management.values[l] {
            CGValue::Variable{data_type,..} => {
                gen_op(&self.cp_backend, data_type.clone());
            },
            _ => unreachable!(),
        }
    }

    fn gen_arith_const(&mut self, gen_op: fn(&CopyPatchBackend, ConstValue), l: usize, r: ConstValue) {
        self.memory_management.put_in_reg(0, l);
        gen_op(&self.cp_backend, r);
    }

    //--------------------------------------------------------------------------------
    // Control flow

    // TODO: Remember what the current stack_ptr and value array length was
    //       before calling the closures and restore it afterwards

    // TODO: Create a way to pass ValueRefs that will be used inside the closure
    //       So that they can then be passed as arguments to the closure and
    //       mutated directly there. This way we could omit the flush of those
    //       to the stack if they already are in a register.

    /// Generate an if statement. You cannot assign to variables outside the closure passed as then_branch
    /// this is by design because it prevents you from accidentially generating nonsensical code. 
    /// Use the `set` function instead.
    pub fn gen_if<E>(&self, mut condition: BoolRef, then_branch: impl Fn() -> Result<(), E>) -> Result<(), E> {
        let mut memory_management = self.memory_management.borrow_mut();
        let i = match &condition.0.inner {
            CGValueRefInner::Value(i) => *i,
            CGValueRefInner::Const(c) => {
                let new_i = memory_management.allocate_stack(DataType::Bool);
                memory_management.init(new_i, *c);
                condition.0.inner = CGValueRefInner::Value(new_i);
                new_i
            }
        };
        memory_management.put_in_reg(0, i);
        drop(memory_management);
        let mut llvm_state = self.llvm_state.borrow_mut();
        let previous_bb = llvm_state.current_block;
        let body_bb = llvm_state.new_basic_block();
        let after_bb = llvm_state.new_basic_block();
        let llvm_cond = llvm_state.get_current_llvm_value(i).into_int_value();
        llvm_state.builder.build_conditional_branch(llvm_cond, llvm_state.basic_blocks[body_bb], llvm_state.basic_blocks[after_bb]).unwrap();
        llvm_state.set_current_block(body_bb);
        drop(llvm_state);
        self.ctx.cp_backend.emit_if(|| {
            self.memory_management.borrow_mut().lose_reg(0);
            then_branch()
        })?;
        let after_body_bb = self.llvm_state.borrow().current_block;
        let mut llvm_state = self.llvm_state.borrow_mut();
        llvm_state.builder.build_unconditional_branch(llvm_state.basic_blocks[after_bb]).unwrap();
        llvm_state.set_current_block(previous_bb);
        llvm_state.set_current_block(after_bb);
        for i in llvm_state.value_set_in_block[&after_body_bb].clone().iter() {
            if let Some(before_value) = llvm_state.value_mapping.get(&previous_bb).unwrap().get(i).cloned() {
                let body_value = llvm_state.get_llvm_value(after_body_bb, *i);
                let phi = llvm_state.builder.build_phi(body_value.get_type(), &format!("phi_{}", i)).unwrap();
                phi.add_incoming(&[(&body_value, llvm_state.basic_blocks[after_body_bb]), (&before_value, llvm_state.basic_blocks[previous_bb])]);
                llvm_state.set_value(*i, phi.as_basic_value());
            }
        }
        let mut memory_management = self.memory_management.borrow_mut();
        memory_management.flush_regs();
        Ok(())
    }

    /// Generate an if else statement. You cannot assign to variables declared outside the closures passed as 
    /// then_branch and else_branch. This is by design because it prevents you from accidentially 
    /// generating nonsensical code. Use the `set` function instead.
    pub fn gen_if_else(&self, mut condition: BoolRef, then_branch: impl Fn(), else_branch: impl Fn()) {
        let mut memory_management = self.memory_management.borrow_mut();
        let i = match &condition.0.inner {
            CGValueRefInner::Value(i) => *i,
            CGValueRefInner::Const(c) => {
                let new_i = memory_management.allocate_stack(DataType::Bool);
                memory_management.init(new_i, *c);
                condition.0.inner = CGValueRefInner::Value(new_i);
                new_i
            }
        };
        memory_management.put_in_reg(0, i);
        drop(memory_management);
        let mut llvm_state = self.llvm_state.borrow_mut();
        let previous_bb = llvm_state.current_block;
        let then_body_bb = llvm_state.new_basic_block();
        let mut then_after_bb = then_body_bb;
        let else_body_bb = llvm_state.new_basic_block();
        let mut else_after_bb = else_body_bb;
        let after_bb = llvm_state.new_basic_block();
        let llvm_cond = llvm_state.get_current_llvm_value(i).into_int_value();
        llvm_state.builder.build_conditional_branch(llvm_cond, llvm_state.basic_blocks[then_body_bb], llvm_state.basic_blocks[else_body_bb]).unwrap();
        drop(llvm_state);
        self.ctx.cp_backend.emit_if_else(|| {
            self.llvm_state.borrow_mut().set_current_block(then_body_bb);
            self.memory_management.borrow_mut().lose_reg(0);
            then_branch();
            let mut memory_management = self.memory_management.borrow_mut();
            memory_management.flush_regs();
            let llvm_state = self.llvm_state.borrow();
            then_after_bb = llvm_state.current_block;
            llvm_state.builder.build_unconditional_branch(llvm_state.basic_blocks[after_bb]).unwrap();
        }, || {
            self.llvm_state.borrow_mut().set_current_block(else_body_bb);
            else_branch();
            let mut memory_management = self.memory_management.borrow_mut();
            memory_management.flush_regs();
            let llvm_state = self.llvm_state.borrow();
            else_after_bb = self.llvm_state.borrow().current_block;
            llvm_state.builder.build_unconditional_branch(llvm_state.basic_blocks[after_bb]).unwrap();
        });
        let mut llvm_state = self.llvm_state.borrow_mut();
        llvm_state.set_current_block(previous_bb);
        llvm_state.set_current_block(after_bb);
        // Calculate the set of values set in either of the two branches that have been present before the if else
        let mut values_set_in_either = BTreeSet::new();
        for i in llvm_state.value_set_in_block[&then_after_bb].iter().chain(llvm_state.value_set_in_block[&else_after_bb].iter()) {
            if llvm_state.value_mapping.contains_key(&previous_bb) && llvm_state.value_mapping[&previous_bb].contains_key(i) {
                values_set_in_either.insert(*i);
            }
        }
        // Generate phi nodes
        for value in values_set_in_either {
            let then_value = if llvm_state.value_set_in_block.contains_key(&then_after_bb) && llvm_state.value_set_in_block[&then_after_bb].contains(&value) {
                llvm_state.get_llvm_value(then_after_bb, value)
            } else {
                llvm_state.value_mapping[&previous_bb][&value].clone()
            };
            let else_value = if llvm_state.value_set_in_block.contains_key(&else_after_bb) && llvm_state.value_set_in_block[&else_after_bb].contains(&value) {
                llvm_state.get_llvm_value(else_after_bb, value)
            } else {
                llvm_state.value_mapping[&previous_bb][&value].clone()
            };
            let phi = llvm_state.builder.build_phi(then_value.get_type(), &format!("phi_{}", value)).unwrap();
            phi.add_incoming(&[(&then_value, llvm_state.basic_blocks[then_after_bb]), (&else_value, llvm_state.basic_blocks[else_after_bb])]);
            llvm_state.set_value(value, phi.as_basic_value());
        }
    }

    /// Generate a while loop. You cannot assign to variables declared outside the closure passed as
    /// condition. This is by design because it prevents you from accidentially generating nonsensical code.
    /// Use the `set` function instead.
    pub fn gen_while<'cg, E>(&self, condition: impl Fn() -> Result<BoolRef<'cg, 'ctx>, E>, body: impl Fn() -> Result<(), E>) -> Result<(), E> where 'ctx: 'cg {
        self.memory_management.borrow_mut().flush_regs();
        let mut llvm_state = self.llvm_state.borrow_mut();
        let previous_bb = llvm_state.current_block;
        let body_bb = llvm_state.new_basic_block();
        let mut after_body_bb = body_bb;
        let cond_bb = llvm_state.new_basic_block();
        let after_bb = llvm_state.new_basic_block();
        let mut after_cond_bb = cond_bb;
        // insert an unconditional branch from previous to cond
        llvm_state.builder.build_unconditional_branch(llvm_state.basic_blocks[cond_bb]).unwrap();
        llvm_state.set_current_block(cond_bb);
        let mut cond_phis = BTreeMap::new();
        // insert "empty" phi nodes 
        for i in llvm_state.value_mapping.get(&previous_bb).unwrap().keys().cloned().collect::<Vec<_>>() {
            let llvm_type = llvm_state.get_current_llvm_value(i).get_type();
            let phi = llvm_state.builder.build_phi(llvm_type, &format!("phi_{}", i)).unwrap();
            cond_phis.insert(i, phi);
            llvm_state.set_value(i, phi.as_basic_value());
        }
        drop(llvm_state);
        self.ctx.cp_backend.emit_loop( || {
            let res = condition()?;
            let i = match res.0.inner {
                CGValueRefInner::Value(i) => i,
                CGValueRefInner::Const(c) => {
                    let new_i = self.memory_management.borrow_mut().allocate_stack(DataType::Bool);
                    self.memory_management.borrow_mut().init(new_i, c);
                    new_i
                }
            };
            self.memory_management.borrow_mut().put_in_reg(0, i);
            let mut llvm_state = self.llvm_state.borrow_mut();
            llvm_state.builder.build_conditional_branch(llvm_state.get_current_llvm_value(i).into_int_value(), llvm_state.basic_blocks[body_bb], llvm_state.basic_blocks[after_bb]).unwrap();
            after_cond_bb = llvm_state.current_block;
            llvm_state.set_current_block(body_bb);
            Ok(())
        }, || {
            let body_out = body();
            after_body_bb = self.llvm_state.borrow().current_block;
            body_out
        })?;
        self.memory_management.borrow_mut().flush_regs();
        let mut llvm_state = self.llvm_state.borrow_mut();
        llvm_state.builder.build_unconditional_branch(llvm_state.basic_blocks[cond_bb]).unwrap();
        for (i, phi) in cond_phis {
            let before_value = llvm_state.value_mapping.get(&previous_bb).unwrap().get(&i).unwrap().clone();
            let body_value = if llvm_state.value_set_in_block.contains_key(&after_body_bb) && llvm_state.value_set_in_block[&after_body_bb].contains(&i) {
                llvm_state.value_set_in_block.entry(cond_bb).or_insert_with(|| BTreeSet::new()).insert(i);
                llvm_state.get_llvm_value(after_body_bb, i)
            } else {
                before_value.clone()
            };
            phi.add_incoming(&[(&body_value, llvm_state.basic_blocks[after_body_bb]), (&before_value, llvm_state.basic_blocks[previous_bb])]);
            llvm_state.set_value(i, phi.as_basic_value());
        }
        llvm_state.set_current_block(after_cond_bb); 
        llvm_state.set_current_block(after_bb);
        Ok(())
    }

    //--------------------------------------------------------------------------------
    // Other operations

    fn deref_ptr(&self, ptr_i: usize, target_i: usize) {
        let mut memory_management = self.memory_management.borrow_mut();
        let data_type = match &memory_management.values[target_i] {
            CGValue::Variable{data_type,..} => data_type.clone(),
            _ => unreachable!(),
        };
        memory_management.put_in_reg(0, ptr_i);
        self.ctx.cp_backend.emit_load(data_type);
        memory_management.reg_state[0] = Some((target_i, true));
        let mut llvm_state = self.llvm_state.borrow_mut();
        let llvm_ptr = llvm_state.get_current_llvm_value(ptr_i).into_pointer_value();
        let llvm_value = llvm_state.builder.build_load(data_type.get_llvm_type(&llvm_state.context), llvm_ptr, "load").unwrap();
        llvm_state.set_value(target_i, llvm_value);
    }

    fn get_ptr(&self, value_i: usize) -> UntypedPtrRef<'_, 'ctx>{
        let mut memory_management = self.memory_management.borrow_mut();
        let stack_pos = match &memory_management.values[value_i] {
            CGValue::Variable{stack_pos,..} => *stack_pos,
            _ => unreachable!(),
        };
        memory_management.free_reg(0);
        self.ctx.cp_backend.emit_get_stackptr(stack_pos);
        let i = memory_management.alloc_stack(8);
        memory_management.reg_state[0] = Some((i, true));
        UntypedPtrRef::new(i, self)
    }

    fn write_to_ptr(&self, ptr_i: usize, value: &CGValueRef) {
        let mut memory_management = self.memory_management.borrow_mut();
        let data_type = match &value.inner {
            CGValueRefInner::Value(i) => {
                match &memory_management.values[*i] {
                    CGValue::Variable{data_type,..} => data_type.clone(),
                    _ => unreachable!(),
                }
            },
            CGValueRefInner::Const(c) => c.get_type(),
        };
        match value.inner {
            CGValueRefInner::Value(i) => {
                memory_management.put_in_regs(i, ptr_i);
                self.ctx.cp_backend.emit_store(data_type);
            },
            CGValueRefInner::Const(c) => {
                memory_management.put_in_reg(1, ptr_i);
                self.ctx.cp_backend.emit_take_1_const(c);
                self.ctx.cp_backend.emit_store(data_type);
            }
        }
    }

    pub fn gen_return(&self, return_value: Option<CGValueRef>) {
        let llvm_return_val = if let Some(return_value) = return_value {
            let i = match return_value.inner {
                CGValueRefInner::Value(i) => i,
                CGValueRefInner::Const(c) => {
                    let new_i = self.memory_management.borrow_mut().allocate_stack(c.get_type());
                    self.memory_management.borrow_mut().init(new_i, c);
                    self.llvm_state.borrow_mut().init_value(new_i, c);
                    new_i
                }
            };
            self.memory_management.borrow_mut().put_in_reg(0, i);
            // TODO: bitcast to pointer here before returning for all other types
            self.llvm_state.borrow().get_current_llvm_value(return_value.inner.into_value_i().into()).into_pointer_value()
        } else { self.llvm_state.borrow().context.i8_type().ptr_type(AddressSpace::default()).get_undef() };
        self.ctx.cp_backend.emit_ret();
        self.llvm_state.borrow_mut().builder.build_return(Some(&llvm_return_val)).unwrap();
    }

    pub fn call_c_function(&self, func: CodegenCFunctionSignature, args_ptr: UntypedPtrRef) -> UntypedPtrRef<'_, 'ctx> {
        // Put args ptr into first register
        let mut memory_management = self.memory_management.borrow_mut();
        memory_management.put_in_reg(0, args_ptr.inner.into_value_i());
        // Allocate a stack region large enough to hold the input arguments
        self.ctx.cp_backend.emit_call_c_func(get_fn_ptr(func), 0);
        // Put first register into a new value
        memory_management.lose_reg(0);
        memory_management.lose_reg(1);
        drop(memory_management);
        let new_var = self.new_var(DataType::Ptr);
        self.memory_management.borrow_mut().reg_state[0] = Some((new_var.inner.into_value_i(), true));

        let llvm_i8_ptr_type = self.ctx.llvm_ctx.i8_type().ptr_type(inkwell::AddressSpace::default());
        let func_type = llvm_i8_ptr_type.fn_type(&[llvm_i8_ptr_type.into(), llvm_i8_ptr_type.into(), llvm_i8_ptr_type.into()], false);

        let const_fn_ptr_int = self.ctx.llvm_ctx.i64_type().const_int(get_fn_ptr(func) as u64, false);
        let const_fn_ptr = self.llvm_state.borrow().builder.build_int_to_ptr(const_fn_ptr_int, func_type.ptr_type(AddressSpace::default()), "const_fn_ptr").unwrap();

        let undef_value = llvm_i8_ptr_type.get_undef();

        // TODO: Handle constant arguments
        // Bitcast everything into a pointer value
        let arg_llvm_value = self.llvm_state.borrow().get_current_llvm_value(args_ptr.inner.into_value_i());
        let arg_llvm_value_as_ptr = match arg_llvm_value {
            BasicValueEnum::PointerValue(p) => p,
            BasicValueEnum::IntValue(i) => self.llvm_state.borrow_mut().builder.build_int_to_ptr(i, llvm_i8_ptr_type, "arg_llvm_value_as_ptr").unwrap(),
            _ => panic!("Unsupported arg type")
        };
        
        let call = self.llvm_state.borrow().builder.build_indirect_call(func_type, const_fn_ptr, &[undef_value.into(), arg_llvm_value_as_ptr.into(), undef_value.into()], "c_call").unwrap();
        call.set_call_convention(LLVMCallConv::LLVMCCallConv as u32);
        self.llvm_state.borrow_mut().set_value(new_var.inner.into_value_i(), call.try_as_basic_value().unwrap_left().into());

        new_var.into()
    }

    //--------------------------------------------------------------------------------
    // Code generation results

    pub fn finalize(&self) -> CodeGenResult<'ctx> {
        let code = self.ctx.cp_backend.generate_code(self.memory_management.borrow().stack_size);
        #[cfg(feature = "print-llvm")]
        self.llvm_state.borrow().module.print_to_stderr();
        let llvm_module = self.llvm_state.borrow().module.clone();
        CodeGenResult { code, llvm_module }
    }
}

