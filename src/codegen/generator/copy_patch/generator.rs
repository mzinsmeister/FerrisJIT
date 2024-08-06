struct RegisterManagement<'ctx> {
    // We save whether a register is potentially dirty
    // one value can only be in one register at a time unless it's a readonly/const value
    // as soon as a readonly/const value is dirtied it becomes a different mutable variable
    pub reg_state: [Option<(usize, bool)>; 2], 
    stack_ptr: usize, // TODO: Use actual byte sizes. For now we just use 8 bytes for everything
    pub stack_size: usize,
    cp_backend: &'ctx CopyPatchBackend,
}

impl<'ctx> RegisterManagement<'ctx> {
    /// Frees a register, saving its content to the stack if necessary
    pub fn free_reg(&mut self, reg: usize) {
        if let Some((i, dirty)) = &mut self.reg_state[reg] {
            if *dirty {
                let cur_value = &self.values[*i];
                match cur_value {
                    CGValue::Variable{readonly, stack_pos,..} => {
                        if *readonly {
                            panic!("We should have allocated a stack slot before dirtying a readonly value");
                        }
                        // Save it to its designated stack location
                        match reg {
                            0 => self.cp_backend.emit_put_1_stack(*stack_pos),
                            1 => self.cp_backend.emit_put_2_stack(*stack_pos),
                            _ => unreachable!(),
                        }
                        self.reg_state[reg].as_mut().unwrap().1 = false;
                    },
                    CGValue::Free => { /* User doesn't need this anymore so we can just throw it away without writing it back. */},
                }
            }
        }
    }

    pub fn flush_regs(&mut self) {
        for reg in 0..2 {
            self.lose_reg(reg);
        }
    }

    /// Put a single value into a single register. Use "put_in_regs" if you want to set both registers
    /// This might change the state of the other register!
    pub fn put_in_reg(&mut self, reg: usize, v: usize) {
                // Do we already have the correct value?
        if let Some((i, _)) = &self.reg_state[reg] {
            if *i == v {
                return;
            }
        }
        let value = &self.values[v];
        // Is our value in the other register? if so we can just swap the registers
        let other_reg_n = (reg + 1) % 2;
        let other_reg_state = self.reg_state[other_reg_n].clone();
        if let Some((i, _)) = other_reg_state {
            if i == v {
                if let CGValue::Variable{readonly, ..} = &value {
                    if *readonly {
                        // We can just duplicate the value
                        self.free_reg(reg);
                        match other_reg_n {
                            0 => self.cp_backend.emit_duplex1(),
                            1 => self.cp_backend.emit_duplex2(),
                            _ => unreachable!(),
                        }
                        self.reg_state[reg] = self.reg_state[other_reg_n].clone();
                        return;
                    }
                }
                // We just swap the registers so that we don't have to move anything
                self.cp_backend.emit_swap12();
                self.reg_state.swap(0, 1);
                return;
            }
        }
        // We have to go to memory, therefore spill the current value if necessary
        self.free_reg(reg);
        let value: &CGValue = &self.values[v];
        // Load the value to the register
        match value {
            CGValue::Variable{stack_pos, ..} => {
                // TODO: do take stack with datatype
                match reg {
                    0 => self.cp_backend.emit_take_1_stack(*stack_pos),
                    1 => self.cp_backend.emit_take_2_stack(*stack_pos),
                    _ => unreachable!(),
                }
            },
            CGValue::Free => unreachable!("We shouldn't even be able to have a reference to a free value"),
        }
        self.reg_state[reg] = Some((v, false));
    }

    // Get two values into registers in the most efficient way possible
    pub fn put_in_regs(&mut self, reg0_v: usize, reg1_v: usize) {
        let v0_reg_n = self.reg_state.iter()
            .enumerate()
            .find(|(_, r)| r.as_ref().map(|r| r.0 == reg0_v).unwrap_or(false))
            .map(|(i, _)| i);
        let v1_reg_n = self.reg_state.iter()
            .enumerate()
            .find(|(_, r)| r.as_ref().map(|r| r.0 == reg1_v).unwrap_or(false))
            .map(|(i, _)| i);

        // Depending on the different constellations we might have here we can do different things
        // Both values are already in the correct (!) registers
        if v0_reg_n == Some(0) && v1_reg_n == Some(1) {
            return;
        }
        // Both want the same value and it already is in a register
        if v0_reg_n.is_some() && v0_reg_n == v1_reg_n {
            // just duplicate the value
            if let Some(0) = v0_reg_n {
                self.free_reg(1);
                self.cp_backend.emit_duplex1();
                self.reg_state[1] = self.reg_state[0].clone();
            } else {
                self.free_reg(0);
                self.cp_backend.emit_duplex2();
                self.reg_state[0] = self.reg_state[1].clone();
            }
            return;
        }

        // registers are swapped
        if matches!(v0_reg_n, Some(1)) && matches!(v1_reg_n, Some(0)) {
            self.cp_backend.emit_swap12();
            self.reg_state.swap(0, 1);
            return;
        }

        if let Some(0) = v1_reg_n {
            // v2 is in register 0
            // Make sure to get that value into register 1 first
            self.put_in_reg(1, reg1_v);
            self.put_in_reg(0, reg0_v);
        } else {
            // In any other case this will be the most efficient way
            // Either v1 is in register 1 or we have to go to memory for both anyway
            self.put_in_reg(0, reg0_v);
            self.put_in_reg(1, reg1_v);
        }
    }

    pub fn dirty_reg(&mut self, reg: usize) {
        let reg_state = self.reg_state[reg].clone();
        if let Some((i, _)) = reg_state {
            // Check whether the value is readonly and if it is we allocate a new value
            self.reg_state[reg].as_mut().unwrap().1 = true;
            match &self.values[i] {
                CGValue::Variable{readonly,..} => {
                    if *readonly {
                        panic!("tried to dirty a readonly value");
                    }
                },
                _ => unreachable!(),
            }
        } else {
            panic!("tried to dirty a register that is not in use");
        }
    }

    pub fn lose_reg(&mut self, reg: usize) {
        self.free_reg(reg);
        self.reg_state[reg] = None;
    }

    pub fn init(&mut self, i: usize, c: ConstValue) {
        self.free_reg(0);
        self.cp_backend.emit_take_1_const(c);
        self.reg_state[0] = Some((i, true));
    }

    pub fn free_value(&mut self, v: usize) {
        let value = &self.values[v];
        match value {
            CGValue::Variable{readonly, stack_pos, data_type} => {
                if *readonly {
                    return;
                }
                self.free_slots.push(v);
                self.free_stack(*stack_pos, get_data_type_size(data_type));
                self.values[v] = CGValue::Free;
                // Check reg slots and free them if necessary
                for reg in self.reg_state.iter_mut() {
                    if let Some((i, _)) = reg {
                        if *i == v {
                            *reg = None;
                        }
                    }
                }
            }
            CGValue::Free => {/* TODO: Make sure double frees cannot happen, even internally */},
        }
    }
}

impl CodeGen {

    fn new(ctx: &'ctx CodeGenContext, arg_types: &[DataType]) -> Self {
        let memory_management = MemoryManagement::new(&ctx.cp_backend, arg_types);
        Self {
            ctx,
            memory_management: RefCell::new(memory_management),
            llvm_state: RefCell::new(LLVMState::new(&ctx.llvm_ctx, arg_types))
        }
    }

    pub fn get_arg(&self, n: usize) -> CGValueRef<'_, 'ctx> {
        // We immediately copy the arg to a new position so that we can handle it like any other value
        // The lazy approach is great but it doesn't work fctxor stuff like loops

        let mut memory_management = self.memory_management.borrow_mut();
        let i = memory_management.clone_value(n);
        self.llvm_state.borrow_mut().map_arg(n, i);
        CGValueRef::new(i, self, memory_management.values[n].get_type())

        //I64Ref(CGValueRef::new_readonly(n, self, DataType::I64))
    }

    fn new_var(&self, data_type: DataType) -> CGValueRef<'_, 'ctx> {
        let mut memory_management = self.memory_management.borrow_mut();
        let i = memory_management.allocate_stack(data_type);
        CGValueRef::new(i, self, data_type)
    }

    fn new_const(&self, c: ConstValue) -> CGValueRef<'_, 'ctx> {
        CGValueRef::new_const(c, self)
    }

    /// Create a new i64 constant. Note that `set` cannot be called on constants!
    pub fn new_i64_const(&self, n: i64) -> I64Ref<'_, 'ctx>{
        let c = self.new_const(ConstValue::I64(n));
        I64Ref(c)
    }

    /// Create a new i64 variable. If you don't need to change the value, use a constant instead.
    pub fn new_i64_var(&self, init: i64) -> I64Ref<'_, 'ctx>{
        let var = self.new_var(DataType::I64);
        let init_val = self.new_i64_const(init);
        self.copy_value(&init_val, &var);
        self.llvm_state.borrow_mut().init_value(var.inner.into_value_i(), ConstValue::I64(init));
        I64Ref(var)
    }

    /// Create a new bool constant. Note that `set` cannot be called on constants!
    pub fn new_bool_const(&self, b: bool) -> BoolRef<'_, 'ctx>{
        BoolRef(self.new_const(ConstValue::Bool(b)))
    }

    /// Create a new bool variable. If you don't need to change the value, use a constant instead.
    pub fn new_bool_var(&self, init: bool) -> BoolRef<'_, 'ctx>{
        let var = self.new_var(DataType::Bool);
        let init_val = self.new_bool_const(init);
        self.copy_value(&init_val, &var);
        self.llvm_state.borrow_mut().init_value(var.inner.into_value_i(), ConstValue::Bool(init));
        BoolRef(var)
    }


    fn load_const(&self, v: usize, c: ConstValue) {
        let mut memory_management = self.memory_management.borrow_mut();
        memory_management.init(v, c);
        self.llvm_state.borrow_mut().init_value(v, c);
    }

    fn free_value(&self, v: &CGValueRef) {
        match v.inner {
            CGValueRefInner::Value(i) => {
                self.memory_management.borrow_mut().free_value(i);
                self.llvm_state.borrow_mut().free_value(i);
            },
            CGValueRefInner::Const(_) => {/* We don't have to do anything here */}
        }
    }

    fn clone_value(&self, v: &CGValueRef<'_, 'ctx>) -> CGValueRef<'_, 'ctx>{
        match v.inner {
            CGValueRefInner::Const(c) => {
                CGValueRef::new_const(c, self)
            },
            CGValueRefInner::Value(i) => {
                let mut memory_management = self.memory_management.borrow_mut();
                let new = memory_management.clone_value(i);
                self.llvm_state.borrow_mut().copy_value(i, new);
                CGValueRef::new(new, self, v.data_type.clone())
            }
        }
    }


    fn copy_value(&self, src: &CGValueRef, dest: &CGValueRef) {
        let (dest_i, dest_ptr) = match dest.inner {
            CGValueRefInner::Value(i) => {
                let memory_management = self.memory_management.borrow();
                let dest_ptr = match &memory_management.values[i] {
                    CGValue::Variable{stack_pos,..} => *stack_pos,
                    _ => unreachable!(),
                };
                (i, dest_ptr)
            },
            CGValueRefInner::Const(_) => {
                // TODO: Can we encode this in the type system so that stuff like this is not possible?
                panic!("Cannot copy to a constant");
            }
        };
        match src.inner {
            CGValueRefInner::Value(i) => {
                let mut memory_management = self.memory_management.borrow_mut();
                memory_management.put_in_reg(0, i);
                self.ctx.cp_backend.emit_put_1_stack(dest_ptr);
                memory_management.put_in_reg(1, dest_i);
                self.llvm_state.borrow_mut().copy_value(i, dest_i);
            },
            CGValueRefInner::Const(c) => {
               self.memory_management.borrow_mut().init(dest_i, c);
                self.llvm_state.borrow_mut().init_value(dest_i, c);
            }

        }
    
    }

    //--------------------------------------------------------------------------------
    // Arithmetic operations
    fn gen_arith<const COMMUTATIVE: bool, const RETURNS_BOOL: bool, LLVMF>(&self,  gen_op: fn(&CopyPatchBackend, DataType), gen_op_const: fn(&CopyPatchBackend, ConstValue), gen_llvm: LLVMF, l: &mut CGValueRef, r: &CGValueRef) 
            where LLVMF: Fn(&LLVMState<'ctx>, DataType, BasicValueEnum<'ctx>, BasicValueEnum<'ctx>) -> BasicValueEnum<'ctx> {

        let mut memory_management = self.memory_management.borrow_mut();
        match (l.inner, r.inner) {
            (CGValueRefInner::Value(li), CGValueRefInner::Value(ri)) => {
                memory_management.put_in_regs(li, ri);
                gen_op(&self.ctx.cp_backend, l.data_type.clone());
                let llvm_l = self.llvm_state.borrow().get_current_llvm_value(li);
                let llvm_r = self.llvm_state.borrow().get_current_llvm_value(ri);
                let new_llvm_l = gen_llvm(&self.llvm_state.borrow(), l.data_type, llvm_l, llvm_r);
                self.llvm_state.borrow_mut().set_value(li, new_llvm_l);
            },
            (CGValueRefInner::Value(li), CGValueRefInner::Const(c)) => {
                let vl = memory_management.values[li].clone();
                match vl {
                    CGValue::Variable{..} => {
                        memory_management.put_in_reg(0, li);
                        gen_op_const(&self.ctx.cp_backend, c);
                        let llvm_l = self.llvm_state.borrow().get_current_llvm_value(li);
                        let llvm_r = self.llvm_state.borrow().get_llvm_const(c);
                        let new_llvm_l = gen_llvm(&self.llvm_state.borrow(), l.data_type, llvm_l, llvm_r);
                        self.llvm_state.borrow_mut().set_value(li, new_llvm_l);
                    },
                    CGValue::Free => unreachable!("We shouldn't even be able to have a reference to a free value"),
                }
            },
            (CGValueRefInner::Const(c), CGValueRefInner::Value(ri)) => {
                let new_li = memory_management.allocate_stack(c.get_type());
                if COMMUTATIVE {
                    memory_management.put_in_reg(0, ri);
                    gen_op_const(&self.ctx.cp_backend, c);
                    memory_management.reg_state[0] = Some((new_li, true));
                } else {
                    memory_management.init(new_li, c);
                    memory_management.put_in_regs(new_li, ri);
                    gen_op(&self.ctx.cp_backend, c.get_type());
                    l.inner = CGValueRefInner::Value(new_li);
                }
                let llvm_l = self.llvm_state.borrow().get_llvm_const(c);
                let llvm_r = self.llvm_state.borrow().get_current_llvm_value(ri);
                let new_llvm_l = gen_llvm(&self.llvm_state.borrow(), l.data_type, llvm_l, llvm_r);
                self.llvm_state.borrow_mut().set_value(new_li, new_llvm_l);
            },
            (CGValueRefInner::Const(c1), CGValueRefInner::Const(c2)) => {
                let new_l = memory_management.allocate_stack(c1.get_type());
                memory_management.init(new_l, c1);
                memory_management.put_in_reg(0, new_l);
                gen_op_const(&self.ctx.cp_backend, c2);
                l.inner = CGValueRefInner::Value(new_l);
                let llvm_l = self.llvm_state.borrow().get_llvm_const(c1);
                let llvm_r = self.llvm_state.borrow().get_llvm_const(c2);
                let new_llvm_l = gen_llvm(&self.llvm_state.borrow(), l.data_type, llvm_l, llvm_r);
                self.llvm_state.borrow_mut().set_value(new_l, new_llvm_l);
            }
        }
        memory_management.dirty_reg(0);
        if RETURNS_BOOL {
            l.data_type = DataType::Bool;
        }
    }

    fn add(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true, false, _>(CopyPatchBackend::emit_add,CopyPatchBackend::emit_add_const, |ls, t, l, r| llvm::int_add(&ls.builder, t, l.into_int_value(), r.into_int_value()).into(), l, r)
    }

    fn sub(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false, false, _>(CopyPatchBackend::emit_sub,CopyPatchBackend::emit_sub_const,  |ls, t, l, r| llvm::int_sub(&ls.builder, t, l.into_int_value(), r.into_int_value()).into(), l , r)
    }

    fn mul(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true, false, _>(CopyPatchBackend::emit_mul,CopyPatchBackend::emit_mul_const,  |ls, t, l, r| llvm::int_mul(&ls.builder, t, l.into_int_value(), r.into_int_value()).into(), l, r)
    }

    fn div(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false, false, _>(CopyPatchBackend::emit_div,CopyPatchBackend::emit_div_const,  |ls, t, l, r| llvm::int_div(&ls.builder, t, l.into_int_value(), r.into_int_value()).into(), l, r)
    }

    fn rem(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false, false, _>(CopyPatchBackend::emit_rem,CopyPatchBackend::emit_rem_const,  |ls, t, l, r| llvm::int_rem(&ls.builder, t, l.into_int_value(), r.into_int_value()).into(), l, r)
    }

    fn eq(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true, true, _>(CopyPatchBackend::emit_eq,CopyPatchBackend::emit_eq_const,  |ls, t, l, r| llvm::int_eq(&ls.builder, t, l.into_int_value(), r.into_int_value()).into(), l, r)
    }

    fn neq(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true, true, _>(CopyPatchBackend::emit_neq,CopyPatchBackend::emit_neq_const,  |ls, t, l, r| llvm::int_ne(&ls.builder, t, l.into_int_value(), r.into_int_value()).into(), l, r)
    }

    // TODO: exchange lt/gte and lte/gt here if the first one is a const
    fn lt(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false, true, _>(CopyPatchBackend::emit_lt,CopyPatchBackend::emit_lt_const,  |ls, t, l, r| llvm::int_lt(&ls.builder, t, l.into_int_value(), r.into_int_value()).into(), l, r)
    }

    fn lte(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false, true, _>(CopyPatchBackend::emit_lte,CopyPatchBackend::emit_lte_const,  |ls, t, l, r| llvm::int_lte(&ls.builder, t, l.into_int_value(), r.into_int_value()).into(), l, r)
    }

    fn gt(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false, true, _>(CopyPatchBackend::emit_gt,CopyPatchBackend::emit_gt_const,  |ls, t, l, r| llvm::int_gt(&ls.builder, t, l.into_int_value(), r.into_int_value()).into(), l, r)
    }

    fn gte(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false, true, _>(CopyPatchBackend::emit_gte,CopyPatchBackend::emit_gte_const, |ls, t, l, r| llvm::int_gte(&ls.builder, t, l.into_int_value(), r.into_int_value()).into(), l, r)
    }

    fn and(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true, true, _>(CopyPatchBackend::emit_and,CopyPatchBackend::emit_and_const, |ls, t, l, r| llvm::int_and(&ls.builder, t, l.into_int_value(), r.into_int_value()).into(), l, r)
    }

    fn or(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true, true, _>(CopyPatchBackend::emit_or,CopyPatchBackend::emit_or_const, |ls, t, l, r| llvm::int_or(&ls.builder, t, l.into_int_value(), r.into_int_value()).into(), l, r)
    }

    fn gep(&self, pointee_type: DataType, ptr: &mut CGValueRef, index: &CGValueRef) {
        self.gen_arith::<true,true, _>(CopyPatchBackend::emit_add, CopyPatchBackend::emit_add_const, |ls, _, l, r| {
            let l = l.into_pointer_value();
            let r = r.into_int_value();
            let gep = unsafe { ls.builder.build_gep(pointee_type.get_llvm_type(ls.context), l, &[r], "gep") }.unwrap();
            gep.into()
        }, ptr, index)
    }

    fn not(&self, l: &mut CGValueRef) {
        let mut memory_management = self.memory_management.borrow_mut();
        match l.inner {
            CGValueRefInner::Value(i) => {
                memory_management.put_in_reg(0, i);
                self.ctx.cp_backend.emit_not(l.data_type.clone());
                memory_management.dirty_reg(0);
            },
            CGValueRefInner::Const(c) => {
                l.inner = CGValueRefInner::Const(c.bit_not());
            }
        }
        let llvm_val = self.llvm_state.borrow().get_current_llvm_value(l.inner.into_value_i()).into_int_value();
        let new_llvm_val = llvm::int_not(&self.llvm_state.borrow().builder, llvm_val);
        self.llvm_state.borrow_mut().set_value(l.inner.into_value_i(), new_llvm_val.into());
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

