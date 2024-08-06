use std::collections::{BTreeMap, BTreeSet};

use inkwell::{context::Context, values::BasicValueEnum, AddressSpace};

use crate::codegen::{generator::Generator, get_data_type_size, ir::{ConstValue, DataType}};

struct LLVMState<'ctx> {
    context: &'ctx Context,
    builder: inkwell::builder::Builder<'ctx>,
    module: inkwell::module::Module<'ctx>,
    function: inkwell::values::FunctionValue<'ctx>,
    current_block: usize,
    // TODO: Maybe use rpds or some other persistent data structure crate to 
    //       Get Copy on write data structures instead of having to clone the maps all the time
    current_value_mapping: BTreeMap<usize, BasicValueEnum<'ctx>>,
    // Mapping from (basic block, value) to llvm value
    // Control flow operations need to make sure to merge values that were sets inside the blocks they
    // created back with a phi block
    value_mapping: BTreeMap<usize, BTreeMap<usize, BasicValueEnum<'ctx>>>,
    value_set_in_block: BTreeMap<usize, BTreeSet<usize>>,
    basic_blocks: Vec<inkwell::basic_block::BasicBlock<'ctx>>,
}

impl<'ctx> LLVMState<'ctx> {
    fn new(context: &'ctx Context, arg_types: &[DataType]) -> Self {
        let module = context.create_module("main");
        let i8_type = context.i8_type();
        let i8_ptr = context.i8_type().ptr_type(inkwell::AddressSpace::default());
        let llvm_arg_types = arg_types.iter().map(|t| t.get_llvm_type(context).into()).collect::<Vec<_>>();
        let function = module.add_function("main", i8_ptr.fn_type( &[i8_ptr.into()], false), None);
        let entry_block = context.append_basic_block(function, "entry");
        let builder = context.create_builder();
        builder.position_at_end(entry_block);
        let inner_function = module.add_function("inner", i8_ptr.fn_type( &llvm_arg_types, false), None);
        // Unpack the arguments with the given types from the pointer given 
        let mut inner_args = vec![];
        let mut offset = 0;
        let args_ptr = function.get_nth_param(0).unwrap().into_pointer_value();
        for (i, arg_type) in arg_types.iter().enumerate() {
            let llvm_arg_type = arg_type.get_llvm_type(context);
            let offset_llvm_value = context.i64_type().const_int(offset as u64, false);
            let arg_ptr = unsafe { builder.build_in_bounds_gep(i8_type, args_ptr, &[offset_llvm_value.into()], "arg_ptr") }.unwrap();
            let value = builder.build_load(arg_type.get_llvm_type(context), arg_ptr, &format!("val{}", i)).unwrap();
            let casted_value = builder.build_bitcast(value, llvm_arg_type, &format!("casted_val{}", i)).unwrap();
            inner_args.push(casted_value.into());
            offset += get_data_type_size(arg_type) as u32;
        }
        let inner_call = builder.build_call(inner_function, &inner_args, "inner_call").unwrap();
        inner_call.set_tail_call(true);
        let ret_val = inner_call.try_as_basic_value().left().unwrap();
        builder.build_return(Some(&ret_val)).unwrap();
        let inner_entry = context.append_basic_block(inner_function, "entry");
        builder.position_at_end(inner_entry);
        Self {
            context,
            builder,
            module,
            function: inner_function,
            current_block: 1,
            current_value_mapping: BTreeMap::new(),
            value_mapping: BTreeMap::new(),
            value_set_in_block: BTreeMap::new(),
            basic_blocks: vec![entry_block, inner_entry],
        }
    }

    fn map_arg(&mut self, arg_i: usize, i: usize) {
        let arg = self.function.get_nth_param(arg_i as u32).unwrap();
        self.current_value_mapping.insert(i, arg.into());
    }

    fn get_llvm_value(&self, basic_block: usize, i: usize) -> BasicValueEnum<'ctx> {
        if basic_block == self.current_block {
            self.current_value_mapping.get(&i).unwrap().clone()
        } else {
            self.value_mapping.get(&basic_block).unwrap().get(&i).unwrap().clone()
        }
    }

    fn get_current_llvm_value(&self, i: usize) -> BasicValueEnum<'ctx> {
        self.current_value_mapping.get(&i).unwrap().clone()
    }

    fn get_llvm_const(&self, const_val: ConstValue) -> BasicValueEnum<'ctx> {
        let data_type = const_val.get_type();
        let llvm_type = data_type.get_llvm_type(self.context);
        match const_val {
            ConstValue::I64(v) => llvm_type.into_int_type().const_int(v as u64, true).into(),
            ConstValue::I32(v) => llvm_type.into_int_type().const_int(v as u64, true).into(),
            ConstValue::I16(v) => llvm_type.into_int_type().const_int(v as u64, true).into(),
            ConstValue::I8(v) => llvm_type.into_int_type().const_int(v as u64, true).into(),
            ConstValue::U64(v) => llvm_type.into_int_type().const_int(v, false).into(),
            ConstValue::U32(v) => llvm_type.into_int_type().const_int(v as u64, false).into(),
            ConstValue::U16(v) => llvm_type.into_int_type().const_int(v as u64, false).into(),
            ConstValue::U8(v) => llvm_type.into_int_type().const_int(v as u64, false).into(),
            ConstValue::Bool(v) => llvm_type.into_int_type().const_int(v as u64, false).into(),
            ConstValue::F64(v) => llvm_type.into_float_type().const_float(v).into(),
            _ => unimplemented!()
        }
    }

    fn init_value(&mut self, i: usize, init_value: ConstValue) {
        let data_type = init_value.get_type();
        let llvm_type = data_type.get_llvm_type(self.context);
        let llvm_value = match init_value {
            ConstValue::I64(_) | ConstValue::I32(_) | ConstValue::I16(_) | ConstValue::I8(_) => 
                llvm_type.into_int_type().const_int(init_value.bitcast_to_u64(), true).into(),
            ConstValue::U64(_) | ConstValue::U32(_) | ConstValue::U16(_) | ConstValue::U8(_) | ConstValue::Bool(_) => 
                llvm_type.into_int_type().const_int(init_value.bitcast_to_u64(), false).into(),
            ConstValue::F64(v) => llvm_type.into_float_type().const_float(v).into(),
            _ => unimplemented!()
        };
        self.current_value_mapping.insert(i, llvm_value);
    }

    fn copy_value(&mut self, src_i: usize, dest_i: usize) {
        let src = self.get_current_llvm_value(src_i);
        self.current_value_mapping.insert(dest_i, src);
        self.value_set_in_block.entry(self.current_block).or_insert_with(|| BTreeSet::new()).insert(dest_i);
    }

    fn set_value(&mut self, i: usize, value: BasicValueEnum<'ctx>) {
        self.current_value_mapping.insert(i, value);
        self.value_set_in_block.entry(self.current_block).or_insert_with(|| BTreeSet::new()).insert(i);
    }

    /// Creates a new basic_block
    fn new_basic_block(&mut self) -> usize {
        let id = self.basic_blocks.len();
        let block = self.context.append_basic_block(self.function, &format!("block{}", id));
        self.basic_blocks.push(block);
        id
    }

    /// It sets the current block to the given block while assuming we branched to it from the current block
    fn set_current_block(&mut self, block: usize) {
        // TODO: Change this behaviour to require a specific "init" before 
        let next_block_mapping = if let Some(v) = self.value_mapping.remove_entry(&block) {
            v.1
        } else {
            self.current_value_mapping.clone()
        };
        let old_block_mapping = std::mem::replace(&mut self.current_value_mapping, next_block_mapping);
        self.value_mapping.insert(self.current_block, old_block_mapping);
        self.current_block = block;
        self.builder.position_at_end(self.basic_blocks[block]);
    }

    fn free_value(&mut self, i: usize) {
        self.current_value_mapping.remove(&i);
        self.value_set_in_block.get_mut(&self.current_block).map(|s| s.remove(&i));
    }
}

struct LLVMGenerator<'ctx> {
    state: LLVMState<'ctx>,
    bb_stack: Vec<usize>,
    loop_phis_stack: Vec<BTreeMap<usize, inkwell::values::PhiValue<'ctx>>>,
}

impl<'ctx> Generator for LLVMGenerator<'ctx> {
    fn init(&mut self) {
    }
    
    fn map_arg(&mut self, arg_n: usize, n: usize) {
        self.state.map_arg(arg_n, n);
    }
    
    fn new_var(&mut self, n: usize) {
    }
    
    fn new_const(&mut self, c: ConstValue, n: usize) {
        self.state.init_value(n, c);
    }
    
    fn init_var(&mut self, c: ConstValue, n: usize) {
        self.state.init_value(n, c);
    }
    
    fn free_value(&mut self, n: usize) {
        self.state.free_value(n);
    }
    
    fn copy_value(&mut self, n: usize, m: usize) {
        self.state.copy_value(n, m);
    }
    
    fn add(&mut self, n: usize, m: usize) {
        let n_val = self.state.get_current_llvm_value(n);
        let m_val = self.state.get_current_llvm_value(m);
        let result = self.state.builder.build_int_add(n_val.into_int_value(), m_val.into_int_value(), "add").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn add_const(&mut self, n: usize, c: ConstValue) {
        let n_val = self.state.get_current_llvm_value(n);
        let c_val = self.state.get_llvm_const(c);
        let result = self.state.builder.build_int_add(n_val.into_int_value(), c_val.into_int_value(), "add").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn sub(&mut self, n: usize, m: usize) {
        let n_val = self.state.get_current_llvm_value(n);
        let m_val = self.state.get_current_llvm_value(m);
        let result = self.state.builder.build_int_sub(n_val.into_int_value(), m_val.into_int_value(), "sub").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn sub_const(&mut self, n: usize, c: ConstValue) {
        let n_val = self.state.get_current_llvm_value(n);
        let c_val = self.state.get_llvm_const(c);
        let result = self.state.builder.build_int_sub(n_val.into_int_value(), c_val.into_int_value(), "sub").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn mul(&mut self, n: usize, m: usize) {
        let n_val = self.state.get_current_llvm_value(n);
        let m_val = self.state.get_current_llvm_value(m);
        let result = self.state.builder.build_int_mul(n_val.into_int_value(), m_val.into_int_value(), "mul").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn mul_const(&mut self, n: usize, c: ConstValue) {
        let n_val = self.state.get_current_llvm_value(n);
        let c_val = self.state.get_llvm_const(c);
        let result = self.state.builder.build_int_mul(n_val.into_int_value(), c_val.into_int_value(), "mul").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn div(&mut self, n: usize, m: usize) {
        let n_val = self.state.get_current_llvm_value(n);
        let m_val = self.state.get_current_llvm_value(m);
        let result = self.state.builder.build_int_signed_div(n_val.into_int_value(), m_val.into_int_value(), "div").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn div_const(&mut self, n: usize, c: ConstValue) {
        let n_val = self.state.get_current_llvm_value(n);
        let c_val = self.state.get_llvm_const(c);
        let result = self.state.builder.build_int_signed_div(n_val.into_int_value(), c_val.into_int_value(), "div").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn rem(&mut self, n: usize, m: usize) {
        let n_val = self.state.get_current_llvm_value(n);
        let m_val = self.state.get_current_llvm_value(m);
        let result = self.state.builder.build_int_signed_rem(n_val.into_int_value(), m_val.into_int_value(), "rem").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn rem_const(&mut self, n: usize, c: ConstValue) {
        let n_val = self.state.get_current_llvm_value(n);
        let c_val = self.state.get_llvm_const(c);
        let result = self.state.builder.build_int_signed_rem(n_val.into_int_value(), c_val.into_int_value(), "rem").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn eq(&mut self, n: usize, m: usize) {
        let n_val = self.state.get_current_llvm_value(n);
        let m_val = self.state.get_current_llvm_value(m);
        let result = self.state.builder.build_int_compare(inkwell::IntPredicate::EQ, n_val.into_int_value(), m_val.into_int_value(), "eq").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn neq(&mut self, n: usize, m: usize) {
        let n_val = self.state.get_current_llvm_value(n);
        let m_val = self.state.get_current_llvm_value(m);
        let result = self.state.builder.build_int_compare(inkwell::IntPredicate::NE, n_val.into_int_value(), m_val.into_int_value(), "ne").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn lt(&mut self, n: usize, m: usize) {
        let n_val = self.state.get_current_llvm_value(n);
        let m_val = self.state.get_current_llvm_value(m);
        let result = self.state.builder.build_int_compare(inkwell::IntPredicate::SLT, n_val.into_int_value(), m_val.into_int_value(), "slt").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn lte(&mut self, n: usize, m: usize) {
        let n_val = self.state.get_current_llvm_value(n);
        let m_val = self.state.get_current_llvm_value(m);
        let result = self.state.builder.build_int_compare(inkwell::IntPredicate::SLE, n_val.into_int_value(), m_val.into_int_value(), "sle").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn gt(&mut self, n: usize, m: usize) {
        let n_val = self.state.get_current_llvm_value(n);
        let m_val = self.state.get_current_llvm_value(m);
        let result = self.state.builder.build_int_compare(inkwell::IntPredicate::SGT, n_val.into_int_value(), m_val.into_int_value(), "sgt").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn gte(&mut self, n: usize, m: usize) {
        let n_val = self.state.get_current_llvm_value(n);
        let m_val = self.state.get_current_llvm_value(m);
        let result = self.state.builder.build_int_compare(inkwell::IntPredicate::SGE, n_val.into_int_value(), m_val.into_int_value(), "sge").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn and(&mut self, n: usize, m: usize) {
        let n_val = self.state.get_current_llvm_value(n);
        let m_val = self.state.get_current_llvm_value(m);
        let result = self.state.builder.build_and(n_val.into_int_value(), m_val.into_int_value(), "and").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn or(&mut self, n: usize, m: usize) {
        let n_val = self.state.get_current_llvm_value(n);
        let m_val = self.state.get_current_llvm_value(m);
        let result = self.state.builder.build_or(n_val.into_int_value(), m_val.into_int_value(), "or").unwrap();
        self.state.set_value(n, result.into());
    }

    fn not(&mut self, n: usize) {
        let n_val = self.state.get_current_llvm_value(n);
        let result = self.state.builder.build_not(n_val.into_int_value(), "not").unwrap();
        self.state.set_value(n, result.into());
    }
    
    fn gep(&mut self, pointee_type: DataType, ptr: usize, idx: usize) {
        let ptr_val = self.state.get_current_llvm_value(ptr);
        let idx_val = self.state.get_current_llvm_value(idx);
        let result = unsafe { self.state.builder.build_gep(pointee_type.get_llvm_ptr_type(&self.state.context), ptr_val.into_pointer_value(), &[idx_val.into_int_value()], "gep").unwrap() };
        self.state.set_value(ptr, result.into());
    }
    
    fn deref_ptr(&mut self, pointee_type: DataType, ptr_i: usize, target_i: usize) {
        let llvm_ptr = self.state.get_current_llvm_value(ptr_i).into_pointer_value();
        let llvm_value = self.state.builder.build_load(pointee_type.get_llvm_type(&self.state.context), llvm_ptr, "load").unwrap();
        self.state.set_value(target_i, llvm_value);
    }
    
    fn write_to_ptr(&mut self, ptr_i: usize, value_i: usize) {
        let llvm_ptr = self.state.get_current_llvm_value(ptr_i).into_pointer_value();
        let llvm_value = self.state.get_current_llvm_value(value_i);
        self.state.builder.build_store(llvm_ptr, llvm_value).unwrap();
    }
    
    fn ret(&mut self, n: Option<usize>) {
        let llvm_return_val = if let Some(return_value) = n {
            // TODO: bitcast to pointer here before returning for all other types
            self.state.get_current_llvm_value(return_value).into_pointer_value()
        } else { self.state.context.i8_type().ptr_type(AddressSpace::default()).get_undef() };
        self.state.builder.build_return(Some(&llvm_return_val)).unwrap();
    }
    
    fn c_call(&mut self, func: *const std::os::raw::c_void, args_ptr: usize) {
        let llvm_i8_ptr_type = self.state.context.i8_type().ptr_type(inkwell::AddressSpace::default());
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
    }
    
    fn pre_if_then(&mut self, condition: usize) {
        let previous_bb = self.state.current_block;
        let body_bb = self.state.new_basic_block();
        let after_bb = self.state.new_basic_block();
        self.bb_stack.push(previous_bb);
        self.bb_stack.push(after_bb);
        let llvm_cond = self.state.get_current_llvm_value(condition).into_int_value();
        self.state.builder.build_conditional_branch(llvm_cond, self.state.basic_blocks[body_bb], self.state.basic_blocks[after_bb]).unwrap();
        self.state.set_current_block(body_bb);
    }
    
    fn post_if_then(&mut self) {
        let previous_bb = self.bb_stack.pop().unwrap();
        let after_bb = self.bb_stack.pop().unwrap();
        let after_body_bb = self.state.current_block;
        self.state.builder.build_unconditional_branch(self.state.basic_blocks[after_bb]).unwrap();
        self.state.set_current_block(previous_bb);
        self.state.set_current_block(after_bb);
        for i in self.state.value_set_in_block[&after_body_bb].clone().iter() {
            if let Some(before_value) = self.state.value_mapping.get(&previous_bb).unwrap().get(i).cloned() {
                let body_value = self.state.get_llvm_value(after_body_bb, *i);
                let phi = self.state.builder.build_phi(body_value.get_type(), &format!("phi_{}", i)).unwrap();
                phi.add_incoming(&[(&body_value, self.state.basic_blocks[after_body_bb]), (&before_value, self.state.basic_blocks[previous_bb])]);
                self.state.set_value(*i, phi.as_basic_value());
            }
        }
    }
    
    fn pre_if(&mut self, condition: usize) {
        let previous_bb = self.state.current_block;
        let then_body_bb = self.state.new_basic_block();
        let else_body_bb = self.state.new_basic_block();
        let after_bb = self.state.new_basic_block();
        self.bb_stack.push(previous_bb);
        self.bb_stack.push(after_bb);
        self.bb_stack.push(then_body_bb);
        self.bb_stack.push(else_body_bb);
        let llvm_cond = self.state.get_current_llvm_value(condition).into_int_value();
        self.state.builder.build_conditional_branch(llvm_cond, self.state.basic_blocks[then_body_bb], self.state.basic_blocks[else_body_bb]).unwrap();
    }
    
    fn pre_then(&mut self) {
        let then_bb = self.bb_stack[self.bb_stack.len() - 2];
        self.state.set_current_block(then_bb);
    }
    
    fn post_then(&mut self) {
        let then_index = self.bb_stack.len() - 2;
        self.bb_stack[then_index] = self.state.current_block;
        let after_bb = self.bb_stack[self.bb_stack.len() - 5];
        self.state.builder.build_unconditional_branch(self.state.basic_blocks[after_bb]).unwrap();
    }
    
    fn pre_else(&mut self) {
        let else_bb = self.bb_stack[self.bb_stack.len() - 1];
        self.state.set_current_block(else_bb);
    }
    
    fn post_else(&mut self) {
        let else_index = self.bb_stack.len() - 1;
        self.bb_stack[else_index] = self.state.current_block;
        let after_bb = self.bb_stack[self.bb_stack.len() - 5];
        self.state.builder.build_unconditional_branch(self.state.basic_blocks[after_bb]).unwrap();
    }
    
    fn post_if(&mut self) {
        let else_after_bb = self.bb_stack.pop().unwrap();
        let then_after_bb = self.bb_stack.pop().unwrap();
        let previous_bb = self.bb_stack.pop().unwrap();
        let after_bb = self.bb_stack.pop().unwrap();
        self.state.set_current_block(previous_bb);
        self.state.set_current_block(after_bb);
        // Calculate the set of values set in either of the two branches that have been present before the if else
        let mut values_set_in_either = BTreeSet::new();
        for i in self.state.value_set_in_block[&then_after_bb].iter().chain(self.state.value_set_in_block[&else_after_bb].iter()) {
            if self.state.value_mapping.contains_key(&previous_bb) && self.state.value_mapping[&previous_bb].contains_key(i) {
                values_set_in_either.insert(*i);
            }
        }
        // Generate phi nodes
        for value in values_set_in_either {
            let then_value = if self.state.value_set_in_block.contains_key(&then_after_bb) && self.state.value_set_in_block[&then_after_bb].contains(&value) {
                self.state.get_llvm_value(then_after_bb, value)
            } else {
                self.state.value_mapping[&previous_bb][&value].clone()
            };
            let else_value = if self.state.value_set_in_block.contains_key(&else_after_bb) && self.state.value_set_in_block[&else_after_bb].contains(&value) {
                self.state.get_llvm_value(else_after_bb, value)
            } else {
                self.state.value_mapping[&previous_bb][&value].clone()
            };
            let phi = self.state.builder.build_phi(then_value.get_type(), &format!("phi_{}", value)).unwrap();
            phi.add_incoming(&[(&then_value, self.state.basic_blocks[then_after_bb]), (&else_value, self.state.basic_blocks[else_after_bb])]);
            self.state.set_value(value, phi.as_basic_value());
        }
    }
    
    fn pre_loop_cond(&mut self) {
        let previous_bb = self.state.current_block;
        let body_bb = self.state.new_basic_block();
        let cond_bb = self.state.new_basic_block();
        let after_bb = self.state.new_basic_block();
        self.bb_stack.push(previous_bb);
        self.bb_stack.push(after_bb);
        self.bb_stack.push(body_bb);
        // insert an unconditional branch from previous to cond
        self.state.builder.build_unconditional_branch(self.state.basic_blocks[cond_bb]).unwrap();
        self.state.set_current_block(cond_bb);
        let mut cond_phis = BTreeMap::new();
        // insert "empty" phi nodes 
        for i in self.state.value_mapping.get(&previous_bb).unwrap().keys().cloned().collect::<Vec<_>>() {
            let llvm_type = self.state.get_current_llvm_value(i).get_type();
            let phi = self.state.builder.build_phi(llvm_type, &format!("phi_{}", i)).unwrap();
            cond_phis.insert(i, phi);
            self.state.set_value(i, phi.as_basic_value());
        }
    }
    
    fn pre_loop_body(&mut self, condition: usize) {
        let body_bb = self.bb_stack.pop().unwrap();
        let after_bb = self.bb_stack[self.bb_stack.len() - 2];
        self.state.builder.build_conditional_branch(self.state.get_current_llvm_value(condition).into_int_value(), self.state.basic_blocks[body_bb], self.state.basic_blocks[after_bb]).unwrap();
        self.bb_stack.push(self.state.current_block);
        self.state.set_current_block(body_bb);
    }
    
    fn post_loop_body(&mut self) {
        let after_body_bb = self.state.current_block;
        self.state.builder.build_unconditional_branch(llvm_state.basic_blocks[cond_bb]).unwrap();
        for (i, phi) in cond_phis {
            let before_value = self.state.value_mapping.get(&previous_bb).unwrap().get(&i).unwrap().clone();
            let body_value = if self.state.value_set_in_block.contains_key(&after_body_bb) && llvm_state.value_set_in_block[&after_body_bb].contains(&i) {
                self.state.value_set_in_block.entry(cond_bb).or_insert_with(|| BTreeSet::new()).insert(i);
                self.state.get_llvm_value(after_body_bb, i)
            } else {
                before_value.clone()
            };
            phi.add_incoming(&[(&body_value, llvm_state.basic_blocks[after_body_bb]), (&before_value, llvm_state.basic_blocks[previous_bb])]);
            llvm_state.set_value(i, phi.as_basic_value());
        }
        llvm_state.set_current_block(after_cond_bb); 
        llvm_state.set_current_block(after_bb);
    }
}
