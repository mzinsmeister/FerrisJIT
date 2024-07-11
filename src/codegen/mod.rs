pub mod ir;
pub mod disassemble;
mod copy_patch;
mod result;
mod llvm;
mod memory_management;
pub mod types;

use core::panic;
use std::{cell::RefCell, collections::{BTreeMap, BTreeSet}, hint::black_box, mem, ops::Deref, ptr};

use crate::codegen::{copy_patch::STENCILS, ir::DataType};

use self::{copy_patch::CopyPatchBackend, ir::ConstValue, memory_management::{CGValue, MemoryManagement}, types::{BoolRef, I64Ref, UntypedPtrRef}};

pub use result::{CodeGenResult, GeneratedCode};
use inkwell::{builder::Builder, context::Context, llvm_sys::LLVMCallConv, values::BasicValueEnum, AddressSpace};
use libc::c_void;

pub type CodegenCFunctionSignature = unsafe extern "C" fn(*mut u8, *mut u8, *mut u8) -> *mut u8; // (state, arg1, arg2) -> result (all can be unused depending on the usecase)

pub(crate) fn init_stencils() {
    // Dummy access to initialize stencils
    let compile_start = std::time::Instant::now();
    black_box(STENCILS.len());
    let compile_elapsed = compile_start.elapsed();
    println!("Stencil initialization: {:?}", compile_elapsed);
}


pub trait Setable<Other> {
    fn set(&self, other: Other);
}

trait PtrTarget<'cg, 'ctx: 'cg>: Into<CGValueRef<'cg, 'ctx>> + From<CGValueRef<'cg, 'ctx>> {
    fn get_data_type() -> DataType;
    fn get_inner(&self) -> &CGValueRef<'cg, 'ctx>;
}

pub trait IntoBaseRef<'cg, 'ctx: 'cg> {
    fn into_base(self) -> CGValueRef<'cg, 'ctx>;
}

impl<'cg, 'ctx: 'cg,  T: Into<CGValueRef<'cg, 'ctx>>> IntoBaseRef<'cg, 'ctx> for T {
    fn into_base(self) -> CGValueRef<'cg, 'ctx> {
        self.into() 
    }
}
// TODO: Evaluate other solutions for this. 

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
enum CGValueRefInner {
    Value(usize),
    Const(ConstValue)
}

impl CGValueRefInner {
    fn into_value_i(&self) -> usize {
        match self {
            CGValueRefInner::Value(i) => *i,
            CGValueRefInner::Const(_) => unreachable!(),
        }
    }
}

pub struct CGValueRef<'cg, 'ctx: 'cg> {
    inner: CGValueRefInner,
    cg: &'cg CodeGen<'ctx>,
    pub data_type: DataType,
}


impl<'cg, 'ctx> Clone for CGValueRef<'cg, 'ctx> {
    fn clone(&self) -> Self {
        self.cg.clone_value(self)
    }
}

impl<'cg, 'ctx> Drop for CGValueRef<'cg, 'ctx> {
    fn drop(&mut self) {
        self.cg.free_value(self);
    }
}

impl<'cg, 'ctx: 'cg> CGValueRef<'cg, 'ctx> {
    fn new(i: usize, cg: &'cg CodeGen<'ctx>, data_type: DataType) -> Self {
        CGValueRef { inner: CGValueRefInner::Value(i), cg, data_type }
    }

    fn new_const(c: ConstValue, cg: &'cg CodeGen<'ctx>) -> Self {
        CGValueRef { inner: CGValueRefInner::Const(c), cg, data_type: c.get_type() }
    }
}

impl<'cg, 'ctx: 'cg, T: Deref<Target=CGValueRef<'cg, 'ctx>> + Into<CGValueRef<'cg, 'ctx>>> Setable<T> for T {
    fn set(&self, other: T) {
        if self.deref() != other.deref() {
            self.cg.copy_value(&other, self);
        }
    }
}

impl<'cg, 'ctx: 'cg, T: Deref<Target=CGValueRef<'cg, 'ctx>> + Into<CGValueRef<'cg, 'ctx>>> Setable<&T> for T {
    fn set(&self, other: &T) {
        if self.deref() != other.deref() {
            self.cg.copy_value(&other, self);
        }
    }
}

impl<'cg, 'ctx> PartialEq for CGValueRef<'cg, 'ctx> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner && ptr::eq(self.cg, other.cg)
    }
}

impl<'cg, 'ctx> Eq for CGValueRef<'cg, 'ctx> {}

impl<'cg, 'ctx> PartialOrd for CGValueRef<'cg, 'ctx> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if !ptr::eq(self.cg, other.cg) {
            None
        } else {
            self.inner.partial_cmp(&other.inner)
        }
    }
}

impl<'cg, 'ctx> std::fmt::Debug for CGValueRef<'cg, 'ctx> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CGValueRef({:?}, {:x})", self.inner, ptr::addr_of!(self.cg) as usize)
    }
}

// How this works is that we have "values" which are roughly equivalent to 
// variables in a high level language. Note that we don't do SSA here. I
// don't think it's really beneficial for Copy and patch as it would probably
// make it harder to detect whether we can reuse a value in a register or not
// However keep the invariant that all mutable values (which for now are all
// values except for the arguments) are only referenced exactly once. The borrow checker
// helps us with this. We also keep the state of our registers, most notably what values
// are currently in them and whether they are dirty or not. Even Constants can be in registers
// however once they become dirty we change their type to variable and allocate stack space for them.
// The same is true for readonly values. We also manage free slots (as soon as the reference to a value)
// is droppped, it is actually freed through RAII (Drop trait).
//
// Operations have to make sure to call the dirty_reg0/2 (only dirty_reg0 is implemented for now)
// function whenever they change the value in a register. This function will then make sure that
// the value is marked dirty and spilled to the stack if necessary. We load values into registers
// lazily. Just because you create a constant, that doesn't mean there will be any code generated
// for it. We only load it into a register when we actually need it. We also try to move between
// registers and stack as little as possible. 

/// Convenience layer around copy and patch compilation backend
/// so that you don't have to think about registers anymore

fn get_data_type_size(data_type: &DataType) -> usize {
    match data_type {
        DataType::I64 | DataType::U64 | DataType::F64 => 8,
        DataType::I32 | DataType::U32 | DataType::F32 => 4,
        DataType::I16 | DataType::U16 => 2,
        DataType::I8 | DataType::U8 | DataType::Bool => 1,
        DataType::Ptr => mem::size_of::<usize>(),
    }
}

// We pull out some context. This will make sure that the CopyPatchBackend and the LLVM Context
// live longer than our CodeGen which makes a lot of stuff easier

pub struct CodeGenContext {
    cp_backend: CopyPatchBackend,
    llvm_ctx: inkwell::context::Context,
    cg_created: bool
}

impl CodeGenContext {
    pub fn new() -> Self {
        let cp_backend = CopyPatchBackend::new();
        let llvm_ctx = inkwell::context::Context::create();
        Self {
            cp_backend,
            llvm_ctx,
            cg_created: false,
        }
    }

    // We borrow mutable here to make sure there's always only just one CodeGen at a time
    // Destroying this and recreating it would also lead to undefined behaviour
    pub fn create_codegen(&mut self, arg_types: &[DataType]) -> CodeGen {
        if self.cg_created {
            panic!("CodeGen can only be created once per context!");
        }
        self.cg_created = true;
        CodeGen::new(self, arg_types)
    }
}

fn get_fn_ptr(f: CodegenCFunctionSignature) -> *const c_void {
    f as unsafe extern "C" fn(*mut u8, *mut u8, *mut u8) -> *mut u8 as *const c_void
}

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

pub struct CodeGen<'ctx> {
    ctx: &'ctx CodeGenContext,
    memory_management: RefCell<MemoryManagement<'ctx>>,
    llvm_state: RefCell<LLVMState<'ctx>>
}

#[allow(dead_code)]
impl<'ctx> CodeGen<'ctx> {

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
                self.llvm_state.borrow_mut().copy_value(dest_i, i);
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
        self.gen_arith::<true, false, _>(CopyPatchBackend::emit_add,CopyPatchBackend::emit_add_const, |ls, t, l, r| llvm::int_sub(&ls.builder, t, l.into_int_value(), r.into_int_value()).into(), l, r)
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
        self.gen_arith::<true,true, _>(CopyPatchBackend::emit_add, CopyPatchBackend::emit_add_const, |ls, t, l, r| {
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
        self.ctx.cp_backend.emit_if(|| {
            self.memory_management.borrow_mut().lose_reg(0);
            then_branch()
        })?;
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
        self.ctx.cp_backend.emit_if_else(|| {
            self.memory_management.borrow_mut().lose_reg(0);
            then_branch();
            let mut memory_management = self.memory_management.borrow_mut();
            memory_management.flush_regs();
        }, || {
            else_branch();
            let mut memory_management = self.memory_management.borrow_mut();
            memory_management.flush_regs();
        });
    }

    /// Generate a while loop. You cannot assign to variables declared outside the closure passed as
    /// condition. This is by design because it prevents you from accidentially generating nonsensical code.
    /// Use the `set` function instead.
    pub fn gen_while<'cg, E>(&self, condition: impl Fn() -> Result<BoolRef<'cg, 'ctx>, E>, body: impl Fn() -> Result<(), E>) -> Result<(), E> where 'ctx: 'cg {
        self.memory_management.borrow_mut().flush_regs();
        let mut llvm_state = self.llvm_state.borrow_mut();
        let previous_bb = llvm_state.current_block;
        let body_bb = llvm_state.new_basic_block();
        let cond_bb = llvm_state.new_basic_block();
        let after_bb = llvm_state.new_basic_block();
        let mut after_cond_bb: usize = cond_bb;
        // insert an unconditional branch from previous to cond
        llvm_state.builder.build_unconditional_branch(llvm_state.basic_blocks[cond_bb]).unwrap();
        self.ctx.cp_backend.emit_loop( || {
            llvm_state.set_current_block(cond_bb);
            drop(llvm_state);
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
            body()
        })?;
        self.memory_management.borrow_mut().flush_regs();
        let mut llvm_state = self.llvm_state.borrow_mut();
        llvm_state.builder.build_unconditional_branch(llvm_state.basic_blocks[after_bb]).unwrap();
        llvm_state.set_current_block(cond_bb);
        llvm_state.builder.position_before(&llvm_state.basic_blocks[cond_bb].get_first_instruction().unwrap());
        for i in llvm_state.value_set_in_block.get(&body_bb).unwrap_or(&BTreeSet::new()).clone() {
            let llvm_value = llvm_state.get_llvm_value(previous_bb, i);
            let phi = llvm_state.builder.build_phi(llvm_value.get_type(), &format!("phi_{}", i)).unwrap();
            let before_value = llvm_state.value_mapping.get(&previous_bb).unwrap().get(&i).unwrap().clone();
            phi.add_incoming(&[(&llvm_value, llvm_state.basic_blocks[after_cond_bb]), (&before_value, llvm_state.basic_blocks[cond_bb])]);
            llvm_state.set_value(i, phi.as_basic_value());
            llvm_state.value_set_in_block.entry(after_bb).or_insert_with(|| BTreeSet::new()).insert(i);
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
        // dump llvm ir
        self.llvm_state.borrow().module.print_to_stderr();
        let llvm_module = self.llvm_state.borrow().module.clone();
        CodeGenResult { code, llvm_module }
    }
}

