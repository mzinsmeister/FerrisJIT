pub mod ir;
pub mod disassemble;
mod copy_patch;
mod generated_code;
mod llvm;
mod memory_management;
pub mod types;

use core::panic;
use std::{cell::RefCell, collections::BTreeMap, hint::black_box, mem, ops::Deref, ptr};

use crate::codegen::{copy_patch::STENCILS, ir::DataType};

use self::{copy_patch::CopyPatchBackend, ir::ConstValue, memory_management::{CGValue, MemoryManagement}, types::{BoolRef, I64Ref, UntypedPtrRef}};

pub use generated_code::GeneratedCode;
use inkwell::{builder::Builder, context::Context, values::{BasicValueEnum, IntValue}};
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
    builder: inkwell::builder::Builder<'ctx>,
    module: inkwell::module::Module<'ctx>,
    function: inkwell::values::FunctionValue<'ctx>,
    current_block: usize,
    // Mapping from (basic block, value) to llvm value
    // Control flow operations need to make sure to merge values that were sets inside the blocks they
    // created back with a phi block
    value_mapping: BTreeMap<(usize, usize), BasicValueEnum<'ctx>>,
    value_set_in_block: BTreeMap<usize, Vec<usize>>,
    basic_block_counter: usize
}

impl<'ctx> LLVMState<'ctx> {
    fn new(context: &'ctx Context, arg_types: &[DataType]) -> Self {
        let module = context.create_module("main");
        let i8_ptr = context.i8_type().ptr_type(inkwell::AddressSpace::default());
        let llvm_arg_types = arg_types.iter().map(|t| t.get_llvm_type(context).into()).collect::<Vec<_>>();
        let function = module.add_function("main", i8_ptr.fn_type( &llvm_arg_types, false), None);
        let entry_block = context.append_basic_block(function, "entry");
        let builder = context.create_builder();
        builder.position_at_end(entry_block);
        // Let's create values for the arguments and add them to the value mapping already
        let mut value_mapping = BTreeMap::new();
        for (i, arg_type) in arg_types.iter().enumerate() {
            let arg = function.get_nth_param(i as u32).unwrap();
            value_mapping.insert((0, i), arg.into());
        }
        Self {
            builder,
            module,
            function,
            current_block: 0,
            value_mapping,
            value_set_in_block: BTreeMap::new(),
            basic_block_counter: 1
        }
    }    
}

pub struct CodeGen<'ctx> {
    ctx: &'ctx CodeGenContext,
    memory_management: RefCell<MemoryManagement<'ctx>>,
    llvm_state: LLVMState<'ctx>
}

#[allow(dead_code)]
impl<'ctx> CodeGen<'ctx> {

    fn new(ctx: &'ctx CodeGenContext, arg_types: &[DataType]) -> Self {
        let memory_management = MemoryManagement::new(&ctx.cp_backend, arg_types);

        Self {
            ctx,
            memory_management: RefCell::new(memory_management),
            llvm_state: LLVMState::new(&ctx.llvm_ctx, arg_types)
        }
    }

    pub fn get_arg(&self, n: usize) -> CGValueRef<'_, 'ctx> {
        // We immediately copy the arg to a new position so that we can handle it like any other value
        // The lazy approach is great but it doesn't work fctxor stuff like loops

        let mut memory_management = self.memory_management.borrow_mut();
        let i = memory_management.clone_value(n);
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
        I64Ref(self.new_const(ConstValue::I64(n)))
    }

    /// Create a new i64 variable. If you don't need to change the value, use a constant instead.
    pub fn new_i64_var(&self, init: i64) -> I64Ref<'_, 'ctx>{
        let var = self.new_var(DataType::I64);
        let init = self.new_i64_const(init);
        self.copy_value(&init, &var);
        I64Ref(var)
    }

    /// Create a new bool constant. Note that `set` cannot be called on constants!
    pub fn new_bool_const(&self, b: bool) -> BoolRef<'_, 'ctx>{
        BoolRef(self.new_const(ConstValue::Bool(b)))
    }

    /// Create a new bool variable. If you don't need to change the value, use a constant instead.
    pub fn new_bool_var(&self, init: bool) -> BoolRef<'_, 'ctx>{
        let var = self.new_var(DataType::Bool);
        let init = self.new_bool_const(init);
        self.copy_value(&init, &var);
        BoolRef(var)
    }


    fn load_const(&self, v: usize, c: ConstValue) {
        let mut memory_management = self.memory_management.borrow_mut();
        memory_management.init(v, c);
    }

    fn free_value(&self, v: &CGValueRef) {
        match v.inner {
            CGValueRefInner::Value(i) => self.memory_management.borrow_mut().free_value(i),
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
                let i = memory_management.clone_value(i);
                CGValueRef::new(i, self, v.data_type.clone())
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
            },
            CGValueRefInner::Const(c) => {
               self.memory_management.borrow_mut().init(dest_i, c);
            }

        }
    
    }

    #[allow(dead_code)]
    fn reset(&mut self) {
        self.memory_management.borrow_mut().reset();
        self.ctx.cp_backend.reset();
    }

    //--------------------------------------------------------------------------------
    // Arithmetic operations

    fn gen_arith<const COMMUTATIVE: bool, const RETURNS_BOOL: bool>(&self,  gen_op: fn(&CopyPatchBackend, DataType), gen_op_const: fn(&CopyPatchBackend, ConstValue), gen_llvm: fn(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx>, l: &mut CGValueRef, r: &CGValueRef) {
        let mut memory_management = self.memory_management.borrow_mut();
        match (l.inner, r.inner) {
            (CGValueRefInner::Value(li), CGValueRefInner::Value(ri)) => {
                memory_management.put_in_regs(li, ri);
                gen_op(&self.ctx.cp_backend, l.data_type.clone());
            },
            (CGValueRefInner::Value(li), CGValueRefInner::Const(c)) => {
                let vl = memory_management.values[li].clone();
                match vl {
                    CGValue::Variable{..} => {
                        memory_management.put_in_reg(0, li);
                        gen_op_const(&self.ctx.cp_backend, c);
                    },
                    CGValue::Free => unreachable!("We shouldn't even be able to have a reference to a free value"),
                }
            },
            (CGValueRefInner::Const(c), CGValueRefInner::Value(ri)) => {
                if COMMUTATIVE {
                    memory_management.put_in_reg(0, ri);
                    gen_op_const(&self.ctx.cp_backend, c);
                } else {
                    let new_l = memory_management.allocate_stack(c.get_type());
                    memory_management.init(new_l, c);
                    memory_management.put_in_regs(new_l, ri);
                    gen_op(&self.ctx.cp_backend, c.get_type());
                    l.inner = CGValueRefInner::Value(new_l);
                }
            },
            (CGValueRefInner::Const(c1), CGValueRefInner::Const(c2)) => {
                let new_l = memory_management.allocate_stack(c1.get_type());
                memory_management.init(new_l, c1);
                memory_management.put_in_reg(0, new_l);
                gen_op_const(&self.ctx.cp_backend, c2);
                l.inner = CGValueRefInner::Value(new_l);
            }
        }
        memory_management.dirty_reg(0);
        if RETURNS_BOOL {
            l.data_type = DataType::Bool;
        }
    }

    fn add(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true, false>(CopyPatchBackend::emit_add,CopyPatchBackend::emit_add_const, llvm::int_add, l, r)
    }

    fn sub(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false, false>(CopyPatchBackend::emit_sub,CopyPatchBackend::emit_sub_const,  llvm::int_sub, l, r)
    }

    fn mul(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true, false>(CopyPatchBackend::emit_mul,CopyPatchBackend::emit_mul_const,  llvm::int_mul, l, r)
    }

    fn div(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false, false>(CopyPatchBackend::emit_div,CopyPatchBackend::emit_div_const,  llvm::int_div,  l, r)
    }

    fn rem(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false, false>(CopyPatchBackend::emit_rem,CopyPatchBackend::emit_rem_const,  llvm::int_rem,  l, r)
    }

    fn eq(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true, true>(CopyPatchBackend::emit_eq,CopyPatchBackend::emit_eq_const,  llvm::int_eq, l, r)
    }

    fn neq(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true, true>(CopyPatchBackend::emit_neq,CopyPatchBackend::emit_neq_const,  llvm::int_ne,  l, r)
    }

    // TODO: exchange lt/gte and lte/gt here if the first one is a const
    fn lt(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false, true>(CopyPatchBackend::emit_lt,CopyPatchBackend::emit_lt_const,  llvm::int_lt, l, r)
    }

    fn lte(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false, true>(CopyPatchBackend::emit_lte,CopyPatchBackend::emit_lte_const,  llvm::int_lte, l, r)
    }

    fn gt(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false, true>(CopyPatchBackend::emit_gt,CopyPatchBackend::emit_gt_const,  llvm::int_gt, l, r)
    }

    fn gte(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false, true>(CopyPatchBackend::emit_gte,CopyPatchBackend::emit_gte_const, llvm::int_gte, l, r)
    }

    fn and(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true, true>(CopyPatchBackend::emit_and,CopyPatchBackend::emit_and_const, llvm::int_and, l, r)
    }

    fn or(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true, true>(CopyPatchBackend::emit_or,CopyPatchBackend::emit_or_const, llvm::int_or, l, r)
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
            Ok(())
        }, || {
            body()
        })?;
        self.memory_management.borrow_mut().flush_regs();
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
        if let Some(return_value) = return_value {
            let i = match return_value.inner {
                CGValueRefInner::Value(i) => i,
                CGValueRefInner::Const(c) => {
                    let new_i = self.memory_management.borrow_mut().allocate_stack(c.get_type());
                    self.memory_management.borrow_mut().init(new_i, c);
                    new_i
                }
            };
            self.memory_management.borrow_mut().put_in_reg(0, i);
        }
        self.ctx.cp_backend.emit_ret();
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
        new_var.into()
    }

    //--------------------------------------------------------------------------------

    pub fn generate_code(&self) -> GeneratedCode {
        self.ctx.cp_backend.generate_code(self.memory_management.borrow().stack_size)
    }
}

