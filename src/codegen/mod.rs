
pub mod stencils;
pub mod ir;
mod disassemble;
mod copy_patch;
mod expr_codegen;
mod generated_code;

use core::panic;
use std::{cell::RefCell, collections::BTreeMap, hint::black_box, ops::Deref, ptr, rc::Rc};
use libc::c_void;

use crate::codegen::{copy_patch::STENCILS, ir::DataType};

use self::{copy_patch::CopyPatchBackend, ir::ConstValue};

pub use expr_codegen::{get_type, generate_code, CodeGenError};
pub use generated_code::GeneratedCode;

// This is a simple example of how we can generate code that calls back into pre-compiled
// functions using copy-and-patch
#[allow(dead_code)]
unsafe extern "C" fn hello_world(args: *mut u8) -> *mut u8 {
    let u64_ptr = args as *mut u64;
    let arg1 = u64_ptr.read_unaligned();
    println!("Hello, world from the generated Code! {}", arg1);
    42 as *mut u8
}

pub(crate) fn init_stencils() {
    // Dummy access to initialize stencils
    let compile_start = std::time::Instant::now();
    black_box(STENCILS.len());
    let compile_elapsed = compile_start.elapsed();
    println!("Stencil initialization: {:?}", compile_elapsed);
}

#[allow(dead_code)]
fn get_fn_ptr(f: unsafe extern "C" fn(*mut u8) -> *mut u8) -> *const c_void {
    f as *const c_void
}


// We try to write a nice rustic API here that is easy to use
// and takes advantage of the borrow checker to make sure that
// we don't do stupid shit. Therefore we will introduce wrappers
// representing values here which will then allow us to do all the
// register/stack moving automatically while still beeing efficient
// and reusing values in registers as much as possible.
// Somewhat similar to the "Tidy Tuples" framework
// (https://db.in.tum.de/~kersten/Tidy%20Tuples%20and%20Flying%20Start%20Fast%20Compilation%20and%20Fast%20Execution%20of%20Relational%20Queries%20in%20Umbra.pdf?lang=de)

#[derive(Debug, Clone)]
enum CGValue {
    Variable{
        data_type: DataType, 
        stack_pos: usize,
        readonly: bool
    },
    Free
}


trait Setable<'o, Other> {
    fn set(&self, other: Other);
}

#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
enum CGValueRefInner {
    Value(usize),
    Const(ConstValue)
}

pub struct CGValueRef<'cg> {
    inner: CGValueRefInner,
    cg: &'cg CodeGen,
    data_type: DataType,
}

impl<'cg> Drop for CGValueRef<'cg> {
    fn drop(&mut self) {
        self.cg.free_value(self);
    }
}

impl<'cg> CGValueRef<'cg> {
    fn new(i: usize, cg: &'cg CodeGen, data_type: DataType) -> Self {
        CGValueRef { inner: CGValueRefInner::Value(i), cg, data_type }
    }

    fn new_const(c: ConstValue, cg: &'cg CodeGen) -> Self {
        CGValueRef { inner: CGValueRefInner::Const(c), cg, data_type: c.get_type() }
    }
}

impl Setable<'_, &Self> for CGValueRef<'_> {
    fn set(&self, other: &Self) {
        if self != other {
            self.cg.copy_value(other, self);
        }
    }
}

impl<'cg, T: Into<CGValueRef<'cg>>,> Setable<'cg, T> for CGValueRef<'cg> {
    fn set(&self, other: T) {
        let other = other.into();
        if self != &other {
            self.cg.copy_value(&other, self);
        }
    }
}

impl<'cg> PartialEq for CGValueRef<'cg> {
    fn eq(&self, other: &Self) -> bool {
        self.inner == other.inner && ptr::eq(self.cg, other.cg)
    }
}

impl<'cg> Eq for CGValueRef<'cg> {}

impl<'cg> PartialOrd for CGValueRef<'cg> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if !ptr::eq(self.cg, other.cg) {
            None
        } else {
            self.inner.partial_cmp(&other.inner)
        }
    }
}

impl<'cg> std::fmt::Debug for CGValueRef<'cg> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CGValueRef({:?}, {:x})", self.inner, ptr::addr_of!(self.cg) as usize)
    }
}
trait CGEq<'o, Other> {
    fn cg_eq(self, other: Other) -> BoolRef<'o>;

    fn cg_neq(self, other: Other) -> BoolRef<'o>;
}

trait CGCmp<'o, Other> {
    fn cg_lt(self, other: Other) -> BoolRef<'o>;

    fn cg_lte(self, other: Other) -> BoolRef<'o>;

    fn cg_gt(self, other: Other) -> BoolRef<'o>;

    fn cg_gte(self, other: Other) -> BoolRef<'o>;
}

// Since we can't 
/*trait CGSet<'o, Other> {
    fn cg_set(self, other: Other);
}*/

#[derive(Debug, PartialEq, PartialOrd, Eq)]
pub struct I64Ref<'cg> (CGValueRef<'cg>);

impl<'cg> Deref for I64Ref<'cg> {
    type Target = CGValueRef<'cg>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'cg> CGEq<'cg, &Self> for I64Ref<'cg> {
    fn cg_eq(mut self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        cg.eq(&mut self.0, &other.0);
        BoolRef(self.0)
    }

    fn cg_neq(mut self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        cg.neq(&mut self.0, &other.0);
        BoolRef(self.0)
    }
}

impl<'cg> CGEq<'cg, i64> for I64Ref<'cg> {
    fn cg_eq(mut self, other: i64) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::I64(other), self.cg);
        cg.eq(&mut self.0, &other);
        BoolRef(self.0)
    }

    fn cg_neq(mut self, other: i64) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::I64(other), self.cg);
        cg.neq(&mut self.0, &other);
        BoolRef(self.0)
    }
}

impl<'cg> CGCmp<'cg, &Self> for I64Ref<'cg> {
    fn cg_lt(mut self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        cg.lt(&mut self.0, &other.0);
        BoolRef(self.0)
    }

    fn cg_lte(mut self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        cg.lte(&mut self.0, &other.0);
        BoolRef(self.0)
    }

    fn cg_gt(mut self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        cg.gt(&mut self.0, &other.0);
        BoolRef(self.0)
    }

    fn cg_gte(mut self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        cg.gte(&mut self.0, &other.0);
        BoolRef(self.0)
    }
}

impl<'cg> CGCmp<'cg, i64> for I64Ref<'cg> {
    fn cg_lt(mut self, other: i64) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::I64(other), self.cg);
        cg.lt(&mut self.0, &other);
        BoolRef(self.0)
    }

    fn cg_lte(mut self, other: i64) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::I64(other), self.cg);
        cg.lte(&mut self.0, &other);
        BoolRef(self.0)
    }
    fn cg_gt(mut self, other: i64) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::I64(other), self.cg);
        cg.gt(&mut self.0, &other);
        BoolRef(self.0)
    }

    fn cg_gte(mut self, other: i64) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::I64(other), self.cg);
        cg.gte(&mut self.0, &other);
        BoolRef(self.0)
    }
}

impl<'cg> Into<CGValueRef<'cg>> for I64Ref<'cg> {
    fn into(self) -> CGValueRef<'cg> {
        self.0
    }
}

impl Clone for I64Ref<'_> {
    fn clone(&self) -> Self {
        let cg = self.0.cg;
        let clone = I64Ref(cg.clone_value(&self.0));
        clone
    }
}

impl<'cg> From<CGValueRef<'cg>> for I64Ref<'cg> {
    fn from(v: CGValueRef<'cg>) -> Self {
        I64Ref(v)
    }
}

// this actually takes ownership of the value which means we can overwrite it
impl<'cg> std::ops::Add<&Self> for I64Ref<'cg> {
    type Output = I64Ref<'cg>;

    fn add(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.add(&mut self.0, &rhs.0);
        self
    }
}

impl<'cg> std::ops::Add<i64> for I64Ref<'cg> {
    type Output = I64Ref<'cg>;

    fn add(mut self, rhs: i64) -> Self::Output {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.add(&mut self.0, &rhs);
        self
    }
}

// TODO: Think about what to do with the assign traits. Could probably work like this:
/*impl<'cg> std::ops::AddAssign<&Self> for I64Ref<'cg> {
    fn add_assign(&mut self, rhs: &Self) {
        let cg = self.0.cg;
        let cgvref = cg.add(&self.0, &rhs.0);
        if cgvref != self.0.i {
            self.0 = CGValueRef::new(cgvref, cg, self.0.data_type.clone());
        }
    }
}*/

impl<'cg> std::ops::Mul<&Self> for I64Ref<'cg> {
    type Output = I64Ref<'cg>;

    fn mul(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.mul(&mut self.0, &rhs.0);
        self
    }
}

impl<'cg> std::ops::Mul<i64> for I64Ref<'cg> {
    type Output = I64Ref<'cg>;

    fn mul(mut self, rhs: i64) -> Self::Output {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.mul(&mut self.0, &rhs);
        self
    }
}

impl<'cg> std::ops::Sub<&Self> for I64Ref<'cg> {
    type Output = I64Ref<'cg>;

    fn sub(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.sub(&mut self.0, &rhs.0);
        self
    }
}

impl<'cg> std::ops::Sub<i64> for I64Ref<'cg> {
    type Output = I64Ref<'cg>;

    fn sub(mut self, rhs: i64) -> Self::Output {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.sub(&mut self.0, &rhs);
        self
    }
}

impl<'cg> std::ops::Div<&Self> for I64Ref<'cg> {
    type Output = I64Ref<'cg>;

    fn div(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.div(&mut self.0, &rhs.0);
        self
    }
}

impl<'cg> std::ops::Div<i64> for I64Ref<'cg> {
    type Output = I64Ref<'cg>;

    fn div(mut self, rhs: i64) -> Self::Output {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.div(&mut self.0, &rhs);
        self
    }
}

impl<'cg> std::ops::Rem<&Self> for I64Ref<'cg> {
    type Output = I64Ref<'cg>;

    fn rem(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.rem(&mut self.0, &rhs.0);
        self
    }
}

impl<'cg> std::ops::Rem<i64> for I64Ref<'cg> {
    type Output = I64Ref<'cg>;

    fn rem(mut self, rhs: i64) -> Self::Output {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.rem(&mut self.0, &rhs);
        self
    }
}


#[derive(Debug, PartialEq, PartialOrd, Eq)]
pub struct BoolRef<'cg> (CGValueRef<'cg>);

impl<'cg> Deref for BoolRef<'cg> {
    type Target = CGValueRef<'cg>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'cg> CGEq<'cg, &Self> for BoolRef<'cg> {
    fn cg_eq(mut self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        cg.eq(&mut self.0, &other.0);
        BoolRef(self.0)
    }

    fn cg_neq(mut self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        cg.neq(&mut self.0, &other.0);
        BoolRef(self.0)
    }
}

impl<'cg> CGEq<'cg, bool> for BoolRef<'cg> {
    fn cg_eq(mut self, other: bool) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::Bool(other), self.cg);
        cg.eq(&mut self.0, &other);
        BoolRef(self.0)
    }

    fn cg_neq(mut self, other: bool) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::Bool(other), self.cg);
        cg.neq(&mut self.0, &other);
        BoolRef(self.0)
    }
}

impl<'cg> Into<CGValueRef<'cg>> for BoolRef<'cg> {
    fn into(self) -> CGValueRef<'cg> {
        self.0
    }
}

impl<'cg> From<CGValueRef<'cg>> for BoolRef<'cg> {
    fn from(v: CGValueRef<'cg>) -> Self {
        BoolRef(v)
    }
}

impl Clone for BoolRef<'_> {
    fn clone(&self) -> Self {
        let cg = self.0.cg;
        let clone = BoolRef(cg.clone_value(&self.0));
        clone
    }
}

impl<'cg> std::ops::BitAnd<&Self> for BoolRef<'cg> {
    type Output = BoolRef<'cg>;

    fn bitand(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.and(&mut self.0, &rhs.0);
        self
    }
}

impl<'cg> std::ops::BitOr<&Self> for BoolRef<'cg> {
    type Output = BoolRef<'cg>;

    fn bitor(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.or(&mut self.0, &rhs.0);
        self
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
        DataType::I64 | DataType::U64 => 8,
        DataType::I32 | DataType::U32 => 4,
        DataType::I16 | DataType::U16 => 2,
        DataType::I8 | DataType::U8 | DataType::Bool => 1,
        _ => unimplemented!()
    }
}

struct MemoryManagement {
    args_size: usize,
    values: Vec<CGValue>,
    free_slots: Vec<usize>,
    free_stack_pos: BTreeMap<usize, Vec<usize>>,
    // We save whether a register is potentially dirty
    // one value can only be in one register at a time unless it's a readonly/const value
    // as soon as a readonly/const value is dirtied it becomes a different mutable variable
    reg_state: [Option<(usize, bool)>; 2], 
    stack_ptr: usize, // TODO: Use actual byte sizes. For now we just use 8 bytes for everything
    stack_size: usize,
    cp_backend: Rc<CopyPatchBackend>
}

impl MemoryManagement {
    fn new(cp_backend: Rc<CopyPatchBackend>, args: usize) -> Self {
        let values = (0..args).into_iter()
            .map(|i| CGValue::Variable{ data_type: DataType::I64, stack_pos: i * 8, readonly: true})
            .collect();
        Self {
            args_size: args,
            values,
            free_slots: Vec::new(),
            free_stack_pos: BTreeMap::new(),
            reg_state: [None, None],
            stack_ptr: args * 8,
            stack_size: args * 8,
            cp_backend
        }
    }

    #[allow(dead_code)]
    fn reset(&mut self) {
        self.reg_state = [None, None];
        self.values.clear();
        self.free_slots.clear();
        self.stack_ptr = self.args_size * 8;
        self.stack_size = self.args_size * 8;
    }

    fn alloc_stack(&mut self, size: usize) -> usize {
        if let Some(next) = self.free_stack_pos.get_mut(&size) {
            let pos = next.pop().unwrap();
            if next.is_empty() {
                self.free_stack_pos.remove(&size);
            }
            pos
        } else {
            let pos = self.stack_ptr;
            self.stack_ptr += size;
            self.stack_size = self.stack_size.max(self.stack_ptr);
            pos
        }
    }

    #[allow(dead_code)]
    fn alloc_stack_at_least(&mut self, size: usize) -> usize {
        if let Some((&next_size, next)) = self.free_stack_pos.range_mut(size..).next() {
            let pos = next.pop().unwrap();
            if next.is_empty() {
                self.free_stack_pos.remove(&next_size);
            }
            pos
        } else {
            let pos = self.stack_ptr;
            self.stack_ptr += size;
            self.stack_size = self.stack_size.max(self.stack_ptr);
            pos
        }
    }

    #[allow(dead_code)]
    fn free_stack(&mut self, pos: usize, size: usize) {
        self.free_stack_pos.entry(size).or_default().push(pos);
    }

    /// Frees a register, saving its content to the stack if necessary
    fn free_reg(&mut self, reg: usize) {
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

    fn flush_regs(&mut self) {
        for reg in 0..2 {
            self.lose_reg(reg);
        }
    }

    /// Put a single value into a single register. Use "put_in_regs" if you want to set both registers
    /// This might change the state of the other register!
    fn put_in_reg(&mut self, reg: usize, v: usize) {
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
    fn put_in_regs(&mut self, reg0_v: usize, reg1_v: usize) {
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

    fn dirty_reg(&mut self, reg: usize) -> Option<usize> {
        let reg_state = self.reg_state[reg].clone();
        if let Some((i, _)) = reg_state {
            // Check whether the value is readonly and if it is we allocate a new value
            self.reg_state[reg].as_mut().unwrap().1 = true;
            match &self.values[i] {
                CGValue::Variable{readonly,..} => {
                    if *readonly {
                        let slot = self.allocate_stack(DataType::I64);
                        self.reg_state[reg].as_mut().unwrap().0 = slot;
                        return Some(slot);
                    } else {
                        return Some(i);
                    }
                },
                _ => unreachable!(),
            }
        } else {
            panic!("tried to dirty a register that is not in use");
        }
    }

    fn allocate_stack(&mut self, data_type: DataType) -> usize {
        let stack_pos = self.alloc_stack(get_data_type_size(&data_type));
        if let Some(i) = self.free_slots.pop() {
            self.values[i] = CGValue::Variable{ data_type, stack_pos, readonly: false};
            i
        } else {
            let i = self.values.len();
            self.values.push(CGValue::Variable{ data_type, stack_pos, readonly: false});
            i
        }
    }

    /// Clones the value and returns it. Use the new value first if possible.
    fn clone_value(&mut self, v: usize) -> usize {
        let value = self.values[v].clone();
        match value {
            CGValue::Variable{data_type,..} => {
                self.free_reg(0);
                self.put_in_reg(0, v);
                let new_i = self.allocate_stack(data_type);
                self.reg_state[0] = Some((new_i, true));
                new_i
            },
            CGValue::Free => unreachable!("We shouldn't even be able to have a reference to a free value"),
        }
    }

    fn free_value(&mut self, v: usize) {
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

    fn lose_reg(&mut self, reg: usize) {
        self.free_reg(reg);
        self.reg_state[reg] = None;
    }

    fn init(&mut self, i: usize, c: ConstValue) {
        self.free_reg(0);
        self.cp_backend.emit_take_1_const(c);
        self.reg_state[0] = Some((i, false));
    }
}

pub struct CodeGen {
    inner: Rc<CopyPatchBackend>,
    memory_management: RefCell<MemoryManagement>
}

#[allow(dead_code)]
impl CodeGen {

    pub fn new(args: usize) -> Self {
        let cp_backend = Rc::new(CopyPatchBackend::new());
        let memory_management = MemoryManagement::new(cp_backend.clone(), args);
        Self {
            inner: cp_backend,
            memory_management: RefCell::new(memory_management)
        }
    }

    pub fn get_arg(&self, n: usize) -> I64Ref {
        // We immediately copy the arg to a new position so that we can handle it like any other value
        // The lazy approach is great but it doesn't work for stuff like loops

        let mut memory_management = self.memory_management.borrow_mut();
        let i = memory_management.clone_value(n);
        I64Ref(CGValueRef::new(i, self, DataType::I64))

        //I64Ref(CGValueRef::new_readonly(n, self, DataType::I64))
    }

    pub fn new_i64_const(&self, n: i64) -> I64Ref {
        I64Ref(CGValueRef::new_const(ConstValue::I64(n), self))
    }

    pub fn new_bool_const(&self, b: bool) -> BoolRef {
        BoolRef(CGValueRef::new_const(ConstValue::Bool(b), self))
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

    fn clone_value(&self, v: &CGValueRef) -> CGValueRef {
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
                self.inner.emit_put_1_stack(dest_ptr);
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
        self.inner.reset();
    }

    //--------------------------------------------------------------------------------
    // Arithmetic operations

    fn gen_arith<const COMMUTATIVE: bool>(&self,  gen_op: fn(&CopyPatchBackend, DataType), gen_op_const: fn(&CopyPatchBackend, ConstValue), l: &mut CGValueRef, r: &CGValueRef) {
        let mut memory_management = self.memory_management.borrow_mut();
        match (l.inner, r.inner) {
            (CGValueRefInner::Value(li), CGValueRefInner::Value(ri)) => {
                memory_management.put_in_regs(li, ri);
                gen_op(&self.inner, l.data_type.clone());
            },
            (CGValueRefInner::Value(li), CGValueRefInner::Const(c)) => {
                let vl = memory_management.values[li].clone();
                match vl {
                    CGValue::Variable{..} => {
                        memory_management.put_in_reg(0, li);
                        gen_op_const(&self.inner, c);
                    },
                    CGValue::Free => unreachable!("We shouldn't even be able to have a reference to a free value"),
                }
            },
            (CGValueRefInner::Const(c), CGValueRefInner::Value(ri)) => {
                if COMMUTATIVE {
                    memory_management.put_in_reg(0, ri);
                    gen_op_const(&self.inner, c);
                } else {
                    let new_l = memory_management.allocate_stack(c.get_type());
                    memory_management.init(new_l, c);
                    memory_management.put_in_regs(new_l, ri);
                    gen_op(&self.inner, c.get_type());
                    l.inner = CGValueRefInner::Value(new_l);
                }
            },
            (CGValueRefInner::Const(c1), CGValueRefInner::Const(c2)) => {
                let new_l = memory_management.allocate_stack(c1.get_type());
                memory_management.init(new_l, c1);
                memory_management.put_in_reg(0, new_l);
                gen_op_const(&self.inner, c2);
                l.inner = CGValueRefInner::Value(new_l);
            }
        }
        memory_management.dirty_reg(0).unwrap();
    }

    fn add(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true>(CopyPatchBackend::emit_add,CopyPatchBackend::emit_add_const, l, r)
    }

    fn sub(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false>(CopyPatchBackend::emit_sub,CopyPatchBackend::emit_sub_const, l, r)
    }

    fn mul(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true>(CopyPatchBackend::emit_mul,CopyPatchBackend::emit_mul_const, l, r)
    }

    fn div(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false>(CopyPatchBackend::emit_div,CopyPatchBackend::emit_div_const,  l, r)
    }

    fn rem(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false>(CopyPatchBackend::emit_rem,CopyPatchBackend::emit_rem_const,  l, r)
    }

    fn eq(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true>(CopyPatchBackend::emit_eq,CopyPatchBackend::emit_eq_const, l, r)
    }

    fn neq(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true>(CopyPatchBackend::emit_neq,CopyPatchBackend::emit_neq_const,  l, r)
    }


    // TODO: exchange lt/gte and lte/gt here if the first one is a const
    fn lt(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false>(CopyPatchBackend::emit_lt,CopyPatchBackend::emit_lt_const, l, r)
    }

    fn lte(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false>(CopyPatchBackend::emit_lte,CopyPatchBackend::emit_lte_const, l, r)
    }

    fn gt(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false>(CopyPatchBackend::emit_gt,CopyPatchBackend::emit_gt_const, l, r)
    }

    fn gte(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<false>(CopyPatchBackend::emit_gte,CopyPatchBackend::emit_gte_const, l, r)
    }

    fn and(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true>(CopyPatchBackend::emit_and,CopyPatchBackend::emit_and_const, l, r)
    }

    fn or(&self, l: &mut CGValueRef, r: &CGValueRef) {
        self.gen_arith::<true>(CopyPatchBackend::emit_or,CopyPatchBackend::emit_or_const, l, r)
    }

    fn not(&self, l: &mut CGValueRef) {
        let mut memory_management = self.memory_management.borrow_mut();
        match l.inner {
            CGValueRefInner::Value(i) => {
                memory_management.put_in_reg(0, i);
                self.inner.emit_not(l.data_type.clone());
                memory_management.dirty_reg(0).unwrap();
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
    pub fn generate_if(&self, mut condition: BoolRef, then_branch: impl Fn()) {
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
        self.inner.emit_if(move || {
            then_branch();
        });
        let mut memory_management = self.memory_management.borrow_mut();
        memory_management.lose_reg(0);
    }

    // TODO: This currently doesn't work with constants because we load them lazily which leads to the
    //       constant instead of the updated value beeing loaded again in the loop condition
    //       What we could do as a quick fix is materialize every constant into a variable before we enter a loop
    pub fn gen_while<'cg>(&self, condition: impl Fn() -> BoolRef<'cg>, body: impl Fn()) {
        self.memory_management.borrow_mut().flush_regs();
        self.inner.emit_loop( || {
            let res = condition();
            let i = match res.0.inner {
                CGValueRefInner::Value(i) => i,
                CGValueRefInner::Const(c) => {
                    let new_i = self.memory_management.borrow_mut().allocate_stack(DataType::Bool);
                    self.memory_management.borrow_mut().init(new_i, c);
                    new_i
                }
            };
            self.memory_management.borrow_mut().put_in_reg(0, i);
        }, || {
            body();
        });
    }

    //--------------------------------------------------------------------------------
    // Other operations

    fn generate_return(&self, return_value: CGValueRef) {
        let i = match return_value.inner {
            CGValueRefInner::Value(i) => i,
            CGValueRefInner::Const(c) => {
                let new_i = self.memory_management.borrow_mut().allocate_stack(c.get_type());
                self.memory_management.borrow_mut().init(new_i, c);
                new_i
            }
        };
        self.memory_management.borrow_mut().put_in_reg(0, i);
        self.inner.emit_put_stack(0);
        self.inner.emit_ret();
    }

    //--------------------------------------------------------------------------------

    fn generate_code(&self) -> GeneratedCode {
        self.inner.generate_code(self.memory_management.borrow().stack_size)
    }
}

