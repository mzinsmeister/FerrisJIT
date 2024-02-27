
pub mod stencils;
pub mod ir;
mod disassemble;
mod copy_patch;
mod expr_codegen;
mod generated_code;

use std::{cell::RefCell, hint::black_box, ops::Deref, ptr};
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
    Constant(ConstValue),
    Free
}

struct CGValueRef<'cg> {
    i: usize,
    cg: &'cg CodeGen,
    data_type: DataType
}

impl<'cg> Drop for CGValueRef<'cg> {
    fn drop(&mut self) {
        self.cg.free_value(self);
    }
}

impl<'cg> CGValueRef<'cg> {
    fn new(i: usize, cg: &'cg CodeGen, data_type: DataType) -> Self {
        CGValueRef { i, cg, data_type }
    }

    fn new_readonly(i: usize, cg: &'cg CodeGen, data_type: DataType) -> Self {
        CGValueRef { i, cg, data_type }
    }
}

impl<'cg> PartialEq for CGValueRef<'cg> {
    fn eq(&self, other: &Self) -> bool {
        self.i == other.i && ptr::eq(self.cg, other.cg)
    }
}

impl<'cg> Eq for CGValueRef<'cg,> {}

impl<'cg> PartialOrd for CGValueRef<'cg> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if !ptr::eq(self.cg, other.cg) {
            None
        } else {
            self.i.partial_cmp(&other.i)
        }
    }
}

impl<'cg> std::fmt::Debug for CGValueRef<'cg> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "CGValueRef({}, {:x})", self.i, ptr::addr_of!(self.cg) as usize)
    }
}

trait CGEq<'o> {
    fn cg_eq(self, other: &Self) -> BoolRef<'o>;

    fn cg_neq(self, other: &Self) -> BoolRef<'o>;
}

trait CGCmp<'o> {
    fn lt(self, other: &Self) -> BoolRef<'o>;

    fn lte(self, other: &Self) -> BoolRef<'o>;

    fn gt(self, other: &Self) -> BoolRef<'o>;

    fn gte(self, other: &Self) -> BoolRef<'o>;
}

#[derive(Debug, PartialEq, PartialOrd, Eq)]
struct I64Ref<'cg> (CGValueRef<'cg>);

impl<'cg> CGEq<'cg> for I64Ref<'cg> {
    fn cg_eq(self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let cgvref = cg.eq(&self.0, &other.0);
        if cgvref == self.0.i {
            BoolRef(self.0)
        } else {
            BoolRef(CGValueRef::new(cgvref, cg, DataType::Bool))
        }
    }

    fn cg_neq(self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let cgvref = cg.neq(&self.0, &other.0);
        if cgvref == self.0.i {
            BoolRef(self.0)
        } else {
            BoolRef(CGValueRef::new(cgvref, cg, DataType::Bool))
        }
    }
}

impl<'cg> CGCmp<'cg> for I64Ref<'cg> {
    fn lt(self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let cgvref = cg.lt(&self.0, &other.0);
        if cgvref == self.0.i {
            BoolRef(self.0)
        } else {
            BoolRef(CGValueRef::new(cgvref, cg, DataType::Bool))
        }
    }

    fn lte(self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let cgvref = cg.lte(&self.0, &other.0);
        if cgvref == self.0.i {
            BoolRef(self.0)
        } else {
            BoolRef(CGValueRef::new(cgvref, cg, DataType::Bool))
        }
    }

    fn gt(self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let cgvref = cg.gt(&self.0, &other.0);
        if cgvref == self.0.i {
            BoolRef(self.0)
        } else {
            BoolRef(CGValueRef::new(cgvref, cg, DataType::Bool))
        }
    }

    fn gte(self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let cgvref = cg.gte(&self.0, &other.0);
        if cgvref == self.0.i {
            BoolRef(self.0)
        } else {
            BoolRef(CGValueRef::new(cgvref, cg, DataType::Bool))
        }
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

    fn add(self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        let cgvref = cg.add(&self.0, &rhs.0);
        if cgvref == self.0.i {
            self
        } else {
            I64Ref(CGValueRef::new(cgvref, cg, self.0.data_type.clone()))
        }
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

    fn mul(self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        let cgvref = cg.mul(&self.0, &rhs.0);
        if cgvref == self.0.i {
            self
        } else {
            I64Ref(CGValueRef::new(cgvref, cg, self.0.data_type.clone()))
        }
    }
}

impl<'cg> std::ops::Sub<&Self> for I64Ref<'cg> {
    type Output = I64Ref<'cg>;

    fn sub(self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        let cgvref = cg.sub(&self.0, &rhs.0);
        if cgvref == self.0.i {
            self
        } else {
            I64Ref(CGValueRef::new(cgvref, cg, self.0.data_type.clone()))
        }
    }
}

impl<'cg> std::ops::Div<&Self> for I64Ref<'cg> {
    type Output = I64Ref<'cg>;

    fn div(self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        let cgvref = cg.div(&self.0, &rhs.0);
        if cgvref == self.0.i {
            self
        } else {
            I64Ref(CGValueRef::new(cgvref, cg, self.0.data_type.clone()))
        }
    }
}

impl<'cg> std::ops::Rem<&Self> for I64Ref<'cg> {
    type Output = I64Ref<'cg>;

    fn rem(self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        let cgvref = cg.rem(&self.0, &rhs.0);
        if cgvref == self.0.i {
            self
        } else {
            I64Ref(CGValueRef::new(cgvref, cg, self.0.data_type.clone()))
        }
    }
}



#[derive(Debug, PartialEq, PartialOrd, Eq)]
struct BoolRef<'cg> (CGValueRef<'cg>);

impl<'cg> CGEq<'cg> for BoolRef<'cg> {
    fn cg_eq(self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let cgvref = cg.eq(&self.0, &other.0);
        if cgvref == self.0.i {
            BoolRef(self.0)
        } else {
            BoolRef(CGValueRef::new(cgvref, cg, DataType::Bool))
        }
    }

    fn cg_neq(self, other: &Self) -> BoolRef<'cg> {
        let cg = self.0.cg;
        let cgvref = cg.neq(&self.0, &other.0);
        if cgvref == self.0.i {
            BoolRef(self.0)
        } else {
            BoolRef(CGValueRef::new(cgvref, cg, DataType::Bool))
        }
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

    fn bitand(self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        let cgvref = cg.and(&self.0, &rhs.0);
        if cgvref == self.0.i {
            self
        } else {
            BoolRef(CGValueRef::new(cgvref, cg, DataType::Bool))
        }
    }
}

impl<'cg> std::ops::BitOr<&Self> for BoolRef<'cg> {
    type Output = BoolRef<'cg>;

    fn bitor(self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        let cgvref = cg.or(&self.0, &rhs.0);
        if cgvref == self.0.i {
            self
        } else {
            BoolRef(CGValueRef::new(cgvref, cg, DataType::Bool))
        }
    }
}


struct CodeGen {
    inner: RefCell<CodeGenInner>,
}

#[allow(dead_code)]
impl CodeGen {

    fn new(args: usize) -> Self {
        let cg = Self {
            inner: RefCell::new(CodeGenInner::new(args)),
        };
        cg        
    }

    // We make sure arguments are immutable so having multiple references to them is not a problem
    pub fn get_arg(&self, n: usize) -> I64Ref {
        I64Ref(CGValueRef::new_readonly(n, self, DataType::I64))
    }

    pub fn new_i64_const(&self, n: i64) -> I64Ref {
        let inner = &mut self.inner.borrow_mut();
        let i = inner.values.len();
        inner.values.push(CGValue::Constant(ConstValue::I64(n)));
        I64Ref(CGValueRef::new(i, self, DataType::I64))
    }

    pub fn new_bool_const(&self, b: bool) -> BoolRef {
        let inner = &mut self.inner.borrow_mut();
        let i = inner.values.len();
        inner.values.push(CGValue::Constant(ConstValue::Bool(b)));
        BoolRef(CGValueRef::new(i, self, DataType::Bool))
    }

    fn free_value(&self, v: &CGValueRef) {
        self.inner.borrow_mut().free_value(v.i);
    }

    fn clone_value(&self, v: &CGValueRef) -> CGValueRef {
        let i = self.inner.borrow_mut().clone_value(v.i);
        CGValueRef::new(i, self, v.data_type.clone())
    }

    #[allow(dead_code)]
    pub fn reset(&mut self) {
        self.inner.borrow_mut().reset();
    }

    //--------------------------------------------------------------------------------
    // Arithmetic operations
    
    fn add(&self, l: &CGValueRef, r: &CGValueRef) -> usize {
        let cg = &mut self.inner.borrow_mut();
        cg.add(l.i, r.i)
    }

    fn sub(&self, l: &CGValueRef, r: &CGValueRef) -> usize {
        let cg = &mut self.inner.borrow_mut();
        cg.sub(l.i, r.i)
    }

    fn mul(&self, l: &CGValueRef, r: &CGValueRef) -> usize {
        let cg = &mut self.inner.borrow_mut();
        cg.mul(l.i, r.i)
    }

    fn div(&self, l: &CGValueRef, r: &CGValueRef) -> usize {
        let cg = &mut self.inner.borrow_mut();
        cg.div(l.i, r.i)
    }

    fn rem(&self, l: &CGValueRef, r: &CGValueRef) -> usize {
        let cg = &mut self.inner.borrow_mut();
        cg.rem(l.i, r.i)
    }

    fn eq(&self, l: &CGValueRef, r: &CGValueRef) -> usize {
        let cg = &mut self.inner.borrow_mut();
        cg.eq(l.i, r.i)
    }

    fn neq(&self, l: &CGValueRef, r: &CGValueRef) -> usize {
        let cg = &mut self.inner.borrow_mut();
        cg.neq(l.i, r.i)
    }

    fn lt(&self, l: &CGValueRef, r: &CGValueRef) -> usize {
        let cg = &mut self.inner.borrow_mut();
        cg.lt(l.i, r.i)
    }

    fn lte(&self, l: &CGValueRef, r: &CGValueRef) -> usize {
        let cg = &mut self.inner.borrow_mut();
        cg.lte(l.i, r.i)
    }

    fn gt(&self, l: &CGValueRef, r: &CGValueRef) -> usize {
        let cg = &mut self.inner.borrow_mut();
        cg.gt(l.i, r.i)
    }

    fn gte(&self, l: &CGValueRef, r: &CGValueRef) -> usize {
        let cg = &mut self.inner.borrow_mut();
        cg.gte(l.i, r.i)
    }

    fn and(&self, l: &CGValueRef, r: &CGValueRef) -> usize {
        let cg = &mut self.inner.borrow_mut();
        cg.and(l.i, r.i)
    }

    fn or(&self, l: &CGValueRef, r: &CGValueRef) -> usize {
        let cg = &mut self.inner.borrow_mut();
        cg.or(l.i, r.i)
    }
    
    //--------------------------------------------------------------------------------
    // Other operations

    pub fn generate_return<'a, D: Deref<Target = CGValueRef<'a>>>(&self, return_value: D) {
        let cg = &mut self.inner.borrow_mut();
        cg.generate_return(return_value.i);
    }

    //--------------------------------------------------------------------------------

    // Takes Ownership of the return value and resets all registers
    // TODO: References will be invalid after this. We cannot enforce 
    pub fn generate_code(&self) -> GeneratedCode {
        let cg = &mut self.inner.borrow_mut();
        cg.generate_code()
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
struct CodeGenInner {
    args_size: usize,
    values: Vec<CGValue>,
    free_slots: Vec<usize>,
    free_stack_pos: Vec<usize>,
    // We save whether a register is potentially dirty
    // one value can only be in one register at a time unless it's a readonly/const value
    // as soon as a readonly/const value is dirtied it becomes a different mutable variable
    reg_state: [Option<(usize, bool)>; 2], 
    inner: CopyPatchBackend,
    stack_ptr: usize, // TODO: Use actual byte sizes. For now we just use 8 bytes for everything
    stack_size: usize
}

#[allow(dead_code)]
impl CodeGenInner {

    fn new(args: usize) -> Self {
        let values = (0..args).into_iter()
            .map(|i| CGValue::Variable{ data_type: DataType::I64, stack_pos: i * 8, readonly: true})
            .collect();
        Self {
            args_size: args,
            values,
            free_slots: Vec::new(),
            free_stack_pos: Vec::new(),
            reg_state: [None, None],
            inner: CopyPatchBackend::new(),
            stack_ptr: args * 8,
            stack_size: args * 8,
        }
    }

    #[allow(dead_code)]
    fn reset(&mut self) {
        self.reg_state = [None, None];
        self.values.clear();
        self.free_slots.clear();
        self.stack_ptr = self.args_size * 8;
        self.stack_size = self.args_size * 8;
        self.inner.reset();
    }

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
                            0 => self.inner.generate_put_1_stack(*stack_pos),
                            1 => self.inner.generate_put_2_stack(*stack_pos),
                            _ => unreachable!(),
                        }
                        self.reg_state[reg].as_mut().unwrap().1 = false;
                    },
                    CGValue::Constant(_) => {
                        panic!("We should have allocated a stack slot before dirtying a const value");
                    },
                    CGValue::Free => { /* User doesn't need this anymore so we can just throw it away without writing it back. */},
                }
            }
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
                            0 => self.inner.generate_duplex1(),
                            1 => self.inner.generate_duplex2(),
                            _ => unreachable!(),
                        }
                        self.reg_state[reg] = self.reg_state[other_reg_n].clone();
                        return;
                    }
                }
                // We just swap the registers so that we don't have to move anything
                self.inner.generate_swap12();
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
                    0 => self.inner.generate_take_1_stack(*stack_pos),
                    1 => self.inner.generate_take_2_stack(*stack_pos),
                    _ => unreachable!(),
                }
            },
            CGValue::Constant(c) => {
                match reg {
                    0 => self.inner.generate_take_1_const(c.clone()),
                    1 => self.inner.generate_take_2_const(c.clone()),
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
                self.inner.generate_duplex1();
                self.reg_state[1] = self.reg_state[0].clone();
            } else {
                self.free_reg(0);
                self.inner.generate_duplex2();
                self.reg_state[0] = self.reg_state[1].clone();
            }
            return;
        }

        // registers are swapped
        if matches!(v0_reg_n, Some(1)) && matches!(v1_reg_n, Some(0)) {
            self.inner.generate_swap12();
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
                CGValue::Constant(c) => {
                    let stack_pos = self.stack_ptr;
                    self.stack_ptr += 8;
                    self.stack_size = self.stack_size.max(self.stack_ptr);
                    self.values[i] = CGValue::Variable { data_type: c.get_type(), stack_pos, readonly: false };
                    return Some(i);
                },
                _ => unreachable!(),
            }
        } else {
            panic!("tried to dirty a register that is not in use");
        }
    }

    fn allocate_stack(&mut self, data_type: DataType) -> usize {
        let i = if let Some(i) = self.free_slots.pop() {
            i
        } else {
            let i = self.values.len();
            self.values.push(CGValue::Free);
            i
        };
        let value = &mut self.values[i];
        match value {
            CGValue::Free => {
                let stack_ofs = if let Some(s) = self.free_stack_pos.pop() {
                    s
                } else {
                    let s = self.stack_ptr;
                    self.stack_ptr += 8;
                    self.stack_size = self.stack_size.max(self.stack_ptr);
                    s
                };
                *value = CGValue::Variable{ data_type, stack_pos: stack_ofs, readonly: false};
            },
            _ => unreachable!(),
        }
        i
    }

    /// Clones the value and returns it. Use the new value first if possible.
    fn clone_value(&mut self, v: usize) -> usize {
        let value = self.values[v].clone();
        match value {
            CGValue::Variable{data_type,..} => {
                if let Some((i, dirty)) = &mut self.reg_state[0] {
                    if *dirty {
                        self.inner.generate_put_stack(*i);
                        *dirty = false;
                    }
                    if *i == v {
                        *i = self.values.len();
                    }
                } else {
                    self.put_in_reg(0, v);
                    self.reg_state[0].unwrap().0 = self.values.len();
                }
                self.allocate_stack(data_type)
            },
            CGValue::Constant(c) => {
                // We can just copy constants 
                // TODO: Do similar free slot handling for constants but only take Free(None)'s
                let i = self.values.len();
                self.values.push(CGValue::Constant(c));
                i
            },
            CGValue::Free => unreachable!("We shouldn't even be able to have a reference to a free value"),
        }
    }

    fn free_value(&mut self, v: usize) {
        let value = &self.values[v];
        match value {
            CGValue::Variable{readonly, stack_pos,..} => {
                if *readonly {
                    return;
                }
                self.free_stack_pos.push(*stack_pos);
                self.values[v] = CGValue::Free;
                self.free_slots.push(v);
                // Check reg slots and free them if necessary
                for reg in self.reg_state.iter_mut() {
                    if let Some((i, _)) = reg {
                        if *i == v {
                            *reg = None;
                        }
                    }
                }
            },
            CGValue::Constant(_) => {
                self.values[v] = CGValue::Free;
                self.free_slots.push(v);
            }
            CGValue::Free => {/* TODO: Make sure double frees cannot happen, even internally */},
        }
    }

    //--------------------------------------------------------------------------------
    // Arithmetic operations

    fn gen_arith<const COMMUTATIVE: bool>(&mut self, gen_op: fn(&mut CopyPatchBackend, DataType), gen_op_const: fn(&mut CopyPatchBackend, ConstValue), l: usize, r: usize) -> usize {
        let vr = self.values[r].clone();
        // TODO: Use commutative property to optimize this
        match vr {
            CGValue::Variable{data_type,..} => {
                self.put_in_regs(l, r);
                gen_op(&mut self.inner, data_type);
            },
            CGValue::Constant(c) => {
                self.put_in_reg(0, l);
                gen_op_const(&mut self.inner, c);
            },
            CGValue::Free => unreachable!("We shouldn't even be able to have a reference to a free value"),
        }
        self.dirty_reg(0).unwrap()
    }

    fn add(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith::<true>(
            CopyPatchBackend::generate_add, 
            CopyPatchBackend::generate_add_const,
             l, r)
    }

    fn sub(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith::<false>(
            CopyPatchBackend::generate_sub, 
            CopyPatchBackend::generate_sub_const,
             l, r)
    }

    fn mul(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith::<true>(
            CopyPatchBackend::generate_mul, 
            CopyPatchBackend::generate_mul_const,
             l, r)
    }

    fn div(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith::<false>(
            CopyPatchBackend::generate_div, 
            CopyPatchBackend::generate_div_const,
             l, r)
    }

    fn rem(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith::<false>(
            CopyPatchBackend::generate_rem, 
            CopyPatchBackend::generate_rem_const,
             l, r)
    }

    fn eq(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith::<true>(
            CopyPatchBackend::generate_eq, 
            CopyPatchBackend::generate_eq_const,
             l, r)
    }

    fn neq(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith::<true>(
            CopyPatchBackend::generate_neq, 
            CopyPatchBackend::generate_neq_const,
             l, r)
    }

    // TODO: exchange lt/gte and lte/gt here if the first one is a const
    fn lt(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith::<false>(
            CopyPatchBackend::generate_lt, 
            CopyPatchBackend::generate_lt_const,
             l, r)
    }

    fn lte(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith::<false>(
            CopyPatchBackend::generate_lte, 
            CopyPatchBackend::generate_lte_const,
             l, r)
    }

    fn gt(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith::<false>(
            CopyPatchBackend::generate_gt, 
            CopyPatchBackend::generate_gt_const,
             l, r)
    }

    fn gte(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith::<false>(
            CopyPatchBackend::generate_gte, 
            CopyPatchBackend::generate_gte_const,
             l, r)
    }

    fn and(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith::<true>(
            CopyPatchBackend::generate_and, 
            CopyPatchBackend::generate_and_const,
             l, r)
    }

    fn or(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith::<true>(
            CopyPatchBackend::generate_or, 
            CopyPatchBackend::generate_or_const,
             l, r)
    }

    //--------------------------------------------------------------------------------
    // Other operations

    fn generate_return(&mut self, return_value: usize) {
        self.put_in_reg(0, return_value);
        self.inner.generate_put_stack(0);
        self.inner.generate_ret();
    }

    //--------------------------------------------------------------------------------
    // Control flow

    // TODO

    //--------------------------------------------------------------------------------

    fn generate_code(&self) -> GeneratedCode {
        self.inner.generate_code(self.stack_size)
    }
}

