
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
    Free(Option<usize>)
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

struct CodeGen {
    inner: RefCell<CodeGenInner>,
}

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
    reg_state: [Option<(usize, bool)>; 2], // We save whether a register is potentially dirty
    inner: CopyPatchBackend,
    stack_ptr: usize, // TODO: Use actual byte sizes. For now we just use 8 bytes for everything
    stack_size: usize
}

impl CodeGenInner {

    fn new(args: usize) -> Self {
        let values = (0..args).into_iter()
            .map(|i| CGValue::Variable{ data_type: DataType::I64, stack_pos: i * 8, readonly: true})
            .collect();
        Self {
            args_size: args,
            values,
            free_slots: Vec::new(),
            reg_state: [None, None],
            inner: CopyPatchBackend::new(args),
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

    fn free_reg(&mut self, reg: usize, new_i: usize) -> bool {
        if let Some((i, dirty)) = &mut self.reg_state[reg] {
            if *dirty {
                let cur_value = &self.values[*i];
                match cur_value {
                    CGValue::Variable{readonly, stack_pos,..} => {
                        if *readonly {
                            panic!("We should have allocated a stack slot before dirtying a readonly value");
                        }
                        if *i != new_i {
                            // Save it to its designated stack location
                            match reg {
                                0 => self.inner.generate_put_stack(*stack_pos),
                                _ => todo!("Implement dirtying of second register"),
                            }
                        } else {
                            return true;
                        }
                    },
                    CGValue::Constant(_) => {
                        panic!("We should have allocated a stack slot before dirtying a const value");
                    },
                    CGValue::Free(_) => { /* User doesn't need this anymore so we can just throw it away without writing it back */},
                }
            }
        }
        false
    }

    fn put_in_reg0(&mut self, v: usize) {
        if self.free_reg(0, v) {
            return;
        }

        // Lookup which value is currently in the first register
        let reg1_state = self.reg_state[1].clone();
        if let Some((i, _)) = reg1_state {
            if i == v {
                todo!("Also support moving from second to first register")
            }
        }
        let value = &self.values[v];
        // Load the value into the first scratch register
        match value {
            CGValue::Variable{stack_pos,..}=> {
                // TODO: do take stack with datatype
                self.inner.generate_take_1_stack(*stack_pos);
            },
            CGValue::Constant(c) => {
                self.inner.generate_take_1_const(*c);
            },
            CGValue::Free(_) => unreachable!("We shouldn't even be able to have a reference to a free value"),
        }
        self.reg_state[0] = Some((v, false));
    }
    
    fn put_in_reg1(&mut self, v: usize) {
        self.free_reg(1, v);

        // Is our value in the first register? if so we can just swap the registers
        let reg0_state = self.reg_state[0].clone();
        if let Some((i, dirty)) = reg0_state {
            if i == v {
                if dirty {
                    todo!("Handle this case"); // should currently not happen
                }
                self.inner.generate_duplex();
                self.reg_state[1] = self.reg_state[0].clone();
                return;
            }
        }
        let value = &self.values[v];
        // Load the value into the first scratch register
        match value {
            CGValue::Variable{stack_pos, ..} => {
                // TODO: do take stack with datatype
                self.inner.generate_take_2_stack(*stack_pos);
            },
            CGValue::Constant(c) => {
                self.inner.generate_take_2_const(*c);
            },
            CGValue::Free(_) => unreachable!("We shouldn't even be able to have a reference to a free value"),
        }
        self.reg_state[1] = Some((v, false));
    }

    fn dirty_reg0(&mut self) -> Option<usize> {
        let reg_state = self.reg_state[0].clone();
        let reg_state2 = self.reg_state[1].clone();
        // If we have the same value in the second register, we must set it to free
        if let Some((i, _)) = reg_state2 { // TODO: What if its dirty here?
            if i == reg_state2.unwrap().0 {
                self.reg_state[1] = None;
            }
        }
        if let Some((i, _)) = reg_state {
            // Check whether the value is readonly and if it is we allocate a new value
            self.reg_state[0].as_mut().unwrap().1 = true;
            match &self.values[i] {
                CGValue::Variable{readonly,..} => {
                    if *readonly {
                        let slot = self.allocate_stack(DataType::I64);
                        self.reg_state[0].as_mut().unwrap().0 = slot;
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
        }
        None
    }

    #[allow(dead_code)]
    fn dirty_reg1(&mut self) -> Option<usize> {
        // This case is non-trivial because we also have to handle the case where value is already in reg0 and is dirty
        // In this case we could have two diverging states for the same value
        // Maybe this can't even happen as long as we keep the "one reference per mutable value" invariant
        todo!("Implement dirtying of second register")
    }

    fn lose_reg(&mut self, reg: usize) -> Option<()> {
        if let Some((i, dirty)) = &mut self.reg_state[reg] {
            let dirty = if *dirty {
                // We lost a dirty value, this should not happen unless we throw away the value anyway
                // We return true here because it was dirty but don't panic yet as the caller has
                // to decide whether this is acceptable or not.
                None
            } else {
                Some(())
            };
            self.reg_state[reg] = None;
            dirty
        } else {
            Some(())
        }
    }

    fn allocate_stack(&mut self, data_type: DataType) -> usize {
        let i = if let Some(i) = self.free_slots.pop() {
            i
        } else {
            let i = self.values.len();
            self.values.push(CGValue::Free(None));
            i
        };
        let value = &mut self.values[i];
        match value {
            CGValue::Free(ref mut slot) => {
                let stack_ofs = if let Some(s) = slot {
                    *s
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
                    self.put_in_reg0(v);
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
            CGValue::Free(_) => unreachable!("We shouldn't even be able to have a reference to a free value"),
        }
    }

    fn free_value(&mut self, v: usize) {
        let value = &self.values[v];
        match value {
            CGValue::Variable{readonly, stack_pos,..} => {
                if *readonly {
                    return;
                }
                self.values[v] = CGValue::Free(Some(*stack_pos));
                self.free_slots.push(v);
            },
            CGValue::Constant(_) => {
                self.values[v] = CGValue::Free(None);
                self.free_slots.push(v);
            }
            CGValue::Free(_) => {/* TODO: Make sure double frees cannot happen, even internally */},
        }
    }

    //--------------------------------------------------------------------------------
    // Arithmetic operations

    fn gen_arith(&mut self, gen_op: fn(&mut CopyPatchBackend, DataType), gen_op_const: fn(&mut CopyPatchBackend, ConstValue), l: usize, r: usize) -> usize {
        let vr = self.values[r].clone();
        self.put_in_reg0(l);
        match vr {
            CGValue::Variable{data_type,..} => {
                self.put_in_reg1(r);
                gen_op(&mut self.inner, data_type);
            },
            CGValue::Constant(c) => {
                gen_op_const(&mut self.inner, c);
            },
            CGValue::Free(_) => unreachable!("We shouldn't even be able to have a reference to a free value"),
        }
        self.lose_reg(1);
        self.dirty_reg0().unwrap()
    }

    fn add(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith(
            CopyPatchBackend::generate_add, 
            CopyPatchBackend::generate_add_const,
             l, r)
    }

    fn sub(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith(
            CopyPatchBackend::generate_sub, 
            CopyPatchBackend::generate_sub_const,
             l, r)
    }

    fn mul(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith(
            CopyPatchBackend::generate_mul, 
            CopyPatchBackend::generate_mul_const,
             l, r)
    }

    fn div(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith(
            CopyPatchBackend::generate_div, 
            CopyPatchBackend::generate_div_const,
             l, r)
    }

    fn rem(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith(
            CopyPatchBackend::generate_rem, 
            CopyPatchBackend::generate_rem_const,
             l, r)
    }

    fn eq(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith(
            CopyPatchBackend::generate_eq, 
            CopyPatchBackend::generate_eq_const,
             l, r)
    }

    fn neq(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith(
            CopyPatchBackend::generate_neq, 
            CopyPatchBackend::generate_neq_const,
             l, r)
    }

    fn lt(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith(
            CopyPatchBackend::generate_lt, 
            CopyPatchBackend::generate_lt_const,
             l, r)
    }

    fn lte(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith(
            CopyPatchBackend::generate_lte, 
            CopyPatchBackend::generate_lte_const,
             l, r)
    }

    fn gt(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith(
            CopyPatchBackend::generate_gt, 
            CopyPatchBackend::generate_gt_const,
             l, r)
    }

    fn gte(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith(
            CopyPatchBackend::generate_gte, 
            CopyPatchBackend::generate_gte_const,
             l, r)
    }

    fn and(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith(
            CopyPatchBackend::generate_and, 
            CopyPatchBackend::generate_and_const,
             l, r)
    }

    fn or(&mut self, l: usize, r: usize) -> usize {
        self.gen_arith(
            CopyPatchBackend::generate_or, 
            CopyPatchBackend::generate_or_const,
             l, r)
    }

    //--------------------------------------------------------------------------------
    // Other operations

    fn generate_return(&mut self, return_value: usize) {
        self.put_in_reg0(return_value);
        self.inner.generate_put_stack(0);
        self.inner.generate_ret();
    }

    //--------------------------------------------------------------------------------
    // Control flow

   // fn generate_if<

    //--------------------------------------------------------------------------------

    fn generate_code(&self) -> GeneratedCode {
        self.inner.generate_code(self.stack_size)
    }
}

