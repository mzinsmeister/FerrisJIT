


// We try to write a nice rustic API here that is easy to use
// and takes advantage of the borrow checker to make sure that
// we don't do stupid shit. Therefore we will introduce wrappers
// representing values here which will then allow us to do all the
// register/stack moving automatically while still beeing efficient
// and reusing values in registers as much as possible.
// Somewhat similar to the "Tidy Tuples" framework
// (https://db.in.tum.de/~kersten/Tidy%20Tuples%20and%20Flying%20Start%20Fast%20Compilation%20and%20Fast%20Execution%20of%20Relational%20Queries%20in%20Umbra.pdf?lang=de)

use std::ops::Deref;

use super::{get_data_type_size, ir::{ConstValue, DataType}, CGValueRef, CodeGen, PtrTarget, Setable};


pub trait CGEq<'cg, 'ctx, Other> {
    fn cg_eq(self, other: Other) -> BoolRef<'cg, 'ctx>;

    fn cg_neq(self, other: Other) -> BoolRef<'cg, 'ctx>;
}

pub trait CGCmp<'cg, 'ctx, Other> {
    fn cg_lt(self, other: Other) -> BoolRef<'cg, 'ctx>;

    fn cg_lte(self, other: Other) -> BoolRef<'cg, 'ctx>;

    fn cg_gt(self, other: Other) -> BoolRef<'cg, 'ctx>;

    fn cg_gte(self, other: Other) -> BoolRef<'cg, 'ctx>;
}

// Since we can't 
/*trait CGSet<'o, Other> {
    fn cg_set(self, other: Other);
}*/

#[derive(Debug, PartialEq, PartialOrd, Eq)]
pub struct I64Ref<'cg, 'ctx> (pub CGValueRef<'cg, 'ctx>);

impl<'cg, 'ctx> Deref for I64Ref<'cg, 'ctx> {
    type Target = CGValueRef<'cg, 'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'cg, 'ctx> CGEq<'cg, 'ctx, &Self> for I64Ref<'cg, 'ctx> {
    fn cg_eq(mut self, other: &Self) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        cg.eq(&mut self.0, &other.0);
        BoolRef(self.0)
    }

    fn cg_neq(mut self, other: &Self) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        cg.neq(&mut self.0, &other.0);
        BoolRef(self.0)
    }
}

impl<'cg, 'ctx> CGEq<'cg, 'ctx, i64> for I64Ref<'cg, 'ctx> {
    fn cg_eq(mut self, other: i64) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::I64(other), self.cg);
        cg.eq(&mut self.0, &other);
        BoolRef(self.0)
    }

    fn cg_neq(mut self, other: i64) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::I64(other), self.cg);
        cg.neq(&mut self.0, &other);
        BoolRef(self.0)
    }
}

impl<'cg, 'ctx> CGCmp<'cg, 'ctx, &Self> for I64Ref<'cg, 'ctx> {
    fn cg_lt(mut self, other: &Self) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        cg.lt(&mut self.0, &other.0);
        BoolRef(self.0)
    }

    fn cg_lte(mut self, other: &Self) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        cg.lte(&mut self.0, &other.0);
        BoolRef(self.0)
    }

    fn cg_gt(mut self, other: &Self) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        cg.gt(&mut self.0, &other.0);
        BoolRef(self.0)
    }

    fn cg_gte(mut self, other: &Self) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        cg.gte(&mut self.0, &other.0);
        BoolRef(self.0)
    }
}

impl<'cg, 'ctx> CGCmp<'cg, 'ctx, i64> for I64Ref<'cg, 'ctx> {
    fn cg_lt(mut self, other: i64) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::I64(other), self.cg);
        cg.lt(&mut self.0, &other);
        BoolRef(self.0)
    }

    fn cg_lte(mut self, other: i64) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::I64(other), self.cg);
        cg.lte(&mut self.0, &other);
        BoolRef(self.0)
    }
    fn cg_gt(mut self, other: i64) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::I64(other), self.cg);
        cg.gt(&mut self.0, &other);
        BoolRef(self.0)
    }

    fn cg_gte(mut self, other: i64) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::I64(other), self.cg);
        cg.gte(&mut self.0, &other);
        BoolRef(self.0)
    }
}

impl<'cg, 'ctx> Into<CGValueRef<'cg, 'ctx>> for I64Ref<'cg, 'ctx> {
    fn into(self) -> CGValueRef<'cg, 'ctx> {
        self.0
    }
}

impl<'cg, 'ctx> From<CGValueRef<'cg, 'ctx>> for I64Ref<'cg, 'ctx> {
    fn from(v: CGValueRef<'cg, 'ctx>) -> Self {
        I64Ref(v)
    }
}

impl Clone for I64Ref<'_, '_> {
    fn clone(&self) -> Self {
        let cg = self.0.cg;
        let clone = I64Ref(cg.clone_value(&self.0));
        clone
    }
}

impl<'cg, 'ctx> PtrTarget<'cg, 'ctx> for I64Ref<'cg, 'ctx> {
    fn get_data_type() -> DataType {
        DataType::I64
    }

    fn get_inner(&self) -> &CGValueRef<'cg, 'ctx> {
        &self.0
    }
}

// this actually takes ownership of the value which means we can overwrite it
impl<'cg, 'ctx> std::ops::Add<&Self> for I64Ref<'cg, 'ctx> {
    type Output = I64Ref<'cg, 'ctx>;

    fn add(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.add(&mut self.0, &rhs.0);
        self
    }
}

impl<'cg, 'ctx> std::ops::Add<i64> for I64Ref<'cg, 'ctx> {
    type Output = I64Ref<'cg, 'ctx>;

    fn add(mut self, rhs: i64) -> Self::Output {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.add(&mut self.0, &rhs);
        self
    }
}

impl<'cg, 'ctx> std::ops::AddAssign<&Self> for I64Ref<'cg, 'ctx> {
    fn add_assign(&mut self, rhs: &Self) {
        let cg = self.0.cg;
        cg.add(&mut self.0, &rhs.0);
    }
}

impl<'cg, 'ctx> std::ops::AddAssign<i64> for I64Ref<'cg, 'ctx> {
    fn add_assign(&mut self, rhs: i64) {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.add(&mut self.0, &rhs);
    }
}

impl<'cg, 'ctx> std::ops::Mul<&Self> for I64Ref<'cg, 'ctx> {
    type Output = I64Ref<'cg, 'ctx>;

    fn mul(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.mul(&mut self.0, &rhs.0);
        self
    }
}

impl<'cg, 'ctx> std::ops::Mul<i64> for I64Ref<'cg, 'ctx> {
    type Output = I64Ref<'cg, 'ctx>;

    fn mul(mut self, rhs: i64) -> Self::Output {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.mul(&mut self.0, &rhs);
        self
    }
}

impl<'cg, 'ctx> std::ops::MulAssign<&Self> for I64Ref<'cg, 'ctx> {
    fn mul_assign(&mut self, rhs: &Self) {
        let cg = self.0.cg;
        cg.mul(&mut self.0, &rhs.0);
    }
}

impl<'cg, 'ctx> std::ops::MulAssign<i64> for I64Ref<'cg, 'ctx> {
    fn mul_assign(&mut self, rhs: i64) {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.mul(&mut self.0, &rhs);
    }
}

impl<'cg, 'ctx> std::ops::Sub<&Self> for I64Ref<'cg, 'ctx> {
    type Output = I64Ref<'cg, 'ctx>;

    fn sub(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.sub(&mut self.0, &rhs.0);
        self
    }
}

impl<'cg, 'ctx> std::ops::Sub<i64> for I64Ref<'cg, 'ctx> {
    type Output = I64Ref<'cg, 'ctx>;

    fn sub(mut self, rhs: i64) -> Self::Output {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.sub(&mut self.0, &rhs);
        self
    }
}

impl<'cg, 'ctx> std::ops::SubAssign<&Self> for I64Ref<'cg, 'ctx> {
    fn sub_assign(&mut self, rhs: &Self) {
        let cg = self.0.cg;
        cg.sub(&mut self.0, &rhs.0);
    }
}

impl<'cg, 'ctx> std::ops::SubAssign<i64> for I64Ref<'cg, 'ctx> {
    fn sub_assign(&mut self, rhs: i64) {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.sub(&mut self.0, &rhs);
    }
}

impl<'cg, 'ctx> std::ops::Div<&Self> for I64Ref<'cg, 'ctx> {
    type Output = I64Ref<'cg, 'ctx>;

    fn div(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.div(&mut self.0, &rhs.0);
        self
    }
}

impl<'cg, 'ctx> std::ops::Div<i64> for I64Ref<'cg, 'ctx> {
    type Output = I64Ref<'cg, 'ctx>;

    fn div(mut self, rhs: i64) -> Self::Output {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.div(&mut self.0, &rhs);
        self
    }
}

impl<'cg, 'ctx> std::ops::DivAssign<&Self> for I64Ref<'cg, 'ctx> {
    fn div_assign(&mut self, rhs: &Self) {
        let cg = self.0.cg;
        cg.div(&mut self.0, &rhs.0);
    }
}

impl<'cg, 'ctx> std::ops::DivAssign<i64> for I64Ref<'cg, 'ctx> {
    fn div_assign(&mut self, rhs: i64) {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.div(&mut self.0, &rhs);
    }
}

impl<'cg, 'ctx> std::ops::Rem<&Self> for I64Ref<'cg, 'ctx> {
    type Output = I64Ref<'cg, 'ctx>;

    fn rem(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.rem(&mut self.0, &rhs.0);
        self
    }
}

impl<'cg, 'ctx> std::ops::Rem<i64> for I64Ref<'cg, 'ctx> {
    type Output = I64Ref<'cg, 'ctx>;

    fn rem(mut self, rhs: i64) -> Self::Output {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.rem(&mut self.0, &rhs);
        self
    }
}

impl<'cg, 'ctx> std::ops::RemAssign<&Self> for I64Ref<'cg, 'ctx> {
    fn rem_assign(&mut self, rhs: &Self) {
        let cg = self.0.cg;
        cg.rem(&mut self.0, &rhs.0);
    }
}

impl<'cg, 'ctx> std::ops::RemAssign<i64> for I64Ref<'cg, 'ctx> {
    fn rem_assign(&mut self, rhs: i64) {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.rem(&mut self.0, &rhs);
    }
}

impl<'cg, 'ctx> std::ops::BitAnd<&Self> for I64Ref<'cg, 'ctx> {
    type Output = I64Ref<'cg, 'ctx>;

    fn bitand(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.and(&mut self.0, &rhs.0);
        self
    }
}

impl<'cg, 'ctx> std::ops::BitAnd<i64> for I64Ref<'cg, 'ctx> {
    type Output = I64Ref<'cg, 'ctx>;

    fn bitand(mut self, rhs: i64) -> Self::Output {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.and(&mut self.0, &rhs);
        self
    }
}

impl<'cg, 'ctx> std::ops::BitAndAssign<&Self> for I64Ref<'cg, 'ctx> {
    fn bitand_assign(&mut self, rhs: &Self) {
        let cg = self.0.cg;
        cg.and(&mut self.0, &rhs.0);
    }
}

impl<'cg, 'ctx> std::ops::BitAndAssign<i64> for I64Ref<'cg, 'ctx> {
    fn bitand_assign(&mut self, rhs: i64) {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.and(&mut self.0, &rhs);
    }
}

impl<'cg, 'ctx> std::ops::BitOr<&Self> for I64Ref<'cg, 'ctx> {
    type Output = I64Ref<'cg, 'ctx>;

    fn bitor(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.or(&mut self.0, &rhs.0);
        self
    }
}

impl<'cg, 'ctx> std::ops::BitOr<i64> for I64Ref<'cg, 'ctx> {
    type Output = I64Ref<'cg, 'ctx>;

    fn bitor(mut self, rhs: i64) -> Self::Output {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.or(&mut self.0, &rhs);
        self
    }
}

impl<'cg, 'ctx> std::ops::BitOrAssign<&Self> for I64Ref<'cg, 'ctx> {
    fn bitor_assign(&mut self, rhs: &Self) {
        let cg = self.0.cg;
        cg.or(&mut self.0, &rhs.0);
    }
}

impl<'cg, 'ctx> std::ops::BitOrAssign<i64> for I64Ref<'cg, 'ctx> {
    fn bitor_assign(&mut self, rhs: i64) {
        let cg = self.0.cg;
        let rhs = CGValueRef::new_const(ConstValue::I64(rhs), self.cg);
        cg.or(&mut self.0, &rhs);
    }
}



#[derive(Debug, PartialEq, PartialOrd, Eq)]
pub struct BoolRef<'cg, 'ctx> (pub CGValueRef<'cg, 'ctx>);

impl<'cg, 'ctx> Deref for BoolRef<'cg, 'ctx> {
    type Target = CGValueRef<'cg, 'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'cg, 'ctx> CGEq<'cg, 'ctx, &Self> for BoolRef<'cg, 'ctx> {
    fn cg_eq(mut self, other: &Self) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        cg.eq(&mut self.0, &other.0);
        BoolRef(self.0)
    }

    fn cg_neq(mut self, other: &Self) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        cg.neq(&mut self.0, &other.0);
        BoolRef(self.0)
    }
}

impl<'cg, 'ctx> CGEq<'cg, 'ctx, bool> for BoolRef<'cg, 'ctx> {
    fn cg_eq(mut self, other: bool) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::Bool(other), self.cg);
        cg.eq(&mut self.0, &other);
        BoolRef(self.0)
    }

    fn cg_neq(mut self, other: bool) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        let other = CGValueRef::new_const(ConstValue::Bool(other), self.cg);
        cg.neq(&mut self.0, &other);
        BoolRef(self.0)
    }
}

impl<'cg, 'ctx> Into<CGValueRef<'cg, 'ctx>> for BoolRef<'cg, 'ctx> {
    fn into(self) -> CGValueRef<'cg, 'ctx> {
        self.0
    }
}

impl<'cg, 'ctx> Into<&'cg CGValueRef<'cg, 'ctx>> for &'cg BoolRef<'cg, 'ctx> {
    fn into(self) -> &'cg CGValueRef<'cg, 'ctx> {
        &self.0
    }
}


impl<'cg, 'ctx> From<CGValueRef<'cg, 'ctx>> for BoolRef<'cg, 'ctx> {
    fn from(v: CGValueRef<'cg, 'ctx>) -> Self {
        BoolRef(v)
    }
}

impl Clone for BoolRef<'_, '_> {
    fn clone(&self) -> Self {
        let cg = self.0.cg;
        let clone = BoolRef(cg.clone_value(&self.0));
        clone
    }
}

impl<'cg, 'ctx> PtrTarget<'cg, 'ctx> for BoolRef<'cg, 'ctx> {
    fn get_data_type() -> DataType {
        DataType::Bool
    }

    fn get_inner(&self) -> &CGValueRef<'cg, 'ctx> {
        &self.0
    }
}

impl<'cg, 'ctx> std::ops::BitAnd<&Self> for BoolRef<'cg, 'ctx> {
    type Output = BoolRef<'cg, 'ctx>;

    fn bitand(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.and(&mut self.0, &rhs.0);
        self
    }
}

impl<'cg, 'ctx> std::ops::BitOr<&Self> for BoolRef<'cg, 'ctx> {
    type Output = BoolRef<'cg, 'ctx>;

    fn bitor(mut self, rhs: &Self) -> Self::Output {
        let cg = self.0.cg;
        cg.or(&mut self.0, &rhs.0);
        self
    }
}

// TODO: Add support for typed pointers
pub struct UntypedPtrRef<'cg, 'ctx> (pub CGValueRef<'cg, 'ctx>);

impl<'cg, 'ctx> Deref for UntypedPtrRef<'cg, 'ctx> {
    type Target = CGValueRef<'cg, 'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'cg, 'ctx> Into<CGValueRef<'cg, 'ctx>> for UntypedPtrRef<'cg, 'ctx> {
    fn into(self) -> CGValueRef<'cg, 'ctx> {
        self.0
    }
}

impl<'cg, 'ctx> From<CGValueRef<'cg, 'ctx>> for UntypedPtrRef<'cg, 'ctx> {
    fn from(v: CGValueRef<'cg, 'ctx>) -> Self {
        UntypedPtrRef(v)
    }
}

impl Clone for UntypedPtrRef<'_, '_> {
    fn clone(&self) -> Self {
        let cg = self.0.cg;
        let clone = UntypedPtrRef(cg.clone_value(&self.0));
        clone
    }
}

// TODO: also offer stuff like "dereference at offset"

pub trait PtrRefByteOffset<Other> {
    fn byte_offset(self, byte_offset: Other) -> Self;
}

impl<'cg, 'ctx> UntypedPtrRef<'cg, 'ctx> {
    pub fn new(i: usize, cg: &'cg CodeGen<'ctx>) -> Self {
        UntypedPtrRef(CGValueRef::new(i, cg, DataType::Ptr))
    }
}

impl<'cg, 'ctx> PtrRefByteOffset<&I64Ref<'cg, 'ctx>> for UntypedPtrRef<'cg, 'ctx> {
    fn byte_offset(mut self, byte_offset: &I64Ref<'cg, 'ctx>) -> Self {
        let cg = self.0.cg;
        cg.add(&mut self.0, &byte_offset.0);
        self
    }
}

impl<'cg, 'ctx> PtrRefByteOffset<i64> for UntypedPtrRef<'cg, 'ctx>  {
    fn byte_offset(mut self, byte_offset: i64) -> Self {
        let byte_offset = self.cg.new_i64_const(byte_offset);
        let cg = self.0.cg;
        cg.add(&mut self.0, &byte_offset.0);
        self
    }
}

impl<'cg, 'ctx> CGEq<'cg, 'ctx, &Self> for UntypedPtrRef<'cg, 'ctx> {
    fn cg_eq(mut self, other: &Self) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        cg.eq(&mut self.0, &other.0);
        BoolRef(self.0)
    }

    fn cg_neq(mut self, other: &Self) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        cg.neq(&mut self.0, &other.0);
        BoolRef(self.0)
    }
}

impl<'cg, 'ctx> CGCmp<'cg, 'ctx, &Self> for UntypedPtrRef<'cg, 'ctx> {
    fn cg_lt(mut self, other: &Self) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        cg.lt(&mut self.0, &other.0);
        BoolRef(self.0)
    }

    fn cg_lte(mut self, other: &Self) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        cg.lte(&mut self.0, &other.0);
        BoolRef(self.0)
    }

    fn cg_gt(mut self, other: &Self) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        cg.gt(&mut self.0, &other.0);
        BoolRef(self.0)
    }

    fn cg_gte(mut self, other: &Self) -> BoolRef<'cg, 'ctx> {
        let cg = self.0.cg;
        cg.gte(&mut self.0, &other.0);
        BoolRef(self.0)
    }
}

impl<'cg, 'ctx> UntypedPtrRef<'cg, 'ctx> {
    // We only take a reference here because we don't overwrite the pointer
    // Since the value we are loading could have a different width
    pub fn load_from(&self, data_type: DataType) -> CGValueRef<'cg, 'ctx> {
        let new_var = self.0.cg.new_var(data_type);
        self.cg.deref_ptr(self.inner.into_value_i(), new_var.inner.into_value_i());
        new_var
    }

    #[allow(dead_code)]
    pub fn write_to(&self, value: &CGValueRef<'cg, 'ctx>) {
        self.cg.write_to_ptr(self.inner.into_value_i(), &value);
    }
}

#[allow(private_bounds)]
pub trait TypedPtrRefOffset<'cg, 'ctx: 'cg, Offset, PtrType: PtrTarget<'cg, 'ctx>> {
    fn typed_offset(&self, offset: Offset) -> Self;
}

#[allow(private_bounds)]
pub struct TypedPtrRef<'cg, 'ctx, PtrType: PtrTarget<'cg, 'ctx>> {
    pub ptr: UntypedPtrRef<'cg, 'ctx>,
    _phantom: std::marker::PhantomData<PtrType>
}

impl<'cg, 'ctx, PtrType: PtrTarget<'cg, 'ctx>> Deref for TypedPtrRef<'cg, 'ctx, PtrType> {
    type Target = UntypedPtrRef<'cg, 'ctx>;

    fn deref(&self) -> &Self::Target {
        &self.ptr
    }
}

impl<'cg, 'ctx, PtrType: PtrTarget<'cg, 'ctx>> Clone for TypedPtrRef<'cg,  'ctx, PtrType> {
    fn clone(&self) -> Self {
        TypedPtrRef { ptr: self.ptr.clone(), _phantom: std::marker::PhantomData }
    }
}

impl<'cg, 'ctx, PtrType: PtrTarget<'cg, 'ctx>> Into<UntypedPtrRef<'cg, 'ctx>> for TypedPtrRef<'cg,  'ctx, PtrType> {
    fn into(self) -> UntypedPtrRef<'cg, 'ctx> {
        self.ptr
    }
}

impl<'cg, 'ctx, PtrType: PtrTarget<'cg, 'ctx>> From<UntypedPtrRef<'cg, 'ctx>> for TypedPtrRef<'cg,  'ctx, PtrType> {
    fn from(ptr: UntypedPtrRef<'cg, 'ctx>) -> Self {
        TypedPtrRef { ptr, _phantom: std::marker::PhantomData }
    }
}

impl<'cg, 'ctx, PtrType: PtrTarget<'cg, 'ctx>> From<CGValueRef<'cg, 'ctx>> for TypedPtrRef<'cg,  'ctx, PtrType> {
    fn from(ptr: CGValueRef<'cg, 'ctx>) -> Self {
        TypedPtrRef { ptr: ptr.into(), _phantom: std::marker::PhantomData }
    }
}

impl<'cg, 'ctx, PtrType: PtrTarget<'cg, 'ctx>> Into<CGValueRef<'cg, 'ctx>> for TypedPtrRef<'cg, 'ctx, PtrType> {
    fn into(self) -> CGValueRef<'cg, 'ctx> {
        self.ptr.0
    }
}

impl<'cg, 'ctx, PtrType: PtrTarget<'cg, 'ctx>> PtrRefByteOffset<i64> for TypedPtrRef<'cg,  'ctx, PtrType> {
    fn byte_offset(self, byte_offset: i64) -> Self {
        let ptr = self.ptr.byte_offset(byte_offset);
        TypedPtrRef { ptr, _phantom: std::marker::PhantomData }
    }
}

impl<'cg, 'ctx, PtrType: PtrTarget<'cg, 'ctx>,> PtrRefByteOffset<&I64Ref<'cg, 'ctx>> for TypedPtrRef<'cg,  'ctx, PtrType> {
    fn byte_offset(self, byte_offset: &I64Ref<'cg, 'ctx>) -> Self {
        let ptr = self.ptr.byte_offset(byte_offset);
        TypedPtrRef { ptr, _phantom: std::marker::PhantomData }
    }
}

impl<'cg, 'ctx, PtrType: PtrTarget<'cg, 'ctx>,> Setable<&Self> for TypedPtrRef<'cg,  'ctx, PtrType> {
    fn set(&self, other: &Self) {
        self.cg.copy_value(&self.ptr.0, &other.ptr.0);
    }
}

#[allow(private_bounds)]
impl<'cg, 'ctx, PtrType: PtrTarget<'cg, 'ctx>,> TypedPtrRef<'cg,  'ctx, PtrType> {
    pub fn read(&self) -> PtrType {
        let value = self.ptr.load_from(PtrType::get_data_type());
        PtrType::from(value)
    }

    #[allow(dead_code)]
    pub fn write(&self, value: &PtrType) {
        self.ptr.write_to(&value.get_inner());
    }
}

impl<'cg, 'ctx, PtrType: PtrTarget<'cg, 'ctx>> TypedPtrRefOffset<'cg, 'ctx, i64, PtrType> for TypedPtrRef<'cg,  'ctx, PtrType> {
    fn typed_offset(&self, offset: i64) -> Self {
        let ptr = self.ptr.clone().byte_offset(offset * get_data_type_size(&PtrType::get_data_type()) as i64);
        TypedPtrRef { ptr, _phantom: std::marker::PhantomData }
    }
}

impl<'cg, 'ctx, PtrType: PtrTarget<'cg, 'ctx>> TypedPtrRefOffset<'cg, 'ctx, &I64Ref<'cg, 'ctx>, PtrType> for TypedPtrRef<'cg,  'ctx, PtrType> {
    fn typed_offset(&self, offset: &I64Ref<'cg, 'ctx>) -> Self {
        let ptr = self.ptr.clone().byte_offset(&(offset.clone() * get_data_type_size(&PtrType::get_data_type()) as i64));
        TypedPtrRef { ptr, _phantom: std::marker::PhantomData }
    }
}
