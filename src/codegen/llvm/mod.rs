pub mod stencils;

use inkwell::{builder::Builder, values::IntValue};

use super::ir::DataType;

pub fn int_add<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    builder.build_int_add(x, y, "add").unwrap()
}
pub fn int_sub<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    builder.build_int_sub(x, y, "sub").unwrap()
}
pub fn int_mul<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    builder.build_int_mul(x, y, "mul").unwrap()
}
pub fn int_div<'ctx>(builder: &Builder<'ctx>, d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    if d_type.is_signed() {
        builder.build_int_signed_div(x, y, "div").unwrap()
    } else {
        builder.build_int_unsigned_div(x, y, "div").unwrap()
    }
}
pub fn int_rem<'ctx>(builder: &Builder<'ctx>, d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    if d_type.is_signed() {
        builder.build_int_signed_rem(x, y, "rem").unwrap()
    } else {
        builder.build_int_unsigned_rem(x, y, "rem").unwrap()
    }
}
pub fn int_and<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    builder.build_and(x, y, "and").unwrap()
}
pub fn int_or<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    builder.build_or(x, y, "or").unwrap()
}
pub fn int_xor<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    builder.build_xor(x, y, "xor").unwrap()
}
pub fn int_shl<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    builder.build_left_shift(x, y, "shl").unwrap()
}
pub fn int_shr<'ctx>(builder: &Builder<'ctx>, d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    if d_type.is_signed() {
        builder.build_right_shift(x, y, true, "ashr").unwrap()
    } else {
        builder.build_right_shift(x, y, false, "lshr").unwrap()
    }
}
pub fn int_eq<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    builder.build_int_compare(inkwell::IntPredicate::EQ, x, y, "eq").unwrap()
}
pub fn int_ne<'ctx>(builder: &Builder<'ctx>, _d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    builder.build_int_compare(inkwell::IntPredicate::NE, x, y, "ne").unwrap()
}
pub fn int_gt<'ctx>(builder: &Builder<'ctx>, d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    if d_type.is_signed() {
        builder.build_int_compare(inkwell::IntPredicate::SGT, x, y, "sgt").unwrap()
    } else {
        builder.build_int_compare(inkwell::IntPredicate::UGT, x, y, "ugt").unwrap()
    }
}
pub fn int_gte<'ctx>(builder: &Builder<'ctx>, d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    if d_type.is_signed() {
        builder.build_int_compare(inkwell::IntPredicate::SGE, x, y, "sge").unwrap()
    } else {
        builder.build_int_compare(inkwell::IntPredicate::UGE, x, y, "uge").unwrap()
    }
}
pub fn int_lt<'ctx>(builder: &Builder<'ctx>, d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    if d_type.is_signed() {
        builder.build_int_compare(inkwell::IntPredicate::SLT, x, y, "slt").unwrap()
    } else {
        builder.build_int_compare(inkwell::IntPredicate::ULT, x, y, "ult").unwrap()
    }
}
pub fn int_lte<'ctx>(builder: &Builder<'ctx>, d_type: DataType, x: IntValue<'ctx>, y: IntValue<'ctx>) -> IntValue<'ctx> {
    if d_type.is_signed() {
        builder.build_int_compare(inkwell::IntPredicate::SLE, x, y, "sle").unwrap()
    } else {
        builder.build_int_compare(inkwell::IntPredicate::ULE, x, y, "ule").unwrap()
    }
}
pub fn int_not<'ctx>(builder: &Builder<'ctx>, x: IntValue<'ctx>) -> IntValue<'ctx> {
    builder.build_not(x, "not").unwrap()
}
