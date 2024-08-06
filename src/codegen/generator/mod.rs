use std::os::raw::c_void;

use super::ir::{ConstValue, DataType};

pub mod copy_patch;
pub mod llvm;

// A generator is the abstraction of a backend for code generation

pub trait Generator {
    fn init(&mut self);
    fn map_arg(&mut self, arg_n: usize, n: usize);
    fn new_var(&mut self, n: usize);
    fn new_const(&mut self, c: ConstValue, n: usize);
    fn init_var(&mut self, c: ConstValue, n: usize);
    fn free_value(&mut self, n: usize);
    fn copy_value(&mut self, n: usize, m: usize);
    fn add(&mut self, n: usize, m: usize);
    fn add_const(&mut self, n: usize, c: ConstValue);
    fn sub(&mut self, n: usize, m: usize);
    fn sub_const(&mut self, n: usize, c: ConstValue);
    fn mul(&mut self, n: usize, m: usize);
    fn mul_const(&mut self, n: usize, c: ConstValue);
    fn div(&mut self, n: usize, m: usize);
    fn div_const(&mut self, n: usize, c: ConstValue);
    fn rem(&mut self, n: usize, m: usize);
    fn rem_const(&mut self, n: usize, c: ConstValue);
    fn eq(&mut self, n: usize, m: usize);
    fn neq(&mut self, n: usize, m: usize);
    fn lt(&mut self, n: usize, m: usize);
    fn lte(&mut self, n: usize, m: usize);
    fn gt(&mut self, n: usize, m: usize);
    fn gte(&mut self, n: usize, m: usize);
    fn and(&mut self, n: usize, m: usize);
    fn or(&mut self, n: usize, m: usize);
    fn not(&mut self, n: usize);
    fn gep(&mut self, pointee_type: DataType, ptr: usize, idx: usize);
    fn deref_ptr(&mut self, pointee_type: DataType, ptr_i: usize, target_i: usize);
    //fn get_ptr(&mut self, n: usize);
    fn write_to_ptr(&mut self, ptr_i: usize, value_i: usize);
    fn ret(&mut self, n: Option<usize>);
    fn c_call(&mut self, func: *const c_void, args_ptr: usize);

    // If
    fn pre_if_then(&mut self, condition: usize);
    fn post_if_then(&mut self);
    // If end

    // If-Else
    fn pre_if(&mut self, condition: usize);
    fn pre_then(&mut self);
    fn post_then(&mut self);
    fn pre_else(&mut self);
    fn post_else(&mut self);
    fn post_if(&mut self);
    // If-Else end

    // Loop
    fn pre_loop_cond(&mut self);
    fn pre_loop_body(&mut self, condition: usize);
    fn post_loop_body(&mut self);
    // Loop end
}