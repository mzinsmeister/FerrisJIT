

// Long term we may need a full blown own IR but for now this houses some data types
// for identifying stencils. For now i will try without my own IR, also because it increases
// codegen/compile times compared to just emitting binary directly.

use std::fmt::{self, Display, Formatter};

use inkwell::{types::IntType, AddressSpace};

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DataType {
    // Integer Types
    Bool, // Special Case (treated as 1 bit integer type in LLVM)
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    // Floating Point Types
    F32,
    F64,
    // Untyped Pointer. For the semantics of operations on values of this type it's enough.
    // Upper level code has to tell us how to interpret dereferenced values
    Ptr,
}

impl<'a> TryFrom<IntType<'a>> for DataType {
    // TODO: Proper Error Type
    type Error = ();

    fn try_from(value: IntType<'a>) -> Result<Self, Self::Error> {
        let bit_width = value.get_bit_width();

        match bit_width {
            1 => Ok(DataType::Bool),
            8 => Ok(DataType::I8),
            16 => Ok(DataType::I16),
            32 => Ok(DataType::I32),
            64 => Ok(DataType::I64),
            _ => Err(()),
        }
    }
}

#[allow(dead_code)]
impl DataType {
    pub fn get_llvm_type<'ctx>(&self, context: &'ctx inkwell::context::Context) -> inkwell::types::BasicTypeEnum<'ctx> {
        match self {
            DataType::I8 | DataType::U8 => context.i8_type().into(),
            DataType::I16 | DataType::U16 => context.i16_type().into(),
            DataType::I32 | DataType::U32 => context.i32_type().into(),
            DataType::I64 | DataType::U64 => context.i64_type().into(),
            DataType::F32 => context.f32_type().into(),
            DataType::F64 => context.f64_type().into(),
            DataType::Bool => context.bool_type().into(),
            DataType::Ptr => {
                // TODO: This is essentially a hack. But should work for now. We should probably
                //       not use this method for pointer types anyway. And for generic pointer op stencils and
                //       stuff like that it should be fine.
                let inner_type = context.i8_type();  
                inner_type.ptr_type(inkwell::AddressSpace::default()).into()
            }
        }
    }

    pub fn get_llvm_ptr_type<'ctx>(&self, context: &'ctx inkwell::context::Context) -> inkwell::types::PointerType<'ctx> {
        match self {
            DataType::I8 | DataType::U8 => context.i8_type().ptr_type(inkwell::AddressSpace::default()),
            DataType::I16 | DataType::U16 => context.i16_type().ptr_type(inkwell::AddressSpace::default()),
            DataType::I32 | DataType::U32 => context.i32_type().ptr_type(inkwell::AddressSpace::default()),
            DataType::I64 | DataType::U64 => context.i64_type().ptr_type(inkwell::AddressSpace::default()),
            DataType::F32 => context.f32_type().ptr_type(inkwell::AddressSpace::default()),
            DataType::F64 => context.f64_type().ptr_type(inkwell::AddressSpace::default()),
            DataType::Bool => context.bool_type().ptr_type(inkwell::AddressSpace::default()),
            DataType::Ptr => {
                let inner_type = context.i8_type().ptr_type(AddressSpace::default());
                inner_type.ptr_type(inkwell::AddressSpace::default())
            }
        }
    }

    pub fn flip_signed(&self) -> Option<Self> {
        match self {
            DataType::I8 => Some(DataType::U8),
            DataType::I16 => Some(DataType::U16),
            DataType::I32 => Some(DataType::U32),
            DataType::I64 => Some(DataType::U64),
            DataType::U8 => Some(DataType::I8),
            DataType::U16 => Some(DataType::I16),
            DataType::U32 => Some(DataType::I32),
            DataType::U64 => Some(DataType::I64),
            _ => None,
        }
    }

    pub fn is_signed(&self) -> bool {
        match self {
            DataType::I8 | DataType::I16 | DataType::I32 | DataType::I64 => true,
            _ => false,
        }
    }

    pub fn is_unsigned(&self) -> bool {
        match self {
            DataType::U8 | DataType::U16 | DataType::U32 | DataType::U64 => true,
            _ => false,
        }
    }

    pub fn is_integer(&self) -> bool {
        match self {
            DataType::I8 | DataType::I16 | DataType::I32 | DataType::I64 | DataType::U8 | DataType::U16 | DataType::U32 | DataType::U64 => true,
            _ => false,
        }
    }

    pub fn is_float(&self) -> bool {
        match self {
            DataType::F32 | DataType::F64 => true,
            _ => false,
        }
    }

    pub fn is_pointer(&self) -> bool {
        match self {
            DataType::Ptr => true,
            _ => false,
        }
    }

    pub fn is_scalar(&self) -> bool {
        self.is_integer() || self.is_float() || self.is_pointer()
    }

    pub fn get_same_width_uint(&self) -> Self {
        match self {
            DataType::Bool => DataType::Bool,
            DataType::I8 | DataType::U8 => DataType::U8,
            DataType::I16 | DataType::U16 => DataType::U16,
            DataType::I32 | DataType::U32 | DataType::F32 => DataType::U32,
            DataType::I64 | DataType::U64 | DataType::F64 | DataType::Ptr => DataType::U64, // We assume we run on 64 bit systems
        }
    }
}

impl Display for DataType {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            DataType::I8 => write!(f, "i8"),
            DataType::I16 => write!(f, "i16"),
            DataType::I32 => write!(f, "i32"),
            DataType::I64 => write!(f, "i64"),
            DataType::U8 => write!(f, "u8"),
            DataType::U16 => write!(f, "u16"),
            DataType::U32 => write!(f, "u32"),
            DataType::U64 => write!(f, "u64"),
            DataType::F32 => write!(f, "f32"),
            DataType::F64 => write!(f, "f64"),
            DataType::Bool => write!(f, "bool"),
            DataType::Ptr => write!(f, "ptr"),
        }
    }
}

#[allow(dead_code)]
#[derive(Debug, Copy, Clone, PartialEq, PartialOrd)]
pub enum ConstValue {
    Bool(bool),
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
}

impl ConstValue {
    pub fn get_type(&self) -> DataType {
        match self {
            ConstValue::Bool(_) => DataType::Bool,
            ConstValue::I8(_) => DataType::I8,
            ConstValue::I16(_) => DataType::I16,
            ConstValue::I32(_) => DataType::I32,
            ConstValue::I64(_) => DataType::I64,
            ConstValue::U8(_) => DataType::U8,
            ConstValue::U16(_) => DataType::U16,
            ConstValue::U32(_) => DataType::U32,
            ConstValue::U64(_) => DataType::U64,
            ConstValue::F32(_) => DataType::F32,
            ConstValue::F64(_) => DataType::F64,
        }
    }

    pub fn bitcast_to_u64(&self) -> u64 {
        match self {
            ConstValue::Bool(b) => if *b { 1 } else { 0 },
            ConstValue::I8(i) => *i as u64,
            ConstValue::I16(i) => *i as u64,
            ConstValue::I32(i) => *i as u64,
            ConstValue::I64(i) => *i as u64,
            ConstValue::U8(u) => *u as u64,
            ConstValue::U16(u) => *u as u64,
            ConstValue::U32(u) => *u as u64,
            ConstValue::U64(u) => *u,
            ConstValue::F32(f) => f.to_bits() as u64,
            ConstValue::F64(f) => f.to_bits(),
        }
    }

    pub fn bit_not(&self) -> ConstValue {
        match self {
            ConstValue::Bool(b) => ConstValue::Bool(!b),
            ConstValue::I8(i) => ConstValue::I8(!i),
            ConstValue::I16(i) => ConstValue::I16(!i),
            ConstValue::I32(i) => ConstValue::I32(!i),
            ConstValue::I64(i) => ConstValue::I64(!i),
            ConstValue::U8(u) => ConstValue::U8(!u),
            ConstValue::U16(u) => ConstValue::U16(!u),
            ConstValue::U32(u) => ConstValue::U32(!u),
            ConstValue::U64(u) => ConstValue::U64(!u),
            _ => panic!("Bitwise not is not defined for floating point types"),
        }
    }
}
