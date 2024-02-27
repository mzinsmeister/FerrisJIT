

// Long term we probably need a full blown own IR but for now this houses some data types
// for identifying stencils

use std::fmt::{self, Display, Formatter};

use inkwell::types::{BasicType, IntType};

#[allow(dead_code)]
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
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
    // Pointer Types
    Ptr(Box<DataType>),
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
            DataType::Ptr(inner) => {
                let inner_type = inner.get_llvm_type(context);
                inner_type.ptr_type(inkwell::AddressSpace::default()).into()
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
            DataType::Ptr(_) => true,
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
            DataType::I64 | DataType::U64 | DataType::F64 | DataType::Ptr(_) => DataType::U64, // We assume we run on 64 bit systems
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
            DataType::Ptr(inner) => write!(f, "{}-ptr", inner),
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
}

enum Operation {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}