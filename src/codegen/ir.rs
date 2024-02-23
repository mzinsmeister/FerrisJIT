

// Long term we probably need a full blown own IR but for now this houses some data types
// for identifying stencils

use std::fmt::{self, Display, Formatter};

use inkwell::types::{BasicType, IntType};

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum DataType {
    I8,
    I16,
    I32,
    I64,
    Ptr(Box<DataType>),
}

impl<'a> TryFrom<IntType<'a>> for DataType {
    // TODO: Proper Error Type
    type Error = ();

    fn try_from(value: IntType<'a>) -> Result<Self, Self::Error> {
        let bit_width = value.get_bit_width();

        match bit_width {
            8 => Ok(DataType::I8),
            16 => Ok(DataType::I16),
            32 => Ok(DataType::I32),
            64 => Ok(DataType::I64),
            _ => Err(()),
        }
    }
}

impl DataType {
    pub fn get_llvm_type<'ctx>(&self, context: &'ctx inkwell::context::Context) -> inkwell::types::BasicTypeEnum<'ctx> {
        match self {
            DataType::I8 => context.i8_type().into(),
            DataType::I16 => context.i16_type().into(),
            DataType::I32 => context.i32_type().into(),
            DataType::I64 => context.i64_type().into(),
            DataType::Ptr(inner) => {
                let inner_type = inner.get_llvm_type(context);
                inner_type.ptr_type(inkwell::AddressSpace::default()).into()
            }
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
            DataType::Ptr(inner) => write!(f, "{}-ptr", inner),
        }
    }
}