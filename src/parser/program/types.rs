use std::fmt::Display;

use crate::{
    id_type,
    parser::program::statement::{Block, Statement},
};

id_type!(CTypeId);
id_type!(CStructId);
id_type!(CEnumId);
id_type!(CFuncTypeId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CType {
    AsIs(CConcreteType),
    PointerTo(CTypeId, Option<u32>),
}

impl CType {
    pub const fn pointer_to(id: CTypeId) -> CType {
        CType::PointerTo(id, None)
    }

    pub const fn array_of(id: CTypeId, size: u32) -> CType {
        CType::PointerTo(id, Some(size))
    }

    pub const fn is_pointer(&self) -> bool {
        matches!(self, CType::PointerTo(_, _))
    }
}

/// A "final" type, which represents a built-in numeric type, struct, or enum. The latter two may be
/// fetched using their specified ID.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CConcreteType {
    Struct(CStructId),
    Enum(CEnumId),
    Func(CFuncTypeId),
    Primitive(CPrimitive),
    Void,
}

impl CConcreteType {
    pub const fn unsigned(self) -> Option<Self> {
        let CConcreteType::Primitive(primitive) = self else {
            return None;
        };

        match primitive.unsigned() {
            Some(unsigned) => Some(Self::Primitive(unsigned)),
            None => None,
        }
    }

    pub const fn signed(self) -> Option<Self> {
        let CConcreteType::Primitive(primitive) = self else {
            return None;
        };

        match primitive.signed() {
            Some(signed) => Some(Self::Primitive(signed)),
            None => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CPrimitive {
    Bool,
    Char,
    SignedChar,
    UnsignedChar,
    Short,
    UnsignedShort,
    Int,
    UnsignedInt,
    Long,
    UnsignedLong,
    LongLong,
    UnsignedLongLong,
    Float,
    Double,
    LongDouble,
}

impl CPrimitive {
    /// Convert this type to an unsigned version, if one exists.
    pub const fn unsigned(self) -> Option<Self> {
        match self {
            Self::Char => Some(Self::UnsignedChar),
            Self::Short => Some(Self::UnsignedShort),
            Self::Int => Some(Self::UnsignedInt),
            Self::Long => Some(Self::UnsignedLong),
            Self::LongLong => Some(Self::UnsignedLongLong),
            _ => None,
        }
    }

    /// Convert this type to a signed version, if one exists.
    pub const fn signed(self) -> Option<Self> {
        match self {
            Self::UnsignedChar => Some(Self::SignedChar),
            Self::UnsignedShort => Some(Self::Short),
            Self::UnsignedInt => Some(Self::Int),
            Self::UnsignedLong => Some(Self::Long),
            Self::UnsignedLongLong => Some(Self::LongLong),
            _ => None,
        }
    }

    pub const fn is_integer_type(self) -> bool {
        match self {
            CPrimitive::Bool => true,
            CPrimitive::Char => true,
            CPrimitive::SignedChar => true,
            CPrimitive::UnsignedChar => true,
            CPrimitive::Short => true,
            CPrimitive::UnsignedShort => true,
            CPrimitive::Int => true,
            CPrimitive::UnsignedInt => true,
            CPrimitive::Long => true,
            CPrimitive::UnsignedLong => true,
            CPrimitive::LongLong => true,
            CPrimitive::UnsignedLongLong => true,
            CPrimitive::Float => false,
            CPrimitive::Double => false,
            CPrimitive::LongDouble => false,
        }
    }

    pub const fn is_floating_type(self) -> bool {
        matches!(
            self,
            CPrimitive::Float | CPrimitive::Double | CPrimitive::LongDouble
        )
    }

    pub const fn is_real_type(self) -> bool {
        self.is_integer_type() | self.is_floating_type()
    }
}

impl Display for CPrimitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                CPrimitive::Bool => "bool",
                CPrimitive::Char => "char",
                CPrimitive::SignedChar => "signed char",
                CPrimitive::UnsignedChar => "unsigned char",
                CPrimitive::Short => "short",
                CPrimitive::UnsignedShort => "unsigned short",
                CPrimitive::Int => "int",
                CPrimitive::UnsignedInt => "unsigned int",
                CPrimitive::Long => "long",
                CPrimitive::UnsignedLong => "unsigned long",
                CPrimitive::LongLong => "long long",
                CPrimitive::UnsignedLongLong => "unsigned long long",
                CPrimitive::Float => "float",
                CPrimitive::Double => "double",
                CPrimitive::LongDouble => "long double",
            }
        )
    }
}

impl From<CPrimitive> for CConcreteType {
    fn from(value: CPrimitive) -> Self {
        Self::Primitive(value)
    }
}

impl From<CStructId> for CConcreteType {
    fn from(value: CStructId) -> Self {
        Self::Struct(value)
    }
}

impl From<CEnumId> for CConcreteType {
    fn from(value: CEnumId) -> Self {
        Self::Enum(value)
    }
}

impl From<CFuncTypeId> for CConcreteType {
    fn from(value: CFuncTypeId) -> Self {
        Self::Func(value)
    }
}

impl From<CConcreteType> for CType {
    fn from(value: CConcreteType) -> Self {
        Self::AsIs(value)
    }
}

impl From<CPrimitive> for CType {
    fn from(value: CPrimitive) -> Self {
        Self::AsIs(CConcreteType::Primitive(value))
    }
}

impl From<CFuncTypeId> for CType {
    fn from(value: CFuncTypeId) -> Self {
        Self::AsIs(CConcreteType::Func(value))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CFuncType {
    pub args: Vec<Member>,
    pub returns: CType,
    pub is_noreturn: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CFunc {
    pub sig_id: CFuncTypeId,
    pub body: CFuncBody,
    pub is_raw_assembly: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub enum CFuncBody {
    Defined(Block),
    Extern,
    None,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CStruct {
    pub members: Option<Vec<Member>>,
}

impl CStruct {
    pub fn opaque() -> Self {
        Self { members: None }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    pub name: Option<String>,
    pub ctype: CType,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CEnum {
    pub entries: Vec<EnumMember>,
}

pub type EnumMember = (String, i32);

/// A type definition, which aliases the given `name` to reference the `target_type`.
#[derive(Debug)]
pub struct TypeDef {
    pub name: String,
    pub ctype: CType,
}
