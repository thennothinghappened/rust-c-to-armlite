use crate::parser::program::{statement::Block, TypeId};

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Inline(TypeInfo),
    WithId(TypeId),
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeInfo {
    Pointer(Box<Type>),
    BuiltIn(BuiltInType),
    Struct(Struct),
    Enum(Enum),
    Function(Function),
    Const(Box<Type>),
}

impl From<TypeInfo> for Type {
    fn from(value: TypeInfo) -> Self {
        Self::Inline(value)
    }
}

impl From<TypeId> for Type {
    fn from(value: TypeId) -> Self {
        Self::WithId(value)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BuiltInType {
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

impl From<BuiltInType> for TypeInfo {
    fn from(value: BuiltInType) -> Self {
        Self::BuiltIn(value)
    }
}

impl From<BuiltInType> for Type {
    fn from(value: BuiltInType) -> Self {
        Self::Inline(value.into())
    }
}

impl BuiltInType {
    /// Convert this type to an unsigned version, if one exists.
    pub fn unsigned(self) -> Option<Self> {
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
    pub fn signed(self) -> Option<Self> {
        match self {
            Self::UnsignedChar => Some(Self::SignedChar),
            Self::UnsignedShort => Some(Self::Short),
            Self::UnsignedInt => Some(Self::Int),
            Self::UnsignedLong => Some(Self::Long),
            Self::UnsignedLongLong => Some(Self::LongLong),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub args: Vec<Member>,
    pub return_type: Box<Type>,
    pub body: Option<Block>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub members: Option<Vec<Member>>,
}

impl Struct {
    pub fn opaque() -> Self {
        Self { members: None }
    }
}

impl From<Struct> for TypeInfo {
    fn from(value: Struct) -> Self {
        TypeInfo::Struct(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    pub name: Option<String>,
    pub type_info: Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Enum {
    pub entries: Vec<EnumMember>,
}

impl From<Enum> for TypeInfo {
    fn from(value: Enum) -> Self {
        TypeInfo::Enum(value)
    }
}

pub type EnumMember = (String, i32);

/// A type definition, which aliases the given `name` to reference the `target_type`.
#[derive(Debug)]
pub struct TypeDef {
    pub name: String,
    pub target_type: Type,
}
