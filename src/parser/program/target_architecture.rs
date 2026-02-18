use phf::{map::Entries, phf_map};

use crate::parser::program::ctype::CPrimitive;

#[derive(Copy, Clone, Debug, Default)]
pub enum TargetArchitecture {
    #[default]
    ArmLite,
    ArmV7,
}

impl TargetArchitecture {
    /// 32-bit ARM exact-size type definitions.
    const ARM32_TYPE_ALIASES: phf::Map<&str, CPrimitive> = phf_map!(
        "__SIZE_TYPE__" => CPrimitive::UnsignedInt,

        "__INT8_TYPE__" => CPrimitive::SignedChar,
        "__INT16_TYPE__" => CPrimitive::Short,
        "__INT32_TYPE__" => CPrimitive::Int,
        "__INT64_TYPE__" => CPrimitive::LongLong,

        "__UINT8_TYPE__" => CPrimitive::UnsignedChar,
        "__UINT16_TYPE__" => CPrimitive::UnsignedShort,
        "__UINT32_TYPE__" => CPrimitive::UnsignedInt,
        "__UINT64_TYPE__" => CPrimitive::UnsignedLongLong,
    );

    /// Get the size of the passed primitive in bytes.
    pub const fn primitive_byte_size(&self, primitive: CPrimitive) -> u32 {
        match self {
            TargetArchitecture::ArmLite | TargetArchitecture::ArmV7 => match primitive {
                CPrimitive::Bool => 1,
                CPrimitive::Char => 1,
                CPrimitive::SignedChar => 1,
                CPrimitive::UnsignedChar => 1,
                CPrimitive::Short => 2,
                CPrimitive::UnsignedShort => 2,
                CPrimitive::Int => 4,
                CPrimitive::UnsignedInt => 4,
                CPrimitive::Long => 4,
                CPrimitive::UnsignedLong => 4,
                CPrimitive::LongLong => 8,
                CPrimitive::UnsignedLongLong => 8,
                CPrimitive::Float => 2,
                CPrimitive::Double => 4,
                CPrimitive::LongDouble => 4,
            },
        }
    }

    /// Get the promotion rank of the passed primitive.
    pub const fn rank_of_primitive(&self, primitive: CPrimitive) -> u32 {
        match self {
            TargetArchitecture::ArmLite | TargetArchitecture::ArmV7 => match primitive {
                CPrimitive::Bool => 0,
                CPrimitive::Char => 1,
                CPrimitive::SignedChar => 1,
                CPrimitive::UnsignedChar => 1,
                CPrimitive::Short => 2,
                CPrimitive::UnsignedShort => 2,
                CPrimitive::Int => 4,
                CPrimitive::UnsignedInt => 4,
                CPrimitive::Long => 4,
                CPrimitive::UnsignedLong => 4,
                CPrimitive::LongLong => 8,
                CPrimitive::UnsignedLongLong => 8,
                CPrimitive::Float => 2,
                CPrimitive::Double => 4,
                CPrimitive::LongDouble => 4,
            },
        }
    }

    /// Get the size of a machine word on this architecture.
    pub const fn word_size(&self) -> u32 {
        match self {
            TargetArchitecture::ArmLite | TargetArchitecture::ArmV7 => 4,
        }
    }

    /// Get the type aliases that are target-defined on this architecture.
    pub fn type_aliases(&self) -> Entries<&str, CPrimitive> {
        match self {
            TargetArchitecture::ArmLite | TargetArchitecture::ArmV7 => {
                Self::ARM32_TYPE_ALIASES.entries()
            }
        }
    }
}
