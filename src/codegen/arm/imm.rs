use crate::codegen::arm::file_builder::StringId;

/// A constant value.
#[derive(Clone, Copy)]
pub enum Imm {
    I32(i32),
    U32(u32),
    F32(f32),
    StringId(StringId),
}

impl From<i32> for Imm {
    fn from(value: i32) -> Self {
        Imm::I32(value)
    }
}

impl From<u32> for Imm {
    fn from(value: u32) -> Self {
        Imm::U32(value)
    }
}

impl From<f32> for Imm {
    fn from(value: f32) -> Self {
        Imm::F32(value)
    }
}

impl From<StringId> for Imm {
    fn from(value: StringId) -> Self {
        Imm::StringId(value)
    }
}
