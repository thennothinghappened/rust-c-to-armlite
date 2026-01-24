use crate::codegen::arm::{file_builder::StringId, reg::Reg, Imm};

/// A register or an immediate constant value.
#[derive(Clone, Copy)]
pub enum RegOrImm {
    Reg(Reg),
    Imm(Imm),
}

impl From<Reg> for RegOrImm {
    fn from(value: Reg) -> Self {
        RegOrImm::Reg(value)
    }
}

impl From<Imm> for RegOrImm {
    fn from(value: Imm) -> Self {
        RegOrImm::Imm(value)
    }
}

impl From<i32> for RegOrImm {
    fn from(value: i32) -> Self {
        RegOrImm::Imm(Imm::I32(value))
    }
}

impl From<u32> for RegOrImm {
    fn from(value: u32) -> Self {
        RegOrImm::Imm(Imm::I32(value as i32))
    }
}

impl From<f32> for RegOrImm {
    fn from(value: f32) -> Self {
        RegOrImm::Imm(Imm::F32(value))
    }
}

impl From<StringId> for RegOrImm {
    fn from(value: StringId) -> Self {
        RegOrImm::Imm(Imm::StringId(value))
    }
}
