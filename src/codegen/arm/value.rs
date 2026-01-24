use crate::codegen::arm::{
    file_builder::StringId, location::Location, reg::Reg, reg_or_imm::RegOrImm, Address, Imm,
};

/// A value that may or may not have an associated location in memory, or a register that holds it.
#[derive(Clone, Copy)]
pub enum Value {
    Address(Address),
    Reg(Reg),
    Imm(Imm),
}

/// The width a value takes up somewhere.
#[derive(Debug)]
pub enum ValueWidth {
    Byte,
    Word,
    DWord,
    QWord,
}

impl TryFrom<u32> for ValueWidth {
    type Error = ();

    fn try_from(value: u32) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(ValueWidth::Byte),
            2 => Ok(ValueWidth::Word),
            4 => Ok(ValueWidth::DWord),
            8 => Ok(ValueWidth::QWord),
            _ => Err(()),
        }
    }
}

impl From<ValueWidth> for u32 {
    fn from(value: ValueWidth) -> Self {
        match value {
            ValueWidth::Byte => 1,
            ValueWidth::Word => 2,
            ValueWidth::DWord => 4,
            ValueWidth::QWord => 8,
        }
    }
}

impl From<Location> for Value {
    fn from(value: Location) -> Self {
        match value {
            Location::Address(address) => address.into(),
            Location::Reg(reg) => reg.into(),
        }
    }
}

impl From<Address> for Value {
    fn from(value: Address) -> Self {
        Value::Address(value)
    }
}

impl From<RegOrImm> for Value {
    fn from(value: RegOrImm) -> Self {
        match value {
            RegOrImm::Reg(reg) => Value::Reg(reg),
            RegOrImm::Imm(imm) => Value::Imm(imm),
        }
    }
}

impl From<Reg> for Value {
    fn from(value: Reg) -> Self {
        Value::Reg(value)
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Value::Imm(value.into())
    }
}

impl From<u32> for Value {
    fn from(value: u32) -> Self {
        Value::Imm(value.into())
    }
}

impl From<f32> for Value {
    fn from(value: f32) -> Self {
        Value::Imm(value.into())
    }
}

impl From<StringId> for Value {
    fn from(value: StringId) -> Self {
        Value::Imm(value.into())
    }
}
