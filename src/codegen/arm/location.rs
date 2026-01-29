use crate::codegen::arm::{address::LiteralIndexAddress, reg::Reg, value::Value, Address};

/// Somewhere that holds a value.
#[derive(Clone, Copy)]
pub enum Location {
    Address(Address),
    Reg(Reg),
}

impl Location {
    pub const NOWHERE: Self = Location::Address(Address::LiteralIndex(LiteralIndexAddress {
        base: Reg::LinkReg,
        offset: i32::MAX,
    }));

    pub const fn is_nowhere(self) -> bool {
        matches!(
            self,
            Location::Address(Address::LiteralIndex(LiteralIndexAddress {
                base: Reg::LinkReg,
                offset: i32::MAX
            }))
        )
    }
}

impl From<Address> for Location {
    fn from(value: Address) -> Self {
        Location::Address(value)
    }
}

impl From<Reg> for Location {
    fn from(value: Reg) -> Self {
        Location::Reg(value)
    }
}

impl TryFrom<Value> for Location {
    type Error = ();

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Address(address) => Ok(address.into()),
            Value::Reg(reg) => Ok(reg.into()),
            _ => Err(()),
        }
    }
}
