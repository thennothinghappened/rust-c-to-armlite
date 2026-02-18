use std::ops::{Add, Sub};

use crate::codegen::arm::reg::Reg;

#[derive(Debug, Clone, Copy)]
pub enum Address {
    RelativeIndex(RelativeIndexAddress),
    LiteralIndex(LiteralIndexAddress),
}

#[derive(Debug, Clone, Copy)]
pub struct RelativeIndexAddress {
    pub base: Reg,
    pub offset: Reg,
    pub negate_offset: bool,
}

#[derive(Debug, Clone, Copy)]
pub struct LiteralIndexAddress {
    pub base: Reg,
    pub offset: i32,
}

impl Address {
    pub const fn at(base: Reg) -> Self {
        Self::offset(base, 0)
    }

    pub const fn relative(base: Reg, offset: Reg, negate_offset: bool) -> Self {
        Self::RelativeIndex(RelativeIndexAddress {
            base,
            offset,
            negate_offset,
        })
    }

    pub const fn offset(base: Reg, offset: i32) -> Self {
        Self::LiteralIndex(LiteralIndexAddress { base, offset })
    }

    pub const fn base(&self) -> Reg {
        match self {
            Address::RelativeIndex(relative_index_address) => relative_index_address.base,
            Address::LiteralIndex(LiteralIndexAddress { base, offset: _ }) => *base,
        }
    }
}

impl LiteralIndexAddress {
    pub const fn at(base: Reg) -> Self {
        Self { base, offset: 0 }
    }
}

impl From<RelativeIndexAddress> for Address {
    fn from(value: RelativeIndexAddress) -> Self {
        Self::RelativeIndex(value)
    }
}

impl From<LiteralIndexAddress> for Address {
    fn from(value: LiteralIndexAddress) -> Self {
        Self::LiteralIndex(value)
    }
}

impl Add<i32> for Reg {
    type Output = LiteralIndexAddress;

    fn add(self, rhs: i32) -> Self::Output {
        LiteralIndexAddress {
            base: self,
            offset: rhs,
        }
    }
}

impl Sub<i32> for Reg {
    type Output = LiteralIndexAddress;

    fn sub(self, rhs: i32) -> Self::Output {
        LiteralIndexAddress {
            base: self,
            offset: -rhs,
        }
    }
}

impl Add<Self> for Reg {
    type Output = RelativeIndexAddress;

    fn add(self, rhs: Self) -> Self::Output {
        RelativeIndexAddress {
            base: self,
            offset: rhs,
            negate_offset: false,
        }
    }
}

impl Sub<Self> for Reg {
    type Output = RelativeIndexAddress;

    fn sub(self, rhs: Self) -> Self::Output {
        RelativeIndexAddress {
            base: self,
            offset: rhs,
            negate_offset: true,
        }
    }
}
