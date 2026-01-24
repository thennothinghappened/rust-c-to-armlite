use std::ops::{Add, Sub};

use crate::codegen::arm::reg::Reg;

#[derive(Clone, Copy)]
pub enum Address {
    RelativeIndex(RelativeIndexAddress),
    LiteralIndex { base: Reg, offset: i32 },
}

#[derive(Clone, Copy)]
pub struct RelativeIndexAddress {
    pub base: Reg,
    pub offset: Reg,
    pub negate_offset: bool,
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
        Self::LiteralIndex { base, offset }
    }

    pub const fn base(&self) -> Reg {
        match self {
            Address::RelativeIndex(relative_index_address) => relative_index_address.base,
            Address::LiteralIndex { base, offset: _ } => *base,
        }
    }
}

impl From<RelativeIndexAddress> for Address {
    fn from(value: RelativeIndexAddress) -> Self {
        Self::RelativeIndex(value)
    }
}

impl Add<i32> for Reg {
    type Output = Address;

    fn add(self, rhs: i32) -> Self::Output {
        Address::offset(self, rhs)
    }
}

impl Sub<i32> for Reg {
    type Output = Address;

    fn sub(self, rhs: i32) -> Self::Output {
        Address::offset(self, -rhs)
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
