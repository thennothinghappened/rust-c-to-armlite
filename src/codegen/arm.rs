use std::{
    fmt::Display,
    ops::{Add, Sub},
};

use itertools::Itertools;

use crate::{
    codegen::{arm::file_builder::StringId, func_builder::LabelId, Generator},
    parser::program::types::{CFunc, CFuncType},
};

pub(super) mod file_builder;
pub(super) mod func_builder;

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub(crate) enum AsmMode {
    #[default]
    ArmLite,
    ArmV7,
}

enum Inst {
    InlineAsm(String),
    Comment(String, CommentPosition),
    Label(LabelId),
    Mov(Reg, RegOrImmediate),
    Add(Reg, Reg, RegOrImmediate),
    Sub(Reg, Reg, RegOrImmediate),
    Store(Reg, Address),
    Load(Reg, Address),
    StoreB(Reg, Address),
    LoadB(Reg, Address),
    Push(OneOrMoreRegisters),
    Pop(OneOrMoreRegisters),
    BitOr(Reg, Reg, RegOrImmediate),
    BitAnd(Reg, Reg, RegOrImmediate),
    BitXor(Reg, Reg, RegOrImmediate),
    BitShl(Reg, Reg, RegOrImmediate),
    BitShr(Reg, Reg, RegOrImmediate),
    Call(String),
    Cmp(Reg, RegOrImmediate),
    B(BranchTarget),
    BNe(BranchTarget),
    BEq(BranchTarget),
    BLt(BranchTarget),
    BGt(BranchTarget),
    Ret,
}

enum CommentPosition {
    Header,
    Footer,
    Line,
    Inline,
}

pub(super) enum OneOrMoreRegisters {
    One(Reg),
    Multiple(Vec<Reg>),
}

impl<const N: usize> From<[Reg; N]> for OneOrMoreRegisters {
    fn from(value: [Reg; N]) -> Self {
        OneOrMoreRegisters::Multiple(value.into())
    }
}

impl From<Reg> for OneOrMoreRegisters {
    fn from(value: Reg) -> Self {
        OneOrMoreRegisters::One(value)
    }
}

impl<'a> IntoIterator for &'a OneOrMoreRegisters {
    type Item = Reg;
    type IntoIter = OneOrMoreRegistersIterator<'a>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            OneOrMoreRegisters::One(reg) => OneOrMoreRegistersIterator::One(Some(*reg)),
            OneOrMoreRegisters::Multiple(regs) => OneOrMoreRegistersIterator::Multiple(regs.iter()),
        }
    }
}

pub(super) enum OneOrMoreRegistersIterator<'a> {
    One(Option<Reg>),
    Multiple(std::slice::Iter<'a, Reg>),
}

impl<'a> Iterator for OneOrMoreRegistersIterator<'a> {
    type Item = Reg;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            OneOrMoreRegistersIterator::One(reg) => {
                let out_reg = *reg;
                *reg = None;

                out_reg
            }
            OneOrMoreRegistersIterator::Multiple(regs) => regs.next().copied(),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub(super) enum BranchTarget {
    Label(LabelId),
    Relative(i32),
    VerbatimLabel(String),
}

impl From<LabelId> for BranchTarget {
    fn from(value: LabelId) -> Self {
        BranchTarget::Label(value)
    }
}

impl From<i32> for BranchTarget {
    fn from(value: i32) -> Self {
        BranchTarget::Relative(value)
    }
}

#[derive(Clone, Copy)]
pub(super) enum Address {
    RelativeIndex(RelativeIndexAddress),
    LiteralIndex(LiteralIndexAddress),
}

#[derive(Clone, Copy)]
pub(super) struct RelativeIndexAddress {
    pub base: Reg,
    pub offset: Reg,
    pub negate_offset: bool,
}

#[derive(Clone, Copy)]
pub(super) struct LiteralIndexAddress {
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
            Address::LiteralIndex(literal_index_address) => literal_index_address.base,
        }
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

impl From<LiteralIndexAddress> for Address {
    fn from(value: LiteralIndexAddress) -> Self {
        Self::LiteralIndex(value)
    }
}

impl From<RelativeIndexAddress> for Address {
    fn from(value: RelativeIndexAddress) -> Self {
        Self::RelativeIndex(value)
    }
}

impl Add<i32> for LiteralIndexAddress {
    type Output = Self;

    fn add(self, rhs: i32) -> Self::Output {
        Self {
            base: self.base,
            offset: self.offset + rhs,
        }
    }
}

impl Sub<i32> for LiteralIndexAddress {
    type Output = Self;

    fn sub(self, rhs: i32) -> Self::Output {
        Self {
            base: self.base,
            offset: self.offset - rhs,
        }
    }
}

#[derive(Clone, Copy)]
pub(super) enum RegOrImmediate {
    Reg(Reg),
    ImmI32(i32),
    ImmF32(f32),
    StringId(StringId),
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(super) enum Reg {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
    Sp,
    ProgCounter,
    LinkReg,
}

impl From<Reg> for RegOrImmediate {
    fn from(value: Reg) -> Self {
        RegOrImmediate::Reg(value)
    }
}

impl From<i32> for RegOrImmediate {
    fn from(value: i32) -> Self {
        RegOrImmediate::ImmI32(value)
    }
}

impl From<u32> for RegOrImmediate {
    fn from(value: u32) -> Self {
        RegOrImmediate::ImmI32(value as i32)
    }
}

impl From<f32> for RegOrImmediate {
    fn from(value: f32) -> Self {
        RegOrImmediate::ImmF32(value)
    }
}

impl From<StringId> for RegOrImmediate {
    fn from(value: StringId) -> Self {
        RegOrImmediate::StringId(value)
    }
}

impl Display for Reg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Reg::R0 => write!(f, "R0"),
            Reg::R1 => write!(f, "R1"),
            Reg::R2 => write!(f, "R2"),
            Reg::R3 => write!(f, "R3"),
            Reg::R4 => write!(f, "R4"),
            Reg::R5 => write!(f, "R5"),
            Reg::R6 => write!(f, "R6"),
            Reg::R7 => write!(f, "R7"),
            Reg::R8 => write!(f, "R8"),
            Reg::R9 => write!(f, "R9"),
            Reg::R10 => write!(f, "R10"),
            Reg::R11 => write!(f, "R11"),
            Reg::R12 => write!(f, "R12"),
            Reg::R13 => write!(f, "R13"),
            Reg::R14 => write!(f, "R14"),
            Reg::R15 => write!(f, "R15"),
            Reg::Sp => write!(f, "SP"),
            Reg::ProgCounter => write!(f, "PC"),
            Reg::LinkReg => write!(f, "LR"),
        }
    }
}
