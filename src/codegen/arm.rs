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

pub(super) enum Inst {
    InlineAsm(String),
    Comment(String),
    InlineComment(String),
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

#[derive(Clone)]
pub(super) struct Address {
    pub base: Reg,
    pub offset: RegOrImmediate,
    pub negate_offset: bool,
}

impl Address {
    pub fn at(reg: Reg) -> Self {
        Self {
            base: reg,
            offset: RegOrImmediate::Imm(0),
            negate_offset: false,
        }
    }

    pub fn relative(base: Reg, offset: impl Into<RegOrImmediate>) -> Self {
        Self {
            base,
            offset: offset.into(),
            negate_offset: false,
        }
    }
}

impl Add<i32> for Reg {
    type Output = Address;

    fn add(self, rhs: i32) -> Self::Output {
        Address::relative(self, rhs)
    }
}

impl Add<Self> for Reg {
    type Output = Address;

    fn add(self, rhs: Self) -> Self::Output {
        Address::relative(self, rhs)
    }
}

#[derive(Clone)]
pub(super) enum RegOrImmediate {
    Reg(Reg),
    Imm(i32),
    Label(String),
}

#[derive(Clone, Copy)]
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

impl Display for Address {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if matches!(self.offset, RegOrImmediate::Imm(0)) {
            return write!(f, "[{}]", self.base);
        }

        write!(
            f,
            "[{}{}{}]",
            self.base,
            if self.negate_offset { "-" } else { "+" },
            self.offset
        )
    }
}

impl Display for RegOrImmediate {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RegOrImmediate::Reg(reg) => reg.fmt(f),
            RegOrImmediate::Imm(value) => write!(f, "#{value}"),
            RegOrImmediate::Label(name) => write!(f, "#{name}"),
        }
    }
}

impl From<Reg> for RegOrImmediate {
    fn from(value: Reg) -> Self {
        RegOrImmediate::Reg(value)
    }
}

impl From<i32> for RegOrImmediate {
    fn from(value: i32) -> Self {
        RegOrImmediate::Imm(value)
    }
}

impl From<u32> for RegOrImmediate {
    fn from(value: u32) -> Self {
        RegOrImmediate::Imm(value as i32)
    }
}

impl From<String> for RegOrImmediate {
    fn from(value: String) -> Self {
        RegOrImmediate::Label(value)
    }
}

impl From<StringId> for RegOrImmediate {
    fn from(value: StringId) -> Self {
        RegOrImmediate::Label(format!("{value}"))
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
