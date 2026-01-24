use std::fmt::Display;

/// A hardware register on ARM.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Reg {
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

impl From<Reg> for [Reg; 1] {
    fn from(value: Reg) -> Self {
        [value]
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

pub enum OneOrMoreRegistersIterator<'a> {
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

pub enum OneOrMoreRegisters {
    One(Reg),
    Multiple(Vec<Reg>),
}

impl<const N: usize> From<[Reg; N]> for OneOrMoreRegisters {
    fn from(value: [Reg; N]) -> Self {
        OneOrMoreRegisters::Multiple(value.into())
    }
}
