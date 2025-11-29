use std::fmt::Display;

use itertools::Itertools;

use crate::parser::program::types::{CFunc, CFuncType};

#[derive(Clone)]
pub(super) enum Inst {
    InlineAsm(String),
    Comment(String),
    InlineComment(String),
    Label(String),
    Mov(Reg, RegOrImmediate),
    Add(Reg, Reg, RegOrImmediate),
    Sub(Reg, Reg, RegOrImmediate),
    Store(Reg, Address),
    Load(Reg, Address),
    StoreB(Reg, Address),
    LoadB(Reg, Address),
    Push(Vec<Reg>),
    Pop(Vec<Reg>),
    Xor(Reg, Reg, Reg),
    Call(String),
    Cmp(Reg, RegOrImmediate),
    B(String),
    BNe(String),
    BEq(String),
    BLt(String),
    BGt(String),
    BExternal(String),
    Ret,
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
    Pc,
    Lr,
}

pub(super) struct FuncBuilder<'a> {
    instructions: Vec<Inst>,
    sig: &'a CFuncType,
    name: &'a str,
}

impl<'a> FuncBuilder<'a> {
    pub fn new(name: &'a str, sig: &'a CFuncType) -> Self {
        Self {
            instructions: Vec::new(),
            sig,
            name,
        }
    }

    pub fn append(&mut self, inst: Inst) -> &mut Self {
        self.instructions.push(inst);
        self
    }

    pub fn comment(&mut self, comment: impl Into<String>) -> &mut Self {
        self.append(Inst::Comment(comment.into()))
    }

    pub fn inline_comment(&mut self, comment: impl Into<String>) -> &mut Self {
        self.append(Inst::InlineComment(comment.into()))
    }

    pub fn label(&mut self, name: impl Into<String>) -> &mut Self {
        self.append(Inst::Label(name.into()))
    }

    pub fn asm(&mut self, asm: impl Into<String>) -> &mut Self {
        self.append(Inst::InlineAsm(asm.into()))
    }

    pub fn push(&mut self, regs: &[Reg]) -> &mut Self {
        self.append(Inst::Push(regs.into()))
    }

    pub fn pop(&mut self, regs: &[Reg]) -> &mut Self {
        self.append(Inst::Pop(regs.into()))
    }

    pub fn mov(&mut self, dest: Reg, src: impl Into<RegOrImmediate>) -> &mut Self {
        self.append(Inst::Mov(dest, src.into()))
    }

    pub fn cmp(&mut self, left: Reg, right: impl Into<RegOrImmediate>) -> &mut Self {
        self.append(Inst::Cmp(left, right.into()))
    }

    pub fn ldr(&mut self, dest: Reg, addr: impl Into<Address>) -> &mut Self {
        self.append(Inst::Load(dest, addr.into()))
    }

    pub fn str(&mut self, src: Reg, addr: impl Into<Address>) -> &mut Self {
        self.append(Inst::Store(src, addr.into()))
    }

    pub fn ldrb(&mut self, dest: Reg, addr: impl Into<Address>) -> &mut Self {
        self.append(Inst::LoadB(dest, addr.into()))
    }

    pub fn strb(&mut self, src: Reg, addr: impl Into<Address>) -> &mut Self {
        self.append(Inst::StoreB(src, addr.into()))
    }

    pub fn add(&mut self, dest: Reg, left: Reg, right: impl Into<RegOrImmediate>) -> &mut Self {
        self.append(Inst::Add(dest, left, right.into()))
    }

    pub fn sub(&mut self, dest: Reg, left: Reg, right: impl Into<RegOrImmediate>) -> &mut Self {
        self.append(Inst::Sub(dest, left, right.into()))
    }

    pub fn b(&mut self, name: impl Into<String>) -> &mut Self {
        self.append(Inst::B(name.into()))
    }

    pub fn beq(&mut self, name: impl Into<String>) -> &mut Self {
        self.append(Inst::BEq(name.into()))
    }

    pub fn bne(&mut self, name: impl Into<String>) -> &mut Self {
        self.append(Inst::BNe(name.into()))
    }

    pub fn blt(&mut self, name: impl Into<String>) -> &mut Self {
        self.append(Inst::BLt(name.into()))
    }

    pub fn bgt(&mut self, name: impl Into<String>) -> &mut Self {
        self.append(Inst::BGt(name.into()))
    }

    pub fn ret(&mut self) -> &mut Self {
        self.append(Inst::Ret)
    }

    fn format_label(&self, label: &str) -> String {
        format!("L{label}__{}", self.name)
    }

    fn format_fn(&self, name: &str) -> String {
        format!("fn_{name}")
    }
}

impl<'a> Display for FuncBuilder<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.sig.args.is_empty() {
            writeln!(f, "; # Arguments")?;

            for arg in &self.sig.args {
                match &arg.name {
                    Some(name) => writeln!(f, "; - {name}"),
                    None => writeln!(f, "; - Unnamed argument"),
                }?;
            }
        }

        writeln!(f, "{}:", self.format_fn(self.name))?;

        let mut inline_comment: Option<&str> = None;

        for inst in &self.instructions {
            match inst {
                Inst::InlineAsm(asm) => write!(f, "\t{asm}"),
                Inst::Comment(text) => write!(f, "\t; {}", text.lines().join("\n\t; ")),
                Inst::InlineComment(text) => {
                    inline_comment = Some(text);
                    Ok(())
                }
                Inst::Label(name) => write!(f, "{}:", self.format_label(name)),
                Inst::Mov(dest, src) => write!(f, "\tMOV {dest}, {src}"),
                Inst::Add(dest, left, right) => write!(f, "\tADD {dest}, {left}, {right}"),
                Inst::Sub(dest, left, right) => write!(f, "\tSUB {dest}, {left}, {right}"),
                Inst::Store(src, address) => write!(f, "\tSTR {src}, {address}"),
                Inst::Load(dest, address) => write!(f, "\tLDR {dest}, {address}"),
                Inst::StoreB(src, address) => write!(f, "\tSTRB {src}, {address}"),
                Inst::LoadB(dest, address) => write!(f, "\tLDRB {dest}, {address}"),
                Inst::Push(regs) => write!(f, "\tPUSH {{{}}}", regs.iter().join(", ")),
                Inst::Pop(regs) => write!(f, "\tPOP {{{}}}", regs.iter().join(", ")),
                Inst::Xor(dest, left, right) => write!(f, "\tEOR {dest}, {left}, {right}"),
                Inst::Call(name) => write!(f, "\tBL fn_{name}"),
                Inst::B(name) => write!(f, "\tB {}", self.format_label(name)),
                Inst::BNe(name) => write!(f, "\tBNE {}", self.format_label(name)),
                Inst::BEq(name) => write!(f, "\tBEQ {}", self.format_label(name)),
                Inst::BLt(name) => write!(f, "\tBLT {}", self.format_label(name)),
                Inst::BGt(name) => write!(f, "\tBGT {}", self.format_label(name)),
                Inst::BExternal(name) => write!(f, "\tB {name}"),
                Inst::Ret => write!(f, "\tRET"),
                Inst::Cmp(reg, reg_or_immediate) => write!(f, "\tCMP {reg}, {reg_or_immediate}"),
            }?;

            if let Some(text) = inline_comment {
                write!(f, "\t\t; {text}")?;
                inline_comment = None;
            }

            writeln!(f)?;
        }

        Ok(())
    }
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
            Reg::Pc => write!(f, "PC"),
            Reg::Lr => write!(f, "LR"),
        }
    }
}
