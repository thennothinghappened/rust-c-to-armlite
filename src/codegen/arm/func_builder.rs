use std::{
    cell::Cell,
    collections::{HashMap, HashSet, VecDeque},
    fmt::Display,
};

use bimap::BiMap;
use itertools::Itertools;

use crate::{
    codegen::{
        arm::{Address, BranchTarget, LiteralIndexAddress, OneOrMoreRegisters, RegOrImmediate},
        Generator, Inst, Reg,
    },
    id_type::GetAndIncrement,
    parser::program::types::CFuncType,
};

id_type!(LabelId);

pub(crate) struct FuncBuilder<'a> {
    pub sig: &'a CFuncType,
    name: &'a str,
    instructions: Vec<Inst>,
    doc_comment: Vec<String>,

    labels: HashMap<LabelId, String>,
    next_label_id: Cell<LabelId>,
}

impl<'a> FuncBuilder<'a> {
    pub fn new(name: &'a str, sig: &'a CFuncType) -> Self {
        Self {
            doc_comment: Vec::new(),
            instructions: Vec::new(),
            sig,
            name,
            next_label_id: Cell::default(),
            labels: HashMap::default(),
        }
    }

    pub fn append_doc_line(&mut self, line: impl Into<String>) {
        self.doc_comment.push(line.into());
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

    pub fn create_label(&mut self, name: impl Into<String>) -> LabelId {
        let id = self.next_label_id.get_and_increment();
        self.labels.insert(id, name.into());

        id
    }

    pub fn label(&mut self, id: LabelId) -> &mut Self {
        self.append(Inst::Label(id))
    }

    pub fn asm(&mut self, asm: impl Into<String>) -> &mut Self {
        self.append(Inst::InlineAsm(asm.into()))
    }

    pub fn push(&mut self, regs: impl Into<OneOrMoreRegisters>) -> &mut Self {
        self.append(Inst::Push(regs.into()))
    }

    pub fn pop(&mut self, regs: impl Into<OneOrMoreRegisters>) -> &mut Self {
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

    pub fn or(&mut self, dest: Reg, left: Reg, right: impl Into<RegOrImmediate>) -> &mut Self {
        self.append(Inst::BitOr(dest, left, right.into()))
    }

    pub fn and(&mut self, dest: Reg, left: Reg, right: impl Into<RegOrImmediate>) -> &mut Self {
        self.append(Inst::BitAnd(dest, left, right.into()))
    }

    pub fn xor(&mut self, dest: Reg, left: Reg, right: impl Into<RegOrImmediate>) -> &mut Self {
        self.append(Inst::BitXor(dest, left, right.into()))
    }

    pub fn shl(&mut self, dest: Reg, left: Reg, right: impl Into<RegOrImmediate>) -> &mut Self {
        self.append(Inst::BitShl(dest, left, right.into()))
    }

    pub fn shr(&mut self, dest: Reg, left: Reg, right: impl Into<RegOrImmediate>) -> &mut Self {
        self.append(Inst::BitShr(dest, left, right.into()))
    }

    pub fn call(&mut self, name: impl Into<String>) -> &mut Self {
        self.append(Inst::Call(name.into()))
    }

    pub fn b(&mut self, name: impl Into<BranchTarget>) -> &mut Self {
        self.append(Inst::B(name.into()))
    }

    pub fn beq(&mut self, name: impl Into<BranchTarget>) -> &mut Self {
        self.append(Inst::BEq(name.into()))
    }

    pub fn bne(&mut self, name: impl Into<BranchTarget>) -> &mut Self {
        self.append(Inst::BNe(name.into()))
    }

    pub fn blt(&mut self, name: impl Into<BranchTarget>) -> &mut Self {
        self.append(Inst::BLt(name.into()))
    }

    pub fn bgt(&mut self, name: impl Into<BranchTarget>) -> &mut Self {
        self.append(Inst::BGt(name.into()))
    }

    pub fn ret(&mut self) -> &mut Self {
        self.append(Inst::Ret)
    }

    /// Load the address `addr` into the destination register `dest`. Works like x86's `LEA`.
    pub fn load_address(&mut self, dest: Reg, addr: impl Into<Address>) {
        let addr = addr.into();

        match addr {
            Address::LiteralIndex(LiteralIndexAddress { base, offset: 0 }) => self.mov(dest, base),

            Address::RelativeIndex(addr) => {
                if addr.negate_offset {
                    self.sub(dest, addr.base, addr.offset)
                } else {
                    self.add(dest, addr.base, addr.offset)
                }
            }

            Address::LiteralIndex(addr) => {
                if addr.offset < 0 {
                    self.sub(dest, addr.base, -addr.offset)
                } else {
                    self.add(dest, addr.base, addr.offset)
                }
            }
        };
    }

    pub fn build(self) -> String {
        format!("{self}")
    }

    fn format_label(&self, id: LabelId) -> String {
        format!("L{}_{}__{}", self.labels[&id], id.0, self.name)
    }

    fn format_branch_target(&self, target: &BranchTarget) -> String {
        match target {
            BranchTarget::Label(id) => self.format_label(*id),
            BranchTarget::Relative(offset) => {
                format!(".{}{}", if *offset >= 0 { '+' } else { '-' }, offset.abs())
            }
            BranchTarget::VerbatimLabel(name) => format!("#{name}"),
        }
    }

    fn format_fn(&self, name: &str) -> String {
        format!("fn_{name}")
    }
}

impl<'a> Display for FuncBuilder<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for line in &self.doc_comment {
            writeln!(f, "; {line}")?;
        }

        writeln!(f, "{}:", self.format_fn(self.name))?;

        let mut prev_was_comment = false;
        let mut inline_comments: Vec<&str> = vec![];

        for inst in &self.instructions {
            match inst {
                Inst::InlineAsm(asm) => write!(f, "\t{asm}"),
                Inst::Comment(text) => {
                    if !prev_was_comment {
                        writeln!(f)?;
                    }

                    write!(f, "\t; {}", text.trim().lines().join("\n\t; "))
                }
                Inst::InlineComment(text) => {
                    inline_comments.push(text);
                    Ok(())
                }
                Inst::Label(id) => write!(f, "{}:", self.format_label(*id)),
                Inst::Mov(dest, src) => write!(f, "\tMOV {dest}, {src}"),
                Inst::Add(dest, left, right) => write!(f, "\tADD {dest}, {left}, {right}"),
                Inst::Sub(dest, left, right) => write!(f, "\tSUB {dest}, {left}, {right}"),
                Inst::Store(src, address) => write!(f, "\tSTR {src}, {address}"),
                Inst::Load(dest, address) => write!(f, "\tLDR {dest}, {address}"),
                Inst::StoreB(src, address) => write!(f, "\tSTRB {src}, {address}"),
                Inst::LoadB(dest, address) => write!(f, "\tLDRB {dest}, {address}"),
                Inst::Push(regs) => write!(f, "\tPUSH {{{}}}", regs.into_iter().join(", ")),
                Inst::Pop(regs) => write!(f, "\tPOP {{{}}}", regs.into_iter().join(", ")),
                Inst::BitOr(dest, left, right) => write!(f, "\tORR {dest}, {left}, {right}"),
                Inst::BitAnd(dest, left, right) => write!(f, "\tAND {dest}, {left}, {right}"),
                Inst::BitXor(dest, left, right) => write!(f, "\tEOR {dest}, {left}, {right}"),
                Inst::Call(name) => write!(f, "\tBL {}", self.format_fn(name)),
                Inst::B(target) => write!(f, "\tB {}", self.format_branch_target(target)),
                Inst::BNe(target) => write!(f, "\tBNE {}", self.format_branch_target(target)),
                Inst::BEq(target) => write!(f, "\tBEQ {}", self.format_branch_target(target)),
                Inst::BLt(target) => write!(f, "\tBLT {}", self.format_branch_target(target)),
                Inst::BGt(target) => write!(f, "\tBGT {}", self.format_branch_target(target)),
                Inst::Ret => write!(f, "\tRET"),
                Inst::Cmp(reg, reg_or_immediate) => write!(f, "\tCMP {reg}, {reg_or_immediate}"),
                Inst::BitShl(dest, left, right) => write!(f, "\tLSL {dest}, {left}, {right}"),
                Inst::BitShr(dest, left, right) => write!(f, "\tLSR {dest}, {left}, {right}"),
            }?;

            prev_was_comment = matches!(inst, Inst::Comment(_));

            match inst {
                Inst::Comment(_) | Inst::InlineComment(_) => {}
                _ => {
                    if let Some(text) = inline_comments.pop() {
                        write!(f, "\t\t; {text}")?;
                    }
                }
            }

            writeln!(f)?;
        }

        Ok(())
    }
}
