use crate::codegen::{
    arm::{
        address::Address,
        reg::{OneOrMoreRegisters, Reg},
        reg_or_imm::RegOrImm,
    },
    func_builder::LabelId,
};

pub enum Inst {
    InlineAsm(String),
    Comment(String, CommentPosition),
    Label(LabelId),
    Mov(Reg, RegOrImm),
    Add(Reg, Reg, RegOrImm),
    Sub(Reg, Reg, RegOrImm),
    Store(Reg, Address),
    Load(Reg, Address),
    StoreB(Reg, Address),
    LoadB(Reg, Address),
    Push(OneOrMoreRegisters),
    Pop(OneOrMoreRegisters),
    BitOr(Reg, Reg, RegOrImm),
    BitAnd(Reg, Reg, RegOrImm),
    BitXor(Reg, Reg, RegOrImm),
    BitShl(Reg, Reg, RegOrImm),
    BitShr(Reg, Reg, RegOrImm),
    Call(String),
    Cmp(Reg, RegOrImm),
    B(BranchTarget),
    BNe(BranchTarget),
    BEq(BranchTarget),
    BLt(BranchTarget),
    BGt(BranchTarget),
    Ret,
}

pub enum CommentPosition {
    Header,
    Footer,
    Line,
    Inline,
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
