use std::collections::HashMap;

use crate::parser::program::{ctype::CType, expr::Expr};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(Block),
    Declare(Variable),
    Expr(Expr),

    If {
        condition: Box<Expr>,
        if_true: Box<Statement>,
        if_false: Option<Box<Statement>>,
    },

    While {
        condition: Box<Expr>,
        block: Box<Statement>,
    },

    Return(Option<Box<Expr>>),
    Break,
    Continue,
    Empty,
    Asm(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
    pub vars: HashMap<String, CType>,
}

impl From<Block> for Statement {
    fn from(value: Block) -> Self {
        Statement::Block(value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    pub ctype: CType,
    pub value: Option<Expr>,
}
