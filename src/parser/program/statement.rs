use crate::parser::program::{expr::Expr, types::Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(Vec<Statement>),
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

    Return(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    pub var_type: Type,
    pub value: Option<Expr>,
}
