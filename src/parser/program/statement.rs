use crate::parser::program::{expr::Expr, types::Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(Vec<Statement>),

    Declare(Variable),

    Assign {
        var_name: String,
        value: Expr,
    },

    If {
        condition: Expr,
        if_true: Box<Statement>,
        if_false: Option<Box<Statement>>,
    },

    While {
        condition: Expr,
        block: Box<Statement>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Variable {
    pub name: String,
    pub var_type: Type,
    pub value: Option<Expr>,
}
