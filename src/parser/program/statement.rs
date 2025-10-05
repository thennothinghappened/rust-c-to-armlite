use crate::parser::program::{expr::Expr, types::Type};

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(Vec<Statement>),

    Declare {
        var_name: String,
        var_type: Type,
        value: Option<Expr>,
    },

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
