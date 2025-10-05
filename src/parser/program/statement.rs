use std::collections::HashMap;

use crate::parser::program::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Block(Block),

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
pub struct Block {
    variable_types: HashMap<String, usize>,
    statements: Vec<Statement>,
}
