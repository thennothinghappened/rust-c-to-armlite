use std::fmt::Display;

use crate::parser::program::expr::call::Call;

pub mod call;

#[derive(Debug, Clone)]
pub enum Expr {
    StringLiteral(String),
    Reference(String),
    Call(Box<Call>),
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::StringLiteral(content) => write!(f, "\"{content}\""),
            Expr::Reference(var_name) => write!(f, "{var_name}"),
            Expr::Call(call) => write!(f, "{call}"),
        }
    }
}
