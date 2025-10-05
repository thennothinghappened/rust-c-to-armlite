use std::fmt::Display;

use crate::{lexer::Token, parser::program::expr::call::Call};

pub mod call;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    StringLiteral(String),
    Reference(String),
    Call(Box<Call>),
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
}

pub trait BindingPower {
    fn binding_power(&self) -> i32;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// Comma operator: evaluate LHS, discard, evaluate RHS, return.
    AndThen,
}

impl BindingPower for BinaryOp {
    fn binding_power(&self) -> i32 {
        // Inverse of https://en.cppreference.com/w/c/language/operator_precedence.html
        match self {
            BinaryOp::AndThen => 0,
        }
    }
}

impl<'a> TryFrom<Token<'a>> for BinaryOp {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value {
            Token::Comma => Ok(BinaryOp::AndThen),
            _ => Err(()),
        }
    }
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            BinaryOp::AndThen => ", ",
        };

        write!(f, "{str}")
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::StringLiteral(content) => write!(f, "\"{content}\""),
            Expr::Reference(var_name) => write!(f, "{var_name}"),
            Expr::Call(call) => write!(f, "{call}"),
            Expr::BinaryOp(op, lhs, rhs) => write!(f, "{lhs}{op}{rhs}"),
        }
    }
}
