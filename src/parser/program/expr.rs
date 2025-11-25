use std::fmt::Display;

use crate::parser::program::{expr::call::Call, types::CType};

pub mod call;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    StringLiteral(String),
    IntLiteral(i32),
    Reference(String),
    Call(Call),
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),
    Cast(Box<Expr>, CType),
}

pub trait BindingPower {
    fn binding_power(&self) -> i32;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    /// Comma operator: evaluate LHS, discard, evaluate RHS, return.
    AndThen,

    /// Assign whatever LHS resolves to, to the evaluated value of RHS, and return that value.
    Assign,

    BooleanEqual,

    LessThan,

    Plus,

    ArraySubscript,
}

impl BindingPower for BinaryOp {
    fn binding_power(&self) -> i32 {
        // Inverse of https://en.cppreference.com/w/c/language/operator_precedence.html
        match self {
            BinaryOp::AndThen => 0,
            BinaryOp::Assign => 1,
            BinaryOp::BooleanEqual => 7,
            BinaryOp::LessThan => 8,
            BinaryOp::Plus => 13,
            BinaryOp::ArraySubscript => 14,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// ++<expr>
    IncrementThenGet,

    /// <expr>++
    GetThenIncrement,

    /// --<expr>
    DecrementThenGet,

    /// <expr>--
    GetThenDecrement,

    /// Get the byte-length of the given operand.
    SizeOf,

    /// Get the boolean-opposite of the given operand's truthiness.
    BooleanNot,

    /// Invert the sign of the operand.
    Negative,

    /// Get the memory address of the operand.
    AddressOf,

    /// Get the data at the given address.
    Dereference,
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::StringLiteral(content) => write!(f, "\"{content}\""),

            Expr::IntLiteral(int) => write!(f, "{int}"),

            Expr::Reference(var_name) => write!(f, "{var_name}"),

            Expr::Call(call) => write!(f, "{call}"),

            Expr::BinaryOp(op, lhs, rhs) => match op {
                BinaryOp::AndThen => write!(f, "{lhs}, {rhs}"),
                BinaryOp::Assign => write!(f, "{lhs} = {rhs}"),
                BinaryOp::BooleanEqual => write!(f, "{lhs} == {rhs}"),
                BinaryOp::LessThan => write!(f, "{lhs} < {rhs}"),
                BinaryOp::Plus => write!(f, "{lhs} + {rhs}"),
                BinaryOp::ArraySubscript => write!(f, "{lhs}[{rhs}]"),
            },

            Expr::Cast(expr, type_id) => write!(f, "({type_id:?}){expr}"),

            Expr::UnaryOp(op, expr) => match op {
                UnaryOp::IncrementThenGet => write!(f, "++{expr}"),
                UnaryOp::GetThenIncrement => write!(f, "{expr}++"),
                UnaryOp::DecrementThenGet => write!(f, "--{expr}"),
                UnaryOp::GetThenDecrement => write!(f, "{expr}--"),
                UnaryOp::SizeOf => write!(f, "sizeof {expr}"),
                UnaryOp::BooleanNot => write!(f, "!{expr}"),
                UnaryOp::Negative => write!(f, "-{expr}"),
                UnaryOp::AddressOf => write!(f, "&{expr}"),
                UnaryOp::Dereference => write!(f, "*{expr}"),
            },
        }
    }
}
