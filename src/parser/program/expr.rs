use std::fmt::Display;

use crate::parser::program::{ctype::CType, expr::call::Call};

pub mod call;

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    StringLiteral(String),
    IntLiteral(i32),
    BoolLiteral(bool),
    NullPtr,
    Reference(String),
    Call(Call),
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
    UnaryOp(UnaryOp, Box<Expr>),
    Cast(Box<Expr>, CType),
    DotAccess { target: Box<Expr>, member: String },
}

pub trait BindingPower {
    fn binding_strength(&self) -> i32;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    ArrayIndex,

    Plus,
    Minus,
    BitwiseLeftShift,
    BitwiseRightShift,
    LogicOrdering(OrderMode),
    LogicEqual(CompareMode),
    BitwiseXor,
    BitwiseAnd,
    BitwiseOr,
    LogicAnd,
    LogicOr,

    /// Assign whatever LHS resolves to, to the evaluated value of RHS, and return that value.
    Assign,

    /// Assign LHS to LHS (op) RHS.
    OpAndAssign(Box<BinaryOp>),

    /// Comma operator: evaluate LHS, discard, evaluate RHS, return.
    AndThen,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOp::ArrayIndex => "[]",
                BinaryOp::Plus => "+",
                BinaryOp::Minus => "-",
                BinaryOp::BitwiseLeftShift => "<<",
                BinaryOp::BitwiseRightShift => ">>",
                BinaryOp::LogicOrdering(order_mode) => match order_mode {
                    OrderMode::LessThan => "<",
                    OrderMode::LessOrEqual => "<=",
                    OrderMode::GreaterThan => ">",
                    OrderMode::GreaterOrEqual => ">=",
                },
                BinaryOp::LogicEqual(compare_mode) => "==",
                BinaryOp::BitwiseXor => "^",
                BinaryOp::BitwiseAnd => "&",
                BinaryOp::BitwiseOr => "|",
                BinaryOp::LogicAnd => "&&",
                BinaryOp::LogicOr => "||",
                BinaryOp::Assign => "=",
                BinaryOp::OpAndAssign(op) => return write!(f, "{op}="),
                BinaryOp::AndThen => ", ",
            }
        )
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CompareMode {
    Equal,
    NotEqual,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OrderMode {
    LessThan,
    LessOrEqual,
    GreaterThan,
    GreaterOrEqual,
}

impl BindingPower for BinaryOp {
    fn binding_strength(&self) -> i32 {
        // https://en.cppreference.com/w/c/language/operator_precedence.html
        //
        // Just for sanity since we do it as higher number -> higher strength, we'll just subtract
        // from the highest value and use their values.
        15 - match self {
            Self::ArrayIndex => 1,
            Self::Minus => 4,
            Self::Plus => 4,
            Self::BitwiseRightShift => 5,
            Self::BitwiseLeftShift => 5,
            Self::LogicOrdering(_) => 6,
            Self::LogicEqual(_) => 7,
            Self::BitwiseAnd => 8,
            Self::BitwiseXor => 9,
            Self::BitwiseOr => 10,
            Self::LogicAnd => 11,
            Self::LogicOr => 12,
            Self::Assign => 14,
            Self::OpAndAssign(_) => 14,
            Self::AndThen => 15,
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
