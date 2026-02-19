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
    LogicOrdering(OrderMode),
    LogicEqual(CompareMode),
    Bitwise(BinaryBitwiseOp),
    LogicAnd,
    LogicOr,

    /// Assign whatever LHS resolves to, to the evaluated value of RHS, and return that value.
    Assign,

    /// Assign LHS to LHS (op) RHS.
    OpAndAssign(Box<BinaryOp>),

    /// Comma operator: evaluate LHS, discard, evaluate RHS, return.
    AndThen,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryBitwiseOp {
    And,
    Or,
    Xor,
    Shift(BitwiseShiftDirection),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BitwiseShiftDirection {
    Left,
    Right,
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
                BinaryOp::LogicOrdering(order_mode) => match order_mode {
                    OrderMode::LessThan => "<",
                    OrderMode::LessOrEqual => "<=",
                    OrderMode::GreaterThan => ">",
                    OrderMode::GreaterOrEqual => ">=",
                },
                BinaryOp::LogicEqual(CompareMode::Equal) => "==",
                BinaryOp::LogicEqual(CompareMode::NotEqual) => "!=",
                BinaryOp::Bitwise(BinaryBitwiseOp::Xor) => "^",
                BinaryOp::Bitwise(BinaryBitwiseOp::And) => "&",
                BinaryOp::Bitwise(BinaryBitwiseOp::Or) => "|",
                BinaryOp::Bitwise(BinaryBitwiseOp::Shift(BitwiseShiftDirection::Left)) => "<<",
                BinaryOp::Bitwise(BinaryBitwiseOp::Shift(BitwiseShiftDirection::Right)) => ">>",
                BinaryOp::LogicAnd => "&&",
                BinaryOp::LogicOr => "||",
                BinaryOp::Assign => "=",
                BinaryOp::OpAndAssign(op) => return write!(f, "{op}="),
                BinaryOp::AndThen => ", ",
            }
        )
    }
}

impl From<bool> for Expr {
    fn from(value: bool) -> Self {
        Expr::BoolLiteral(value)
    }
}

impl From<i32> for Expr {
    fn from(value: i32) -> Self {
        Expr::IntLiteral(value)
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
            Self::Bitwise(BinaryBitwiseOp::Shift(_)) => 5,
            Self::LogicOrdering(_) => 6,
            Self::LogicEqual(_) => 7,
            Self::Bitwise(BinaryBitwiseOp::And) => 8,
            Self::Bitwise(BinaryBitwiseOp::Xor) => 9,
            Self::Bitwise(BinaryBitwiseOp::Or) => 10,
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
