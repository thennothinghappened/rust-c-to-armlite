use std::{fmt::Display, rc::Rc};

use itertools::Itertools;

use crate::parser::program::{
    expr::Expr,
    types::{CFunc, CFuncType, CFuncTypeId},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub target: Box<Expr>,
    pub args: Vec<Expr>,
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}({})", self.target, self.args.iter().join(", "))
    }
}
