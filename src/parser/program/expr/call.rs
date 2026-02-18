use std::{fmt::Display, rc::Rc};

use itertools::Itertools;

use crate::parser::program::{
    ctype::{CFunc, CSig, CSigId},
    expr::Expr,
};

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub target: Box<Expr>,
    pub args: Vec<Expr>,
}
