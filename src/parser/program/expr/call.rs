use std::{fmt::Display, rc::Rc};

use crate::parser::program::{
    expr::Expr,
    types::{CFunc, CFuncType, CFuncTypeId},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub sig_id: CFuncTypeId,
    pub target: Box<Expr>,
    pub args: Vec<Expr>,
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}(", self.target)?;

        for arg in &self.args {
            write!(f, "{arg}")?;
        }

        write!(f, ")")
    }
}
