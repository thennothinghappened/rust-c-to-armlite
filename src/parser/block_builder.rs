use std::collections::HashMap;

use crate::parser::program::{
    ctype::{CType, CTypeId},
    statement::{Block, Statement},
    ExecutionScope,
};

pub(super) struct BlockBuilder<'a> {
    vars: HashMap<String, CType>,
    parent: Option<&'a BlockBuilder<'a>>,
}

impl<'a> BlockBuilder<'a> {
    pub fn new() -> Self {
        BlockBuilder {
            vars: HashMap::new(),
            parent: None,
        }
    }

    pub fn with_child_scope<T, F>(&'a self, block: F) -> T
    where
        F: FnOnce(Self) -> T,
    {
        block(Self {
            vars: HashMap::new(),
            parent: Some(self),
        })
    }

    pub fn declare_var(&mut self, name: String, ctype: CType) {
        if let Some(existing) = self.vars.insert(name, ctype) {
            todo!("handle overlapping variable decl against {existing:?}")
        }
    }

    pub fn build(self) -> HashMap<String, CType> {
        self.vars
    }
}

impl<'a> ExecutionScope for BlockBuilder<'a> {
    fn variable_ctype(&self, name: &str) -> Option<CType> {
        if let Some(ctype) = self.vars.get(name) {
            return Some(*ctype);
        }

        self.parent?.variable_ctype(name)
    }
}
