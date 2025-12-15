use std::collections::HashMap;

use crate::parser::program::{
    statement::{Block, Statement},
    types::CType,
};

pub(super) struct BlockBuilder {
    vars: HashMap<String, CType>,
}

impl BlockBuilder {
    pub fn new() -> Self {
        BlockBuilder {
            vars: HashMap::new(),
        }
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
