use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt::Display,
};

use itertools::Itertools;

use crate::{
    codegen::AsmMode,
    id_type::{self, GetAndIncrement},
};

use crate::{codegen::func_builder::FuncBuilder, parser::program::ctype::CFuncType};

id_type!(StringId);

#[derive(Default)]
pub(crate) struct FileBuilder {
    function_builders: RefCell<Vec<String>>,
    constant_strings: RefCell<HashMap<StringId, String>>,
    global_vars: RefCell<HashMap<String, u32>>,
    next_string_id: Cell<StringId>,
    asm_mode: AsmMode,
}

impl FileBuilder {
    pub fn new(asm_mode: AsmMode) -> Self {
        Self {
            asm_mode,
            ..Default::default()
        }
    }

    pub fn create_function<F>(&self, name: &str, sig: &CFuncType, block: F)
    where
        F: FnOnce(&mut FuncBuilder<'_>),
    {
        let mut builder = FuncBuilder::new(name, sig, self.asm_mode);
        block(&mut builder);

        self.function_builders.borrow_mut().push(builder.build());
    }

    pub fn create_string(&self, value: impl Into<String>) -> StringId {
        let id = self.next_string_id.get_and_increment();
        self.constant_strings.borrow_mut().insert(id, value.into());

        id
    }

    pub fn create_global(&self, name: impl Into<String>, size: u32) {
        self.global_vars.borrow_mut().insert(name.into(), size);
    }

    pub fn build(self) -> String {
        let mut output = String::new();

        match self.asm_mode {
            AsmMode::ArmLite => (),
            AsmMode::ArmV7 => output += ".TEXT\n",
        }

        output += "\n";
        output += &self.function_builders.into_inner().join("\n\n\n");
        output += "\n";

        for (string_id, value) in self.constant_strings.borrow().iter() {
            output += &format!("str_{}: .ASCIZ \"{value}\"\n", string_id.0);
        }

        output += "\n.DATA\n";

        for (name, size) in self.global_vars.into_inner() {
            output += &format!("var_{name}: .BLOCK {size}");
        }

        if !output.ends_with('\n') {
            output += "\n";
        }

        output
    }
}
