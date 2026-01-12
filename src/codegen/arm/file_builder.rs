use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    fmt::Display,
};

use itertools::Itertools;

use crate::id_type::{self, GetAndIncrement};

use crate::{codegen::func_builder::FuncBuilder, parser::program::types::CFuncType};

const PRELUDE: &str = r#"
; ==================================================================================================
; C RUNTIME PRELUDE
; ==================================================================================================

c_entry:
	BL fn_main
c_entry_post_run:
	MOV R4, R0						; Grab the return code.

	MOV R0, #const_c_entry_exitcode_msg_start
	PUSH {R0}
	BL fn_WriteString

	PUSH {R4}
	BL fn_WriteSignedNum

	MOV R0, #const_c_entry_exitcode_msg_end
	PUSH {R0}
	BL fn_WriteString
c_halt:
	HLT
	B c_halt

const_c_entry_exitcode_msg_start:	.ASCIZ "\nProgram exited with code "
const_c_entry_exitcode_msg_end:		.ASCIZ ".\n"

; ==================================================================================================
; END OF PRELUDE
; ==================================================================================================
"#;

id_type!(StringId);

impl Display for StringId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "str_{}", self.0)
    }
}

#[derive(Default)]
pub(crate) struct FileBuilder {
    function_builders: RefCell<Vec<String>>,
    constant_strings: RefCell<HashMap<StringId, String>>,
    global_vars: RefCell<HashMap<String, u32>>,
    next_string_id: Cell<StringId>,
}

impl FileBuilder {
    pub fn create_function<F>(&self, name: &str, sig: &CFuncType, block: F)
    where
        F: FnOnce(&mut FuncBuilder<'_>),
    {
        let mut builder = FuncBuilder::new(name, sig);
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

        output += PRELUDE;
        output += "\n";
        output += &self.function_builders.into_inner().join("\n");
        output += "\n";

        for (name, value) in self.constant_strings.borrow().iter() {
            output += &format!("{name}: .ASCIZ \"{value}\"\n",);
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
