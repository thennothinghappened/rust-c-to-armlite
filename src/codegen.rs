//! ARMLite code generation from a `Program`.

use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    ops::Not,
    rc::Rc,
    sync::LazyLock,
};

use itertools::Itertools;
use phf::phf_map;

use crate::{
    codegen::{
        arm::{file_builder::FileBuilder, func_builder, Address, AsmMode, Reg, RegOrImmediate},
        func_builder::FuncBuilder,
        func_generator::FuncGenerator,
    },
    context::Context,
    parser::program::{
        expr::{call::Call, BinaryOp, Expr, UnaryOp},
        statement::{Block, Statement, Variable},
        types::{CConcreteType, CFunc, CFuncBody, CFuncType, CPrimitive, CType, CTypeId},
        Program, Symbol,
    },
};

pub mod arm;
mod func_generator;

const WORD_SIZE: u32 = 4;

pub struct Generator {
    program: Program,
    file_builder: FileBuilder,
    ctype_size_cache: RefCell<HashMap<CType, u32>>,
}

impl Generator {
    pub fn new(program: Program, asm_mode: AsmMode) -> Self {
        Self {
            program,
            file_builder: FileBuilder::new(asm_mode),
            ctype_size_cache: HashMap::default().into(),
        }
    }

    pub fn generate(self) -> String {
        self.file_builder.create_function(
            "start",
            &CFuncType {
                args: vec![],
                returns: CPrimitive::Void.into(),
                is_noreturn: true,
            },
            |b| {
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
                b.append_doc_line("C Runtime Entry Point");
                b.append_doc_line("Initialises the C environment and calls main().");

                b.call("main");

                b.asm("c_entry_post_run:");
                b.inline_comment("Grab the return code.");
                b.mov(Reg::R4, Reg::R0);

                let exitcode_msg_start = self
                    .file_builder
                    .create_string("\\nProgram exited with code ");

                let exitcode_msg_end = self.file_builder.create_string(".\\n");

                b.mov(Reg::R0, exitcode_msg_start);
                b.push(Reg::R0);
                b.call("WriteString");

                b.push(Reg::R4);
                b.call("WriteSignedNum");

                b.mov(Reg::R0, exitcode_msg_end);
                b.push(Reg::R0);
                b.call("WriteString");

                b.asm("c_halt:");
                b.asm("HLT");
                b.asm("B c_halt");
            },
        );

        let mut failures = Vec::<(&str, anyhow::Error)>::new();

        for (name, cfunc) in self
            .program
            .get_defined_functions()
            .filter(|(_, cfunc)| !matches!(cfunc.body, CFuncBody::Extern))
        {
            let sig = self.program.get_cfunc_sig(cfunc.sig_id);

            self.file_builder.create_function(name, sig, |b| {
                if !b.sig.args.is_empty() {
                    b.append_doc_line("# Arguments");

                    for arg in &b.sig.args {
                        match &arg.name {
                            Some(name) => b.append_doc_line(format!(
                                "- {type} {name}",
                                type = self.program.format_ctype(arg.ctype)
                            )),
                            None => b.append_doc_line(format!(
                                "- Unnamed argument ({})",
                                self.program.format_ctype(arg.ctype)
                            )),
                        };
                    }
                }

                if let Err(error) = FuncGenerator::new(&self, b).generate_func(cfunc) {
                    b.asm(format!("\nERROR!:\n{error:?}"));
                    failures.push((name, error));
                }
            });
        }

        let output = self.file_builder.build();

        if !failures.is_empty() {
            println!(
                "\n================================================================================"
            );

            println!(
                "{count} function bodies failed to generate. Their errors are listed below.\n",
                count = failures.len()
            );

            for (name, error) in failures {
                println!("==> Failed to generate {name}:\n{error:?}\n");
            }

            println!(
                "================================================================================\n"
            );
        }

        output
    }

    fn sizeof_ctype(&self, ctype: impl Into<CTypeOrId>) -> u32 {
        let ctype = match ctype.into() {
            CTypeOrId::CType(ctype) => ctype,
            CTypeOrId::Id(ctype_id) => self.program.get_ctype(ctype_id),
        };

        if let Some(size) = self.ctype_size_cache.borrow().get(&ctype) {
            return *size;
        }

        let size = match ctype {
            CType::PointerTo(_) => WORD_SIZE,

            CType::ArrayOf(inner_id, element_count) => {
                self.sizeof_ctype(self.program.get_ctype(inner_id)) * element_count
            }

            CType::AsIs(concrete) => match concrete {
                CConcreteType::Struct(cstruct_id) => self
                    .program
                    .get_struct(cstruct_id)
                    .members
                    .as_ref()
                    .map(|members| {
                        members
                            .iter()
                            .map(|member| self.sizeof_ctype(member.ctype))
                            .sum()
                    })
                    .unwrap_or(0),

                CConcreteType::Enum(cenum_id) => WORD_SIZE,
                CConcreteType::Func(_) => WORD_SIZE,

                CConcreteType::Primitive(builtin) => match builtin {
                    CPrimitive::Void => 0,
                    CPrimitive::Bool => 1,
                    CPrimitive::Char => 1,
                    CPrimitive::SignedChar => 1,
                    CPrimitive::UnsignedChar => 1,
                    CPrimitive::Short => 2,
                    CPrimitive::UnsignedShort => 2,
                    CPrimitive::Int => WORD_SIZE,
                    CPrimitive::UnsignedInt => WORD_SIZE,
                    CPrimitive::Long => WORD_SIZE,
                    CPrimitive::UnsignedLong => WORD_SIZE,
                    CPrimitive::LongLong => 8,
                    CPrimitive::UnsignedLongLong => 8,
                    CPrimitive::Float => 2,
                    CPrimitive::Double => 4,
                    CPrimitive::LongDouble => 4,
                },
            },
        };

        self.ctype_size_cache.borrow_mut().insert(ctype, size);
        size
    }
}

enum CTypeOrId {
    CType(CType),
    Id(CTypeId),
}

impl From<CType> for CTypeOrId {
    fn from(value: CType) -> Self {
        CTypeOrId::CType(value)
    }
}

impl From<CTypeId> for CTypeOrId {
    fn from(value: CTypeId) -> Self {
        CTypeOrId::Id(value)
    }
}
