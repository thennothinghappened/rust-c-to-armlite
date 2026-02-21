//! ARMLite code generation from a `Program`.

use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    ops::{Not, Shl, Shr},
    rc::Rc,
    sync::LazyLock,
};

use anyhow::{anyhow, bail};
use itertools::Itertools;
use phf::phf_map;

use crate::{
    codegen::{
        arm::{file_builder::FileBuilder, func_builder, reg::Reg, AsmMode},
        func_builder::FuncBuilder,
        func_generator::FuncGenerator,
    },
    context::Context,
    parser::program::{
        ctype::{CConcreteType, CFunc, CFuncBody, CPrimitive, CSig, CStructKind, CType, CTypeId},
        expr::{call::Call, BinaryOp, Expr, UnaryOp},
        statement::{Block, Statement, Variable},
        Program, Symbol,
    },
};

pub mod arm;
mod func_generator;

const WORD_SIZE: u32 = 4;

pub struct Generator {
    program: Program,
    file_builder: FileBuilder,
}

impl Generator {
    pub fn new(program: Program, asm_mode: AsmMode) -> Self {
        Self {
            program,
            file_builder: FileBuilder::new(asm_mode),
        }
    }

    pub fn generate(self) -> String {
        // FIXME FIXME FIXME: This is a disgusting workaround to get globals initialised and could
        // technically cause naming clashes, plus is just a waste of instructions during startup.
        //
        // This is caused by C runtime initialisation being handled *above* the level of
        // FuncGenerator, thus if we want to do any C evaluation, we need to invoke a FuncGenerator,
        // which in turn, because there's no inline support, means we have to create a whole new
        // named function in the output just to run `<global> = <expr>;`.
        //
        // All of that points to the fact that the weird distinction between Generator,
        // FuncGenerator, FuncBuilder, and FileBuilder needs to be refactored desperately. The
        // relationship between all of these structs is really weird and interdependent.
        let global_init_functions: Vec<(String, CFunc)> = self
            .program
            .get_global_variables()
            .filter_map(|(name, (&ctype, value))| {
                self.file_builder
                    .create_global(name, self.sizeof_ctype(ctype));

                let value = value.as_ref()?;
                let init_func_name = format!("__{name}__init__");
                Some((
                    init_func_name,
                    CFunc {
                        sig_id: self.program.get_signature_id(CSig {
                            args: vec![],
                            returns: CConcreteType::Void.into(),
                            is_noreturn: false,
                        }),
                        body: CFuncBody::Defined(Block {
                            statements: vec![Statement::Expr(Expr::BinaryOp(
                                BinaryOp::Assign,
                                Box::new(Expr::Reference(name.to_string())),
                                Box::new(value.clone()),
                            ))],
                            vars: HashMap::default(),
                        }),
                        is_raw_assembly: false,
                    },
                ))
            })
            .collect_vec();

        self.file_builder.create_function(
            "start",
            &CSig {
                args: vec![],
                returns: CConcreteType::Void.into(),
                is_noreturn: true,
            },
            |b| {
                b.append_doc_line("C Runtime Entry Point");
                b.append_doc_line("Initialises the C environment and calls main().");

                b.header("Initialise global variables.");

                for (init_func_name, _) in &global_init_functions {
                    b.call(init_func_name.clone());
                }

                b.header("Jump to user code.");
                b.call("main");

                b.asm("c_entry_post_run:");
                b.inline_comment("Grab the return code.");
                b.push(Reg::R0);

                let exitcode_msg_start = self
                    .file_builder
                    .create_string("\\nProgram exited with code ");

                let exitcode_msg_end = self.file_builder.create_string(".\\n");

                b.move_dword(Reg::R0, exitcode_msg_start);
                b.push(Reg::R0);
                b.call("WriteString");
                b.call("WriteSignedNum");

                b.move_dword(Reg::R0, exitcode_msg_end);
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
            .get_functions()
            .filter(|(_, cfunc)| !matches!(cfunc.body, CFuncBody::Extern))
            .chain(
                global_init_functions
                    .iter()
                    .map(|entry| (entry.0.as_str(), &entry.1)),
            )
        {
            let sig = self.program.get_signature(cfunc.sig_id);

            self.file_builder.create_function(name, &sig, |b| {
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

                    b.append_doc_line("");
                }

                b.append_doc_line("# Returns");
                b.append_doc_line(self.program.format_ctype(b.sig.returns));

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

    /// Get the size, in bytes, of the given C type. The returned size is *not* aligned to the
    /// architecture's byte size.
    pub fn sizeof_ctype(&self, ctype: CType) -> u32 {
        self.program.sizeof_ctype(ctype)
    }

    pub fn sizeof_primitive(&self, primitive: CPrimitive) -> u32 {
        self.program.sizeof_primitive(primitive)
    }

    pub fn align<Scalar>(&self, offset: Scalar) -> Scalar
    where
        Scalar: Shl<Output = Scalar>,
        Scalar: Shr<Output = Scalar>,
        Scalar: Ord,
        Scalar: From<u8>,
        Scalar: Copy,
    {
        self.program.align(offset)
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

impl From<CPrimitive> for CTypeOrId {
    fn from(value: CPrimitive) -> Self {
        CTypeOrId::CType(CType::AsIs(CConcreteType::Primitive(value)))
    }
}

impl From<CTypeId> for CTypeOrId {
    fn from(value: CTypeId) -> Self {
        CTypeOrId::Id(value)
    }
}
