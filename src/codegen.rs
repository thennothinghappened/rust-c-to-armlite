//! ARMLite code generation from a `Program`.

use std::{
    cell::{Cell, RefCell},
    collections::HashMap,
    ops::Not,
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
        ctype::{CConcreteType, CFunc, CFuncBody, CFuncType, CPrimitive, CType, CTypeId},
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
                returns: CConcreteType::Void.into(),
                is_noreturn: true,
            },
            |b| {
                b.append_doc_line("C Runtime Entry Point");
                b.append_doc_line("Initialises the C environment and calls main().");

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
            CType::PointerTo(_, None) => WORD_SIZE,

            CType::PointerTo(inner_id, Some(element_count)) => {
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
                CConcreteType::Primitive(primitive) => self.sizeof_primitive(primitive),
                CConcreteType::Void => 0,
            },
        };

        self.ctype_size_cache.borrow_mut().insert(ctype, size);
        size
    }

    fn sizeof_primitive(&self, primitive: CPrimitive) -> u32 {
        match primitive {
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
        }
    }

    /// Obtain the most specific common type between `a` and `b`, if one exists.
    fn common_ctype(&self, a: &CType, b: &CType) -> anyhow::Result<CType> {
        if a == b {
            return Ok(*a);
        }

        match (a, b) {
            (CType::PointerTo(a_inner, None), CType::PointerTo(b_inner, None)) => {
                match (
                    self.program.is_generic_pointer(*a),
                    self.program.is_generic_pointer(*b),
                ) {
                    // Either will do.
                    (true, true) => Ok(*a),

                    // Take the non-generic (a).
                    (false, true) => Ok(*a),

                    // Take the non-generic (b).
                    (true, false) => Ok(*b),

                    // No overlap between these types.
                    (false, false) => bail!(
                        "No overlap between pointer types `{}` and `{}`",
                        self.program.format_ctype(*a),
                        self.program.format_ctype(*b)
                    ),
                }
            }

            (
                CType::AsIs(CConcreteType::Primitive(a_primitive)),
                CType::AsIs(CConcreteType::Primitive(b_primitive)),
            ) => Ok(self
                .common_real_primitive(*a_primitive, *b_primitive)
                .into()),

            _ => bail!(
                "No overlap between types {} and {}",
                self.program.format_ctype(*a),
                self.program.format_ctype(*b)
            ),
        }
    }

    fn rank_of_primitive(&self, primitive: CPrimitive) -> u32 {
        if primitive == CPrimitive::Bool {
            return 0;
        }

        self.sizeof_primitive(primitive)
    }

    /// Obtain a common primitive type from the given primitives `a` and `b` which can most
    /// adequately represent the range of values the input types can hold.
    fn common_real_primitive(&self, a: CPrimitive, b: CPrimitive) -> CPrimitive {
        let (promoted_a, promoted_b) = match (a.is_floating_type(), b.is_floating_type()) {
            // Pass through to picking the larger float type.
            (true, true) => (a, b),

            // Convert the integer to a float.
            (true, false) => (a, a),
            (false, true) => (b, b),

            // Both are integers, promote!
            (false, false) => (self.promote_integer(a), self.promote_integer(b)),
        };

        if self.rank_of_primitive(promoted_a) > self.rank_of_primitive(promoted_b) {
            promoted_a
        } else {
            promoted_b
        }
    }

    /// Promote an integer type up to `int` or `unsigned int`, if its rank is lower, or preserve the
    /// given type should its rank exceed either of those.
    fn promote_integer(&self, integer: CPrimitive) -> CPrimitive {
        assert!(
            integer.is_integer_type(),
            "Shouldn't pass a non-integer primitive type to promotion"
        );

        if self.rank_of_primitive(integer) < self.rank_of_primitive(CPrimitive::Int) {
            match self.is_signed_primitive(integer) {
                true => CPrimitive::Int,
                false => CPrimitive::UnsignedInt,
            }
        } else {
            integer
        }
    }

    /// Return whether the given primitive is a signed type on this platform.
    fn is_signed_primitive(&self, primitive: CPrimitive) -> bool {
        match primitive {
            CPrimitive::Char
            | CPrimitive::SignedChar
            | CPrimitive::Short
            | CPrimitive::Int
            | CPrimitive::Long
            | CPrimitive::LongLong
            | CPrimitive::Float
            | CPrimitive::Double
            | CPrimitive::LongDouble => true,

            CPrimitive::Bool
            | CPrimitive::UnsignedChar
            | CPrimitive::UnsignedShort
            | CPrimitive::UnsignedInt
            | CPrimitive::UnsignedLong
            | CPrimitive::UnsignedLongLong => false,
        }
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
