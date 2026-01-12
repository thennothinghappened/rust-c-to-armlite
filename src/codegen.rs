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
        arm::{file_builder::FileBuilder, func_builder, Address, Inst, Reg, RegOrImmediate},
        func_builder::FuncBuilder,
        func_generator::FuncGenerator,
    },
    context::Context,
    parser::program::{
        expr::{call::Call, BinaryOp, Expr, UnaryOp},
        statement::{Block, Statement, Variable},
        types::{CConcreteType, CFunc, CFuncType, CPrimitive, CType, CTypeId},
        Program, Symbol,
    },
};

mod arm;
mod func_generator;

const WORD_SIZE: u32 = 4;

static BUILTIN_FUNCS: phf::Map<&str, fn(&mut FuncBuilder)> = phf_map! {
    "WriteChar" => |b| {
        b.ldrb(Reg::R0, Reg::Sp+0);
        b.asm("STRB R0, .WriteChar");
        b.add(Reg::Sp, Reg::Sp, 4);
        b.ret();
    },
    "WriteString" => |b| {
        b.pop(Reg::R0);
        b.asm("STR R0, .WriteString");
        b.ret();
    },
    "WriteSignedNum" => |b| {
        b.pop(Reg::R0);
        b.asm("STR R0, .WriteSignedNum");
        b.ret();
    },
    "WriteUnsignedNum" => |b| {
        b.pop(Reg::R0);
        b.asm("STR R0, .WriteUnsignedNum");
        b.ret();
    },
    "ReadString" => |b| {
        b.pop(Reg::R0);
        b.asm("STR R0, .ReadString");
        b.ret();
    },
    "memcpy" => |b| {
        let loop_label = b.create_label("loop");
        let done_label = b.create_label("done");

        b.header("R0: Destination address\nR1: Source address\nR2: Byte count");
        b.pop([Reg::R0, Reg::R1, Reg::R2]);

        b.header("Check if caller asked us to copy 0 bytes for some reason.");
        b.cmp(Reg::R2, 0);
        b.beq(done_label);

        b.label(loop_label);
        b.asm("SUBS R2, R2, #1");
        b.beq(done_label);
        b.ldrb(Reg::R3, Reg::R1 + Reg::R2);
        b.strb(Reg::R3, Reg::R0 + Reg::R2);
        b.b(loop_label);

        b.label(done_label);
        b.ret();
    },
    "Panic" => |b| {
        b.asm("MOV R0, #const_str_PanicMessage");
        b.asm("STR R0, .WriteString");
        b.call("WriteString");
        b.asm("B c_halt");
        b.asm("const_str_PanicMessage: .ASCIZ \"\\n\\nProgram panicked with error message: \"");
    },
    "Exit" => |b| {
        b.pop(Reg::R0);
        b.asm("B c_entry_post_run");
    },
};

pub struct Generator {
    program: Program,
    file_builder: FileBuilder,
    ctype_size_cache: RefCell<HashMap<CType, u32>>,
}

impl Generator {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            file_builder: FileBuilder::default(),
            ctype_size_cache: HashMap::default().into(),
        }
    }

    pub fn generate(self) -> String {
        for (name, cfunc) in self.program.get_defined_functions() {
            let sig = self.program.get_cfunc_sig(cfunc.sig_id);

            self.file_builder.create_function(name, sig, |b| {
                if !b.sig.args.is_empty() {
                    b.append_doc_line("# Arguments");

                    for arg in &b.sig.args {
                        match &arg.name {
                            Some(name) => b.append_doc_line(format!(
                                "- {name} ({})",
                                self.program.format_ctype(arg.ctype)
                            )),
                            None => b.append_doc_line(format!(
                                "- Unnamed argument ({})",
                                self.program.format_ctype(arg.ctype)
                            )),
                        };
                    }
                }

                // Hack to allow built-ins (inline asm) to work with the existing system :)
                if let Some(func) = BUILTIN_FUNCS.get(name) {
                    return func(b);
                }

                FuncGenerator::new(&self, b).generate_func(cfunc);
            });
        }

        self.file_builder.build()
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
