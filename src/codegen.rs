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
        types::{CBuiltinType, CConcreteType, CFunc, CFuncType, CType},
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
        b.pop(&[Reg::R0]);
        b.asm("STR R0, .WriteString");
        b.ret();
    },
    "WriteSignedNum" => |b| {
        b.pop(&[Reg::R0]);
        b.asm("STR R0, .WriteSignedNum");
        b.ret();
    },
    "WriteUnsignedNum" => |b| {
        b.pop(&[Reg::R0]);
        b.asm("STR R0, .WriteUnsignedNum");
        b.ret();
    },
    "ReadString" => |b| {
        b.pop(&[Reg::R0]);
        b.asm("STR R0, .ReadString");
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
        b.pop(&[Reg::R0]);
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

    fn sizeof_ctype(&self, ctype: CType) -> u32 {
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

                CConcreteType::Builtin(builtin) => match builtin {
                    CBuiltinType::Void => 0,
                    CBuiltinType::Bool => 1,
                    CBuiltinType::Char => 1,
                    CBuiltinType::SignedChar => 1,
                    CBuiltinType::UnsignedChar => 1,
                    CBuiltinType::Short => 2,
                    CBuiltinType::UnsignedShort => 2,
                    CBuiltinType::Int => WORD_SIZE,
                    CBuiltinType::UnsignedInt => WORD_SIZE,
                    CBuiltinType::Long => WORD_SIZE,
                    CBuiltinType::UnsignedLong => WORD_SIZE,
                    CBuiltinType::LongLong => 8,
                    CBuiltinType::UnsignedLongLong => 8,
                    CBuiltinType::Float => 2,
                    CBuiltinType::Double => 4,
                    CBuiltinType::LongDouble => 4,
                },
            },
        };

        self.ctype_size_cache.borrow_mut().insert(ctype, size);
        size
    }
}
