//! ARMLite code generation from a `Program`.

use std::collections::HashMap;

use crate::parser::{
    program::{
        statement::{Block, Statement},
        types::{BuiltInType, Type, TypeInfo},
        Program,
    },
    TodoType,
};

pub fn generate(program: Program) -> String {
    let mut lines: Vec<String> = vec![];

    for (name, func) in program.get_functions() {
        lines.push("".to_string());

        if !func.args.is_empty() {
            lines.push("; # Arguments".to_string());

            for arg in &func.args {
                lines.push(format!("; - {:?}", arg.name));
            }
        }

        lines.push(format!("{name}:"));

        if let Some(Block(statements)) = &func.body {
            for statement in statements {
                // match statement {
                //     Statement::Block(_) => panic!("lone blocks are not valid c!"),

                //     Statement::Declare(variable) => todo!(),

                //     Statement::Expr(expr) => todo!(),

                //     Statement::If {
                //         condition,
                //         if_true,
                //         if_false,
                //     } => todo!(),

                //     Statement::While { condition, block } => todo!(),

                //     Statement::Return(expr) => todo!(),
                // }

                lines.push(format!("\t{statement:?}"));
            }
        }

        lines.push("\tret".to_string());
    }

    lines.join("\n")
}

struct Generator {
    program: Program,
}

impl Generator {
    fn new(program: Program) -> Self {
        Self { program }
    }

    fn sizeof_type_in_bytes(&self, type_ref: &Type) -> usize {
        let concrete_type = self
            .program
            .resolve_concrete_type(type_ref)
            .expect("type must be defined");

        match concrete_type {
            TypeInfo::Pointer(_) => 4,

            TypeInfo::BuiltIn(built_in_type) => match built_in_type {
                BuiltInType::Void => panic!("void doesn't have a size!"),
                BuiltInType::Bool => 1,
                BuiltInType::Char => 1,
                BuiltInType::SignedChar => 1,
                BuiltInType::UnsignedChar => 1,
                BuiltInType::Short => 2,
                BuiltInType::UnsignedShort => 2,
                BuiltInType::Int => 4,
                BuiltInType::UnsignedInt => 4,
                BuiltInType::Long => 4,
                BuiltInType::UnsignedLong => 4,
                BuiltInType::LongLong => 8,
                BuiltInType::UnsignedLongLong => 8,
                BuiltInType::Float => 2,
                BuiltInType::Double => 4,
                BuiltInType::LongDouble => 4,
            },

            TypeInfo::Struct(inner) => inner
                .members
                .map(|members| {
                    members
                        .iter()
                        .map(|member| self.sizeof_type_in_bytes(&member.type_info))
                        .sum()
                })
                .unwrap_or(0),

            TypeInfo::Enum(_) => 4,
            TypeInfo::Function(_) => 4,
            TypeInfo::Const(inner) => self.sizeof_type_in_bytes(&inner),
        }
    }
}

struct RegisterHandler {}

impl RegisterHandler {
    fn give_me_a_register(&mut self) -> Reg {
        todo!("no. you cant have any")
    }

    fn num_registers_required_for(size_in_bytes: usize) -> usize {
        size_in_bytes / 4
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Reg {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R12,
    R13,
    R14,
    R15,
    FramePtr,
    StackPtr,
    InstPtr,
}
