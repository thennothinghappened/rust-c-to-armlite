//! ARMLite code generation from a `Program`.

use std::{cell::Cell, collections::HashMap};

use itertools::Itertools;

use crate::parser::{
    program::{
        expr::{call::Call, Expr},
        statement::{Block, Statement},
        types::{BuiltInType, Function, Type, TypeInfo},
        Program,
    },
    TodoType,
};

const PRELUDE: &str = r#"
; ==================================================================================================
; C RUNTIME PRELUDE
; ==================================================================================================

c_entry:
	BL fn_main

	PUSH {R0}
	MOV R0, #const_c_entry_exitcode_msg_start
	BL fn_WriteString

	POP {R0}
	BL fn_WriteSignedNum

	MOV R0, #const_c_entry_exitcode_msg_end
	BL fn_WriteString
c_halt:
	HLT
	B c_halt

const_c_entry_exitcode_msg_start: .ASCIZ "Program exited with code "
const_c_entry_exitcode_msg_end: .ASCIZ ".\n"

fn_WriteString:
	STR R0, .WriteString
	RET

fn_WriteSignedNum:
	STR R0, .WriteSignedNum
	RET

fn_WriteUnsignedNum:
	STR R0, .WriteUnsignedNum
	RET

fn_ReadString:
	STR R0, .ReadString
	RET

; ==================================================================================================
; BEGIN USER CODE
; ==================================================================================================
"#;

pub struct Generator {
    program: Program,
    next_anon_id: Cell<usize>,
}

impl Generator {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            next_anon_id: 0.into(),
        }
    }

    pub fn generate(self) -> String {
        let mut output = PRELUDE.to_owned();

        for (name, func) in self.program.get_functions() {
            output += &self.generate_func(name, func);
        }

        output
    }

    fn generate_func(&self, func_name: &str, func: &Function) -> String {
        let return_type_size = self.sizeof_type_in_bytes(&func.return_type);

        let mut output = String::new();
        output += "\n";

        if !func.args.is_empty() {
            output += "; # Arguments\n";

            for arg in &func.args {
                match &arg.name {
                    Some(name) => output += &format!("; - {name}\n"),
                    None => output += "; - Unnamed argument\n",
                };
            }
        }

        output += &format!("{}:\n", self.name_of_func(func_name));

        let Some(Block(statements)) = &func.body else {
            panic!("Function {func:?} left undefined!");
        };

        output += "\tPUSH {R11, LR}\n";
        output += "\tMOV R11, SP\n";

        let mut scope = GenScope::new(&self, func_name);
        let body_content = self.generate_block(&mut scope, statements);

        output += "\n\t; # Stack Locals\n";
        for (name, offset) in scope.var_offsets.iter().sorted_by(|&a, &b| a.1.cmp(b.1)) {
            output += &format!("\t; - {name}: [R11-#{offset}]\n");
        }

        output += &format!("\tSUB SP, SP, #{}\n\n", scope.frame_offset);
        output += &body_content;
        output += &format!("{}:\n", self.name_of_label(func_name, "done"));
        output += "\tPOP {R11, LR}\n";
        output += "\tRET\n";

        output
    }

    fn generate_block(&self, scope: &mut GenScope, statements: &Vec<Statement>) -> String {
        let mut body_output = String::new();

        for statement in statements {
            match statement {
                Statement::Declare(variable) => {
                    let variable_size = self.sizeof_type_in_bytes(&variable.var_type);
                    let variable_offset = scope.allocate_var(variable.name.clone(), variable_size);

                    if let Some(expr) = &variable.value {
                        match expr {
                            Expr::StringLiteral(value) => {
                                let string_const_name = self.name_of_constant(&self.next_anon_id());
                                let string_const_after_label =
                                    self.name_of_label(scope.func_name, &self.next_anon_id());

                                body_output += &format!("\tB {string_const_after_label}\n");
                                body_output += &format!(
                                    "{string_const_name}: .ASCIZ \"{}\"\n",
                                    self.escape_string_literal(value)
                                );
                                body_output += &format!("{string_const_after_label}:\n");
                                body_output += &format!("\tMOV R0, #{string_const_name}\n");
                                body_output += &format!("\tSTR R0, [R11-#{variable_offset}]\n");
                            }

                            Expr::IntLiteral(value) => {
                                body_output += &format!("\tMOV R0, #{value}\n");
                                body_output += &format!("\tSTR R0, [R11-#{variable_offset}]\n");
                            }

                            Expr::Reference(name) => {
                                // Figure out where the hell this reference points!!!

                                // is it local?!
                                if let Some(source_var_offset) = scope.offset_of_var(name) {
                                    body_output += &format!(
                                            "\tLDR R0, [R11-#{source_var_offset}]\t\t; Get local `{name}`\n"
                                        );

                                    body_output += &format!(
                                        "\tSTR R0, [R11-#{variable_offset}]\t\t; Store into `{}`\n",
                                        variable.name
                                    );
                                } else {
                                    panic!("unknown storage for {name}")
                                }
                            }

                            Expr::Call(Call { target, args }) => {
                                // push args to stack first
                                for arg in args {}

                                match &**target {
                                    Expr::Reference(name) => {
                                        // resolve what the hell this ref points at. better be a
                                        // goddamn function directly >:(

                                        // pretend theres no local func ptrs for now
                                        if let Some(func) = self.program.get_functions().get(name) {
                                            // hooray for sanity!!!!
                                            body_output += &format!("\tBL fn_{name}\n");
                                        }
                                    }

                                    Expr::Call(call) => todo!("func that returns func >:("),

                                    Expr::BinaryOp(binary_op, expr, expr1) => {
                                        todo!("weird-ass binary op shenanigans returning a func")
                                    }

                                    Expr::UnaryOp(unary_op, expr) => {
                                        todo!("weird-ass unary op shenanigans returning a func")
                                    }

                                    Expr::Cast(expr, _) => todo!("casting func ptrs"),

                                    _ => panic!("bad call target {target:?}"),
                                };
                            }

                            Expr::BinaryOp(binary_op, expr, expr1) => todo!(),
                            Expr::UnaryOp(unary_op, expr) => todo!(),
                            Expr::Cast(expr, _) => todo!(),
                        }
                    }
                }

                _ => body_output += &format!("\tUnhandled statement: {statement:?}\n"),
            };
        }

        body_output
    }

    fn name_of_func(&self, name: &str) -> String {
        format!("fn_{name}")
    }

    fn name_of_label(&self, func_name: &str, label_name: &str) -> String {
        format!("L{label_name}__{}", self.name_of_func(func_name))
    }

    fn name_of_constant(&self, name: &str) -> String {
        format!("const_{name}")
    }

    fn escape_string_literal(&self, string: &str) -> String {
        string.replace("\\", "\\\\").replace("\"", "\\\"")
    }

    fn next_anon_id(&self) -> String {
        let id = self.next_anon_id.get();
        self.next_anon_id.update(|next_id| next_id + 1);

        id.to_string()
    }

    // fn generate_expr_call(&self, ) -> {

    // }

    fn type_of_expr(&self, expr: &Expr) -> Type {
        todo!("determine the type of an expression")
    }

    const WORD_SIZE: u32 = 4;

    fn sizeof_type_in_bytes(&self, type_ref: &Type) -> u32 {
        let concrete_type = self
            .program
            .resolve_concrete_type(type_ref)
            .expect("type must be defined");

        match concrete_type {
            TypeInfo::Pointer(_) => Self::WORD_SIZE,

            TypeInfo::Array(inner, element_count) => {
                self.sizeof_type_in_bytes(&inner) * element_count
            }

            TypeInfo::BuiltIn(built_in_type) => match built_in_type {
                BuiltInType::Void => 0,
                BuiltInType::Bool => 1,
                BuiltInType::Char => 1,
                BuiltInType::SignedChar => 1,
                BuiltInType::UnsignedChar => 1,
                BuiltInType::Short => 2,
                BuiltInType::UnsignedShort => 2,
                BuiltInType::Int => Self::WORD_SIZE,
                BuiltInType::UnsignedInt => Self::WORD_SIZE,
                BuiltInType::Long => Self::WORD_SIZE,
                BuiltInType::UnsignedLong => Self::WORD_SIZE,
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

            TypeInfo::Enum(_) => Self::WORD_SIZE,
            TypeInfo::Const(inner) => self.sizeof_type_in_bytes(&inner),
        }
    }
}

struct GenScope<'a> {
    generator: &'a Generator,
    func_name: &'a str,
    frame_offset: u32,
    var_offsets: HashMap<String, u32>,
}

impl<'a> GenScope<'a> {
    pub fn new(generator: &'a Generator, func_name: &'a str) -> Self {
        Self {
            generator,
            func_name,
            frame_offset: 0,
            var_offsets: HashMap::default(),
        }
    }

    /// Allocate space for a local variable, return its stack frame offset.
    pub fn allocate_var(&mut self, name: String, size: u32) -> u32 {
        self.frame_offset += size;
        self.var_offsets.insert(name, self.frame_offset);

        self.frame_offset
    }

    /// Get the offset of a previously defined local variable, if it exists.
    pub fn offset_of_var(&self, name: &str) -> Option<u32> {
        self.var_offsets.get(name).copied()
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
