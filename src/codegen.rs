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
    context::Context,
    parser::{
        program::{
            expr::{call::Call, BinaryOp, Expr},
            statement::{Block, Statement, Variable},
            types::{CConcreteType, CFunc, CFuncType, CType, CTypeBuiltin},
            Program,
        },
        TodoType,
    },
};

const PRELUDE: &str = r#"
; ==================================================================================================
; C RUNTIME PRELUDE
; ==================================================================================================

c_entry:
BL fn_main
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

const_c_entry_exitcode_msg_start:	.ASCIZ "Program exited with code "
const_c_entry_exitcode_msg_end:		.ASCIZ ".\n"

; ==================================================================================================
; END OF PRELUDE
; ==================================================================================================
"#;

static BUILTIN_FUNCS: phf::Map<&str, &str> = phf_map! {
    "WriteString" => "\tPOP {R0}\n\tSTR R0, .WriteString\n\tRET\n",
    "WriteSignedNum" => "\tPOP {R0}\n\tSTR R0, .WriteSignedNum\n\tRET\n",
    "WriteUnsignedNum" => "\tPOP {R0}\n\tSTR R0, .WriteUnsignedNum\n\tRET\n",
    "ReadString" => "\tPOP {R0}\n\tSTR R0, .ReadString\n\tRET\n",
    "add" => "\tPOP {R0, R1}\n\tADD R0, R0, R1\n\tRET\n"
};

pub struct Generator {
    program: Program,
    constant_strings: RefCell<HashMap<String, String>>,
    next_anon_id: Cell<usize>,
}

impl Generator {
    pub fn new(program: Program) -> Self {
        Self {
            program,
            next_anon_id: 0.into(),
            constant_strings: HashMap::default().into(),
        }
    }

    pub fn generate(self) -> String {
        let mut output = PRELUDE.to_owned();

        for (name, func) in self.program.get_defined_functions() {
            output += &self.generate_func(name, func);
        }

        output += "\n.DATA\n";

        for (name, value) in self.constant_strings.into_inner() {
            output += &format!(
                "{name}: .ASCIZ \"{}\"\n",
                Self::escape_string_literal(&value)
            );
        }

        output
    }

    fn generate_func(&self, func_name: &str, func: &CFunc) -> String {
        let mut output = String::new();
        output += "\n";

        let sig = self.program.get_func_type_by_id(func.sig_id);

        if !sig.args.is_empty() {
            output += "; # Arguments\n";

            for arg in &sig.args {
                match &arg.name {
                    Some(name) => output += &format!("; - {name}\n"),
                    None => output += "; - Unnamed argument\n",
                };
            }
        }

        output += &format!("{}:\n", self.name_of_func(func_name));

        // Hack to allow built-ins (inline asm) to work with the existing system :)
        if let Some(builtin_content) = BUILTIN_FUNCS.get(func_name) {
            output += builtin_content;
            return output;
        }

        let Some(Block(statements)) = &func.body else {
            panic!("Function {func:?} left undefined!");
        };

        output += "\tPUSH {R11, LR}\n";
        output += "\tMOV R11, SP\n";

        let mut scope = GenScope::new(
            self,
            func_name,
            self.program.get_func_type_by_id(func.sig_id),
        );

        let body_content = self.generate_block(&mut scope, statements);

        output += "\n\t; # Named Stack Variables\n";
        for (name, offset) in scope.named_vars.iter().sorted_by(|&a, &b| a.1.cmp(b.1)) {
            output += &format!("\t; - {name}: [R11-#{offset}]\n");
        }

        output += &format!("\t;\n\t; Stack allocated size = {}\n", scope.stack_top_pos);

        output += &format!("\tSUB SP, SP, #{}\n\n", scope.stack_top_pos);
        output += &body_content;
        output += &format!("{}:\n", self.name_of_label(func_name, "done"));
        output += "\tMOV SP, R11\n";
        output += "\tPOP {R11, LR}\n";
        output += "\tRET\n";

        output
    }

    fn generate_block(&self, scope: &mut GenScope, statements: &Vec<Statement>) -> String {
        let mut out = String::new();

        for statement in statements {
            match statement {
                Statement::Declare(variable) => {
                    let variable_size = self.sizeof_type_in_bytes(&variable.ctype);
                    let variable_offset = scope.allocate_var(variable.name.clone(), variable_size);

                    if let Some(expr) = &variable.value {
                        // eval the initial val.
                        out += &self.generate_expr(
                            scope,
                            expr,
                            Some((variable_offset, &variable.ctype)),
                        );
                    }
                }

                Statement::Expr(expr) => {
                    out += &self.generate_expr(scope, expr, None);
                }

                Statement::Return(expr) => {
                    let return_type_size = self.sizeof_type_in_bytes(&scope.cfunc_type.returns);
                    let temp_storage = scope.allocate_anon(return_type_size);

                    out += "\t; <return>\n";

                    out += &self.generate_expr(
                        scope,
                        expr,
                        Some((temp_storage, &scope.cfunc_type.returns)),
                    );

                    if return_type_size > Self::WORD_SIZE as u32 {
                        // the caller pushes an address where they want us to copy the returned
                        // (presumably) struct to to return it to them, rather than splitting it
                        // across registers or something.
                        out += "\tLDR R0, [R11+#4]\t\t; Grab the return storage address.\n";
                        out += &format!("\tLDR R1, [R11-#{temp_storage}]\n");
                        out += "\tSTR R1, [R0]\n";
                    } else {
                        // we can neatly return the value in one register. yay!
                        out += &format!(
                            "\tLDR R0, [R11-#{temp_storage}]\t\t; Put the returned value in R0.\n"
                        );
                    }

                    scope.forget(temp_storage);

                    out += &format!(
                        "\tB {}\t\t; Perform the cleanup.\n",
                        self.name_of_label(scope.cfunc_name, "done")
                    );

                    out += "\t; </return>\n\n";
                }

                _ => out += &format!("\tUnhandled statement: {statement:?}\n"),
            };
        }

        out
    }

    /// Generate instructions to perform the given operation, returning the instructions required to
    /// perform it, and place the output at `[R11-#result_offset]`.
    fn generate_expr<'a>(
        &self,
        scope: &mut GenScope,
        expr: &'a Expr,
        result_info: Option<(i32, &'a CType)>,
    ) -> String {
        let mut out = String::new();

        match expr {
            Expr::StringLiteral(value) => {
                let Some((result_offset, ctype)) = result_info else {
                    // pointless no-op.
                    return out;
                };

                let anon_name = format!("str_{}", self.next_anon_id());

                out += &format!("\tMOV R0, #{anon_name}\n");
                out += &format!("\tSTR R0, [R11-#{result_offset}]\n");

                self.constant_strings
                    .borrow_mut()
                    .insert(anon_name, value.clone());
            }

            Expr::IntLiteral(value) => {
                let Some((result_offset, ctype)) = result_info else {
                    // pointless no-op.
                    return out;
                };

                // ints are nice and relaxed :)
                out += &format!("\tMOV R0, #{value}\n");
                out += &format!("\tSTR R0, [R11-#{result_offset}]\n");
            }

            Expr::Reference(name) => {
                let Some((result_offset, ctype)) = result_info else {
                    // pointless no-op.
                    return out;
                };

                // Figure out where the hell this reference points!!!

                // is it local?!
                if let Some(source_offset) = scope.offset_of_var(name) {
                    out += &format!("\t; === query localvar `{name}` ===\n");
                    out += &format!("\tLDR R0, [R11-#{source_offset}]\n");
                    out += &format!("\tSTR R0, [R11-#{result_offset}]\n");
                    out += &format!("\t; === done query `{name}` ===\n\n");
                } else {
                    panic!("unknown storage for {name}")
                }
            }

            Expr::Call(Call {
                target,
                args,
                sig_id,
            }) => {
                let sig = self.program.get_func_type_by_id(*sig_id);

                // push args to stack first. caller pushes args, callee expected to pop em. args
                // pushed last first, so top of stack is the first argument.
                let initial_frame_top = scope.stack_top_pos;

                let arg_target_spots = sig
                    .args
                    .iter()
                    .rev()
                    .map(|member| {
                        (
                            scope.allocate_anon(self.sizeof_type_in_bytes(&member.ctype)),
                            &member.ctype,
                        )
                    })
                    .collect_vec();

                assert_eq!(
                    arg_target_spots.len(),
                    args.len(),
                    "must pass the expected arg count in calling {target:?} with signature {sig_id:?}"
                );

                let new_frame_top = scope.stack_top_pos;
                let stack_pointer_adjustment = new_frame_top - initial_frame_top;

                out += &format!("\tSUB SP, SP, #{stack_pointer_adjustment}\t\t\t; Adjust SP to point at 1st call argument!\n");

                for (arg, out_info) in args.iter().zip(&arg_target_spots) {
                    out += &self.generate_expr(scope, arg, Some(*out_info));
                }

                match &**target {
                    Expr::Reference(name) => {
                        out += &format!("\tBL fn_{name}\n");
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

                for (spot, _) in arg_target_spots {
                    scope.forget(spot);
                }

                let Some((out_pos, out_ctype)) = result_info else {
                    return out;
                };

                if self.sizeof_type_in_bytes(&sig.returns) <= Self::WORD_SIZE.into() {
                    out += &format!("\tSTR R0, [R11-#{out_pos}]\n");
                } else {
                    todo!("handle large (>1 register) return types")
                }
            }

            Expr::BinaryOp(op, left, right) => match op {
                BinaryOp::AndThen => todo!(),
                BinaryOp::Assign => todo!(),
                BinaryOp::BooleanEqual => todo!(),
                BinaryOp::LessThan => todo!(),

                BinaryOp::Plus => {
                    // 1. allocate a var for both operands
                    // 2. perform op
                    // 3. store result in new var
                }

                BinaryOp::ArraySubscript => todo!(),
            },

            Expr::UnaryOp(op, expr) => todo!(),
            Expr::Cast(expr, _) => todo!(),
        };

        out
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

    fn escape_string_literal(string: &str) -> String {
        string.replace("\\", "\\\\").replace("\"", "\\\"")
    }

    fn next_anon_id(&self) -> String {
        let id = self.next_anon_id.get();
        self.next_anon_id.update(|next_id| next_id + 1);

        id.to_string()
    }

    // fn generate_expr_call(&self, ) -> {

    // }

    fn type_of_expr(&self, expr: &Expr) -> CType {
        todo!("determine the type of an expression")
    }

    const WORD_SIZE: u16 = 4;

    fn sizeof_type_in_bytes(&self, ctype: &CType) -> u32 {
        match ctype {
            CType::PointerTo(_) => Self::WORD_SIZE.into(),

            CType::ArrayOf(inner_id, element_count) => {
                self.sizeof_type_in_bytes(self.program.get_ctype_by_id(*inner_id)) * element_count
            }

            CType::AsIs(concrete) => match concrete {
                CConcreteType::Struct(cstruct_id) => self
                    .program
                    .get_struct_by_id(*cstruct_id)
                    .members
                    .as_ref()
                    .map(|members| {
                        members
                            .iter()
                            .map(|member| self.sizeof_type_in_bytes(&member.ctype))
                            .sum()
                    })
                    .unwrap_or(0),

                CConcreteType::Enum(cenum_id) => Self::WORD_SIZE.into(),
                CConcreteType::Func(_) => Self::WORD_SIZE.into(),

                CConcreteType::Builtin(builtin) => match builtin {
                    CTypeBuiltin::Void => 0,
                    CTypeBuiltin::Bool => 1,
                    CTypeBuiltin::Char => 1,
                    CTypeBuiltin::SignedChar => 1,
                    CTypeBuiltin::UnsignedChar => 1,
                    CTypeBuiltin::Short => 2,
                    CTypeBuiltin::UnsignedShort => 2,
                    CTypeBuiltin::Int => Self::WORD_SIZE.into(),
                    CTypeBuiltin::UnsignedInt => Self::WORD_SIZE.into(),
                    CTypeBuiltin::Long => Self::WORD_SIZE.into(),
                    CTypeBuiltin::UnsignedLong => Self::WORD_SIZE.into(),
                    CTypeBuiltin::LongLong => 8,
                    CTypeBuiltin::UnsignedLongLong => 8,
                    CTypeBuiltin::Float => 2,
                    CTypeBuiltin::Double => 4,
                    CTypeBuiltin::LongDouble => 4,
                },
            },
        }
    }
}

struct GenScope<'a> {
    generator: &'a Generator,
    cfunc_name: &'a str,
    cfunc_type: &'a CFuncType,
    stack_top_pos: i32,
    named_vars: HashMap<String, i32>,
    frame: Vec<(u32, bool)>,
}

impl<'a> GenScope<'a> {
    pub fn new(generator: &'a Generator, cfunc_name: &'a str, cfunc_type: &'a CFuncType) -> Self {
        Self {
            generator,
            cfunc_name,
            cfunc_type,
            stack_top_pos: 0,
            named_vars: HashMap::default(),
            frame: Vec::default(),
        }
    }

    /// Allocate space for a local variable, return its stack frame offset.
    pub fn allocate_var(&mut self, name: String, size: u32) -> i32 {
        let offset = self.allocate_anon(size);
        self.named_vars.insert(name, offset);

        offset
    }

    /// Allocate space for an anonymous variable, return its stack frame offset.
    pub fn allocate_anon(&mut self, size: u32) -> i32 {
        // look for a free spot first before allocing.
        let mut offset = 0;

        for (spot_size, in_use) in self.frame.iter_mut() {
            offset += *spot_size;

            if *spot_size == size && !*in_use {
                *in_use = true;
                return offset as i32;
            }
        }

        self.stack_top_pos += size as i32;
        self.frame.push((size, true));

        self.stack_top_pos
    }

    /// Forget about a specific variable or anon var in the stack. its spot can now get recycled.
    pub fn forget(&mut self, offset: i32) {
        if offset == self.stack_top_pos {
            self.frame.last_mut().unwrap().1 = false;

            // cascade downward and clear out all the unused vars before us.
            while let Some((size, _)) = self.frame.pop_if(|(_, in_use)| !*in_use) {
                self.stack_top_pos -= size as i32;
            }

            println!(
                "frame after pop: {:?}, top = {}",
                self.frame, self.stack_top_pos
            );

            return;
        }

        // stack surgery time (aka me writing horribly inefficient code that's Good Enough(tm) for learning)
        let mut total_offset = 0;

        for (size, in_use) in self.frame.iter_mut() {
            total_offset += *size;

            // if this is the target, mark as free.
            if total_offset == offset as u32 {
                *in_use = false;
                break;
            }
        }
    }

    /// Get the offset of a previously defined local variable, if it exists.
    pub fn offset_of_var(&self, name: &str) -> Option<i32> {
        self.named_vars.get(name).copied()
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
