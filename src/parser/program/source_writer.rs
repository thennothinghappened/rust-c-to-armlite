use itertools::Itertools;

use crate::parser::program::{
    ctype::{CConcreteType, CType},
    expr::{
        call::Call, BinaryBitwiseOp, BinaryOp, BitwiseShiftDirection, CompareMode, Expr, OrderMode,
        UnaryOp,
    },
    statement::{Statement, Variable},
    Program,
};

/// Formatter to take AST information from a `Program` and convert it back to readable source code.
pub struct SourceWriter {
    indentation_string: String,
}

impl Default for SourceWriter {
    fn default() -> Self {
        Self::new(4)
    }
}

impl SourceWriter {
    pub fn new(indent_by: usize) -> Self {
        Self {
            indentation_string: " ".repeat(indent_by),
        }
    }

    /// Format an AST statement as a human-readable representation of the C source.
    pub fn format_statement(&self, program: &Program, statement: &Statement) -> String {
        match statement {
            Statement::Block(block) => {
                let mut str = "{".to_owned();

                if !block.statements.is_empty() {
                    str += "\n";

                    for statement in &block.statements {
                        for line in self.format_statement(program, statement).lines() {
                            str += &format!("{}{line}\n", self.indentation_string);
                        }
                    }
                }

                str += "}";
                str
            }

            Statement::Declare(Variable { name, ctype, value }) => {
                let typename = self.format_ctype(program, *ctype);

                if let Some(expr) = value {
                    format!(
                        "{typename} {name} = {value};",
                        value = self.format_expr(program, expr)
                    )
                } else {
                    format!("{typename} {name};")
                }
            }

            Statement::Expr(expr) => format!("{expr};", expr = self.format_expr(program, expr)),

            Statement::If {
                condition,
                if_true,
                if_false: None,
            } => format!(
                "if ({condition}) {if_true}",
                condition = self.format_expr(program, condition),
                if_true = self.format_statement(program, if_true),
            ),

            Statement::If {
                condition,
                if_true,
                if_false: Some(if_false),
            } => format!(
                "if ({condition}) {if_true} else {if_false}",
                condition = self.format_expr(program, condition),
                if_true = self.format_statement(program, if_true),
                if_false = self.format_statement(program, if_false),
            ),

            Statement::While { condition, block } => format!(
                "while ({condition}) {block}",
                condition = self.format_expr(program, condition),
                block = self.format_statement(program, block)
            ),

            Statement::Return(None) => "return;".to_owned(),

            Statement::Return(Some(expr)) => {
                format!("return {expr};", expr = self.format_expr(program, expr))
            }

            Statement::Break => "break;".to_owned(),

            Statement::Continue => "continue;".to_owned(),

            Statement::Empty => ";".to_owned(),

            Statement::Asm(asm) => format!("__asm__(\"{asm}\");"),
        }
    }

    /// Format an AST expression as a human-readable representation of the C source.
    pub fn format_expr(&self, program: &Program, expr: &Expr) -> String {
        match expr {
            Expr::StringLiteral(content) => format!("\"{content}\""),
            Expr::IntLiteral(int) => format!("{int}"),
            Expr::BoolLiteral(bool) => format!("{bool}"),
            Expr::NullPtr => "nullptr".to_owned(),
            Expr::Reference(var_name) => var_name.clone(),
            Expr::Call(Call { target, args }) => self.format_call(program, target, args),
            Expr::BinaryOp(op, lhs, rhs) => self.format_binary_op(program, op, lhs, rhs),

            Expr::Cast(expr, ctype) => {
                format!(
                    "({typename}) {expr}",
                    typename = self.format_ctype(program, *ctype),
                    expr = self.format_expr(program, expr),
                )
            }

            Expr::UnaryOp(op, expr) => {
                let expr = self.format_expr(program, expr);

                match op {
                    UnaryOp::IncrementThenGet => format!("++{expr}"),
                    UnaryOp::GetThenIncrement => format!("{expr}++"),
                    UnaryOp::DecrementThenGet => format!("--{expr}"),
                    UnaryOp::GetThenDecrement => format!("{expr}--"),
                    UnaryOp::SizeOf => format!("sizeof {expr}"),
                    UnaryOp::BooleanNot => format!("!{expr}"),
                    UnaryOp::Negative => format!("-{expr}"),
                    UnaryOp::AddressOf => format!("&{expr}"),
                    UnaryOp::Dereference => format!("*{expr}"),
                }
            }

            Expr::DotAccess { target, member } => format!(
                "{target}.{member}",
                target = self.format_expr(program, target)
            ),
        }
    }

    /// Format a binary operation expression.
    pub fn format_binary_op(
        &self,
        program: &Program,
        op: &BinaryOp,
        lhs: &Expr,
        rhs: &Expr,
    ) -> String {
        let lhs = self.format_expr(program, lhs);
        let rhs = self.format_expr(program, rhs);

        match op {
            BinaryOp::AndThen => format!("{lhs}, {rhs}"),
            BinaryOp::Assign => format!("{lhs} = {rhs}"),
            BinaryOp::OpAndAssign(op) => format!("{lhs} {op}= {rhs}"),
            BinaryOp::LogicOrdering(mode) => match mode {
                OrderMode::LessThan => format!("{lhs} < {rhs}"),
                OrderMode::LessOrEqual => format!("{lhs} <= {rhs}"),
                OrderMode::GreaterThan => format!("{lhs} > {rhs}"),
                OrderMode::GreaterOrEqual => format!("{lhs} >= {rhs}"),
            },
            BinaryOp::LogicEqual(mode) => match mode {
                CompareMode::Equal => format!("{lhs} == {rhs}"),
                CompareMode::NotEqual => format!("{lhs} != {rhs}"),
            },
            BinaryOp::Plus => format!("{lhs} + {rhs}"),
            BinaryOp::Minus => format!("{lhs} - {rhs}"),
            BinaryOp::ArrayIndex => format!("{lhs}[{rhs}]"),

            BinaryOp::Bitwise(BinaryBitwiseOp::Shift(BitwiseShiftDirection::Left)) => {
                format!("{lhs} << {rhs}")
            }

            BinaryOp::Bitwise(BinaryBitwiseOp::Shift(BitwiseShiftDirection::Right)) => {
                format!("{lhs} >> {rhs}")
            }

            BinaryOp::Bitwise(BinaryBitwiseOp::Xor) => format!("{lhs} ^ {rhs}"),
            BinaryOp::Bitwise(BinaryBitwiseOp::And) => format!("{lhs} & {rhs}"),
            BinaryOp::Bitwise(BinaryBitwiseOp::Or) => format!("{lhs} | {rhs}"),
            BinaryOp::LogicAnd => format!("{lhs} && {rhs}"),
            BinaryOp::LogicOr => format!("{lhs} || {rhs}"),
        }
    }

    /// Format a function call to `target` with the argument list given by `args`.
    pub fn format_call(&self, program: &Program, target: &Expr, args: &[Expr]) -> String {
        format!(
            "{target}({args})",
            target = self.format_expr(program, target),
            args = args
                .iter()
                .map(|arg| self.format_expr(program, arg))
                .join(", "),
        )
    }

    /// Format a type as it would appear in C source code.
    pub fn format_ctype(&self, program: &Program, ctype: impl Into<CType>) -> String {
        let ctype: CType = ctype.into();

        match ctype {
            CType::AsIs(cconcrete_type) => match cconcrete_type {
                CConcreteType::Struct(cstruct_id) => {
                    match program.struct_names.get_by_right(&cstruct_id) {
                        Some(name) => format!("struct {name}"),
                        None => "struct".to_string(),
                    }
                }

                CConcreteType::Enum(cenum_id) => todo!("Enum ctypes"),

                CConcreteType::Func(cfunc_type_id) => {
                    let sig = program.get_signature(cfunc_type_id);

                    format!(
                        "{}(*)({})",
                        self.format_ctype(program, sig.returns),
                        sig.args
                            .iter()
                            .map(|arg| self.format_ctype(program, arg.ctype))
                            .join(", ")
                    )
                }

                CConcreteType::Primitive(cbuiltin_type) => format!("{cbuiltin_type}"),
                CConcreteType::Void => "void".to_string(),
            },

            CType::PointerTo(ctype_id, None) => {
                format!(
                    "{}*",
                    self.format_ctype(program, program.get_ctype(ctype_id))
                )
            }

            CType::PointerTo(ctype_id, Some(length)) => {
                format!(
                    "{}[{length}]",
                    self.format_ctype(program, program.get_ctype(ctype_id))
                )
            }
        }
    }
}
