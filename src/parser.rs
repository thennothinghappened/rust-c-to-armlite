use std::collections::{HashMap, VecDeque};

use thiserror::Error;

use crate::{
    lexer::{tokenkind::TokenKind, Lexer, Token},
    parser::{
        block_builder::BlockBuilder,
        program::{
            ctype::{
                CConcreteType, CFunc, CFuncBody, CPrimitive, CSig, CSigId, CStruct, CType, Member,
            },
            expr::{call::Call, BinaryOp, BindingPower, CompareMode, Expr, OrderMode, UnaryOp},
            statement::{Block, Statement, Variable},
            Program, StructBuilder, Symbol,
        },
    },
    span::Span,
};
pub mod program;

mod block_builder;
mod parser_advancing;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    program: program::Program,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            program: Program::new(lexer.context.target),
            lexer,
        }
    }

    /// Parse the top level of the program, which may include:
    /// 1. Type declarations.
    /// 2. Global variables.
    /// 3. Function declarations (& definitions.)
    pub fn parse(mut self) -> Result<Program, ParseError> {
        loop {
            match self.peek().kind {
                TokenKind::Semicolon => {
                    self.next();
                }

                TokenKind::TypeDef => {
                    self.parse_typedef()?;
                }

                TokenKind::Eof => break,

                _ => {
                    let Some(statement) = self.parse_func_or_var_decl()? else {
                        continue;
                    };

                    match statement {
                        Statement::Declare(variable) => {
                            self.program.create_global_variable(variable).unwrap();
                        }

                        _ => panic!("Invalid top-level statement {statement:?}"),
                    }
                }
            };
        }

        Ok(self.program)
    }

    fn parse_func_or_var_decl(&mut self) -> Result<Option<Statement>, ParseError> {
        let mut is_raw_assembly = false;
        let mut is_noreturn = false;

        if self.accept(TokenKind::OpenAttributeList) {
            while !self.accept(TokenKind::CloseAttributeList) {
                let namespace_or_name = self.consume_ident()?;

                let (namespace, name) = if self.accept(TokenKind::NamespaceAccessor) {
                    let name = self.consume_ident()?;

                    (Some(namespace_or_name), name)
                } else {
                    (None, namespace_or_name)
                };

                match namespace.as_deref() {
                    // One of our custom attributes!
                    Some("armlite_c") => match name.as_str() {
                        "raw_assembly" => is_raw_assembly = true,
                        _ => return self.error(format!("Unknown attribute armlite_c::{name}")),
                    },

                    // Standard attribute.
                    None => match name.as_str() {
                        "noreturn" => is_noreturn = true,
                        "deprecated" => (),
                        _ => (),
                    },

                    // Some other compiler's namespace.
                    _ => {}
                };

                if !self.accept(TokenKind::Comma) {
                    self.expect(TokenKind::CloseAttributeList)?;
                    break;
                }
            }
        }

        let is_extern = self.accept(TokenKind::Extern);

        // This might be a lone type declaration, or it might be the start of a
        // function, or a variable...
        let initial_type = self.parse_type()?;

        if self.accept(TokenKind::Semicolon) {
            // Lone type definition.
            return Ok(None);
        }

        let (name, this_type) = self.parse_variable_name(initial_type)?;

        if self.next_is(TokenKind::OpenParen) {
            // Function declaration!
            let return_ctype = this_type;

            // Parse args.
            let args = self.parse_func_decl_args()?;

            let func = CFunc {
                sig_id: self.program.get_signature_id(CSig {
                    args,
                    returns: return_ctype,
                    is_noreturn,
                }),
                body: if is_extern {
                    CFuncBody::Extern
                } else {
                    CFuncBody::None
                },
                is_raw_assembly,
            };

            // Pre-declare the function w/o body, so it is available for recursion.
            self.program
                .create_function(name.clone(), func.clone())
                .unwrap();

            // May or may not have function body.
            if self.next_is(TokenKind::OpenCurly) {
                let body = self.parse_block(BlockBuilder::new())?;

                self.program
                    .set_function_body(&name, body)
                    .map_err(|err| self.bad_definition(Span::at(self.lexer.index), err))?;
            } else {
                self.expect(TokenKind::Semicolon)?;
            };

            return Ok(None);
        }

        // Global variable decl (optionally with initial value!)
        let value = if self.accept(TokenKind::Assign) {
            Some(self.parse_expr(1)?)
        } else {
            None
        };

        Ok(Some(Statement::Declare(Variable {
            name,
            ctype: this_type,
            value,
        })))
    }

    fn parse_variable_name(&mut self, initial_type: CType) -> Result<(String, CType), ParseError> {
        if self.accept(TokenKind::OpenParen) {
            let (name, mut this_type) = self.parse_variable_name(initial_type)?;

            if self.next_is(TokenKind::OpenParen) {
                // function pointer AHHHHHHHHHHHH
                let CType::PointerTo(return_ctype_id, None) = this_type else {
                    return self.error("Expecting a pointer when parsing a function type");
                };

                let args = self.parse_func_decl_args()?;

                this_type = CType::AsIs(CConcreteType::Func(self.program.get_signature_id(CSig {
                    args,
                    returns: self.program.get_ctype(return_ctype_id),
                    is_noreturn: false,
                })));
            }

            self.expect(TokenKind::CloseParen)?;
            return Ok((name, this_type));
        }

        let mut this_type = self.parse_type_pointersssss(initial_type)?;
        let name = self.consume_ident()?;

        if self.accept(TokenKind::OpenSquare) {
            let element_count = self
                .maybe_map_next(|kind| match kind {
                    TokenKind::IntLiteral(element_count) => Some(element_count),
                    _ => None,
                })
                .ok_or_else(|| ParseError {
                    span: Span::at(self.lexer.index),
                    kind: ParseErrorKind::UnexpectedToken(self.next().kind),
                })?;

            self.expect(TokenKind::CloseSquare)?;

            this_type = CType::array_of(
                self.program.get_ctype_id(&this_type),
                element_count
                    .try_into()
                    .expect("must be a positive array size"),
            );
        }

        Ok((name.to_string(), this_type))
    }

    fn parse_type_pointersssss(&mut self, initial_type: CType) -> Result<CType, ParseError> {
        let mut this_type = initial_type;

        // Evil right-associative pointer syntax >:(((
        while self.accept(TokenKind::Star) {
            this_type = self.program.pointer_to(this_type);
        }

        Ok(this_type)
    }

    fn parse_func_decl_args(&mut self) -> Result<Vec<Member>, ParseError> {
        let mut args: Vec<Member> = Vec::new();

        self.expect(TokenKind::OpenParen)?;

        while !self.accept(TokenKind::CloseParen) {
            let initial_type = self.parse_type()?;
            let arg_type = self.parse_type_pointersssss(initial_type)?;
            let arg_name = self.accept_ident();

            args.push(Member {
                name: arg_name,
                ctype: arg_type,
            });

            if self.accept(TokenKind::CloseParen) {
                break;
            }

            self.expect(TokenKind::Comma)?;
        }

        Ok(args)
    }

    fn parse_block(&mut self, mut scope: BlockBuilder) -> Result<Block, ParseError> {
        let mut statements: Vec<Statement> = Vec::new();

        self.expect(TokenKind::OpenCurly)?;
        self.consume_semicolons();

        loop {
            self.consume_semicolons();

            if self.accept(TokenKind::CloseCurly) {
                break;
            }

            statements.push(self.parse_statement(&mut scope)?);
        }

        Ok(Block {
            statements,
            vars: scope.build(),
        })
    }

    fn parse_statement(&mut self, scope: &mut BlockBuilder) -> Result<Statement, ParseError> {
        match self.peek().kind {
            TokenKind::Semicolon => {
                self.next();

                Ok(Statement::Empty)
            }

            TokenKind::OpenCurly => {
                scope.with_child_scope(|scope| Ok(self.parse_block(scope)?.into()))
            }

            TokenKind::If => {
                self.next();

                let condition = self.parse_expr_in_brackets()?;
                let if_true = self.parse_statement(scope)?;

                if !self.accept(TokenKind::Else) {
                    return Ok(Statement::If {
                        condition: Box::new(condition),
                        if_true: Box::new(if_true),
                        if_false: None,
                    });
                };

                let if_false = self.parse_statement(scope)?;

                Ok(Statement::If {
                    condition: Box::new(condition),
                    if_true: Box::new(if_true),
                    if_false: Some(Box::new(if_false)),
                })
            }

            TokenKind::While => {
                self.next();

                let condition = self.parse_expr_in_brackets()?;
                let block = self.parse_statement(scope)?;

                Ok(Statement::While {
                    condition: Box::new(condition),
                    block: Box::new(block),
                })
            }

            TokenKind::For => {
                self.next();

                scope.with_child_scope(|mut scope| {
                    self.expect(TokenKind::OpenParen)?;

                    let initialisation_statement = if !self.accept(TokenKind::Semicolon) {
                        self.parse_statement(&mut scope)?
                    } else {
                        Statement::Empty
                    };

                    let loop_condition = if !self.next_is(TokenKind::Semicolon) {
                        Some(self.parse_expr(0)?)
                    } else {
                        None
                    };

                    self.expect(TokenKind::Semicolon)?;

                    let repeated_expression = if !self.next_is(TokenKind::CloseParen) {
                        Some(self.parse_expr(0)?)
                    } else {
                        None
                    };

                    self.expect(TokenKind::CloseParen)?;

                    let mut block = self.parse_statement(&mut scope)?;

                    if let Some(repeated_expression) = repeated_expression {
                        block = Statement::Block(Block {
                            statements: vec![block, Statement::Expr(repeated_expression)],
                            vars: HashMap::default(),
                        });
                    }

                    Ok(Statement::Block(Block {
                        statements: vec![
                            initialisation_statement,
                            Statement::While {
                                condition: Box::new(loop_condition.unwrap_or(Expr::IntLiteral(1))),
                                block: Box::new(block),
                            },
                        ],
                        vars: HashMap::default(),
                    }))
                })
            }

            TokenKind::Return => {
                self.next();

                let value = self.parse_expr(0)?;
                self.expect(TokenKind::Semicolon)?;

                Ok(Statement::Return(Box::new(value)))
            }

            TokenKind::Break => {
                self.next();
                self.expect(TokenKind::Semicolon)?;

                Ok(Statement::Break)
            }

            TokenKind::Continue => {
                self.next();
                self.expect(TokenKind::Semicolon)?;

                Ok(Statement::Continue)
            }

            TokenKind::Asm => {
                self.next();

                self.expect(TokenKind::OpenParen)?;
                let content = self.consume_string()?;
                self.expect(TokenKind::CloseParen)?;

                Ok(Statement::Asm(content))
            }

            _ => {
                // Whatever is left could either be a variable declaration, or could be a lone
                // expression. We'll cheat by spawning two clones of this parser and parsing both
                // possible interpretations, then picking the one that works.

                let old_lexer = self.lexer.clone();
                let parse_as_variable_result = self.parse_variable_decl(scope);

                if let Ok(decl) = parse_as_variable_result {
                    self.expect(TokenKind::Semicolon)?;
                    return Ok(decl);
                }

                self.lexer = old_lexer;
                let parse_as_expr_result = self.parse_expr(0);

                if let Ok(expr) = parse_as_expr_result {
                    self.expect(TokenKind::Semicolon)?;
                    return Ok(Statement::Expr(expr));
                }

                let parse_as_variable_error = parse_as_variable_result.unwrap_err();
                let parse_as_expr_error = parse_as_expr_result.unwrap_err();

                Err(ParseError {
                    span: parse_as_variable_error.span.union(parse_as_expr_error.span),
                    kind: ParseErrorKind::Lazy(format!(
                        "Not a variable decl: {}\nAnd not an expression: {}",
                        parse_as_variable_error, parse_as_expr_error
                    )),
                })
            }
        }
    }

    fn parse_variable_decl(&mut self, scope: &mut BlockBuilder) -> Result<Statement, ParseError> {
        let initial_type = self.parse_type()?;

        // todo: support multiple decls on same line (ew)
        let (var_name, var_ctype) = self.parse_variable_name(initial_type)?;

        let value = if self.accept(TokenKind::Assign) {
            Some(self.parse_expr(1)?)
        } else {
            None
        };

        scope.declare_var(var_name.clone(), var_ctype);

        Ok(Statement::Declare(Variable {
            name: var_name,
            ctype: var_ctype,
            value,
        }))
    }

    fn parse_expr_in_brackets(&mut self) -> Result<Expr, ParseError> {
        self.expect(TokenKind::OpenParen)?;
        let expr = self.parse_expr(0)?;
        self.expect(TokenKind::CloseParen)?;

        Ok(expr)
    }

    fn parse_expr(&mut self, min_power: i32) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_unary_expr()?;

        while let Some(op) = self.maybe_map_next(|token| {
            match token {
                TokenKind::Comma => Some(BinaryOp::AndThen),
                TokenKind::Assign => Some(BinaryOp::Assign),
                TokenKind::PlusAssign => Some(BinaryOp::OpAndAssign(BinaryOp::Plus.into())),
                TokenKind::MinusAssign => Some(BinaryOp::OpAndAssign(BinaryOp::Minus.into())),
                TokenKind::BooleanEqual => Some(BinaryOp::LogicEqual(CompareMode::Equal)),
                TokenKind::BooleanNotEqual => Some(BinaryOp::LogicEqual(CompareMode::NotEqual)),
                TokenKind::BooleanAnd => Some(BinaryOp::LogicAnd),
                TokenKind::BooleanOr => Some(BinaryOp::LogicOr),
                TokenKind::LessThan => Some(BinaryOp::LogicOrdering(OrderMode::LessThan)),
                TokenKind::GreaterThan => Some(BinaryOp::LogicOrdering(OrderMode::GreaterThan)),
                TokenKind::Plus => Some(BinaryOp::Plus),
                TokenKind::Minus => Some(BinaryOp::Minus),
                _ => None,
            }
            .filter(|op| op.binding_strength() >= min_power)
        }) {
            let binding_strength = op.binding_strength();

            lhs = Expr::BinaryOp(
                op,
                Box::new(lhs),
                Box::new(self.parse_expr(binding_strength)?),
            );
        }

        Ok(lhs)
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, ParseError> {
        if self.next_is(TokenKind::OpenParen) {
            let old_lexer = self.lexer.clone();

            match self.parse_cast_expr() {
                Ok(expr) => return Ok(expr),
                Err(_) => {
                    // Roll back the lexer.
                    self.lexer = old_lexer;
                }
            }
        }

        let Some(op) = self.maybe_map_next(|token| match token {
            TokenKind::PlusPlus => Some(UnaryOp::IncrementThenGet),
            TokenKind::MinusMinus => Some(UnaryOp::DecrementThenGet),
            TokenKind::SizeOf => Some(UnaryOp::SizeOf),
            TokenKind::BooleanNot => Some(UnaryOp::BooleanNot),
            TokenKind::Ampersand => Some(UnaryOp::AddressOf),
            TokenKind::Star => Some(UnaryOp::Dereference),
            _ => None,
        }) else {
            return self.parse_postfix_expr();
        };

        let operand = self.parse_unary_expr()?;
        Ok(Expr::UnaryOp(op, Box::new(operand)))
    }

    fn parse_postfix_expr(&mut self) -> Result<Expr, ParseError> {
        let mut operand = self.parse_terminal_expr()?;

        loop {
            if self.accept(TokenKind::OpenParen) {
                // Function call args!
                let mut args: Vec<Expr> = Vec::new();

                while !self.accept(TokenKind::CloseParen) {
                    // Args are not allowed to use the comma operator, since that'd conflict with
                    // comma being the function arg separator.
                    args.push(self.parse_expr(BinaryOp::AndThen.binding_strength() + 1)?);

                    if !self.accept(TokenKind::Comma) {
                        self.expect(TokenKind::CloseParen)?;
                        break;
                    }
                }

                operand = Expr::Call(Call {
                    target: Box::new(operand),
                    args,
                });

                continue;
            }

            // FIXME: i think currently you can't go like, func()++() tho that seems like nonsense
            // anyway lol
            if let Some(op) = self.maybe_map_next(|token| match token {
                TokenKind::PlusPlus => Some(UnaryOp::GetThenIncrement),
                TokenKind::MinusMinus => Some(UnaryOp::GetThenDecrement),
                _ => None,
            }) {
                operand = Expr::UnaryOp(op, Box::new(operand));
                continue;
            }

            if self.accept(TokenKind::OpenSquare) {
                operand = Expr::BinaryOp(
                    BinaryOp::ArrayIndex,
                    Box::new(operand),
                    Box::new(self.parse_expr(0)?),
                );

                self.expect(TokenKind::CloseSquare)?;
                continue;
            }

            if self.accept(TokenKind::DotAccessor) {
                let member_name = self.consume_ident()?;

                operand = Expr::DotAccess {
                    target: Box::new(operand),
                    member: member_name,
                };

                continue;
            }

            if self.accept(TokenKind::ArrowAccessor) {
                let member_name = self.consume_ident()?;

                operand = Expr::DotAccess {
                    target: Box::new(Expr::UnaryOp(UnaryOp::Dereference, Box::new(operand))),
                    member: member_name,
                };

                continue;
            }

            break;
        }

        Ok(operand)
    }

    fn parse_cast_expr(&mut self) -> Result<Expr, ParseError> {
        self.expect(TokenKind::OpenParen)?;
        let initial_type = self.parse_type()?;
        let target_type = self.parse_type_pointersssss(initial_type)?;
        self.expect(TokenKind::CloseParen)?;

        let expr = self.parse_expr(BinaryOp::AndThen.binding_strength() + 1)?;
        Ok(Expr::Cast(Box::new(expr), target_type))
    }

    fn parse_terminal_expr(&mut self) -> Result<Expr, ParseError> {
        match self.peek().kind {
            TokenKind::Ident(id) => {
                self.next();
                Ok(Expr::Reference(self.lexer.context.get_ident(id)))
            }

            TokenKind::StringLiteral(id) => {
                self.next();
                Ok(Expr::StringLiteral(self.lexer.context.get_ident(id)))
            }

            TokenKind::IntLiteral(int) => {
                self.next();
                Ok(Expr::IntLiteral(int))
            }

            TokenKind::BoolLiteral(bool) => {
                self.next();
                Ok(Expr::BoolLiteral(bool))
            }

            TokenKind::NullPtr => {
                self.next();
                Ok(Expr::NullPtr)
            }

            TokenKind::OpenParen => self.parse_expr_in_brackets(),

            _ => Err(self.unexpected_token()),
        }
    }

    fn parse_typedef(&mut self) -> Result<(), ParseError> {
        self.expect(TokenKind::TypeDef)?;

        let initial_type = self.parse_type()?;
        let pointer_type = self.parse_type_pointersssss(initial_type)?;
        let name = self.consume_ident()?.to_string();

        self.program
            .create_type_alias(name, self.program.get_ctype_id(&pointer_type))
            .map_err(|err| self.bad_definition(Span::at(0), err))?;

        self.expect(TokenKind::Semicolon)?;
        Ok(())
    }

    fn parse_struct(&mut self) -> Result<CType, ParseError> {
        let start = self.expect(TokenKind::Struct)?;
        let struct_name = self.accept_ident();

        if !self.accept(TokenKind::OpenCurly) {
            let Some(struct_name) = struct_name else {
                return Err(self.bad_definition(
                    start.until(self.lexer.index),
                    ParseStructError::UnnamedReference,
                ));
            };

            // Opaque struct.
            return self
                .program
                .create_struct(struct_name, CStruct::opaque())
                .map(|id| CType::AsIs(id.into()))
                .map_err(|err| self.bad_definition(start.until(self.lexer.index), err));
        }

        let mut builder = StructBuilder::new();

        while !self.accept(TokenKind::CloseCurly) {
            // Parse each member!
            let start_pos = self.lexer.index;

            let member_type = self.parse_type()?;
            let (member_name, member_type) = self.parse_variable_name(member_type)?;
            self.expect(TokenKind::Semicolon)?;

            let end_pos = self.lexer.index;

            builder
                .member(Some(member_name), member_type)
                .map_err(|err| {
                    self.bad_definition(Span::new(start_pos, end_pos), anyhow::anyhow!(err))
                })?;
        }

        match struct_name {
            Some(name) => {
                let id = self
                    .program
                    .create_struct(name, builder.build())
                    .map_err(|err| self.bad_definition(start.until(self.lexer.index), err))?;

                Ok(CType::AsIs(id.into()))
            }

            None => Ok(CType::AsIs(
                self.program
                    .create_anonymous_struct(builder.build())
                    .map_err(|err| self.bad_definition(start.until(self.lexer.index), err))?
                    .into(),
            )),
        }
    }

    /// Parse the type given for a symbol, i.e. a variable, struct member etc.
    ///
    /// C's pointer syntax is such that you can't know the full type until you then look at each
    /// variable (or what have you), as the "*" part of the type is right-associative *for some
    /// reason*.
    fn parse_type(&mut self) -> Result<CType, ParseError> {
        let _is_const = self.accept(TokenKind::Const);

        match self.peek().kind {
            // Inline struct type.
            TokenKind::Struct => self.parse_struct(),

            // Typedef'd something-or-other!
            TokenKind::Ident(id) => {
                self.next();

                self.program
                    .get_type_alias_by_name(&self.lexer.context.get_ident(id))
                    .ok_or_else(|| self.unexpected_token())
            }

            // Maybe we've got a number?
            _ => self.parse_numeric_type().map(CType::AsIs),
        }
    }

    fn parse_numeric_type(&mut self) -> Result<CConcreteType, ParseError> {
        if self.accept(TokenKind::Unsigned) {
            return self
                .parse_numeric_type()?
                .unsigned()
                .ok_or_else(|| todo!("handle bad numeric unsigned"));
        }

        if self.accept(TokenKind::Signed) {
            return self
                .parse_numeric_type()?
                .signed()
                .ok_or_else(|| todo!("handle bad numeric signed"));
        }

        if let Some(basic_type) = self.maybe_map_next(|token| match token {
            TokenKind::Void => Some(CConcreteType::Void),
            TokenKind::Bool => Some(CPrimitive::Bool.into()),
            TokenKind::Int => Some(CPrimitive::Int.into()),
            TokenKind::Char => Some(CPrimitive::Char.into()),
            TokenKind::Float => Some(CPrimitive::Float.into()),
            TokenKind::Double => Some(CPrimitive::Double.into()),
            _ => None,
        }) {
            return Ok(basic_type);
        }

        if self.accept(TokenKind::Short) {
            if self.accept(TokenKind::Int) {
                return Ok(CPrimitive::Short.into());
            }

            return Ok(CPrimitive::Short.into());
        }

        if self.accept(TokenKind::Long) {
            if self.accept(TokenKind::Long) {
                // long long long long long long long long int!!!!!!!!!
                // (optional "int" suffix here)
                self.accept(TokenKind::Int);

                return Ok(CPrimitive::LongLong.into());
            }

            if self.accept(TokenKind::Double) {
                return Ok(CPrimitive::LongDouble.into());
            }

            // again, optional "int".
            self.accept(TokenKind::Int);

            return Ok(CPrimitive::Long.into());
        }

        Err(self.unexpected_token())
    }

    fn consume_semicolons(&mut self) {
        while self.accept(TokenKind::Semicolon) {}
    }

    fn unexpected_eof(&self) -> ParseError {
        ParseError {
            span: Span::at(self.lexer.index),
            kind: ParseErrorKind::EarlyEof,
        }
    }

    fn unexpected_token(&mut self) -> ParseError {
        let Token { kind, span } = self.peek();

        if kind == TokenKind::Eof {
            return self.unexpected_eof();
        }

        ParseError {
            span,
            kind: ParseErrorKind::UnexpectedToken(kind),
        }
    }

    fn bad_definition<S: Into<Span>, E: Into<ParseErrorKind>>(
        &self,
        span: S,
        err: E,
    ) -> ParseError {
        ParseError {
            span: span.into(),
            kind: err.into(),
        }
    }

    /// create a generic (ie, lazy) parser error right here, right now
    fn error<S: Into<String>, T>(&self, message: S) -> Result<T, ParseError> {
        Err(ParseError {
            span: self.lexer.index.into(),
            kind: ParseErrorKind::Lazy(message.into()),
        })
    }
}

#[derive(Debug, Error)]
#[error("{kind} at {span}")]
pub struct ParseError {
    pub span: Span,
    pub kind: ParseErrorKind,
}

#[derive(Debug, Error)]
pub enum ParseErrorKind {
    #[error("Unexpected token {0}")]
    UnexpectedToken(TokenKind),

    #[error("Unexpected token {actual}, expected {expected}")]
    WrongToken {
        expected: TokenKind,
        actual: TokenKind,
    },

    #[error("Unexpected End Of File")]
    EarlyEof,

    #[error(transparent)]
    ParseStructError(#[from] ParseStructError),

    /// lazy catch-all error because the alternative is panic or spending 1000000 years making error
    /// kinds when really this project needs a big ol' refactor!!
    #[error("{0}")]
    Lazy(String),
}

impl From<anyhow::Error> for ParseErrorKind {
    fn from(value: anyhow::Error) -> Self {
        Self::Lazy(value.to_string())
    }
}

#[derive(Debug, Error)]
pub enum ParseStructError {
    #[error("Reference to a struct without a name")]
    UnnamedReference,
}
