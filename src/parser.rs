use thiserror::Error;

use crate::{
    lexer::{tokenkind::TokenKind, Lexer, LexerError, LexerErrorKind, Token},
    parser::program::{
        expr::{call::Call, BinaryOp, BindingPower, Expr, UnaryOp},
        statement::{Statement, Variable},
        types::{BuiltInType, Function, Member, Struct, Type, TypeDef, TypeInfo},
        DefineTypeError, Program, StructBuilder,
    },
    span::Span,
};
pub mod program;

pub enum TodoType {}

#[derive(Clone)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    program: program::Program,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer,
            program: Program::default(),
        }
    }

    /// Parse the top level of the program, which may include:
    /// 1. Type declarations.
    /// 2. Global variables.
    /// 3. Function declarations (& definitions.)
    pub fn parse(mut self) -> Result<Program, ParseError> {
        loop {
            match self.lexer.peek().kind {
                TokenKind::Semicolon => {
                    self.lexer.next();
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
                            self.program.declare_global_var(variable).unwrap();
                        }

                        _ => panic!("Invalid top-level statement {statement:?}"),
                    }
                }
            };
        }

        Ok(self.program)
    }

    fn parse_func_or_var_decl(&mut self) -> Result<Option<Statement>, ParseError> {
        // This might be a lone type declaration, or it might be the start of a
        // function, or a variable...
        let initial_type = self.parse_type()?;

        if self.lexer.accept(TokenKind::Semicolon) {
            // Lone type definition.
            return Ok(None);
        }

        let (name, this_type) = self.parse_variable_name(initial_type)?;

        if self.lexer.next_is(TokenKind::OpenParen) {
            // Function declaration!
            let return_type = this_type;

            // Parse args.
            let args = self.parse_func_decl_args()?;

            // May or may not have function body.
            let body = if self.lexer.next_is(TokenKind::OpenCurly) {
                Some(Box::new(self.parse_block()?))
            } else {
                self.lexer.expect(TokenKind::Semicolon)?;
                None
            };

            let func = Function {
                args,
                return_type: Box::new(return_type),
                body,
            };

            self.program.declare_function(name, func);
            return Ok(None);
        }

        // Global variable decl (optionally with initial value!)
        let value = if self.lexer.accept(TokenKind::Assign) {
            Some(self.parse_expr(1)?)
        } else {
            None
        };

        Ok(Some(Statement::Declare(Variable {
            name,
            var_type: this_type,
            value,
        })))
    }

    fn parse_variable_name(&mut self, initial_type: Type) -> Result<(String, Type), ParseError> {
        let this_type = self.parse_type_pointersssss(initial_type)?;
        let name = self.lexer.consume_ident()?;

        Ok((name.to_string(), this_type))
    }

    fn parse_type_pointersssss(&mut self, initial_type: Type) -> Result<Type, ParseError> {
        let mut this_type = initial_type;

        // Evil right-associative pointer syntax >:(((
        while self.lexer.accept(TokenKind::Star) {
            this_type = TypeInfo::Pointer(Box::new(this_type)).into();
        }

        Ok(this_type)
    }

    fn parse_func_decl_args(&mut self) -> Result<Vec<Member>, ParseError> {
        let mut args: Vec<Member> = Vec::new();

        self.lexer.expect(TokenKind::OpenParen)?;

        while !self.lexer.accept(TokenKind::CloseParen) {
            let initial_type = self.parse_type()?;
            let arg_type = self.parse_type_pointersssss(initial_type)?;
            let arg_name = self.lexer.accept_ident();

            args.push(Member {
                name: arg_name,
                type_info: arg_type,
            });

            if self.lexer.accept(TokenKind::CloseParen) {
                break;
            }

            self.lexer.expect(TokenKind::Comma)?;
        }

        Ok(args)
    }

    fn parse_block(&mut self) -> Result<Statement, ParseError> {
        let mut statements: Vec<Statement> = Vec::new();

        self.lexer.expect(TokenKind::OpenCurly)?;
        self.consume_semicolons();

        loop {
            self.consume_semicolons();

            if self.lexer.accept(TokenKind::CloseCurly) {
                break;
            }

            statements.push(self.parse_statement()?);
        }

        Ok(Statement::Block(statements))
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.lexer.peek().kind {
            TokenKind::OpenCurly => self.parse_block(),

            TokenKind::If => {
                self.lexer.next();

                let condition = self.parse_expr_in_brackets()?;
                let if_true = self.parse_statement()?;

                if !self.lexer.accept(TokenKind::Else) {
                    return Ok(Statement::If {
                        condition: Box::new(condition),
                        if_true: Box::new(if_true),
                        if_false: None,
                    });
                };

                let if_false = self.parse_statement()?;

                Ok(Statement::If {
                    condition: Box::new(condition),
                    if_true: Box::new(if_true),
                    if_false: Some(Box::new(if_false)),
                })
            }

            TokenKind::While => {
                self.lexer.next();

                let condition = self.parse_expr_in_brackets()?;
                let block = self.parse_statement()?;

                Ok(Statement::While {
                    condition: Box::new(condition),
                    block: Box::new(block),
                })
            }

            TokenKind::Return => {
                self.lexer.next();

                let value = self.parse_expr(0)?;
                self.lexer.expect(TokenKind::Semicolon)?;

                Ok(Statement::Return(Box::new(value)))
            }

            _ => {
                // Whatever is left could either be a variable declaration, or could be a lone
                // expression. We'll cheat by spawning two clones of this parser and parsing both
                // possible interpretations, then picking the one that works.

                if self.clone().parse_variable_decl().is_ok() {
                    let decl = self.parse_variable_decl().unwrap();
                    self.lexer.expect(TokenKind::Semicolon)?;

                    return Ok(decl);
                }

                if self.clone().parse_expr(0).is_ok() {
                    let expr = self.parse_expr(0).unwrap();
                    self.lexer.expect(TokenKind::Semicolon)?;

                    return Ok(Statement::Expr(expr));
                }

                Err(self.unexpected_token())
            }
        }
    }

    fn parse_variable_decl(&mut self) -> Result<Statement, ParseError> {
        let initial_type = self.parse_type()?;

        // todo: support multiple decls on same line (ew)
        let (var_name, var_type) = self.parse_variable_name(initial_type)?;

        let value = if self.lexer.accept(TokenKind::Assign) {
            Some(self.parse_expr(1)?)
        } else {
            None
        };

        Ok(Statement::Declare(Variable {
            name: var_name,
            var_type,
            value,
        }))
    }

    fn parse_expr_in_brackets(&mut self) -> Result<Expr, ParseError> {
        self.lexer.expect(TokenKind::OpenParen)?;
        let expr = self.parse_expr(0)?;
        self.lexer.expect(TokenKind::CloseParen)?;

        Ok(expr)
    }

    fn parse_expr(&mut self, min_power: i32) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_unary_expr()?;

        while let Some(op) = self.lexer.maybe_map_next(|token| {
            match token {
                TokenKind::Comma => Some(BinaryOp::AndThen),
                TokenKind::Assign => Some(BinaryOp::Assign),
                _ => None,
            }
            .filter(|op| op.binding_power() >= min_power)
        }) {
            lhs = Expr::BinaryOp(
                op,
                Box::new(lhs),
                Box::new(self.parse_expr(op.binding_power())?),
            );
        }

        Ok(lhs)
    }

    fn parse_unary_expr(&mut self) -> Result<Expr, ParseError> {
        if self.lexer.next_is(TokenKind::OpenParen) {
            // rust stop asking me to collapse this pretty pls
            if self.clone().parse_cast_expr().is_ok() {
                return self.parse_cast_expr();
            }
        }

        let Some(op) = self.lexer.maybe_map_next(|token| match token {
            TokenKind::PlusPlus => Some(UnaryOp::GetThenIncrement),
            TokenKind::MinusMinus => Some(UnaryOp::GetThenDecrement),
            TokenKind::SizeOf => Some(UnaryOp::SizeOf),
            _ => None,
        }) else {
            return self.parse_postfix_expr();
        };

        let operand = self.parse_unary_expr()?;
        Ok(Expr::UnaryOp(op, Box::new(operand)))
    }

    fn parse_postfix_expr(&mut self) -> Result<Expr, ParseError> {
        let mut operand = self.parse_terminal_expr()?;

        while self.lexer.accept(TokenKind::OpenParen) {
            // Function call args!
            let mut args: Vec<Expr> = Vec::new();

            while !self.lexer.accept(TokenKind::CloseParen) {
                // Args are not allowed to use the comma operator, since that'd conflict with
                // comma being the function arg separator.
                args.push(self.parse_expr(BinaryOp::AndThen.binding_power() + 1)?);

                if !self.lexer.accept(TokenKind::Comma) {
                    self.lexer.expect(TokenKind::CloseParen)?;
                    break;
                }
            }

            operand = Expr::Call(Call {
                target: Box::new(operand),
                args,
            });
        }

        // FIXME: i think currently you can't go like, func()++() tho that seems like nonsense
        // anyway lol
        while let Some(op) = self.lexer.maybe_map_next(|token| match token {
            TokenKind::PlusPlus => Some(UnaryOp::GetThenIncrement),
            TokenKind::MinusMinus => Some(UnaryOp::GetThenDecrement),
            _ => None,
        }) {
            operand = Expr::UnaryOp(op, Box::new(operand));
        }

        Ok(operand)
    }

    fn parse_cast_expr(&mut self) -> Result<Expr, ParseError> {
        self.lexer.expect(TokenKind::OpenParen)?;
        let initial_type = self.parse_type()?;
        let target_type = self.parse_type_pointersssss(initial_type)?;
        self.lexer.expect(TokenKind::CloseParen)?;

        let expr = self.parse_expr(0)?;
        Ok(Expr::Cast(Box::new(expr), target_type))
    }

    fn parse_terminal_expr(&mut self) -> Result<Expr, ParseError> {
        match self.lexer.peek().kind {
            TokenKind::Ident(name) => {
                self.lexer.next();
                Ok(Expr::Reference(name.to_string()))
            }

            TokenKind::StringLiteral(content) => {
                self.lexer.next();
                Ok(Expr::StringLiteral(content.to_string()))
            }

            TokenKind::IntLiteral(int) => {
                self.lexer.next();
                Ok(Expr::IntLiteral(int))
            }

            TokenKind::OpenParen => self.parse_expr_in_brackets(),

            _ => Err(self.unexpected_token()),
        }
    }

    fn parse_typedef(&mut self) -> Result<(), ParseError> {
        self.lexer.expect(TokenKind::TypeDef)?;

        let initial_type = self.parse_type()?;
        let pointer_type = self.parse_type_pointersssss(initial_type)?;
        let name = self.lexer.consume_ident()?.to_string();

        let typedef = TypeDef {
            name,
            target_type: pointer_type,
        };

        self.program
            .typedef(typedef)
            .map_err(|err| self.bad_definition(Span::at(0), err))?;

        self.lexer.expect(TokenKind::Semicolon)?;
        Ok(())
    }

    fn parse_struct(&mut self) -> Result<Type, ParseError> {
        let start = self.lexer.expect(TokenKind::Struct)?;
        let struct_name = self.lexer.accept_ident();

        if !self.lexer.accept(TokenKind::OpenCurly) {
            let Some(struct_name) = struct_name else {
                return Err(self.bad_definition(
                    start.until(self.lexer.index),
                    ParseStructError::UnnamedReference,
                ));
            };

            // Opaque struct.
            return self
                .program
                .declare_named_struct(struct_name, Struct::opaque())
                .map(|id| id.into())
                .map_err(|err| self.bad_definition(start.until(self.lexer.index), err));
        }

        let mut builder = StructBuilder::new();

        while !self.lexer.accept(TokenKind::CloseCurly) {
            // Parse each member!
            let start_pos = self.lexer.index;

            let member_type = self.parse_type()?;
            let (member_name, member_type) = self.parse_variable_name(member_type)?;
            self.lexer.expect(TokenKind::Semicolon)?;

            let end_pos = self.lexer.index;

            builder
                .member(Some(member_name), member_type)
                .map_err(|err| {
                    self.bad_definition(Span::new(start_pos, end_pos), DefineTypeError::from(err))
                })?;
        }

        match struct_name {
            Some(name) => {
                let type_id = self
                    .program
                    .declare_named_struct(name, builder.build())
                    .map_err(|err| self.bad_definition(start.until(self.lexer.index), err))?;

                Ok(Type::WithId(type_id))
            }

            None => Ok(Type::Inline(builder.build().into())),
        }
    }

    /// Parse the type given for a symbol, i.e. a variable, struct member etc.
    ///
    /// C's pointer syntax is such that you can't know the full type until you then look at each
    /// variable (or what have you), as the "*" part of the type is right-associative *for some
    /// reason*.
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        match self.lexer.peek().kind {
            // Inline struct type.
            TokenKind::Struct => self.parse_struct(),

            // Typedef'd something-or-other!
            TokenKind::Ident(_) => self
                .program
                .typedef_get_id(&self.lexer.consume_ident()?)
                .map(|&id| Type::WithId(id))
                .ok_or_else(|| self.unexpected_token()),

            // Maybe we've got a number?
            _ => self
                .parse_numeric_type()
                .map(|numeric_type| numeric_type.into()),
        }
    }

    fn parse_numeric_type(&mut self) -> Result<BuiltInType, ParseError> {
        if self.lexer.accept(TokenKind::Unsigned) {
            return self
                .parse_terminal_numeric_type()?
                .unsigned()
                .ok_or_else(|| todo!("handle bad numeric unsigned"));
        }

        if self.lexer.accept(TokenKind::Signed) {
            return self
                .parse_terminal_numeric_type()?
                .signed()
                .ok_or_else(|| todo!("handle bad numeric signed"));
        }

        self.parse_terminal_numeric_type()
    }

    fn parse_terminal_numeric_type(&mut self) -> Result<BuiltInType, ParseError> {
        if let Some(basic_type) = self.lexer.maybe_map_next(|token| match token {
            TokenKind::Void => Some(BuiltInType::Void),
            TokenKind::Bool => Some(BuiltInType::Bool),
            TokenKind::Int => Some(BuiltInType::Int),
            TokenKind::Char => Some(BuiltInType::Char),
            TokenKind::Float => Some(BuiltInType::Float),
            TokenKind::Double => Some(BuiltInType::Double),
            _ => None,
        }) {
            return Ok(basic_type);
        }

        if self.lexer.accept(TokenKind::Short) {
            if self.lexer.accept(TokenKind::Int) {
                return Ok(BuiltInType::Short);
            }

            return Ok(BuiltInType::Short);
        }

        if self.lexer.accept(TokenKind::Long) {
            if self.lexer.accept(TokenKind::Long) {
                // long long long long long long long long int!!!!!!!!!
                // (optional "int" suffix here)
                self.lexer.accept(TokenKind::Int);

                return Ok(BuiltInType::LongLong);
            }

            if self.lexer.accept(TokenKind::Double) {
                return Ok(BuiltInType::LongDouble);
            }

            // again, optional "int".
            self.lexer.accept(TokenKind::Int);

            return Ok(BuiltInType::Long);
        }

        Err(self.unexpected_token())
    }

    fn consume_semicolons(&mut self) {
        while self.lexer.accept(TokenKind::Semicolon) {}
    }

    fn unexpected_eof(&self) -> ParseError {
        ParseError {
            span: Span::at(self.lexer.index),
            kind: ParseErrorKind::EarlyEof,
        }
    }

    fn unexpected_token(&mut self) -> ParseError {
        let Token { kind, span } = self.lexer.peek();

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

    #[error(transparent)]
    DefineTypeError(#[from] DefineTypeError),
}

impl<'a> From<LexerError> for ParseError {
    fn from(value: LexerError) -> Self {
        Self {
            span: value.span,
            kind: value.kind.into(),
        }
    }
}

impl<'a> From<LexerErrorKind> for ParseErrorKind {
    fn from(value: LexerErrorKind) -> Self {
        match value {
            LexerErrorKind::WrongToken { expected, actual } => {
                ParseErrorKind::WrongToken { expected, actual }
            }
            LexerErrorKind::EarlyEof => ParseErrorKind::EarlyEof,
        }
    }
}

#[derive(Debug, Error)]
pub enum ParseStructError {
    #[error("Reference to a struct without a name")]
    UnnamedReference,
}
