use thiserror::Error;

use crate::{
    lexer::{tokenkind::TokenKind, Lexer, LexerError, LexerErrorKind, Token},
    parser::program::{
        expr::{call::Call, BinaryOp, BindingPower, Expr, UnaryOp},
        statement::{Block, Statement, Variable},
        types::{CBuiltinType, CConcreteType, CFunc, CFuncType, CStruct, CType, Member, TypeDef},
        Program, StructBuilder,
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
            let return_ctype = this_type;

            // Parse args.
            let args = self.parse_func_decl_args()?;

            let mut func = CFunc {
                sig_id: self.program.func_type(CFuncType {
                    args,
                    returns: return_ctype,
                }),
                body: None,
            };

            // Pre-declare the function w/o body, so it is available for recursion.
            self.program
                .declare_function(name.clone(), func.clone())
                .unwrap();

            // May or may not have function body.
            if self.lexer.next_is(TokenKind::OpenCurly) {
                func.body = Some(self.parse_block()?);
                self.program.declare_function(name, func).unwrap();
            } else {
                self.lexer.expect(TokenKind::Semicolon)?;
            };

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
            ctype: this_type,
            value,
        })))
    }

    fn parse_variable_name(&mut self, initial_type: CType) -> Result<(String, CType), ParseError> {
        if self.lexer.accept(TokenKind::OpenParen) {
            let (name, mut this_type) = self.parse_variable_name(initial_type)?;

            if self.lexer.next_is(TokenKind::OpenParen) {
                // function pointer AHHHHHHHHHHHH
                let CType::PointerTo(return_ctype_id) = this_type else {
                    return self.error("Expecting a pointer when parsing a function type");
                };

                let args = self.parse_func_decl_args()?;

                this_type = CType::AsIs(CConcreteType::Func(self.program.func_type(CFuncType {
                    args,
                    returns: self.program.get_ctype(return_ctype_id),
                })));
            }

            self.lexer.expect(TokenKind::CloseParen)?;
            return Ok((name, this_type));
        }

        let mut this_type = self.parse_type_pointersssss(initial_type)?;
        let name = self.lexer.consume_ident()?;

        if self.lexer.accept(TokenKind::OpenSquare) {
            let element_count = self
                .lexer
                .maybe_map_next(|kind| match kind {
                    TokenKind::IntLiteral(element_count) => Some(element_count),
                    _ => None,
                })
                .ok_or_else(|| ParseError {
                    span: Span::at(self.lexer.index),
                    kind: ParseErrorKind::UnexpectedToken(self.lexer.next().kind),
                })?;

            self.lexer.expect(TokenKind::CloseSquare)?;

            this_type = CType::ArrayOf(
                self.program.ctype_id_of(this_type),
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
        while self.lexer.accept(TokenKind::Star) {
            this_type = CType::PointerTo(self.program.ctype_id_of(this_type)).into();
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
                ctype: arg_type,
            });

            if self.lexer.accept(TokenKind::CloseParen) {
                break;
            }

            self.lexer.expect(TokenKind::Comma)?;
        }

        Ok(args)
    }

    fn parse_block(&mut self) -> Result<Block, ParseError> {
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

        Ok(Block(statements))
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError> {
        match self.lexer.peek().kind {
            TokenKind::OpenCurly => Ok(self.parse_block()?.into()),

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

                let parse_as_variable_result = self.clone().parse_variable_decl();

                if parse_as_variable_result.is_ok() {
                    let decl = self.parse_variable_decl().unwrap();
                    self.lexer.expect(TokenKind::Semicolon)?;

                    return Ok(decl);
                }

                let parse_as_expr_result = self.clone().parse_expr(0);

                if parse_as_expr_result.is_ok() {
                    let expr = self.parse_expr(0).unwrap();
                    self.lexer.expect(TokenKind::Semicolon)?;

                    return Ok(Statement::Expr(expr));
                }

                self.error(format!(
                    "Not a variable decl: {}\nAnd not an expression: {}",
                    parse_as_variable_result.unwrap_err(),
                    parse_as_expr_result.unwrap_err()
                ))
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
            ctype: var_type,
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
                TokenKind::BooleanEqual => Some(BinaryOp::BooleanEqual),
                TokenKind::LessThan => Some(BinaryOp::LessThan),
                TokenKind::Plus => Some(BinaryOp::Plus),
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

        loop {
            if self.lexer.accept(TokenKind::OpenParen) {
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

                let sig_id = match &self.resolve_ctype(&operand)? {
                    CType::AsIs(CConcreteType::Func(id)) => *id,
                    ctype => return self.error(format!("{ctype:?} is not a function type")),
                };

                operand = Expr::Call(Call {
                    target: Box::new(operand),
                    sig_id,
                    args,
                });

                println!("{operand}");

                continue;
            }

            // FIXME: i think currently you can't go like, func()++() tho that seems like nonsense
            // anyway lol
            if let Some(op) = self.lexer.maybe_map_next(|token| match token {
                TokenKind::PlusPlus => Some(UnaryOp::GetThenIncrement),
                TokenKind::MinusMinus => Some(UnaryOp::GetThenDecrement),
                _ => None,
            }) {
                operand = Expr::UnaryOp(op, Box::new(operand));
                continue;
            }

            if self.lexer.accept(TokenKind::OpenSquare) {
                operand = Expr::BinaryOp(
                    BinaryOp::ArrayIndex,
                    Box::new(operand),
                    Box::new(self.parse_expr(0)?),
                );

                self.lexer.expect(TokenKind::CloseSquare)?;
                continue;
            }

            break;
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
            TokenKind::Ident(id) => {
                self.lexer.next();
                Ok(Expr::Reference(self.lexer.context.get_ident(id)))
            }

            TokenKind::StringLiteral(id) => {
                self.lexer.next();
                Ok(Expr::StringLiteral(self.lexer.context.get_ident(id)))
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
            ctype: pointer_type,
        };

        self.program
            .typedef(typedef)
            .map_err(|err| self.bad_definition(Span::at(0), err))?;

        self.lexer.expect(TokenKind::Semicolon)?;
        Ok(())
    }

    fn parse_struct(&mut self) -> Result<CType, ParseError> {
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
                .declare_named_struct(struct_name, CStruct::opaque())
                .map(|id| CType::AsIs(id.into()))
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
                    self.bad_definition(Span::new(start_pos, end_pos), anyhow::anyhow!(err))
                })?;
        }

        match struct_name {
            Some(name) => {
                let id = self
                    .program
                    .declare_named_struct(name, builder.build())
                    .map_err(|err| self.bad_definition(start.until(self.lexer.index), err))?;

                Ok(CType::AsIs(id.into()))
            }

            None => Ok(CType::AsIs(
                self.program
                    .declare_named_struct("".to_owned(), builder.build())
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
        match self.lexer.peek().kind {
            // Inline struct type.
            TokenKind::Struct => self.parse_struct(),

            // Typedef'd something-or-other!
            TokenKind::Ident(_) => self
                .program
                .resolve_typedef(&self.lexer.consume_ident()?)
                .ok_or_else(|| self.unexpected_token()),

            // Maybe we've got a number?
            _ => self
                .parse_numeric_type()
                .map(|numeric_type| CType::AsIs(numeric_type.into())),
        }
    }

    fn parse_numeric_type(&mut self) -> Result<CBuiltinType, ParseError> {
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

    fn parse_terminal_numeric_type(&mut self) -> Result<CBuiltinType, ParseError> {
        if let Some(basic_type) = self.lexer.maybe_map_next(|token| match token {
            TokenKind::Void => Some(CBuiltinType::Void),
            TokenKind::Bool => Some(CBuiltinType::Bool),
            TokenKind::Int => Some(CBuiltinType::Int),
            TokenKind::Char => Some(CBuiltinType::Char),
            TokenKind::Float => Some(CBuiltinType::Float),
            TokenKind::Double => Some(CBuiltinType::Double),
            _ => None,
        }) {
            return Ok(basic_type);
        }

        if self.lexer.accept(TokenKind::Short) {
            if self.lexer.accept(TokenKind::Int) {
                return Ok(CBuiltinType::Short);
            }

            return Ok(CBuiltinType::Short);
        }

        if self.lexer.accept(TokenKind::Long) {
            if self.lexer.accept(TokenKind::Long) {
                // long long long long long long long long int!!!!!!!!!
                // (optional "int" suffix here)
                self.lexer.accept(TokenKind::Int);

                return Ok(CBuiltinType::LongLong);
            }

            if self.lexer.accept(TokenKind::Double) {
                return Ok(CBuiltinType::LongDouble);
            }

            // again, optional "int".
            self.lexer.accept(TokenKind::Int);

            return Ok(CBuiltinType::Long);
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

    /// create a generic (ie, lazy) parser error right here, right now
    fn error<S: Into<String>, T>(&self, message: S) -> Result<T, ParseError> {
        Err(ParseError {
            span: self.lexer.index.into(),
            kind: ParseErrorKind::Lazy(message.into()),
        })
    }

    fn resolve_ctype(&self, expr: &Expr) -> Result<CType, ParseError> {
        match &expr {
            Expr::StringLiteral(_) => self.error("tried calling a string"),
            Expr::IntLiteral(_) => self.error("tried calling a number"),

            Expr::Reference(name) => {
                // aaaaaaaaaaaaaaaaaaaaaaaaaaaa help we need contexttttt

                // FIXME FIXME FIXME: pretending everything is global!!!!!!!!
                let Some(symbol) = self.program.get_symbol(name) else {
                    return self.error(format!("couldn't resolve reference to `{name}`"));
                };

                match symbol {
                    program::Symbol::Func(cfunc) => Ok(CType::AsIs(cfunc.sig_id.into())),
                    program::Symbol::Var(ctype, _) => Ok(*ctype),
                }
            }

            Expr::Call(call) => Ok(self.program.get_func_type(call.sig_id).returns),

            Expr::BinaryOp(op, left, right) => match op {
                BinaryOp::AndThen | BinaryOp::Assign => self.resolve_ctype(right),

                BinaryOp::BooleanEqual | BinaryOp::LessThan => {
                    Ok(CType::AsIs(CBuiltinType::Bool.into()))
                }

                BinaryOp::Plus => self.resolve_ctype(left),

                BinaryOp::ArrayIndex => match self.resolve_ctype(left)? {
                    CType::PointerTo(element_ctype) => Ok(self.program.get_ctype(element_ctype)),

                    CType::ArrayOf(element_ctype, _) => Ok(self.program.get_ctype(element_ctype)),

                    ctype => self.error(format!("cant index non-array/ptr type {ctype:#?}")),
                },
            },

            Expr::UnaryOp(op, expr) => todo!(),

            // fixme: blindly accepting whatever the programmer is saying here right now! we really
            // should define a stricter ast that simply doesn't allow invalid constructs, like
            // casting a struct to a number, for instance.
            Expr::Cast(_, ctype) => Ok(*ctype),
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

    /// lazy catch-all error because the alternative is panic or spending 1000000 years making error
    /// kinds when really this project needs a big ol' refactor!!
    #[error("{0}")]
    Lazy(String),
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

impl<'a> From<anyhow::Error> for ParseErrorKind {
    fn from(value: anyhow::Error) -> Self {
        Self::Lazy(value.to_string())
    }
}

#[derive(Debug, Error)]
pub enum ParseStructError {
    #[error("Reference to a struct without a name")]
    UnnamedReference,
}
