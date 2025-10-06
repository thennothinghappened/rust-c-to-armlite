use thiserror::Error;

use crate::{
    lexer::{Lexer, LexerError, LexerErrorKind, Token, TokenInfo},
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
    pub fn parse(mut self) -> Result<Program, ParseError<'a>> {
        while let Some((token, _)) = self.lexer.peek() {
            match token {
                Token::Semicolon => {
                    self.lexer.next();
                }

                Token::TypeDef => {
                    self.parse_typedef()?;
                }

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

    fn parse_func_or_var_decl(&mut self) -> Result<Option<Statement>, ParseError<'a>> {
        // This might be a lone type declaration, or it might be the start of a
        // function, or a variable...
        let initial_type = self.parse_type()?;

        if self.lexer.accept(Token::Semicolon) {
            // Lone type definition.
            return Ok(None);
        }

        let (name, this_type) = self.parse_variable_name(initial_type)?;

        if self.lexer.next_is(Token::OpenParen) {
            // Function declaration!
            let return_type = this_type;

            // Parse args.
            let args = self.parse_func_decl_args()?;

            // May or may not have function body.
            let body = if self.lexer.next_is(Token::OpenCurly) {
                Some(Box::new(self.parse_block()?))
            } else {
                self.lexer.expect(Token::Semicolon)?;
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
        let value = if self.lexer.accept(Token::Assign) {
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

    fn parse_variable_name(
        &mut self,
        initial_type: Type,
    ) -> Result<(String, Type), ParseError<'a>> {
        let this_type = self.parse_type_pointersssss(initial_type)?;
        let name = self.lexer.consume_ident()?;

        Ok((name.to_string(), this_type))
    }

    fn parse_type_pointersssss(&mut self, initial_type: Type) -> Result<Type, ParseError<'a>> {
        let mut this_type = initial_type;

        // Evil right-associative pointer syntax >:(((
        while self.lexer.accept(Token::Star) {
            this_type = TypeInfo::Pointer(Box::new(this_type)).into();
        }

        Ok(this_type)
    }

    fn parse_func_decl_args(&mut self) -> Result<Vec<Member>, ParseError<'a>> {
        let mut args: Vec<Member> = Vec::new();

        self.lexer.expect(Token::OpenParen)?;

        while !self.lexer.accept(Token::CloseParen) {
            let initial_type = self.parse_type()?;
            let arg_type = self.parse_type_pointersssss(initial_type)?;
            let arg_name = self.lexer.accept_ident();

            args.push(Member {
                name: arg_name.map(str::to_string),
                type_info: arg_type,
            });

            if self.lexer.accept(Token::CloseParen) {
                break;
            }

            self.lexer.expect(Token::Comma)?;
        }

        Ok(args)
    }

    fn parse_block(&mut self) -> Result<Statement, ParseError<'a>> {
        let mut statements: Vec<Statement> = Vec::new();

        self.lexer.expect(Token::OpenCurly)?;
        self.consume_semicolons();

        loop {
            self.consume_semicolons();

            if self.lexer.accept(Token::CloseCurly) {
                break;
            }

            statements.push(self.parse_statement()?);
        }

        Ok(Statement::Block(statements))
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError<'a>> {
        match self.peek_token()?.0 {
            Token::OpenCurly => self.parse_block(),

            Token::If => {
                self.lexer.next();

                let condition = self.parse_expr_in_brackets()?;
                let if_true = self.parse_statement()?;

                if !self.lexer.accept(Token::Else) {
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

            Token::While => {
                self.lexer.next();

                let condition = self.parse_expr_in_brackets()?;
                let block = self.parse_statement()?;

                Ok(Statement::While {
                    condition: Box::new(condition),
                    block: Box::new(block),
                })
            }

            Token::Return => {
                self.lexer.next();

                let value = self.parse_expr(0)?;
                self.lexer.expect(Token::Semicolon)?;

                Ok(Statement::Return(Box::new(value)))
            }

            _ => {
                // Whatever is left could either be a variable declaration, or could be a lone
                // expression. We'll cheat by spawning two clones of this parser and parsing both
                // possible interpretations, then picking the one that works.

                if self.clone().parse_variable_decl().is_ok() {
                    let decl = self.parse_variable_decl().unwrap();
                    self.lexer.expect(Token::Semicolon)?;

                    return Ok(decl);
                }

                if self.clone().parse_expr(0).is_ok() {
                    let expr = self.parse_expr(0).unwrap();
                    self.lexer.expect(Token::Semicolon)?;

                    return Ok(Statement::Expr(expr));
                }

                Err(self.unexpected_token())
            }
        }
    }

    fn parse_variable_decl(&mut self) -> Result<Statement, ParseError<'a>> {
        let initial_type = self.parse_type()?;

        // todo: support multiple decls on same line (ew)
        let (var_name, var_type) = self.parse_variable_name(initial_type)?;

        let value = if self.lexer.accept(Token::Assign) {
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

    fn parse_expr_in_brackets(&mut self) -> Result<Expr, ParseError<'a>> {
        self.lexer.expect(Token::OpenParen)?;
        let expr = self.parse_expr(0)?;
        self.lexer.expect(Token::CloseParen)?;

        Ok(expr)
    }

    fn parse_expr(&mut self, min_power: i32) -> Result<Expr, ParseError<'a>> {
        let mut lhs = self.parse_unary_expr()?;

        while let Some(op) = self.lexer.maybe_map_next(|token| {
            BinaryOp::try_from(token)
                .ok()
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

    fn parse_unary_expr(&mut self) -> Result<Expr, ParseError<'a>> {
        if self.lexer.next_is(Token::OpenParen) {
            // rust stop asking me to collapse this pretty pls
            if self.clone().parse_cast_expr().is_ok() {
                return self.parse_cast_expr();
            }
        }

        let Some(op) = self.lexer.maybe_map_next(|token| match token {
            Token::PlusPlus => Some(UnaryOp::GetThenIncrement),
            Token::MinusMinus => Some(UnaryOp::GetThenDecrement),
            Token::SizeOf => Some(UnaryOp::SizeOf),
            _ => None,
        }) else {
            return self.parse_postfix_expr();
        };

        let operand = self.parse_unary_expr()?;
        Ok(Expr::UnaryOp(op, Box::new(operand)))
    }

    fn parse_postfix_expr(&mut self) -> Result<Expr, ParseError<'a>> {
        let mut operand = self.parse_terminal_expr()?;

        while self.lexer.accept(Token::OpenParen) {
            // Function call args!
            let mut args: Vec<Expr> = Vec::new();

            while !self.lexer.accept(Token::CloseParen) {
                // Args are not allowed to use the comma operator, since that'd conflict with
                // comma being the function arg separator.
                args.push(self.parse_expr(BinaryOp::AndThen.binding_power() + 1)?);

                if !self.lexer.accept(Token::Comma) {
                    self.lexer.expect(Token::CloseParen)?;
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
            Token::PlusPlus => Some(UnaryOp::GetThenIncrement),
            Token::MinusMinus => Some(UnaryOp::GetThenDecrement),
            _ => None,
        }) {
            operand = Expr::UnaryOp(op, Box::new(operand));
        }

        Ok(operand)
    }

    fn parse_cast_expr(&mut self) -> Result<Expr, ParseError<'a>> {
        self.lexer.expect(Token::OpenParen)?;
        let initial_type = self.parse_type()?;
        let target_type = self.parse_type_pointersssss(initial_type)?;
        self.lexer.expect(Token::CloseParen)?;

        let expr = self.parse_expr(0)?;
        Ok(Expr::Cast(Box::new(expr), target_type))
    }

    fn parse_terminal_expr(&mut self) -> Result<Expr, ParseError<'a>> {
        match self.peek_token()?.0 {
            Token::Ident(name) => {
                self.lexer.next();
                Ok(Expr::Reference(name.to_string()))
            }

            Token::StringLiteral(content) => {
                self.lexer.next();
                Ok(Expr::StringLiteral(content.to_string()))
            }

            Token::IntLiteral(int) => {
                self.lexer.next();
                Ok(Expr::IntLiteral(int))
            }

            Token::OpenParen => self.parse_expr_in_brackets(),

            _ => Err(self.unexpected_token()),
        }
    }

    fn parse_typedef(&mut self) -> Result<(), ParseError<'a>> {
        self.lexer.expect(Token::TypeDef)?;

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

        self.lexer.expect(Token::Semicolon)?;
        Ok(())
    }

    fn parse_struct(&mut self) -> Result<Type, ParseError<'a>> {
        let start = self.lexer.expect(Token::Struct)?;
        let struct_name = self.lexer.accept_ident().map(str::to_string);

        if !self.lexer.accept(Token::OpenCurly) {
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

        while !self.lexer.accept(Token::CloseCurly) {
            // Parse each member!
            let start_pos = self.lexer.index;

            let member_type = self.parse_type()?;
            let (member_name, member_type) = self.parse_variable_name(member_type)?;
            self.lexer.expect(Token::Semicolon)?;

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
    fn parse_type(&mut self) -> Result<Type, ParseError<'a>> {
        match self.peek_token()?.0 {
            // Inline struct type.
            Token::Struct => self.parse_struct(),

            // Typedef'd something-or-other!
            Token::Ident(name) => {
                self.lexer.next();

                self.program
                    .typedef_get_id(name)
                    .map(|&id| Type::WithId(id))
                    .ok_or_else(|| self.unexpected_token())
            }

            // Maybe we've got a number?
            _ => self
                .parse_numeric_type()
                .map(|numeric_type| numeric_type.into()),
        }
    }

    fn parse_numeric_type(&mut self) -> Result<BuiltInType, ParseError<'a>> {
        if self.lexer.accept(Token::Unsigned) {
            return self
                .parse_terminal_numeric_type()?
                .unsigned()
                .ok_or_else(|| todo!("handle bad numeric unsigned"));
        }

        if self.lexer.accept(Token::Signed) {
            return self
                .parse_terminal_numeric_type()?
                .signed()
                .ok_or_else(|| todo!("handle bad numeric signed"));
        }

        self.parse_terminal_numeric_type()
    }

    fn parse_terminal_numeric_type(&mut self) -> Result<BuiltInType, ParseError<'a>> {
        if let Some(basic_type) = self.lexer.maybe_map_next(|token| match token {
            Token::Void => Some(BuiltInType::Void),
            Token::Bool => Some(BuiltInType::Bool),
            Token::Int => Some(BuiltInType::Int),
            Token::Char => Some(BuiltInType::Char),
            Token::Float => Some(BuiltInType::Float),
            Token::Double => Some(BuiltInType::Double),
            _ => None,
        }) {
            return Ok(basic_type);
        }

        if self.lexer.accept(Token::Short) {
            if self.lexer.accept(Token::Int) {
                return Ok(BuiltInType::Short);
            }

            return Ok(BuiltInType::Short);
        }

        if self.lexer.accept(Token::Long) {
            if self.lexer.accept(Token::Long) {
                // long long long long long long long long int!!!!!!!!!
                // (optional "int" suffix here)
                self.lexer.accept(Token::Int);

                return Ok(BuiltInType::LongLong);
            }

            if self.lexer.accept(Token::Double) {
                return Ok(BuiltInType::LongDouble);
            }

            // again, optional "int".
            self.lexer.accept(Token::Int);

            return Ok(BuiltInType::Long);
        }

        Err(self.unexpected_token())
    }

    fn next_token(&mut self) -> Result<TokenInfo<'a>, ParseError<'a>> {
        self.lexer.next().ok_or_else(|| self.unexpected_eof())
    }

    fn peek_token(&mut self) -> Result<TokenInfo<'a>, ParseError<'a>> {
        self.lexer.peek().ok_or_else(|| self.unexpected_eof())
    }

    fn consume_semicolons(&mut self) {
        while self.lexer.accept(Token::Semicolon) {}
    }

    fn unexpected_eof(&self) -> ParseError<'a> {
        ParseError {
            location: Span::at(self.lexer.index),
            kind: ParseErrorKind::EarlyEof,
        }
    }

    fn unexpected_token(&mut self) -> ParseError<'a> {
        let Some((token, location)) = self.lexer.peek() else {
            return self.unexpected_eof();
        };

        ParseError {
            location,
            kind: ParseErrorKind::UnexpectedToken(token),
        }
    }

    fn bad_definition<S: Into<Span>, E: Into<ParseErrorKind<'a>>>(
        &self,
        location: S,
        err: E,
    ) -> ParseError<'a> {
        ParseError {
            location: location.into(),
            kind: err.into(),
        }
    }
}

#[derive(Debug, Error)]
#[error("{kind} at {location}")]
pub struct ParseError<'a> {
    pub location: Span,
    pub kind: ParseErrorKind<'a>,
}

#[derive(Debug, Error)]
pub enum ParseErrorKind<'a> {
    #[error("Unexpected token {0}")]
    UnexpectedToken(Token<'a>),

    #[error("Unexpected token {actual}, expected {expected}")]
    WrongToken {
        expected: Token<'static>,
        actual: Token<'a>,
    },

    #[error("Unexpected End Of File")]
    EarlyEof,

    #[error(transparent)]
    ParseStructError(#[from] ParseStructError),

    #[error(transparent)]
    DefineTypeError(#[from] DefineTypeError),
}

impl<'a> From<LexerError<'a>> for ParseError<'a> {
    fn from(value: LexerError<'a>) -> Self {
        Self {
            location: value.location,
            kind: value.kind.into(),
        }
    }
}

impl<'a> From<LexerErrorKind<'a>> for ParseErrorKind<'a> {
    fn from(value: LexerErrorKind<'a>) -> Self {
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
