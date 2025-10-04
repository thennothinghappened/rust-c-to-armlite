use std::{collections::HashMap, fmt::Display, ops::Range, str::Chars};

use thiserror::Error;

use crate::{
    lexer::{self, Lexer, LexerError, LexerErrorKind, Token, TokenInfo},
    parser::program::{
        statement::Statement,
        types::{BuiltInType, Type, TypeDef, TypeInfo},
        DefineTypeError, Program, StructBuilder, TypeId,
    },
    span::Span,
};
pub mod program;

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
        while let Some((token, location)) = self.lexer.peek() {
            match token {
                Token::Struct => {
                    self.parse_struct()?;
                }

                Token::TypeDef => {
                    self.parse_typedef()?;
                }

                _ => {
                    // Only valid options now are function or global var decl. Both start with a
                    // type.
                    let type_of_next_thing = self.parse_type()?;

                    todo!()
                }
            };
        }

        Ok(self.program)
    }

    fn parse_statement(&mut self) -> Result<Statement, ParseError<'a>> {
        // if let Token::Ident(ident) = self.lexer.peek()? {
        //     if let Some(type_info) = self.parse_type() {
        //         // This is either a global variable or a function. Keep reading!
        //         let Some(TokenInfo {
        //             token: Token::Ident(global_name),
        //             span: _,
        //         }) = self.lexer.next()
        //         else {
        //             return Some(Err(ParseError { span: (), kind: () }));
        //         };

        //         if self.lexer.accept(Token::OpenParen).is_some() {
        //             // Function declaration!
        //             // let args =
        //         } else if self.lexer.accept(Token::Assign).is_some() {
        //             // Variable assignment!
        //         }
        //     }
        // }

        todo!()
    }

    fn parse_typedef(&mut self) -> Result<(), ParseError<'a>> {
        self.lexer.expect(Token::TypeDef)?;

        let target_type = self.parse_type()?;
        let name = self.lexer.consume_ident()?.to_string();

        let typedef = TypeDef { name, target_type };

        self.program
            .typedef(typedef)
            .map_err(|err| self.bad_definition(Span::at(0), err))?;

        self.lexer.expect(Token::Semicolon)?;
        Ok(())
    }

    fn parse_struct(&mut self) -> Result<Type, ParseError<'a>> {
        let start = self.lexer.expect(Token::Struct)?;

        let struct_name = self.lexer.maybe_map_next(|token| match token {
            Token::Ident(name) => Some(name.to_string()),
            _ => None,
        });

        self.lexer.expect(Token::OpenCurly)?;
        let mut builder = StructBuilder::new();

        while !self.lexer.accept(Token::CloseCurly) {
            // Parse each member!
            let member_type = self.parse_type()?;
            let member_name = self.lexer.consume_ident()?;
            self.lexer.expect(Token::Semicolon)?;

            builder
                .member(member_name.to_string(), member_type)
                .map_err(|err| self.bad_definition(todo!(), err.into()))?;
        }

        let type_id = self
            .program
            .define_struct(struct_name, builder.build())
            .map_err(|err| self.bad_definition(start.until(self.lexer.index), err))?;

        Ok(type_id.into())
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

    fn bad_definition(&self, span: Span, err: DefineTypeError) -> ParseError<'a> {
        ParseError {
            location: span,
            kind: ParseErrorKind::DefineTypeError(err),
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
    DefineTypeError(DefineTypeError),
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

impl<'a> From<DefineTypeError> for ParseErrorKind<'a> {
    fn from(value: DefineTypeError) -> Self {
        Self::DefineTypeError(value)
    }
}
