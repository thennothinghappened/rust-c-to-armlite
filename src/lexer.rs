use std::{fmt::Display, str::Chars};

use thiserror::Error;

use crate::{lexer::tokenkind::TokenKind, span::Span};

pub mod tokenkind;

#[derive(Clone, Copy, PartialEq)]
pub(crate) struct Token<'a> {
    pub kind: TokenKind<'a>,
    pub span: Span,
}

impl<'a> Token<'a> {
    pub fn new(token: TokenKind<'a>, span: Span) -> Self {
        Self { kind: token, span }
    }
}

#[derive(Clone)]
pub(crate) struct Lexer<'a> {
    pub index: usize,
    chars: Chars<'a>,
    maybe_peeked: Option<Token<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            chars: text.chars(),
            index: 0,
            maybe_peeked: None,
        }
    }

    pub fn next(&mut self) -> Token<'a> {
        if let Some(peeked) = self.maybe_peeked {
            self.maybe_peeked = None;
            return peeked;
        }

        // Discard whitespace.
        self.take_chars_while(|char| char.is_ascii_whitespace());

        let start = self.index;

        let token: TokenKind<'a> = 'get_token: {
            let Some(char) = self.peek_char() else {
                break 'get_token TokenKind::Eof;
            };

            let basic_token = match char {
                '{' => Some(TokenKind::OpenCurly),
                '}' => Some(TokenKind::CloseCurly),
                '(' => Some(TokenKind::OpenParen),
                ')' => Some(TokenKind::CloseParen),
                ';' => Some(TokenKind::Semicolon),
                '&' => Some(TokenKind::Ampersand),
                '*' => Some(TokenKind::Star),
                ',' => Some(TokenKind::Comma),
                '?' => Some(TokenKind::QuestionMark),
                ':' => Some(TokenKind::Colon),

                '+' => {
                    self.next_char();

                    if self.accept_char('+') {
                        break 'get_token TokenKind::PlusPlus;
                    }

                    break 'get_token TokenKind::Plus;
                }

                '-' => {
                    self.next_char();

                    if self.accept_char('-') {
                        break 'get_token TokenKind::MinusMinus;
                    }

                    break 'get_token TokenKind::Minus;
                }

                _ => None,
            };

            if let Some(basic_token) = basic_token {
                self.next_char();
                break 'get_token basic_token;
            }

            if self.accept_char('=') {
                break 'get_token if self.accept_char('=') {
                    TokenKind::BooleanEqual
                } else {
                    TokenKind::Assign
                };
            }

            if self.accept_char('"') {
                let content = self.take_chars_while(|char| char != '"');
                self.consume_char('"');

                break 'get_token TokenKind::StringLiteral(content);
            }

            if char.is_numeric() {
                // TODO: support 0x, 0b, etc.
                let int = self.take_chars_while(char::is_numeric).parse().unwrap();
                break 'get_token TokenKind::IntLiteral(int);
            }

            if char == '_' || char.is_alphabetic() {
                let str = self.take_chars_while(|char| char == '_' || char.is_alphanumeric());

                if let Some(token) = match str {
                    "if" => Some(TokenKind::If),
                    "else" => Some(TokenKind::Else),
                    "while" => Some(TokenKind::While),
                    "return" => Some(TokenKind::Return),
                    "struct" => Some(TokenKind::Struct),
                    "typedef" => Some(TokenKind::TypeDef),
                    "union" => Some(TokenKind::Union),
                    "enum" => Some(TokenKind::Enum),
                    "unsigned" => Some(TokenKind::Unsigned),
                    "signed" => Some(TokenKind::Signed),
                    "bool" => Some(TokenKind::Bool),
                    "int" => Some(TokenKind::Int),
                    "long" => Some(TokenKind::Long),
                    "short" => Some(TokenKind::Short),
                    "char" => Some(TokenKind::Char),
                    "float" => Some(TokenKind::Float),
                    "double" => Some(TokenKind::Double),
                    "void" => Some(TokenKind::Void),
                    "sizeof" => Some(TokenKind::SizeOf),
                    "const" => Some(TokenKind::Const),
                    _ => None,
                } {
                    break 'get_token token;
                }

                break 'get_token TokenKind::Ident(str);
            }

            self.next_char();
            TokenKind::Unknown(char)
        };

        Token::new(token, Span::new(start, self.index))
    }

    pub fn peek(&mut self) -> Token<'a> {
        if let Some(peeked) = self.maybe_peeked {
            return peeked;
        }

        let peeked = self.next();
        self.maybe_peeked = Some(peeked);

        peeked
    }

    pub fn next_if<F>(&mut self, predicate: F) -> Option<Token<'a>>
    where
        F: FnOnce(TokenKind<'a>) -> bool,
    {
        let token = self.peek();

        if predicate(token.kind) {
            return Some(self.next());
        }

        None
    }

    pub fn maybe_map_next<F, R>(&mut self, map: F) -> Option<R>
    where
        F: FnOnce(TokenKind<'a>) -> Option<R>,
    {
        let out = map(self.peek().kind);

        if out.is_some() {
            self.next();
        }

        out
    }

    pub fn accept(&mut self, expected: TokenKind<'a>) -> bool {
        self.next_if(|token| token == expected).is_some()
    }

    pub fn next_is(&mut self, expected: TokenKind<'a>) -> bool {
        self.peek().kind == expected
    }

    pub fn accept_ident(&mut self) -> Option<&'a str> {
        self.maybe_map_next(|token| match token {
            TokenKind::Ident(name) => Some(name),
            _ => None,
        })
    }

    pub fn consume(&mut self) -> Result<Token<'a>, LexerError<'a>> {
        let token = self.next();

        if token.kind == TokenKind::Eof {
            return Err(self.err_here(LexerErrorKind::EarlyEof));
        }

        Ok(token)
    }

    pub fn expect(&mut self, expected: TokenKind<'static>) -> Result<Span, LexerError<'a>> {
        let token = self.consume()?;

        if token.kind == expected {
            return Ok(token.span);
        }

        Err(LexerError {
            span: token.span,
            kind: LexerErrorKind::WrongToken {
                expected,
                actual: token.kind,
            },
        })
    }

    pub fn consume_ident(&mut self) -> Result<&'a str, LexerError<'a>> {
        let tkinfo = self.consume()?;

        if let TokenKind::Ident(name) = tkinfo.kind {
            return Ok(name);
        }

        Err(LexerError::wrong_token(
            TokenKind::Ident("identifier"),
            tkinfo,
        ))
    }

    fn next_char_if<F>(&mut self, predicate: F) -> bool
    where
        F: FnOnce(char) -> bool,
    {
        if self.peek_char().filter(|&char| predicate(char)).is_some() {
            self.next_char();
            return true;
        }

        false
    }

    fn take_chars_while<F>(&mut self, predicate: F) -> &'a str
    where
        F: Fn(char) -> bool,
    {
        let num = self
            .chars
            .clone()
            .take_while(|&char| predicate(char))
            .count();

        if num == 0 {
            return "";
        }

        let slice = &self.chars.as_str()[0..num];

        self.chars.nth(num - 1);
        self.index += num;

        slice
    }

    fn accept_char(&mut self, expected: char) -> bool {
        self.next_char_if(|char| char == expected)
    }

    fn consume_char(&mut self, expected: char) {
        assert_eq!(self.next_char(), Some(expected));
    }

    fn next_char(&mut self) -> Option<char> {
        let char = self.chars.next()?;
        self.index += 1;

        Some(char)
    }

    fn peek_char(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn err_here(&self, err: LexerErrorKind<'a>) -> LexerError<'a> {
        LexerError {
            span: self
                .maybe_peeked
                .map(|tk| tk.span)
                .unwrap_or(Span::at(self.index)),
            kind: err,
        }
    }
}

#[derive(Debug, Error)]
#[error("{kind} at {span}")]
pub struct LexerError<'a> {
    pub span: Span,
    pub kind: LexerErrorKind<'a>,
}

#[derive(Debug, Error)]
pub enum LexerErrorKind<'a> {
    #[error("Incorrect token {actual}, expected {expected}")]
    WrongToken {
        expected: TokenKind<'static>,
        actual: TokenKind<'a>,
    },

    #[error("Unexpected End Of File")]
    EarlyEof,
}

impl<'a> LexerError<'a> {
    pub(super) fn wrong_token(expected: TokenKind<'static>, actual: Token<'a>) -> Self {
        Self {
            span: actual.span,
            kind: LexerErrorKind::WrongToken {
                expected,
                actual: actual.kind,
            },
        }
    }
}
