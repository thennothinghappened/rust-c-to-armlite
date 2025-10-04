use std::{fmt::Display, ops::Range, str::Chars};

use thiserror::Error;

use crate::span::Span;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Token<'a> {
    Semicolon,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Ampersand,
    Star,
    Assign,
    BooleanEqual,
    If,
    Else,
    While,
    Return,
    Struct,
    Union,
    TypeDef,
    Enum,
    Const,
    Signed,
    Unsigned,
    Short,
    Int,
    Long,
    Char,
    Float,
    Double,
    Void,
    StringLiteral(&'a str),
    Ident(&'a str),
    Unknown(char),
}

impl<'a> TryFrom<Token<'a>> for &'static str {
    type Error = ();

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        Ok(match value {
            Token::Semicolon => ";",
            Token::OpenParen => "(",
            Token::CloseParen => ")",
            Token::OpenCurly => "{",
            Token::CloseCurly => "}",
            Token::Ampersand => "&",
            Token::Star => "*",
            Token::Assign => "=",
            Token::BooleanEqual => "==",
            Token::If => "if",
            Token::Else => "else",
            Token::While => "while",
            Token::Return => "return",
            Token::Struct => "struct",
            Token::Union => "union",
            Token::TypeDef => "typedef",
            Token::Enum => "enum",
            Token::Unsigned => "unsigned",
            Token::Signed => "signed",
            Token::Int => "int",
            Token::Long => "long",
            Token::Short => "short",
            Token::Char => "char",
            Token::Float => "float",
            Token::Double => "double",
            _ => return Err(()),
        })
    }
}

impl<'a> Display for Token<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Ok(str) = <&'static str>::try_from(*self) {
            return write!(f, "{str}");
        }

        match self {
            Token::StringLiteral(content) => write!(f, "\"{content}\""),
            Token::Ident(name) => write!(f, "Ident({name})"),
            Token::Unknown(char) => write!(f, "Unknown({char})"),
            _ => panic!("unhandled case {self:?}, somebody forgot to add it"),
        }
    }
}

pub(crate) type TokenInfo<'a> = (Token<'a>, Span);

#[derive(Clone)]
pub(crate) struct Lexer<'a> {
    pub index: usize,
    chars: Chars<'a>,
    maybe_peeked: Option<TokenInfo<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self {
            chars: text.chars(),
            index: 0,
            maybe_peeked: None,
        }
    }

    pub fn next(&mut self) -> Option<TokenInfo<'a>> {
        if let Some(peeked) = self.maybe_peeked {
            self.maybe_peeked = None;
            return Some(peeked);
        }

        loop {
            // Discard whitespace.
            let Some(_) = self.take_chars_while(|char| char.is_ascii_whitespace()) else {
                break;
            };
        }

        let start = self.index;

        let token: Token<'a> = 'get_token: {
            let char = self.peek_char()?;

            let basic_token = match char {
                '{' => Some(Token::OpenCurly),
                '}' => Some(Token::CloseCurly),
                '(' => Some(Token::OpenParen),
                ')' => Some(Token::CloseParen),
                ';' => Some(Token::Semicolon),
                '&' => Some(Token::Ampersand),
                '*' => Some(Token::Star),
                _ => None,
            };

            if let Some(basic_token) = basic_token {
                self.next_char();
                break 'get_token basic_token;
            }

            if self.accept_char('=') {
                break 'get_token if self.accept_char('=') {
                    Token::BooleanEqual
                } else {
                    Token::Assign
                };
            }

            if self.accept_char('"') {
                let content = self.take_chars_while(|char| char != '"');
                self.consume_char('"');

                break 'get_token Token::StringLiteral(content.unwrap_or(""));
            }

            if char == '_' || char.is_alphabetic() {
                let str = self
                    .take_chars_while(|char| char == '_' || char.is_alphanumeric())
                    .unwrap();

                if let Some(token) = match str {
                    "if" => Some(Token::If),
                    "else" => Some(Token::Else),
                    "while" => Some(Token::While),
                    "return" => Some(Token::Return),
                    "struct" => Some(Token::Struct),
                    "typedef" => Some(Token::TypeDef),
                    "union" => Some(Token::Union),
                    "enum" => Some(Token::Enum),
                    "unsigned" => Some(Token::Unsigned),
                    "signed" => Some(Token::Signed),
                    "int" => Some(Token::Int),
                    "long" => Some(Token::Long),
                    "short" => Some(Token::Short),
                    "char" => Some(Token::Char),
                    "float" => Some(Token::Float),
                    "double" => Some(Token::Double),
                    _ => None,
                } {
                    break 'get_token token;
                }

                break 'get_token Token::Ident(str);
            }

            Token::Unknown(self.next_char()?)
        };

        Some((token, Span::new(start, self.index)))
    }

    pub fn peek(&mut self) -> Option<TokenInfo<'a>> {
        if let Some(peeked) = self.maybe_peeked {
            return Some(peeked);
        }

        let peeked = self.next()?;

        self.maybe_peeked = Some(peeked);
        Some(peeked)
    }

    pub fn next_if<F>(&mut self, predicate: F) -> Option<TokenInfo<'a>>
    where
        F: FnOnce(Token<'a>) -> bool,
    {
        let (token, _) = self.peek()?;

        if predicate(token) {
            return self.next();
        }

        None
    }

    pub fn maybe_map_next<F, R>(&mut self, map: F) -> Option<R>
    where
        F: FnOnce(Token<'a>) -> Option<R>,
    {
        let out = self.peek().and_then(|(token, _)| map(token))?;
        self.next();

        Some(out)
    }

    pub fn accept(&mut self, expected: Token<'a>) -> bool {
        self.next_if(|token| token == expected).is_some()
    }

    pub fn accept_ident(&mut self) -> Option<&'a str> {
        self.maybe_map_next(|token| match token {
            Token::Ident(name) => Some(name),
            _ => None,
        })
    }

    pub fn consume(&mut self) -> Result<TokenInfo<'a>, LexerError<'a>> {
        self.next()
            .ok_or_else(|| self.err_here(LexerErrorKind::EarlyEof))
    }

    pub fn expect(&mut self, expected: Token<'static>) -> Result<Span, LexerError<'a>> {
        let (token, location) = self.consume()?;

        if token == expected {
            return Ok(location);
        }

        Err(LexerError {
            location,
            kind: LexerErrorKind::WrongToken {
                expected,
                actual: token,
            },
        })
    }

    pub fn consume_ident(&mut self) -> Result<&'a str, LexerError<'a>> {
        let (token, location) = self
            .next()
            .ok_or_else(|| self.err_here(LexerErrorKind::EarlyEof))?;

        if let Token::Ident(name) = token {
            return Ok(name);
        }

        Err(LexerError::wrong_token(
            Token::Ident("identifier"),
            (token, location),
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

    fn take_chars_while<F>(&mut self, predicate: F) -> Option<&'a str>
    where
        F: Fn(char) -> bool,
    {
        let num = self
            .chars
            .clone()
            .take_while(|&char| predicate(char))
            .count();

        if num == 0 {
            return None;
        }

        let slice = &self.chars.as_str()[0..num];

        self.chars.nth(num - 1);
        self.index += num;

        Some(slice)
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
            location: self
                .maybe_peeked
                .map(|(_, location)| location)
                .unwrap_or(Span::at(self.index)),
            kind: err,
        }
    }
}

#[derive(Debug, Error)]
#[error("{kind} at {location}")]
pub struct LexerError<'a> {
    pub location: Span,
    pub kind: LexerErrorKind<'a>,
}

#[derive(Debug, Error)]
pub enum LexerErrorKind<'a> {
    #[error("Incorrect token {actual}, expected {expected}")]
    WrongToken {
        expected: Token<'static>,
        actual: Token<'a>,
    },

    #[error("Unexpected End Of File")]
    EarlyEof,
}

impl<'a> LexerError<'a> {
    pub(super) fn wrong_token(expected: Token<'static>, (actual, location): TokenInfo<'a>) -> Self {
        Self {
            location,
            kind: LexerErrorKind::WrongToken { expected, actual },
        }
    }
}
