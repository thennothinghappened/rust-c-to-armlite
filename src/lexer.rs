use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Display,
    fs,
    ops::Not,
    rc::Rc,
    str::Chars,
};

use phf::phf_map;
use thiserror::Error;

use crate::{
    context::{Context, SourceId},
    lexer::{
        charreading::{is_newline, is_valid_identifier, is_whitespace},
        tokenkind::{IdentId, TokenKind},
    },
    span::Span,
};

pub mod charreading;
mod preprocessor;
pub mod tokenkind;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(token: TokenKind, span: Span) -> Self {
        Self { kind: token, span }
    }
}

#[derive(Clone)]
pub struct Lexer<'a> {
    source_id: SourceId,
    pub index: usize,
    chars: Chars<'a>,
    token_buffer_stream: VecDeque<Token>,
    pub context: Rc<Context<'a>>,
    if_stack_depth: usize,
}

const KEYWORD_MAP: phf::Map<&'static str, TokenKind> = phf_map!(
    "if" => TokenKind::If,
    "else" => TokenKind::Else,
    "while" => TokenKind::While,
    "for" => TokenKind::For,
    "return" => TokenKind::Return,
    "break" => TokenKind::Break,
    "continue" => TokenKind::Continue,
    "struct" => TokenKind::Struct,
    "typedef" => TokenKind::TypeDef,
    "union" => TokenKind::Union,
    "enum" => TokenKind::Enum,
    "unsigned" => TokenKind::Unsigned,
    "signed" => TokenKind::Signed,
    "bool" => TokenKind::Bool,
    "true" => TokenKind::BoolLiteral(true),
    "false" => TokenKind::BoolLiteral(false),
    "int" => TokenKind::Int,
    "long" => TokenKind::Long,
    "short" => TokenKind::Short,
    "char" => TokenKind::Char,
    "float" => TokenKind::Float,
    "double" => TokenKind::Double,
    "void" => TokenKind::Void,
    "sizeof" => TokenKind::SizeOf,
    "const" => TokenKind::Const,
    "extern" => TokenKind::Extern,
    "__asm__" => TokenKind::Asm,
    "nullptr" => TokenKind::NullPtr,
);

const SINGLE_CHARACTER_TOKEN_MAP: phf::Map<char, TokenKind> = phf_map!(
    '{' => TokenKind::OpenCurly,
    '}' => TokenKind::CloseCurly,
    '(' => TokenKind::OpenParen,
    ')' => TokenKind::CloseParen,
    ';' => TokenKind::Semicolon,
    '*' => TokenKind::Star,
    ',' => TokenKind::Comma,
    '<' => TokenKind::LessThan,
    '>' => TokenKind::GreaterThan,
    '?' => TokenKind::QuestionMark,
);

impl<'a> Lexer<'a> {
    pub fn new(context: Rc<Context<'a>>, source_id: SourceId) -> Self {
        Self {
            source_id,
            chars: context.get_source(source_id).chars(),
            index: context.get_source_start_index(source_id),
            token_buffer_stream: Default::default(),
            context,
            if_stack_depth: 0,
        }
    }

    pub fn next(&mut self) -> Token {
        loop {
            if let Some(peeked) = self.token_buffer_stream.pop_front() {
                return peeked;
            }

            self.skip_whitespace_and_newlines();

            let start = self.index;
            let kind = self.lex_next();
            let end = self.index;

            if kind == TokenKind::DiscardMarker {
                continue;
            }

            return Token::new(kind, Span::new(start, end));
        }
    }

    pub fn peek(&mut self) -> Token {
        if let Some(peeked) = self.token_buffer_stream.front() {
            return *peeked;
        }

        let peeked = self.next();
        self.token_buffer_stream.push_front(peeked);

        peeked
    }

    fn lex_next(&mut self) -> TokenKind {
        let Some(char) = self.peek_char() else {
            return TokenKind::Eof;
        };

        if let Some(&basic_token) =
            self.maybe_map_next_char(|char| SINGLE_CHARACTER_TOKEN_MAP.get(&char))
        {
            return basic_token;
        }

        if self.accept_char('!') {
            return if self.accept_char('=') {
                TokenKind::BooleanNotEqual
            } else {
                TokenKind::BooleanNot
            };
        }

        if self.accept_char('&') {
            return if self.accept_char('&') {
                TokenKind::BooleanAnd
            } else {
                TokenKind::Ampersand
            };
        }

        if self.accept_char('|') {
            return if self.accept_char('|') {
                TokenKind::BooleanOr
            } else {
                TokenKind::BitwiseOr
            };
        }

        if self.accept_char('[') {
            return if self.accept_char('[') {
                TokenKind::OpenAttributeList
            } else {
                TokenKind::OpenSquare
            };
        }

        if self.accept_char(']') {
            return if self.accept_char(']') {
                TokenKind::CloseAttributeList
            } else {
                TokenKind::CloseSquare
            };
        }

        if self.accept_char(':') {
            return if self.accept_char(':') {
                TokenKind::NamespaceAccessor
            } else {
                TokenKind::Colon
            };
        }

        if self.accept_char('/') {
            if self.accept_char('/') {
                self.take_chars_until(is_newline);
                return TokenKind::DiscardMarker;
            }

            if self.accept_char('*') {
                return loop {
                    self.take_chars_until(|char| char == '*');
                    self.next_char();

                    match self.next_char() {
                        Some('/') => break TokenKind::DiscardMarker,
                        None => break TokenKind::DiscardMarker,
                        _ => continue,
                    }
                };
            }

            return TokenKind::Divide;
        }

        if self.accept_char('#') {
            return self.preprocessor_directive();
        }

        if self.accept_char('+') {
            if let Some(kind) = self.maybe_map_next_char(|char| match char {
                '+' => Some(TokenKind::PlusPlus),
                '=' => Some(TokenKind::PlusAssign),
                _ => None,
            }) {
                return kind;
            }

            return TokenKind::Plus;
        }

        if self.accept_char('-') {
            if let Some(kind) = self.maybe_map_next_char(|char| match char {
                '-' => Some(TokenKind::MinusMinus),
                '=' => Some(TokenKind::MinusAssign),
                _ => None,
            }) {
                return kind;
            }

            return TokenKind::Minus;
        }

        if self.accept_char('=') {
            return if self.accept_char('=') {
                TokenKind::BooleanEqual
            } else {
                TokenKind::Assign
            };
        }

        if self.peek_char() == Some('"') {
            let mut string = String::new();

            while self.accept_char('"') {
                let mut escape_next = false;

                string += self.take_chars_while(|char| {
                    if escape_next {
                        escape_next = false;
                        return true;
                    }

                    if char == '\\' {
                        escape_next = true;
                        return true;
                    }

                    char != '"'
                });

                self.consume_char('"');
                self.skip_whitespace_and_newlines();
            }

            return TokenKind::StringLiteral(self.context.allocate_ident(string));
        }

        if self.accept_char('\'') {
            let Some(char) = (if self.accept_char('\\') {
                self.consume_c_escape()
            } else {
                self.next_char()
            }) else {
                return TokenKind::Eof;
            };

            self.consume_char('\'');

            // NOTE: Questionable goings on here.
            let mut buf: [u8; 4] = [0, 0, 0, 0];
            char.encode_utf8(&mut buf);

            return TokenKind::IntLiteral(i32::from_le_bytes(buf));
        }

        if char.is_numeric() {
            // TODO: support 0x, 0b, etc.
            let int = self.take_chars_while(char::is_numeric).parse().unwrap();
            return TokenKind::IntLiteral(int);
        }

        if char == '_' || char.is_alphabetic() {
            let str = self.take_chars_while(is_valid_identifier);

            if let Some(&token) = KEYWORD_MAP.get(str) {
                return token;
            }

            if let Some(tokens) = self.context.get_macro(str) {
                for &token in tokens.iter() {
                    self.token_buffer_stream.push_back(token);
                }

                return TokenKind::DiscardMarker;
            }

            return TokenKind::Ident(self.context.allocate_ident(str));
        }

        self.next_char();
        TokenKind::Unknown(char)
    }

    fn maybe_map_next_char<F, R>(&mut self, map: F) -> Option<R>
    where
        F: FnOnce(char) -> Option<R>,
    {
        let out = map(self.peek_char()?)?;
        self.next_char();

        Some(out)
    }
}
