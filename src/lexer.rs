use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Display,
    fs,
    rc::Rc,
    str::Chars,
};

use thiserror::Error;

use crate::{
    context::Context,
    lexer::{
        charreading::{is_newline, is_valid_identifier, is_whitespace},
        tokenkind::{IdentId, TokenKind},
    },
    span::Span,
};

pub mod charreading;
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
    pub index: usize,
    chars: Chars<'a>,
    token_buffer_stream: VecDeque<Token>,
    context: Rc<Context>,
}

impl<'a> Lexer<'a> {
    pub fn new(text: &'a str) -> Self {
        Self::new_with_context(Context::default().into(), text)
    }

    fn new_with_context(context: Rc<Context>, text: &'a str) -> Self {
        Self {
            chars: text.chars(),
            index: 0,
            token_buffer_stream: Default::default(),
            context,
        }
    }

    pub fn next(&mut self) -> Token {
        if let Some(peeked) = self.token_buffer_stream.pop_front() {
            return peeked;
        }

        // Discard whitespace.
        self.take_chars_while(|char| char.is_ascii_whitespace());

        let start = self.index;
        let kind = self.lex_next();

        Token::new(kind, Span::new(start, self.index))
    }

    pub fn peek(&mut self) -> Token {
        if let Some(peeked) = self.token_buffer_stream.front() {
            return *peeked;
        }

        let peeked = self.next();

        if peeked.kind == TokenKind::MacroExpansionMarker {
            return self.peek();
        }

        self.token_buffer_stream.push_back(peeked);
        peeked
    }

    pub fn next_if<F>(&mut self, predicate: F) -> Option<Token>
    where
        F: FnOnce(TokenKind) -> bool,
    {
        let token = self.peek();

        if predicate(token.kind) {
            return Some(self.next());
        }

        None
    }

    pub fn maybe_map_next<F, R>(&mut self, map: F) -> Option<R>
    where
        F: FnOnce(TokenKind) -> Option<R>,
    {
        let out = map(self.peek().kind);

        if out.is_some() {
            self.next();
        }

        out
    }

    pub fn accept(&mut self, expected: TokenKind) -> bool {
        self.next_if(|token| token == expected).is_some()
    }

    pub fn next_is(&mut self, expected: TokenKind) -> bool {
        self.peek().kind == expected
    }

    pub fn accept_ident(&mut self) -> Option<String> {
        if let TokenKind::Ident(id) = self.peek().kind {
            self.next();
            return Some(self.context.get_ident(id));
        }

        None
    }

    pub fn consume(&mut self) -> Result<Token, LexerError> {
        let token = self.next();

        if token.kind == TokenKind::Eof {
            return Err(self.err_here(LexerErrorKind::EarlyEof));
        }

        Ok(token)
    }

    pub fn expect(&mut self, expected: TokenKind) -> Result<Span, LexerError> {
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

    pub fn consume_ident(&mut self) -> Result<String, LexerError> {
        let Some(ident) = self.accept_ident() else {
            let actual = self.peek().kind;

            return Err(self.err_here(LexerErrorKind::WrongToken {
                expected: TokenKind::Ident(0),
                actual,
            }));
        };

        Ok(ident)
    }

    fn lex_next(&mut self) -> TokenKind {
        let Some(char) = self.peek_char() else {
            return TokenKind::Eof;
        };

        if let Some(basic_token) = self.maybe_map_next_char(|char| match char {
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
            _ => None,
        }) {
            return basic_token;
        }

        if self.accept_char('#') {
            // Preprocessor directive!
            let directive = self.take_chars_while(is_valid_identifier);

            match directive {
                "pragma" => {
                    self.skip_whitespace();
                    todo!("pragma directive")
                }

                "include" => {
                    // We'll make a temporary lil lexer to deal with the new file, then brutally
                    // murder it once it does its job (drop)
                    self.skip_whitespace();

                    if !self.accept_char('"') {
                        todo!(
                            "Recover from syntax error in #include directive: missing opening \""
                        );
                    }

                    let path = self.take_chars_until(|char| char == '"' || is_newline(char));

                    if !self.accept_char('"') {
                        todo!(
                            "Recover from syntax error in #include directive: missing closing \""
                        );
                    }

                    let src = self.context.load_file(path);

                    let mut temp_lexer =
                        Lexer::new_with_context(self.context.clone(), src.as_str());

                    loop {
                        let token = temp_lexer.next();

                        if token.kind == TokenKind::Eof {
                            break;
                        }

                        self.token_buffer_stream.push_back(token);
                    }

                    return TokenKind::MacroExpansionMarker;
                }

                _ => panic!("Unknown directive `#{directive}`"),
            }
        }

        if self.accept_char('+') {
            if self.accept_char('+') {
                return TokenKind::PlusPlus;
            }

            return TokenKind::Plus;
        }

        if self.accept_char('-') {
            if self.accept_char('-') {
                return TokenKind::MinusMinus;
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

        if self.accept_char('"') {
            let content = self.take_chars_while(|char| char != '"');
            self.consume_char('"');

            return TokenKind::StringLiteral(self.context.allocate_ident(content));
        }

        if char.is_numeric() {
            // TODO: support 0x, 0b, etc.
            let int = self.take_chars_while(char::is_numeric).parse().unwrap();
            return TokenKind::IntLiteral(int);
        }

        if char == '_' || char.is_alphabetic() {
            let str = self.take_chars_while(is_valid_identifier);

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
                return token;
            }

            return TokenKind::Ident(self.context.allocate_ident(str));
        }

        self.next_char();
        TokenKind::Unknown(char)
    }

    pub fn maybe_map_next_char<F, R>(&mut self, map: F) -> Option<R>
    where
        F: FnOnce(char) -> Option<R>,
    {
        let out = map(self.peek_char()?)?;
        self.next_char();

        Some(out)
    }

    fn err_here(&self, err: LexerErrorKind) -> LexerError {
        LexerError {
            span: self
                .token_buffer_stream
                .front()
                .map(|tk| tk.span)
                .unwrap_or(Span::at(self.index)),
            kind: err,
        }
    }
}

#[derive(Debug, Error)]
#[error("{kind} at {span}")]
pub struct LexerError {
    pub span: Span,
    pub kind: LexerErrorKind,
}

#[derive(Debug, Error)]
pub enum LexerErrorKind {
    #[error("Incorrect token {actual}, expected {expected}")]
    WrongToken {
        expected: TokenKind,
        actual: TokenKind,
    },

    #[error("Unexpected End Of File")]
    EarlyEof,
}

impl LexerError {
    pub(super) fn wrong_token(expected: TokenKind, actual: Token) -> Self {
        Self {
            span: actual.span,
            kind: LexerErrorKind::WrongToken {
                expected,
                actual: actual.kind,
            },
        }
    }
}
