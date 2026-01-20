use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt::Display,
    fs,
    ops::Not,
    rc::Rc,
    str::Chars,
};

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

        if let Some(basic_token) = self.maybe_map_next_char(|char| match char {
            '{' => Some(TokenKind::OpenCurly),
            '}' => Some(TokenKind::CloseCurly),
            '(' => Some(TokenKind::OpenParen),
            ')' => Some(TokenKind::CloseParen),
            '[' => Some(TokenKind::OpenSquare),
            ']' => Some(TokenKind::CloseSquare),
            ';' => Some(TokenKind::Semicolon),
            '*' => Some(TokenKind::Star),
            ',' => Some(TokenKind::Comma),
            '<' => Some(TokenKind::LessThan),
            '>' => Some(TokenKind::GreaterThan),
            '?' => Some(TokenKind::QuestionMark),
            ':' => Some(TokenKind::Colon),
            '!' => Some(TokenKind::BooleanNot),
            _ => None,
        }) {
            return basic_token;
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
            // Preprocessor directive!
            let directive = self.take_chars_while(is_valid_identifier);

            match directive {
                "pragma" => {
                    self.skip_whitespace();

                    let content = self.take_chars_until(is_newline).trim();

                    match content {
                        "once" => self.context.source_enable_pragma_once(self.source_id),
                        _ => println!("Unrecognised #pragma `{content}`"),
                    }
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

                    let source_id = match self.context.add_source_file_path(path.to_owned()) {
                        Ok(id) => id,
                        Err(_) => {
                            return TokenKind::IncludeFileNotFound(
                                self.context.allocate_ident(path),
                            )
                        }
                    };

                    if self.context.allow_reading(source_id) {
                        let mut temp_lexer = Lexer::new(self.context.clone(), source_id);

                        loop {
                            let token = temp_lexer.next();

                            if token.kind == TokenKind::Eof {
                                break;
                            }

                            self.token_buffer_stream.push_back(token);
                        }
                    }
                }

                "ifndef" => self.ifdef(true),
                "ifdef" => self.ifdef(false),

                "endif" => {
                    if self.if_stack_depth == 0 {
                        todo!("handle unbalanced #if/#endif stack")
                    }

                    self.if_stack_depth -= 1;
                }

                "define" => {
                    self.skip_whitespace();
                    let definition_name = self.take_chars_while(is_valid_identifier);
                    self.skip_whitespace();

                    if !self.accept_newline() {
                        todo!("#define with value!");
                    }

                    self.context.preproc_define(definition_name, "");
                }

                "error" => {
                    self.skip_whitespace();
                    let message = self.take_chars_until(is_newline);

                    return TokenKind::ErrorPreprocessorDirective(
                        self.context.allocate_ident(message),
                    );
                }

                directive => {
                    // Eat the rest of the line so we can continue.
                    self.take_chars_until(is_newline);

                    return TokenKind::UnknownPreprocessorDirective(
                        self.context.allocate_ident(directive),
                    );
                }
            }

            return TokenKind::DiscardMarker;
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

            if let Some(token) = match str {
                "if" => Some(TokenKind::If),
                "else" => Some(TokenKind::Else),
                "while" => Some(TokenKind::While),
                "return" => Some(TokenKind::Return),
                "break" => Some(TokenKind::Break),
                "continue" => Some(TokenKind::Continue),
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
                "extern" => Some(TokenKind::Extern),
                "__asm__" => Some(TokenKind::Asm),
                _ => None,
            } {
                return token;
            }

            return TokenKind::Ident(self.context.allocate_ident(str));
        }

        self.next_char();
        TokenKind::Unknown(char)
    }

    fn ifdef(&mut self, is_if_not_def: bool) {
        self.skip_whitespace();
        let definition_name = self.take_chars_while(is_valid_identifier);

        if self.context.preproc_get(definition_name).is_none() == is_if_not_def {
            self.if_stack_depth += 1;
        } else {
            // FIXME: this is a very bad way of doing this because strings n stuff could cause
            // issues. lets pretend those don't exist for now :P
            let mut if_stack_depth = 1;

            while if_stack_depth > 0 {
                self.take_chars_until(|char| char == '#');

                if self.next_char().is_none() {
                    break;
                }

                let directive = self.take_chars_while(is_valid_identifier);

                match directive {
                    "if" | "ifdef" | "ifndef" => {
                        if_stack_depth += 1;
                    }

                    "endif" => {
                        if_stack_depth -= 1;
                    }

                    _ => (),
                }

                self.take_chars_until(is_newline);
            }
        }
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
