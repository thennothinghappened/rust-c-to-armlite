use std::{fmt::Display, ops::Range, str::Chars};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Token<'a> {
    Whitespace(&'a str),
    Semicolon,
    OpenParen,
    CloseParen,
    OpenCurly,
    CloseCurly,
    Ampersand,
    Star,
    Assign,
    BooleanEqual,
    StringLiteral(&'a str),
    Ident(&'a str),
    Unknown(char),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Span {
    start: usize,
    end: usize,
}

pub(crate) type TokenInfo<'a> = (Token<'a>, Range<usize>);

#[derive(Clone)]
pub(crate) struct Lexer<T> {
    chars: T,
    index: usize,
}

impl<'a> Lexer<Chars<'a>> {
    pub fn new(text: &'a str) -> Self {
        Self {
            chars: text.chars(),
            index: 0,
        }
    }

    pub fn next(&mut self) -> Option<TokenInfo<'a>> {
        let start = self.index;

        let token: Token<'a> = 'get_token: {
            if let Some(whitespace) = self.take_chars_while(|char| char.is_ascii_whitespace()) {
                break 'get_token Token::Whitespace(whitespace);
            }

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

            if self.accept('=') {
                break 'get_token if self.accept('=') {
                    Token::BooleanEqual
                } else {
                    Token::Assign
                };
            }

            if self.accept('"') {
                let content = self.take_chars_while(|char| char != '"');
                self.consume('"');

                break 'get_token Token::StringLiteral(content.unwrap_or(""));
            }

            if char == '_' || char.is_alphabetic() {
                let str = self
                    .take_chars_while(|char| char == '_' || char.is_alphanumeric())
                    .unwrap();

                break 'get_token Token::Ident(str);
            }

            Token::Unknown(self.next_char()?)
        };

        Some((token, start..self.index))
    }

    pub fn peek(&mut self) -> Option<TokenInfo<'a>> {
        self.clone().next()
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

    fn accept(&mut self, expected: char) -> bool {
        self.next_char_if(|char| char == expected)
    }

    fn consume(&mut self, expected: char) {
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
}
