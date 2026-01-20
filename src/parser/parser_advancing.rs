use anyhow::bail;

use crate::{
    lexer::{tokenkind::TokenKind, Token},
    parser::{ParseError, ParseErrorKind, Parser},
    span::Span,
};

impl<'a> Parser<'a> {
    pub(super) fn next(&mut self) -> Token {
        self.lexer.next()
    }

    pub(super) fn peek(&mut self) -> Token {
        self.lexer.peek()
    }

    pub(super) fn next_if<F>(&mut self, predicate: F) -> Option<Token>
    where
        F: FnOnce(TokenKind) -> bool,
    {
        let token = self.peek();

        if predicate(token.kind) {
            return Some(self.next());
        }

        None
    }

    pub(super) fn maybe_map_next<F, R>(&mut self, map: F) -> Option<R>
    where
        F: FnOnce(TokenKind) -> Option<R>,
    {
        let out = map(self.peek().kind);

        if out.is_some() {
            self.next();
        }

        out
    }

    pub(super) fn accept(&mut self, expected: TokenKind) -> bool {
        self.next_if(|token| token == expected).is_some()
    }

    pub(super) fn next_is(&mut self, expected: TokenKind) -> bool {
        self.lexer.peek().kind == expected
    }

    pub(super) fn accept_ident(&mut self) -> Option<String> {
        if let TokenKind::Ident(id) = self.lexer.peek().kind {
            self.lexer.next();
            return Some(self.lexer.context.get_ident(id));
        }

        None
    }

    pub(super) fn accept_string(&mut self) -> Option<String> {
        if let TokenKind::StringLiteral(id) = self.lexer.peek().kind {
            self.lexer.next();
            return Some(self.lexer.context.get_ident(id));
        }

        None
    }

    pub(super) fn consume(&mut self) -> Result<Token, ParseError> {
        let token = self.lexer.next();

        if token.kind == TokenKind::Eof {
            return Err(ParseError {
                span: token.span,
                kind: ParseErrorKind::EarlyEof,
            });
        }

        Ok(token)
    }

    pub(super) fn expect(&mut self, expected: TokenKind) -> Result<Span, ParseError> {
        let token = self.consume()?;

        if token.kind == expected {
            return Ok(token.span);
        }

        Err(ParseError {
            span: token.span,
            kind: ParseErrorKind::WrongToken {
                expected,
                actual: token.kind,
            },
        })
    }

    pub(super) fn consume_ident(&mut self) -> Result<String, ParseError> {
        let Some(ident) = self.accept_ident() else {
            let actual = self.peek();

            return Err(ParseError {
                span: actual.span,
                kind: ParseErrorKind::WrongToken {
                    expected: TokenKind::Ident(0),
                    actual: actual.kind,
                },
            });
        };

        Ok(ident)
    }

    pub(super) fn consume_string(&mut self) -> Result<String, ParseError> {
        let Some(ident) = self.accept_string() else {
            let actual = self.peek();

            return Err(ParseError {
                span: actual.span,
                kind: ParseErrorKind::WrongToken {
                    expected: TokenKind::StringLiteral(0),
                    actual: actual.kind,
                },
            });
        };

        Ok(ident)
    }
}
