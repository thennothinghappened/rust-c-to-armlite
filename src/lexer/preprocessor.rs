use std::ops::Not;

use crate::lexer::{is_newline, is_valid_identifier, tokenkind::TokenKind, Lexer};

impl<'a> Lexer<'a> {
    pub(super) fn preprocessor_directive(&mut self) -> TokenKind {
        match self.take_chars_while(is_valid_identifier) {
            "pragma" => self.pragma_directive(),
            "include" => self.include_directive(),
            "ifndef" => self.if_directive(true),
            "ifdef" => self.if_directive(false),
            "endif" => self.endif_directive(),
            "define" => self.define_directive(),
            "error" => self.error_directive(),

            directive => {
                // Eat the rest of the line so we can continue.
                self.take_chars_until(is_newline);

                TokenKind::UnknownPreprocessorDirective(self.context.allocate_ident(directive))
            }
        }
    }

    fn pragma_directive(&mut self) -> TokenKind {
        self.skip_whitespace();

        let content = self.take_chars_until(is_newline).trim();

        match content {
            "once" => self.context.source_enable_pragma_once(self.source_id),
            _ => println!("Unrecognised #pragma `{content}`"),
        };

        TokenKind::DiscardMarker
    }

    fn include_directive(&mut self) -> TokenKind {
        // We'll make a temporary lil lexer to deal with the new file, then brutally
        // murder it once it does its job (drop)
        self.skip_whitespace();

        if !self.accept_char('"') {
            todo!("Recover from syntax error in #include directive: missing opening \"");
        }

        let path = self.take_chars_until(|char| char == '"' || is_newline(char));

        if !self.accept_char('"') {
            todo!("Recover from syntax error in #include directive: missing closing \"");
        }

        let Ok(source_id) = self.context.add_source_file_path(path.to_owned()) else {
            return TokenKind::IncludeFileNotFound(self.context.allocate_ident(path));
        };

        if self.context.allow_reading(source_id).not() {
            // Skip this include - we've encountered it before and it specifies #pragma once.
            return TokenKind::DiscardMarker;
        }

        let mut temp_lexer = Lexer::new(self.context.clone(), source_id);

        loop {
            let token = temp_lexer.next();

            if token.kind == TokenKind::Eof {
                break;
            }

            self.token_buffer_stream.push_back(token);
        }

        TokenKind::DiscardMarker
    }

    fn if_directive(&mut self, is_if_not_def: bool) -> TokenKind {
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

        TokenKind::DiscardMarker
    }

    fn endif_directive(&mut self) -> TokenKind {
        if self.if_stack_depth == 0 {
            todo!("handle unbalanced #if/#endif stack")
        }

        self.if_stack_depth -= 1;

        TokenKind::DiscardMarker
    }

    fn define_directive(&mut self) -> TokenKind {
        self.skip_whitespace();
        let definition_name = self.take_chars_while(is_valid_identifier);
        self.skip_whitespace();

        if !self.accept_newline() {
            todo!("#define with value!");
        }

        self.context.preproc_define(definition_name, "");

        TokenKind::DiscardMarker
    }

    fn error_directive(&mut self) -> TokenKind {
        self.skip_whitespace();
        let message = self.take_chars_until(is_newline);

        TokenKind::ErrorPreprocessorDirective(self.context.allocate_ident(message))
    }
}
