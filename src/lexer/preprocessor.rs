use std::ops::Not;

use crate::{
    context::IncludeType,
    lexer::{is_newline, is_valid_identifier, tokenkind::TokenKind, Lexer},
};

impl<'a> Lexer<'a> {
    pub(super) fn preprocessor_directive(&mut self) -> TokenKind {
        match self.take_chars_while(is_valid_identifier) {
            "pragma" => self.pragma_directive(),
            "include" => self.include_directive(),
            "ifdef" => self.ifdef_directive(true),
            "ifndef" => self.ifdef_directive(false),
            "else" | "elifdef" | "elifndef" => self.else_directive(),
            "endif" => self.endif_directive(),
            "define" => self.define_directive(),
            "undef" => self.undef_directive(),
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

        let Some((closer, include_type)) = self.maybe_map_next_char(|char| match char {
            '<' => Some(('>', IncludeType::System)),
            '"' => Some(('"', IncludeType::Local)),
            _ => None,
        }) else {
            todo!("Recover from syntax error in #include directive: missing path opener");
        };

        let path = self.take_chars_until(|char| char == closer);

        if !self.accept_char(closer) {
            todo!("Recover from syntax error in #include directive: missing closing {closer}");
        }

        let Ok(source_id) = self
            .context
            .add_source_file_path(path.to_owned(), include_type)
        else {
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

    fn ifdef_directive(&mut self, wants_defined: bool) -> TokenKind {
        self.skip_whitespace();
        let definition_name = self.take_chars_while(is_valid_identifier);

        if self.macro_is_defined(definition_name) == wants_defined {
            // The condition succeeded, we should lex the enclosed tokens.
            self.if_stack_depth += 1;
            return TokenKind::DiscardMarker;
        }

        self.skip_false_conditional()
    }

    fn else_directive(&mut self) -> TokenKind {
        // If we're seeing an `#else`, that means the current `#if` was true, so whatever we're
        // about to read should be discarded until we reach the `#endif`.

        if self.if_stack_depth == 0 {
            todo!("handle unbalanced #if/#endif stack")
        }

        self.skip_false_conditional()
    }

    fn endif_directive(&mut self) -> TokenKind {
        if self.if_stack_depth == 0 {
            todo!("handle unbalanced #if/#endif stack")
        }

        self.if_stack_depth -= 1;

        TokenKind::DiscardMarker
    }

    /// The preprocessor condition failed, let's speed through whatever comes next and find our
    /// relevant `#else` block (if it exists), otherwise our `#endif`.
    fn skip_false_conditional(&mut self) -> TokenKind {
        // FIXME: this is a very bad way of doing this because strings n stuff could cause
        // issues. lets pretend those don't exist for now :P
        let mut if_stack_depth = 1;

        while if_stack_depth > 0 {
            self.take_chars_until(|char| char == '#');

            if self.next_char().is_none() {
                // Reached EOF without closing, that's okay.
                break;
            }

            match self.take_chars_while(is_valid_identifier) {
                "if" | "ifdef" | "ifndef" => if_stack_depth += 1,

                "endif" => if_stack_depth -= 1,

                "elifdef" if if_stack_depth == 1 => return self.ifdef_directive(true),

                "elifndef" if if_stack_depth == 1 => return self.ifdef_directive(false),

                "else" if if_stack_depth == 1 => {
                    self.if_stack_depth += 1;
                    if_stack_depth -= 1;
                }

                _ => (),
            }

            self.take_chars_until(is_newline);
        }

        TokenKind::DiscardMarker
    }

    fn define_directive(&mut self) -> TokenKind {
        self.skip_whitespace();
        let definition_name = self.take_chars_while(is_valid_identifier);

        if self.accept_char('(') {
            todo!("Parameterised #define");
        }

        self.skip_whitespace();
        let mut content = vec![];

        while self.peek_char().is_some() && !self.accept_newline() {
            content.push(self.next());
        }

        self.context.define_macro(definition_name, content);

        TokenKind::DiscardMarker
    }

    fn undef_directive(&mut self) -> TokenKind {
        self.skip_whitespace();

        let name = self.take_chars_while(is_valid_identifier);
        self.context.remove_macro(name);

        TokenKind::DiscardMarker
    }

    fn error_directive(&mut self) -> TokenKind {
        self.skip_whitespace();
        let message = self.take_chars_until(is_newline);

        TokenKind::ErrorPreprocessorDirective(self.context.allocate_ident(message))
    }

    fn macro_is_defined(&self, name: &str) -> bool {
        self.context.get_macro(name).is_some()
    }
}
