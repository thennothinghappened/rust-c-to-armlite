use crate::lexer::Lexer;

impl<'a> Lexer<'a> {
    pub(super) fn take_chars_while<F>(&mut self, mut predicate: F) -> &'a str
    where
        F: FnMut(char) -> bool,
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

    pub(super) fn take_chars_until<F>(&mut self, predicate: F) -> &'a str
    where
        F: Fn(char) -> bool,
    {
        self.take_chars_while(|char| !predicate(char))
    }

    pub(super) fn accept_char(&mut self, expected: char) -> bool {
        self.next_char_if(|char| char == expected)
    }

    pub(super) fn consume_char(&mut self, expected: char) {
        assert_eq!(self.next_char(), Some(expected));
    }

    pub(super) fn next_char(&mut self) -> Option<char> {
        let char = self.chars.next()?;
        self.index += 1;

        Some(char)
    }

    pub(super) fn next_char_if<F>(&mut self, predicate: F) -> bool
    where
        F: FnOnce(char) -> bool,
    {
        if self.peek_char().filter(|&char| predicate(char)).is_some() {
            self.next_char();
            return true;
        }

        false
    }

    pub(super) fn peek_char(&self) -> Option<char> {
        self.chars.clone().next()
    }

    /// Skip valid whitespace characters. Newlines are **NOT** considered whitespace.
    pub(super) fn skip_whitespace(&mut self) {
        self.take_chars_while(is_whitespace);
    }

    /// Skip valid whitespace characters and line breaks.
    pub(super) fn skip_whitespace_and_newlines(&mut self) {
        self.take_chars_while(is_whitespace_or_newline);
    }

    /// Optionally accept a new line character. The forms `\n`, `\r\n`, and `\r` are accepted.
    pub(super) fn accept_newline(&mut self) -> bool {
        if self.accept_char('\n') {
            return true;
        }

        if self.accept_char('\r') {
            self.accept_char('\n');
            return true;
        }

        false
    }
}

pub(super) fn is_valid_identifier(char: char) -> bool {
    char == '_' || char.is_alphanumeric()
}

pub(super) fn is_whitespace(char: char) -> bool {
    char.is_whitespace() && !is_newline(char)
}

pub(super) fn is_whitespace_or_newline(char: char) -> bool {
    char.is_whitespace() || is_newline(char)
}

pub(super) fn is_newline(char: char) -> bool {
    char == '\n' || char == '\r'
}
