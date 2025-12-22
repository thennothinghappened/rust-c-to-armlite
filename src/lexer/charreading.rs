use crate::lexer::{tokenkind::TokenKind, Lexer};

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

    pub(super) fn take_chars(&mut self, count: usize) -> &'a str {
        let slice = &self.chars.as_str()[0..count];
        self.chars.nth(count - 1);

        slice
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

    pub(super) fn consume_c_escape(&mut self) -> Option<char> {
        let code = match self.peek_char()? {
            'a' => 0x07.into(),
            'b' => 0x08.into(),
            'e' => 0x1B.into(),
            'f' => 0x0C.into(),
            'n' => '\n',
            'r' => '\r',
            't' => '\t',
            'v' => 0x0B.into(),
            '0' => 0.into(),

            // TODO: Support numeric escape sequences.
            // 'x' => {
            //     self.next_char();
            //     let chars = self.take_chars_while(|char| char.is_ascii_hexdigit());
            //     let num = u8::from_str_radix(chars, 16).unwrap();

            //     return Some(char::from(num));
            // }

            // 'u' => {
            //     self.next_char();

            //     todo!()
            // }

            // first_octal if is_octal(first_octal) => {
            //     self.next_char();
            //     let octal_str = self.take_chars(3);

            //     u8
            //     todo!()
            // }
            plain => plain,
        };

        self.next_char();
        Some(code)
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

fn is_octal(char: char) -> bool {
    if let Ok(ascii_index) = u8::try_from(char) {
        if (b'0'..=b'7').contains(&ascii_index) {
            return true;
        }
    }

    false
}
