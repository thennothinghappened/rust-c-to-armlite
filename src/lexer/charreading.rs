use crate::lexer::Lexer;

impl<'a> Lexer<'a> {
    pub(super) fn take_chars_while<F>(&mut self, predicate: F) -> &'a str
    where
        F: Fn(char) -> bool,
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
}
