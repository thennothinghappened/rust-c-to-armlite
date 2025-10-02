use std::io::stdin;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

use crate::lexer::Lexer;

mod lexer;

fn main() {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let stdin = stdin();

    loop {
        let mut buf = String::new();
        stdin.read_line(&mut buf).unwrap();

        let mut lexer = Lexer::new(&buf);

        let mut files = SimpleFiles::new();
        let file = files.add("main.c", &buf);

        while let Some((token, pos)) = lexer.next() {
            let diagnostic = Diagnostic::note()
                .with_label(Label::primary(file, pos).with_message(format!("{token:?}")));

            term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
            // println!("Token {token:?} at {pos}");
        }
    }
}
