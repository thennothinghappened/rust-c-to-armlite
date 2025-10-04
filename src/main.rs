use std::io::stdin;

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term::{
        self,
        termcolor::{ColorChoice, StandardStream},
    },
};

use crate::{
    lexer::{Lexer, Token, TokenInfo},
    parser::Parser,
};

mod lexer;
mod parser;
mod span;

fn main() {
    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = codespan_reporting::term::Config::default();

    let stdin = stdin();

    loop {
        let mut buf = String::new();
        stdin.read_line(&mut buf).unwrap();

        let lexer = Lexer::new(&buf);
        let parser = Parser::new(lexer);

        let program = match parser.parse() {
            Ok(program) => program,
            Err(err) => {
                let mut files = SimpleFiles::new();
                let file = files.add("main.c", &buf);

                let diagnostic = Diagnostic::error()
                    .with_label(Label::primary(file, err.location).with_message(err.kind));

                term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
                continue;
            }
        };

        println!("Program: {program}");
    }
}
