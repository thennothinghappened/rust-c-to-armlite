use std::{
    env::args,
    fs::{read, read_to_string},
    io::stdin,
};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term,
};

use crate::{lexer::Lexer, parser::Parser};

mod codegen;
mod context;
mod lexer;
mod parser;
mod span;

fn main() {
    let codespan_writer =
        term::termcolor::StandardStream::stderr(term::termcolor::ColorChoice::Always);

    let codespan_config = codespan_reporting::term::Config::default();

    if let Some(path) = args().nth(1) {
        let buf = read_to_string(&path).expect("Must pass a valid path to read from");
        return parse_program(&buf, &path, &codespan_writer, &codespan_config);
    }

    let stdin = stdin();

    loop {
        let mut buf = String::new();

        loop {
            stdin.read_line(&mut buf).unwrap();

            if buf.trim_end().ends_with(";;") {
                break;
            }

            parse_program(&buf, "stdin", &codespan_writer, &codespan_config);
        }
    }
}

fn parse_program(
    buf: &str,
    file_name: &str,
    codespan_writer: &term::termcolor::StandardStream,
    codespan_config: &term::Config,
) {
    let lexer = Lexer::new(buf);
    let parser = Parser::new(lexer);

    let program = match parser.parse() {
        Ok(program) => program,
        Err(err) => {
            let mut files = SimpleFiles::new();
            let file = files.add(file_name, &buf);

            let diagnostic = Diagnostic::error()
                .with_label(Label::primary(file, err.span).with_message(err.kind));

            term::emit(
                &mut codespan_writer.lock(),
                codespan_config,
                &files,
                &diagnostic,
            )
            .unwrap();
            return;
        }
    };

    println!("Program: {program}");

    let output = codegen::Generator::new(program).generate();
    println!("--- Code generated ---\n{output}");
}
