use std::{
    env::args,
    fs::{self, read, read_to_string},
    io::stdin,
    rc::Rc,
};

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
    term,
};

use crate::{context::Context, lexer::Lexer, parser::Parser};

#[macro_use]
mod id_type;

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

        let Some(output) = parse_program(buf, Some(path), &codespan_writer, &codespan_config)
        else {
            return;
        };

        if let Some(out_path) = args().nth(2) {
            fs::write(out_path, output).expect("Failed to write output asm file");
        }

        return;
    }

    let stdin = stdin();

    loop {
        loop {
            let mut buf = String::new();
            stdin.read_line(&mut buf).unwrap();

            if buf.trim_end().ends_with(";;") {
                break;
            }

            parse_program(buf, None, &codespan_writer, &codespan_config);
        }
    }
}

fn parse_program(
    text: String,
    file_name: Option<String>,
    codespan_writer: &term::termcolor::StandardStream,
    codespan_config: &term::Config,
) -> Option<String> {
    let context = Rc::new(Context::default());

    let source_id = match file_name {
        Some(path) => context.add_source_file_path(path).ok()?,
        None => context.add_source_text(text),
    };

    let lexer = Lexer::new(context.clone(), source_id);
    let parser = Parser::new(lexer);

    let program = match parser.parse() {
        Ok(program) => program,
        Err(err) => {
            let mut files = SimpleFiles::new();

            let offending_source_id = context.get_source_id_from_index(err.span.start);
            let sourcemap_offset = context.get_source_start_index(offending_source_id);

            let file = files.add(
                context.get_source_name(offending_source_id),
                context.get_source(offending_source_id),
            );

            let diagnostic = Diagnostic::error().with_label(
                Label::primary(file, err.span - sourcemap_offset).with_message(err.kind),
            );

            term::emit(
                &mut codespan_writer.lock(),
                codespan_config,
                &files,
                &diagnostic,
            )
            .unwrap();
            return None;
        }
    };

    println!("Program: {program}");

    let output = codegen::Generator::new(program).generate();
    println!("--- Code generated ---\n{output}");

    return Some(output);
}
