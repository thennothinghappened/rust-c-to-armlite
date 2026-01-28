use std::{
    collections::VecDeque,
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

use crate::{
    codegen::arm::AsmMode,
    context::{Context, IncludeType},
    lexer::Lexer,
    parser::Parser,
};

#[macro_use]
mod id_type;

mod codegen;
mod context;
mod lexer;
mod parser;
mod span;

fn main() {
    let mut asm_mode = AsmMode::default();

    let codespan_config = codespan_reporting::term::Config::default();
    let codespan_writer =
        term::termcolor::StandardStream::stderr(term::termcolor::ColorChoice::Always);

    let (options, mut args): (VecDeque<String>, VecDeque<String>) =
        args().skip(1).partition(|arg| arg.starts_with("--"));

    for option in options {
        match &option[2..] {
            "armv7" => {
                asm_mode = AsmMode::ArmV7;
            }
            _ => panic!("Unknown option {option}"),
        }
    }

    if let Some(path) = args.pop_front() {
        let buf = read_to_string(&path).expect("Must pass a valid path to read from");

        let Some(output) = parse_program(
            buf,
            asm_mode,
            Some(path),
            &codespan_writer,
            &codespan_config,
        ) else {
            return;
        };

        if let Some(out_path) = args.pop_front() {
            fs::write(out_path, output).expect("Failed to write output asm file");
        } else {
            println!("--- Code generated ---\n{output}");
        }

        return;
    }

    let stdin = stdin();

    loop {
        let mut buf = String::new();

        loop {
            stdin.read_line(&mut buf).unwrap();

            if buf.trim_end().ends_with(";;") {
                break;
            }
        }

        if let Some(output) = parse_program(buf, asm_mode, None, &codespan_writer, &codespan_config)
        {
            println!("--- Code generated ---\n{output}");
        }
    }
}

fn parse_program(
    text: String,
    asm_mode: AsmMode,
    file_name: Option<String>,
    codespan_writer: &term::termcolor::StandardStream,
    codespan_config: &term::Config,
) -> Option<String> {
    let context = Rc::new(Context::new(asm_mode));

    let source_id = match file_name {
        Some(path) => context
            .add_source_file_path(path, IncludeType::Local)
            .ok()?,
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

    Some(codegen::Generator::new(program, asm_mode).generate())
}
