use std::{io::stdin, path::PathBuf};

use clap::Parser;

use crate::tokenizer::tokenize;

mod ast;
mod tokenizer;
mod vm;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    file: Option<PathBuf>,
}

fn main() {
    println!("Hello, world!");

    let args = Args::parse();
    dbg!(&args);

    if let Some(_file) = args.file {
    } else {
        let mut vm = vm::VM::default();
        let mut buf = String::new();
        while let Ok(_result) = stdin().read_line(&mut buf) {
            let tokens = match tokenize(&buf.trim()) {
                Err(nom::Err::Incomplete(_needed)) => {
                    println!("Incomplete, {_needed:?}");
                    continue;
                }
                Err(nom::Err::Failure(err)) => {
                    println!("Invalid syntax, {err:?}");
                    continue;
                }
                Err(nom::Err::Error(err)) => {
                    println!("Invalid syntax, {err:?}");
                    continue;
                }
                Ok((rest, tokens)) => {
                    if !rest.is_empty() {
                        dbg!(rest);
                    }
                    tokens
                }
            };
            dbg!(&tokens);
            let ast = match ast::Parser::new(tokens.into_iter(), &mut vm.strings).parse() {
                Ok(ast) => ast,
                Err(err) => {
                    eprintln!("Syntax error: {:?}", err);
                    Vec::new()
                }
            };
            dbg!(&ast);

            for node in &ast {
                let _ = dbg!(vm.eval(node));
            }

            buf.clear();
        }
    }
}
