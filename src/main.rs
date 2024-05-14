use std::{io::stdin, path::PathBuf};

use clap::Parser;

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
            let mut iter = tokenizer::tokenize(&buf.trim());
            let tokens = iter.collect::<Vec<_>>();
            match iter.finish() {
                Err(nom::Err::Incomplete(_needed)) => {
                    println!("Incomplete, {_needed:?}");
                    continue;
                }
                Err(nom::Err::Failure(err)) => {
                    println!("Invalid syntax, {err:?}")
                }
                Err(nom::Err::Error(err)) => {
                    println!("Invalid syntax, {err:?}")
                }
                Ok((rest, _)) => {
                    dbg!(&rest);
                    // vm.exec(expr.clone());
                }
            }
            dbg!(&tokens);
            let ast = match ast::Parser::new(tokens.into_iter()).parse() {
                Ok(ast) => ast,
                Err(err) => {
                    eprintln!("Syntax error: {:?}", err);
                    continue;
                }
            };
            dbg!(&ast);

            for node in &ast {
                dbg!(vm.eval(node));
            }

            buf.clear();
        }
    }
}
