use std::{fs::read_to_string, io::stdin, path::PathBuf};

use anyhow::Result;
use clap::Parser;

mod ast;
mod tokenizer;
mod vm;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    file: Option<PathBuf>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    dbg!(&args);

    if let Some(file) = args.file {
        let code = read_to_string(file)?.into_boxed_str();
        let (rest, tokens) = tokenizer::tokenize(code.as_ref())
            .map_err(|err| anyhow::Error::msg(err.to_string()))?;
        if !rest.is_empty() {
            dbg!(rest);
        }
        let mut vm = vm::VM::default();
        let ast = ast::Parser::new(tokens.into_iter(), &mut vm.strings).parse()?;
        vm.eval_iter(ast.iter())?;
        stdin_eval(vm)?;
    } else {
        let vm = vm::VM::default();
        stdin_eval(vm)?;
    }
    Ok(())
}

fn stdin_eval(mut vm: vm::VM) -> Result<()> {
    let mut buf = String::new();
    while let Ok(_result) = stdin().read_line(&mut buf) {
        let tokens = match tokenizer::tokenize(&buf.trim()) {
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
    Ok(())
}
