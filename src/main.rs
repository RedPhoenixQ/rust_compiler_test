use std::{io::stdin, path::PathBuf};

use clap::Parser;

mod tokenizer;

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
        let mut buf = String::new();
        while let Ok(_result) = stdin().read_line(&mut buf) {
            let input = buf.clone();
            let mut iter = tokenizer::tokenize(&input.trim());
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

            buf.clear();
        }
    }
}
