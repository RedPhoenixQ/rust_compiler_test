use std::{fs::read_to_string, io::stdin, path::PathBuf};

use anyhow::Result;
use clap::Parser;
use vm::VM;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    file: Option<PathBuf>,
}

fn main() -> Result<()> {
    let args = Args::parse();
    dbg!(&args);

    if let Some(file) = args.file {
        let mut vm = VM::default();
        vm.eval_file(&file)?;
        stdin_eval(vm)?;
    } else {
        let vm = VM::default();
        stdin_eval(vm)?;
    }
    Ok(())
}

fn stdin_eval(mut vm: VM) -> Result<()> {
    let mut buf = String::new();
    while let Ok(_result) = stdin().read_line(&mut buf) {
        match vm.eval_str(&buf.trim()) {
            Err(err) => eprintln!("{:?}", err),
            Ok(value) => println!("> {value:#?}"),
        };
        buf.clear();
    }
    Ok(())
}
