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
        let code = read_to_string(file)?;
        let ops = VM::compile_str(&code)?;
        let mut vm = VM::default();
        vm.eval_ops(&ops)?;
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
        let ops = match VM::compile_str(&buf) {
            Err(err) => {
                eprintln!("{:?}", err);
                continue;
            }
            Ok(ops) => ops,
        };
        match vm.eval_ops(&ops) {
            Err(err) => eprintln!("{:?}", err),
            Ok(_) => println!("> {:#?}", vm.get_accumulator()),
        };
        buf.clear();
    }
    Ok(())
}
