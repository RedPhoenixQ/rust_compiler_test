use std::{fs::read_to_string, io::stdin, path::PathBuf};

use anyhow::Result;
use clap::Parser;
use vm::VM;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    file: Option<PathBuf>,
    #[arg(short, long)]
    verbose: bool,
}

fn main() -> Result<()> {
    let args = Args::parse();
    dbg!(&args);

    let mut vm = VM::new(args.verbose);
    if let Some(file) = args.file {
        let code = read_to_string(file)?;
        let bundle = VM::compile_str(&code)?;
        vm.eval(&bundle.code)?;
    }
    stdin_eval(vm)?;
    Ok(())
}

fn stdin_eval(mut vm: VM) -> Result<()> {
    let mut buf = String::new();
    while let Ok(_result) = stdin().read_line(&mut buf) {
        let bundle = match VM::compile_str(&buf) {
            Err(err) => {
                eprintln!("{:?}", err);
                buf.clear();
                continue;
            }
            Ok(bundle) => bundle,
        };
        if vm.debug {
            println!("{:?}", &bundle);
        }
        match vm.eval(&bundle.code) {
            Err(err) => eprintln!("{:?}", err),
            Ok(value) => println!("> {:#?}", value),
        };
        buf.clear();
    }
    Ok(())
}
