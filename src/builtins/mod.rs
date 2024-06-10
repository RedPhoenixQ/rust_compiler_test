use std::rc::Rc;

use anyhow::{bail, Result};
use ustr::Ustr;

use crate::{value::Value, Scope, VM};

#[derive(Debug, Clone, Copy)]
pub enum Builtin {
    Print,
    Dump,
}

impl From<Builtin> for Ustr {
    fn from(value: Builtin) -> Self {
        Ustr::from(match value {
            Builtin::Print => "print",
            Builtin::Dump => "dump",
        })
    }
}

impl Builtin {
    const VALUES: &'static [Self] = &[Self::Print, Self::Dump];

    pub fn call(&self, vm: &mut VM, number_of_arguments: usize) -> Result<Value> {
        match self {
            Builtin::Print => {
                if number_of_arguments < 1 {
                    bail!("Print takes atleast one argument");
                }
                for i in 0..number_of_arguments {
                    if i > 0 {
                        print!(", ")
                    }
                    let value = vm.pop_eval_stack()?;
                    let Value::String(string) = value.as_string() else {
                        unreachable!("Value::as_string() should always return Value::String")
                    };
                    print!("{}", string);
                }
                println!("");
                Ok(Value::Undefined)
            }
            Builtin::Dump => {
                dbg!(vm);
                Ok(Value::Undefined)
            }
        }
    }
}

const BUILTINS: &[&str] = &["print", "dump"];

pub fn get_builtins() -> Scope {
    Scope::from_iter(Builtin::VALUES.iter().map(|&builtin| {
        (
            builtin.into(),
            Rc::new(Value::BuiltInFunction(builtin).into()),
        )
    }))
}
