use anyhow::{bail, Result};

use crate::{value::Value, VM};

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Builtin {
    Print,
    Dump,
    Assert
}

impl TryFrom<&str> for Builtin {
    type Error = ();
    fn try_from(value: &str) -> std::result::Result<Self, Self::Error> {
        Ok(match value {
            "print" => Self::Print,
            "dump" => Self::Dump,
            "assert" => Self::Assert,
            _ => return Err(()),
        })
    }
}

impl VM {
    pub fn call_builtin(&mut self, builtin: Builtin, number_of_arguments: usize) -> Result<Value> {
        match builtin {
            Builtin::Print => {
                if number_of_arguments < 1 {
                    bail!("Print takes atleast one argument");
                }
                for i in 0..number_of_arguments {
                    if i > 0 {
                        print!(", ")
                    }
                    let value = self.pop_eval_stack()?;
                    let Value::String(string) = value.as_string() else {
                        unreachable!("Value::as_string() should always return Value::String")
                    };
                    print!("{}", string);
                }
                println!();
                Ok(Value::Undefined)
            }
            Builtin::Dump => {
                if number_of_arguments > 0 {
                    bail!("Dump does not take any arguments");
                }
                dbg!(self);
                Ok(Value::Undefined)
            }
            Builtin::Assert => {
                let (pred, msg) = match number_of_arguments {
                    1 => (self.pop_eval_stack()?, None),
                    2 => (self.pop_eval_stack()?, Some(self.pop_eval_stack()?.as_str())),
                    0 => bail!("Assert takes atleast one predicate and optionally an error message"),
                    _ => bail!("Assert takes no more than 2 arguments"),
                };
                if !pred.is_truthy() {
                    if let Some(msg) = msg {
                        eprintln!("Assertion failed: {}", msg);
                    } else {
                        eprintln!("Assertion failed")
                    }
                }

                Ok(Value::Undefined)
            }
        }
    }
}
