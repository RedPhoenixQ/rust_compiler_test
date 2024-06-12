use anyhow::{bail, Result};

use super::Value;

#[derive(Debug, Default)]
pub struct Array(pub Vec<Value>);

#[derive(Debug, Clone, Copy)]
pub enum ArrayMethod {
    Length,
    Pop,
    Push,
}

impl TryFrom<&str> for ArrayMethod {
    type Error = ();
    fn try_from(value: &str) -> Result<Self, ()> {
        Ok(match value {
            "len" => Self::Length,
            "pop" => Self::Pop,
            "push" => Self::Push,
            _ => return Err(()),
        })
    }
}

impl Array {
    pub fn call(&mut self, method: ArrayMethod, arguments: Vec<Value>) -> Result<Value> {
        Ok(match method {
            ArrayMethod::Length => {
                if !arguments.is_empty() {
                    bail!("{method:?} does not take any arguments");
                }
                Value::Int(self.0.len() as i64)
            }
            ArrayMethod::Pop => {
                if !arguments.is_empty() {
                    bail!("{method:?} does not take any arguments");
                }
                self.0.pop().unwrap_or_default()
            }
            ArrayMethod::Push => {
                if arguments.is_empty() {
                    bail!("{method:?} expects atleast one argument");
                }
                self.0.extend(arguments);
                Value::Undefined
            }
        })
    }
}
