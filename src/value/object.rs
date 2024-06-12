use std::rc::Rc;

use anyhow::{bail, Result};
use ustr::UstrMap;

use super::{array::Array, Value};

#[derive(Debug, Default)]
pub struct Object(pub UstrMap<Value>);

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ObjectMethod {
    Entries,
    Values,
    Keys,
}

impl TryFrom<&str> for ObjectMethod {
    type Error = ();
    fn try_from(value: &str) -> Result<Self, ()> {
        Ok(match value {
            "entrie" => Self::Entries,
            "values" => Self::Values,
            "keys" => Self::Keys,
            _ => return Err(()),
        })
    }
}

impl From<Object> for Value {
    fn from(value: Object) -> Self {
        Value::Object(Rc::new(value.into()))
    }
}

impl Object {
    pub fn call(&mut self, method: ObjectMethod, arguments: Vec<Value>) -> Result<Value> {
        Ok(match method {
            ObjectMethod::Entries => {
                if !arguments.is_empty() {
                    bail!("{method:?} does not take any arguments");
                }
                Array(
                    self.0
                        .iter()
                        .map(|(key, value)| Array(vec![Value::String(*key), value.clone()]).into())
                        .collect(),
                )
                .into()
            }
            ObjectMethod::Values => {
                if !arguments.is_empty() {
                    bail!("{method:?} does not take any arguments");
                }
                Array(self.0.values().cloned().collect()).into()
            }
            ObjectMethod::Keys => {
                if !arguments.is_empty() {
                    bail!("{method:?} does not take any arguments");
                }
                Array(self.0.keys().map(|key| Value::String(*key)).collect()).into()
            }
        })
    }
}
