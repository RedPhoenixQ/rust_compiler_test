use std::rc::Rc;

use ustr::{Ustr, UstrSet};

use crate::{bytecode::Op, Scope};

use super::Value;

#[derive(Debug)]
pub struct Function {
    pub arguments: Vec<(Ustr, Value)>,
    pub constants: Vec<Value>,
    pub code: Vec<Op>,
    pub foreign_idents: UstrSet,
}

impl From<Function> for Value {
    fn from(value: Function) -> Self {
        Value::Function(Rc::new(value))
    }
}

impl From<Rc<Function>> for Value {
    fn from(value: Rc<Function>) -> Self {
        Value::Function(value)
    }
}

#[derive(Debug)]
pub struct Closure {
    pub function: Rc<Function>,
    pub scope: Scope,
}

impl From<Closure> for Value {
    fn from(value: Closure) -> Self {
        Value::Closure(Rc::new(value))
    }
}

impl From<Rc<Closure>> for Value {
    fn from(value: Rc<Closure>) -> Self {
        Value::Closure(value)
    }
}
