use std::{cell::RefCell, rc::Rc};

use anyhow::{bail, Result};
use ustr::{Ustr, UstrSet};

use crate::{
    builtins::Builtin,
    bytecode::Op,
    parser::{BinaryOp, UnaryOp},
    Scope,
};

#[derive(Debug)]
pub struct Function {
    pub arguments: Vec<(Ustr, Value)>,
    pub constants: Vec<Value>,
    pub code: Vec<Op>,
    pub foreign_idents: UstrSet,
}

#[derive(Debug)]
pub struct Closure {
    pub function: Rc<Function>,
    pub scope: Scope,
}

pub type Variable<T = Value> = Rc<RefCell<T>>;

#[derive(Debug, Default, Clone)]
pub enum Value {
    Int(i64),
    Float(f64),
    String(Ustr),
    Boolean(bool),
    Array(Variable<Vec<Value>>),
    Function(Rc<Function>),
    Closure(Rc<Closure>),
    BuiltInFunction(Builtin),
    #[default]
    Undefined,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => lhs == rhs,
            (Value::Float(lhs), Value::Float(rhs)) => lhs == rhs,
            (Value::String(lhs), Value::String(rhs)) => lhs == rhs,
            (Value::Boolean(lhs), Value::Boolean(rhs)) => lhs == rhs,
            (Value::Function(lhs), Value::Function(rhs)) => Rc::ptr_eq(lhs, rhs),
            _ => false,
        }
    }
}

impl From<i64> for Value {
    fn from(value: i64) -> Self {
        Self::Int(value)
    }
}

impl From<f64> for Value {
    fn from(value: f64) -> Self {
        if value.fract() < f64::EPSILON {
            Self::Int(value as i64)
        } else {
            Self::Float(value)
        }
    }
}

impl From<bool> for Value {
    fn from(value: bool) -> Self {
        Self::Boolean(value)
    }
}

impl From<Ustr> for Value {
    fn from(value: Ustr) -> Self {
        Self::String(value)
    }
}

impl From<&str> for Value {
    fn from(value: &str) -> Self {
        Self::String(value.into())
    }
}

impl Value {
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::String(value) => !value.is_empty(),
            Value::Int(value) => *value != 0,
            Value::Float(value) => value.is_normal(),
            Value::Boolean(value) => *value,
            Value::Array(vec) => !vec.borrow().is_empty(),
            Value::Function(_) => true,
            Value::Closure(_) => true,
            Value::BuiltInFunction(_) => true,
            Value::Undefined => false,
        }
    }

    pub fn as_boolean(&self) -> Self {
        Value::Boolean(self.is_truthy())
    }

    pub fn as_string(&self) -> Self {
        Value::String(match self {
            Value::String(value) => *value,
            Value::Int(value) => format!("{value}").into(),
            Value::Float(value) => format!("{value}").into(),
            Value::Boolean(value) => format!("{value}").into(),
            Value::Array(_) => "array".into(),
            Value::Function(_) => "function".into(),
            Value::Closure(_) => "function".into(),
            Value::BuiltInFunction(_) => "function".into(),
            Value::Undefined => "undefined".into(),
        })
    }

    // Number may be NaN
    pub fn as_number(&self) -> Result<Self> {
        Ok(match self {
            Self::String(value) => value.parse::<f64>().unwrap_or(f64::NAN).into(),
            Self::Int(value) => (*value).into(),
            Self::Float(value) => (*value).into(),
            Self::Boolean(true) => 1.into(),
            Self::Boolean(false) => 0.into(),
            Self::Array(_) => bail!("TypeError: Array can not be used as a number"),
            Self::Function(_) => bail!("TypeError: Function can not be used as a number"),
            Self::Closure(_) => bail!("TypeError: Function can not be used as a number"),
            Self::BuiltInFunction(_) => bail!("TypeError: Function can not be used as a number"),
            Self::Undefined => bail!("TypeError: Undefined can not be used as a number"),
        })
    }

    fn array_index(key: Value, array_len: usize) -> Result<usize> {
        let index = match key.as_number()? {
            Value::Float(float) => {
                if float.is_nan() {
                    bail!("NaN cannot be used to index an array")
                }
                float as i64
            }
            Value::Int(int) => int as i64,
            _ => unreachable!("as_number() should always return a number"),
        };
        Ok(if index.is_negative() {
            array_len.wrapping_sub(index.abs() as usize) % array_len
        } else {
            index as usize
        })
    }

    pub fn get(&self, key: Value) -> Result<Value> {
        match self {
            Self::Array(vec) => {
                let vec = vec.borrow();
                let index = Self::array_index(key, vec.len())?;
                vec.get(index).cloned().ok_or(anyhow::anyhow!(
                    "Index out of bounds: Index {}, length {}",
                    index,
                    vec.len()
                ))
            }
            _ => bail!("TypeError: Cannot access key {:?}, on {:?}", key, self),
        }
    }
    pub fn set(&mut self, key: Value, value: Value) -> Result<()> {
        match self {
            Self::Array(vec) => {
                let mut vec = vec.borrow_mut();
                let length = vec.len();
                let index = Self::array_index(key, length)?;
                if length == index {
                    // Allow pushing to array by setting the index after the last item
                    vec.push(value);
                } else {
                    let current = vec.get_mut(index as usize).ok_or(anyhow::anyhow!(
                        "Index out of bounds: Index {index}, length {length}",
                    ))?;
                    *current = value;
                }
            }
            _ => bail!("TypeError: Cannot set key {:?}, on {:?}", key, self),
        }
        Ok(())
    }

    pub fn eval_unary_op(&self, operation: &UnaryOp) -> Result<Self> {
        Ok(match operation {
            UnaryOp::LogicalNot => Value::Boolean(!self.is_truthy()),

            UnaryOp::BitwiseNot => {
                if let Value::Int(value) = self {
                    (!value).into()
                } else {
                    bail!("TypeError: {:?} is not applicable to {:?}", operation, self)
                }
            }

            UnaryOp::Negative => match self.as_number()? {
                Value::Int(value) => (-value).into(),
                Value::Float(value) => (-value).into(),
                _ => unreachable!("Value::as_number() should always return a number"),
            },

            UnaryOp::Positive => self.as_number()?,
        })
    }

    pub fn eval_binary_op(&self, other: &Value, op: &BinaryOp) -> Result<Value> {
        Ok(match (self, other) {
            (Value::Int(lhs), Value::Int(rhs)) => match op {
                BinaryOp::Add => Value::Int(lhs + rhs),
                BinaryOp::Sub => Value::Int(lhs - rhs),
                BinaryOp::Div => Value::Int(lhs / rhs),
                BinaryOp::Mul => Value::Int(lhs * rhs),
                BinaryOp::Mod => Value::Int(lhs % rhs),
                BinaryOp::Eq => Value::Boolean(lhs == rhs),
                BinaryOp::Neq => Value::Boolean(lhs != rhs),
                BinaryOp::Lt => Value::Boolean(lhs < rhs),
                BinaryOp::Gt => Value::Boolean(lhs > rhs),
                BinaryOp::LtEq => Value::Boolean(lhs <= rhs),
                BinaryOp::GtEq => Value::Boolean(lhs >= rhs),
                BinaryOp::BitwiseAnd => Value::Int(lhs & rhs),
                BinaryOp::BitwiseOr => Value::Int(lhs | rhs),
                BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                    self.as_boolean().eval_binary_op(&other.as_boolean(), op)?
                }
            },
            (Value::Float(lhs), Value::Float(rhs)) => match op {
                BinaryOp::Add => (lhs + rhs).into(),
                BinaryOp::Sub => (lhs - rhs).into(),
                BinaryOp::Div => (lhs / rhs).into(),
                BinaryOp::Mul => (lhs * rhs).into(),
                BinaryOp::Mod => (lhs % rhs).into(),
                BinaryOp::Eq => Value::Boolean(lhs == rhs),
                BinaryOp::Neq => Value::Boolean(lhs != rhs),
                BinaryOp::Lt => Value::Boolean(lhs < rhs),
                BinaryOp::Gt => Value::Boolean(lhs > rhs),
                BinaryOp::LtEq => Value::Boolean(lhs <= rhs),
                BinaryOp::GtEq => Value::Boolean(lhs >= rhs),
                BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                    self.as_boolean().eval_binary_op(&other.as_boolean(), op)?
                }
                BinaryOp::BitwiseOr | BinaryOp::BitwiseAnd => {
                    bail!("TypeError: '{op:?}' cannot be used on {self:?} and {other:?}")
                }
            },
            (Value::String(lhs), Value::String(rhs)) => match op {
                BinaryOp::Add => {
                    let mut string = lhs.to_string();
                    string.push_str(&rhs);
                    Value::String(string.into())
                }
                BinaryOp::Eq => Value::Boolean(lhs == rhs),
                BinaryOp::Neq => Value::Boolean(lhs != rhs),
                BinaryOp::Lt => Value::Boolean(lhs < rhs),
                BinaryOp::Gt => Value::Boolean(lhs > rhs),
                BinaryOp::LtEq => Value::Boolean(lhs <= rhs),
                BinaryOp::GtEq => Value::Boolean(lhs >= rhs),
                BinaryOp::LogicalOr | BinaryOp::LogicalAnd => {
                    self.as_boolean().eval_binary_op(&other.as_boolean(), op)?
                }
                BinaryOp::Sub
                | BinaryOp::Div
                | BinaryOp::Mul
                | BinaryOp::Mod
                | BinaryOp::BitwiseOr
                | BinaryOp::BitwiseAnd => {
                    bail!("TypeError: '{op:?}' cannot be used on {self:?} and {other:?}")
                }
            },
            (lhs, rhs @ Value::Boolean(_)) | (lhs @ Value::Boolean(_), rhs) => {
                let (Value::Boolean(lhs), Value::Boolean(rhs)) =
                    (lhs.as_boolean(), rhs.as_boolean())
                else {
                    unreachable!("Value::as_boolean should always return booleans")
                };
                match op {
                    BinaryOp::Eq => Value::Boolean(lhs == rhs),
                    BinaryOp::Neq => Value::Boolean(lhs != rhs),
                    BinaryOp::Lt => Value::Boolean(lhs < rhs),
                    BinaryOp::Gt => Value::Boolean(lhs > rhs),
                    BinaryOp::LtEq => Value::Boolean(lhs <= rhs),
                    BinaryOp::GtEq => Value::Boolean(lhs >= rhs),
                    BinaryOp::LogicalAnd => Value::Boolean(lhs && rhs),
                    BinaryOp::LogicalOr => Value::Boolean(lhs || rhs),
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Div
                    | BinaryOp::Mul
                    | BinaryOp::Mod
                    | BinaryOp::BitwiseOr
                    | BinaryOp::BitwiseAnd => {
                        bail!("TypeError: '{op:?}' cannot be used on {self:?} and {other:?}")
                    }
                }
            }

            (Value::Int(int), rhs @ Value::Float(_)) => {
                Value::Float(*int as f64).eval_binary_op(rhs, op)?
            }
            (lhs @ Value::Float(_), Value::Int(int)) => {
                lhs.eval_binary_op(&Value::Float(*int as f64), op)?
            }

            (lhs @ Value::String(_), rhs) => lhs.eval_binary_op(&rhs.as_string(), op)?,
            (lhs, rhs @ Value::String(_)) => lhs.as_string().eval_binary_op(rhs, op)?,

            (_lhs, _rhs) => bail!("TypeError: '{op:?}' cannot be used on {self:?} and {other:?}"),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn add() -> Result<()> {
        let op = &BinaryOp::Add;
        assert_eq!(
            Value::Int(2).eval_binary_op(&2.into(), op).unwrap(),
            Value::Int(4)
        );
        assert_eq!(
            Value::Float(2.).eval_binary_op(&2.into(), op).unwrap(),
            Value::Int(4)
        );
        assert_eq!(
            Value::Float(2.).eval_binary_op(&2.into(), op).unwrap(),
            Value::Int(4)
        );
        Ok(())
    }
}
