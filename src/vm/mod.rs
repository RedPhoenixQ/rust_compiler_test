use std::collections::BTreeMap;

use anyhow::{anyhow, bail, Result};

use crate::ast::{Ast, BinaryOp, Ident, Literal};

#[derive(Debug, Default)]
pub struct VM {
    globals: BTreeMap<Ident, Value>,
}

#[derive(Debug, Clone)]
pub enum Value {
    String(Box<str>),
    Int(i64),
    Float(f64),
    Boolean(bool),
    None,
}

impl From<&Literal> for Value {
    fn from(value: &Literal) -> Self {
        match value {
            Literal::String(inner) => Value::String(inner.clone()),
            Literal::Int(inner) => Value::Int(*inner),
            Literal::Float(inner) => Value::Float(*inner),
            Literal::Boolean(inner) => Value::Boolean(*inner),
        }
    }
}

impl VM {
    pub fn run<'a>(&mut self, script: impl Iterator<Item = &'a Ast>) -> Result<Value> {
        let mut out = Value::None;
        for ast in script {
            out = self.eval(ast)?;
        }
        Ok(out)
    }

    pub fn eval(&mut self, ast: &Ast) -> Result<Value> {
        Ok(match ast {
            Ast::Assignment { ident, value } => {
                let value = self.eval(value)?;
                self.globals.insert(ident.clone(), value.clone());
                value
            }
            Ast::Literal(literal) => literal.into(),
            Ast::Ident(ident) => self
                .globals
                .get(&ident)
                .ok_or(anyhow!("Undefined variable: {:?}", ident))?
                .clone(),
            Ast::UniaryOp(_, _) => todo!(),
            Ast::BinaryOp(op, lhs, rhs) => {
                Self::eval_binary_op(op, self.eval(lhs)?, self.eval(rhs)?)?
            }
            Ast::VariableDecl { ident, value } => {
                if self.globals.contains_key(&ident) {
                    bail!("Variable {:?} already exists", ident)
                }

                match value {
                    Some(ast) => {
                        let value = self.eval(ast)?;
                        self.globals.insert(ident.clone(), value)
                    }
                    None => self.globals.insert(ident.clone(), Value::None),
                };
                Value::None
            }
            Ast::If {
                predicate,
                then_branch,
                else_branch,
            } => {
                let predicate = Self::coerce_boolean(self.eval(predicate)?);
                if matches!(predicate, Value::Boolean(true)) {
                    self.eval(then_branch)?
                } else if let Some(else_branch) = else_branch {
                    self.eval(else_branch)?
                } else {
                    Value::None
                }
            }
            Ast::Block(lines) => self.run(lines.iter())?,
        })
    }

    fn eval_binary_op(op: &BinaryOp, lhs: Value, rhs: Value) -> Result<Value> {
        Ok(match (lhs, rhs) {
            (Value::Int(lhs), Value::Int(rhs)) => match op {
                BinaryOp::Add => Value::Int(lhs + rhs),
                BinaryOp::Sub => Value::Int(lhs - rhs),
                BinaryOp::Div => Value::Int(lhs / rhs),
                BinaryOp::Mul => Value::Int(lhs * rhs),
                BinaryOp::Eq => Value::Boolean(lhs == rhs),
                BinaryOp::Neq => Value::Boolean(lhs != rhs),
                BinaryOp::Lt => Value::Boolean(lhs < rhs),
                BinaryOp::Gt => Value::Boolean(lhs > rhs),
                BinaryOp::LtEq => Value::Boolean(lhs <= rhs),
                BinaryOp::GtEq => Value::Boolean(lhs >= rhs),
                BinaryOp::And => bail!("Invalid AND boolean operation on ints"),
                BinaryOp::Or => bail!("Invalid OR boolean operation on ints"),
            },
            (Value::Float(lhs), Value::Float(rhs)) => match op {
                BinaryOp::Add => Value::Float(lhs + rhs),
                BinaryOp::Sub => Value::Float(lhs - rhs),
                BinaryOp::Div => Value::Float(lhs / rhs),
                BinaryOp::Mul => Value::Float(lhs * rhs),
                BinaryOp::Eq => Value::Boolean(lhs == rhs),
                BinaryOp::Neq => Value::Boolean(lhs != rhs),
                BinaryOp::Lt => Value::Boolean(lhs < rhs),
                BinaryOp::Gt => Value::Boolean(lhs > rhs),
                BinaryOp::LtEq => Value::Boolean(lhs <= rhs),
                BinaryOp::GtEq => Value::Boolean(lhs >= rhs),
                BinaryOp::And => bail!("Invalid AND boolean operation on floats"),
                BinaryOp::Or => bail!("Invalid OR boolean operation on floats"),
            },
            (Value::String(lhs), Value::String(rhs)) => match op {
                BinaryOp::Add => {
                    let mut string = lhs.to_string();
                    string.push_str(&rhs);
                    Value::String(string.into_boxed_str())
                }
                BinaryOp::Sub => bail!("Type error: '-' cannot be used on strings"),
                BinaryOp::Div => bail!("Type error: '/' cannot be used on strings"),
                BinaryOp::Mul => bail!("Type error: '*' cannot be used on strings"),
                BinaryOp::Eq => Value::Boolean(lhs == rhs),
                BinaryOp::Neq => Value::Boolean(lhs != rhs),
                BinaryOp::Lt => Value::Boolean(lhs < rhs),
                BinaryOp::Gt => Value::Boolean(lhs > rhs),
                BinaryOp::LtEq => Value::Boolean(lhs <= rhs),
                BinaryOp::GtEq => Value::Boolean(lhs >= rhs),
                BinaryOp::And => bail!("Invalid AND boolean operation on strings"),
                BinaryOp::Or => bail!("Invalid OR boolean operation on strings"),
            },
            (Value::Boolean(lhs), Value::Boolean(rhs)) => match op {
                BinaryOp::Add => bail!("Type error: '-' cannot be used on booleans"),
                BinaryOp::Sub => bail!("Type error: '-' cannot be used on booleans"),
                BinaryOp::Div => bail!("Type error: '/' cannot be used on booleans"),
                BinaryOp::Mul => bail!("Type error: '*' cannot be used on booleans"),
                BinaryOp::Eq => Value::Boolean(lhs == rhs),
                BinaryOp::Neq => Value::Boolean(lhs != rhs),
                BinaryOp::Lt => Value::Boolean(lhs < rhs),
                BinaryOp::Gt => Value::Boolean(lhs > rhs),
                BinaryOp::LtEq => Value::Boolean(lhs <= rhs),
                BinaryOp::GtEq => Value::Boolean(lhs >= rhs),
                BinaryOp::And => Value::Boolean(lhs && rhs),
                BinaryOp::Or => Value::Boolean(lhs || rhs),
            },
            (lhs @ Value::Boolean(_), rhs) => {
                Self::eval_binary_op(op, lhs, Self::coerce_boolean(rhs))?
            }
            (lhs, rhs @ Value::Boolean(_)) => {
                Self::eval_binary_op(op, Self::coerce_boolean(lhs), rhs)?
            }
            (lhs @ Value::String(_), rhs) => {
                Self::eval_binary_op(op, lhs, Self::coerce_string(rhs))?
            }
            (lhs, rhs @ Value::String(_)) => {
                Self::eval_binary_op(op, Self::coerce_string(lhs), rhs)?
            }
            (lhs @ Value::Float(_), rhs) => {
                Self::eval_binary_op(op, lhs, Self::coerce_float(rhs)?)?
            }
            (lhs, rhs @ Value::Float(_)) => {
                Self::eval_binary_op(op, Self::coerce_float(lhs)?, rhs)?
            }
            (lhs @ Value::Int(_), rhs) => {
                if matches!(rhs, Value::Float(_)) {
                    Self::eval_binary_op(op, Self::coerce_float(lhs)?, rhs)?
                } else if let Ok(rhs) = Self::coerce_int(rhs) {
                    Self::eval_binary_op(op, lhs, rhs)?
                } else {
                    bail!("Type error: Operand {op:?}")
                }
            }
            (lhs, rhs @ Value::Int(_)) => {
                if matches!(lhs, Value::Float(_)) {
                    Self::eval_binary_op(op, lhs, Self::coerce_float(rhs)?)?
                } else if let Ok(lhs) = Self::coerce_int(lhs) {
                    Self::eval_binary_op(op, lhs, rhs)?
                } else {
                    bail!("Type error: Operand {op:?}")
                }
            }
            (lhs, rhs) => bail!(
                "Type error: Cannot perform {:?} on {:?} and {:?}",
                op,
                lhs,
                rhs
            ),
        })
    }

    fn coerce_boolean(value: Value) -> Value {
        Value::Boolean(match value {
            Value::String(value) => value.is_empty(),
            Value::Int(value) => value < 0,
            Value::Float(value) => value.is_normal(),
            Value::Boolean(value) => value,
            Value::None => false,
        })
    }

    fn coerce_string(value: Value) -> Value {
        Value::String(match value {
            Value::String(value) => value,
            Value::Int(value) => format!("{value}").into_boxed_str(),
            Value::Float(value) => format!("{value}").into_boxed_str(),
            Value::Boolean(value) => format!("{value}").into_boxed_str(),
            Value::None => "None".to_string().into_boxed_str(),
        })
    }

    fn coerce_float(value: Value) -> Result<Value> {
        Ok(Value::Float(match value {
            Value::String(value) => value.parse()?,
            Value::Int(value) => value as f64,
            Value::Float(value) => value,
            Value::Boolean(true) => 1.,
            Value::Boolean(false) => 0.,
            Value::None => 0.,
        }))
    }

    fn coerce_int(value: Value) -> Result<Value> {
        Ok(Value::Int(match value {
            Value::String(value) => value.parse()?,
            Value::Int(value) => value,
            Value::Float(value) => bail!("Cannot convert {:?} to int", value),
            Value::Boolean(true) => 1,
            Value::Boolean(false) => 0,
            Value::None => 0,
        }))
    }
}
