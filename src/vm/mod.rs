use std::collections::BTreeMap;

use anyhow::{anyhow, bail, Result};

use crate::ast::{Ast, BinaryOp, Ident, Literal, UniaryOp};

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

impl Value {
    fn eval_uniary_op(self, op: &UniaryOp) -> Result<Value> {
        Ok(match op {
            UniaryOp::Not => {
                let Value::Boolean(bool) = self.coerce_boolean() else {
                    unreachable!("coerce_boolean is not boolean")
                };
                Value::Boolean(!bool)
            }
        })
    }

    fn eval_binary_op(self, other: Value, op: &BinaryOp) -> Result<Value> {
        Ok(match (self, other) {
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
            (lhs @ Value::Boolean(_), rhs) => lhs.eval_binary_op(rhs.coerce_boolean(), op)?,
            (lhs, rhs @ Value::Boolean(_)) => lhs.coerce_boolean().eval_binary_op(rhs, op)?,
            (lhs @ Value::String(_), rhs) => lhs.eval_binary_op(rhs.coerce_string(), op)?,
            (lhs, rhs @ Value::String(_)) => lhs.coerce_string().eval_binary_op(rhs, op)?,
            (lhs @ Value::Float(_), rhs) => lhs.eval_binary_op(rhs.coerce_float()?, op)?,
            (lhs, rhs @ Value::Float(_)) => lhs.coerce_float()?.eval_binary_op(rhs, op)?,
            (lhs @ Value::Int(_), rhs) => {
                if matches!(rhs, Value::Float(_)) {
                    lhs.coerce_float()?.eval_binary_op(rhs, op)?
                } else if let Ok(rhs) = rhs.coerce_int() {
                    lhs.eval_binary_op(rhs, op)?
                } else {
                    bail!("Type error: Operand {op:?}")
                }
            }
            (lhs, rhs @ Value::Int(_)) => {
                if matches!(lhs, Value::Float(_)) {
                    lhs.eval_binary_op(rhs.coerce_float()?, op)?
                } else if let Ok(lhs) = lhs.coerce_int() {
                    lhs.eval_binary_op(rhs, op)?
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

    fn is_truthy(&self) -> bool {
        match self {
            Value::String(value) => !value.is_empty(),
            Value::Int(value) => *value < 0,
            Value::Float(value) => value.is_normal(),
            Value::Boolean(value) => *value,
            Value::None => false,
        }
    }

    fn coerce_boolean(self) -> Self {
        Self::Boolean(self.is_truthy())
    }

    fn coerce_string(self) -> Self {
        Self::String(match self {
            Self::String(value) => value,
            Self::Int(value) => format!("{value}").into_boxed_str(),
            Self::Float(value) => format!("{value}").into_boxed_str(),
            Self::Boolean(value) => format!("{value}").into_boxed_str(),
            Self::None => "None".to_string().into_boxed_str(),
        })
    }

    fn coerce_float(self) -> Result<Self> {
        Ok(Self::Float(match self {
            Self::String(value) => value.parse()?,
            Self::Int(value) => value as f64,
            Self::Float(value) => value,
            Self::Boolean(true) => 1.,
            Self::Boolean(false) => 0.,
            Self::None => 0.,
        }))
    }

    fn coerce_int(self) -> Result<Self> {
        Ok(Self::Int(match self {
            Self::String(value) => value.parse()?,
            Self::Int(value) => value,
            Self::Float(value) => bail!("Cannot convert {:?} to int", value),
            Self::Boolean(true) => 1,
            Self::Boolean(false) => 0,
            Self::None => 0,
        }))
    }
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
            Ast::UniaryOp(op, value) => self.eval(value)?.eval_uniary_op(op)?,
            Ast::BinaryOp(op, lhs, rhs) => self.eval(lhs)?.eval_binary_op(self.eval(rhs)?, op)?,
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
                let predicate = self.eval(predicate)?.coerce_boolean();
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
}
