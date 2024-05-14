use std::{collections::BTreeMap, rc::Rc};

use anyhow::{anyhow, bail, Result};

use crate::ast::{Ast, BinaryOp, Function, Ident, Literal, UniaryOp};

#[derive(Debug, Default)]
pub struct VM {
    globals: BTreeMap<Ident, Value>,
    stack: Vec<BTreeMap<Ident, Value>>,
}

#[derive(Debug, Clone)]
pub enum Value {
    String(Rc<str>),
    Int(i64),
    Float(f64),
    Boolean(bool),
    Function(Rc<Function>),
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
                    bail!("Type error: '{:?}' cannot be used on floats", op)
                }
            },
            (Value::Float(lhs), Value::Float(rhs)) => match op {
                BinaryOp::Add => Value::Float(lhs + rhs),
                BinaryOp::Sub => Value::Float(lhs - rhs),
                BinaryOp::Div => Value::Float(lhs / rhs),
                BinaryOp::Mul => Value::Float(lhs * rhs),
                BinaryOp::Mod => Value::Float(lhs % rhs),
                BinaryOp::Eq => Value::Boolean(lhs == rhs),
                BinaryOp::Neq => Value::Boolean(lhs != rhs),
                BinaryOp::Lt => Value::Boolean(lhs < rhs),
                BinaryOp::Gt => Value::Boolean(lhs > rhs),
                BinaryOp::LtEq => Value::Boolean(lhs <= rhs),
                BinaryOp::GtEq => Value::Boolean(lhs >= rhs),
                BinaryOp::LogicalOr
                | BinaryOp::LogicalAnd
                | BinaryOp::BitwiseOr
                | BinaryOp::BitwiseAnd => {
                    bail!("Type error: '{:?}' cannot be used on floats", op)
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
                BinaryOp::Sub
                | BinaryOp::Div
                | BinaryOp::Mul
                | BinaryOp::Mod
                | BinaryOp::LogicalOr
                | BinaryOp::LogicalAnd
                | BinaryOp::BitwiseOr
                | BinaryOp::BitwiseAnd => {
                    bail!("Type error: '{:?}' cannot be used on booleans", op)
                }
            },
            (Value::Boolean(lhs), Value::Boolean(rhs)) => match op {
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
                    bail!("Type error: '{:?}' cannot be used on booleans", op)
                }
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
            Value::Function(_) => true,
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
            Self::Function(value) => {
                format!("{}({:?})", value.ident.0, value.args).into_boxed_str()
            }
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
            Self::Function(_) => bail!("Cannot coerce function to float"),
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
            Self::Function(_) => bail!("Cannot coerce function to int"),
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
            Ast::Ident(ident) => self.get_ident_value(ident).cloned().unwrap_or(Value::None),
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
            Ast::FunctionDecl(function) => {
                self.globals.insert(
                    function.ident.clone(),
                    Value::Function(Rc::new(function.clone())),
                );
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
            Ast::FunctionCall { ident, args } => {
                let Ok(Value::Function(function)) = self.get_ident_value(ident).cloned() else {
                    bail!("{} is not a function", ident.0)
                };

                if args.len() != function.args.len() {
                    bail!(
                        "Invalid number of arguments. Expected {}, got {}",
                        function.args.len(),
                        args.len()
                    )
                }

                let mut scope = BTreeMap::new();
                for (arg, value) in function.args.iter().cloned().zip(args.iter()) {
                    let value = self.eval(value)?;
                    scope.insert(arg, value);
                }
                self.stack.push(scope);
                let value = self.eval(&function.body)?;
                self.stack.pop();
                value
            }
            Ast::Group(ast) => self.eval(ast)?,
            Ast::Block(lines) => self.run(lines.iter())?,
        })
    }

    fn get_ident_value(&self, ident: &Ident) -> Result<&Value> {
        self.stack
            .iter()
            .find_map(|scope| scope.get(ident))
            .or_else(|| self.globals.get(ident))
            .ok_or(anyhow!("Undefined variable: {:?}", ident))
    }
}
