use std::{
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
};

use anyhow::{anyhow, bail, Result};

use crate::ast::{Ast, BinaryOp, Block, Ident, Literal, UniaryOp};

#[derive(Debug, Default)]
pub struct VM {
    globals: BTreeMap<Ident, Value>,
    stack: Vec<BTreeMap<Ident, Value>>,
    pub strings: BTreeSet<Rc<str>>,
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

#[derive(Debug)]
pub struct Function {
    pub args: Vec<Ident>,
    pub body: Box<Block>,
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
            Value::Int(value) => value.is_positive(),
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
            Self::Int(value) => format!("{value}").into(),
            Self::Float(value) => format!("{value}").into(),
            Self::Boolean(value) => format!("{value}").into(),
            Self::Function(value) => format!("fn({:?})", value.args).into(),
            Self::None => "None".to_string().into(),
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
            Literal::String(inner) => Value::String(inner.to_string().into()),
            Literal::Int(inner) => Value::Int(*inner),
            Literal::Float(inner) => Value::Float(*inner),
            Literal::Boolean(inner) => Value::Boolean(*inner),
        }
    }
}

impl VM {
    pub fn eval_iter<'a>(&mut self, script: impl Iterator<Item = &'a Ast>) -> Result<Value> {
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
                if let Some(scope) = self.stack.last_mut() {
                    scope.insert(ident.clone(), value);
                } else {
                    self.globals.insert(ident.clone(), value);
                }
                Value::None
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
            Ast::FunctionDecl { args, body } => Value::Function(Rc::new(Function {
                args: args.clone(),
                body: body.clone(),
            })),
            Ast::If {
                predicate,
                then_branch,
                else_branch,
            } => {
                let predicate = self.eval(predicate)?.coerce_boolean();
                let mut out = Value::None;
                if matches!(predicate, Value::Boolean(true)) {
                    let value = self.eval_iter(then_branch.content.iter())?;
                    if then_branch.implicit_return {
                        out = value
                    }
                } else if let Some(else_branch) = else_branch {
                    let value = self.eval_iter(else_branch.content.iter())?;
                    if else_branch.implicit_return {
                        out = value
                    }
                };
                out
            }
            Ast::FunctionCall { ident, args } => {
                let Ok(Value::Function(function)) = self.get_ident_value(ident).cloned() else {
                    match self.call_builtin(ident) {
                        Ok(value) => return Ok(value),
                        Err(_err) => bail!("{} is not a function", ident.0),
                    }
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
                let value = self.eval_iter(function.body.content.iter())?;
                self.stack.pop();
                value
            }
            Ast::Return(value) => todo!("handle return of {:?}", value),
            Ast::Group(ast) => self.eval(ast)?,
            Ast::Block(Block {
                content,
                implicit_return,
            }) => match self.eval_iter(content.iter()) {
                Ok(value) if *implicit_return => value,
                Ok(_) => Value::None,
                Err(err) => return Err(err),
            },
        })
    }

    fn get_ident_value(&self, ident: &Ident) -> Result<&Value> {
        self.stack
            .iter()
            .find_map(|scope| scope.get(ident))
            .or_else(|| self.globals.get(ident))
            .ok_or(anyhow!("Undefined variable: {:?}", ident))
    }

    fn call_builtin(&self, ident: &Ident) -> Result<Value> {
        Ok(match ident.0.as_ref() {
            "dump" => {
                dbg!(self);
                Value::None
            }
            name => bail!("No builtin named {} exists", name),
        })
    }
}

#[cfg(test)]
mod test {
    use std::fmt::Write;

    use super::*;
    use crate::{ast::Parser, tokenizer::tokenize};

    #[test]
    #[ignore = "unimplemented"]
    fn int_ops() {}

    #[test]
    #[ignore = "unimplemented"]
    fn float_ops() {}

    #[test]
    #[ignore = "unimplemented"]
    fn string_ops() {}

    #[test]
    #[ignore = "unimplemented"]
    fn mixed_ops() {}

    #[test]
    fn variable_declaration() {
        const CODE: &str = r#"let a = 123;
        a;
        let b = "test" + 321;
        b;
        a + b;
        "#;
        let mut vm = VM::default();
        let (_rest, tokens) = tokenize(CODE).unwrap();
        let mut code = Parser::new(tokens.into_iter(), &mut vm.strings)
            .parse()
            .expect("Code to compile")
            .into_iter();

        let decl_a = vm.eval(&code.next().unwrap());
        assert!(
            matches!(decl_a, Ok(Value::None)),
            "Declaration failed: {decl_a:?}"
        );

        let var_a = vm.eval(&code.next().unwrap());
        assert!(
            matches!(var_a, Ok(Value::Int(123))),
            "a not defined: {var_a:?}"
        );

        let decl_b = vm.eval(&code.next().unwrap());
        assert!(
            matches!(decl_b, Ok(Value::None)),
            "Declaration failed: {decl_b:?}"
        );

        let var_b = vm.eval(&code.next().unwrap());
        assert!(
            matches!(
                var_b,
                Ok(Value::String(ref str)) if str.as_ref() == "test321"
            ),
            "b not defined: {var_b:?}"
        );

        let a_plus_b = vm.eval(&code.next().unwrap());
        assert!(
            matches!(
                a_plus_b,
                Ok(Value::String(ref str)) if str.as_ref() == "123test321"
            ),
            "{a_plus_b:?}"
        );
    }

    #[test]
    fn long_input() {
        let mut code = String::new();
        for i in 0..20 {
            writeln!(code, "fn fn{i}(b) {{ b + {i} }}").unwrap();
        }
        dbg!(&code);
        let mut vm = VM::default();
        let (_rest, tokens) = tokenize(&code).unwrap();
        let ast = dbg!(Parser::new(tokens.into_iter(), &mut vm.strings)
            .parse()
            .expect("Code to compile"));
        dbg!(ast);
    }

    #[test]
    fn function_declaration() {
        const CODE: &str = r#"fn add_one(b) {
            b + 1
        }
        add_one(3);
        fn sub(b, c) {
            b - c
        }
        sub(2, 1);
        add_one(sub(2, 1));
        "#;
        let mut vm = VM::default();
        let (_rest, tokens) = tokenize(CODE).unwrap();
        assert_eq!(_rest.fragment().trim(), "");
        let mut code = Parser::new(tokens.into_iter(), &mut vm.strings)
            .parse()
            .expect("Code to compile")
            .into_iter();

        let decl_add_one = vm.eval(&code.next().unwrap());
        assert!(
            matches!(decl_add_one, Ok(Value::None)),
            "Declaration of add_one failed: {decl_add_one:?}"
        );

        let fn_add_one = vm.globals.get(&Ident("add_one".to_owned().into()));
        assert!(
            matches!(fn_add_one, Some(Value::Function(_))),
            "add_one not a function: {fn_add_one:?}"
        );

        let call_add_one = vm.eval(&code.next().unwrap());
        assert!(
            matches!(call_add_one, Ok(Value::Int(4))),
            "Call add_one failed: {call_add_one:?}"
        );

        let decl_sub = vm.eval(&code.next().unwrap());
        assert!(
            matches!(decl_sub, Ok(Value::None)),
            "Declaration of sub failed: {decl_sub:?}"
        );

        let fn_sub = vm.globals.get(&Ident("sub".to_owned().into()));
        assert!(
            matches!(fn_sub, Some(Value::Function(_))),
            "sub not a function: {fn_sub:?}"
        );

        let call_sub = vm.eval(&code.next().unwrap());
        assert!(
            matches!(call_sub, Ok(Value::Int(1))),
            "Call add_one failed: {call_sub:?}"
        );

        let call_sub_with_add_one = vm.eval(&code.next().unwrap());
        assert!(
            matches!(call_sub_with_add_one, Ok(Value::Int(2))),
            "{call_sub_with_add_one:?}"
        );
    }
}
