use std::{
    collections::{BTreeMap, BTreeSet},
    rc::Rc,
    usize,
};

use anyhow::{anyhow, bail, Result};

mod ast;
mod tokenizer;

use ast::{Ast, BinaryOp, Block, Ident, Literal, UniaryOp};

#[derive(Debug, Default)]
pub struct VM {
    globals: BTreeMap<Ident, Value>,
    stack: Vec<BTreeMap<Ident, Value>>,
    return_register: Value,
    pub strings: BTreeSet<Rc<str>>,
}

#[derive(Debug, Default, Clone)]
pub enum Value {
    String(Rc<str>),
    Int(i64),
    Float(f64),
    Boolean(bool),
    Function(Rc<Function>),
    #[default]
    None,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Function(lhs), Self::Function(rhs)) => Rc::ptr_eq(lhs, rhs),
            (Self::Boolean(lhs), Self::Boolean(rhs)) => lhs == rhs,
            (Self::Float(lhs), Self::Float(rhs)) => lhs == rhs,
            (Self::Int(lhs), Self::Int(rhs)) => lhs == rhs,
            (Self::String(lhs), Self::String(rhs)) => lhs == rhs,
            (Self::None, Self::None) => true,
            _ => false,
        }
    }
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
            Literal::String(inner) => Value::String(inner.clone()),
            Literal::Int(inner) => Value::Int(*inner),
            Literal::Float(inner) => Value::Float(*inner),
            Literal::Boolean(inner) => Value::Boolean(*inner),
        }
    }
}

#[derive(Debug)]
pub enum ControlFlow {
    Return,
}

impl std::fmt::Display for ControlFlow {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)?;
        Ok(())
    }
}

impl std::error::Error for ControlFlow {}

impl VM {
    pub fn eval_file(&mut self, path: &std::path::Path) -> Result<Value> {
        let code = std::fs::read_to_string(path)?;
        let tokens = tokenizer::tokenize(code.as_ref())
            .map_err(|err| anyhow::Error::msg(err.to_string()))?;
        let mut parser = ast::Parser::new(tokens.into_iter(), &mut self.strings);
        if let Ok(path) = path.canonicalize() {
            parser.set_file(path.to_string_lossy().as_ref());
        }
        let ast = parser.parse()?;
        self.eval_iter(ast.iter())
    }

    pub fn eval_str(&mut self, code: &str) -> Result<Value> {
        let tokens = tokenizer::tokenize(code.as_ref())
            .map_err(|err| anyhow::Error::msg(err.to_string()))?;
        let mut parser = ast::Parser::new(tokens.into_iter(), &mut self.strings);
        let ast = parser.parse()?;
        self.eval_iter(ast.iter())
    }

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
                self.set_ident_value(ident, value)?;
                Value::None
            }
            Ast::Literal(literal) => literal.into(),
            Ast::Ident(ident) => self.get_ident_value(ident).cloned().unwrap_or(Value::None),
            Ast::UniaryOp(op, value) => self.eval(value)?.eval_uniary_op(op)?,
            Ast::BinaryOp(op, lhs, rhs) => self.eval(lhs)?.eval_binary_op(self.eval(rhs)?, op)?,
            Ast::VariableDecl { ident, value } => {
                if self.ident_declared(&ident) {
                    bail!("Variable {:?} already exists", ident)
                }
                match value {
                    Some(ast) => {
                        let value = self.eval(ast)?;
                        self.set_ident_value(ident, value)?;
                    }
                    None => {
                        self.set_ident_value(ident, Value::None)?;
                    }
                };
                Value::None
            }
            Ast::FunctionDecl { args, body } => Value::Function(Rc::new(Function {
                args: args.clone(),
                body: body.clone(),
            })),
            Ast::If(branches) => {
                let mut out = Value::None;
                for (predicate, block) in branches {
                    let predicate = self.eval(predicate)?.coerce_boolean();
                    if matches!(predicate, Value::Boolean(true)) {
                        out = self.eval_block(&block)?;
                    }
                }
                out
            }
            Ast::FunctionCall { ident, args } => {
                let Ok(Value::Function(function)) = self.get_ident_value(ident).cloned() else {
                    match self.call_builtin(ident, args) {
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
                let value = match self.eval_block(&function.body) {
                    Ok(value) => value,
                    Err(err) => match err.downcast_ref::<ControlFlow>() {
                        Some(ControlFlow::Return) => self.return_register.to_owned(),
                        _ => return Err(err),
                    },
                };
                self.stack.pop();
                value
            }
            Ast::Return(value) => {
                self.return_register = if let Some(value) = value {
                    self.eval(value)?
                } else {
                    Value::None
                };
                return Err(ControlFlow::Return.into());
            }
            Ast::Group(ast) => self.eval(ast)?,
            Ast::Block(block) => self.eval_block(block)?,
        })
    }

    fn eval_block(&mut self, block: &Block) -> Result<Value> {
        Ok(match self.eval_iter(block.content.iter()) {
            Ok(value) if block.implicit_return => value,
            Ok(_) => Value::None,
            Err(err) => return Err(err),
        })
    }

    fn set_ident_value(&mut self, ident: &Ident, value: Value) -> Result<Option<Value>> {
        let scope = self.stack.last_mut().unwrap_or(&mut self.globals);
        Ok(scope.insert(ident.clone(), value))
    }

    fn ident_declared(&self, ident: &Ident) -> bool {
        let scope = self.stack.last().unwrap_or(&self.globals);
        scope.contains_key(ident)
    }

    fn get_ident_value(&self, ident: &Ident) -> Result<&Value> {
        self.stack
            .iter()
            .find_map(|scope| scope.get(ident))
            .or_else(|| self.globals.get(ident))
            .ok_or(anyhow!("Undefined variable: {:?}", ident))
    }

    fn call_builtin(&mut self, ident: &Ident, args: &[Ast]) -> Result<Value> {
        let name = ident.0.as_ref();

        let exact_args =
            |n: usize| format!("{name} expected {n} arguments, recived {}", args.len());
        let atleast_args = |n: usize| {
            format!(
                "{name} expected at least {n} arguments, recived {}",
                args.len()
            )
        };

        Ok(match name {
            "dump" => match args {
                [] => {
                    dbg!(self);
                    Value::None
                }
                _ => bail!(exact_args(0)),
            },
            "print" => match args {
                [first, rest @ ..] => {
                    let value = self.eval(first)?;
                    let Value::String(str) = value.coerce_string() else {
                        unreachable!("Coerse string will always succeed")
                    };
                    print!("{str}");
                    for arg in rest {
                        let value = self.eval(arg)?;
                        let Value::String(str) = value.coerce_string() else {
                            unreachable!("Coerse string will always succeed")
                        };
                        print!(", {str}");
                    }
                    println!("");
                    Value::None
                }
                _ => bail!(atleast_args(1)),
            },
            name => bail!("No builtin named {name} exists"),
        })
    }
}

#[cfg(test)]
mod test {
    use std::fmt::Write;

    use super::*;

    fn setup(code: &str) -> (VM, Vec<Ast>) {
        let mut vm = VM::default();
        let tokens = crate::tokenizer::tokenize(code).unwrap();
        let ast = crate::ast::Parser::new(tokens.into_iter(), &mut vm.strings)
            .parse()
            .expect("Code to compile");
        (vm, ast)
    }

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
        let (mut vm, ast) = setup(CODE);
        let mut ast = ast.into_iter();

        let decl_a = vm.eval(&ast.next().unwrap());
        assert!(
            matches!(decl_a, Ok(Value::None)),
            "Declaration failed: {decl_a:?}"
        );

        let var_a = vm.eval(&ast.next().unwrap());
        assert!(
            matches!(var_a, Ok(Value::Int(123))),
            "a not defined: {var_a:?}"
        );

        let decl_b = vm.eval(&ast.next().unwrap());
        assert!(
            matches!(decl_b, Ok(Value::None)),
            "Declaration failed: {decl_b:?}"
        );

        let var_b = vm.eval(&ast.next().unwrap());
        assert!(
            matches!(
                var_b,
                Ok(Value::String(ref str)) if str.as_ref() == "test321"
            ),
            "b not defined: {var_b:?}"
        );

        let a_plus_b = vm.eval(&ast.next().unwrap());
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
        let (_vm, ast) = setup(&code);
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
        let (mut vm, ast) = setup(CODE);
        let mut ast = ast.into_iter();

        let decl_add_one = vm.eval(&ast.next().unwrap());
        assert!(
            matches!(decl_add_one, Ok(Value::None)),
            "Declaration of add_one failed: {decl_add_one:?}"
        );

        let fn_add_one = vm.globals.get(&Ident("add_one".to_owned().into()));
        assert!(
            matches!(fn_add_one, Some(Value::Function(_))),
            "add_one not a function: {fn_add_one:?}"
        );

        let call_add_one = vm.eval(&ast.next().unwrap());
        assert!(
            matches!(call_add_one, Ok(Value::Int(4))),
            "Call add_one failed: {call_add_one:?}"
        );

        let decl_sub = vm.eval(&ast.next().unwrap());
        assert!(
            matches!(decl_sub, Ok(Value::None)),
            "Declaration of sub failed: {decl_sub:?}"
        );

        let fn_sub = vm.globals.get(&Ident("sub".to_owned().into()));
        assert!(
            matches!(fn_sub, Some(Value::Function(_))),
            "sub not a function: {fn_sub:?}"
        );

        let call_sub = vm.eval(&ast.next().unwrap());
        assert!(
            matches!(call_sub, Ok(Value::Int(1))),
            "Call sub failed: {call_sub:?}"
        );

        let call_sub_with_add_one = vm.eval(&ast.next().unwrap());
        assert!(
            matches!(call_sub_with_add_one, Ok(Value::Int(2))),
            "{call_sub_with_add_one:?}"
        );
    }

    #[test]
    fn function_return() {
        const CODE: &str = r#"fn check(in) {
            if in == 3 {
                return true;
            } else if in > 3 {
                return false;
            }
            in
        }
        check(10);
        check(1);
        check(3);
        "#;
        let (mut vm, ast) = setup(CODE);
        let mut ast = ast.into_iter();

        let decl_check = vm.eval(&ast.next().unwrap());
        assert!(
            matches!(decl_check, Ok(Value::None)),
            "Declaration of check failed: {decl_check:?}"
        );

        let fn_check = vm.globals.get(&Ident("check".to_owned().into()));
        assert!(
            matches!(fn_check, Some(Value::Function(_))),
            "check not a function: {fn_check:?}"
        );

        let call_check_10 = vm.eval(&ast.next().unwrap());
        assert!(
            matches!(call_check_10, Ok(Value::Boolean(false))),
            "Call check with 10 failed: {call_check_10:?}"
        );

        let call_check_1 = vm.eval(&ast.next().unwrap());
        assert!(
            matches!(call_check_1, Ok(Value::Int(1))),
            "Call check with 1 failed: {call_check_1:?}"
        );

        let call_check_3 = vm.eval(&ast.next().unwrap());
        assert!(
            matches!(call_check_3, Ok(Value::Boolean(true))),
            "Call check with 3 failed: {call_check_3:?}"
        );
    }
}
