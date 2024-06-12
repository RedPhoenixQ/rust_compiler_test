use std::rc::Rc;

use anyhow::{bail, Result};

mod builtins;
mod bytecode;
mod compiler;
mod parser;
mod value;

use builtins::Builtin;
use bytecode::Op;
use compiler::Bundle;
use ustr::{Ustr, UstrMap};
use value::{Closure, Function, Value, Variable};

type Scope = UstrMap<Variable>;

#[derive(Debug)]
enum Block {
    Loop {
        label: Option<Ustr>,
        start_index: usize,
        end_index: usize,
    },
}

#[derive(Debug, Default)]
struct CallFrame {
    locals: Scope,
    eval_stack: Vec<Value>,
    block_stack: Vec<Block>,
}

#[derive(Debug)]
pub struct VM {
    call_stack: Vec<CallFrame>,
    global_scope: Scope,
    global_eval: Vec<Value>,
    global_block_stack: Vec<Block>,
    pub debug: bool,
}

impl VM {
    pub fn new(debug: bool) -> Self {
        VM {
            call_stack: Vec::new(),
            global_eval: Vec::new(),
            global_block_stack: Vec::new(),
            global_scope: builtins::get_builtins(),
            debug,
        }
    }

    pub fn eval_str(&mut self, input: &str) -> Result<Value> {
        let bundle = Self::compile_str(input)?;

        self.eval(&bundle.code)?;

        self.global_eval.pop().ok_or(anyhow::anyhow!(
            "Expected atleast one value in the eval stack"
        ))
    }

    pub fn compile_str(input: &str) -> Result<Bundle> {
        let ast = parser::parse_code(input).map_err(|e| {
            anyhow::anyhow!(
                "{}",
                e.map_locations(|span| {
                    let line_skip = span.location_line().saturating_sub(3);
                    let mut lines = input.lines().skip(line_skip as usize);

                    let mut string = String::new();
                    string.push('"');
                    let mut last_line_len = 0;
                    for _ in 0..(span.location_line() - line_skip) {
                        if let Some(line) = lines.next() {
                            string.push('\n');
                            string.push_str(line);
                            last_line_len = line.len();
                        }
                    }

                    let error_is_at_eof = span.location_offset() == input.len();

                    string.push('\n');
                    string.extend([' '].iter().cycle().take(if error_is_at_eof {
                        last_line_len
                    } else {
                        span.get_utf8_column() - 1
                    }));
                    string.push('^');

                    for line in lines.take(2) {
                        string.push('\n');
                        string.push_str(line);
                    }
                    string.push('\n');
                    string.push('"');

                    string
                })
            )
        })?;

        compiler::Compiler::default().compile(&ast)
    }

    pub fn eval(&mut self, ops: &[Op]) -> Result<Value> {
        let mut pc = 0;

        while let Some(op) = ops.get(pc) {
            if self.debug {
                println!("Running {}:{pc}: {op:?}", self.call_stack.len());
            }
            pc += 1;
            match op {
                Op::LoadFast(key) => {
                    let value = self
                        .call_stack
                        .last_mut()
                        .ok_or(anyhow::anyhow!("Expected atleast one callstack to exist"))?
                        .locals
                        .get(key)
                        .ok_or(anyhow::anyhow!("Expected {key} to exist in local scope"))?
                        .borrow()
                        .clone();
                    self.push_eval_stack(value);
                }
                Op::Load(ident) => {
                    let value = self.get_ident_value(ident)?;
                    self.push_eval_stack(value);
                }
                Op::LoadKey => {
                    let key = self.pop_eval_stack()?;
                    let store = self.pop_eval_stack()?;
                    self.push_eval_stack(store.get(key)?);
                }
                Op::LoadConst(value) => {
                    self.push_eval_stack(value.clone());
                }
                Op::DeclareVar(ident) => {
                    let value = self.pop_eval_stack()?;
                    let scope = if let Some(frame) = self.call_stack.last_mut() {
                        &mut frame.locals
                    } else {
                        &mut self.global_scope
                    };
                    if let Some(var) = scope.get_mut(ident) {
                        var.replace(value);
                    } else {
                        scope.insert(*ident, Rc::new(value.into()));
                    }
                }
                Op::StoreFast(ident) => {
                    let value = self.pop_eval_stack()?;
                    let scope = if let Some(frame) = self.call_stack.last_mut() {
                        &mut frame.locals
                    } else {
                        &mut self.global_scope
                    };
                    let Some(var) = scope.get_mut(ident) else {
                        bail!(
                            "Cannot assign to an undeclared variable, tried to assign to {ident}",
                        );
                    };
                    var.replace(value);
                }
                Op::Store(ident) => {
                    let value = self.pop_eval_stack()?;
                    let Some(var) = (match self
                        .call_stack
                        .iter_mut()
                        .rev()
                        .find_map(|frame| frame.locals.get(ident))
                    {
                        Some(var) => Some(var),
                        None => self.global_scope.get(ident),
                    }) else {
                        bail!(
                            "Cannot assign to an undeclared variable, tried to assign to {ident}",
                        );
                    };
                    var.replace(value);
                }
                Op::StoreKey => {
                    let key = self.pop_eval_stack()?;
                    let value = self.pop_eval_stack()?;
                    let mut store = self.pop_eval_stack()?;
                    store.set(key, value)?;
                }
                Op::Jump(jump) => {
                    assert_ne!(*jump, 0, "An invalid jump to 0 was present in the code");
                    pc += *jump;
                }
                Op::JumpBack(jump) => {
                    assert_ne!(*jump, 0, "An invalid jump to 0 was present in the code");
                    pc -= jump;
                }
                Op::JumpIfTrue(jump) => {
                    assert_ne!(*jump, 0, "An invalid jump to 0 was present in the code");
                    let value = self.pop_eval_stack()?;
                    if value.is_truthy() {
                        pc += *jump;
                    }
                }
                Op::JumpIfFalse(jump) => {
                    assert_ne!(*jump, 0, "An invalid jump to 0 was present in the code");
                    let value = self.pop_eval_stack()?;
                    if !value.is_truthy() {
                        pc += *jump
                    }
                }
                Op::BinaryOp(op) => {
                    let rhs = self.pop_eval_stack()?;
                    let lhs = self.pop_eval_stack()?;
                    let value = lhs.eval_binary_op(&rhs, op)?;
                    self.push_eval_stack(value);
                }
                Op::UnaryOp(op) => {
                    let value = self.pop_eval_stack()?;
                    let value = value.eval_unary_op(op)?;
                    self.push_eval_stack(value);
                }
                Op::PushBlock(block) => {
                    let block_stack = match self.call_stack.last_mut() {
                        Some(frame) => &mut frame.block_stack,
                        None => &mut self.global_block_stack,
                    };

                    let block = match block {
                        bytecode::BlockType::Loop { label, loop_length } => Block::Loop {
                            label: *label,
                            start_index: pc,
                            end_index: pc + loop_length,
                        },
                    };
                    block_stack.push(block);
                }
                Op::PopBlock => {
                    let block_stack = match self.call_stack.last_mut() {
                        Some(frame) => &mut frame.block_stack,
                        None => &mut self.global_block_stack,
                    };
                    block_stack.pop();
                }
                Op::MakeClosure(function) => {
                    let function = function.clone();
                    if function.foreign_idents.len() > 0 {
                        let mut scope = Scope::default();
                        for ident in function.foreign_idents.iter() {
                            if let Some(var) = match self
                                .call_stack
                                .iter()
                                .rev()
                                .find_map(|frame| frame.locals.get(ident).cloned())
                            {
                                Some(var) => Some(var),
                                None => self.global_scope.get(ident).cloned(),
                            } {
                                scope.insert(*ident, var);
                            };
                        }
                        self.push_eval_stack(Value::Closure(Closure { function, scope }.into()));
                    } else {
                        self.push_eval_stack(Value::Function(function));
                    }
                }
                Op::Call(number_of_arguments) => {
                    enum Call {
                        Builtin(Builtin),
                        Function {
                            function: Rc<Function>,
                            locals: Scope,
                        },
                    }
                    match match self.pop_eval_stack()? {
                        Value::BuiltInFunction(builtin) => Call::Builtin(builtin),
                        Value::Closure(closure) => Call::Function {
                            function: closure.function.clone(),
                            locals: closure.scope.clone(),
                        },
                        Value::Function(function) => Call::Function {
                            function,
                            locals: Scope::default(),
                        },
                        calling => bail!("Attempting to call a non function, called {:?}", calling),
                    } {
                        Call::Builtin(builtin) => {
                            let return_value = builtin.call(self, *number_of_arguments)?;
                            self.push_eval_stack(return_value);
                        }
                        Call::Function {
                            function,
                            mut locals,
                        } => {
                            locals.reserve(function.arguments.len());

                            for i in 0..function.arguments.len() {
                                let (name, default) = function
                                    .arguments
                                    .get(i)
                                    .expect("Function arguments to exist");

                                let argument = if i < *number_of_arguments {
                                    self.pop_eval_stack()?
                                } else {
                                    default.clone()
                                };
                                locals.insert(*name, Rc::new(argument.into()));
                            }

                            self.call_stack.push(CallFrame {
                                locals,
                                ..Default::default()
                            });
                            let return_value = self.eval(&function.code)?;
                            self.call_stack.pop();
                            self.push_eval_stack(return_value);
                        }
                    };
                }
                Op::Return => return Ok(self.pop_eval_stack().unwrap_or(Value::Undefined)),
                Op::Break(break_label) => {
                    let block_stack = if let Some(frame) = self.call_stack.last_mut() {
                        &mut frame.block_stack
                    } else {
                        &mut self.global_block_stack
                    };
                    let Some(end_index) = block_stack.iter().rev().find_map(|block| match block {
                        Block::Loop {
                            label, end_index, ..
                        } if break_label.is_none()
                            || matches!(label, Some(label) if label == &break_label.unwrap()) =>
                        {
                            Some(*end_index)
                        }
                        _ => None,
                    }) else {
                        bail!("Cannot break outside a loop")
                    };
                    pc = end_index;
                }
                Op::Continue(continue_label) => {
                    let block_stack = if let Some(frame) = self.call_stack.last_mut() {
                        &mut frame.block_stack
                    } else {
                        &mut self.global_block_stack
                    };
                    let Some(start_index) =
                        block_stack.iter().rev().find_map(|block| match block {
                            Block::Loop {
                                label, start_index, ..
                            } if continue_label.is_none()
                                || matches!(label, Some(label) if label == &continue_label.unwrap()) =>
                            {
                                Some(*start_index)
                            }
                            _ => None,
                        })
                    else {
                        bail!("Cannot continue outside a loop")
                    };
                    pc = start_index;
                }
            }
        }

        Ok(self.pop_eval_stack().unwrap_or(Value::Undefined))
    }

    #[allow(unused)]
    fn peek_eval_stack(&mut self) -> Option<&Value> {
        let eval_stack = if let Some(frame) = self.call_stack.last_mut() {
            &mut frame.eval_stack
        } else {
            &mut self.global_eval
        };
        eval_stack.last()
    }
    fn pop_eval_stack(&mut self) -> Result<Value> {
        let eval_stack = if let Some(frame) = self.call_stack.last_mut() {
            &mut frame.eval_stack
        } else {
            &mut self.global_eval
        };
        eval_stack
            .pop()
            .ok_or(anyhow::anyhow!("Eval stack to not be empty"))
    }
    fn push_eval_stack(&mut self, value: Value) {
        let eval_stack = if let Some(frame) = self.call_stack.last_mut() {
            &mut frame.eval_stack
        } else {
            &mut self.global_eval
        };
        eval_stack.push(value);
    }

    fn get_ident_value(&self, ident: &ustr::Ustr) -> Result<Value> {
        Ok(if let Some(var) = self
            .call_stack
            .iter()
            .rev()
            .find_map(|frame| frame.locals.get(ident))
        {
            Ok(var)
        } else {
            self.global_scope
                .get(ident)
                .ok_or(anyhow::anyhow!("{ident} is not defined in any scope"))
        }?
        .borrow()
        .clone())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn binary_operation() {
        let mut vm = VM::new(true);

        let bundle = VM::compile_str("1 + 2").unwrap();
        let value = vm.eval(&bundle.code).unwrap();

        assert_eq!(Value::Int(3), value);
    }

    #[test]
    fn basic_variables() {
        let mut vm = VM::new(true);

        let bundle = VM::compile_str(
            "let one = 1;
            let two = 2;
            let one_again = one;
            one = 3",
        )
        .unwrap();
        let _value = vm.eval(&bundle.code).unwrap();
        assert_eq!(Some(Value::Int(3)), vm.get_ident_value(&"one".into()).ok());
        assert_eq!(Some(Value::Int(2)), vm.get_ident_value(&"two".into()).ok());
        assert_eq!(
            Some(Value::Int(1)),
            vm.get_ident_value(&"one_again".into()).ok()
        );
    }

    #[test]
    fn basic_if() {
        let mut vm = VM::new(true);

        let bundle = VM::compile_str(
            r#"if (true) {
                "then_branch";
            }"#,
        )
        .unwrap();
        let if_true_no_else = vm.eval(&bundle.code).unwrap();
        assert_eq!(
            Value::String("then_branch".into()),
            if_true_no_else,
            "if_true_no_else"
        );

        let bundle = VM::compile_str(
            r#"if (false) {
                "then_branch";
            }"#,
        )
        .unwrap();
        let if_false_no_else = vm.eval(&bundle.code).unwrap();
        assert_ne!(
            Value::String("then_branch".into()),
            if_false_no_else,
            "if_false_no_else"
        );

        let bundle = VM::compile_str(
            r#"if (true) {
                "then_branch";
            } else {
                "else_branch";
            }"#,
        )
        .unwrap();
        let if_false_else = vm.eval(&bundle.code).unwrap();
        assert_eq!(
            Value::String("then_branch".into()),
            if_false_else,
            "if_false_else"
        );

        let bundle = VM::compile_str(
            r#"if (false) {
                "then_branch";
            } else {
                "else_branch";
            }"#,
        )
        .unwrap();
        let if_false_else = vm.eval(&bundle.code).unwrap();
        assert_eq!(
            Value::String("else_branch".into()),
            if_false_else,
            "if_false_else"
        );
    }

    #[test]
    fn basic_loops() {
        let mut vm = VM::new(true);

        let bundle = VM::compile_str(
            r#"
            let b = 1;
            let i = 0;
            while (i < 3) {
                i += 1;
                b *= 2;
            }
            b;
            "#,
        )
        .unwrap();
        let value = vm.eval(&bundle.code).unwrap();
        assert_eq!(Value::Int(8), value, "while 2^3 == 8");

        let bundle = VM::compile_str(
            r#"
            let i = 0;
            while (true) {
                i += 1;
                if (i > 5) {
                    break;
                }
            }
            i;
            "#,
        )
        .unwrap();
        let value = vm.eval(&bundle.code).unwrap();
        assert_eq!(Value::Int(6), value, "while break i > 5");

        let bundle = VM::compile_str(
            r#"
            let s = "";
            let i = 0;
            while (i < 3) {
                i += 1;
                if (i == 1) {
                    continue;
                }
                s += i;
            }
            s;
            "#,
        )
        .unwrap();
        let value = vm.eval(&bundle.code).unwrap();
        assert_eq!(
            Value::String("23".into()),
            value,
            "while continue to skip index 1"
        );

        let bundle = VM::compile_str(
            r#"
            let s = "";
            let i = 0;
            outer: while (i < 3) {
                i += 1;
                let x = 0;
                while (x < 3) {
                    x += 1;
                    s += x;
                    if (i == 2 && x == 2) {
                        break :outer;
                    }
                }
            }
            s;
            "#,
        )
        .unwrap();
        let value = vm.eval(&bundle.code).unwrap();
        assert_eq!(
            Value::String("12312".into()),
            value,
            "while break outer at inner index 2"
        );

        let bundle = VM::compile_str(
            r#"
            let s = "";
            let i = 0;
            outer: while (i < 3) {
                i += 1;
                let x = 0;
                while (x < 3) {
                    x += 1;
                    s += x;
                    if (i == 2 && x == 2) {
                        continue :outer;
                    }
                }
            }
            s;
            "#,
        )
        .unwrap();
        let value = vm.eval(&bundle.code).unwrap();
        assert_eq!(
            Value::String("12312123".into()),
            value,
            "while continue outer at inner index 2"
        );
    }
}
