use std::{cell::RefCell, rc::Rc};

use anyhow::Result;

mod bytecode;
mod compiler;
mod parser;
mod value;

use bytecode::Op;
use compiler::Bundle;
// use compiler::{Bundle, Compiler};
use ustr::UstrMap;
use value::Value;

type Scope = UstrMap<Rc<RefCell<Value>>>;

#[derive(Debug)]
struct CallFrame {
    locals: Scope,
    eval_stack: Vec<Value>,
}

#[derive(Debug, Default)]
pub struct VM {
    call_stack: Vec<CallFrame>,
    global_scope: Scope,
    global_eval: Vec<Value>,
}

impl VM {
    pub fn eval_str(&mut self, input: &str) -> Result<Value> {
        let bundle = Self::compile_str(input)?;

        self.eval(&bundle.code)?;

        self.global_eval.pop().ok_or(anyhow::anyhow!(
            "Expected atleast one value in the eval stack"
        ))
    }

    pub fn compile_str(input: &str) -> Result<Bundle> {
        let (_, ast) = parser::parse_code(input).map_err(|e| anyhow::anyhow!("{e:?}"))?;

        compiler::Compiler::default().compile(&ast)
    }

    pub fn eval(&mut self, ops: &[Op]) -> Result<Value> {
        let mut pc = 0;

        while let Some(op) = ops.get(pc) {
            match op {
                Op::LoadFast(key) => {
                    let var = self
                        .call_stack
                        .last_mut()
                        .ok_or(anyhow::anyhow!("Expected atleast one callstack to exist"))?
                        .locals
                        .get(key)
                        .ok_or(anyhow::anyhow!("Expected {key} to exist in local scope"))?
                        .clone();
                    self.push_eval_stack(Value::Variable(var));
                }
                Op::Load(ident) => {
                    let value = if let Some(var) = self
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
                    .as_ref()
                    .borrow()
                    .clone();

                    self.push_eval_stack(value)
                }
                Op::LoadConst(value) => {
                    self.push_eval_stack(value.clone());
                }
                Op::StoreFast(ident) => {
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
                Op::Store(ident) => {
                    let value = self.pop_eval_stack()?;
                    if let Some(var) = self
                        .call_stack
                        .iter_mut()
                        .rev()
                        .find_map(|frame| frame.locals.get(ident))
                    {
                        var.replace(value);
                    } else {
                        if let Some(var) = self.global_scope.get_mut(ident) {
                            var.replace(value);
                        } else {
                            self.global_scope.insert(*ident, Rc::new(value.into()));
                        }
                    };
                }
                Op::Jump(jump) => {
                    assert_ne!(*jump, 0, "An invalid jump to 0 was present in the code");
                    if jump.is_positive() {
                        pc += *jump as usize
                    } else {
                        pc -= jump.abs() as usize
                    }
                    continue;
                }
                Op::JumpIfTrue(jump) => {
                    assert_ne!(*jump, 0, "An invalid jump to 0 was present in the code");
                    let value = self.pop_eval_stack()?;
                    if value.is_truthy() {
                        if jump.is_positive() {
                            pc += *jump as usize
                        } else {
                            pc -= jump.abs() as usize
                        }
                        continue;
                    }
                }
                Op::JumpIfFalse(jump) => {
                    assert_ne!(*jump, 0, "An invalid jump to 0 was present in the code");
                    let value = self.pop_eval_stack()?;
                    if !value.is_truthy() {
                        if jump.is_positive() {
                            pc += *jump as usize
                        } else {
                            pc -= jump.abs() as usize
                        }
                        continue;
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
                Op::Call(_) => todo!(),
                Op::Return => todo!(),
            }

            pc += 1;
        }

        Ok(self.pop_eval_stack().unwrap_or(Value::Undefined))
    }

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
}

#[cfg(test)]
#[cfg(never)]
mod test {
    use super::*;

    #[test]
    fn binary_operation() {
        let mut vm = VM::default();

        let ops = VM::compile_str("1 + 2").unwrap();
        vm.eval_ops(&ops).unwrap();

        assert_eq!(Value::Int(3), vm.get_accumulator())
    }

    #[test]
    fn basic_variables() {
        let mut vm = VM::default();

        let ops = VM::compile_str(
            "let one = 1;
            let two = 2;
            let one_again = one;
            one = 3",
        )
        .unwrap();
        vm.eval_ops(&ops).unwrap();
        assert_eq!(Some(Value::Int(3)), vm.get_ident_value(&"one".into()));
        assert_eq!(Some(Value::Int(2)), vm.get_ident_value(&"two".into()));
        assert_eq!(Some(Value::Int(1)), vm.get_ident_value(&"one_again".into()));
    }

    #[test]
    fn basic_if() {
        let mut vm = VM::default();

        let ops = VM::compile_str(
            r#"if (true) {
                "then_branch";
            }"#,
        )
        .unwrap();
        vm.eval_ops(&ops).unwrap();
        assert_eq!(
            Value::String("then_branch".into()),
            vm.get_accumulator(),
            "if_true_no_else"
        );

        let ops = VM::compile_str(
            r#"if (false) {
                "then_branch";
            }"#,
        )
        .unwrap();
        vm.eval_ops(&ops).unwrap();
        assert_ne!(
            Value::String("then_branch".into()),
            vm.get_accumulator(),
            "if_false_no_else"
        );

        let ops = VM::compile_str(
            r#"if (true) {
                "then_branch";
            } else {
                "else_branch";
            }"#,
        )
        .unwrap();
        vm.eval_ops(&ops).unwrap();
        assert_eq!(
            Value::String("then_branch".into()),
            vm.get_accumulator(),
            "if_false_else"
        );

        let ops = VM::compile_str(
            r#"if (false) {
                "then_branch";
            } else {
                "else_branch";
            }"#,
        )
        .unwrap();
        vm.eval_ops(&ops).unwrap();
        assert_eq!(
            Value::String("else_branch".into()),
            vm.get_accumulator(),
            "if_false_else"
        );
    }

    #[test]
    fn basic_loops() {
        let mut vm = VM::default();

        let ops = VM::compile_str(
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
        vm.eval_ops(&ops).unwrap();
        assert_eq!(Value::Int(8), vm.get_accumulator(), "while 2^3 == 8");

        let ops = VM::compile_str(
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
        vm.eval_ops(&ops).unwrap();
        assert_eq!(Value::Int(6), vm.get_accumulator(), "while break i > 5");

        let ops = VM::compile_str(
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
        vm.eval_ops(&ops).unwrap();
        assert_eq!(
            Value::String("23".into()),
            vm.get_accumulator(),
            "while continue to skip index 1"
        );
    }
}
