use std::{cell::RefCell, rc::Rc, usize};

use anyhow::{bail, Result};

mod ast;
mod compiler;
mod parser;
mod tokenizer;
mod value;

use ustr::{Ustr, UstrMap};
use value::Value;

use crate::compiler::Op;

#[derive(Debug, Default)]
pub struct VM {
    reg_accumulator: Value,
    reg_operand: Value,
    global_scope: UstrMap<Rc<RefCell<Value>>>,
}

impl VM {
    pub fn get_accumulator(&self) -> Value {
        self.reg_accumulator
    }

    pub fn get_ident_value(&self, ident: &Ustr) -> Option<Value> {
        self.global_scope
            .get(ident)
            .map(|val| val.as_ref().borrow().clone())
    }

    pub fn compile_str(code: &str) -> Result<Box<[Op]>> {
        let (_, asts) = parser::parse_code(code).map_err(|err| anyhow::anyhow!(err.to_string()))?;
        Ok(compiler::compile_program(&asts))
    }

    pub fn eval_ops(&mut self, ops: &[Op]) -> Result<()> {
        let mut program_counter = 0;
        while let Some(op) = ops.get(program_counter) {
            dbg!(program_counter, op);
            match op {
                Op::StoreVariable(ident) => {
                    let Some(var) = self.global_scope.get(ident) else {
                        bail!("Variable {} is not declared", ident)
                    };
                    var.replace(self.reg_accumulator);
                }
                Op::DeclareVariable(ident) => {
                    let var = Rc::new(RefCell::new(self.reg_accumulator));
                    self.global_scope.insert(*ident, var);
                }
                Op::LoadVariable(ident) => {
                    let Some(var) = self.get_ident_value(ident) else {
                        bail!("Variable {} is not declared", ident)
                    };
                    self.reg_accumulator = var;
                }
                Op::LoadImmediate(value) => self.reg_accumulator = *value,

                Op::StoreOperand => self.reg_operand = self.reg_accumulator,

                Op::Jump(location) => {
                    program_counter = *location;
                    continue;
                }
                Op::JumpIfTrue(location) => {
                    if self.reg_accumulator.is_truthy() {
                        program_counter = *location;
                        continue;
                    }
                }
                Op::JumpIfFalse(location) => {
                    if !self.reg_accumulator.is_truthy() {
                        program_counter = *location;
                        continue;
                    }
                }

                Op::UnaryOperation(operation) => {
                    self.reg_accumulator = self.reg_accumulator.eval_unary_op(operation)?;
                }
                Op::BinaryOperation(operation) => {
                    self.reg_accumulator = self
                        .reg_accumulator
                        .eval_binary_op(self.reg_operand, operation)?;
                }
            }
            program_counter += 1;
        }
        Ok(())
    }
}

#[cfg(test)]
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
}
