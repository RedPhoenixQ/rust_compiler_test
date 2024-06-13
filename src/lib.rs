use std::rc::Rc;

use anyhow::{bail, Result};

mod builtins;
mod bytecode;
mod compiler;
mod parser;
mod value;

use builtins::Builtin;
use bytecode::{BlockType, Op};
use compiler::Bundle;
use ustr::{Ustr, UstrMap};
use value::{
    array::{Array, ArrayMethod},
    function::{Closure, Function},
    object::{Object, ObjectMethod},
    Value, Variable,
};

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
    /// Should always contain atleast one frame, the global frame
    call_stack: Vec<CallFrame>,
    pub debug: bool,
}

impl VM {
    pub fn new(debug: bool) -> Self {
        VM {
            call_stack: Vec::from([CallFrame::default()]),
            debug,
        }
    }

    pub fn eval_str(&mut self, input: &str) -> Result<Value> {
        let bundle = Self::compile_str(input)?;

        self.eval(&bundle.code)?;

        self.current_frame().eval_stack.pop().ok_or(anyhow::anyhow!(
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

        compiler::Compiler::new().compile(&ast)
    }

    pub fn eval(&mut self, ops: &[Op]) -> Result<Value> {
        let mut pc = 0;

        while let Some(op) = ops.get(pc) {
            if self.debug {
                println!("Running {}:{pc}: {op:?}", self.call_stack.len());
            }
            pc += 1;
            match op {
                Op::Load(ident) => {
                    let value = self.get_ident_value(ident)?;
                    self.push_eval_stack(value);
                }
                Op::LoadAttribute(attribute) => {
                    let store = self.pop_eval_stack()?;
                    self.push_eval_stack(store.get(Value::String(*attribute))?);
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
                    let frame = self.current_frame();
                    if let Some(var) = frame.locals.get_mut(ident) {
                        var.replace(value);
                    } else {
                        frame.locals.insert(*ident, value.into());
                    }
                }
                Op::Store(ident) => {
                    let value = self.pop_eval_stack()?;
                    let Some(var) = self
                        .call_stack
                        .iter_mut()
                        .rev()
                        .find_map(|frame| frame.locals.get(ident))
                    else {
                        bail!(
                            "Cannot assign to an undeclared variable, tried to assign to {ident}",
                        );
                    };
                    var.replace(value);
                }
                Op::StoreAttribute(attribute) => {
                    let value = self.pop_eval_stack()?;
                    let mut store = self.pop_eval_stack()?;
                    store.set(Value::String(*attribute), value)?;
                }
                Op::StoreKey => {
                    let value = self.pop_eval_stack()?;
                    let key = self.pop_eval_stack()?;
                    let mut store = self.pop_eval_stack()?;
                    store.set(key, value)?;
                }
                Op::Duplicate => {
                    let value = self
                        .peek_eval_stack()
                        .ok_or(anyhow::anyhow!(
                            "Stack should contain atleast one item to duplicate"
                        ))?
                        .clone();
                    self.push_eval_stack(value);
                }
                Op::PushDown(down) => {
                    let value = self.pop_eval_stack()?;
                    let frame = self.current_frame();
                    frame
                        .eval_stack
                        .insert(frame.eval_stack.len() - down, value);
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
                    let frame = self.current_frame();
                    let block = match block {
                        BlockType::Loop { label, loop_length } => Block::Loop {
                            label: *label,
                            start_index: pc,
                            end_index: pc + loop_length,
                        },
                    };
                    frame.block_stack.push(block);
                }
                Op::PopBlock => {
                    let frame = self.current_frame();
                    frame.block_stack.pop();
                }
                Op::BuildArray(number_of_elements) => {
                    let mut vec = Vec::with_capacity(*number_of_elements);
                    for _ in 0..*number_of_elements {
                        vec.push(self.pop_eval_stack()?);
                    }
                    self.push_eval_stack(Array(vec).into());
                }
                Op::BuildObject(number_of_elements) => {
                    let mut map = UstrMap::default();
                    for _ in 0..*number_of_elements {
                        let key = match self.pop_eval_stack()? {
                            Value::String(key) => key,
                            value => bail!("The key was not a string, recived {value:?}"),
                        };
                        let value = self.pop_eval_stack()?;
                        map.insert(key, value);
                    }
                    self.push_eval_stack(Object(map).into());
                }
                Op::MakeClosure(function) => {
                    let function = function.clone();
                    if !function.foreign_idents.is_empty() {
                        let mut scope = Scope::default();
                        for ident in function.foreign_idents.iter() {
                            if ident.as_str() == "self" {
                                // Skip ident lookup for 'self' which is used to reference the object of a closure
                                continue;
                            }
                            if let Some(var) = self
                                .call_stack
                                .iter()
                                .rev()
                                .find_map(|frame| frame.locals.get(ident).cloned())
                            {
                                scope.insert(*ident, var);
                            };
                        }
                        self.push_eval_stack(Closure { function, scope }.into());
                    } else {
                        self.push_eval_stack(function.into());
                    }
                }
                op @ Op::Call(number_of_arguments) | op @ Op::CallMethod(number_of_arguments) => {
                    enum Call {
                        Builtin(Builtin),
                        ArrayMethod(ArrayMethod),
                        ObjectMethod(ObjectMethod),
                        Function {
                            function: Rc<Function>,
                            locals: Scope,
                        },
                    }

                    impl TryFrom<Value> for Call {
                        type Error = anyhow::Error;
                        fn try_from(value: Value) -> std::result::Result<Self, Self::Error> {
                            Ok(match value {
                                Value::BuiltInFunction(builtin) => Call::Builtin(builtin),
                                Value::ArrayMethod(method) => Call::ArrayMethod(method),
                                Value::ObjectMethod(method) => Call::ObjectMethod(method),
                                Value::Closure(closure) => Call::Function {
                                    function: closure.function.clone(),
                                    locals: closure.scope.clone(),
                                },
                                Value::Function(function) => Call::Function {
                                    function,
                                    locals: Scope::default(),
                                },
                                calling => {
                                    bail!("Attempting to call a non function, called {:?}", calling)
                                }
                            })
                        }
                    }

                    match self.pop_eval_stack()?.try_into()? {
                        Call::Builtin(builtin) => {
                            if matches!(op, Op::CallMethod(_)) {
                                let _ignored_self_value = self.pop_eval_stack()?;
                            }
                            let return_value = self.call_builtin(builtin, *number_of_arguments)?;
                            self.push_eval_stack(return_value);
                        }
                        Call::ArrayMethod(method) => {
                            if !matches!(op, Op::CallMethod(_)) {
                                bail!("{method:?} not called as a method")
                            }
                            let array = match self.pop_eval_stack()? {
                                Value::Array(array) => array,
                                value => bail!("{method:?} called on {value:?}"),
                            };
                            let mut arguments = Vec::with_capacity(*number_of_arguments);
                            for _ in 0..*number_of_arguments {
                                arguments.push(self.pop_eval_stack()?);
                            }
                            let return_value = array.borrow_mut().call(method, arguments)?;
                            self.push_eval_stack(return_value);
                        }
                        Call::ObjectMethod(method) => {
                            if !matches!(op, Op::CallMethod(_)) {
                                bail!("{method:?} not called as a method")
                            }
                            let object = match self.pop_eval_stack()? {
                                Value::Object(object) => object,
                                value => bail!("{method:?} called on {value:?}"),
                            };
                            let mut arguments = Vec::with_capacity(*number_of_arguments);
                            for _ in 0..*number_of_arguments {
                                arguments.push(self.pop_eval_stack()?);
                            }
                            let return_value = object.borrow_mut().call(method, arguments)?;
                            self.push_eval_stack(return_value);
                        }
                        Call::Function {
                            function,
                            mut locals,
                        } => {
                            locals.reserve(function.arguments.len());

                            if matches!(op, Op::CallMethod(_)) {
                                locals.insert("self".into(), self.pop_eval_stack()?.into());
                            }

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
                                locals.insert(*name, argument.into());
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
                    let frame = self.current_frame();

                    let Some(end_index) = frame.block_stack.iter().rev().find_map(|block| {
                        match block {
                            Block::Loop {
                                label, end_index, ..
                            } if break_label.is_none()
                                || matches!(label, Some(label) if label == &break_label.unwrap()) =>
                            {
                                Some(*end_index)
                            }
                            _ => None,
                        }
                    }) else {
                        bail!("Cannot break outside a loop")
                    };
                    pc = end_index;
                }
                Op::Continue(continue_label) => {
                    let frame = self.current_frame();
                    let Some(start_index) =
                        frame.block_stack.iter().rev().find_map(|block| match block {
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

    fn current_frame(&mut self) -> &mut CallFrame {
        self.call_stack
            .last_mut()
            .expect("Atleast one CallFrame to exist on the call_stack")
    }

    fn peek_eval_stack(&mut self) -> Option<&Value> {
        let frame = self.current_frame();
        frame.eval_stack.last()
    }
    fn pop_eval_stack(&mut self) -> Result<Value> {
        let frame = self.current_frame();
        frame
            .eval_stack
            .pop()
            .ok_or(anyhow::anyhow!("Eval stack to not be empty"))
    }
    fn push_eval_stack(&mut self, value: Value) {
        let frame = self.current_frame();
        frame.eval_stack.push(value);
    }

    fn get_ident_value(&self, ident: &ustr::Ustr) -> Result<Value> {
        Ok(
            if let Some(var) = self
                .call_stack
                .iter()
                .rev()
                .find_map(|frame| frame.locals.get(ident))
            {
                var.borrow().clone()
            } else if let Ok(builtin) = ident.as_str().try_into() {
                Value::BuiltInFunction(builtin)
            } else {
                bail!("{ident} could not be found in any scope")
            },
        )
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

    #[test]
    fn builtin_functions() {
        let mut vm = VM::new(true);

        let bundle = VM::compile_str(r#"print"#).unwrap();
        assert!(matches!(
            vm.eval(&bundle.code),
            Ok(Value::BuiltInFunction(Builtin::Print))
        ));
        let bundle = VM::compile_str(r#"let print = 0; print"#).unwrap();
        assert!(matches!(vm.eval(&bundle.code), Ok(Value::Int(0))));
    }
}
