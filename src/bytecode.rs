use std::rc::Rc;

use ustr::Ustr;

use crate::{
    parser::{BinaryOp, UnaryOp},
    value::{Function, Value},
};

#[derive(Debug)]
pub enum BlockType {
    Loop {
        label: Option<Ustr>,
        loop_length: usize,
    },
}

#[derive(Debug)]
pub enum Op {
    /// Load local variable from current call stack
    LoadFast(Ustr),
    /// Load local variable from outside scope
    Load(Ustr),

    /// Load a constant from the current const array in the callstack onto the stack
    // TODO: Crate a const array for each scope to make each Op smaller
    LoadConst(Value),

    /// Declare a variable to the local scope and store the topmost value on the stack
    DeclareVar(Ustr),

    /// Store local variable in the current call stack
    StoreFast(Ustr),
    /// Store variable to first scope that contains the name
    Store(Ustr),

    /// Move the program counter relative to the current intruction
    Jump(isize),
    /// Move the program counter relative to the current intruction, but only if the topmost value in the stack is "truthy"
    JumpIfTrue(isize),
    /// Move the program counter relative to the current intruction, but only if the topmost value in the stack is "falsy"
    JumpIfFalse(isize),

    /// Pops the first two items from the stack and performs the operation.
    /// lhs should be bellow rhs in the stack
    BinaryOp(BinaryOp),
    /// Pops the first item from the stack, performas the operation and pushes the result back
    UnaryOp(UnaryOp),

    PushBlock(BlockType),
    PopBlock,

    /// This will grab a reference to all the foreign_idents within the function and push the close to the stack.
    ///
    /// If the function does not reference any foreign_ident, a normal function will be pushed
    MakeClosure(Rc<Function>),

    /// Calls the function at the top of the stack with the specified number of arguments
    ///
    /// The first argument should be at the bottom of the stack (stack[-number_of_arguments])
    Call(usize),
    /// Pop the first item from the stack and return it
    Return,

    /// Pop the first item from the stack and return it
    Break(Option<Ustr>),
    /// Pop the first item from the stack and return it
    Continue(Option<Ustr>),
}
