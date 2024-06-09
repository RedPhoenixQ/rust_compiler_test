use ustr::Ustr;

use crate::{
    parser::{BinaryOp, UnaryOp},
    value::Value,
};

#[derive(Debug)]
pub enum Op {
    /// Load local variable from current call stack
    LoadFast(Ustr),
    /// Load local variable from outside scope
    Load(Ustr),

    /// Load a constant from the current const array in the callstack onto the stack
    // TODO: Crate a const array for each scope to make each Op smaller
    LoadConst(Value),

    /// Store local variable in the current call stack
    StoreFast(Ustr),
    /// Store variable to first scope that contains the name
    Store(Ustr),

    /// Pops the first two items from the stack and performs the operation.
    /// lhs should be bellow rhs in the stack
    BinaryOp(BinaryOp),
    /// Pops the first item from the stack, performas the operation and pushes the result back
    UnaryOp(UnaryOp),

    /// Calls the function at the top of the stack with the specified number of arguments
    ///
    /// The first argument should be at the bottom of the stack (stack[-number_of_arguments])
    Call(usize),
    /// Pop the first item from the stack and return it
    Return,
}
