use std::rc::Rc;

use ustr::Ustr;

use crate::{
    parser::{BinaryOp, UnaryOp},
    value::{function::Function, Value},
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
    /// Load the value from the storage at the top of the stack
    LoadAttribute(Ustr),
    /// Load the value from the key (stack[-1]) from the value at stack[-2]
    LoadKey,

    /// Load a constant from the current const array in the callstack onto the stack
    // TODO: Crate a const array for each scope to make each Op smaller
    LoadConst(Value),

    /// Declare a variable to the local scope and store the topmost value on the stack
    DeclareVar(Ustr),

    /// Store local variable in the current call stack
    StoreFast(Ustr),
    /// Store variable to first scope that contains the name
    Store(Ustr),
    /// Store a value (stack[-1]) into the attribute on the storage (stack[-2])
    StoreAttribute(Ustr),
    /// Store a value (stack[-1]) into a key (stack[-2]) on the storage (stack[-3])
    StoreKey,

    /// Duplicates the topmost item on the stack
    Duplicate,

    /// Push stack[-1] down in the stack to position stack[-i]
    PushDown(usize),

    /// Move the program counter relative to the current intruction
    Jump(usize),
    /// Move the program counter backwards relative to the current intruction
    JumpBack(usize),
    /// Move the program counter relative to the current intruction, but only if the topmost value in the stack is "truthy"
    JumpIfTrue(usize),
    /// Move the program counter relative to the current intruction, but only if the topmost value in the stack is "falsy"
    JumpIfFalse(usize),

    /// Pops the first two items from the stack and performs the operation.
    /// lhs should be bellow rhs in the stack
    BinaryOp(BinaryOp),
    /// Pops the first item from the stack, performas the operation and pushes the result back
    UnaryOp(UnaryOp),

    PushBlock(BlockType),
    PopBlock,

    /// Pops the number of values from the stack and creates an array of those items
    BuildArray(usize),
    /// Pops the number*2 of values from the stack and creates an object of those key-value pairs.
    /// The key should be above the value
    BuildObject(usize),

    /// This will grab a reference to all the foreign_idents within the function and push the close to the stack.
    ///
    /// If the function does not reference any foreign_ident, a normal function will be pushed
    MakeClosure(Rc<Function>),

    /// Calls the function at the top of the stack with the specified number of arguments
    ///
    /// The self value should be below the function being called (stack[-2])
    ///
    /// The leftmost argument should be at the top of the stack
    CallMethod(usize),

    /// Calls the function at the top of the stack with the specified number of arguments
    ///
    /// The leftmost argument should be at the top of the stack
    Call(usize),
    /// Pop the first item from the stack and return it
    Return,

    /// Pop the first item from the stack and return it
    Break(Option<Ustr>),
    /// Pop the first item from the stack and return it
    Continue(Option<Ustr>),
}
