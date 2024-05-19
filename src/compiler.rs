use ustr::Ustr;

use crate::{
    parser::{Ast, BinaryOp, Node, UnaryOp},
    value::Value,
};

pub type Block = Box<[Op]>;

#[derive(Debug)]
pub enum Op {
    /// Store accumulator into already existing identifier
    StoreVariable(Ustr),
    /// Declare a new variable and store accumulator into it
    DeclareVariable(Ustr),
    /// Store identifier into accumulator
    LoadVariable(Ustr),
    /// Load value into accumulator
    LoadImmediate(Value),

    /// Store accumulator into the operand register
    StoreOperand,

    /// Execute the block if accumulator is truthy
    JumpIfTrue(isize),
    /// Execute the block if accumulator is NOT truthy
    JumpIfFalse(isize),
    Jump(isize),

    /// Performs the operation on the accumulator
    UnaryOperation(UnaryOp),
    /// Performs the operation between the operand register and the accumulator
    BinaryOperation(BinaryOp),
}

pub fn compile_program(asts: &[Ast]) -> Block {
    let mut block = Vec::with_capacity(asts.len());

    for Ast { node, .. } in asts {
        compile_node(&mut block, node);
    }

    block.into_boxed_slice()
}

fn compile_node(mut block: &mut Vec<Op>, node: &Node) {
    match node {
        Node::Ident(ident) => block.push(Op::LoadVariable(*ident)),
        Node::Literal(literal) => block.push(Op::LoadImmediate(*literal)),
        Node::If {
            branches,
            else_block,
        } => {
            for (branch_index, (predicate, body)) in branches.iter().enumerate() {
                let should_handle_else_branch =
                    else_block.is_some() && branch_index == branches.len() - 1;

                compile_node(&mut block, &predicate.node);

                // If the predicate is false, jump over the body
                // The jump length will be changed after body length is known
                let predicate_jump_index = block.len();
                block.push(Op::JumpIfFalse(0));

                // Compile the body of the predicate
                let start_of_body_index = block.len();
                for ast in body.iter() {
                    compile_node(&mut block, &ast.node);
                }

                let else_jump_index = block.len();
                if should_handle_else_branch {
                    // Jump over else body if we did not jump past it if the predicate was falsy
                    // The jump length will be set after the else body is complied
                    block.push(Op::Jump(0));
                }

                let body_len = (block.len() as isize) - (start_of_body_index as isize);
                assert_ne!(body_len, 0);
                // Set the jump instruction before the body to skip the body if the predicate is falsy
                block[predicate_jump_index] = Op::JumpIfFalse(body_len);

                if should_handle_else_branch {
                    if let Some(else_body) = else_block {
                        let start_of_else_body_index = block.len();
                        for ast in else_body.iter() {
                            compile_node(&mut block, &ast.node);
                        }

                        let else_and_then_body_len = body_len
                            + ((block.len() as isize) - (start_of_else_body_index as isize));
                        assert_ne!(else_and_then_body_len, 0);
                        // Set the jump instruction before the body to skip the body,
                        // unless we jumped past the jump inctruction if the predicate was falsy
                        block[else_jump_index] = Op::Jump(else_and_then_body_len)
                    }
                }
            }
        }
        Node::VariableDeclaration { ident, value } => {
            if let Some(ast) = value {
                compile_node(&mut block, &ast.node);
            }
            block.push(Op::DeclareVariable(*ident));
        }
        Node::FunctionDeclaration { .. } => todo!("FunctionDeclaration"),
        Node::Assignment { ident, value } => {
            compile_node(&mut block, &value.node);
            block.push(Op::StoreVariable(*ident));
        }
        Node::Return(_) => todo!("Return"),
        Node::UnaryOp(operation, value) => {
            compile_node(&mut block, &value.node);
            block.push(Op::UnaryOperation(*operation));
        }
        Node::BinaryOp(operation, lhs, rhs) => {
            compile_node(&mut block, &lhs.node);
            block.push(Op::StoreOperand);
            compile_node(&mut block, &rhs.node);
            block.push(Op::BinaryOperation(*operation));
        }
    }
}
