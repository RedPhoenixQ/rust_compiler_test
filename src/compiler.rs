use anyhow::{Context, Result};
use ustr::Ustr;

use crate::{
    parser::{Ast, BinaryOp, Node, UnaryOp},
    value::Value,
};

pub type Block = Vec<Op>;

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

    /// Move the programcounter if accumulator is truthy
    JumpIfTrue(usize),
    /// Move the programcounter if accumulator is NOT truthy
    JumpIfFalse(usize),
    /// Move the programcounter
    Jump(usize),

    /// Performs the operation on the accumulator
    UnaryOperation(UnaryOp),
    /// Performs the operation between the operand register and the accumulator
    BinaryOperation(BinaryOp),
}

#[derive(Debug, Default)]
pub struct Compiler {
    /// A stack of indicies for where to insert continue and break jump indicies
    controlflow: Vec<Vec<(Controlflow, usize)>>,

    block: Vec<Op>,
}

#[derive(Debug)]
enum Controlflow {
    Continue,
    Break,
}

impl Compiler {
    pub fn compile_program(asts: &[Ast]) -> Result<Block> {
        let mut compiler = Self::default();
        for Ast { node, .. } in asts {
            compiler.compile_node(node)?;
        }
        Ok(compiler.block)
    }

    fn compile_node(&mut self, node: &Node) -> Result<()> {
        match node {
            Node::Ident(ident) => self.block.push(Op::LoadVariable(*ident)),
            Node::Literal(literal) => self.block.push(Op::LoadImmediate(*literal)),
            Node::If {
                branches,
                else_block,
            } => {
                for (branch_index, (predicate, body)) in branches.iter().enumerate() {
                    let should_handle_else_branch =
                        else_block.is_some() && branch_index == branches.len() - 1;

                    self.compile_node(&predicate.node)?;

                    // If the predicate is false, jump over the body
                    // The jump location will be changed after body length is known
                    let predicate_jump_index = self.block.len();
                    self.block.push(Op::JumpIfFalse(0));

                    // Compile the body of the predicate
                    let start_of_body_index = self.block.len();
                    for ast in body.iter() {
                        self.compile_node(&ast.node)?;
                    }

                    let else_jump_index = self.block.len();
                    if should_handle_else_branch {
                        // Skip the else body if we did not jump past it when the predicate was falsy
                        // The jump length will be set after the else body is compiled
                        self.block.push(Op::Jump(0));
                    }

                    // Set the jump instruction before the body to skip the body if the predicate is falsy
                    self.block[predicate_jump_index] = Op::JumpIfFalse(self.block.len());

                    if should_handle_else_branch {
                        if let Some(else_body) = else_block {
                            for ast in else_body.iter() {
                                self.compile_node(&ast.node)?;
                            }

                            // Point the jump instruction to after the body to skip the body if we fell through from the last predicate
                            // The jump will be skipped by the predicate jump if it was false
                            self.block[else_jump_index] = Op::Jump(self.block.len())
                        }
                    }
                }
            }
            Node::While { predicate, body } => {
                let start_of_loop_index = self.block.len();
                self.compile_node(&predicate.node)?;

                let predicate_jump_index = self.block.len();
                self.block.push(Op::JumpIfFalse(0));

                self.controlflow.push(Vec::new());
                for ast in body.iter() {
                    self.compile_node(&ast.node)?;
                }

                self.block.push(Op::Jump(start_of_loop_index));

                // Set predicate jump to after the while loop
                self.block[predicate_jump_index] = Op::JumpIfFalse(self.block.len());

                for (control, index) in self
                    .controlflow
                    .pop()
                    .expect("Scope for this loop to exist")
                {
                    self.block[index] = match control {
                        Controlflow::Break => Op::Jump(self.block.len()),
                        Controlflow::Continue => Op::Jump(start_of_loop_index),
                    };
                }
            }
            Node::Break => {
                self.controlflow
                    .first_mut()
                    .context("Controlflow stack to be created by loop for break node")?
                    .push((Controlflow::Break, self.block.len()));
                self.block.push(Op::Jump(0));
            }
            Node::Continue => {
                self.controlflow
                    .first_mut()
                    .context("Controlflow stack to be created by loop for continue node")?
                    .push((Controlflow::Continue, self.block.len()));
                self.block.push(Op::Jump(0));
            }
            Node::VariableDeclaration { ident, value } => {
                if let Some(ast) = value {
                    self.compile_node(&ast.node)?;
                }
                self.block.push(Op::DeclareVariable(*ident));
            }
            Node::FunctionDeclaration { .. } => todo!("FunctionDeclaration"),
            Node::Assignment { ident, value } => {
                self.compile_node(&value.node)?;
                self.block.push(Op::StoreVariable(*ident));
            }
            Node::Return(_) => todo!("Return"),
            Node::UnaryOp(operation, value) => {
                self.compile_node(&value.node)?;
                self.block.push(Op::UnaryOperation(*operation));
            }
            Node::BinaryOp(operation, lhs, rhs) => {
                self.compile_node(&lhs.node)?;
                self.block.push(Op::StoreOperand);
                self.compile_node(&rhs.node)?;
                self.block.push(Op::BinaryOperation(*operation));
            }
            Node::FunctionCall { calling, arguments } => todo!(),
            Node::ClosureDeclaration { arguments, body } => todo!(),
        }
        Ok(())
    }
}
