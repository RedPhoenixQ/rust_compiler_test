use anyhow::{bail, Result};

use crate::{
    bytecode::Op,
    parser::{Ast, Node},
    value::Value,
};

#[derive(Debug)]
pub struct Bundle {
    pub consts: Vec<Value>,
    pub code: Vec<Op>,
}

#[derive(Debug, Default)]
pub struct Compiler {
    consts: Vec<Value>,
    code: Vec<Op>,
}

impl Compiler {
    pub fn compile(mut self, asts: &[Ast]) -> Result<Bundle> {
        for ast in asts {
            self.compile_node(&ast.node)?;
        }
        Ok(Bundle {
            code: self.code,
            consts: self.consts,
        })
    }

    fn compile_node(&mut self, node: &Node) -> Result<()> {
        match node {
            Node::VariableDeclaration { ident, value } => {
                if let Some(value) = value {
                    self.compile_node(&value.node)?;
                    self.code.push(Op::StoreFast(*ident))
                }
            }
            Node::Ident(ident) => self.code.push(Op::Load(*ident)),
            Node::Literal(value) => {
                // let index = match self
                //     .consts
                //     .iter()
                //     .enumerate()
                //     .find_map(|(i, v)| (v == value).then_some(i))
                // {
                //     Some(i) => i,
                //     None => {
                //         self.consts.push(value.clone());
                //         self.consts.len() - 1
                //     }
                // };
                self.code.push(Op::LoadConst(value.clone()))
            }
            Node::If {
                branches,
                else_block,
            } => {
                for (i, (predicate, body)) in branches.iter().enumerate() {
                    let is_final_predicate = i == branches.len() - 1;

                    self.compile_node(&predicate.node)?;
                    self.code.push(Op::JumpIfFalse(0));
                    let start_of_body_index = self.code.len();

                    for ast in body {
                        self.compile_node(&ast.node)?;
                    }

                    if is_final_predicate {
                        // Jump to skip final else block
                        self.code.push(Op::Jump(0))
                    }

                    let end_of_body_index = self.code.len();

                    let Some(Op::JumpIfFalse(ref mut jump)) =
                        self.code.get_mut(start_of_body_index - 1)
                    else {
                        bail!("Jump operation was not at the expected index")
                    };
                    *jump = end_of_body_index as isize - start_of_body_index as isize;

                    if is_final_predicate {
                        if let Some(else_block) = else_block {
                            for ast in else_block {
                                self.compile_node(&ast.node)?;
                            }
                            let end_of_else_block = self.code.len();

                            let Some(Op::Jump(ref mut jump)) =
                                self.code.get_mut(end_of_body_index - 1)
                            else {
                                bail!("Jump operation was not at the expected index")
                            };
                            *jump = end_of_else_block as isize - (end_of_body_index as isize);
                        }
                    }
                }
            }
            Node::While { predicate, body } => todo!(),
            Node::FunctionDeclaration {
                ident,
                arguments,
                body,
            } => todo!(),
            Node::ClosureDeclaration { arguments, body } => todo!(),
            Node::FunctionCall { calling, arguments } => todo!(),
            Node::Assignment { ident, value } => {
                self.compile_node(&value.node)?;
                self.code.push(Op::Store(*ident))
            }
            Node::Return(_) => todo!(),
            Node::Break => todo!(),
            Node::Continue => todo!(),
            Node::UnaryOp(op, ast) => {
                self.compile_node(&ast.node)?;
                self.code.push(Op::UnaryOp(*op))
            }
            Node::BinaryOp(op, lhs, rhs) => {
                self.compile_node(&lhs.node)?;
                self.compile_node(&rhs.node)?;
                self.code.push(Op::BinaryOp(*op))
            }
        }

        Ok(())
    }
}
