use anyhow::{bail, Result};
use ustr::UstrSet;

use crate::{
    bytecode::{BlockType, Op},
    parser::{Ast, Node},
    value::{Function, Value},
    Scope,
};

#[derive(Debug)]
pub struct Bundle {
    pub consts: Vec<Value>,
    pub code: Vec<Op>,
    pub foreign_idents: UstrSet,
}

#[derive(Debug, Default)]
pub struct Compiler {
    consts: Vec<Value>,
    code: Vec<Op>,
    declared_idents: UstrSet,
    foreign_idents: UstrSet,
}

impl Compiler {
    pub fn compile(mut self, asts: &[Ast]) -> Result<Bundle> {
        for ast in asts {
            self.compile_node(&ast.node)?;
        }
        Ok(Bundle {
            code: self.code,
            consts: self.consts,
            foreign_idents: self.foreign_idents,
        })
    }

    fn compile_node(&mut self, node: &Node) -> Result<()> {
        match node {
            Node::VariableDeclaration { ident, value } => {
                if let Some(value) = value {
                    self.compile_node(&value.node)?;
                } else {
                    self.code.push(Op::LoadConst(Value::Undefined))
                }
                self.declared_idents.insert(*ident);
                self.code.push(Op::DeclareVar(*ident))
            }
            Node::Ident(ident) => {
                if !self.declared_idents.contains(ident) {
                    self.foreign_idents.insert(*ident);
                }
                // if self.declared_idents.contains(ident) {
                //     self.code.push(Op::LoadFast(*ident));
                // } else {
                //     self.code.push(Op::Load(*ident));
                // }
                self.code.push(Op::Load(*ident));
            }
            Node::Literal(value) => self.code.push(Op::LoadConst(value.clone())),
            Node::If {
                branches,
                else_block,
            } => {
                for (i, (predicate, body)) in branches.iter().enumerate() {
                    let is_final_predicate = i == branches.len() - 1;

                    self.compile_node(&predicate.node)?;
                    let start_of_body_index = self.code.len();
                    self.code.push(Op::JumpIfFalse(0));

                    for ast in body {
                        self.compile_node(&ast.node)?;
                    }

                    if is_final_predicate && else_block.is_some() {
                        // Jump to skip final else block
                        self.code.push(Op::Jump(0))
                    }

                    let end_of_body_index = self.code.len();

                    let Some(Op::JumpIfFalse(ref mut jump)) =
                        self.code.get_mut(start_of_body_index)
                    else {
                        bail!("Jump operation was not at the expected index")
                    };
                    *jump = end_of_body_index as isize - start_of_body_index as isize;

                    if is_final_predicate {
                        if let Some(else_block) = else_block {
                            let start_of_else_index = end_of_body_index - 1;
                            for ast in else_block {
                                self.compile_node(&ast.node)?;
                            }
                            let end_of_else_block = self.code.len();

                            let Some(Op::Jump(ref mut jump)) =
                                self.code.get_mut(start_of_else_index)
                            else {
                                bail!("Jump operation was not at the expected index")
                            };
                            *jump = end_of_else_block as isize - start_of_else_index as isize;
                        }
                    }
                }
            }
            Node::While {
                label,
                predicate,
                body,
            } => {
                let block_push_index = self.code.len();
                self.code.push(Op::PushBlock(BlockType::Loop {
                    label: *label,
                    loop_length: 0,
                }));

                let start_of_loop_index = self.code.len();
                self.compile_node(&predicate.node)?;
                let predicate_jump_index = self.code.len();
                self.code.push(Op::JumpIfFalse(0));

                for ast in body {
                    self.compile_node(&ast.node)?;
                }
                let end_jump_index = self.code.len();
                self.code.push(Op::Jump(0));
                let end_of_loop_index = self.code.len();

                self.code.push(Op::PopBlock);

                let Some(Op::JumpIfFalse(ref mut jump)) = self.code.get_mut(predicate_jump_index)
                else {
                    bail!("Jump operation was not at the expected index")
                };
                *jump = end_of_loop_index as isize - predicate_jump_index as isize;

                let Some(Op::Jump(ref mut jump)) = self.code.get_mut(end_jump_index) else {
                    bail!("Jump operation was not at the expected index")
                };
                *jump = start_of_loop_index as isize - end_jump_index as isize;

                let Some(Op::PushBlock(ref mut block)) = self.code.get_mut(block_push_index) else {
                    bail!("Block push was not at the expected index")
                };
                *block = BlockType::Loop {
                    label: *label,
                    loop_length: end_of_loop_index - start_of_loop_index,
                };
            }
            Node::FunctionDeclaration {
                ident,
                arguments,
                body,
            } => {
                let function_bundle = Self::default().compile(&body)?;
                self.code.push(Op::LoadConst(Value::Function(
                    Function {
                        arguments: arguments.to_owned(),
                        code: function_bundle.code,
                        constants: function_bundle.consts,
                    }
                    .into(),
                )));

                self.declared_idents.insert(*ident);
                self.code.push(Op::DeclareVar(*ident))
            }
            Node::ClosureDeclaration { arguments, body } => todo!(),
            Node::FunctionCall { calling, arguments } => {
                for ast in arguments {
                    self.compile_node(&ast.node)?;
                }
                self.compile_node(&calling.node)?;
                self.code.push(Op::Call(arguments.len()));
            }
            Node::Assignment { ident, value } => {
                self.compile_node(&value.node)?;
                self.code.push(Op::Store(*ident))
            }
            Node::Return(return_value) => {
                if let Some(value) = return_value {
                    self.compile_node(&value.node)?;
                } else {
                    self.code.push(Op::LoadConst(Value::Undefined));
                }
                self.code.push(Op::Return);
            }
            Node::Break { label } => {
                self.code.push(Op::Break(*label));
            }
            Node::Continue { label } => {
                self.code.push(Op::Continue(*label));
            }
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
