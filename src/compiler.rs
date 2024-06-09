use anyhow::{Context, Result};
use ustr::Ustr;

use crate::{
    bytecode::Op,
    parser::{Ast, BinaryOp, Node, UnaryOp},
    value::Value,
};

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
            } => todo!(),
            Node::While { predicate, body } => todo!(),
            Node::FunctionDeclaration {
                ident,
                arguments,
                body,
            } => todo!(),
            Node::ClosureDeclaration { arguments, body } => todo!(),
            Node::FunctionCall { calling, arguments } => todo!(),
            Node::Assignment { ident, value } => todo!(),
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
