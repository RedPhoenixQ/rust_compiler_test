use anyhow::{bail, Result};
use ustr::{Ustr, UstrSet};

use crate::{
    bytecode::{BlockType, Op},
    parser::{Ast, Node},
    value::{function::Function, Value},
};

#[derive(Debug)]
pub struct Bundle {
    pub consts: Vec<Value>,
    pub code: Vec<Op>,
    pub foreign_idents: UstrSet,
}

#[derive(Debug, Default)]
enum CompileContext {
    #[default]
    Global,
    Function,
}

#[derive(Debug)]
pub struct Compiler {
    context: CompileContext,
    consts: Vec<Value>,
    code: Vec<Op>,
    declared_idents: UstrSet,
    foreign_idents: UstrSet,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            context: Default::default(),
            consts: Default::default(),
            code: Default::default(),
            declared_idents: UstrSet::from_iter([Ustr::from("self")].into_iter()),
            foreign_idents: Default::default(),
        }
    }

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
                self.declare_ident(*ident);
                self.code.push(Op::DeclareVar(*ident))
            }
            Node::Ident(ident) => {
                self.reference_ident(*ident);
                self.code.push(Op::Load(*ident));
            }
            Node::Literal(value) => self.code.push(Op::LoadConst(value.clone())),
            Node::ArrayLiteral(array) => {
                for ast in array.into_iter().rev() {
                    self.compile_node(&ast.node)?;
                }
                self.code.push(Op::BuildArray(array.len()));
            }
            Node::ObjectLiteral(entries) => {
                for (key, value) in entries {
                    self.compile_node(&value.node)?;
                    self.code.push(Op::LoadConst(Value::String(*key)));
                }
                self.code.push(Op::BuildObject(entries.len()));
            }
            Node::If {
                branches,
                else_block,
            } => {
                for (i, (predicate, body)) in branches.iter().enumerate() {
                    let is_final_predicate = i == branches.len() - 1;

                    self.compile_node(&predicate.node)?;
                    let predicate_jump_index = self.code.len();
                    self.code.push(Op::JumpIfFalse(0));
                    let start_of_body_index = self.code.len();

                    for ast in body {
                        self.compile_node(&ast.node)?;
                    }

                    if is_final_predicate && else_block.is_some() {
                        // Jump to skip final else block
                        self.code.push(Op::Jump(0))
                    }

                    let end_of_body_index = self.code.len();

                    let Some(Op::JumpIfFalse(ref mut jump)) =
                        self.code.get_mut(predicate_jump_index)
                    else {
                        bail!("Jump operation was not at the expected index")
                    };
                    *jump = end_of_body_index - start_of_body_index;

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
                            *jump = end_of_else_block - start_of_else_index;
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
                self.code.push(Op::JumpBack(0));
                let end_of_loop_index = self.code.len();

                self.code.push(Op::PopBlock);

                let Some(Op::JumpIfFalse(ref mut jump)) = self.code.get_mut(predicate_jump_index)
                else {
                    bail!("Jump operation was not at the expected index")
                };
                *jump = end_of_loop_index - predicate_jump_index;

                let Some(Op::JumpBack(ref mut jump)) = self.code.get_mut(end_jump_index) else {
                    bail!("Jump operation was not at the expected index")
                };
                *jump = end_of_loop_index - start_of_loop_index;

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
                self.declare_ident(*ident);

                let mut compiler = Self::new();
                compiler.context = CompileContext::Function;
                compiler
                    .declared_idents
                    .extend(arguments.iter().map(|(arg, _)| arg));
                let function_bundle = compiler.compile(body)?;
                self.code.push(Op::LoadConst(
                    Function {
                        arguments: arguments
                            .iter()
                            .map(|(arg, default)| {
                                (*arg, default.as_ref().cloned().unwrap_or_default())
                            })
                            .collect(),
                        code: function_bundle.code,
                        constants: function_bundle.consts,
                        foreign_idents: function_bundle.foreign_idents,
                    }
                    .into(),
                ));

                self.code.push(Op::DeclareVar(*ident))
            }
            Node::ClosureDeclaration { arguments, body } => {
                let mut compiler = Self::new();
                compiler.context = CompileContext::Function;
                compiler
                    .declared_idents
                    .extend(arguments.iter().map(|(arg, _)| arg));
                let function_bundle = compiler.compile(body)?;

                self.code.push(Op::MakeClosure(
                    Function {
                        arguments: arguments
                            .iter()
                            .map(|(arg, default)| {
                                (*arg, default.as_ref().cloned().unwrap_or_default())
                            })
                            .collect(),
                        code: function_bundle.code,
                        constants: function_bundle.consts,
                        foreign_idents: function_bundle.foreign_idents,
                    }
                    .into(),
                ));
            }
            Node::FunctionCall { calling, arguments } => {
                // Reverse the arguments in the call to make the topmost stack item the first argument
                for ast in arguments.iter().rev() {
                    self.compile_node(&ast.node)?;
                }

                match &calling.node {
                    Node::AccessKey { source, key } => {
                        // Reimplements Node::AccessKey but also duplicates the object for reuse as the self object
                        self.compile_node(&source.node)?;
                        self.code.push(Op::Duplicate);
                        self.compile_node(&key.node)?;
                        self.code.push(Op::LoadKey);
                        self.code.push(Op::CallMethod(arguments.len()));
                    }
                    Node::AccessAttribute { source, attribute } => {
                        // Reimplements Node::AccessAttribute but also duplicates the object for reuse as the self object
                        self.compile_node(&source.node)?;
                        self.code.push(Op::Duplicate);
                        self.code.push(Op::LoadAttribute(*attribute));
                        self.code.push(Op::CallMethod(arguments.len()));
                    }
                    node => {
                        self.compile_node(node)?;
                        self.code.push(Op::Call(arguments.len()));
                    }
                }
            }
            Node::AccessKey { source, key } => {
                self.compile_node(&source.node)?;
                self.compile_node(&key.node)?;
                self.code.push(Op::LoadKey);
            }
            Node::AccessAttribute { source, attribute } => {
                self.compile_node(&source.node)?;
                self.code.push(Op::LoadAttribute(*attribute));
            }
            Node::Assignment {
                to,
                value,
                operation,
            } => match &to.node {
                Node::Ident(ident) => {
                    self.reference_ident(*ident);
                    match operation {
                        Some(op) => {
                            self.code.push(Op::Load(*ident));
                            self.compile_node(&value.node)?;
                            self.code.push(Op::BinaryOp(*op));
                        }
                        None => self.compile_node(&value.node)?,
                    }
                    self.code.push(Op::Store(*ident))
                }
                Node::AccessAttribute { source, attribute } => {
                    self.compile_node(&source.node)?;
                    match operation {
                        Some(op) => {
                            // Duplicate the source for the load operation
                            self.code.push(Op::Duplicate);
                            self.code.push(Op::LoadAttribute(*attribute));
                            self.compile_node(&value.node)?;
                            self.code.push(Op::BinaryOp(*op));
                        }
                        None => self.compile_node(&value.node)?,
                    }
                    self.code.push(Op::StoreAttribute(*attribute))
                }
                Node::AccessKey { source, key } => {
                    match operation {
                        Some(op) => {
                            self.compile_node(&source.node)?;
                            // Duplicate the source for the load operation
                            self.code.push(Op::Duplicate);
                            self.compile_node(&key.node)?;
                            // Duplicate the key for the load operation
                            self.code.push(Op::Duplicate);
                            // Move the duplicated source down to make the stack [key, source, key, source]
                            self.code.push(Op::PushDown(2));
                            self.code.push(Op::LoadKey);
                            self.compile_node(&value.node)?;
                            self.code.push(Op::BinaryOp(*op));
                        }
                        None => {
                            self.compile_node(&source.node)?;
                            self.compile_node(&key.node)?;
                            self.compile_node(&value.node)?;
                        }
                    }
                    self.code.push(Op::StoreKey);
                }
                ast => bail!("Invalid left hand side of an assignment, recived {ast:?}"),
            },
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

    fn declare_ident(&mut self, ident: Ustr) {
        self.declared_idents.insert(ident);
    }
    fn reference_ident(&mut self, ident: Ustr) -> bool {
        self.foreign_idents.insert(ident)
    }
}
