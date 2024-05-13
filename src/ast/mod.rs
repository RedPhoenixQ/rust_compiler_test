use std::{collections::BTreeMap, iter::Peekable};

use anyhow::{bail, Result};

use crate::tokenizer::{self, Keyword, Symbol, Token, TokenType};

macro_rules! is_expr_end {
    () => {
        None | Some(crate::tokenizer::Token {
            token: crate::tokenizer::TokenType::Symbol(crate::tokenizer::Symbol::SemiColon),
            ..
        })
    };
}

#[derive(Debug)]
pub enum Ast {
    Literal(Literal),
    Ident(Ident),
    UniaryOp(UniaryOp, Box<Ast>),
    BinaryOp(BinaryOp, Box<Ast>, Box<Ast>),
    VariableDecl {
        ident: Ident,
        value: Option<Box<Ast>>,
    },
    Assignment {
        ident: Ident,
        value: Box<Ast>,
    },
    If {
        predicate: Box<Ast>,
        then_branch: Box<Ast>,
        else_branch: Option<Box<Ast>>,
    },
    Block(Vec<Ast>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident(Box<str>);

#[derive(Debug)]
pub enum Literal {
    String(Box<str>),
    Int(i64),
    Float(f64),
    Boolean(bool),
}

#[derive(Debug)]
pub enum UniaryOp {
    Not,
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Div,
    Mul,
    Eq,
    Neq,
    Lt,
    Gt,
    LtEq,
    GtEq,
    Mod,
    LogicalAnd,
    LogicalOr,
    BitwiseOr,
    BitwiseAnd,
}

// pub fn parse_ast<'a>(tokens: impl Iterator<Item = Token<'a>>) {
//     let tokens = tokens.peekable();
// }

pub struct Parser<'a, I: Iterator<Item = Token<'a>>> {
    tokens: Peekable<I>,
    idents: BTreeMap<&'a str, Ident>,
}

impl<'a, I: Iterator<Item = Token<'a>>> Parser<'a, I> {
    pub fn new(tokens: I) -> Self {
        Self {
            tokens: tokens.peekable(),
            idents: BTreeMap::new(),
        }
    }

    pub fn parse(&mut self) -> Vec<Ast> {
        let mut out = Vec::new();
        loop {
            match self.parse_next() {
                Ok(ast) => out.push(ast),
                Err(err) => {
                    eprintln!("{:?}", err);
                    break;
                }
            }
        }
        out
    }

    fn parse_next(&mut self) -> Result<Ast> {
        let Some(Token { token, .. }) = self.tokens.next() else {
            bail!("No more tokens");
        };
        Ok(match token {
            TokenType::Ident(ident) => {
                let ident = self.make_ident(ident);
                if self.parse_end() {
                    Ast::Ident(ident)
                } else if let Ok(op) = self.parse_assignment(ident.clone()) {
                    op
                } else if let Ok(op) = self.parse_binary_op(Box::new(Ast::Ident(ident))) {
                    op
                } else {
                    bail!(
                        "Invalid syntax: Ident followed by '{:?}' is invalid",
                        self.tokens.next()
                    )
                }
            }
            TokenType::Keyword(word) => match word {
                tokenizer::Keyword::True => Ast::Literal(Literal::Boolean(true)),
                tokenizer::Keyword::False => Ast::Literal(Literal::Boolean(false)),
                tokenizer::Keyword::Let => self.parse_let_variable_declaration()?,
                tokenizer::Keyword::If => self.parse_if()?,
                _ => todo!("handle keyword '{word:?}' at ast start"),
            },
            TokenType::Literal(literal) => {
                let literal = Ast::Literal(match literal {
                    tokenizer::Literal::String(str) => {
                        Literal::String(str.to_string().into_boxed_str())
                    }
                    tokenizer::Literal::Int(value) => Literal::Int(value),
                    tokenizer::Literal::Float(value) => Literal::Float(value),
                });
                // Naked literal
                if self.parse_end() {
                    literal
                } else if let Ok(op) = self.parse_binary_op(Box::new(literal)) {
                    op
                } else {
                    bail!("Invalid literal followed by {:?}", self.tokens.next())
                }
            }
            TokenType::Symbol(symbol) => match symbol {
                Symbol::OpenCurlyBrace => {
                    let mut content = Vec::new();
                    while !matches!(
                        self.tokens.peek(),
                        Some(Token {
                            token: TokenType::Symbol(Symbol::CloseCurlyBrace),
                            ..
                        })
                    ) {
                        content.push(self.parse_next()?);
                    }
                    // Consume the ending curlybrace
                    self.tokens.next();
                    Ast::Block(content)
                }
                Symbol::Exclamation => Ast::UniaryOp(UniaryOp::Not, Box::new(self.parse_next()?)),
                _ => bail!("Syntax error: Symbol is invalid as the start of an expression"),
            },
        })
    }

    fn parse_end(&mut self) -> bool {
        match self.tokens.peek() {
            None
            | Some(Token {
                token: TokenType::Symbol(Symbol::SemiColon),
                ..
            }) => {
                self.tokens.next();
                true
            }
            _ => false,
        }
    }

    fn parse_binary_op(&mut self, lhs: Box<Ast>) -> Result<Ast> {
        match self.tokens.peek() {
            Some(Token {
                token: TokenType::Symbol(symbol),
                ..
            }) => {
                let operation = match symbol {
                    Symbol::Asterisk => BinaryOp::Mul,
                    Symbol::Dash => BinaryOp::Sub,
                    Symbol::DoubleEquals => BinaryOp::Eq,
                    Symbol::Plus => BinaryOp::Add,
                    Symbol::Slash => BinaryOp::Div,
                    Symbol::DoublePipe => BinaryOp::LogicalOr,
                    Symbol::DoubleAnd => BinaryOp::LogicalAnd,
                    Symbol::Pipe => BinaryOp::BitwiseOr,
                    Symbol::And => BinaryOp::BitwiseAnd,
                    Symbol::LessThan => BinaryOp::Lt,
                    Symbol::GreaterThan => BinaryOp::Gt,
                    Symbol::LessThanEquals => BinaryOp::LtEq,
                    Symbol::GreaterThanEquals => BinaryOp::GtEq,
                    Symbol::Percent => BinaryOp::Mod,
                    Symbol::DashEquals
                    | Symbol::PlusEquals
                    | Symbol::AsteriskEquals
                    | Symbol::SlashEquals
                    | Symbol::PercentEquals
                    | Symbol::Equals
                    | Symbol::Exclamation
                    | Symbol::SemiColon
                    | Symbol::OpenParen
                    | Symbol::CloseParen
                    | Symbol::OpenCurlyBrace
                    | Symbol::CloseCurlyBrace
                    | Symbol::OpenSqaureBracket
                    | Symbol::CloseSqaureBracket => {
                        bail!("'{symbol:?}' is not a valid binary operation")
                    }
                };
                // Consume operand token
                self.tokens.next();
                Ok(Ast::BinaryOp(operation, lhs, Box::new(self.parse_next()?)))
            }
            Some(token) => {
                bail!("'{token:?}' is not a valid binary operation")
            }
            None => bail!("Unexpected end of input when parsing binary op"),
        }
    }

    fn parse_let_variable_declaration(&mut self) -> Result<Ast> {
        let next_token = self.tokens.next();
        let Some(Token {
            token: TokenType::Ident(ident),
            ..
        }) = next_token
        else {
            bail!(
                "Invalid let statement. Expexted identifier, recived '{:?}'",
                next_token
            )
        };
        let ident = self.make_ident(ident);

        Ok(match self.tokens.next() {
            is_expr_end!() => Ast::VariableDecl { ident, value: None },
            Some(Token {
                token: TokenType::Symbol(tokenizer::Symbol::Equals),
                ..
            }) => Ast::VariableDecl {
                ident,
                value: Some(Box::new(self.parse_next()?)),
            },
            Some(token) => {
                bail!(
                    "Invalid let statement. Expected '=', recived {:?}",
                    token.token
                )
            }
        })
    }

    fn parse_assignment(&mut self, ident: Ident) -> Result<Ast> {
        let token = self.tokens.peek();
        let Some(Token {
            token: TokenType::Symbol(symbol),
            ..
        }) = token
        else {
            bail!("Invalid assignment token, recived {:?}", token)
        };
        let assignment_type: Option<BinaryOp> = match symbol {
            Symbol::Equals => None,
            Symbol::DashEquals => Some(BinaryOp::Sub),
            Symbol::PlusEquals => Some(BinaryOp::Add),
            Symbol::AsteriskEquals => Some(BinaryOp::Mul),
            Symbol::SlashEquals => Some(BinaryOp::Div),
            Symbol::PercentEquals => Some(BinaryOp::Mod),
            _ => bail!("Invalid assignment operation: {:?}", symbol),
        };
        // Consume assignment operator
        self.tokens.next();

        Ok(match assignment_type {
            None => Ast::Assignment {
                ident,
                value: Box::new(self.parse_next()?),
            },
            Some(op) => Ast::Assignment {
                ident: ident.clone(),
                value: Box::new(Ast::BinaryOp(
                    op,
                    Box::new(Ast::Ident(ident)),
                    Box::new(self.parse_next()?),
                )),
            },
        })
    }

    fn parse_if(&mut self) -> Result<Ast> {
        let token = self.tokens.next();
        let Some(Token {
            token: TokenType::Symbol(Symbol::OpenParen),
            ..
        }) = token
        else {
            bail!(
                "Missing opening paren '(' at start of predicate, recived {:?}",
                token
            )
        };

        let predicate = Box::new(self.parse_next()?);

        let token = self.tokens.next();
        let Some(Token {
            token: TokenType::Symbol(Symbol::CloseParen),
            ..
        }) = token
        else {
            bail!(
                "Missing closinh paren ')' at end of predicate, recived {:?}",
                token
            )
        };
        let then_branch @ Ast::Block(_) = self.parse_next()? else {
            bail!("Missing body of if statement")
        };
        let then_branch = Box::new(then_branch);
        let else_branch = if matches!(
            self.tokens.peek(),
            Some(Token {
                token: TokenType::Keyword(Keyword::Else),
                ..
            })
        ) {
            // Consume 'else' token
            self.tokens.next();
            let else_branch @ Ast::Block(_) = self.parse_next()? else {
                bail!("Missing body of if statement")
            };
            Some(Box::new(else_branch))
        } else {
            None
        };
        Ok(Ast::If {
            predicate,
            then_branch,
            else_branch,
        })
    }

    fn make_ident(&mut self, str: &'a str) -> Ident {
        match self.idents.get(str) {
            Some(ident) => ident.clone(),
            None => {
                let ident = Ident(str.to_string().into_boxed_str());
                self.idents.insert(str, ident.clone());
                ident
            }
        }
    }
}
