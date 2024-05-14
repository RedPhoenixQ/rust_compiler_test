use std::{collections::BTreeSet, iter::Peekable, rc::Rc};

use anyhow::{bail, Context, Result};

use crate::tokenizer::{self, Keyword, Span, Symbol, Token, TokenType};

macro_rules! is_expr_end {
    () => {
        None | Some(crate::tokenizer::Token {
            token: crate::tokenizer::TokenType::Symbol(crate::tokenizer::Symbol::SemiColon),
            ..
        })
    };
}

#[derive(Debug, Clone)]
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
        then_branch: Box<Block>,
        else_branch: Option<Box<Block>>,
    },
    FunctionDecl {
        args: Vec<Ident>,
        body: Box<Block>,
    },
    FunctionCall {
        ident: Ident,
        args: Vec<Ast>,
    },
    Group(Box<Ast>),
    Block(Block),
    Return(Option<Box<Ast>>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident(pub Rc<str>);

#[derive(Debug, Clone)]
pub enum Literal {
    String(Rc<str>),
    Int(i64),
    Float(f64),
    Boolean(bool),
}

#[derive(Debug, Clone, Copy)]
pub enum UniaryOp {
    Not,
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug, Clone, Default)]
pub struct Block {
    pub content: Vec<Ast>,
    pub implicit_return: bool,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    Done,
    UnexpectedEnd,
}

impl std::error::Error for ParseError {}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
pub enum SyntaxError {
    InvalidToken,
    InvalidOperand,
    MissingClosingDelimiter,
    ShouldHaveBeenConsumed,
}

impl std::error::Error for SyntaxError {}

impl std::fmt::Display for SyntaxError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

// pub fn parse_ast<'a>(tokens: impl Iterator<Item = Token<'a>>) {
//     let tokens = tokens.peekable();
// }

pub struct Parser<'a, I: Iterator<Item = Token<'a>>> {
    file: Box<str>,
    tokens: Peekable<I>,
    strings: &'a mut BTreeSet<Rc<str>>,
}

impl<'a, I: Iterator<Item = Token<'a>>> Iterator for Parser<'a, I> {
    type Item = Result<Ast>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(match self.parse_next() {
            Ok(ast) => Ok(ast),
            Err(err) if matches!(err.downcast_ref::<ParseError>(), Some(ParseError::Done)) => {
                return None
            }
            Err(err) => Err(err),
        })
    }
}

impl<'a, I: Iterator<Item = Token<'a>>> Parser<'a, I> {
    pub fn new(tokens: I, strings: &'a mut BTreeSet<Rc<str>>) -> Self {
        Self {
            tokens: tokens.peekable(),
            strings,
            file: "".into(),
        }
    }

    pub fn set_file(&mut self, file: impl AsRef<str>) {
        self.file = file.as_ref().into();
    }

    pub fn parse(&mut self) -> Result<Vec<Ast>> {
        let mut out = Vec::new();
        loop {
            match self.parse_next() {
                Ok(ast) => out.push(ast),
                Err(err) if matches!(err.downcast_ref::<ParseError>(), Some(ParseError::Done)) => {
                    return Ok(out)
                }
                Err(err) => return Err(err).context(format!("Last parsed: {:?}", out.last())),
            }
        }
    }

    fn parse_next(&mut self) -> Result<Ast> {
        let Some(Token { token, span }) = self.tokens.next() else {
            return Err(ParseError::Done)?;
        };
        Ok(match token {
            TokenType::Ident(ident) => {
                let ident = self.make_ident(ident);
                if self.is_end_of_expr() {
                    Ast::Ident(ident)
                } else if let Ok(op) = self.parse_assignment(ident.clone()) {
                    op
                } else if let Ok(op) = self.parse_binary_op(Box::new(Ast::Ident(ident.clone()))) {
                    op
                } else if let Ok(op) = self.parse_function_call(ident.clone()) {
                    op
                } else {
                    return Err(SyntaxError::InvalidToken)
                        .context(self.location(&span))
                        .with_context(|| format!("Ident followed by {:?}", self.tokens.next()));
                }
            }
            TokenType::Keyword(word) => match word {
                boolean @ tokenizer::Keyword::True | boolean @ tokenizer::Keyword::False => {
                    let boolean = Ast::Literal(Literal::Boolean(matches!(
                        boolean,
                        tokenizer::Keyword::True
                    )));
                    if self.is_end_of_expr() {
                        boolean
                    } else if let Ok(op) = self.parse_binary_op(Box::new(boolean)) {
                        op
                    } else {
                        return Err(SyntaxError::InvalidToken)
                            .context(self.location(&span))
                            .with_context(|| {
                                format!("Boolean followed by {:?}", self.tokens.next())
                            });
                    }
                }
                tokenizer::Keyword::Let => self.parse_let_variable_declaration()?,
                tokenizer::Keyword::If => self.parse_if()?,
                tokenizer::Keyword::Function => self.parse_function_declaration()?,
                Keyword::Else => {
                    return Err(SyntaxError::InvalidToken)
                        .context(self.location(&span))
                        .with_context(|| {
                            format!(
                                "Keyword {:?} should not appear at the start of expression",
                                word,
                            )
                        })
                }
                Keyword::Return => {
                    if self.is_end_of_expr() {
                        Ast::Return(None)
                    } else {
                        Ast::Return(Some(Box::new(self.parse_next()?)))
                    }
                }
                Keyword::Continue | Keyword::Break => {
                    todo!("handle keyword '{word:?}' at ast start")
                }
            },
            TokenType::Literal(literal) => {
                let literal = Ast::Literal(match literal {
                    tokenizer::Literal::String(str) => {
                        let string = self.strings.get(str).cloned().unwrap_or_else(|| {
                            let string: Rc<str> = str.into();
                            self.strings.insert(string.clone());
                            string
                        });
                        Literal::String(string)
                    }
                    tokenizer::Literal::Int(value) => Literal::Int(value),
                    tokenizer::Literal::Float(value) => Literal::Float(value),
                });
                // Naked literal
                if self.is_end_of_expr() {
                    literal
                } else if let Ok(op) = self.parse_binary_op(Box::new(literal)) {
                    op
                } else {
                    return Err(SyntaxError::InvalidToken)
                        .context(self.location(&span))
                        .with_context(|| {
                            format!("Invalid literal followed by {:?}", self.tokens.next())
                        });
                }
            }
            TokenType::Symbol(symbol) => match symbol {
                Symbol::OpenCurlyBrace => Ast::Block(self.parse_block_content()?),
                Symbol::OpenParen => {
                    let group = Ast::Group(Box::new(self.parse_next()?));
                    // Consume the ending curlybrace
                    match self.tokens.next() {
                        Some(Token {
                            token: TokenType::Symbol(Symbol::CloseParen),
                            ..
                        }) => {}
                        token => {
                            return Err(SyntaxError::InvalidToken)
                                .context(self.location(&span))
                                .with_context(|| {
                                    format!(
                                        "Exprexted {:?}, recived {:?}",
                                        Symbol::CloseParen,
                                        token
                                    )
                                })
                        }
                    };

                    if self.is_end_of_expr() {
                        group
                    } else if let Ok(op) = self.parse_binary_op(Box::new(group)) {
                        op
                    } else {
                        return Err(SyntaxError::InvalidToken)
                            .context(self.location(&span))
                            .with_context(|| {
                                format!("Group followed by {:?}", self.tokens.next())
                            });
                    }
                }
                Symbol::Exclamation => Ast::UniaryOp(UniaryOp::Not, Box::new(self.parse_next()?)),
                Symbol::SemiColon => self.parse_next()?,
                Symbol::CloseParen | Symbol::CloseCurlyBrace => {
                    return Err(SyntaxError::MissingClosingDelimiter)
                        .context(self.location(&span))
                        .with_context(|| {
                            format!("{:?} should be consumed by other structure", symbol,)
                        })
                }
                _ => {
                    return Err(SyntaxError::InvalidToken)
                        .context(self.location(&span))
                        .with_context(|| format!("Symbol: {:?}", symbol))
                }
            },
        })
    }

    fn is_end_of_expr(&mut self) -> bool {
        matches!(
            self.tokens.peek(),
            None | Some(Token {
                token: TokenType::Symbol(Symbol::SemiColon),
                ..
            }) | Some(Token {
                token: TokenType::Symbol(Symbol::CloseParen),
                ..
            }) | Some(Token {
                token: TokenType::Symbol(Symbol::CloseCurlyBrace),
                ..
            }) | Some(Token {
                token: TokenType::Symbol(Symbol::Comma),
                ..
            })
        )
    }

    fn parse_block_content(&mut self) -> Result<Block> {
        let mut block = Block::default();
        while let Some(token) = self.tokens.peek() {
            match token {
                Token {
                    token: TokenType::Symbol(Symbol::CloseCurlyBrace),
                    ..
                } => break,
                Token {
                    token: TokenType::Symbol(Symbol::SemiColon),
                    ..
                } => {
                    block.implicit_return = false;
                    // Comsume the semicolon to prevent infinite loop
                    self.tokens.next();
                    continue;
                }
                _ => block.implicit_return = true,
            }
            block.content.push(self.parse_next()?);
        }
        // Consume the ending curlybrace
        self.tokens.next();
        Ok(block)
    }

    fn parse_binary_op(&mut self, lhs: Box<Ast>) -> Result<Ast> {
        match self.tokens.peek().cloned() {
            Some(Token {
                token: TokenType::Symbol(symbol),
                span,
            }) => {
                // TODO: Fix math default order of operations
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
                    | Symbol::ExclamationEquals
                    | Symbol::Equals
                    | Symbol::Exclamation
                    | Symbol::Comma
                    | Symbol::Period
                    | Symbol::SemiColon
                    | Symbol::OpenParen
                    | Symbol::CloseParen
                    | Symbol::OpenCurlyBrace
                    | Symbol::CloseCurlyBrace
                    | Symbol::OpenSqaureBracket
                    | Symbol::CloseSqaureBracket => {
                        return Err(SyntaxError::InvalidOperand)
                            .context(self.location(&span))
                            .with_context(|| format!("{:?} is not a valid infix operand", symbol))
                    }
                };
                // Consume operand token
                self.tokens.next();
                Ok(Ast::BinaryOp(operation, lhs, Box::new(self.parse_next()?)))
            }
            Some(Token { token, span }) => {
                return Err(SyntaxError::InvalidToken)
                    .context(self.location(&span))
                    .with_context(|| format!("{:?} is not a valid operand", token))
            }
            None => {
                return Err(ParseError::UnexpectedEnd)
                    .context("No more tokens when parsing binary op")
            }
        }
    }

    fn parse_let_variable_declaration(&mut self) -> Result<Ast> {
        let ident = match self.tokens.next() {
            Some(Token {
                token: TokenType::Ident(ident),
                ..
            }) => self.make_ident(ident),
            Some(Token { span, token }) => {
                return Err(SyntaxError::InvalidToken)
                    .context(self.location(&span))
                    .with_context(|| format!("Expected identifier, recived {:?}", token));
            }
            None => {
                return Err(ParseError::UnexpectedEnd)
                    .context("No more tokens when parsing variable declaration");
            }
        };

        Ok(match self.tokens.next() {
            is_expr_end!() => Ast::VariableDecl { ident, value: None },
            Some(Token {
                token: TokenType::Symbol(tokenizer::Symbol::Equals),
                ..
            }) => Ast::VariableDecl {
                ident,
                value: Some(Box::new(self.parse_next()?)),
            },
            Some(Token { span, token }) => {
                return Err(SyntaxError::InvalidToken)
                    .context(self.location(&span))
                    .with_context(|| {
                        format!("Expected {:?}, recived {:?}", Symbol::Equals, token)
                    });
            }
        })
    }

    fn parse_assignment(&mut self, ident: Ident) -> Result<Ast> {
        let symbol = match self.tokens.peek().cloned() {
            Some(Token {
                token: TokenType::Symbol(symbol),
                ..
            }) if matches!(
                symbol,
                Symbol::Equals
                    | Symbol::DashEquals
                    | Symbol::PlusEquals
                    | Symbol::AsteriskEquals
                    | Symbol::SlashEquals
                    | Symbol::PercentEquals
                    | Symbol::ExclamationEquals
            ) =>
            {
                symbol
            }
            Some(Token { span, token }) => {
                return Err(SyntaxError::InvalidToken)
                    .context(self.location(&span))
                    .with_context(|| format!("Expected assignment operand, recived {:?}", token));
            }
            None => {
                return Err(ParseError::UnexpectedEnd)
                    .context("No more tokens when parsing variable declaration");
            }
        };
        // Consume assignment operator
        self.tokens.next();

        let assignment_type: Option<BinaryOp> = match symbol {
            Symbol::Equals => None,
            Symbol::DashEquals => Some(BinaryOp::Sub),
            Symbol::PlusEquals => Some(BinaryOp::Add),
            Symbol::AsteriskEquals => Some(BinaryOp::Mul),
            Symbol::SlashEquals => Some(BinaryOp::Div),
            Symbol::PercentEquals => Some(BinaryOp::Mod),
            Symbol::ExclamationEquals => Some(BinaryOp::Neq),
            _ => bail!("Invalid assignment operation: {:?}", symbol),
        };

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

    fn parse_function_call(&mut self, ident: Ident) -> Result<Ast> {
        match self.tokens.next() {
            Some(Token {
                token: TokenType::Symbol(Symbol::OpenParen),
                ..
            }) => {}
            Some(Token { span, token }) => {
                return Err(SyntaxError::InvalidToken)
                    .context(self.location(&span))
                    .with_context(|| {
                        format!("Expected {:?}, recived {:?}", Symbol::OpenParen, token)
                    });
            }
            None => {
                return Err(ParseError::UnexpectedEnd)
                    .context("No more tokens when parsing variable declaration");
            }
        }

        let mut args = Vec::new();
        loop {
            match self.tokens.peek() {
                Some(Token {
                    token: TokenType::Symbol(Symbol::Comma),
                    ..
                }) => {
                    self.tokens.next();
                    continue;
                }
                Some(Token {
                    token: TokenType::Symbol(Symbol::CloseParen),
                    ..
                }) => {
                    self.tokens.next();
                    break;
                }
                _ => args.push(self.parse_next()?),
            }
        }
        if self.is_end_of_expr() {
            Ok(Ast::FunctionCall { ident, args })
        } else {
            let Token { span, token } = self
                .tokens
                .next()
                .expect("is_end_of_expr should handle None case");
            return Err(SyntaxError::InvalidToken)
                .context(self.location(&span))
                .with_context(|| {
                    format!("Unexpected token after function call, recived {:?}", token)
                });
        }
    }

    fn parse_if(&mut self) -> Result<Ast> {
        let predicate = self.parse_next().context("Could not parse if predicate")?;
        let predicate = Box::new(predicate);
        let then_branch = Box::new(
            self.parse_block_content()
                .context("Could not then_brach block")?,
        );
        let else_branch = if matches!(
            self.tokens.peek(),
            Some(Token {
                token: TokenType::Keyword(Keyword::Else),
                ..
            })
        ) {
            // Consume 'else' token
            self.tokens.next();
            Some(Box::new(
                self.parse_block_content()
                    .context("Could not else_brach block")?,
            ))
        } else {
            None
        };
        Ok(Ast::If {
            predicate,
            then_branch,
            else_branch,
        })
    }

    fn parse_function_declaration(&mut self) -> Result<Ast> {
        let token = self.tokens.next();
        let Some(Token {
            token: TokenType::Ident(ident),
            ..
        }) = token
        else {
            bail!("Syntax error: Expexten function name, recived {:?}", token)
        };
        let ident = self.make_ident(ident);

        let token = self.tokens.next();
        let Some(Token {
            token: TokenType::Symbol(Symbol::OpenParen),
            ..
        }) = token
        else {
            bail!(
                "Syntax error: Expexten opening paren '(', recived {:?}",
                token
            )
        };

        let mut args = Vec::new();
        while let Some(Token { token, .. }) = self.tokens.next() {
            match token {
                TokenType::Ident(ident) => args.push(self.make_ident(ident)),
                TokenType::Symbol(Symbol::Comma) => continue,
                TokenType::Symbol(Symbol::CloseParen) => break,
                _ => bail!(
                    "Syntax error: Expected function arguments names, recived {:?}",
                    token
                ),
            }
        }

        match self.tokens.next() {
            Some(Token {
                token: TokenType::Symbol(Symbol::OpenCurlyBrace),
                ..
            }) => {}
            token => bail!(
                "Syntax error: Expexten opening curly brace '{{', recived {:?}",
                token
            ),
        }

        let body = Box::new(self.parse_block_content()?);

        Ok(Ast::Assignment {
            ident,
            value: Box::new(Ast::FunctionDecl { args, body }),
        })
    }

    fn make_ident(&mut self, str: &'a str) -> Ident {
        match self.strings.get(str) {
            Some(ident) => Ident(ident.clone()),
            None => {
                let string: Rc<str> = str.into();
                self.strings.insert(string.clone());
                Ident(string)
            }
        }
    }

    fn location(&self, span: &Span) -> String {
        format!(
            "{}{}{}:{} -> {}",
            self.file,
            if self.file.is_empty() { "" } else { ":" },
            span.location_line(),
            span.get_column(),
            span.fragment()
        )
    }
}
