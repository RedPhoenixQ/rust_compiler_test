use nom::{
    branch::*,
    bytes::complete::*,
    character::complete::*,
    combinator::*,
    error::{context, ParseError, VerboseError},
    multi::*,
    sequence::*,
    IResult, Parser,
};
use nom_locate::LocatedSpan;
use ustr::Ustr;

use crate::value::Value;

pub type Span<'a> = LocatedSpan<&'a str>;
type SResult<'a, T> = IResult<Span<'a>, T, VerboseError<Span<'a>>>;

#[derive(Debug)]
pub struct Ast<'a> {
    pub node: Node<'a>,
    pub span: Span<'a>,
}

#[derive(Debug)]
pub enum Node<'a> {
    Ident(Ustr),
    Literal(Value),
    Group(Box<Ast<'a>>),
    If {
        /// There should always be a root branch in this Vec
        ///
        /// branches.len() >= 1
        branches: Box<[(Ast<'a>, Box<[Ast<'a>]>)]>,
        else_block: Option<Box<[Ast<'a>]>>,
    },
    VariableDeclaration {
        ident: Ustr,
        value: Option<Box<Ast<'a>>>,
    },
    FunctionDeclaration {
        ident: Ustr,
        arguments: Box<[Ustr]>,
        body: Box<[Ast<'a>]>,
    },
    Assignment {
        ident: Ustr,
        value: Box<Ast<'a>>,
    },
    Return(Option<Box<Ast<'a>>>),
    UnaryOp(UnaryOp, Box<Ast<'a>>),
    BinaryOp(BinaryOp, Box<Ast<'a>>, Box<Ast<'a>>),
}

#[derive(Debug, Clone, Copy)]
pub enum UnaryOp {
    LogicalNot,
    BitwiseNot,
    Negative,
    Positive,
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

impl BinaryOp {
    /// https://en.cppreference.com/w/c/language/operator_precedence
    /// Lower value should be evaluated first
    fn priority(&self) -> u8 {
        match self {
            Self::Mul | Self::Div | Self::Mod => 3,
            Self::Add | Self::Sub => 4,
            Self::Lt | Self::Gt | Self::LtEq | Self::GtEq => 6,
            Self::Eq | Self::Neq => 7,
            Self::BitwiseAnd => 8,
            Self::BitwiseOr => 10,
            Self::LogicalAnd => 11,
            Self::LogicalOr => 12,
        }
    }
}

fn statement(input: Span) -> SResult<Ast> {
    context(
        "Statement",
        ws(alt((
            let_statement,
            if_statement,
            fn_statement,
            assignment_statement,
            return_statement,
            terminated(expr, ws(terminator)),
            fail,
        ))),
    )
    .parse(input)
}

fn expr(input: Span) -> SResult<Ast> {
    ws(alt((
        unary_operation_expr,
        binary_operation_expr,
        value_expr,
        fail,
    )))
    .parse(input)
}

fn value_expr(input: Span) -> SResult<Ast> {
    context("Value", alt((ident_expr, literal_expr, fail))).parse(input)
}

fn let_statement(input: Span) -> SResult<Ast> {
    context(
        "Variable declaration",
        terminated(
            consumed(preceded(
                tag("let"),
                pair(
                    ws(ident),
                    opt(preceded(ws(tag("=")), ws(expr)).map(Box::new)),
                ),
            )),
            ws(terminator),
        ),
    )
    .map(|(span, (ident, value))| Ast {
        node: Node::VariableDeclaration { ident, value },
        span,
    })
    .parse(input)
}

fn if_statement(input: Span) -> SResult<Ast> {
    context(
        "If statement",
        consumed(tuple((
            preceded(
                keyword("if"),
                pair(
                    delimited(ws(char('(')), expr, ws(char(')'))),
                    delimited(
                        ws(char('{')),
                        many1(statement).map(|body| body.into_boxed_slice()),
                        ws(char('}')),
                    ),
                ),
            ),
            many0(preceded(
                pair(ws(keyword("else")), ws(keyword("if"))),
                pair(
                    delimited(ws(char('(')), expr, ws(char(')'))),
                    delimited(
                        ws(char('{')),
                        many1(statement).map(|body| body.into_boxed_slice()),
                        ws(char('}')),
                    ),
                ),
            )),
            opt(preceded(
                ws(keyword("else")),
                delimited(
                    ws(char('{')),
                    many1(statement).map(|body| body.into_boxed_slice()),
                    ws(char('}')),
                ),
            )),
        ))),
    )
    .map(|(span, (root_if, mut branches, else_block))| {
        // Put root at the first index
        branches.insert(0, root_if);
        Ast {
            node: Node::If {
                branches: branches.into_boxed_slice(),
                else_block,
            },
            span,
        }
    })
    .parse(input)
}

fn fn_statement(input: Span) -> SResult<Ast> {
    context(
        "Function declaration",
        consumed(preceded(
            keyword("fn"),
            tuple((
                ws(ident),
                context(
                    "Function arguments",
                    ws(delimited(
                        char('('),
                        separated_list0(ws(char(',')), ws(ident))
                            .map(|args| args.into_boxed_slice()),
                        char(')'),
                    )),
                ),
                delimited(
                    ws(context("Start of function block", char('{'))),
                    context(
                        "Function body",
                        many1(statement).map(|body| body.into_boxed_slice()),
                    ),
                    ws(context("End of function block", char('}'))),
                ),
            )),
        )),
    )
    .map(|(span, (ident, arguments, body))| Ast {
        node: Node::FunctionDeclaration {
            ident,
            arguments,
            body,
        },
        span,
    })
    .parse(input)
}

fn assignment_statement(input: Span) -> SResult<Ast> {
    context(
        "Variable assignment",
        terminated(
            consumed(tuple((
                consumed(ident),
                ws(terminated(
                    opt(alt((
                        value(BinaryOp::Add, char('+')),
                        value(BinaryOp::Sub, char('-')),
                        value(BinaryOp::Div, char('/')),
                        value(BinaryOp::Mul, char('*')),
                        value(BinaryOp::Mod, char('%')),
                        value(BinaryOp::BitwiseAnd, char('&')),
                        value(BinaryOp::BitwiseOr, char('|')),
                    ))),
                    char('='),
                )),
                expr,
            ))),
            terminator,
        ),
    )
    .map(|(span, ((ident_span, ident), operation, value))| Ast {
        node: Node::Assignment {
            ident,
            value: match operation {
                Some(operation) => Ast {
                    node: Node::BinaryOp(
                        operation,
                        Ast {
                            node: Node::Ident(ident),
                            span: ident_span,
                        }
                        .into(),
                        value.into(),
                    ),
                    span,
                },
                None => value,
            }
            .into(),
        }
        .into(),
        span,
    })
    .parse(input)
}

fn return_statement(input: Span) -> SResult<Ast> {
    context(
        "Return statement",
        terminated(
            consumed(preceded(keyword("return"), ws(opt(expr)))),
            ws(terminator),
        ),
    )
    .map(|(span, value)| Ast {
        node: Node::Return(value.map(Box::new)),
        span,
    })
    .parse(input)
}

fn unary_operation_expr(input: Span) -> SResult<Ast> {
    context(
        "Unary operation",
        consumed(pair(
            alt((
                value(UnaryOp::LogicalNot, char('!')),
                value(UnaryOp::BitwiseNot, char('~')),
                value(UnaryOp::Negative, char('-')),
                value(UnaryOp::Positive, char('+')),
                fail,
            )),
            ws(alt((delimited(char('('), expr, char(')')), value_expr))),
        )),
    )
    .map(|(span, (operation, value))| Ast {
        node: Node::UnaryOp(operation, Box::new(value)),
        span,
    })
    .parse(input)
}

fn binary_operation_expr(input: Span) -> SResult<Ast> {
    let (input, (span, ((lhs_grouped, lhs), first_operator, (rhs_grouped, rhs)))) =
        consumed(tuple((
            alt((
                delimited(char('('), expr, char(')')).map(|e| (true, e)),
                value_expr.map(|e| (false, e)),
            )),
            ws(context(
                "Binary operator",
                alt((
                    value(BinaryOp::LogicalAnd, tag("&&")),
                    value(BinaryOp::LogicalOr, tag("||")),
                    value(BinaryOp::Eq, tag("==")),
                    value(BinaryOp::Neq, tag("!=")),
                    value(BinaryOp::LtEq, tag("<=")),
                    value(BinaryOp::GtEq, tag(">=")),
                    value(BinaryOp::Lt, char('<')),
                    value(BinaryOp::Gt, char('>')),
                    value(BinaryOp::Add, char('+')),
                    value(BinaryOp::Sub, char('-')),
                    value(BinaryOp::Div, char('/')),
                    value(BinaryOp::Mul, char('*')),
                    value(BinaryOp::Mod, char('%')),
                    value(BinaryOp::BitwiseAnd, char('&')),
                    value(BinaryOp::BitwiseOr, char('|')),
                    fail,
                )),
            )),
            ws(alt((
                delimited(char('('), expr, char(')')).map(|e| (true, e)),
                expr.map(|e| (false, e)),
            ))),
        )))
        .parse(input)?;

    let node = match rhs {
        Ast {
            node: Node::BinaryOp(other_operator, middle, rhs),
            span,
        } if !lhs_grouped
            && !rhs_grouped
            && first_operator.priority() < other_operator.priority() =>
        {
            // If non of the sides are grouped with parentheses, group them based on priority
            // Operation with a lower priority number should be evaluated first
            // The order in the tree is swap so the right hand side evaluates first
            Node::BinaryOp(
                other_operator,
                Ast {
                    node: Node::BinaryOp(first_operator, lhs.into(), middle),
                    span,
                }
                .into(),
                rhs,
            )
            } else {
                Node::BinaryOp(
                    first_operator,
                    lhs.into(),
                    Ast {
                        node: Node::BinaryOp(other_operator, middle, rhs),
                        span,
                    }
                    .into(),
                )
            }
        }
        _ => Node::BinaryOp(first_operator, lhs.into(), rhs.into()),
    };

    Ok((input, Ast { node, span }))
}

fn group_expr(input: Span) -> SResult<Ast> {
    context("Group", consumed(delimited(char('('), expr, ws(char(')')))))
        .map(|(span, group)| Ast {
            node: Node::Group(group.into()),
            span,
        })
        .parse(input)
}

fn ident_expr(input: Span) -> SResult<Ast> {
    consumed(ident)
        .map(|(span, ident)| Ast {
            node: Node::Ident(ident),
            span,
        })
        .parse(input)
}

fn literal_expr(input: Span) -> SResult<Ast> {
    consumed(alt((string, number, boolean, fail)))
        .map(|(span, literal)| Ast {
            node: Node::Literal(literal),
            span,
        })
        .parse(input)
}

fn ident(input: Span) -> SResult<Ustr> {
    context(
        "Identifier",
        recognize(tuple((
            alpha1,
            many0(preceded(many0(char('_')), alphanumeric1)),
        )))
        .map(|ident: Span| ident.into_fragment().into()),
    )
    .parse(input)
}

fn string(input: Span) -> SResult<Value> {
    // TODO: Handle escaped strings
    context(
        "String literal",
        delimited(
            char::<Span, VerboseError<_>>('"'),
            take_until("\""),
            char('"'),
        )
        .map(|span| Value::String(span.into_fragment().into())),
    )
    .parse(input)
}

fn number(input: Span) -> SResult<Value> {
    context(
        "Number literal",
        nom::number::complete::double.map(|value| {
            if value.fract() == 0.0 {
                Value::Int(value as i64)
            } else {
                Value::Float(value)
            }
        }),
    )
    .parse(input)
}

fn boolean(input: Span) -> SResult<Value> {
    context(
        "Boolean literal",
        alt((
            value(Value::Boolean(true), keyword("true")),
            value(Value::Boolean(false), keyword("false")),
        )),
    )
    .parse(input)
}

fn terminator(input: Span) -> SResult<Span> {
    context("Terminator", alt((tag(";"), eof))).parse(input)
}

fn keyword<'a>(word: &'static str) -> impl FnMut(Span<'a>) -> SResult<Span> {
    move |input: Span| {
        context(
            "Keyword",
            terminated(
                tag(word),
                context(
                    "Is part of another word",
                    peek(not(alt((alphanumeric1, tag("_"))))),
                ),
            ),
        )
        .parse(input)
    }
}

fn ws<'a, P: Parser<Span<'a>, O, E>, O, E: ParseError<Span<'a>>>(
    mut parser: P,
) -> impl FnMut(Span<'a>) -> IResult<Span, O, E> {
    move |input: Span| {
        let (input, _whitespace) = multispace0.parse(input)?;
        parser.parse(input)
    }
}

#[cfg(test)]
mod test {
    use insta::assert_debug_snapshot;

    use super::*;

    #[test]
    fn parse_ident() {
        assert_debug_snapshot!(ident_expr("haha".into()));
        assert_debug_snapshot!(ident_expr("AS_23ds_s".into()));
        assert_debug_snapshot!(ident_expr("12haha".into()));
        assert_debug_snapshot!(ident_expr("left-right".into()));
        assert_debug_snapshot!(ident_expr("a".into()));
    }

    #[test]
    fn parse_strings() {
        assert_debug_snapshot!(string(r#""123""#.into()));
    }

    #[test]
    fn parse_numbers() {
        assert_debug_snapshot!(number("123".into()));
        assert_debug_snapshot!(number("123.123".into()));
        assert_debug_snapshot!(number("12s3.123".into()));
    }

    #[test]
    fn parse_booleans() {
        assert_debug_snapshot!(boolean(r#"true"#.into()));
        assert_debug_snapshot!(boolean(r#"false"#.into()));
    }

    #[test]
    fn test_keyword() {
        assert_debug_snapshot!(keyword("let").parse("let test".into()));
        assert_debug_snapshot!(keyword("let").parse("leting test".into()));
        assert_debug_snapshot!(keyword("true").parse("true)".into()));
        assert_debug_snapshot!(keyword("true").parse("true|".into()));
        assert_debug_snapshot!(keyword("true").parse("true_".into()));
    }

    #[test]
    fn parse_let_expr() {
        assert_debug_snapshot!(let_statement("let yeet;".into()));
        assert_debug_snapshot!(let_statement("let yeet = 123;".into()));
        assert_debug_snapshot!(let_statement("let yeet = ;".into()));
    }

    #[test]
    fn parse_fn_expr() {
        assert_debug_snapshot!(fn_statement("fn test(a, b) { let c = 123; }".into()));
        assert_debug_snapshot!(fn_statement("fn test(a, b) { 123; }".into()));
        assert_debug_snapshot!(fn_statement(
            "fn invalid_argument(123, b) { let c = 123; }".into()
        ));
        assert_debug_snapshot!(fn_statement("fn incomplete_args(a { 123; }".into()));
        assert_debug_snapshot!(fn_statement("fn no_args() { 123; }".into()));
        assert_debug_snapshot!(fn_statement("fn missing_block_start()  123; }".into()));
        assert_debug_snapshot!(fn_statement("fn missing_block_end() { 123; ".into()));
        assert_debug_snapshot!(fn_statement("fn only_ident".into()));
        assert_debug_snapshot!(fn_statement("fn ".into()));
        // assert_debug_snapshot!(fn_statement(
        //     "fn nesting(a, b) { fn nested() { let c = 123 } }".into()
        // ));
    }

    #[test]
    fn parse_if() {
        assert_debug_snapshot!(if_statement("if (a == 1) { a; }".into()));
        assert_debug_snapshot!(if_statement("if (a == 1) { a; } else { b; }".into()));
        assert_debug_snapshot!(if_statement(
            "if (a == 1) { a; } else if (a < 1) { b; }".into()
        ));
        assert_debug_snapshot!(if_statement(
            "if (a == 1) { a; } else if (a < 1) { b; } else { c; }".into()
        ));
    }

    #[test]
    fn parse_return() {
        assert_debug_snapshot!(return_statement("return;".into()));
        assert_debug_snapshot!(return_statement("return a".into()));
        assert_debug_snapshot!(return_statement("return a + b - c".into()));
    }

    #[test]
    fn parse_assignment() {
        assert_debug_snapshot!(assignment_statement("yeet = 123;".into()));
        assert_debug_snapshot!(assignment_statement("yeet += 123;".into()));
        assert_debug_snapshot!(assignment_statement("yeet -= 123;".into()));
        assert_debug_snapshot!(assignment_statement("yeet /= 123;".into()));
        assert_debug_snapshot!(assignment_statement("yeet *= 123;".into()));
        assert_debug_snapshot!(assignment_statement("yeet %= 123;".into()));
        assert_debug_snapshot!(assignment_statement("yeet &= 123;".into()));
        assert_debug_snapshot!(assignment_statement("yeet |= 123;".into()));

        // Failures
        assert_debug_snapshot!(assignment_statement("yeet + 123;".into()));
    }

    #[test]
    fn parse_group() {
        assert_debug_snapshot!(group_expr("(123)".into()));
        assert_debug_snapshot!(group_expr("123)".into()));
        assert_debug_snapshot!(group_expr("(123".into()));
    }

    #[test]
    fn parse_binary_operations() {
        // Basic operations
        assert_debug_snapshot!(binary_operation_expr("a + b".into()));
        assert_debug_snapshot!(binary_operation_expr("a - b".into()));
        assert_debug_snapshot!(binary_operation_expr("a / b".into()));
        assert_debug_snapshot!(binary_operation_expr("a * b".into()));
        assert_debug_snapshot!(binary_operation_expr("a % b".into()));
        assert_debug_snapshot!(binary_operation_expr("a & b".into()));
        assert_debug_snapshot!(binary_operation_expr("a | b".into()));
        assert_debug_snapshot!(binary_operation_expr("a && b".into()));
        assert_debug_snapshot!(binary_operation_expr("a || b".into()));
        assert_debug_snapshot!(binary_operation_expr("a < b".into()));
        assert_debug_snapshot!(binary_operation_expr("a > b".into()));
        assert_debug_snapshot!(binary_operation_expr("a <= b".into()));
        assert_debug_snapshot!(binary_operation_expr("a >= b".into()));
        assert_debug_snapshot!(binary_operation_expr("a == b".into()));
        assert_debug_snapshot!(binary_operation_expr("a != b".into()));

        // Operator priority
        assert_debug_snapshot!(binary_operation_expr("a * b + c".into()));
        assert_debug_snapshot!(binary_operation_expr("a + b * c".into()));

        assert_debug_snapshot!(binary_operation_expr("a > b && c".into()));
        assert_debug_snapshot!(binary_operation_expr("b && a > 0".into()));
        assert_debug_snapshot!(binary_operation_expr("a && b || c".into()));
        assert_debug_snapshot!(binary_operation_expr("a || b && c".into()));
        assert_debug_snapshot!(binary_operation_expr("a && (b || c)".into()));
        assert_debug_snapshot!(binary_operation_expr("(a || b) && c".into()));

        // Formatings
        assert_debug_snapshot!(binary_operation_expr("a + b;".into()));
        assert_debug_snapshot!(binary_operation_expr("a-b".into()));

        // Failures
        assert_debug_snapshot!(binary_operation_expr("a && b = c".into()));
        assert_debug_snapshot!(binary_operation_expr("a = b".into()));
    }

    #[test]
    fn parse_unary_operations() {
        assert_debug_snapshot!(unary_operation_expr("!a".into()));
        assert_debug_snapshot!(unary_operation_expr("~a".into()));
        assert_debug_snapshot!(unary_operation_expr("!(a || b) + c".into()));
        assert_debug_snapshot!(unary_operation_expr("+a".into()));
        assert_debug_snapshot!(unary_operation_expr("-a".into()));
    }
}
