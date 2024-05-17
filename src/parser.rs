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

pub type Span<'a> = LocatedSpan<&'a str>;
type SResult<'a, T> = IResult<Span<'a>, T, VerboseError<Span<'a>>>;

#[derive(Debug)]
pub struct Ast<'a> {
    node: Node<'a>,
    span: Span<'a>,
}

#[derive(Debug)]
pub enum Node<'a> {
    Ident(&'a str),
    Literal(Literal<'a>),
    VariableDeclaration {
        ident: &'a str,
        value: Option<Box<Ast<'a>>>,
    },
    FunctionDeclaration {
        ident: &'a str,
        arguments: Vec<Ast<'a>>,
        body: Vec<Ast<'a>>,
    },
}

#[derive(Debug, Clone)]
pub enum Literal<'a> {
    String(&'a str),
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

fn expr(input: Span) -> SResult<Ast> {
    ws(alt((let_expr, fn_expr, ident_expr, literal_expr))).parse(input)
}

fn let_expr(input: Span) -> SResult<Ast> {
    context(
        "Variable declaration",
        terminated(
            tuple((
                ws(tag("let")),
                ws(ident),
                opt(preceded(ws(tag("=")), ws(expr)).map(Box::new)),
            )),
            ws(end_of_expr),
        ),
    )
    .map(|(span, ident, value)| Ast {
        node: Node::VariableDeclaration {
            ident: ident.fragment(),
            value,
        },
        span,
    })
    .parse(input)
}

fn fn_expr(input: Span) -> SResult<Ast> {
    context(
        "Function declaration",
        tuple((
            ws(tag("fn")),
            ws(ident),
            context(
                "Function arguments",
                ws(delimited(
                    char('('),
                    separated_list0(ws(char(',')), ws(ident_expr)),
                    char(')'),
                )),
            ),
            context(
                "Function body",
                delimited(ws(char('{')), many1(ws(expr)), ws(char('}'))),
            ),
        )),
    )
    .map(|(span, ident, arguments, body)| Ast {
        node: Node::FunctionDeclaration {
            ident: ident.fragment(),
            arguments,
            body,
        },
        span,
    })
    .parse(input)
}

fn ident_expr(input: Span) -> SResult<Ast> {
    terminated(consumed(ident), ws(end_of_expr))
        .map(|(span, ident)| Ast {
            node: Node::Ident(ident),
            span,
        })
        .parse(input)
}

fn literal_expr(input: Span) -> SResult<Ast> {
    terminated(consumed(alt((string, number))), ws(end_of_expr))
        .map(|(span, literal)| Ast {
            node: Node::Literal(literal),
            span,
        })
        .parse(input)
}

fn ident(input: Span) -> SResult<Ident> {
    context(
        "Identifier",
        recognize(tuple((
            alpha1,
            many0(preceded(many0(char('_')), alphanumeric1)),
        )))
        .map(|ident: Span| Ident(ident.fragment())),
    )
    .parse(input)
}

fn string(input: Span) -> SResult<Literal> {
    // TODO: Handle escaped strings
    context(
        "String literal",
        delimited(
            char::<Span, VerboseError<_>>('"'),
            take_until("\""),
            char('"'),
        )
        .map(|span| Literal::String(span.into_fragment())),
    )
    .parse(input)
}

fn number(input: Span) -> SResult<Literal> {
    context(
        "Number literal",
        nom::number::complete::double.map(|value| {
            if value.fract() == 0.0 {
                Literal::Int(value as i64)
            } else {
                Literal::Float(value)
            }
        }),
    )
    .parse(input)
}

fn end_of_expr(input: Span) -> SResult<Span> {
    context("Terminator", alt((tag(";"), eof))).parse(input)
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
    fn parse_let_expr() {
        assert_debug_snapshot!(let_expr("let yeet;".into()));
        assert_debug_snapshot!(let_expr("let yeet = 123;".into()));
        assert_debug_snapshot!(let_expr("let yeet = ;".into()));
    }

    #[test]
    fn parse_fn_expr() {
        assert_debug_snapshot!(fn_expr("fn test(a, b) { let c = 123; }".into()));
    }
}
