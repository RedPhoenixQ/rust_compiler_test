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
    VariableDeclaration(&'a str, Option<Box<Ast<'a>>>),
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

#[derive(Debug, Clone, Copy)]
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

fn expr(input: Span) -> SResult<Ast> {
    alt((ident_expr, string_expr, number_expr)).parse(input)
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
        node: Node::VariableDeclaration(ident.fragment(), value),
        span,
    })
    .parse(input)
}

fn ident_expr(input: Span) -> SResult<Ast> {
    ident
        .map(|word| Ast {
            node: Node::Ident(*word.fragment()),
            span: word,
        })
        .parse(input)
}

fn string_expr(input: Span) -> SResult<Ast> {
    // TODO: Handle escaped strings

    context(
        "String literal",
        consumed(delimited(
            char::<Span, VerboseError<_>>('"'),
            take_until("\""),
            char('"'),
        )),
    )
    .map(|(span, str)| Ast {
        node: Node::Literal(Literal::String(*str.fragment())),
        span,
    })
    .parse(input)
}

fn number_expr(input: Span) -> SResult<Ast> {
    context(
        "Number literal",
        consumed(nom::number::complete::double).map(|(span, value)| Ast {
            node: Node::Literal(if value.fract() == 0.0 {
                Literal::Int(value as i64)
            } else {
                Literal::Float(value)
            }),
            span,
        }),
    )
    .parse(input)
}

fn ident(input: Span) -> SResult<Span> {
    context(
        "Identifier",
        recognize(tuple((
            alpha1,
            many0(preceded(many0(char('_')), alphanumeric1)),
        ))),
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
        assert_debug_snapshot!(string_expr(r#""123""#.into()));
    }

    #[test]
    fn parse_numbers() {
        assert_debug_snapshot!(number_expr("123".into()));
        assert_debug_snapshot!(number_expr("123.123".into()));
        assert_debug_snapshot!(number_expr("12s3.123".into()));
    }

    #[test]
    fn parse_let_expr() {
        assert_debug_snapshot!(let_expr("let yeet;".into()));
        assert_debug_snapshot!(let_expr("let yeet = 123;".into()));
        assert_debug_snapshot!(let_expr("let yeet = ;".into()));
    }
}
