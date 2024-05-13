use nom::{
    branch::alt,
    bytes::complete::*,
    character::complete::*,
    combinator::{iterator, peek, recognize, ParserIterator},
    multi::many0,
    number::complete::double,
    sequence::{delimited, preceded, tuple},
    IResult, Parser,
};
use nom_locate::LocatedSpan;

type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug)]
pub struct Token<'a> {
    pub span: Span<'a>,
    pub token: TokenType<'a>,
}

#[derive(Debug, PartialEq)]
pub enum TokenType<'a> {
    Ident(&'a str),
    Keyword(Keyword),
    Symbol(Symbol),
    Literal(Literal<'a>),
}

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Let,
    Function,
    If,
    Else,
    Continue,
    Break,
    True,
    False,
}

#[derive(Debug, PartialEq)]
pub enum Symbol {
    DashEquals,
    PlusEquals,
    AsteriskEquals,
    SlashEquals,
    DoubleEquals,
    DoublePipe,
    DoubleAnd,
    LessThanEquals,
    GreaterThanEquals,

    Equals,
    Dash,
    Plus,
    Slash,
    Asterisk,
    LessThan,
    GreaterThan,
    Exclamation,
    Pipe,
    And,
    Percent,

    SemiColon,

    OpenParen,
    CloseParen,
    OpenCurlyBrace,
    CloseCurlyBrace,
    OpenSqaureBracket,
    CloseSqaureBracket,
}

#[derive(Debug, PartialEq)]
pub enum Literal<'a> {
    String(&'a str),
    Int(i64),
    Float(f64),
}

pub fn tokenize(
    input: &str,
) -> ParserIterator<
    Span,
    nom::error::Error<Span>,
    impl FnMut(Span) -> IResult<Span, Token, nom::error::Error<Span>>,
> {
    iterator(Span::new(input), token)
}

fn token(i: Span) -> IResult<Span, Token> {
    preceded(multispace0, alt((number, string, keyword, symbol, ident))).parse(i)
}

macro_rules! keywords {
    ($($word:literal => $token:path),+ $(,)?) => {
        alt((
            $(tag($word)),+
        )).map(|word| Token {
            span: word,
            token: TokenType::Keyword(match *word.fragment() {
                $($word => $token),+,
                _ => unreachable!("There is a tag with no matching keyword"),
            }),
        })
    };
}

fn keyword(i: Span) -> IResult<Span, Token> {
    keywords! {
        "let" => Keyword::Let,
        "fn" => Keyword::Function,
        "if" => Keyword::If,
        "else" => Keyword::Else,
        "continue" => Keyword::Continue,
        "break" => Keyword::Break,
        "true" => Keyword::True,
        "false" => Keyword::False,
    }
    .parse(i)
}

macro_rules! symbols {
    ($($symbol:literal => $token:path),+ $(,)?) => {
        alt((
            $(tag($symbol)),+
        )).map(|word| Token {
            span: word,
            token: TokenType::Symbol(match *word.fragment() {
                $($symbol => $token),+,
                _ => unreachable!("There is a tag with no matching keyword"),
            }),
        })
    };
}

fn symbol(i: Span) -> IResult<Span, Token> {
    alt((
        symbols! {
            "||" => Symbol::DoublePipe,
            "&&" => Symbol::DoubleAnd,
            "==" => Symbol::DoubleEquals,
            "-=" => Symbol::DashEquals,
            "+=" => Symbol::PlusEquals,
            "*=" => Symbol::AsteriskEquals,
            "/=" => Symbol::SlashEquals,
            "<=" => Symbol::LessThanEquals,
            ">=" => Symbol::GreaterThanEquals,
        },
        symbols! {
            "=" => Symbol::Equals,
            ";" => Symbol::SemiColon,
            "-" => Symbol::Dash,
            "+" => Symbol::Plus,
            "/" => Symbol::Slash,
            "*" => Symbol::Asterisk,
            "<" => Symbol::LessThan,
            ">" => Symbol::GreaterThan,
            "|" => Symbol::Pipe,
            "&" => Symbol::And,
            "%" => Symbol::Percent,
            "(" => Symbol::OpenParen,
            ")" => Symbol::CloseParen,
            "{" => Symbol::OpenCurlyBrace,
            "}" => Symbol::CloseCurlyBrace,
            "[" => Symbol::OpenSqaureBracket,
            "]" => Symbol::CloseSqaureBracket,
            "!" => Symbol::Exclamation,
        },
    ))
    .parse(i)
}

fn ident(i: Span) -> IResult<Span, Token> {
    recognize(tuple((
        alpha1,
        many0(preceded(many0(one_of("_-")), alphanumeric1)),
    )))
    .map(|word| Token {
        span: word,
        token: TokenType::Ident(*word.fragment()),
    })
    .parse(i)
}

fn string(i: Span) -> IResult<Span, Token> {
    delimited(char('"'), take_until("\""), char('"'))
        .map(|word| Token {
            span: word,
            token: TokenType::Literal(Literal::String(*word.fragment())),
        })
        .parse(i)
}

// fn int_literal(i: Span) -> IResult<Span, Token> {
//     let (i, span) = peek(recognize(i64)).parse(i)?;
//     let (i, value) = i64.parse(i)?;
//     Ok((
//         i,
//         Token {
//             span,
//             token: TokenType::Literal(Literal::Int(value)),
//         },
//     ))
// }

fn number(i: Span) -> IResult<Span, Token> {
    let (i, span) = peek(recognize(double)).parse(i)?;
    let (i, value) = double.parse(i)?;
    Ok((
        i,
        Token {
            span,
            token: TokenType::Literal(if value.fract() == 0.0 {
                Literal::Int(value as i64)
            } else {
                Literal::Float(value)
            }),
        },
    ))
}
