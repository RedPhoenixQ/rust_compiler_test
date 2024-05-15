#[cfg(test)]
mod test {
    use chumsky::prelude::*;

    #[test]
    fn test() {
        #[derive(Debug, PartialEq)]
        enum Expr {
            Int(i64),
            Let {
                ident: String,
                value: Option<Box<Expr>>,
            },
        }

        let r#let = text::keyword::<char, _, Simple<char>>("let")
            .labelled("let keyword")
            .padded()
            .ignore_then(text::ident().labelled("variable name"))
            .then(
                just('=')
                    .labelled("let assign operator")
                    .padded()
                    .ignore_then(
                        text::digits(10)
                            .labelled("value")
                            .map(|d: String| d.parse::<i64>().unwrap())
                            .map(Expr::Int)
                            .map(Box::new),
                    )
                    .padded()
                    .or_not(),
            )
            .then_ignore(just(';').ignored().or(end()).labelled("End of expression"))
            .map(|(ident, value)| Expr::Let { ident, value });

        assert_eq!(
            r#let.parse("let haha = 12"),
            Ok(Expr::Let {
                ident: "haha".to_string(),
                value: Some(Box::new(Expr::Int(12)))
            })
        );
        assert_eq!(
            r#let.parse("let haha;"),
            Ok(Expr::Let {
                ident: "haha".to_string(),
                value: None,
            })
        );

        match r#let.parse("let ") {
            Err(err) => {
                assert_eq!(1, err.len());
                let simple = err.first().unwrap();
                assert_eq!(simple.span(), 4..4);
            }
            res => panic!("Incorrect error, recived {:?}", res),
        }

        match r#let.parse("let haha = ") {
            Err(err) => {
                assert_eq!(1, err.len());
                let simple = err.first().unwrap();
                assert_eq!(simple.span(), 11..11);
                assert_eq!(simple.label(), Some("value"));
            }
            res => panic!("Incorrect error, recived {:?}", res),
        }
    }
}
