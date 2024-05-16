use anyhow::Result;

#[derive(Debug)]
enum Ast<'a> {
    Let { ident: &'a str },
}

struct Parser<'a> {
    code: &'a str,
    line: usize,
    offset: usize,
    last_line_offset: usize,
}

#[derive(Debug, Clone, Copy)]
enum ParseErr {
    MissingDelimiter,
    MissingTerminator,
}

impl std::error::Error for ParseErr {}

impl std::fmt::Display for ParseErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl<'a> Parser<'a> {
    fn new(code: &'a str) -> Self {
        Self {
            code,
            line: 1,
            offset: 0,
            last_line_offset: 0,
        }
    }

    fn consume(&mut self, n: usize) -> &str {
        self.offset += n.min(self.code.len());
        let consumed = &self.code[..n];
        self.code = &self.code[n..];
        consumed
    }

    fn skip_whitespace(&mut self) {
        for (i, c) in self.code.char_indices() {
            if !c.is_whitespace() {
                break;
            }
            self.code = &self.code[i..];
            self.offset += 1;
            if c == '\n' {
                self.last_line_offset = self.offset;
                self.line += 1;
            }
        }
    }

    fn column(&self) -> usize {
        1 + self.offset - self.last_line_offset
    }

    fn next(&mut self) -> Result<Ast> {
        self.skip_whitespace();
        anyhow::bail!("Unhandled")
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn slice_str() {
        let s = "haha yeet";
        assert_eq!(&s[5..], "yeet");
        assert_eq!(&s[0..], "haha yeet");
    }

    #[test]
    fn skip_whitespace() {
        let s = "haha yeet";
        assert_eq!(&s[5..], "yeet")
    }

    #[test]
    fn variable_declaration() {
        let code = r#"const new_variable = 123;
        let var1;"#;
        let mut parser = Parser::new(code);
        for _ in 0..2 {
            let ast = parser.next();
            assert!(ast.is_ok(), "Error: {:?}", ast);
        }
    }

    #[test]
    #[should_panic = ""]
    fn variable_declaration_no_value() {
        let code = r#"const new_variable"#;
        let mut parser = Parser::new(code);
        parser.next().expect("const variable without");
    }
}
