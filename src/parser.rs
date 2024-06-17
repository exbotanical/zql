use crate::{
    lexer::Lexer,
    stmt::Statement,
    token::{self, Token, TokenKind},
};

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Parser {
        Parser { tokens, pos: 0 }
    }

    fn parse(&mut self) -> Result<(), ()> {
        if self.tokens[self.tokens.len() - 2] != Token::Semicolon {
            return Err(());
        }

        let statements: Vec<Statement> = Vec::new();

        let stmt_kws = [TokenKind::Insert, TokenKind::Select, TokenKind::Create];
        loop {
            if self.matches_any(&stmt_kws) {}
        }

        Ok(())
    }

    fn peek(&self) -> &Token {
        match self.tokens.get(self.pos) {
            Some(t) => t,
            None => &Token::Eof,
        }
    }

    fn next(&mut self) -> &Token {
        match self.tokens.get(self.pos) {
            Some(t) => {
                self.pos = self.pos + 1;
                t
            }
            None => &Token::Eof,
        }
    }

    fn check(&self, t: TokenKind) -> bool {
        TokenKind::from(self.peek()) == t
    }

    fn consume(&mut self, t: TokenKind, msg: &str) -> Result<&Token, ()> {
        if self.check(t) {
            Ok(self.next())
        } else {
            Err(())
        }
    }

    fn matches_any(&mut self, ts: &[TokenKind]) -> bool {
        for t in ts {
            if self.check(*t) {
                return true;
            }
        }

        false
    }

    fn parse_stmt(&mut self) -> Result<Statement, ()> {
        let token = self.next();

        match token {
            Token::Select => self.parse_select(),
            Token::Insert => self.parse_insert(),
            Token::Create => self.parse_create_table(),
            _ => Err(()),
        }
    }

    fn parse_select(&mut self) -> Result<Statement, ()> {
        let mut columns: Vec<&str> = Vec::new();

        if self.check(TokenKind::Star) {
            match self.consume(TokenKind::Star, "expect *") {
                Ok(_) => columns.push("*"),
                Err(e) => return Err(e),
            }
        } else {
            let cs: Vec<&str> = self.parse_list().into_iter().map(|s| s.as_str()).collect();

            columns.concat();
        }

        Err(())
    }

    fn parse_insert(&mut self) -> Result<Statement, ()> {
        Err(())
    }

    fn parse_create_table(&mut self) -> Result<Statement, ()> {
        Err(())
    }

    fn parse_list(&mut self) -> Vec<String> {
        let mut columns: Vec<String> = Vec::new();

        while self.check(TokenKind::Identifier) {
            match self.consume(TokenKind::Identifier, "expect ident") {
                Ok(Token::Identifier(v)) => {
                    // TODO: ref?
                    columns.push(v.to_owned());
                }
                _ => panic!("TODO:"),
            }

            if self.check(TokenKind::Comma) {
                // TODO: result
                self.consume(TokenKind::Comma, "expect comma");
            } else {
                break;
            }
        }

        columns
    }
}
// }

#[cfg(test)]
mod tests {
    #[test]
    fn test_d() {
        let to = vec![1];
    }
}
