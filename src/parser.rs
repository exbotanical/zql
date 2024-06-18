use crate::{
    pos::WithPosRange,
    stmt::{Expression, Literal, Statement},
    token::{Operator, Token, TokenKind},
};

static EOF_TOKEN: WithPosRange<Token> = WithPosRange::empty(Token::Eof);

pub struct Parser<'a> {
    tokens: &'a [WithPosRange<Token>],
    cursor: usize,
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [WithPosRange<Token>]) -> Parser {
        Parser { tokens, cursor: 0 }
    }

    fn parse(&mut self) -> Result<(), ()> {
        // if self.tokens[self.tokens.len() - 2] != Token::Semicolon {
        //     return Err(());
        // }

        let statements: Vec<Statement> = Vec::new();

        // TODO: hoise
        let stmt_kws = [TokenKind::Insert, TokenKind::Select, TokenKind::Create];
        loop {
            if self.matches_any(&stmt_kws) {}
        }

        Ok(())
    }

    fn peek(&self) -> &'a WithPosRange<Token> {
        match self.tokens.get(self.cursor) {
            Some(t) => t,
            None => &EOF_TOKEN,
        }
    }

    fn next(&mut self) -> &'a WithPosRange<Token> {
        match self.tokens.get(self.cursor) {
            Some(t) => {
                self.cursor = self.cursor + 1;
                t
            }
            None => &EOF_TOKEN,
        }
    }

    fn check(&self, t: TokenKind) -> bool {
        TokenKind::from(self.peek()) == t
    }

    fn consume(&mut self, t: TokenKind, msg: &str) -> Result<&WithPosRange<Token>, String> {
        if self.check(t) {
            Ok(self.next())
        } else {
            Err("TODO:".to_string())
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

    fn parse_stmt(&mut self) -> Result<Statement, String> {
        let token = self.next();

        match token.value {
            Token::Select => self.parse_select(),
            Token::Insert => self.parse_insert(),
            Token::Create => self.parse_create_table(),
            _ => Err("TODO:".to_string()),
        }
    }

    fn parse_select(&mut self) -> Result<Statement, String> {
        let mut columns: Vec<String> = Vec::new();

        if self.check(TokenKind::Star) {
            match self.consume(TokenKind::Star, "expect *") {
                Ok(_) => columns.push("*".into()),
                Err(e) => return Err(e),
            }
        } else {
            columns.append(&mut self.parse_list());
        }

        self.consume(TokenKind::From, "expect FROM").expect("TODO:");

        let source = match self.consume(TokenKind::Identifier, "expect ident") {
            Ok(v) => v,
            Err(e) => panic!("TODO:"),
        };

        self.consume(TokenKind::Where, "expect WHERE")
            .expect("TODO:");

        Err("TODO:".to_string())
    }

    fn parse_insert(&mut self) -> Result<Statement, String> {
        Err("TODO:".to_string())
    }

    fn parse_create_table(&mut self) -> Result<Statement, String> {
        Err("TODO:".to_string())
    }

    fn parse_list(&mut self) -> Vec<String> {
        let mut columns: Vec<String> = Vec::new();

        while self.check(TokenKind::Identifier) {
            match &self.next().value {
                Token::Identifier(v) => {
                    columns.push(v.to_owned());
                }
                _ => panic!("TODO:"),
            }

            if self.check(TokenKind::Comma) {
                self.consume(TokenKind::Comma, "expect comma")
                    .expect("TODO:");
            } else {
                break;
            }
        }

        columns
    }

    fn parse_exprs(&mut self) -> Result<Vec<Expression>, String> {
        let mut exprs: Vec<Expression> = Vec::new();

        // TODO: hoist
        let until_kws = [TokenKind::And, TokenKind::Or];

        loop {
            let expr = self.parse_expr()?;
            exprs.push(expr);

            // TODO: cleanup
            if !self.matches_any(&until_kws) {
                break;
            } else {
                self.next();
            }
        }

        Ok(exprs)
    }

    fn parse_expr(&mut self) -> Result<Expression, String> {
        let left = match &self.next().value {
            Token::Identifier(v) => v.to_owned(),
            _ => return Err("TODO:".to_string()),
        };
        let operator: Operator = self.next().value.clone().try_into()?;
        let right = self.parse_literal()?;

        Ok(Expression {
            left,
            operator,
            right,
        })
    }

    fn parse_literal(&mut self) -> Result<Literal, String> {
        match &self.next().value {
            Token::False => Ok(Literal::Boolean(false)),
            Token::True => Ok(Literal::Boolean(true)),
            Token::Nil => Ok(Literal::Nil),
            Token::UnsignedInt(v) => Ok(Literal::UnsignedInt(*v)),
            Token::Float(v) => Ok(Literal::Float(*v)),
            Token::String(v) => Ok(Literal::String(v.to_owned())),
            _ => Err("TODO:".to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        lexer::Lexer,
        stmt::{Expression, Literal},
        token::Operator,
    };

    use super::Parser;

    // TODO:
    // fn get_p(text: &str) -> Parser<'static> {
    //     let mut l = Lexer::new(text);
    //     let tokens = l.tokenize();
    //     let mut p = Parser::new(&tokens);

    //     p
    // }

    fn parse_literal(text: &str) -> Result<Literal, String> {
        let mut l = Lexer::new(text);
        let tokens = l.tokenize();
        let mut p = Parser::new(&tokens);

        p.parse_literal()
    }

    fn parse_list(text: &str) -> Vec<String> {
        let mut l = Lexer::new(text);
        let tokens = l.tokenize();
        let mut p = Parser::new(&tokens);

        p.parse_list()
    }

    fn parse_expr(text: &str) -> Result<Expression, String> {
        let mut l = Lexer::new(text);
        let tokens = l.tokenize();
        let mut p = Parser::new(&tokens);

        p.parse_expr()
    }

    fn parse_exprs(text: &str) -> Result<Vec<Expression>, String> {
        let mut l = Lexer::new(text);
        let tokens = l.tokenize();
        let mut p = Parser::new(&tokens);

        p.parse_exprs()
    }

    #[test]
    fn test_parse_literal_ok() {
        assert_eq!(
            parse_literal("\"text\"").expect("expected valid literal"),
            Literal::String("text".to_string())
        );

        assert_eq!(
            parse_literal("100").expect("expected valid literal"),
            Literal::UnsignedInt(100)
        );

        assert_eq!(
            parse_literal("100.100").expect("expected valid literal"),
            Literal::Float(100.100)
        );

        assert_eq!(
            parse_literal("true").expect("expected valid literal"),
            Literal::Boolean(true)
        );

        assert_eq!(
            parse_literal("false").expect("expected valid literal"),
            Literal::Boolean(false)
        );

        assert_eq!(
            parse_literal("nil").expect("expected valid literal"),
            Literal::Nil
        );
    }

    #[test]
    fn test_parse_literal_err_not_literal() {
        assert_eq!(parse_literal("nile").unwrap_err(), "TODO:");
    }

    #[test]
    fn test_parse_literal_err_empty() {
        assert_eq!(parse_literal("").unwrap_err(), "TODO:");
    }

    #[test]
    fn test_parse_list() {
        let list = parse_list("list, test, yay");

        assert_eq!(list.len(), 3);
        assert_eq!(list[0], "list");
        assert_eq!(list[1], "test");
        assert_eq!(list[2], "yay");
    }

    #[test]
    fn test_parse_list_single_el() {
        let list = parse_list("test");

        assert_eq!(list.len(), 1);
        assert_eq!(list[0], "test");
    }

    #[test]
    fn test_parse_list_single_el_trailing_comma() {
        let list = parse_list("test,");

        assert_eq!(list.len(), 1);
        assert_eq!(list[0], "test");
    }

    #[test]
    fn test_parse_list_empty() {
        let list = parse_list("");

        assert_eq!(list.len(), 0);
    }

    #[test]
    fn test_parse_expr_ok() {
        let maybe_expr = parse_expr("hello == \"world\"").expect("expected valid expression");

        assert_eq!(maybe_expr.left, "hello");
        assert_eq!(maybe_expr.operator, Operator::EqualEqual);
        assert_eq!(maybe_expr.right, Literal::String("world".to_string()));

        let maybe_expr = parse_expr("hello != true").expect("expected valid expression");

        assert_eq!(maybe_expr.left, "hello");
        assert_eq!(maybe_expr.operator, Operator::BangEqual);
        assert_eq!(maybe_expr.right, Literal::Boolean(true));

        let maybe_expr = parse_expr("hello > 9").expect("expected valid expression");

        assert_eq!(maybe_expr.left, "hello");
        assert_eq!(maybe_expr.operator, Operator::Greater);
        assert_eq!(maybe_expr.right, Literal::UnsignedInt(9));

        // TODO: disallow comp on bools and null
        let maybe_expr = parse_expr("hello >= \"world\"").expect("expected valid expression");

        assert_eq!(maybe_expr.left, "hello");
        assert_eq!(maybe_expr.operator, Operator::GreaterEqual);
        assert_eq!(maybe_expr.right, Literal::String("world".to_string()));

        let maybe_expr = parse_expr("hello < 100.21").expect("expected valid expression");

        assert_eq!(maybe_expr.left, "hello");
        assert_eq!(maybe_expr.operator, Operator::Less);
        assert_eq!(maybe_expr.right, Literal::Float(100.21));

        let maybe_expr = parse_expr("hello <= \"world\"").expect("expected valid expression");

        assert_eq!(maybe_expr.left, "hello");
        assert_eq!(maybe_expr.operator, Operator::LessEqual);
        assert_eq!(maybe_expr.right, Literal::String("world".to_string()));
    }

    #[test]
    fn test_parse_expr_err_no_right() {
        let err = parse_expr("hello <=").unwrap_err();
        assert_eq!(err, "TODO:");
    }

    #[test]
    fn test_parse_expr_err_no_left() {
        let err = parse_expr(" <= \"world\"").unwrap_err();
        assert_eq!(err, "TODO:");
    }

    #[test]
    fn test_parse_expr_err_no_operator() {
        let err = parse_expr("hello \"world\"").unwrap_err();

        assert_eq!(err, "value TODO: impl display is not a valid operator");
    }

    #[test]
    fn test_parse_expr_err_invalid_operator() {
        let err = parse_expr("hello ! \"world\"").unwrap_err();

        assert_eq!(err, "value TODO: impl display is not a valid operator");
    }

    #[test]
    fn test_parse_expr_err_non_literal_right() {
        let err = parse_expr("hello == CREATE").unwrap_err();

        assert_eq!(err, "TODO:");
    }

    #[test]
    fn test_parse_exprs_ok() {
        let maybe_exprs =
            parse_exprs("hello == \"world\" AND won == 3").expect("expected valid expressions");
    }
}
