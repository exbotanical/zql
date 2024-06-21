use crate::{
    pos::WithPosMetadata,
    stmt::{
        Binary, ColumnConfig, ColumnType, CreateTableStatement, Expr, Grouping, Identifier,
        InsertStatement, Literal, Logical, SelectOpt, SelectStatement, Statement,
    },
    token::{Operator, Token, TokenKind},
};

static EOF_TOKEN: WithPosMetadata<Token> = WithPosMetadata::empty(Token::Eof);

static STATEMENT_KEYWORDS: [TokenKind; 3] =
    [TokenKind::Insert, TokenKind::Select, TokenKind::Create];
static COMPARISON_TOKENS: [TokenKind; 4] = [
    TokenKind::Greater,
    TokenKind::GreaterEqual,
    TokenKind::Less,
    TokenKind::LessEqual,
];
static EQUALITY_TOKENS: [TokenKind; 2] = [TokenKind::BangEqual, TokenKind::EqualEqual];

pub struct Parser {
    tokens: Vec<WithPosMetadata<Token>>,
    cursor: usize,
}

impl Parser {
    pub fn new(tokens: Vec<WithPosMetadata<Token>>) -> Parser {
        Parser { tokens, cursor: 0 }
    }

    pub fn parse(&mut self) -> Result<Vec<Statement>, String> {
        let mut statements: Vec<Statement> = Vec::new();

        while self.matches_any(&STATEMENT_KEYWORDS) {
            statements.push(self.parse_stmt()?);
        }

        let next = self.next();
        match next.value {
            Token::Eof => Ok(statements),
            _ => Err(format!(
                "Expected a new statement to begin after `;` at Ln {}",
                next.pos.line,
            )),
        }
    }

    fn peek(&self) -> &WithPosMetadata<Token> {
        match self.tokens.get(self.cursor) {
            Some(t) => t,
            None => &EOF_TOKEN,
        }
    }

    fn next(&mut self) -> &WithPosMetadata<Token> {
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

    fn matches_any(&self, ts: &[TokenKind]) -> bool {
        for t in ts {
            if self.check(*t) {
                return true;
            }
        }

        false
    }

    fn consume(&mut self, t: TokenKind, msg: &str) -> Result<&WithPosMetadata<Token>, String> {
        if self.check(t) {
            Ok(self.next())
        } else {
            Err(msg.to_string())
        }
    }

    fn parse_stmt(&mut self) -> Result<Statement, String> {
        // TODO: lower the first kw down into stmt?
        let result = match &self.next().value {
            Token::Select => self.parse_select(),
            Token::Insert => self.parse_insert(),
            Token::Create => self.parse_create_table(),
            _ => return Err("Expected a valid statement".to_string()),
        };

        match &self.next().value {
            Token::Semicolon => result,
            _ => Err("Expected statement to be terminated by ';'".to_string()),
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
            let mut cols = self.parse_columns()?;
            if cols.len() == 0 {
                return Err("Expected at least 1 column in SELECT statement".to_string());
            }

            columns.append(&mut cols);
        }

        self.consume(TokenKind::From, "Expected 'FROM' keyword after columns")?;

        let source = match &self.next().value {
            Token::Identifier(v) => v,
            _ => return Err("Expected a source table name".to_string()),
        }
        .to_owned();

        if !self.check(TokenKind::Where) {
            return Ok(Statement::Select(SelectStatement {
                source,
                columns,
                expr: Expr::None,
                opt: SelectOpt::None,
            }));
        }

        self.consume(TokenKind::Where, "expect WHERE")?;

        let expr = self.parse_expr()?;

        let opt = if self.check(TokenKind::Limit) {
            self.parse_opt()?
        } else {
            SelectOpt::None
        };

        Ok(Statement::Select(SelectStatement {
            source,
            columns,
            expr,
            opt,
        }))
    }

    fn parse_insert(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::Into, "Expected 'INTO' keyword")?;

        let dest = match &self.next().value {
            Token::Identifier(v) => v.to_owned(),
            _ => return Err("Expected a destination table name".to_string()),
        };

        self.consume(TokenKind::LeftParen, "Expected '(' before columns")?;
        let columns = self.parse_columns()?;
        self.consume(TokenKind::RightParen, "Expected ')' after columns")?;
        if columns.len() == 0 {
            return Err("Expected at least one column".to_string());
        }

        self.consume(TokenKind::Values, "Expected 'VALUES' keyword")?;
        self.consume(TokenKind::LeftParen, "Expected '(' before values")?;
        let values = self.parse_literals()?;
        self.consume(TokenKind::RightParen, "Expected ')' after values")?;

        if columns.len() != values.len() {
            return Err(format!(
                "Mismatched columns and values; expected {} values but got {}",
                columns.len(),
                values.len()
            )
            .to_string());
        }

        Ok(Statement::Insert(InsertStatement {
            dest,
            columns,
            values,
        }))
    }

    fn parse_create_table(&mut self) -> Result<Statement, String> {
        self.consume(TokenKind::Table, "Expected 'TABLE' keyword")?;

        let name = match &self.next().value {
            Token::Identifier(v) => v.to_owned(),
            _ => return Err("Expected a valid table name".to_string()),
        };

        self.consume(TokenKind::LeftParen, "Expected '(' before columns")?;

        let mut columns: Vec<ColumnConfig> = Vec::new();

        // TODO: Generalize
        loop {
            match &self.peek().value {
                Token::Identifier(_) => {
                    columns.push(self.parse_column_config()?);
                }
                _ => return Err("Expected a valid column name".to_string()),
            }

            if !self.check(TokenKind::Comma) {
                break;
            } else {
                self.next();
            }
        }

        self.consume(TokenKind::RightParen, "Expected ')' after columns")?;

        Ok(Statement::CreateTable(CreateTableStatement {
            name,
            columns,
        }))
    }

    fn parse_columns(&mut self) -> Result<Vec<String>, String> {
        let mut columns: Vec<String> = Vec::new();

        loop {
            match &self.next().value {
                Token::Identifier(v) => {
                    columns.push(v.to_owned());
                }
                _ => return Err("Expected a valid column name".to_string()),
            }

            if !self.check(TokenKind::Comma) {
                break;
            } else {
                self.next();
            }
        }

        Ok(columns)
    }

    fn parse_column_config(&mut self) -> Result<ColumnConfig, String> {
        let name = match &self.next().value {
            Token::Identifier(v) => v.to_owned(),
            _ => return Err("Expected a valid column name".to_string()),
        };

        let t = match &self.next().value {
            // TODO: try_from?
            Token::Int => ColumnType::Int,
            Token::Text => ColumnType::Text,
            // TODO: get token value somehow?
            _ => return Err("Expected a valid column type".to_string()),
        };

        Ok(ColumnConfig { name, t })
    }

    fn parse_opt(&mut self) -> Result<SelectOpt, String> {
        match &self.next().value {
            Token::Limit => match &self.next().value {
                Token::UnsignedInt(v) => Ok(SelectOpt::Limit(*v)),
                _ => Err("Expected an unsigned integer".to_string()),
            },
            _ => Err("Expected a valid opt type".to_string()),
        }
    }

    fn parse_literals(&mut self) -> Result<Vec<Literal>, String> {
        let mut literals: Vec<Literal> = Vec::new();

        loop {
            literals.push(self.parse_literal()?);

            if !self.check(TokenKind::Comma) {
                break;
            } else {
                self.next();
            }
        }

        Ok(literals)
    }

    fn parse_literal(&mut self) -> Result<Literal, String> {
        match &self.next().value {
            // TODO: try_into?
            Token::False => Ok(Literal::Boolean(false)),
            Token::True => Ok(Literal::Boolean(true)),
            Token::Nil => Ok(Literal::Nil),
            Token::UnsignedInt(v) => Ok(Literal::UnsignedInt(*v)),
            Token::Float(v) => Ok(Literal::Float(*v)),
            Token::String(v) => Ok(Literal::String(v.to_owned())),
            _ => Err("Expected a valid literal".to_string()),
        }
    }

    fn parse_expr(&mut self) -> Result<Expr, String> {
        match self.parse_or()? {
            Expr::Literal(_) | Expr::Identifier(_) => Err("Expected an expression".to_string()),
            expr => Ok(expr),
        }
    }

    fn parse_or(&mut self) -> Result<Expr, String> {
        let mut expr = self.parse_and(true)?;

        while self.check(TokenKind::Or) {
            self.consume(TokenKind::Or, "Expected 'OR'")?;

            let right = self.parse_and(false)?;
            expr = Expr::Logical(Logical {
                left: Box::new(expr),
                operator: Operator::Or,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn parse_and(&mut self, is_left: bool) -> Result<Expr, String> {
        let mut expr = self.parse_eq(is_left)?;

        while self.check(TokenKind::And) {
            self.consume(TokenKind::And, "Expected 'AND'")?;

            let right = self.parse_eq(false)?;
            expr = Expr::Logical(Logical {
                left: Box::new(expr),
                operator: Operator::And,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self, is_left: bool) -> Result<Expr, String> {
        let mut expr = if is_left {
            self.parse_left_primary()
        } else {
            self.parse_right_primary()
        }?;

        while self.matches_any(&COMPARISON_TOKENS) {
            let token = self.next().value.clone();
            let right = self.parse_right_primary()?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator: token.try_into()?,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn parse_eq(&mut self, is_left: bool) -> Result<Expr, String> {
        let mut expr: Expr = self.parse_comparison(is_left)?;

        while self.matches_any(&EQUALITY_TOKENS) {
            let token = self.next().value.clone();
            let right: Expr = self.parse_comparison(false)?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator: token.try_into()?,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn parse_left_primary(&mut self) -> Result<Expr, String> {
        match &self.next().value {
            Token::Identifier(v) => Ok(Expr::Identifier(Identifier {
                value: v.to_owned(),
            })),
            Token::LeftParen => {
                let expr = self.parse_expr()?;
                self.consume(
                    TokenKind::RightParen,
                    "Expected ')' after nested expression",
                )?;

                Ok(Expr::Grouping(Grouping {
                    expr: Box::new(expr),
                }))
            }
            _ => Err(
                "Expected either an identifier or nested expression for left side of expression"
                    .to_string(),
            ),
        }
    }

    fn parse_right_primary(&mut self) -> Result<Expr, String> {
        match &self.next().value {
            // TODO: try_into?
            // TODO: dedupe parse_literal
            Token::False => Ok(Expr::Literal(Literal::Boolean(false))),
            Token::True => Ok(Expr::Literal(Literal::Boolean(true))),
            Token::Nil => Ok(Expr::Literal(Literal::Nil)),
            Token::UnsignedInt(v) => Ok(Expr::Literal(Literal::UnsignedInt(*v))),
            Token::Float(v) => Ok(Expr::Literal(Literal::Float(*v))),
            Token::String(v) => Ok(Expr::Literal(Literal::String(v.to_owned()))),
            Token::Identifier(v) => Ok(Expr::Identifier(Identifier {
                value: v.to_owned(),
            })),
            Token::LeftParen => {
                let expr = self.parse_expr()?;
                self.consume(
                    TokenKind::RightParen,
                    "Expected ')' after nested expression",
                )?;

                Ok(Expr::Grouping(Grouping {
                    expr: Box::new(expr),
                }))
            }
            _ => Err("Expected either a primitive, identifier, or nested expression for right side of expression".to_string()),
        }
    }
}

#[cfg(test)]
mod tests {
    use lazy_static::lazy_static;

    use crate::{
        lexer::Lexer,
        stmt::{
            Binary, ColumnConfig, ColumnType, CreateTableStatement, Expr, Grouping, Identifier,
            InsertStatement, Literal, Logical, SelectOpt, SelectStatement, Statement,
        },
        token::{Operator, Token, TokenKind},
    };

    use super::Parser;

    lazy_static! {
        #[derive(Debug)]
        static ref SELECT_STMT: Statement = Statement::Select(SelectStatement {
            source: "blog_post".to_string(),
            columns: vec!["uuid", "title", "subtitle", "img_src", "slug", "tags", "body",]
                .into_iter()
                .map(|s| s.to_string())
                .collect(),
            expr: Expr::Binary(Binary{
              left: Box::new(Expr::Identifier(Identifier{value:"slug".to_string()})),
              operator: Operator::EqualEqual,
              right: Box::new(Expr::Literal(Literal::String("hello".to_string())))
            }),
            opt: SelectOpt::Limit(1),
        });

        static ref INSERT_STMT: Statement = Statement::Insert(InsertStatement {
                columns: vec!["title", "subtitle", "img_src", "slug", "tags", "body"]
                    .into_iter()
                    .map(|s| s.to_string())
                    .collect(),
                dest: "blog_post".to_string(),
                values: vec![
                    Literal::String("a".to_string()),
                    Literal::String("b".to_string()),
                    Literal::Nil,
                    Literal::UnsignedInt(1),
                    Literal::Float(2.0),
                    Literal::Boolean(true)
                ]
            });

            static ref CREATE_TABLE_STMT: Statement = Statement::CreateTable(CreateTableStatement {
                name: "blog_post_comment".to_string(),
                columns: vec![
                    ColumnConfig {
                        name: "uuid".to_string(),
                        t: ColumnType::Text
                    },
                    ColumnConfig {
                        name: "created_at".to_string(),
                        t: ColumnType::Int
                    },
                    ColumnConfig {
                        name: "comment".to_string(),
                        t: ColumnType::Text
                    },
                    ColumnConfig {
                        name: "email".to_string(),
                        t: ColumnType::Text
                    }
                ]
            });
    }

    fn get_parser(text: &str) -> Parser {
        let mut l = Lexer::new(text);
        let tokens = l.tokenize();

        Parser::new(tokens)
    }

    #[test]
    fn test_peek() {
        let mut p = get_parser("SELECT hello FROM world;");

        assert_eq!(p.peek().value, Token::Select);
        p.next();
        assert_eq!(p.peek().value, Token::Identifier("hello".to_string()));
        p.next();
        assert_eq!(p.peek().value, Token::From);
        p.next();
        assert_eq!(p.peek().value, Token::Identifier("world".to_string()));
        p.next();
        assert_eq!(p.peek().value, Token::Semicolon);
        p.next();
        assert_eq!(p.peek().value, Token::Eof);
        assert_eq!(p.peek().value, Token::Eof);
        p.next();
        assert_eq!(p.peek().value, Token::Eof);
    }

    #[test]
    fn test_peek_no_tokens() {
        let mut p = get_parser("");

        assert_eq!(p.peek().value, Token::Eof);
        assert_eq!(p.peek().value, Token::Eof);
        p.next();
        assert_eq!(p.peek().value, Token::Eof);
    }

    #[test]
    fn test_next() {
        let mut p = get_parser("SELECT hello FROM world;");

        assert_eq!(p.next().value, Token::Select);
        assert_eq!(p.next().value, Token::Identifier("hello".to_string()));
        assert_eq!(p.next().value, Token::From);
        assert_eq!(p.next().value, Token::Identifier("world".to_string()));
        assert_eq!(p.next().value, Token::Semicolon);
        assert_eq!(p.next().value, Token::Eof);
        assert_eq!(p.next().value, Token::Eof);
        assert_eq!(p.next().value, Token::Eof);
    }

    #[test]
    fn test_next_no_tokens() {
        let mut p = get_parser("");

        assert_eq!(p.next().value, Token::Eof);
        assert_eq!(p.next().value, Token::Eof);
        assert_eq!(p.next().value, Token::Eof);
    }

    #[test]
    fn test_check() {
        let mut p = get_parser("SELECT hello FROM world;");

        assert_eq!(p.check(TokenKind::Select), true);
        assert_eq!(p.check(TokenKind::Identifier), false);
        p.next();
        assert_eq!(p.check(TokenKind::Identifier), true);
        p.next();
        assert_eq!(p.check(TokenKind::From), true);
        p.next();
        assert_eq!(p.check(TokenKind::Identifier), true);
        p.next();
        assert_eq!(p.check(TokenKind::Semicolon), true);
        p.next();
        assert_eq!(p.check(TokenKind::Eof), true);
        assert_eq!(p.check(TokenKind::Eof), true);
        p.next();
        assert_eq!(p.check(TokenKind::Eof), true);
    }

    #[test]
    fn test_matches_any() {
        let mut p = get_parser("SELECT hello FROM world;");

        assert_eq!(
            p.matches_any(&[TokenKind::Select, TokenKind::Identifier]),
            true
        );
        assert_eq!(p.matches_any(&[TokenKind::Identifier]), false);
        p.next();
        assert_eq!(
            p.matches_any(&[TokenKind::Identifier, TokenKind::And]),
            true
        );
        p.next();
        assert_eq!(
            p.matches_any(&[TokenKind::From, TokenKind::Or, TokenKind::Slash]),
            true
        );
        p.next();
        assert_eq!(p.matches_any(&[TokenKind::Identifier]), true);
        p.next();
        assert_eq!(p.matches_any(&[TokenKind::Eof, TokenKind::Semicolon]), true);
        p.next();
        assert_eq!(p.matches_any(&[TokenKind::Eof, TokenKind::Semicolon]), true);
        assert_eq!(p.matches_any(&[TokenKind::Eof]), true);
        p.next();
        assert_eq!(p.matches_any(&[TokenKind::Eof]), true);
    }

    #[test]
    fn test_consume() {
        let mut p = get_parser("SELECT hello FROM world;");

        assert_eq!(
            p.consume(TokenKind::Identifier, "expect ident")
                .unwrap_err(),
            "expect ident"
        );

        assert_eq!(
            p.consume(TokenKind::Select, "")
                .expect("expected valid token")
                .value,
            Token::Select
        );

        assert_eq!(
            p.consume(TokenKind::Identifier, "")
                .expect("expected valid token")
                .value,
            Token::Identifier("hello".to_string())
        );

        assert_eq!(
            p.consume(TokenKind::From, "")
                .expect("expected valid token")
                .value,
            Token::From
        );

        assert_eq!(
            p.consume(TokenKind::Identifier, "")
                .expect("expected valid token")
                .value,
            Token::Identifier("world".to_string())
        );

        assert_eq!(
            p.consume(TokenKind::Semicolon, "")
                .expect("expected valid token")
                .value,
            Token::Semicolon
        );

        assert_eq!(
            p.consume(TokenKind::Eof, "")
                .expect("expected valid token")
                .value,
            Token::Eof
        );

        assert_eq!(
            p.consume(TokenKind::Identifier, "expect ident")
                .unwrap_err(),
            "expect ident"
        );

        assert_eq!(
            p.consume(TokenKind::Eof, "")
                .expect("expected valid token")
                .value,
            Token::Eof
        );

        assert_eq!(
            p.consume(TokenKind::Eof, "")
                .expect("expected valid token")
                .value,
            Token::Eof
        );
    }

    #[test]
    fn test_parse() {
        let stmts = get_parser(
            "
        SELECT uuid, title, subtitle, img_src, slug, tags, body
        FROM blog_post
        WHERE slug == \"hello\"
        LIMIT 1;

        INSERT INTO blog_post(title, subtitle, img_src, slug, tags, body)
        VALUES(\"a\", \"b\", nil, 1, 2.0, true);

        // comment

        CREATE TABLE blog_post_comment(
            uuid TEXT,
            created_at INT,
            comment TEXT,
            // another comment
            email TEXT
        );
        ",
        )
        .parse()
        .expect("expected valid statements");

        assert_eq!(stmts.len(), 3);
        assert_eq!(stmts[0], *SELECT_STMT);
        assert_eq!(stmts[1], *INSERT_STMT);
        assert_eq!(stmts[2], *CREATE_TABLE_STMT);
    }

    #[test]
    fn test_parse_err_trailing_content() {
        let err = get_parser(
            "
        SELECT uuid, title, subtitle, img_src, slug, tags, body
        FROM blog_post
        WHERE slug == \"hello\"
        LIMIT 1;

        INSERT INTO blog_post(title, subtitle, img_src, slug, tags, body)
        VALUES(\"a\", \"b\", nil, 1, 2.0, true);

        // comment

        CREATE TABLE blog_post_comment(
            uuid TEXT,
            created_at INT,
            comment TEXT,
            // another comment
            email TEXT
        );

        something something
        ",
        )
        .parse()
        .unwrap_err();

        assert_eq!(err, "Expected a new statement to begin after `;` at Ln 20");
    }

    #[test]
    fn test_parse_stmt() {
        let stmt = get_parser(
            "
        SELECT uuid, title, subtitle, img_src, slug, tags, body
        FROM blog_post WHERE slug == \"hello\" LIMIT 1;",
        )
        .parse_stmt()
        .expect("expected a valid statement");

        assert_eq!(stmt, *SELECT_STMT)
    }

    #[test]
    fn test_parse_stmt_err_no_semi() {
        let err = get_parser(
            "
        SELECT uuid, title, subtitle, img_src, slug, tags, body
        FROM blog_post WHERE slug == \"hello\" LIMIT 1",
        )
        .parse_stmt()
        .unwrap_err();

        assert_eq!(
            err,
            "Expected statement to be terminated by ';'".to_string()
        )
    }

    #[test]
    fn test_parse_stmt_err_unknown_stmt() {
        let err = get_parser(
            "
        BLABLABLA uuid, title, subtitle, img_src, slug, tags, body
        FROM blog_post WHERE slug == \"hello\" LIMIT 1;",
        )
        .parse_stmt()
        .unwrap_err();

        assert_eq!(err, "Expected a valid statement".to_string())
    }

    #[test]
    fn test_parse_select_ok() {
        // SELECT literal will have been consumed already
        let stmt = get_parser(
            "
        uuid, title, subtitle, img_src, slug, tags, body
        FROM blog_post WHERE slug == \"hello\" LIMIT 1;",
        )
        .parse_select()
        .expect("expected valid SELECT statement");

        assert_eq!(stmt, *SELECT_STMT)
    }

    #[test]
    fn test_parse_select_ok_sans_opt() {
        let stmt = get_parser(
            "
        uuid, title, subtitle, img_src, slug, tags, body
        FROM blog_post WHERE slug == \"hello\";",
        )
        .parse_select()
        .expect("expected valid SELECT statement");

        assert_eq!(
            stmt,
            Statement::Select(SelectStatement {
                source: "blog_post".to_string(),
                columns: vec!["uuid", "title", "subtitle", "img_src", "slug", "tags", "body",]
                    .into_iter()
                    .map(|s| s.to_string())
                    .collect(),
                expr: Expr::Binary(Binary {
                    left: Box::new(Expr::Identifier(Identifier {
                        value: "slug".to_string()
                    })),
                    operator: Operator::EqualEqual,
                    right: Box::new(Expr::Literal(Literal::String("hello".to_string())))
                }),
                opt: SelectOpt::None
            })
        )
    }

    #[test]
    fn test_parse_select_ok_no_exprs() {
        let stmt = get_parser(
            "
        uuid, title, subtitle, img_src, slug, tags, body
        FROM blog_post;",
        )
        .parse_select()
        .expect("expected valid SELECT statement");

        assert_eq!(
            stmt,
            Statement::Select(SelectStatement {
                source: "blog_post".to_string(),
                columns: vec!["uuid", "title", "subtitle", "img_src", "slug", "tags", "body",]
                    .into_iter()
                    .map(|s| s.to_string())
                    .collect(),
                expr: Expr::None,
                opt: SelectOpt::None
            })
        )
    }

    #[test]
    fn test_parse_select_err_no_columns() {
        let err = get_parser(
            "
        FROM blog_post WHERE slug == \"hello\";",
        )
        .parse_select()
        .unwrap_err();

        assert_eq!(err, "Expected a valid column name".to_string())
    }

    #[test]
    fn test_parse_select_err_no_from() {
        let err = get_parser(
            "
        uuid, title, subtitle, img_src, slug, tags, body
         blog_post WHERE slug == \"hello\";",
        )
        .parse_select()
        .unwrap_err();

        assert_eq!(err, "Expected 'FROM' keyword after columns".to_string())
    }

    #[test]
    fn test_parse_insert_ok() {
        let stmt = get_parser(
            "
        INTO blog_post(title, subtitle, img_src, slug, tags, body)
        VALUES(\"a\", \"b\", nil, 1, 2.0, true);",
        )
        .parse_insert()
        .expect("expected valid insert statement");

        assert_eq!(
            stmt,
            Statement::Insert(InsertStatement {
                columns: vec!["title", "subtitle", "img_src", "slug", "tags", "body"]
                    .into_iter()
                    .map(|s| s.to_string())
                    .collect(),
                dest: "blog_post".to_string(),
                values: vec![
                    Literal::String("a".to_string()),
                    Literal::String("b".to_string()),
                    Literal::Nil,
                    Literal::UnsignedInt(1),
                    Literal::Float(2.0),
                    Literal::Boolean(true)
                ]
            })
        )
    }

    #[test]
    fn test_parse_insert_err_no_into() {
        let err = get_parser(
            "
         blog_post(title, subtitle, img_src, slug, tags, body)
        VALUES(\"a\", \"b\", nil, 1, 2.0, true);",
        )
        .parse_insert()
        .unwrap_err();

        assert_eq!(err, "Expected 'INTO' keyword");
    }

    #[test]
    fn test_parse_insert_err_no_dest() {
        let err = get_parser(
            "
        INTO (title, subtitle, img_src, slug, tags, body)
        VALUES(\"a\", \"b\", nil, 1, 2.0, true);",
        )
        .parse_insert()
        .unwrap_err();

        assert_eq!(err, "Expected a destination table name");
    }

    #[test]
    fn test_parse_insert_err_empty_columns() {
        let err = get_parser(
            "
        INTO blog_post()
        VALUES(\"a\", \"b\", nil, 1, 2.0, true);",
        )
        .parse_insert()
        .unwrap_err();

        assert_eq!(err, "Expected a valid column name");
    }

    #[test]
    fn test_parse_insert_err_no_columns_parens() {
        let err = get_parser(
            "
        INTO blog_post title, subtitle, img_src, slug, tags, body
        VALUES(\"a\", \"b\", nil, 1, 2.0, true);",
        )
        .parse_insert()
        .unwrap_err();

        assert_eq!(err, "Expected '(' before columns");
    }

    #[test]
    fn test_parse_insert_err_no_columns_no_parens() {
        let err = get_parser(
            "
        INTO blog_post
        VALUES(\"a\", \"b\", nil, 1, 2.0, true);",
        )
        .parse_insert()
        .unwrap_err();

        assert_eq!(err, "Expected '(' before columns");
    }

    #[test]
    fn test_parse_insert_err_no_values() {
        let err = get_parser(
            "
        INTO blog_post (title, subtitle, img_src, slug, tags, body)
        (\"a\", \"b\", nil, 1, 2.0, true);",
        )
        .parse_insert()
        .unwrap_err();

        assert_eq!(err, "Expected 'VALUES' keyword");
    }

    #[test]
    fn test_parse_insert_err_no_values_parens() {
        let err = get_parser(
            "
        INTO blog_post (title, subtitle, img_src, slug, tags, body)
        VALUES \"a\", \"b\", nil, 1, 2.0, true;",
        )
        .parse_insert()
        .unwrap_err();

        assert_eq!(err, "Expected '(' before values");
    }

    #[test]
    fn test_parse_insert_err_empty_values() {
        let err = get_parser(
            "
        INTO blog_post (title, subtitle, img_src, slug, tags, body)
        VALUES ();",
        )
        .parse_insert()
        .unwrap_err();

        assert_eq!(err, "Expected a valid literal");
    }

    #[test]
    fn test_parse_insert_err_no_values_no_parens() {
        let err = get_parser(
            "
        INTO blog_post (title, subtitle, img_src, slug, tags, body)
        VALUES;",
        )
        .parse_insert()
        .unwrap_err();

        assert_eq!(err, "Expected '(' before values");
    }

    #[test]
    fn test_parse_insert_err_mismatched_values() {
        let err = get_parser(
            "
        INTO blog_post(title, subtitle, img_src, slug, tags, body)
        VALUES (1,2,3,4,5);",
        )
        .parse_insert()
        .unwrap_err();

        assert_eq!(
            err,
            "Mismatched columns and values; expected 6 values but got 5"
        );
    }

    #[test]
    fn test_parse_create_table_ok() {
        let stmt = get_parser(
            "
            TABLE blog_post_comment(
                uuid TEXT,
                created_at INT,
                comment TEXT      ,
                // comment
                email TEXT
                // TODO: words TEXT[]
            );",
        )
        .parse_create_table()
        .expect("expected valid CREATE statement");

        assert_eq!(stmt, *CREATE_TABLE_STMT)
    }

    #[test]
    fn test_parse_create_table_err_no_table_kw() {
        let err = get_parser(
            "
            blog_post_comment(
                uuid TEXT,
                created_at INT,
                comment TEXT      ,
                // comment
                email TEXT
            );",
        )
        .parse_create_table()
        .unwrap_err();

        assert_eq!(err, "Expected 'TABLE' keyword".to_string());
    }

    #[test]
    fn test_parse_create_table_err_no_name() {
        let err = get_parser(
            "
            TABLE (
                uuid TEXT,
                created_at INT,
                comment TEXT      ,
                // comment
                email TEXT
            );",
        )
        .parse_create_table()
        .unwrap_err();

        assert_eq!(err, "Expected a valid table name".to_string());
    }

    #[test]
    fn test_parse_create_table_err_no_open_paren() {
        let err = get_parser(
            "
            TABLE blog_post_comment
                uuid TEXT,
                created_at INT,
                comment TEXT      ,
                // comment
                email TEXT
            );",
        )
        .parse_create_table()
        .unwrap_err();

        assert_eq!(err, "Expected '(' before columns".to_string());
    }

    #[test]
    fn test_parse_create_table_err_no_columns() {
        let err = get_parser(
            "
            TABLE blog_post_comment;",
        )
        .parse_create_table()
        .unwrap_err();

        assert_eq!(err, "Expected '(' before columns".to_string());
    }

    #[test]
    fn test_parse_create_table_err_empty_columns() {
        let err = get_parser(
            "
            TABLE blog_post_comment(
                // words TEXT[]
            );",
        )
        .parse_create_table()
        .unwrap_err();

        assert_eq!(err, "Expected a valid column name".to_string());
    }

    #[test]
    fn test_parse_create_table_err_dangling_comma() {
        let err = get_parser(
            "
            TABLE blog_post_comment(
                email TEXT,
            );",
        )
        .parse_create_table()
        .unwrap_err();

        assert_eq!(err, "Expected a valid column name".to_string());
    }

    #[test]
    fn test_parse_create_table_err_no_closing_paren() {
        let err = get_parser(
            "
            TABLE blog_post_comment(
                email TEXT
            ;",
        )
        .parse_create_table()
        .unwrap_err();

        assert_eq!(err, "Expected ')' after columns".to_string());
    }

    #[test]
    fn test_parse_columns() {
        let list = get_parser("list, test, yay")
            .parse_columns()
            .expect("expected valid ident list");

        assert_eq!(list.len(), 3);
        assert_eq!(list[0], "list");
        assert_eq!(list[1], "test");
        assert_eq!(list[2], "yay");
    }

    #[test]
    fn test_parse_columns_single_el() {
        let list = get_parser("test")
            .parse_columns()
            .expect("expected valid ident list");

        assert_eq!(list.len(), 1);
        assert_eq!(list[0], "test");
    }

    #[test]
    fn test_parse_columns_err_empty() {
        let err = get_parser("").parse_columns().unwrap_err();
        assert_eq!(err, "Expected a valid column name".to_string());
    }

    #[test]
    fn test_parse_columns_err_trailing_comma() {
        let err = get_parser("test,").parse_columns().unwrap_err();
        assert_eq!(err, "Expected a valid column name");
    }

    #[test]
    fn test_parse_column_config_ok() {
        let col_conf = get_parser("my_col TEXT")
            .parse_column_config()
            .expect("expected valid column config");

        assert_eq!(
            col_conf,
            ColumnConfig {
                name: "my_col".to_string(),
                t: ColumnType::Text
            }
        );
    }

    #[test]
    fn test_parse_column_config_err_invalid_type() {
        let err = get_parser("my_col TExXT")
            .parse_column_config()
            .unwrap_err();

        assert_eq!(err, "Expected a valid column type".to_string());
    }

    #[test]
    fn test_parse_column_config_err_kw_used_as_colname() {
        let err = get_parser("false TEXT").parse_column_config().unwrap_err();

        assert_eq!(err, "Expected a valid column name".to_string());
    }

    #[test]
    fn test_parse_opt_ok() {
        let opt = get_parser("LIMIT 5")
            .parse_opt()
            .expect("expected valid select opt");
        assert_eq!(opt, SelectOpt::Limit(5));
    }

    #[test]
    fn test_parse_opt_err_invalid_value_type() {
        let err = get_parser("LIMIT 5.0").parse_opt().unwrap_err();
        assert_eq!(err, "Expected an unsigned integer".to_string());
    }

    #[test]
    fn test_parse_opt_err_no_value_type() {
        let err = get_parser("LIMIT ").parse_opt().unwrap_err();
        assert_eq!(err, "Expected an unsigned integer".to_string());
    }

    #[test]
    fn test_parse_opt_err_non_opt() {
        let err = get_parser("SELECT 5").parse_opt().unwrap_err();
        assert_eq!(err, "Expected a valid opt type".to_string());
    }

    #[test]
    fn test_parse_expr_ok() {
        let expr = get_parser("hello == \"world\"")
            .parse_expr()
            .expect("expected valid expression");

        assert_eq!(
            expr,
            Expr::Binary(Binary {
                left: Box::new(Expr::Identifier(Identifier {
                    value: "hello".to_string()
                })),
                operator: Operator::EqualEqual,
                right: Box::new(Expr::Literal(Literal::String("world".to_string())))
            })
        );
        let expr = get_parser("hello != true")
            .parse_expr()
            .expect("expected valid expression");

        assert_eq!(
            expr,
            Expr::Binary(Binary {
                left: Box::new(Expr::Identifier(Identifier {
                    value: "hello".to_string()
                })),
                operator: Operator::BangEqual,
                right: Box::new(Expr::Literal(Literal::Boolean(true)))
            })
        );

        let expr = get_parser("hello > 9")
            .parse_expr()
            .expect("expected valid expression");

        assert_eq!(
            expr,
            Expr::Binary(Binary {
                left: Box::new(Expr::Identifier(Identifier {
                    value: "hello".to_string()
                })),
                operator: Operator::Greater,
                right: Box::new(Expr::Literal(Literal::UnsignedInt(9)))
            })
        );

        // TODO: disallow comp on bools and null
        let expr = get_parser("hello >= \"world\"")
            .parse_expr()
            .expect("expected valid expression");

        assert_eq!(
            expr,
            Expr::Binary(Binary {
                left: Box::new(Expr::Identifier(Identifier {
                    value: "hello".to_string()
                })),
                operator: Operator::GreaterEqual,
                right: Box::new(Expr::Literal(Literal::String("world".to_string())))
            })
        );

        let expr = get_parser("hello < 100.21")
            .parse_expr()
            .expect("expected valid expression");

        assert_eq!(
            expr,
            Expr::Binary(Binary {
                left: Box::new(Expr::Identifier(Identifier {
                    value: "hello".to_string()
                })),
                operator: Operator::Less,
                right: Box::new(Expr::Literal(Literal::Float(100.21)))
            })
        );

        let expr = get_parser("hello <= \"world\"")
            .parse_expr()
            .expect("expected valid expression");

        assert_eq!(
            expr,
            Expr::Binary(Binary {
                left: Box::new(Expr::Identifier(Identifier {
                    value: "hello".to_string()
                })),
                operator: Operator::LessEqual,
                right: Box::new(Expr::Literal(Literal::String("world".to_string())))
            })
        );
    }

    #[test]
    fn test_parse_expr_ok_compound() {
        let expr = get_parser("hello == \"world\" AND won == 3 OR won != 22")
            .parse_expr()
            .expect("expected valid expressions");

        assert_eq!(
            expr,
            Expr::Logical(Logical {
                left: Box::new(Expr::Logical(Logical {
                    left: Box::new(Expr::Binary(Binary {
                        left: Box::new(Expr::Identifier(Identifier {
                            value: "hello".to_string(),
                        })),
                        operator: Operator::EqualEqual,
                        right: Box::new(Expr::Literal(Literal::String("world".to_string()))),
                    })),
                    operator: Operator::And,
                    right: Box::new(Expr::Binary(Binary {
                        left: Box::new(Expr::Identifier(Identifier {
                            value: "won".to_string(),
                        })),
                        operator: Operator::EqualEqual,
                        right: Box::new(Expr::Literal(Literal::UnsignedInt(3))),
                    })),
                })),
                operator: Operator::Or,
                right: Box::new(Expr::Binary(Binary {
                    left: Box::new(Expr::Identifier(Identifier {
                        value: "won".to_string(),
                    })),
                    operator: Operator::BangEqual,
                    right: Box::new(Expr::Literal(Literal::UnsignedInt(22))),
                })),
            })
        );
    }

    #[test]
    fn test_parse_expr_ok_left_compound() {
        let expr = get_parser("(hello == \"world\" AND won == 3) OR won != 22")
            .parse_expr()
            .expect("expected valid expressions");

        assert_eq!(
            expr,
            Expr::Logical(Logical {
                left: Box::new(Expr::Grouping(Grouping {
                    expr: Box::new(Expr::Logical(Logical {
                        left: Box::new(Expr::Binary(Binary {
                            left: Box::new(Expr::Identifier(Identifier {
                                value: "hello".to_string(),
                            })),
                            operator: Operator::EqualEqual,
                            right: Box::new(Expr::Literal(Literal::String("world".to_string()))),
                        })),
                        operator: Operator::And,
                        right: Box::new(Expr::Binary(Binary {
                            left: Box::new(Expr::Identifier(Identifier {
                                value: "won".to_string(),
                            })),
                            operator: Operator::EqualEqual,
                            right: Box::new(Expr::Literal(Literal::UnsignedInt(3))),
                        })),
                    })),
                })),
                operator: Operator::Or,
                right: Box::new(Expr::Binary(Binary {
                    left: Box::new(Expr::Identifier(Identifier {
                        value: "won".to_string(),
                    })),
                    operator: Operator::BangEqual,
                    right: Box::new(Expr::Literal(Literal::UnsignedInt(22))),
                })),
            })
        );
    }

    #[test]
    fn test_parse_expr_ok_right_compound() {
        let expr = get_parser("hello == \"world\" AND (won == 3 OR won != 22)")
            .parse_expr()
            .expect("expected valid expressions");

        assert_eq!(
            expr,
            Expr::Logical(Logical {
                left: Box::new(Expr::Binary(Binary {
                    left: Box::new(Expr::Identifier(Identifier {
                        value: String::from("hello")
                    })),
                    operator: Operator::EqualEqual,
                    right: Box::new(Expr::Literal(Literal::String(String::from("world"))))
                })),
                operator: Operator::And,
                right: Box::new(Expr::Grouping(Grouping {
                    expr: Box::new(Expr::Logical(Logical {
                        left: Box::new(Expr::Binary(Binary {
                            left: Box::new(Expr::Identifier(Identifier {
                                value: String::from("won")
                            })),
                            operator: Operator::EqualEqual,
                            right: Box::new(Expr::Literal(Literal::UnsignedInt(3)))
                        })),
                        operator: Operator::Or,
                        right: Box::new(Expr::Binary(Binary {
                            left: Box::new(Expr::Identifier(Identifier {
                                value: String::from("won")
                            })),
                            operator: Operator::BangEqual,
                            right: Box::new(Expr::Literal(Literal::UnsignedInt(22)))
                        }))
                    }))
                }))
            })
        );
    }

    #[test]
    fn test_parse_expr_err_empty() {
        let err = get_parser("").parse_expr().unwrap_err();
        assert_eq!(
            err,
            "Expected either an identifier or nested expression for left side of expression"
        );
    }

    #[test]
    fn test_parse_expr_err_trailing_conjunction() {
        let err = get_parser("world == \"earth\" AND")
            .parse_expr()
            .unwrap_err();
        assert_eq!(err, "Expected either a primitive, identifier, or nested expression for right side of expression");
    }

    #[test]
    fn test_parse_expr_err_no_right() {
        let err = get_parser("hello <=").parse_expr().unwrap_err();
        assert_eq!(err, "Expected either a primitive, identifier, or nested expression for right side of expression");
    }

    #[test]
    fn test_parse_expr_err_no_left() {
        let err = get_parser(" <= \"world\"").parse_expr().unwrap_err();
        assert_eq!(
            err,
            "Expected either an identifier or nested expression for left side of expression"
        );
    }

    #[test]
    fn test_parse_expr_err_no_operator() {
        let err = get_parser("hello \"world\"").parse_expr().unwrap_err();
        assert_eq!(err, "Expected an expression");
    }

    #[test]
    fn test_parse_expr_err_invalid_operator() {
        let err = get_parser("hello ! \"world\"").parse_expr().unwrap_err();
        assert_eq!(err, "Expected an expression");
    }

    #[test]
    fn test_parse_expr_err_non_literal_right() {
        let err = get_parser("hello == CREATE").parse_expr().unwrap_err();
        assert_eq!(
            err,
            "Expected either a primitive, identifier, or nested expression for right side of expression"
        );
    }

    #[test]
    fn test_parse_literal_ok() {
        assert_eq!(
            get_parser("\"text\"")
                .parse_literal()
                .expect("expected valid literal"),
            Literal::String("text".to_string())
        );

        assert_eq!(
            get_parser("100")
                .parse_literal()
                .expect("expected valid literal"),
            Literal::UnsignedInt(100)
        );

        assert_eq!(
            get_parser("100.100")
                .parse_literal()
                .expect("expected valid literal"),
            Literal::Float(100.100)
        );

        assert_eq!(
            get_parser("true")
                .parse_literal()
                .expect("expected valid literal"),
            Literal::Boolean(true)
        );

        assert_eq!(
            get_parser("false")
                .parse_literal()
                .expect("expected valid literal"),
            Literal::Boolean(false)
        );

        assert_eq!(
            get_parser("nil")
                .parse_literal()
                .expect("expected valid literal"),
            Literal::Nil
        );
    }

    // TODO: code cov tool
    #[test]
    fn test_parse_literals_ok() {
        let lits = get_parser("true, false, nil, 1, \"hi\"")
            .parse_literals()
            .expect("expected valid literals");
        assert_eq!(lits.len(), 5);
    }

    #[test]
    fn test_parse_literals_ok_single() {
        let lits = get_parser("true")
            .parse_literals()
            .expect("expected valid literals");
        assert_eq!(lits.len(), 1);
    }

    #[test]
    fn test_parse_literals_err_trailing_comma() {
        let err = get_parser("true,").parse_literals().unwrap_err();
        assert_eq!(err, "Expected a valid literal");
    }

    #[test]
    fn test_parse_literals_err_empty() {
        let err = get_parser("").parse_literals().unwrap_err();
        assert_eq!(err, "Expected a valid literal");
    }

    #[test]
    fn test_parse_literal_err_not_literal() {
        assert_eq!(
            get_parser("nile").parse_literal().unwrap_err(),
            "Expected a valid literal"
        );
    }

    #[test]
    fn test_parse_literal_err_empty() {
        assert_eq!(
            get_parser("").parse_literal().unwrap_err(),
            "Expected a valid literal"
        );
    }
}
