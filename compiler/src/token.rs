use std::fmt::Display;
use std::str::FromStr;

use crate::{expr::Literal, stmt::ColumnType};

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    LeftParen,
    RightParen,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier(String),
    String(String),
    SignedInt(i64),
    UnsignedInt(u64),
    Float(f64),

    Create,
    Insert,
    Table,
    Where,
    Select,
    From,
    Into,
    Limit,
    And,
    Or,
    Values,

    TextType,
    TextArrType,
    IntType,
    IntArrType,

    True,
    False,
    Nil,

    Erroneous(String),
    Eof,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let literal = match self {
            Token::LeftParen => "(".to_owned(),
            Token::RightParen => ")".to_owned(),
            Token::Comma => ".to_owned(),".to_owned(),
            Token::Dot => ".".to_owned(),
            Token::Minus => "-".to_owned(),
            Token::Plus => "+".to_owned(),
            Token::Semicolon => ";".to_owned(),
            Token::Slash => "/".to_owned(),
            Token::Star => "*".to_owned(),
            Token::Bang => "!".to_owned(),
            Token::BangEqual => "!=".to_owned(),
            Token::Equal => "=".to_owned(),
            Token::EqualEqual => "==".to_owned(),
            Token::Greater => ">".to_owned(),
            Token::GreaterEqual => ">=".to_owned(),
            Token::Less => "<".to_owned(),
            Token::LessEqual => "<=".to_owned(),
            Token::Identifier(v) => v.to_owned(),
            Token::String(v) => v.to_owned(),
            Token::UnsignedInt(v) => v.to_string(),
            Token::SignedInt(v) => v.to_string(),
            Token::Float(v) => v.to_string(),
            Token::Create => "CREATE".to_owned(),
            Token::Insert => "INSERT".to_owned(),
            Token::Table => "TABLE".to_owned(),
            Token::Where => "WHERE".to_owned(),
            Token::Select => "SELECT".to_owned(),
            Token::From => "FROM".to_owned(),
            Token::Into => "INTO".to_owned(),
            Token::Limit => "LIMIT".to_owned(),
            Token::And => "AND".to_owned(),
            Token::Or => "OR".to_owned(),
            Token::Values => "VALUES".to_owned(),
            Token::TextType => "TEXT".to_owned(),
            Token::TextArrType => "TEXT[]".to_owned(),
            Token::IntType => "INT".to_owned(),
            Token::IntArrType => "INT[]".to_owned(),
            Token::False => "false".to_owned(),
            Token::True => "true".to_owned(),
            Token::Nil => "nil".to_owned(),
            Token::Erroneous(v) => v.to_owned(),
            Token::Eof => "EOF".to_owned(),
        }
        .to_owned();

        f.write_str(&literal)
    }
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    Identifier,
    String,
    SignedInt,
    UnsignedInt,
    Float,

    Create,
    Insert,
    Table,
    Where,
    Select,
    From,
    Into,
    Limit,
    And,
    Or,
    Values,

    TextType,
    TextArrType,
    IntType,
    IntArrType,

    True,
    False,
    Nil,

    Erroneous,
    Eof,
}

#[derive(Debug, PartialEq)]
pub enum Operator {
    BangEqual,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Or,
    And,
}

impl FromStr for Token {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "CREATE" => Ok(Token::Create),
            "INSERT" => Ok(Token::Insert),
            "TABLE" => Ok(Token::Table),
            "WHERE" => Ok(Token::Where),
            "SELECT" => Ok(Token::Select),
            "FROM" => Ok(Token::From),
            "INTO" => Ok(Token::Into),
            "LIMIT" => Ok(Token::Limit),
            "AND" => Ok(Token::And),
            "OR" => Ok(Token::Or),
            "VALUES" => Ok(Token::Values),
            "TEXT" => Ok(Token::TextType),
            "INT" => Ok(Token::IntType),
            "false" => Ok(Token::False),
            "true" => Ok(Token::True),
            "nil" => Ok(Token::Nil),
            v => Err(format!("Not a valid keyword: {}", v).to_owned()),
        }
    }
}

impl TryInto<Operator> for Token {
    type Error = String;

    fn try_into(self) -> Result<Operator, Self::Error> {
        match self {
            Token::BangEqual => Ok(Operator::BangEqual),
            Token::EqualEqual => Ok(Operator::EqualEqual),
            Token::Greater => Ok(Operator::Greater),
            Token::GreaterEqual => Ok(Operator::GreaterEqual),
            Token::Less => Ok(Operator::Less),
            Token::LessEqual => Ok(Operator::LessEqual),
            _ => Err("Expected a valid operator".to_owned()),
        }
    }
}

impl TryInto<Literal> for &Token {
    type Error = String;

    fn try_into(self) -> Result<Literal, Self::Error> {
        match self {
            Token::False => Ok(Literal::Boolean(false)),
            Token::True => Ok(Literal::Boolean(true)),
            Token::Nil => Ok(Literal::Nil),
            Token::UnsignedInt(v) => Ok(Literal::UnsignedInt(*v)),
            Token::Float(v) => Ok(Literal::Float(*v)),
            Token::String(v) => Ok(Literal::String(v.to_owned())),
            _ => Err("Expected a valid literal".to_owned()),
        }
    }
}

impl TryInto<ColumnType> for &Token {
    type Error = String;

    fn try_into(self) -> Result<ColumnType, Self::Error> {
        Ok(match self {
            Token::IntType => ColumnType::Int,
            Token::TextType => ColumnType::Text,
            Token::IntArrType => ColumnType::IntArr,
            Token::TextArrType => ColumnType::TextArr,
            v => return Err(format!("Expected a valid column type but got '{}'", v).to_owned()),
        })
    }
}

impl From<&Token> for TokenKind {
    fn from(value: &Token) -> Self {
        match value {
            Token::LeftParen => TokenKind::LeftParen,
            Token::RightParen => TokenKind::RightParen,
            Token::Comma => TokenKind::Comma,
            Token::Dot => TokenKind::Dot,
            Token::Minus => TokenKind::Minus,
            Token::Plus => TokenKind::Plus,
            Token::Semicolon => TokenKind::Semicolon,
            Token::Slash => TokenKind::Slash,
            Token::Star => TokenKind::Star,
            Token::Bang => TokenKind::Bang,
            Token::BangEqual => TokenKind::BangEqual,
            Token::Equal => TokenKind::Equal,
            Token::EqualEqual => TokenKind::EqualEqual,
            Token::Greater => TokenKind::Greater,
            Token::GreaterEqual => TokenKind::GreaterEqual,
            Token::Less => TokenKind::Less,
            Token::LessEqual => TokenKind::LessEqual,
            Token::Identifier(_) => TokenKind::Identifier,
            Token::String(_) => TokenKind::String,
            Token::UnsignedInt(_) => TokenKind::UnsignedInt,
            Token::SignedInt(_) => TokenKind::SignedInt,
            Token::Float(_) => TokenKind::Float,
            Token::Create => TokenKind::Create,
            Token::Insert => TokenKind::Insert,
            Token::Table => TokenKind::Table,
            Token::Where => TokenKind::Where,
            Token::Select => TokenKind::Select,
            Token::From => TokenKind::From,
            Token::Into => TokenKind::Into,
            Token::Limit => TokenKind::Limit,
            Token::And => TokenKind::And,
            Token::Or => TokenKind::Or,
            Token::Values => TokenKind::Values,
            Token::TextType => TokenKind::TextType,
            Token::TextArrType => TokenKind::TextArrType,
            Token::IntType => TokenKind::IntType,
            Token::IntArrType => TokenKind::IntArrType,
            Token::False => TokenKind::False,
            Token::True => TokenKind::True,
            Token::Nil => TokenKind::Nil,
            Token::Erroneous(_) => TokenKind::Erroneous,
            Token::Eof => TokenKind::Eof,
        }
    }
}

impl From<&crate::pos::WithTokenMetadata<Token>> for TokenKind {
    fn from(t: &crate::pos::WithTokenMetadata<Token>) -> Self {
        TokenKind::from(&t.value)
    }
}

#[cfg(test)]
mod tests {
    use crate::{expr::Literal, stmt::ColumnType, token::Token};

    #[test]
    fn test_from_token_into_literal() {
        let l: Literal = (&Token::False)
            .try_into()
            .expect("expected a valid literal");
        assert_eq!(l, Literal::Boolean(false));

        let l: Literal = (&Token::UnsignedInt(100))
            .try_into()
            .expect("expected valid literal");
        assert_eq!(l, Literal::UnsignedInt(100));

        let l: Literal = (&Token::Float(100.100))
            .try_into()
            .expect("expected valid literal");
        assert_eq!(l, Literal::Float(100.100));

        let l: Literal = (&Token::False).try_into().expect("expected valid literal");
        assert_eq!(l, Literal::Boolean(false));

        let l: Literal = (&Token::True).try_into().expect("expected valid literal");
        assert_eq!(l, Literal::Boolean(true));

        let l: Literal = (&Token::Nil).try_into().expect("expected valid literal");
        assert_eq!(l, Literal::Nil);
    }

    #[test]
    fn test_parse_literal_err_not_literal() {
        let err = ((&Token::Identifier("whatever".to_owned())).try_into()
            as Result<Literal, String>)
            .unwrap_err();

        assert_eq!(err, "Expected a valid literal");
    }

    #[test]
    fn test_parse_literal_err_empty() {
        let err = ((&Token::Eof).try_into() as Result<Literal, String>).unwrap_err();

        assert_eq!(err, "Expected a valid literal");
    }

    #[test]
    fn test_from_token_into_coltype() {
        let token = Token::IntType;
        let col_type: ColumnType = (&token).try_into().expect("Expected a valid column type");
        assert_eq!(col_type, ColumnType::Int);

        let token = Token::TextType;
        let col_type: ColumnType = (&token).try_into().expect("Expected a valid column type");
        assert_eq!(col_type, ColumnType::Text);
    }

    #[test]
    fn test_from_token_into_coltype_err() {
        let token = Token::UnsignedInt(1);
        let err = ((&token).try_into() as Result<ColumnType, String>).unwrap_err();
        assert_eq!(err, "Expected a valid column type but got '1'");

        let token = Token::Create;
        let err = ((&token).try_into() as Result<ColumnType, String>).unwrap_err();
        assert_eq!(err, "Expected a valid column type but got 'CREATE'");
    }
}
