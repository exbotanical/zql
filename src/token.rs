#[derive(PartialEq, Debug, Clone)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
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
    // TODO: signed ints
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
    // TODO: Rename these (Text, Int) - they're type specifiers; not values
    Text,
    Int,
    True,
    False,
    Nil,

    Erroneous(String),
    Eof,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
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
    Text,
    Int,
    True,
    False,
    Nil,

    Erroneous,
    Eof,
}

impl From<&Token> for TokenKind {
    fn from(value: &Token) -> Self {
        match value {
            Token::LeftParen => TokenKind::LeftParen,
            Token::RightParen => TokenKind::RightParen,
            Token::LeftBrace => TokenKind::LeftBrace,
            Token::RightBrace => TokenKind::RightBrace,
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
            Token::Text => TokenKind::Text,
            Token::Int => TokenKind::Int,
            Token::False => TokenKind::False,
            Token::True => TokenKind::True,
            Token::Nil => TokenKind::Nil,
            Token::Erroneous(_) => TokenKind::Erroneous,
            Token::Eof => TokenKind::Eof,
        }
    }
}

impl From<&crate::pos::WithPosMetadata<Token>> for TokenKind {
    fn from(t: &crate::pos::WithPosMetadata<Token>) -> Self {
        TokenKind::from(&t.value)
    }
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
            _ => Err("Expected a valid operator".to_string()),
        }
    }
}
