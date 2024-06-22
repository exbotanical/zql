use crate::token::Operator;

#[derive(Debug, PartialEq)]
pub enum Expr {
    Grouping(Grouping),
    Logical(Logical),
    Binary(Binary),
    Literal(Literal),
    Identifier(Identifier),
    None,
}

#[derive(Debug, PartialEq)]
pub struct Grouping {
    pub expr: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Logical {
    pub left: Box<Expr>,
    pub operator: Operator,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct Binary {
    pub left: Box<Expr>,
    pub operator: Operator,
    pub right: Box<Expr>,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Boolean(bool),
    String(String),
    Float(f64),
    UnsignedInt(u64),
    Nil,
}

#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub value: String,
}
