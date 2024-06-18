use crate::token::Operator;

pub enum Statement<'a> {
    Select(SelectStatement<'a>),
    Insert(InsertStatement<'a>),
    CreateTable(CreateTableStatement<'a>),
}

struct SelectOpt<'a> {
    t: &'a str,
    value: &'a str,
}

struct SelectStatement<'a> {
    source: &'a str,
    columns: Vec<&'a str>,
    exprs: Vec<Expression>,
    opts: Vec<SelectOpt<'a>>,
}

#[derive(Debug)]
pub struct Expression {
    // TODO: allow both sides to be literal or identifier
    pub left: String,
    pub operator: Operator,
    pub right: Literal,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Boolean(bool),
    String(String),
    Float(f64),
    UnsignedInt(u64),
    Nil,
}

struct ColumnConfig<'a> {
    name: &'a str,
    // strongly-typed/enum
    t: &'a str,
}

struct CreateTableStatement<'a> {
    name: &'a str,
    columns: Vec<ColumnConfig<'a>>,
}

struct InsertStatement<'a> {
    dest: &'a str,
    columns: Vec<&'a str>,
    values: Vec<&'a str>,
}
