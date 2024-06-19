use crate::token::Operator;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Select(SelectStatement),
    Insert(InsertStatement),
    CreateTable(CreateTableStatement),
}

#[derive(Debug, PartialEq)]
pub enum SelectOpt {
    None,
    Limit(u64),
}

#[derive(Debug, PartialEq)]
pub struct SelectStatement {
    pub source: String,
    pub columns: Vec<String>,
    pub exprs: Vec<Expression>,
    pub opt: SelectOpt, // TODO: multiple?
}

#[derive(Debug, PartialEq)]
pub struct Expression {
    // TODO: allow both sides to be literal or identifier
    pub left: String,
    pub operator: Operator,
    pub right: Literal,
}

/*
    SELECT * FROM table WHERE
    col2 == 2 OR (col3 == 4 AND col1 == 2)
    (col2 == 2 OR col3 == 4) AND col1 == 2
    LIMIT 100;
*/

#[derive(Debug, PartialEq)]
pub enum Literal {
    Boolean(bool),
    String(String),
    Float(f64),
    UnsignedInt(u64),
    Nil,
}

#[derive(Debug, PartialEq)]
pub enum ColumnType {
    Int,
    Text,
}

#[derive(Debug, PartialEq)]
pub struct ColumnConfig {
    pub name: String,
    pub t: ColumnType,
}

#[derive(Debug, PartialEq)]
pub struct CreateTableStatement {
    pub name: String,
    pub columns: Vec<ColumnConfig>,
}

#[derive(Debug, PartialEq)]
pub struct InsertStatement {
    pub dest: String,
    pub columns: Vec<String>,
    pub values: Vec<Literal>,
}

// TODO: is it better to pub the fields or have init fn
// TODO: also is there something like lombok for rust that can generate these?
