use crate::expr::{Expr, Literal};

#[derive(Debug, PartialEq)]
pub enum SelectOpt {
    Limit(u64),
}

#[derive(Debug, PartialEq)]
pub enum ColumnType {
    Int,
    IntArr,
    Text,
    TextArr,
}

#[derive(Debug, PartialEq)]
pub struct ColumnConfig {
    pub name: String,
    pub t: ColumnType,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Select(SelectStatement),
    Insert(InsertStatement),
    CreateTable(CreateTableStatement),
}

#[derive(Debug, PartialEq)]
pub struct SelectStatement {
    pub source: String,
    pub columns: Vec<String>,
    pub expr: Expr,
    pub opts: Vec<SelectOpt>,
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
