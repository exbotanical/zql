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
    exprs: Vec<Expression<'a>>,
    opts: Vec<SelectOpt<'a>>,
}

struct Expression<'a> {
    left: Literal<'a>,
    // TODO: strongly-typed/enum
    operator: &'a str,
    right: Literal<'a>,
}

struct Literal<'a> {
    value: &'a str,
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
