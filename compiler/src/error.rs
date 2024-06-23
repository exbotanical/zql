#[derive(Debug, PartialEq)]
pub enum CompilerError {
    ParseError(String),
}

impl From<String> for CompilerError {
    fn from(value: String) -> Self {
        CompilerError::ParseError(value)
    }
}
