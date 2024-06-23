use std::io;

use compiler::error::CompilerError;

#[derive(Debug)]
pub enum ZqlError {
    ParseError(String),
    IoError(String),
}

impl From<io::Error> for ZqlError {
    fn from(value: io::Error) -> Self {
        ZqlError::IoError(value.to_string())
    }
}

impl From<CompilerError> for ZqlError {
    fn from(value: CompilerError) -> Self {
        match value {
            CompilerError::ParseError(e) => ZqlError::ParseError(e.to_owned()),
        }
    }
}
