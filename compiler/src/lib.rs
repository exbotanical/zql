use error::CompilerError;
use lexer::Lexer;
use parser::Parser;
use stmt::Statement;

pub mod error;
mod expr;
mod lexer;
mod parser;
mod pos;
mod scanner;
mod stmt;
mod token;

// TODO: prune parser and lexer to scope down to current grammar
pub fn parse(buf: &str) -> Result<Vec<Statement>, CompilerError> {
    Parser::new(Lexer::new(buf).tokenize()).parse()
}
