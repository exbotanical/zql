use std::io::Write;

use rustyline::{error::ReadlineError, history::FileHistory};
use zql::error::ZqlError;

#[derive(Debug, PartialEq)]
enum IterStatus {
    Continue,
    Break,
}

struct Repl {
    prompt: String,
    editor: rustyline::Editor<(), FileHistory>,
    out: Box<dyn Write>,
    buffer: String,
}

impl Repl {
    pub fn new() -> Self {
        Repl {
            out: Box::new(std::io::stderr()),
            editor: rustyline::DefaultEditor::new().expect("failed to start readline impl"),
            prompt: "> ".into(),
            buffer: String::new(),
        }
    }

    fn iter(&mut self) -> Result<IterStatus, ZqlError> {
        match self.editor.readline(&self.prompt) {
            Ok(line) => {
                if !line.trim().is_empty() {
                    // TODO: debug
                    self.editor.add_history_entry(line.trim());
                    self.handle_line(&line)?;

                    Ok(IterStatus::Continue)
                } else {
                    Ok(IterStatus::Continue)
                }
            }
            Err(ReadlineError::Interrupted) => {
                writeln!(&mut self.out, "SIGINT received; exiting...")?;
                Ok(IterStatus::Break)
            }
            Err(ReadlineError::Eof) => Ok(IterStatus::Break),
            Err(err) => {
                writeln!(&mut self.out, "Error: {err:?}")?;
                Ok(IterStatus::Continue)
            }
        }
    }

    fn process_buffer(&mut self) -> Result<IterStatus, ZqlError> {
        // TODO: why is this different than match &self.buffer?
        let s: &str = &self.buffer;
        match s {
            // TODO: improve this
            "quit;" => return Ok(IterStatus::Break),
            _ => {
                let statements = compiler::parse(&self.buffer)?;
                println!("{:#?}", statements);
            }
        }

        Ok(IterStatus::Continue)
    }

    fn handle_line(&mut self, line: &str) -> Result<IterStatus, ZqlError> {
        self.buffer.push_str(line);

        if self.buffer.ends_with(";") {
            match self.process_buffer() {
                Ok(s) => {
                    if s == IterStatus::Break {
                        return Ok(s);
                    }
                }
                Err(ZqlError::ParseError(e)) => println!("{:#?}", e),
                _ => {}
            }

            self.buffer = String::new();
        } else {
            self.buffer.push('\n');
        }

        Ok(IterStatus::Continue)
    }

    pub fn run(&mut self) -> Result<(), ZqlError> {
        while self.iter()? == IterStatus::Continue {}
        Ok(())
    }
}

fn main() -> Result<(), ZqlError> {
    Repl::new().run()?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::{IterStatus, Repl};

    #[test]
    fn test_repl_quit() {
        assert_eq!(
            Repl::new()
                .handle_line("quit;")
                .expect("Expected IterStatus"),
            IterStatus::Break
        );
    }
}
