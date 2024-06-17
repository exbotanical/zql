use std::collections::HashMap;

use lazy_static::lazy_static;

use crate::{scanner::Scanner, token::Token};

lazy_static! {
    static ref KEYWORDS_MAP: HashMap<&'static str, &'static str> = {
        let mut m = HashMap::new();

        m.insert("CREATE", "CREATE");
        m.insert("INSERT", "INSERT");
        m.insert("TABLE", "TABLE");
        m.insert("WHERE", "WHERE");
        m.insert("SELECT", "SELECT");
        m.insert("FROM", "FROM");
        m.insert("INTO", "INTO");
        m.insert("LIMIT", "LIMIT");
        m.insert("AND", "AND");
        m.insert("OR", "OR");
        m.insert("VALUES", "VALUES");
        m.insert("TEXT", "TEXT");
        m.insert("INT", "INT");

        m
    };
}

pub struct Lexer<'a> {
    scanner: Scanner<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(buf: &str) -> Lexer {
        Lexer {
            scanner: Scanner::new(buf),
        }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens: Vec<Token> = Vec::new();

        loop {
            // TODO: use pos for diagnostics
            // let start = self.scanner.pos;
            let c = match self.scanner.next() {
                Some(c) => c,
                None => break,
            };

            if let Some(tok) = self.match_token(c) {
                match tok {
                    Token::Erroneous(s) => panic!("error: {}", s),
                    _ => tokens.push(tok),
                }
            }
        }

        tokens
    }

    fn match_token(&mut self, c: char) -> Option<Token> {
        match c {
            '(' => Some(Token::LeftParen),
            ')' => Some(Token::RightParen),
            ',' => Some(Token::Comma),
            '.' => Some(Token::Dot),
            '-' => Some(Token::Minus),
            ';' => Some(Token::Semicolon),
            '*' => Some(Token::Star),
            '!' => {
                if self.scanner.consume_if(|c| c == '=') {
                    Some(Token::BangEqual)
                } else {
                    Some(Token::Bang)
                }
            }
            '=' => {
                if self.scanner.consume_if(|c| c == '=') {
                    Some(Token::EqualEqual)
                } else {
                    Some(Token::Equal)
                }
            }

            '<' => {
                if self.scanner.consume_if(|c| c == '=') {
                    Some(Token::LessEqual)
                } else {
                    Some(Token::Less)
                }
            }

            '>' => {
                if self.scanner.consume_if(|c| c == '=') {
                    Some(Token::GreaterEqual)
                } else {
                    Some(Token::Greater)
                }
            }

            '/' => {
                if self.scanner.consume_if(|c| c == '/') {
                    self.scanner.consume_while(|c| c != '\n');
                    None
                } else {
                    Some(Token::Slash)
                }
            }
            ' ' => None,
            '\r' => None,
            '\t' => None,
            '\n' => None,
            //   line++
            '"' => {
                let string: String = self
                    .scanner
                    .consume_while(|c| c != '"')
                    .into_iter()
                    .collect();

                match self.scanner.next() {
                    None => Some(Token::Erroneous("expect terminal '\"'".into())),
                    _ => Some(Token::String(string)),
                }
            }
            c if c.is_numeric() => self.tokenize_number(c),
            c if c.is_ascii_alphabetic() => self.tokenize_ident(c),
            c => Some(Token::Erroneous(format!("unknown char {}", c))),
        }
    }

    fn tokenize_number(&mut self, start: char) -> Option<Token> {
        let mut number: String = String::new();
        number.push(start);

        let part1: String = self
            .scanner
            .consume_while(|c| c.is_numeric())
            .into_iter()
            .collect();
        number.push_str(part1.as_str());

        if self.scanner.peek() == Some('.') {
            number.push(self.scanner.next().unwrap());

            let part2: String = self
                .scanner
                .consume_while(|c| c.is_numeric())
                .into_iter()
                .collect();

            number.push_str(part2.as_str());
        }

        Some(Token::Number(number.parse::<f64>().unwrap()))
    }

    fn tokenize_ident(&mut self, start: char) -> Option<Token> {
        let mut string: String = String::new();
        string.push(start);

        let part: String = self
            .scanner
            .consume_while(|c| c.is_ascii_alphanumeric())
            .into_iter()
            .collect();

        string.push_str(part.as_str());

        Some(
            match KEYWORDS_MAP.get(string.to_ascii_uppercase().as_str()) {
                Some(kw) => Token::Keyword(kw.to_string()),
                None => Token::Identifier(string),
            },
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::token::{self, Token};

    use super::Lexer;

    fn get_tokens(str: &str) -> Vec<token::Token> {
        Lexer::new(str).tokenize()
    }

    #[test]
    fn test_lexer() {
        assert_eq!(get_tokens("("), vec![Token::LeftParen]);
        assert_eq!(get_tokens(")"), vec![Token::RightParen]);
        assert_eq!(get_tokens(","), vec![Token::Comma]);
        assert_eq!(get_tokens("."), vec![Token::Dot]);
        assert_eq!(get_tokens("-"), vec![Token::Minus]);
        assert_eq!(get_tokens(";"), vec![Token::Semicolon]);
        assert_eq!(get_tokens("*"), vec![Token::Star]);
        assert_eq!(get_tokens("!"), vec![Token::Bang]);
        assert_eq!(get_tokens("!="), vec![Token::BangEqual]);
        assert_eq!(get_tokens("="), vec![Token::Equal]);
        assert_eq!(get_tokens("=="), vec![Token::EqualEqual]);
        assert_eq!(get_tokens("<"), vec![Token::Less]);
        assert_eq!(get_tokens("<="), vec![Token::LessEqual]);
        assert_eq!(get_tokens(">"), vec![Token::Greater]);
        assert_eq!(get_tokens(">="), vec![Token::GreaterEqual]);
        assert_eq!(get_tokens("// this is just a comment"), vec![]);
        assert_eq!(get_tokens(" "), vec![]);
        assert_eq!(get_tokens("\r"), vec![]);
        assert_eq!(get_tokens("\t"), vec![]);
        assert_eq!(get_tokens("\n"), vec![]);
        assert_eq!(get_tokens("1.33"), vec![Token::Number(1.33)]);
        assert_eq!(get_tokens("\"hello\""), vec![Token::String("hello".into())]);
        // TODO: ?
        assert_eq!(
            get_tokens("\"hello\n\""),
            vec![Token::String("hello\n".into())]
        );
    }

    #[test]
    #[should_panic]
    fn test_lexer_sad_case() {
        assert_eq!(get_tokens("|"), vec![]);
    }

    #[test]
    fn test_tokenize_number() {
        assert_eq!(
            Lexer::new("20").tokenize_number('3'),
            Some(Token::Number(320.))
        );
        assert_eq!(
            Lexer::new("20.12").tokenize_number('3'),
            Some(Token::Number(320.12))
        );
    }

    #[test]
    fn test_tokenize_number_single_digit() {
        assert_eq!(
            Lexer::new("mousely").tokenize_number('9'),
            Some(Token::Number(9.))
        );
    }

    #[test]
    fn test_tokenize_ident() {
        assert_eq!(
            Lexer::new("20").tokenize_ident('a'),
            Some(Token::Identifier("a20".into()))
        );
    }

    #[test]
    fn test_tokenize_ident_single_char() {
        assert_eq!(
            Lexer::new(" snoobers").tokenize_ident('x'),
            Some(Token::Identifier("x".into()))
        );
    }

    #[test]
    fn test_tokenize_ident_kw() {
        assert_eq!(
            Lexer::new("REATE").tokenize_ident('C'),
            Some(Token::Keyword("CREATE".into()))
        );

        assert_eq!(
            Lexer::new("ND").tokenize_ident('A'),
            Some(Token::Keyword("AND".into()))
        );
    }

    #[test]
    fn test_tokenize_ident_kw_case_agnostic() {
        // TODO: enforce either all upper or all lower
        assert_eq!(
            Lexer::new("REaTE").tokenize_ident('C'),
            Some(Token::Keyword("CREATE".into()))
        );

        assert_eq!(
            Lexer::new("nd").tokenize_ident('a'),
            Some(Token::Keyword("AND".into()))
        );
    }
}
