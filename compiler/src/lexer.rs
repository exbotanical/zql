use std::str::FromStr;

use crate::{
    pos::{BytePos, WithTokenMetadata},
    scanner::Scanner,
    token::Token,
};

pub struct Lexer<'a> {
    scanner: Scanner<'a>,
    line: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(buf: &str) -> Lexer {
        Lexer {
            scanner: Scanner::new(buf),
            line: 1,
        }
    }

    pub fn tokenize(&mut self) -> Vec<WithTokenMetadata<Token>> {
        let mut tokens: Vec<WithTokenMetadata<Token>> = Vec::new();

        loop {
            let start = self.scanner.pos;

            let c = match self.scanner.next() {
                Some(c) => c,
                None => break,
            };

            if let Some(token) = self.match_token(c) {
                match token {
                    Token::Erroneous(s) => panic!("error: {}", s),
                    _ => tokens.push(WithTokenMetadata::new(
                        token,
                        start,
                        BytePos {
                            0: self.scanner.pos.0 - 1,
                        },
                        self.line,
                    )),
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
            '\n' => {
                self.line = self.line + 1;
                None
            }
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
            c if c.is_numeric() || c == '-' => self.tokenize_number(c),
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

            Some(Token::Float(number.parse::<f64>().unwrap()))
        } else {
            if start == '-' {
                Some(Token::SignedInt(number.parse::<i64>().unwrap()))
            } else {
                Some(Token::UnsignedInt(number.parse::<u64>().unwrap()))
            }
        }
    }

    fn tokenize_ident(&mut self, start: char) -> Option<Token> {
        let mut string: String = String::new();
        string.push(start);

        let part: String = self
            .scanner
            .consume_while(|c| c.is_ascii_alphanumeric() || c == '_')
            .into_iter()
            .collect();

        string.push_str(part.as_str());

        Some(match Token::from_str(string.as_str()) {
            Ok(v) => match v {
                Token::TextType => self.get_column_type_token(Token::TextType, Token::TextArrType),
                Token::IntType => self.get_column_type_token(Token::IntType, Token::IntArrType),
                v => v,
            },
            Err(_) => Token::Identifier(string),
        })
    }

    fn get_column_type_token(&mut self, base_type: Token, arr_type: Token) -> Token {
        if self.scanner.consume_if(|c| c == '[') {
            if self.scanner.consume_if(|c| c == ']') {
                arr_type
            } else {
                Token::Erroneous("Expected a closing ']' after '['".to_owned())
            }
        } else {
            base_type
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::{self, Token};

    use super::Lexer;

    fn get_tokens(str: &str) -> Vec<token::Token> {
        Lexer::new(str)
            .tokenize()
            .iter()
            .map(|t| t.value.clone())
            .collect()
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
        assert_eq!(get_tokens("1.33"), vec![Token::Float(1.33)]);
        assert_eq!(get_tokens("\"hello\""), vec![Token::String("hello".into())]);
        assert_eq!(
            get_tokens("\"hello\n\""),
            vec![Token::String("hello\n".into())]
        );
    }

    #[test]
    #[should_panic]
    fn test_lexer_unknown_char() {
        assert_eq!(get_tokens("|"), vec![]);
    }

    #[test]
    fn test_tokenize_number() {
        assert_eq!(
            Lexer::new("20.0").tokenize_number('3'),
            Some(Token::Float(320.))
        );
        assert_eq!(
            Lexer::new("20.12").tokenize_number('3'),
            Some(Token::Float(320.12))
        );
        assert_eq!(
            Lexer::new("20").tokenize_number('3'),
            Some(Token::UnsignedInt(320))
        );
    }

    #[test]
    fn test_tokenize_number_single_digit() {
        assert_eq!(
            Lexer::new("mousely").tokenize_number('9'),
            Some(Token::UnsignedInt(9))
        );
    }

    #[test]
    fn test_tokenize_number_negative_int() {
        assert_eq!(
            Lexer::new("20").tokenize_number('-'),
            Some(Token::SignedInt(-20))
        );
    }

    #[test]
    fn test_tokenize_number_negative_float() {
        assert_eq!(
            Lexer::new("20.0").tokenize_number('-'),
            Some(Token::Float(-20.0))
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
        assert_eq!(Lexer::new("REATE").tokenize_ident('C'), Some(Token::Create));

        assert_eq!(Lexer::new("ND").tokenize_ident('A'), Some(Token::And));

        assert_eq!(Lexer::new("alse").tokenize_ident('f'), Some(Token::False));
    }

    #[test]
    fn test_tokenize_ident_with_underscore() {
        assert_eq!(
            Lexer::new("_snoobers").tokenize_ident('x'),
            Some(Token::Identifier("x_snoobers".into()))
        );
    }

    #[test]
    fn test_position() {
        let tokens = Lexer::new("hello\nhello\nhello\n\t\t9\nAND\n").tokenize();

        let token = &tokens[0];
        assert_eq!(token.pos.start_inclusive.0, 0);
        assert_eq!(token.pos.end_inclusive.0, 4);
        assert_eq!(token.pos.line, 1);

        let token = &tokens[1];
        assert_eq!(token.pos.start_inclusive.0, 6);
        assert_eq!(token.pos.end_inclusive.0, 10);
        assert_eq!(token.pos.line, 2);

        let token = &tokens[2];
        assert_eq!(token.pos.start_inclusive.0, 12);
        assert_eq!(token.pos.end_inclusive.0, 16);
        assert_eq!(token.pos.line, 3);

        let token = &tokens[3];
        assert_eq!(token.pos.start_inclusive.0, 20);
        assert_eq!(token.pos.end_inclusive.0, 20);
        assert_eq!(token.pos.line, 4);

        let token = &tokens[4];
        assert_eq!(token.pos.start_inclusive.0, 22);
        assert_eq!(token.pos.end_inclusive.0, 24);
        assert_eq!(token.pos.line, 5);
    }
}
