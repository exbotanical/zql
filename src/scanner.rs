use std::{iter::Peekable, str::Chars};

const NULL_CHAR: char = '\u{0000}';

#[derive(Debug, Default, Clone, Copy)]
pub struct BytePos(pub u32);

impl BytePos {
    fn shift(self, ch: char) -> Self {
        BytePos(self.0 + ch.len_utf8() as u32)
    }
}

pub struct Scanner<'a> {
    line: i64,
    pos: BytePos,
    buf: Peekable<Chars<'a>>,
}

impl<'a> Scanner<'a> {
    pub fn new(buf: &str) -> Scanner {
        Scanner {
            line: 1,
            pos: BytePos::default(),
            buf: buf.chars().peekable(),
        }
    }

    pub fn next(&mut self) -> Option<char> {
        let next = self.buf.next();
        if let Some(c) = next {
            self.pos = self.pos.shift(c);
        }

        next
    }

    pub fn peek(&mut self) -> Option<char> {
        self.buf.peek().copied()
    }

    pub fn consume_if<F>(&mut self, f: F) -> bool
    where
        F: Fn(char) -> bool,
    {
        if let Some(ch) = self.peek() {
            if f(ch) {
                self.next().unwrap();
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn consume_while<F>(&mut self, f: F) -> Vec<char>
    where
        F: Fn(char) -> bool,
    {
        let mut chars: Vec<char> = Vec::new();
        while let Some(ch) = self.peek() {
            if f(ch) {
                self.next();
                chars.push(ch)
            } else {
                break;
            }
        }

        chars
    }

    #[cfg(test)]
    fn assert_next(&mut self, pos: u32, c: Option<char>) {
        assert_eq!(self.pos.0, pos);
        assert_eq!(self.peek(), c);
        assert_eq!(self.next(), c);
    }
}

#[cfg(test)]
mod tests {
    use super::Scanner;

    #[test]
    fn test_scanner_basic_happy_case() {
        let text = "text";
        let mut scanner = Scanner::new(text);

        scanner.assert_next(0, Some('t'));
        scanner.assert_next(1, Some('e'));
        scanner.assert_next(2, Some('x'));
        scanner.assert_next(3, Some('t'));
        scanner.assert_next(4, None);
    }

    #[test]
    fn test_scanner_nonascii_happy_case() {
        let text = "🗿x🗿x🗿🗿";
        let mut scanner = Scanner::new(text);

        scanner.assert_next(0, Some('🗿'));
        scanner.assert_next(4, Some('x'));
        scanner.assert_next(5, Some('🗿'));
        scanner.assert_next(9, Some('x'));
        scanner.assert_next(10, Some('🗿'));
        scanner.assert_next(14, Some('🗿'));
        scanner.assert_next(18, None);
    }

    #[test]
    fn test_consume_if() {
        let text = "🗿x🗿x🗿🗿";
        let mut scanner = Scanner::new(text);

        assert_eq!(scanner.consume_if(|c| c == '🗿'), true);
        assert_eq!(scanner.consume_if(|c| c == 'x'), true);
        assert_eq!(scanner.consume_if(|c| c == 'q'), false);
        assert_eq!(scanner.consume_if(|c| c == '🗿'), true);
        assert_eq!(scanner.consume_if(|c| c == '🗿'), false);
        assert_eq!(scanner.consume_if(|c| c == 'x'), true);
        assert_eq!(scanner.consume_if(|c| c == '🗿'), true);
        assert_eq!(scanner.consume_if(|c| c == '🗿'), true);
        assert_eq!(scanner.consume_if(|c| c == '🗿'), false);
        assert_eq!(scanner.consume_if(|c| c == '🗿'), false);
    }

    #[test]
    fn test_consume_while() {
        let text = "xxx🗿🗿xxx";
        let mut scanner = Scanner::new(text);

        let x_chars = scanner.consume_while(|c| c == 'x');
        let noop_chars = scanner.consume_while(|c| c == 'c');
        let gumgum_chars = scanner.consume_while(|c| c == '🗿');

        assert_eq!(x_chars, vec!['x', 'x', 'x']);
        assert_eq!(noop_chars, vec![]);
        assert_eq!(gumgum_chars, vec!['🗿', '🗿']);

        let x_chars = scanner.consume_while(|c| c == 'x');
        assert_eq!(x_chars, vec!['x', 'x', 'x']);
    }
}
