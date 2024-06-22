#[derive(Debug, Default, Clone, Copy)]
pub struct BytePos(pub u32);

impl BytePos {
    pub fn shift(self, ch: char) -> Self {
        BytePos(self.0 + ch.len_utf8() as u32)
    }
}

#[derive(Debug)]
pub struct TokenMetadata {
    pub start_inclusive: BytePos,
    pub end_inclusive: BytePos,
    pub line: usize,
}

impl TokenMetadata {
    pub const fn empty() -> TokenMetadata {
        let zero = BytePos(0);
        TokenMetadata {
            start_inclusive: zero,
            end_inclusive: zero,
            line: 0,
        }
    }
}

#[derive(Debug)]
pub struct WithTokenMetadata<T> {
    pub value: T,
    pub pos: TokenMetadata,
}

impl<T> WithTokenMetadata<T> {
    pub fn new(
        value: T,
        start_inclusive: BytePos,
        end_inclusive: BytePos,
        line: usize,
    ) -> WithTokenMetadata<T> {
        WithTokenMetadata {
            value,
            pos: TokenMetadata {
                start_inclusive,
                end_inclusive,
                line,
            },
        }
    }

    pub const fn empty(value: T) -> WithTokenMetadata<T> {
        WithTokenMetadata {
            value,
            pos: TokenMetadata::empty(),
        }
    }
}
