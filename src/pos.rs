#[derive(Debug, Default, Clone, Copy)]
pub struct BytePos(pub u32);

impl BytePos {
    pub fn shift(self, ch: char) -> Self {
        BytePos(self.0 + ch.len_utf8() as u32)
    }
}

#[derive(Debug)]
pub struct PosMetadata {
    pub start_inclusive: BytePos,
    pub end_inclusive: BytePos,
    pub line: usize,
    // TODO: col
}

impl PosMetadata {
    pub const fn empty() -> PosMetadata {
        let zero = BytePos(0);
        PosMetadata {
            start_inclusive: zero,
            end_inclusive: zero,
            line: 0,
        }
    }
}

#[derive(Debug)]
pub struct WithPosMetadata<T> {
    pub value: T,
    pub pos: PosMetadata,
}

impl<T> WithPosMetadata<T> {
    pub fn new(
        value: T,
        start_inclusive: BytePos,
        end_inclusive: BytePos,
        line: usize, // TODO: when to use usize vs u64?
    ) -> WithPosMetadata<T> {
        WithPosMetadata {
            value,
            pos: PosMetadata {
                start_inclusive,
                end_inclusive,
                line,
            },
        }
    }

    pub const fn empty(value: T) -> WithPosMetadata<T> {
        WithPosMetadata {
            value,
            pos: PosMetadata::empty(),
        }
    }
}
