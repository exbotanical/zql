#[derive(Debug, Default, Clone, Copy)]
pub struct BytePos(pub u32);

impl BytePos {
    pub fn shift(self, ch: char) -> Self {
        BytePos(self.0 + ch.len_utf8() as u32)
    }
}

pub struct PosRange {
    pub start_inclusive: BytePos,
    pub end_inclusive: BytePos,
}

impl PosRange {
    pub const fn empty() -> PosRange {
        let zero = BytePos(0);
        PosRange {
            start_inclusive: zero,
            end_inclusive: zero,
        }
    }
}

pub struct WithPosRange<T> {
    pub value: T,
    pub pos: PosRange,
}

impl<T> WithPosRange<T> {
    pub fn new(value: T, start_inclusive: BytePos, end_inclusive: BytePos) -> WithPosRange<T> {
        WithPosRange {
            value,
            pos: PosRange {
                start_inclusive,
                end_inclusive,
            },
        }
    }

    pub const fn empty(value: T) -> WithPosRange<T> {
        WithPosRange {
            value,
            pos: PosRange::empty(),
        }
    }
}
