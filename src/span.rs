use std::{
    cmp::{max, min},
    fmt::Display,
    ops::Range,
};

use crate::context::SourceId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Span {
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn at(pos: usize) -> Self {
        Self::new(pos, pos)
    }

    pub fn is_empty(&self) -> bool {
        self.start >= self.end
    }

    pub fn size(&self) -> usize {
        if self.is_empty() {
            return 0;
        }

        self.end - self.start
    }

    /// Create a span encompassing the endpoints of this span and another.
    pub fn union(self, other: Self) -> Self {
        Self::new(min(self.start, other.start), max(self.end, other.end))
    }

    /// Create a span with our starting point, ending at the new end point.
    pub fn until(self, end: usize) -> Self {
        Self::new(self.start, end)
    }
}

impl Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

impl From<Span> for Range<usize> {
    fn from(value: Span) -> Self {
        value.start..value.end
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Self {
            start: value.start,
            end: value.end,
        }
    }
}

impl From<usize> for Span {
    fn from(value: usize) -> Self {
        Self::at(value)
    }
}
