#[derive(Debug)]
/// Byte offset that is always relative to start of file
struct ByteOffset(usize);

/// Annotations to AST nodes so that they can be located in their original files
#[derive(Debug)]
pub struct Span {
    start: ByteOffset,
    end: ByteOffset,
}

impl Span {
    pub fn from(start: usize, end: usize) -> Self {
        Self {
            start: ByteOffset(start),
            end: ByteOffset(end),
        }
    }
}
