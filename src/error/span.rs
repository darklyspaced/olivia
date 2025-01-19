#[derive(Debug)]
pub struct Span {
    /// Pointer to start of span
    start: *const u8,
    /// Byte size of span
    size: usize,
    /// Pointer to source of span
    origin: *const u8,
}
