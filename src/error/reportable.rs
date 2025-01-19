pub trait Reportable<'de> {
    /// Returns the error code of the error
    fn code() -> usize;
    /// Returns context for error in the form of lines, their numbers, and the error message and
    /// the offset of the annotation for each message relative to start of line
    fn ctxt(&self) -> Vec<Ctxt<'de>>;
    /// Returns the error message
    fn msg() -> String;
}

pub struct Ctxt<'de> {
    /// The line of context itself
    pub line: &'de str,
    /// Line number
    pub num: usize,
    /// Offset of annotation
    pub offset: usize,
    /// Annotation for the context
    pub annotation: String,
}
