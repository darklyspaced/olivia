use std::fmt::Debug;

pub trait Reportable: Debug {
    /// Returns the error code of the error
    fn code(&self) -> usize;
    /// Returns context for error in the form of lines, their numbers, and the error message and
    /// the offset of the annotation for each message relative to start of line
    fn ctxt(&mut self) -> Vec<Ctxt>;
    /// Returns the error message
    fn msg(&self) -> String;
}

pub struct Ctxt {
    /// The rest of the context, unannotated produced by errors inline when they are generated
    pub inner: RawCtxt,
    /// Annotation for the context
    pub annotation: String,
}

/// This is all the context for an error that is computed when it is initially generated
#[derive(Debug)]
pub struct RawCtxt {
    /// The line of context itself
    pub line: String,
    /// Line number
    pub num: usize,
    /// Offset of annotation
    pub offset: usize,
    /// Where in code this error was generated (line, column)
    pub code_pos: (usize, usize),
}
