use lerr::{LexError, LexErrorKind};
use perr::{ParseError, ParseErrorKind};
use reportable::{Ctxt, Reportable};

pub mod lerr;
pub mod perr;
pub mod report;
pub mod reportable;
pub mod source_map;
pub mod span;

#[derive(Debug)]
/// The main error type produced by the compiler
pub struct Error<E>
where
    E: Reportable,
{
    /// The actual error
    pub inner: Box<E>,
}

impl Error<ParseError> {
    pub fn kind(&self) -> &ParseErrorKind {
        &self.inner.kind
    }
}

impl Error<LexError> {
    pub fn kind(&self) -> &LexErrorKind {
        &self.inner.kind
    }
}

impl<E> Reportable for Error<E>
where
    E: Reportable,
{
    fn msg(&self) -> String {
        self.inner.msg()
    }

    fn ctxt(&mut self) -> Vec<Ctxt> {
        self.inner.ctxt()
    }

    fn code(&self) -> usize {
        1
    }
}

impl<E> From<E> for Error<E>
where
    E: Reportable,
{
    fn from(value: E) -> Self {
        Self {
            inner: Box::new(value),
        }
    }
}

impl From<Error<LexError>> for Error<ParseError> {
    fn from(value: Error<LexError>) -> Self {
        ParseError {
            kind: perr::ParseErrorKind::Lexing(value.inner.kind),
            ctxt: value.inner.ctxt,
        }
        .into()
    }
}
