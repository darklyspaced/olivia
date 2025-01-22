use std::fmt::Debug;
use std::fmt::Display;

use lerr::LexError;
use perr::ParseError;
use reportable::{Ctxt, Reportable};

pub mod lerr;
pub mod perr;
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
    inner: E,
    /// Where in code this error was generated (line, column)
    code_pos: (usize, usize),
}

impl<'de, E> Reportable for Error<E>
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

impl<E> Display for Error<E>
where
    E: Reportable,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "error[{}]: {}", self.code(), self.msg())
    }
}

impl<E> std::error::Error for Error<E> where E: Reportable + Debug {}
