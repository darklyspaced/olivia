use std::fmt::Display;

use super::{
    lerr::LexErrorKind,
    reportable::{Ctxt, Reportable},
};

#[derive(Debug)]
pub struct ParseErr<'de> {
    pub kind: ParseErrorKind,
    /// The line and line number
    pub ctxt: (usize, &'de str),
}

impl<'de> Reportable<'de> for ParseErr<'de> {
    fn ctxt(&self) -> Vec<Ctxt<'de>> {
        let annotation = String::from(match self.kind {
            ParseErrorKind::ExpectedExprFoundEOF => "EOF here",
            ParseErrorKind::Lexing(kind) => kind.annotation().as_str(),
        });
        vec![Ctxt {
            line: self.ctxt.1,
            num: self.ctxt.0,
            offset: todo!(),
            annotation,
        }]
    }
    fn msg() -> String {
        todo!()
    }
    fn code() -> usize {
        todo!()
    }
}

#[derive(Debug)]
pub enum ParseErrorKind {
    Lexing(LexErrorKind),
    ExpectedExprFoundEOF,
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErrorKind::ExpectedExprFoundEOF => writeln!(f, "expected Expr, found EOF"),
            ParseErrorKind::Lexing(kind) => writeln!(f, "{}", kind),
        }
    }
}

impl std::error::Error for ParseErr<'_> {}
