use std::fmt::Display;

use super::reportable::{Ctxt, RawCtxt, Reportable};

#[derive(Debug)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub ctxt: Option<RawCtxt>,
}

#[derive(Debug, Clone, Copy)]
pub enum LexErrorKind {
    UnexpectedCharacter,
    UnterminatedStringLiteral,
}

impl Reportable for LexError {
    fn ctxt(&mut self) -> Vec<Ctxt> {
        let annotation = self.kind.annotation();

        vec![Ctxt {
            inner: self.ctxt.take().expect("should only call ctxt once"), // should function fine
            // if only called once
            annotation,
        }]
    }

    fn msg(&self) -> String {
        self.kind.message()
    }

    fn code(&self) -> usize {
        1
    }
}

impl LexErrorKind {
    pub fn annotation(&self) -> String {
        String::from(match self {
            Self::UnexpectedCharacter => "unexpected character here",
            Self::UnterminatedStringLiteral => "starts here",
        })
    }

    pub fn message(&self) -> String {
        String::from(match self {
            LexErrorKind::UnexpectedCharacter => "unexpected character found here",
            LexErrorKind::UnterminatedStringLiteral => {
                "unterminated string literal, expected '\"' but found EOF"
            }
        })
    }
}

impl Display for LexErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexErrorKind::UnexpectedCharacter => writeln!(f, "unexpected character found"),
            LexErrorKind::UnterminatedStringLiteral => writeln!(f, "unterminated string literal"),
        }
    }
}
