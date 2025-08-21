use std::fmt::Display;

use super::reportable::{Ctxt, RawCtxt, Reportable};

#[derive(Debug)]
pub struct NameResError {
    pub kind: NameResErrorKind,
    pub ctxt: Option<RawCtxt>,
}

#[derive(Debug, Clone, Copy)]
pub enum NameResErrorKind {
    UnexpectedCharacter,
    UnterminatedStringLiteral,
}

impl Reportable for NameResError {
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

impl NameResErrorKind {
    pub fn annotation(&self) -> String {
        String::from(match self {
            Self::UnexpectedCharacter => "unexpected character here",
            Self::UnterminatedStringLiteral => "starts here",
        })
    }

    pub fn message(&self) -> String {
        String::from(match self {
            NameResErrorKind::UnexpectedCharacter => "unexpected character found here",
            NameResErrorKind::UnterminatedStringLiteral => {
                "unterminated string literal, expected '\"' but found EOF"
            }
        })
    }
}

impl Display for NameResErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NameResErrorKind::UnexpectedCharacter => writeln!(f, "unexpected character found"),
            NameResErrorKind::UnterminatedStringLiteral => {
                writeln!(f, "unterminated string literal")
            }
        }
    }
}
