use crate::lexer::TokenKind;

use super::{
    lerr::LexErrorKind,
    reportable::{Ctxt, RawCtxt, Reportable},
};

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    /// The line number, line, and offset of annotation
    pub ctxt: Option<RawCtxt>,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    Lexing(LexErrorKind),
    ExpectedExprFoundEOF,
    ExpectedOpFound(String),
    ExpectedOperandFound(String),
    ExpectedIdentFound(String),
    ExpectedSemicolonFound(String),
    Expected(Vec<TokenKind>, String),
}

impl Reportable for ParseError {
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

impl ParseErrorKind {
    /// The annotation that goes below the upticks in the context of an error message
    pub fn annotation(&self) -> String {
        match self {
            ParseErrorKind::Lexing(lex_error_kind) => lex_error_kind.annotation(),
            ParseErrorKind::ExpectedExprFoundEOF => String::from("expected expr here"),
            ParseErrorKind::ExpectedOpFound(_) => format!("expected op"),
            ParseErrorKind::ExpectedOperandFound(_) => format!("expected operand"),
            ParseErrorKind::ExpectedIdentFound(_) => format!("expected ident"),
            ParseErrorKind::ExpectedSemicolonFound(_) => format!("expected semicolon"),
            ParseErrorKind::Expected(vec, x) => {
                let list = vec
                    .iter()
                    .map(|kind| format!("`{}`", kind.to_string()))
                    .collect::<Vec<_>>()
                    .join(",");
                format!("expected one of {list}")
            }
        }
    }

    /// What goes in the title of an error message
    pub fn message(&self) -> String {
        match self {
            ParseErrorKind::Lexing(lex_error_kind) => lex_error_kind.message(),
            ParseErrorKind::ExpectedExprFoundEOF => {
                String::from("found EOF in place of an expression ")
            }
            ParseErrorKind::ExpectedOpFound(x) => {
                format!("found `{}` where operator was expected", x)
            }
            ParseErrorKind::ExpectedOperandFound(x) => {
                format!("found `{}` where operand was expected", x)
            }
            ParseErrorKind::ExpectedIdentFound(x) => {
                format!("found `{}` where ident was expected", x)
            }
            ParseErrorKind::ExpectedSemicolonFound(x) => {
                format!("found `{x}` where semicolon was expected")
            }
            ParseErrorKind::Expected(vec, x) => {
                let list = vec
                    .iter()
                    .map(|kind| format!("`{}`", kind.to_string()))
                    .collect::<Vec<_>>()
                    .join(",");
                format!("expected one of {list}, found {x}")
            }
        }
    }
}
