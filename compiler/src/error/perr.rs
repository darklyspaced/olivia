use super::{
    lerr::LexErrorKind,
    reportable::{Ctxt, RawCtxt, Reportable},
};
use crate::lexer::TokenKind;

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    /// The line number, line, and offset of annotation
    pub ctxt: Option<RawCtxt>,
}

#[derive(Debug)]
pub enum ParseErrorKind {
    Lexing(LexErrorKind),
    ExpExprFound(String),
    ExpOpFound(String),
    ExpOperandFound(String),
    ExpIdentFound(String),
    ExpSemicolonFound(String),
    Expected(Vec<TokenKind>, String),
    /// Places where an error kind is expected but it will never be produced
    Unreachable,
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
            ParseErrorKind::ExpExprFound(_) => String::from("expected expr here"),
            ParseErrorKind::ExpOpFound(_) => "expected op".to_string(),
            ParseErrorKind::ExpOperandFound(_) => "expected operand".to_string(),
            ParseErrorKind::ExpIdentFound(_) => "expected ident".to_string(),
            ParseErrorKind::ExpSemicolonFound(_) => "expected semicolon".to_string(),
            ParseErrorKind::Expected(vec, _) => {
                let list = vec
                    .iter()
                    .map(|kind| format!("`{}`", kind.to_string()))
                    .collect::<Vec<_>>()
                    .join(",");
                format!("expected one of `{list}`")
            }
            ParseErrorKind::Unreachable => unreachable!(),
        }
    }

    /// What goes in the title of an error message
    pub fn message(&self) -> String {
        match self {
            ParseErrorKind::Lexing(lex_error_kind) => lex_error_kind.message(),
            ParseErrorKind::ExpExprFound(x) => {
                format!("found `{x}` in place of an expression")
            }
            ParseErrorKind::ExpOpFound(x) => {
                format!("found `{}` where operator was expected", x)
            }
            ParseErrorKind::ExpOperandFound(x) => {
                format!("found `{}` where operand was expected", x)
            }
            ParseErrorKind::ExpIdentFound(x) => {
                format!("found `{}` where ident was expected", x)
            }
            ParseErrorKind::ExpSemicolonFound(x) => {
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
            ParseErrorKind::Unreachable => unreachable!(),
        }
    }
}
