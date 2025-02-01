use super::{
    lex_err::LexErrorKind,
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
    ExpEqualFound(String),
    ExpFound(Vec<TokenKind>, String),
    SolelyAssDecl,
    /// Places where an error kind is expected but it will never be produced
    Unreachable(String),
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
            ParseErrorKind::ExpOpFound(_) => String::from("expected op"),
            ParseErrorKind::ExpOperandFound(_) => String::from("expected operand"),
            ParseErrorKind::ExpIdentFound(_) => String::from("expected ident"),
            ParseErrorKind::ExpSemicolonFound(_) => String::from("expected semicolon"),
            ParseErrorKind::ExpFound(vec, _) => {
                let list = vec
                    .iter()
                    .map(|kind| format!("`{}`", kind.to_string()))
                    .collect::<Vec<_>>()
                    .join(",");
                format!("expected one of `{list}`")
            }
            ParseErrorKind::Unreachable(_) => unreachable!(),
            ParseErrorKind::SolelyAssDecl => String::from("not allowed here"),
            ParseErrorKind::ExpEqualFound(_) => String::from("expected equals here"),
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
            ParseErrorKind::ExpEqualFound(x) => {
                format!("found `{x}` where equals was expected")
            }
            ParseErrorKind::ExpSemicolonFound(x) => {
                format!("found `{x}` where semicolon was expected")
            }
            ParseErrorKind::ExpFound(vec, x) => {
                let list = vec
                    .iter()
                    .map(|kind| format!("`{}`", kind.to_string()))
                    .collect::<Vec<_>>()
                    .join(",");
                format!("expected one of {list}, found {x}")
            }
            ParseErrorKind::SolelyAssDecl => {
                String::from("only assignment and declarations are supported as of now")
            }
            ParseErrorKind::Unreachable(_) => unreachable!(),
        }
    }
}
