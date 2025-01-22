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
    pub fn annotation(&self) -> String {
        match self {
            ParseErrorKind::Lexing(lex_error_kind) => lex_error_kind.annotation(),
            ParseErrorKind::ExpectedExprFoundEOF => String::from("expected expr here"),
        }
    }

    pub fn message(&self) -> String {
        match self {
            ParseErrorKind::Lexing(lex_error_kind) => lex_error_kind.message(),
            ParseErrorKind::ExpectedExprFoundEOF => {
                String::from("found EOF in place of an expression ")
            }
        }
    }
}

//impl Display for ParseErrorKind {
//    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//        match self {
//            ParseErrorKind::ExpectedExprFoundEOF => writeln!(f, "expected Expr, found EOF"),
//            ParseErrorKind::Lexing(kind) => writeln!(f, "{}", kind),
//        }
//    }
//}
