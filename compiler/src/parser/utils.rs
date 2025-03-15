use super::{Parser, State};

use crate::{
    ast::Ident,
    error::{
        self,
        lex_err::LexErrorKind,
        parse_err::{ParseError, ParseErrorKind as PEKind},
    },
    token::{Token, TokenKind},
};

type Error = error::Error<ParseError>;

macro_rules! error {
    ($self:ident, $kind:expr, $tok:expr) => {{
        let context = Some(
            $self
                .source_map
                .ctxt_from_tok($tok)
                .with(line!(), column!()),
        );
        $self.state = State::Recover;
        return Err(ParseError {
            kind: $kind,
            ctxt: context,
        }
        .into());
    }};
    // if it's something to do with an EOF
    ($self:ident, $kind:expr) => {
        return Err(ParseError {
            kind: $kind,
            ctxt: Some($self.source_map.ctxt_from_end().with(line!(), column!())),
        }
        .into())
    };
}

impl Parser<'_> {
    /// Peeks the next token and handles error cases. `err_kind` is for the EOF case
    pub fn peek(&mut self, eof_err: impl FnOnce(String) -> PEKind) -> Result<&Token, Error> {
        if let Some(Err(e)) = self.toks.peek() {
            let kind = e.kind();
            self.state = match kind {
                LexErrorKind::UnexpectedCharacter => State::Recover,
                LexErrorKind::UnterminatedStringLiteral => State::Abort,
            };

            let err = self.toks.next().unwrap().unwrap_err();
            return Err(err.into());
        }

        match self.toks.peek() {
            Some(Ok(tok)) => Ok(tok),
            Some(Err(_)) => unreachable!(),
            None => error!(self, eof_err(String::from("EOF"))),
        }
    }

    /// Should only be used the case that the next token exists but isn't what it should be.
    pub fn make_err(&mut self, make_kind: impl FnOnce(String) -> PEKind) -> Error {
        self.state = State::Recover;

        let erroneous_tok = self
            .toks
            .next()
            .expect("shouldn't be EOF")
            .expect("should have been a valid token, not error");
        let kind = make_kind(String::from(erroneous_tok.lexeme));
        let context = Some(
            self.source_map
                .ctxt_from_tok(&erroneous_tok)
                .with(line!(), column!()),
        );
        ParseError {
            kind,
            ctxt: context,
        }
        .into()
    }

    /// Attempts to parse an ident and interns its symbol, returning any errors generated along
    /// the way
    pub fn ident(&mut self) -> Result<Ident, Error> {
        let next = self.peek(PEKind::ExpIdentFound)?;
        let ident = match next.kind {
            TokenKind::Ident => self.toks.next().unwrap().unwrap(),
            _ => return Err(self.make_err(PEKind::ExpIdentFound)),
        };

        Ok(crate::ast::Ident {
            name: self.interner.intern(ident.lexeme),
            span: self.source_map.span_from_tok(&ident),
        })
    }

    pub fn eat<G>(&mut self, kind: TokenKind, err: G) -> Result<Token, Error>
    where
        G: FnOnce(String) -> PEKind + Clone,
    {
        let equals = self.peek(err.clone())?;
        let tok = if equals.kind == kind {
            self.toks.next().unwrap().unwrap()
        } else {
            return Err(self.make_err(err));
        };
        Ok(tok)
    }
}
