use std::iter::Peekable;

use super::{Parser, State};

use crate::{
    error::{
        self,
        lex_err::{LexError, LexErrorKind},
        parse_err::{ParseError, ParseErrorKind as PEKind},
    },
    lexer::Lexer,
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

impl<'de> Parser<'de> {
    /// Peeks the next token and handles error cases. `err_kind` is for the EOF case
    // TODO: make this a try_peek macro so that line and column information can be captured
    // correctly
    pub(super) fn peek(
        &mut self,
        eof_err: impl FnOnce(String) -> PEKind,
    ) -> Result<&Token<'_>, Error> {
        self.eat_ws();
        if let Some(Err(e)) = self.peek_raw() {
            let kind = e.kind();
            self.state = match kind {
                LexErrorKind::UnexpectedCharacter => State::Recover,
                LexErrorKind::UnterminatedStringLiteral => State::Abort,
            };

            let err = self.toks.next().unwrap().unwrap_err();
            return Err(err.into());
        }

        self.eat_ws();
        match self.toks.peek() {
            Some(Ok(tok)) => Ok(tok),
            Some(Err(_)) => unreachable!(),
            None => error!(self, eof_err(String::from("EOF"))),
        }
    }

    /// Should only be used the case that the next token **exists** but isn't what it should be.
    // TODO: make a wrapper for this so that the line and column are captured correctly
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

    /// Attempts to parse an ident and interns its symbol, returning any `err` if it fails
    pub(super) fn ident(&mut self, err: impl FnOnce(String) -> PEKind) -> Result<(), Error> {
        self.eat_ws();
        let next = self.peek(PEKind::ExpIdentFound)?;
        let ident = match next.kind {
            TokenKind::Ident => self.toks.next().unwrap().unwrap(),
            _ => return Err(self.make_err(err)),
        };

        let width = ident.lexeme.len();
        let ident = crate::ast::Ident {
            sym: self.interner.intern(ident.lexeme),
            width,
            // span: self.source_map.span_from_tok(&ident),
        };

        self.builder.leaf_name(ident);
        Ok(())
    }

    /// Eats `kind` otherwise throws `err`
    pub(super) fn eat<G>(&mut self, kind: TokenKind, err: G) -> Result<(), Error>
    where
        G: FnOnce(String) -> PEKind + Clone,
    {
        self.eat_ws();
        let equals = self.peek(err.clone())?;
        if equals.kind == kind {
            Ok(self.consume())
        } else {
            return Err(self.make_err(err));
        }
    }

    pub(super) fn consume(&mut self) {
        self.eat_ws();
        self.builder.leaf(self.toks.next().unwrap().unwrap())
    }

    pub(super) fn next_if(
        &mut self,
        cb: impl FnOnce(Option<&<Peekable<Lexer<'_>> as Iterator>::Item>) -> bool,
    ) -> bool {
        self.eat_ws();
        if cb(self.toks.peek()) {
            self.consume();
            true
        } else {
            false
        }
    }

    /// Eats whitespace. Should be called at the start of every helper function because otherwise
    /// it might not match correctly
    fn eat_ws(&mut self) {
        if let Some(Ok(Token {
            kind: TokenKind::Whitespace,
            ..
        })) = self.toks.peek()
        {
            self.consume();
        }
    }

    pub(super) fn peek_raw(&mut self) -> Option<&<Peekable<Lexer<'_>> as Iterator>::Item> {
        self.eat_ws();
        self.toks.peek()
    }
}

mod no_reg {
    //! This is only for the expression subsection of the parser since the order for parsing and
    //! adding to the green tree there is slightly goofy. Parses things and returns them
    //! **without** adding them to the green tree that's being created.

    use super::Parser;

    use crate::{
        ast::Ident,
        error::{
            self,
            parse_err::{ParseError, ParseErrorKind as PEKind},
        },
        token::{Token, TokenKind},
    };

    type Error = error::Error<ParseError>;

    impl Parser<'_> {
        pub(in super::super) fn ident_noreg(
            &mut self,
            err: impl FnOnce(String) -> PEKind,
        ) -> Result<Ident, Error> {
            self.eat_ws();
            let next = self.peek(PEKind::ExpIdentFound)?;
            let ident = match next.kind {
                TokenKind::Ident => self.toks.next().unwrap().unwrap(),
                _ => return Err(self.make_err(err)),
            };

            let width = ident.lexeme.len();
            let ident = crate::ast::Ident {
                sym: self.interner.intern(ident.lexeme),
                width,
            };

            Ok(ident)
        }
    }
}
