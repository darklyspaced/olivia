use std::fmt::Debug;
use std::{fmt::Display, iter::Peekable};

use crate::{
    error::{
        self,
        lerr::{LexError, LexErrorKind},
        perr::ParseError,
        reportable::Reportable,
        source_map::SourceMap,
    },
    lexer::{Lexer, TokenKind},
    ty::Ty,
};

type Error = error::Error<ParseError>;

#[derive(Debug)]
pub enum Node {
    Statement,
    Declaration,
    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    BinOp(TokenKind, Box<Expr>, Box<Expr>),
    UnaryOp(TokenKind, Box<Expr>),
    Atom(Ty),
}

impl Iterator for Parser<'_> {
    type Item = Result<Node, Error>;
    /// How this function operates is dependant on `State`. If `State::Parse`, then we parse. If
    /// `State::Recover` then we must recover then change the state back to `State::Parse` and then
    /// return a token.
    ///
    /// If `State::Recover`, then we can assume that the last item returned from this iterater was
    /// `Error`
    ///
    /// Need some way to know where to recover until specific token?
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.state {
                State::Parse => return Some(Self::parse(self)),
                State::Recover => {
                    while self
                        .toks
                        .next_if(|tok| {
                            tok.as_ref().is_ok_and(|x| {
                                !matches!(
                                    x.kind,
                                    TokenKind::Semicolon | TokenKind::While | TokenKind::For
                                )
                            })
                        })
                        .is_some()
                    {}
                    self.state = State::Parse;
                }
                State::Abort => return None,
            }
        }
    }
}

/// The state of the parser
pub enum State {
    /// Parse
    Parse,
    /// Recover until the next keywork or end of statement
    Recover,
    /// Return None ad infinitum
    Abort,
}

pub struct Parser<'de> {
    state: State,
    source_map: SourceMap,
    toks: Peekable<Lexer<'de>>,
}

impl<'de> Parser<'de> {
    pub fn new(iter: Lexer<'de>, source_map: SourceMap) -> Self {
        Self {
            state: State::Parse,
            toks: iter.peekable(),
            source_map,
        }
    }

    pub fn parse(&mut self) -> Result<Node, Error> {
        if self
            .toks
            .next_if(|tok| tok.as_ref().is_ok_and(|x| x.kind == TokenKind::Let))
            .is_some()
        {
            self.declaration()
        } else {
            Ok(Node::Expr(self.expr(0)?))
        }
    }

    fn declaration(&mut self) -> Result<Node, Error> {
        unimplemented!()
    }

    /// An implementation of Pratt Parsing to deal with mathematical operations. All calls to this
    /// function from outside of itself must have `min_bp` = 0.
    fn expr(&mut self, min_bp: u8) -> Result<Expr, Error> {
        let next = match self.toks.peek() {
            Some(x) => match x {
                Ok(_) => self.toks.next().unwrap()?,
                Err(e) => {
                    self.state = match e.kind {
                        LexErrorKind::UnexpectedCharacter => State::Recover,
                        LexErrorKind::UnterminatedStringLiteral => State::Abort,
                    };
                    let err = self.toks.next().unwrap().unwrap_err();
                    return Err(err.into());
                }
            },
            None => unimplemented!(
                "TODO: add an error that says expected expression, found end of file"
            ),
        };
        let mut lhs = match next.kind {
            TokenKind::Number | TokenKind::Float => Expr::Atom(next.val()),
            x => unimplemented!(
                "TODO: Error that says that {x} was found in place of was found in place of ..."
            ),
        };

        while let Some(tok) = self.toks.peek() {
            let tok = tok.as_ref().unwrap();
            match infix_binding_power(&tok.kind) {
                Some((l, r)) => {
                    if l < min_bp {
                        // at this point, we fold towards the left
                        break;
                    }

                    // only want to consume once we know that we're folding so that after
                    // folding we can resume on the operator that had a lower BP to the left of
                    // it
                    let next = self.toks.next().unwrap()?;
                    let rhs = self.expr(r)?;
                    lhs = Expr::BinOp(next.kind, Box::new(lhs), Box::new(rhs))
                }

                None => panic!("expected {{+, -, /, *}} found {}", next.kind),
            };
        }

        Ok(lhs)
    }
}

fn infix_binding_power(op: &TokenKind) -> Option<(u8, u8)> {
    match op {
        TokenKind::Plus | TokenKind::Minus => Some((1, 2)),
        TokenKind::Star | TokenKind::Slash => Some((3, 4)),
        _ => None,
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::BinOp(token_kind, er, e1) => write!(f, "({} {} {})", token_kind, er, e1),
            Expr::UnaryOp(token_kind, e) => write!(f, "({}{})", token_kind, e),
            Expr::Atom(ty) => write!(f, "{}", ty),
        }
    }
}
