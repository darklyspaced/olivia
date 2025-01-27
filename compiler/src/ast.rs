use std::{
    fmt::{Debug, Display},
    iter::Peekable,
};

use crate::{
    error::{
        self,
        lerr::LexErrorKind,
        perr::{ParseError, ParseErrorKind as PEKind},
        source_map::SourceMap,
    },
    lexer::{Lexer, Token, TokenKind},
    ty::Ty,
};

type Error = error::Error<ParseError>;

macro_rules! lex_err {
    ($self:ident, $err:ident, $until:expr) => {
        $self.state = match $err.kind() {
            LexErrorKind::UnexpectedCharacter => State::Recover($until),
            LexErrorKind::UnterminatedStringLiteral => State::Abort,
        };
        let err = $self.toks.next().unwrap().unwrap_err();
        return Err(err.into());
    };
}

macro_rules! error {
    ($self:ident, $kind:expr, $tok:expr) => {
        return Err(ParseError {
            kind: $kind,
            ctxt: Some(
                $self
                    .source_map
                    .ctxt_from_tok($tok)
                    .with(line!(), column!()),
            ),
        }
        .into())
    };
    ($self:ident, $kind:expr) => {
        return Err(ParseError {
            kind: $kind,
            ctxt: Some($self.source_map.ctxt_from_end().with(line!(), column!())),
        }
        .into())
    };
}

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

pub enum Op {
    Add,
    Mult,
    Div,
    Sub,
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
            match &self.state {
                State::Parse => return Some(Self::parse(self)),
                State::Recover(kinds) => {
                    while self
                        .toks
                        .next_if(|tok| {
                            tok.as_ref().is_ok_and(|x| {
                                for kind in kinds {
                                    if x.kind == *kind {
                                        return true;
                                    }
                                }
                                false
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
    /// Recover until we see one of`.0`
    Recover(Vec<TokenKind>),
    /// Return None ad infinitum
    Abort,
}

pub struct Parser<'de> {
    state: State,
    source_map: &'de SourceMap,
    toks: Peekable<Lexer<'de>>,
}

impl<'de> Parser<'de> {
    pub fn new(iter: Lexer<'de>, source_map: &'de SourceMap) -> Self {
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

    /// Parse a declaration, given that `let` has already been consumed
    fn declaration(&mut self) -> Result<Node, Error> {
        // ident
        match self.toks.peek() {
            Some(res) => match res {
                Ok(tok) => match tok.kind {
                    TokenKind::Ident => {
                        let _ident = self.toks.next();
                    }
                    _ => {
                        self.state = State::Recover(vec![TokenKind::Equal, TokenKind::Semicolon]);
                        error!(
                            self,
                            PEKind::ExpectedIdentFound(String::from(tok.lexeme)),
                            tok
                        )
                    }
                },
                Err(e) => {
                    lex_err!(self, e, vec![TokenKind::Equal]);
                }
            },
            None => error!(self, PEKind::ExpectedIdentFound(String::from("EOF"))),
        }

        // equals + expr OR semi
        match self.toks.peek() {
            Some(res) => match res {
                Ok(tok) => match tok.kind {
                    TokenKind::Semicolon => todo!("bare decl with no value"),
                    TokenKind::Equal => {
                        self.toks.next();

                        let expr = self.expr(0);
                        let Ok(expr) = expr else {
                            self.state = State::Recover(vec![TokenKind::Semicolon]);
                            return Err(expr.unwrap_err());
                        };

                        match self.toks.peek() {
                            Some(res) => match res {
                                Ok(tok) => match tok {},
                            },
                            None => {
                                error!(self, PEKind::ExpectedSemicolonFound(String::from("EOF")))
                            }
                        }

                        Ok(Node::Expr(Expr::Atom(Ty::Integer(0))))
                    }
                    _ => {
                        self.state = State::Recover(vec![TokenKind::Semicolon, TokenKind::Equal]);
                        error!(
                            self,
                            PEKind::Expected(
                                vec![TokenKind::Semicolon, TokenKind::Equal],
                                String::from(tok.lexeme)
                            )
                        );
                    }
                },
                Err(e) => {
                    lex_err!(self, e, vec![TokenKind::Equal, TokenKind::Semicolon]);
                }
            },
            None => error!(self, PEKind::ExpectedSemicolonFound(String::from("EOF"))),
        }
    }

    /// An implementation of Pratt Parsing to deal with mathematical operations. All calls to this
    /// function from outside of itself must have `min_bp` = 0.
    fn expr(&mut self, min_bp: u8) -> Result<Expr, Error> {
        let next = match self.toks.peek() {
            Some(x) => match x {
                Ok(_) => self.toks.next().unwrap()?,
                Err(e) => {
                    self.state = match e.kind() {
                        LexErrorKind::UnexpectedCharacter => {
                            State::Recover(vec![TokenKind::Semicolon])
                        }
                        LexErrorKind::UnterminatedStringLiteral => State::Abort,
                    };
                    let err = self.toks.next().unwrap().unwrap_err();
                    return Err(err.into());
                }
            },
            None => {
                return Err(ParseError {
                    kind: PEKind::ExpectedExprFoundEOF,
                    ctxt: Some(self.source_map.ctxt_from_end().with(line!(), column!())),
                }
                .into());
            }
        };
        let mut lhs = match next.kind {
            TokenKind::Number | TokenKind::Float => Expr::Atom(next.val()),
            _ => {
                return Err(ParseError {
                    kind: PEKind::ExpectedOperandFound(String::from(next.lexeme)),
                    ctxt: Some(self.source_map.ctxt_from_tok(&next)),
                }
                .into());
            }
        };

        while let Some(res) = self.toks.peek() {
            let tok = match res {
                Ok(t) => t,
                Err(e) => {
                    lex_err!(self, e, vec![TokenKind::Semicolon]);
                }
            };

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

                None => {
                    return Err(ParseError {
                        kind: PEKind::ExpectedOpFound(tok.lexeme.to_string()),
                        ctxt: Some(self.source_map.ctxt_from_tok(tok).with(line!(), column!())),
                    }
                    .into());
                }
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

impl<'de> TryFrom<Token<'de>> for Op {
    type Error = PEKind;

    fn try_from(value: Token<'de>) -> Result<Self, Self::Error> {
        match value.kind {
            TokenKind::Plus => Ok(Op::Add),
            TokenKind::Minus => Ok(Op::Sub),
            TokenKind::Star => Ok(Op::Mult),
            TokenKind::Slash => Ok(Op::Div),
            _ => Err(PEKind::ExpectedOpFound(String::from(value.lexeme))),
        }
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
