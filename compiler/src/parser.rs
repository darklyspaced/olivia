pub mod utils;

use std::iter::Peekable;

use crate::{
    ast::{Expr, Ident, Node, Op, OpKind},
    error::{
        self,
        parse_err::{ParseError, ParseErrorKind as PEKind},
        source_map::SourceMap,
    },
    interner::Interner,
    lexer::{Lexer, TokenKind},
    value::Value,
};

type Error = error::Error<ParseError>;

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
                State::Parse => {
                    if self.toks.peek().is_none() {
                        self.state = State::Finished;
                        continue;
                    }
                    return Some(self.stmt());
                }
                State::Recover => {
                    while self
                        .toks
                        .next_if(|tok| {
                            tok.as_ref().is_ok_and(|x| {
                                matches!(
                                    x.kind,
                                    TokenKind::Semicolon
                                        | TokenKind::While
                                        | TokenKind::Equal
                                        | TokenKind::For
                                )
                            })
                        })
                        .is_some()
                    {}
                    self.state = State::Parse;
                }
                State::Abort | State::Finished => break None,
            }
        }
    }
}

/// The state of the parser
pub enum State {
    /// Parse
    Parse,
    /// Recover until we see a statement boundary
    Recover,
    /// Return `None` forever
    Abort,
    /// Return `None` forever
    Finished,
}

pub struct Parser<'de> {
    state: State,
    interner: &'de mut Interner,
    source_map: &'de SourceMap,
    toks: Peekable<Lexer<'de>>,
}

impl<'de> Parser<'de> {
    pub fn new(iter: Lexer<'de>, source_map: &'de SourceMap, interner: &'de mut Interner) -> Self {
        Self {
            state: State::Parse,
            toks: iter.peekable(),
            source_map,
            interner,
        }
    }

    pub fn stmt(&mut self) -> Result<Node, Error> {
        if self
            .toks
            .next_if(|tok| tok.as_ref().is_ok_and(|x| x.kind == TokenKind::Let))
            .is_some()
        {
            self.declaration()
        } else {
            self.assignment()
        }
    }

    fn assignment(&mut self) -> Result<Node, Error> {
        let next = self.peek(PEKind::ExpIdentFound)?; // simply cannot be EOF since that's checked
        let ident = match next.kind {
            TokenKind::Ident => self.toks.next().unwrap().unwrap(),
            _ => return Err(self.make_err(PEKind::ExpIdentFound)),
        };

        let ident = Ident {
            name: self.interner.intern(ident.lexeme),
            span: self.source_map.span_from_tok(&ident),
        };

        let _equal = self.eat(TokenKind::Equal, PEKind::ExpEqualFound)?;

        let res = self.expr(0);
        let Ok(expr) = res else {
            self.state = State::Recover;
            return Err(res.unwrap_err());
        };

        Ok(Node::Assignment(ident, Box::new(expr)))
    }

    /// Parse a declaration, given that `let` has already been consumed
    fn declaration(&mut self) -> Result<Node, Error> {
        // ident
        let ident = self.peek(PEKind::ExpIdentFound)?;
        let ident = match ident.kind {
            TokenKind::Ident => self.toks.next().unwrap().unwrap(),
            _ => return Err(self.make_err(PEKind::ExpIdentFound)),
        };

        let ident = Ident {
            name: self.interner.intern(ident.lexeme),
            span: self.source_map.span_from_tok(&ident),
        };

        let s_err = PEKind::ExpSemicolonFound;
        let branch =
            self.peek(|x| PEKind::ExpFound(vec![TokenKind::Semicolon, TokenKind::Equal], x))?;

        match branch.kind {
            TokenKind::Semicolon => {
                self.toks.next();

                Ok(Node::Declaration(ident, None))
            }
            TokenKind::Equal => {
                self.toks.next();

                let res = self.expr(0);
                let Ok(expr) = res else {
                    self.state = State::Recover;
                    return Err(res.unwrap_err());
                };

                let semicolon = self.peek(s_err)?;

                if let TokenKind::Semicolon = semicolon.kind {
                    self.toks.next();
                } else {
                    return Err(self.make_err(s_err));
                }

                Ok(Node::Declaration(ident, Some(Box::new(expr))))
            }
            _ => Err(self.make_err(|tok| {
                PEKind::ExpFound(vec![TokenKind::Semicolon, TokenKind::Equal], tok)
            })),
        }
    }

    /// An implementation of Pratt Parsing to deal with mathematical operations. All calls to this
    /// function from outside of itself must have `min_bp` = 0.
    fn expr(&mut self, min_bp: u8) -> Result<Expr, Error> {
        let next = self.peek(PEKind::ExpExprFound)?;
        let mut lhs = match next.kind {
            TokenKind::Number | TokenKind::Float => {
                let val_tok = self.toks.next().unwrap().unwrap();
                Expr::Atom(Value {
                    kind: val_tok.val(),
                    span: self.source_map.span_from_tok(&val_tok),
                })
            }
            _ => {
                return Err(self.make_err(PEKind::ExpOperandFound));
            }
        };

        while self.toks.peek().is_some() {
            let tok = self.peek(PEKind::Unreachable)?; // lex errors annoying

            let op_kind = match OpKind::try_from(tok.kind) {
                Ok(op) => op,
                Err(_) => break,
            };

            let (l, r) = infix_binding_power(&op_kind);
            if l < min_bp {
                // at this point, we fold towards the left
                break;
            }

            // only want to consume once we know that we're folding so that after
            // folding we can resume on the operator that had a lower BP to the left of
            // it
            let tok_op = self.toks.next().unwrap().unwrap();
            let op = Op {
                kind: op_kind,
                span: self.source_map.span_from_tok(&tok_op),
            };
            let rhs = self.expr(r)?;
            lhs = Expr::BinOp(op, Box::new(lhs), Box::new(rhs))
        }

        Ok(lhs)
    }
}

fn infix_binding_power(op: &OpKind) -> (u8, u8) {
    match op {
        OpKind::Add | OpKind::Sub => (1, 2),
        OpKind::Mult | OpKind::Div => (3, 4),
    }
}
