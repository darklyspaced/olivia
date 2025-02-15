pub mod utils;

use std::iter::Peekable;

use crate::{
    ast::{BindIdent, Expr, FnIdent, Node, Op, OpKind, TyIdent},
    error::{
        self,
        parse_err::{ParseError, ParseErrorKind as PEKind},
        source_map::SourceMap,
    },
    interner::Interner,
    lexer::Lexer,
    token::TokenKind,
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

    fn fn_decl(&mut self) -> Result<Node, Error> {
        let _fn = self.toks.next();
        let ident = self.ident()?;
        let _l_paren = self.eat(TokenKind::LeftParen, PEKind::ExpLParenFound)?;

        let mut params = vec![];

        let mut next =
            self.peek(|x| PEKind::ExpFound(vec![TokenKind::Ident, TokenKind::RightParen], x))?;
        if next.kind != TokenKind::RightParen {
            loop {
                let ty = TyIdent(self.ident()?);
                let binding = BindIdent(self.ident()?);

                params.push((ty, binding));

                let err = |x| PEKind::ExpFound(vec![TokenKind::Comma, TokenKind::RightParen], x);
                next = self.peek(err)?;
                match next.kind {
                    TokenKind::RightParen => {
                        let _r_paren = self.toks.next();
                        break;
                    }
                    TokenKind::Comma => {
                        let _comma = self.toks.next();
                        continue;
                    }
                    _ => return Err(self.make_err(err)),
                };
            }
        }

        let err = |x| PEKind::ExpFound(vec![TokenKind::LeftBrace, TokenKind::Arrow], x);
        let branch = self.peek(err)?;
        let ret = match branch.kind {
            TokenKind::Arrow => Some(TyIdent(self.ident()?)),
            TokenKind::LeftBrace => None,
            _ => return Err(self.make_err(err)),
        };

        let block = self.block()?;

        Ok(Node::FunDeclaration {
            ident,
            params,
            ret,
            block: Box::new(block),
        })
    }

    fn block(&mut self) -> Result<Node, Error> {
        let mut stmts = vec![];
        let _l_brace = self.eat(TokenKind::LeftBrace, PEKind::ExpLParenFound)?;

        let mut next = self.peek(PEKind::ExpLBraceFound)?;
        if next.kind != TokenKind::RightBrace {
            loop {
                stmts.push(self.stmt()?);
                next = self.peek(PEKind::ExpRBraceFound)?;
                if next.kind == TokenKind::RightBrace {
                    let _r_paren = self.toks.next();
                    break;
                }
            }
        }

        Ok(Node::Block(stmts))
    }

    /// func | var | assignment
    fn stmt(&mut self) -> Result<Node, Error> {
        match self
            .peek(|x| PEKind::ExpFound(vec![TokenKind::Fn, TokenKind::Let], x))?
            .kind
        {
            TokenKind::Let => self.declaration(),
            TokenKind::Fn => self.fn_decl(),
            _ => self.assignment(),
        }
    }

    /// Parse an assignment of a value to an ident
    fn assignment(&mut self) -> Result<Node, Error> {
        let ident = self.ident()?;
        let _equal = self.eat(TokenKind::Equal, PEKind::ExpEqualFound)?;
        let expr = self.expression()?;
        let _semicolon = self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;

        Ok(Node::Assignment(ident, expr))
    }

    /// Parse a declaration
    fn declaration(&mut self) -> Result<Node, Error> {
        let _let = self.toks.next();
        let ident = BindIdent(self.ident()?);

        let branch =
            self.peek(|x| PEKind::ExpFound(vec![TokenKind::Semicolon, TokenKind::Equal], x))?;

        match branch.kind {
            TokenKind::Semicolon => {
                self.toks.next();

                Ok(Node::Declaration(ident, None))
            }
            TokenKind::Equal => {
                self.toks.next();

                let expr = self.expression()?;

                let _semicolon = self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;

                Ok(Node::Declaration(ident, Some(expr)))
            }
            _ => Err(self.make_err(|tok| {
                PEKind::ExpFound(vec![TokenKind::Semicolon, TokenKind::Equal], tok)
            })),
        }
    }

    /// Extract an expression, handling any errors that were raised
    fn expression(&mut self) -> Result<Expr, Error> {
        let res = self.expr(0);
        let Ok(expr) = res else {
            self.state = State::Recover;
            return Err(res.unwrap_err());
        };
        Ok(expr)
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
            TokenKind::Ident => {
                let ident = FnIdent(self.ident()?);
                let mut params = vec![];

                let next = self.peek(PEKind::ExpSemicolonFound)?;
                match next.kind {
                    TokenKind::LeftParen => {
                        let mut next = self.peek(PEKind::ExpExprFound)?;
                        if next.kind != TokenKind::RightParen {
                            loop {
                                params.push(self.expression()?);

                                let err = |x| {
                                    PEKind::ExpFound(
                                        vec![TokenKind::Comma, TokenKind::RightParen],
                                        x,
                                    )
                                };
                                next = self.peek(err)?;
                                match next.kind {
                                    TokenKind::RightParen => {
                                        let _r_paren = self.toks.next();
                                        break;
                                    }
                                    TokenKind::Comma => {
                                        let _comma = self.toks.next();
                                        continue;
                                    }
                                    _ => return Err(self.make_err(err)),
                                };
                            }
                        }

                        Expr::FnInvoc(
                            ident,
                            if params.is_empty() {
                                None
                            } else {
                                Some(params)
                            },
                        )
                    }
                    _ => {
                        let ident = BindIdent(self.ident()?);
                        Expr::Ident(ident)
                    }
                }
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
