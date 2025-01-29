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
        span::Span,
    },
    lexer::{Lexer, Token, TokenKind},
    ty::Value,
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

#[derive(Debug)]
pub struct Ident;

#[derive(Debug)]
/// The spans of higher level things are the sums of the spans of their components
pub enum Node {
    Statement(Ident),
    Declaration(Ident, Box<Expr>),
    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    BinOp(Op, Box<Expr>, Box<Expr>),
    UnaryOp(Op, Box<Expr>),
    Atom(Value),
}

#[derive(Debug)]
pub struct Op {
    kind: OpKind,
    span: Span,
}

#[derive(Debug)]
pub enum OpKind {
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
                State::Abort => return None,
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

    /// Peeks the next token and handles error cases. `err_kind` is for the EOF case
    fn peek(&mut self, err_kind: PEKind) -> Result<&Token, Error> {
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
            None => error!(self, err_kind),
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

    /// Should only be used the case that the next token exists but isn't what it should be.
    ///
    /// This returns a result so that it can easily be returned by the parser function
    fn make_err(&mut self, kind: impl FnOnce(String) -> PEKind) -> Error {
        self.state = State::Recover;

        let erroneous_tok = self
            .toks
            .next()
            .expect("shouldn't be EOF")
            .expect("should have been a valid token, not error");
        let kind = kind(String::from(erroneous_tok.lexeme));
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

    /// Parse a declaration, given that `let` has already been consumed
    fn declaration(&mut self) -> Result<Node, Error> {
        // ident
        let i_err = |x| PEKind::ExpIdentFound(x);
        let ident = self.peek(i_err(String::from("EOF")))?;
        match ident.kind {
            TokenKind::Ident => {
                let _ident = self.toks.next();
            }
            _ => {
                return Err(self.make_err(i_err));
            }
        }

        let s_err = |x| PEKind::ExpSemicolonFound(x);
        let branch = self.peek(s_err(String::from("EOF")))?;
        match branch.kind {
            TokenKind::Semicolon => todo!("bare decl with no value"),
            TokenKind::Equal => {
                self.toks.next();

                let res = self.expr(0);
                let Ok(expr) = res else {
                    self.state = State::Recover;
                    return Err(res.unwrap_err());
                };

                let semicolon = self.peek(s_err(String::from("EOF")))?;

                if let TokenKind::Semicolon = semicolon.kind {
                    self.toks.next();
                } else {
                    return Err(self.make_err(s_err));
                }

                Ok(Node::Declaration(Ident, Box::new(expr)))
            }
            _ => Err(self.make_err(|tok| {
                PEKind::Expected(vec![TokenKind::Semicolon, TokenKind::Equal], tok)
            })),
        }
    }

    /// An implementation of Pratt Parsing to deal with mathematical operations. All calls to this
    /// function from outside of itself must have `min_bp` = 0.
    fn expr(&mut self, min_bp: u8) -> Result<Expr, Error> {
        let next = self.peek(PEKind::ExpExprFound(String::from("EOF")))?;
        let mut lhs = match next.kind {
            TokenKind::Number | TokenKind::Float => Expr::Atom(next.val()),
            _ => {
                return Err(self.make_err(PEKind::ExpOperandFound));
            }
        };
        self.toks.next();

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

impl TryFrom<TokenKind> for OpKind {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Plus => Ok(OpKind::Add),
            TokenKind::Minus => Ok(OpKind::Sub),
            TokenKind::Star => Ok(OpKind::Mult),
            TokenKind::Slash => Ok(OpKind::Div),
            _ => Err(()),
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

impl Display for OpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            OpKind::Add => "+",
            OpKind::Mult => "*",
            OpKind::Div => "/",
            OpKind::Sub => "-",
        })
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}
