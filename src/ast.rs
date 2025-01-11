use std::{fmt::Display, iter::Peekable};

use crate::{
    lexer::{Lexer, TokenKind},
    r#type::Ty,
};

#[derive(Debug)]
pub enum Expr {
    BinOp(TokenKind, Box<Expr>, Box<Expr>),
    UnaryOp(TokenKind, Box<Expr>),
    Atom(Ty),
}

pub struct Parser<'de> {
    toks: Peekable<Lexer<'de>>,
}

impl<'de> Parser<'de> {
    pub fn new(iter: Lexer<'de>) -> Self {
        Self {
            toks: iter.peekable(),
        }
    }

    /// just panic on errors for now
    pub fn parse(&mut self, min_bp: u8) -> Option<Expr> {
        let next = self.toks.next()?.unwrap();
        let mut lhs = match next.kind {
            TokenKind::Number | TokenKind::Float => Expr::Atom(next.val()),
            x => unimplemented!("{x}"),
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
                    let next = self.toks.next()?.unwrap();
                    let rhs = self.parse(r)?;

                    lhs = Expr::BinOp(next.kind, Box::new(lhs), Box::new(rhs))
                }

                None => panic!("expected {{+, -, /, *}} found {}", next.kind),
            };
        }

        Some(lhs)
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
