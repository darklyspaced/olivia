use crate::lexer::{Lexer, TokenKind};

pub enum Expr {
    BinOp(TokenKind, Box<Expr>, Box<Expr>),
    UnaryOp(TokenKind, Box<Expr>),
    Atom(Type),
}

/// Built in primitive types
pub enum Type {
    Integer(i128),
    Float,
}

pub struct Parser<'de> {
    toks: Lexer<'de>,
}

impl Parser<'_> {
    /// just panic on errors for now
    fn parse(&mut self, min_bp: u8) -> Option<Expr> {
        let mut next = self.toks.next()?.unwrap();
        let mut lhs = match next.kind {
            TokenKind::Number | TokenKind::Float => Expr::Atom(Type::Integer(0)),
            _ => unimplemented!(),
        };

        loop {
            next = self.toks.next()?.unwrap();
            match next.kind {
                TokenKind::Plus => match infix_binding_power(&next.kind) {
                    Some((l, r)) => {
                        if l < min_bp {
                            break;
                        }

                        let rhs = self.parse(r)?;

                        lhs = Expr::BinOp(next.kind, Box::new(lhs), Box::new(rhs))
                    }

                    None => panic!("expected {{+, -, /, *}} found {}", next.kind),
                },
                _ => unimplemented!(),
            }
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
