use std::fmt::{Debug, Display};

use crate::{error::span::Span, interner::Idx, lexer::TokenKind, value::Value};

#[derive(Debug)]
/// The spans of higher level things are the sums of the spans of their components
pub enum Node {
    Statement,
    Declaration(Ident, Option<Box<Expr>>), // spanned + derived
    Expr(Expr),                            // derived
    Assignment(Ident, Box<Expr>),          // spanned + derived
}

#[derive(Debug)]
pub enum Expr {
    BinOp(Op, Box<Expr>, Box<Expr>), // spanned + derived
    UnaryOp(Op, Box<Expr>),          // spanned + derived
    Atom(Value),                     // spanned
}

#[derive(Debug)]
/// Needs to implement string interning
pub struct Ident {
    pub name: Idx,
    pub span: Span,
}

#[derive(Debug)]
pub struct Op {
    pub kind: OpKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum OpKind {
    Add,
    Mult,
    Div,
    Sub,
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
            Expr::Atom(ty) => write!(f, "{:?}", ty), // TODO: fix this for debug
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
