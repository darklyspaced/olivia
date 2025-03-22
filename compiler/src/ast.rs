use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
};

use crate::{error::span::Span, interner::Symbol, token::TokenKind, value::Value};

#[derive(Debug)]
/// The spans of higher level things are the sums of the spans of their components
pub enum Ast {
    Block(VecDeque<Ast>),
    /// Identifier, then the expression being assigned to it
    Declaration(BindIdent, Option<Expr>),
    /// The name of the function, the parameters to the function, block, and return type
    FunDeclaration {
        ident: Ident,
        params: Vec<(TyIdent, BindIdent)>,
        ret: Option<TyIdent>,
        block: Box<Ast>,
    },
    ForLoop {
        decl: Box<Ast>,
        predicate: Expr,
        assignment: Box<Ast>,
        block: Box<Ast>,
    },
    If {
        predicate: Expr,
        then: Box<Ast>,
        otherwise: Option<Box<Ast>>, // this is `(Block || If)`
    },
    Application {
        ident: Ident,
        params: Vec<Expr>,
    },
    Assignment(Ident, Expr),
    Expr(Expr),
}

#[derive(Debug)]
pub enum Expr {
    BinOp(Op, Box<Expr>, Box<Expr>),
    UnaryOp(Op, Box<Expr>),
    FnInvoc(FnIdent, Option<Vec<Expr>>),
    Ident(BindIdent),
    Atom(Value),
}

impl Iterator for Ast {
    /// Whether it is something that needs to be matched on or not
    type Item = Option<Ast>;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            Ast::Block(vec) => Some(vec.pop_front()),
            _ => Some(None),
        }
    }
}

#[derive(Debug)]
pub struct BindIdent(pub Ident);
#[derive(Debug)]
pub struct FnIdent(pub Ident);
#[derive(Debug)]
pub struct TyIdent(pub Ident);

#[derive(Debug)]
/// An identifier to the function, type, or variable
pub struct Ident {
    pub name: Symbol,
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
    Greater,
    Less,
    GreaterEqual,
    LessEqual,
    Or,
    And,
}

impl TryFrom<TokenKind> for OpKind {
    type Error = ();

    fn try_from(value: TokenKind) -> Result<Self, Self::Error> {
        match value {
            TokenKind::Plus => Ok(OpKind::Add),
            TokenKind::Minus => Ok(OpKind::Sub),
            TokenKind::Star => Ok(OpKind::Mult),
            TokenKind::Slash => Ok(OpKind::Div),
            TokenKind::Greater => Ok(OpKind::Greater),
            TokenKind::Less => Ok(OpKind::Less),
            TokenKind::LessEqual => Ok(OpKind::LessEqual),
            TokenKind::GreaterEqual => Ok(OpKind::GreaterEqual),
            TokenKind::DoubleAmpersand => Ok(OpKind::And),
            TokenKind::DoublePipe => Ok(OpKind::Or),
            _ => Err(()),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::BinOp(token_kind, er, e1) => write!(f, "({} {} {})", token_kind, er, e1),
            Expr::UnaryOp(token_kind, e) => write!(f, "({}{})", token_kind, e),
            Expr::Atom(ty) => write!(f, "{:?}", ty),
            Expr::FnInvoc(ident, x) => write!(f, "{:?}({:?})", ident, x),
            Expr::Ident(ident) => write!(f, "{:?}", ident),
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
            OpKind::Greater => ">",
            OpKind::Less => "<",
            OpKind::GreaterEqual => ">=",
            OpKind::LessEqual => "<=",
            OpKind::Or => "||",
            OpKind::And => "&&",
        })
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}
