use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
};

use strum::EnumIter;

use crate::{error::span::Span, interner::Symbol, token::TokenKind, value::Literal};

#[derive(Debug)]
/// The spans of higher level things are the sums of the spans of their components
pub enum Ast {
    /// The name of the function, the parameters to the function, block, and return type
    // TODO: technically all the names should be `BindIdent` but i cba rn
    FunDeclaration {
        name: Ident,
        params: Vec<(BindIdent, Option<TyIdent>)>,
        ret: Option<TyIdent>,
        block: Box<Ast>,
    },
    Struct {
        name: Ident,
        fields: Vec<(BindIdent, TyIdent)>,
    },
    ForLoop {
        decl: Box<Ast>,
        predicate: Box<Ast>,
        assignment: Box<Ast>,
        block: Box<Ast>,
    },
    If {
        predicate: Box<Ast>,
        then: Box<Ast>,
        otherwise: Option<Box<Ast>>, // this is `(Block || If)`
    },
    Application {
        name: Ident,
        params: Vec<Box<Ast>>,
    },
    Block(VecDeque<Ast>),
    Declaration(BindIdent, Option<TyIdent>, Option<Box<Ast>>),
    Assignment(Ident, Box<Ast>),
    BinOp(Op, Box<Ast>, Box<Ast>),
    UnaryOp(Op, Box<Ast>),
    FnInvoc(BindIdent, Option<Vec<Ast>>),
    Ident(BindIdent),
    Atom(Literal),
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
    pub ty: OpType,
    pub span: Span,
}

#[derive(Debug, strum::Display, EnumIter)]
pub enum OpKind {
    Add,
    Mult,
    Div,
    Sub,
    Greater,
    Less,
    Equal,
    GreaterEqual,
    LessEqual,
    Or,
    And,
}

#[derive(Debug, strum::Display, EnumIter)]
pub enum OpType {
    /// 2 + 1
    IntOp,
    /// 2 +. 1
    FloatOp,
    /// 2 > 1
    IntCmp,
    /// 2 >. 1
    FloatCmp,
    /// Generic == and !=
    AnyCmp,
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
            TokenKind::EqualEqual => Ok(OpKind::Equal),
            TokenKind::LessEqual => Ok(OpKind::LessEqual),
            TokenKind::GreaterEqual => Ok(OpKind::GreaterEqual),
            TokenKind::DoubleAmpersand => Ok(OpKind::And),
            TokenKind::DoublePipe => Ok(OpKind::Or),
            _ => Err(()),
        }
    }
}

impl From<TokenKind> for OpType {
    fn from(value: TokenKind) -> Self {
        use TokenKind::*;
        match value {
            Plus => Self::IntOp,
            PlusDot => Self::FloatOp,
            Minus => Self::IntOp,
            MinusDot => Self::FloatOp,
            Greater => Self::IntCmp,
            GreaterDot => Self::FloatCmp,
            GreaterEqual => Self::IntCmp,
            GreaterEqualDot => Self::FloatCmp,
            Less => Self::IntCmp,
            LessDot => Self::FloatCmp,
            LessEqual => Self::IntCmp,
            LessEqualDot => Self::FloatCmp,
            x => unreachable!(
                "{} should have been filtered out by pratt parsing algo before this via try_from",
                x
            ),
        }
    }
}

// impl Display for Expr {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self {
//             Expr::BinOp(token_kind, er, e1) => write!(f, "({} {} {})", token_kind, er, e1),
//             Expr::UnaryOp(token_kind, e) => write!(f, "({}{})", token_kind, e),
//             Expr::Atom(ty) => write!(f, "{:?}", ty),
//             Expr::FnInvoc(ident, x) => write!(f, "{:?}({:?})", ident, x),
//             Expr::Ident(ident) => write!(f, "{:?}", ident),
//         }
//     }
// }

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}
