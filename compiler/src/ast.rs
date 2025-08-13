use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
};

use strum::EnumIter;

use crate::{error::span::Span, interner::Symbol, token::TokenKind, ty::Ty, value::Value};

pub trait Pass {
    type XArg;
    type XRet;
    type XVar;
    type XTy;
}

#[derive(Debug)]
pub struct Untyped;
#[derive(Debug)]
pub struct Annotated;
#[derive(Debug)]
pub struct Typed;

/// Initial state of the
impl Pass for Untyped {
    type XArg = (Ident, Option<Ident>);
    type XRet = Option<Ident>;
    type XVar = (Ident, Option<Ident>);
    type XTy = Ident;
}

/// All the provided types have been resolved (name resolution) + defined in the type system and
/// they aren't `TyIdent`s but `Ty`s now.
impl Pass for Annotated {
    type XArg = (Ident, Option<Ty>);
    type XRet = Option<Ty>;
    type XVar = (Ident, Option<Ty>);
    type XTy = Ty;
}

impl Pass for Typed {
    type XArg = (Ident, Ty);
    type XRet = Ty;
    type XVar = (Ident, Ty);
    type XTy = Ty;
}

/// The spans of higher level things are the sums of the spans of their components
pub enum Ast<P: Pass> {
    /// The name of the function, the parameters to the function, block, and return type
    // TODO: technically all the names should be `BindIdent` but i cba rn
    FunDeclaration {
        name: Ident,
        // params: Vec<(BindIdent, Option<TyIdent>)>,
        params: Vec<P::XArg>,
        // ret: Option<TyIdent>,
        ret: P::XRet,
        block: Box<InnerAst<P>>,
    },
    Struct {
        name: Ident,
        fields: Vec<(Ident, P::XTy)>,
    },
    ForLoop {
        decl: Box<InnerAst<P>>,
        predicate: Box<InnerAst<P>>,
        assignment: Box<InnerAst<P>>,
        block: Box<InnerAst<P>>,
    },
    If {
        predicate: Box<InnerAst<P>>,
        then: Box<InnerAst<P>>,
        otherwise: Option<Box<InnerAst<P>>>, // this is `(Block || If)`
    },
    Application {
        name: Ident,
        params: Vec<Box<InnerAst<P>>>,
    },
    Block(VecDeque<InnerAst<P>>),
    Declaration(P::XVar, Option<Box<InnerAst<P>>>),
    Assignment(Ident, Box<InnerAst<P>>),
    BinOp(Op, Box<InnerAst<P>>, Box<InnerAst<P>>),
    UnaryOp(Op, Box<InnerAst<P>>),
    FnInvoc(Ident, Option<Vec<InnerAst<P>>>),
    Ident(Ident),
    Atom(Value),
}

pub struct AstId(pub usize);

pub struct InnerAst<P: Pass> {
    pub inner: Ast<P>,
    pub id: AstId,
}

// impl<P: Pass> Iterator for Ast<P> {
//     /// Whether it is something that needs to be matched on or not
//     type Item = Option<Ast<P>>;
//
//     fn next(&mut self) -> Option<Self::Item> {
//         match self {
//             Ast::Block(vec) => Some(vec.pop_front()),
//             _ => Some(None),
//         }
//     }
// }

#[derive(Debug)]
/// An identifier to the function, type, or variable
pub struct Ident {
    pub sym: Symbol,
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
