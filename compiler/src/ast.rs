use std::{
    collections::VecDeque,
    fmt::{Debug, Display},
};

use strum::EnumIter;

use crate::{error::span::Span, interner::Symbol, token::TokenKind, ty::Ty, value::Value};

pub trait Pass {
    type XArg<'ast>: Debug;
    type XRet<'ast>: Debug;
    type XVar<'ast>: Debug;
    type XTy<'ast>: Debug;
}

#[derive(Debug)]
pub struct Untyped;
#[derive(Debug)]
pub struct Annotated;
#[derive(Debug)]
pub struct Typed;

/// Initial state of the
impl Pass for Untyped {
    type XArg<'ast> = (Ident, Option<Ident>);
    type XRet<'ast> = Option<Ident>;
    type XVar<'ast> = (Ident, Option<Ident>);
    type XTy<'ast> = Ident;
}

/// All the provided types have been resolved (name resolution) + defined in the type system and
/// they aren't `TyIdent`s but `Ty`s now.
impl Pass for Annotated {
    type XArg<'ast> = (Ident, Option<&'ast Ty>);
    type XRet<'ast> = Option<&'ast Ty>;
    type XVar<'ast> = (Ident, Option<&'ast Ty>);
    type XTy<'ast> = &'ast Ty;
}

impl Pass for Typed {
    type XArg<'ast> = (Ident, &'ast Ty);
    type XRet<'ast> = &'ast Ty;
    type XVar<'ast> = (Ident, &'ast Ty);
    type XTy<'ast> = &'ast Ty;
}

pub type Itself<'ast, P> = Box<InnerAst<'ast, P>>;

/// The spans of higher level things are the sums of the spans of their components
#[derive(Debug)]
pub enum Ast<'ast, P: Pass> {
    /// The name of the function, the parameters to the function, block, and return type
    // TODO: technically all the names should be `BindIdent` but i cba rn
    FunDeclaration {
        name: Ident,
        // params: Vec<(BindIdent, Option<TyIdent>)>,
        params: Vec<P::XArg<'ast>>,
        // ret: Option<TyIdent>,
        ret: P::XRet<'ast>,
        block: Itself<'ast, P>,
    },
    Struct {
        name: Ident,
        fields: Vec<(Ident, P::XTy<'ast>)>,
    },
    ForLoop {
        decl: Itself<'ast, P>,
        predicate: Itself<'ast, P>,
        assignment: Itself<'ast, P>,
        block: Itself<'ast, P>,
    },
    If {
        predicate: Itself<'ast, P>,
        then: Itself<'ast, P>,
        otherwise: Option<Itself<'ast, P>>, // this is `(Block || If)`
    },
    Application {
        name: Ident,
        params: Vec<Itself<'ast, P>>,
    },
    Block(VecDeque<InnerAst<'ast, P>>),
    Declaration(P::XVar<'ast>, Option<Itself<'ast, P>>),
    Assignment(Ident, Itself<'ast, P>),
    BinOp(Op, Itself<'ast, P>, Itself<'ast, P>),
    UnaryOp(Op, Itself<'ast, P>),
    FnInvoc(Ident, Option<Vec<InnerAst<'ast, P>>>),
    Ident(Ident),
    Atom(Value),
}

#[derive(Debug)]
pub struct AstId(pub usize);

#[derive(Debug)]
pub struct InnerAst<'ast, P: Pass> {
    pub inner: Ast<'ast, P>,
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
