use std::fmt::Display;

use crate::error::span::Span;

#[derive(Debug)]
pub struct Literal {
    pub kind: LiteralKind,
    pub span: Span,
}

/// Primitive types
#[derive(Debug)]
pub enum LiteralKind {
    Integer(i128),
    Float(f64),
    Boolean(bool),
}

impl Display for LiteralKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{}", i),
            Self::Float(i) => write!(f, "{}", i),
            Self::Boolean(b) => write!(f, "{}", b),
        }
    }
}

macro_rules! impl_from {
    ($wrapper:path; $inner_type:ty; $($from:ty),+) => {
        $(impl From<$from> for LiteralKind {
            fn from(value: $from) -> Self {
                $wrapper(<$inner_type>::from(value))
            }
        })+
    };
}

impl_from!(LiteralKind::Integer; i128; u8, u16, u32, u64);
