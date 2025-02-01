use std::fmt::Display;

use crate::error::span::Span;

#[derive(Debug)]
pub struct Value {
    pub kind: ValueKind,
    pub span: Span,
}

/// Primitive types
#[derive(Debug)]
pub enum ValueKind {
    Integer(i128),
    Float(f64),
}

impl Display for ValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{}", i),
            Self::Float(i) => write!(f, "{}", i),
        }
    }
}

macro_rules! impl_from {
    ($wrapper:path; $inner_type:ty; $($from:ty),+) => {
        $(impl From<$from> for ValueKind {
            fn from(value: $from) -> Self {
                $wrapper(<$inner_type>::from(value))
            }
        })+
    };
}

impl_from!(ValueKind::Integer; i128; u8, u16, u32, u64);
