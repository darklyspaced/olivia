use std::fmt::Display;

/// Primitive types
#[derive(Debug)]
pub enum Ty {
    Integer(i128),
    Float(f64),
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer(i) => write!(f, "{}", i),
            Self::Float(i) => write!(f, "{}", i),
        }
    }
}

macro_rules! impl_from {
    ($wrapper:path; $inner_type:ty; $($from:ty),+) => {
        $(impl From<$from> for Ty {
            fn from(value: $from) -> Self {
                $wrapper(<$inner_type>::from(value))
            }
        })+
    };
}

impl_from!(Ty::Integer; i128; u8, u16, u32, u64);
