/// Built in primitive types
#[derive(Debug)]
pub enum Type {
    Integer(i128),
    Float(f64),
}

macro_rules! impl_from {
    ($wrapper:path; $inner_type:ty; $($from:ty),+) => {
        $(impl From<$from> for Type {
            fn from(value: $from) -> Self {
                $wrapper(<$inner_type>::from(value))
            }
        })+
    };
}

impl_from!(Type::Integer; i128; u8, u16, u32, u64);
