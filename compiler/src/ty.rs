#[derive(Copy, Clone, PartialEq)]
pub struct TypeId(pub usize);

pub enum Ty {
    Int,
    Float,
    Composite(TypeId),
}
