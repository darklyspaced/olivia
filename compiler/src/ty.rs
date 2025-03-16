#[derive(Copy, Clone, PartialEq)]
pub struct TyId(pub usize);

pub enum Ty {
    Int,
    Float,
    Composite(TyId),
}
