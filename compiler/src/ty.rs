#[derive(Copy, Clone)]
pub struct TyId(pub usize);

pub enum Ty {
    Int,
    Float,
    Composite(TyId),
}
