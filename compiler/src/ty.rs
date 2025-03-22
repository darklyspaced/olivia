#[derive(Copy, Clone, PartialEq)]
pub struct TypeId(pub usize);

pub enum Ty {
    Int,
    Float,
    Composite(TypeId),
}

pub struct Ty {
    id: TypeId,
}

/// Constructs a type when given parametres
pub struct TyConst {}
