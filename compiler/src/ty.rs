use crate::interner::Symbol;

#[derive(Copy, Clone, PartialEq)]
pub struct TypeId(pub usize);

pub enum Ty {
    Const {
        name: Symbol, // Figure out a way to intern this so it can be a `Symbol`
        id: TypeId,
        params: Vec<Ty>,
    },
    Var {
        name: Symbol,
        id: TypeId,
    },
}

#[derive(strum::Display, strum::EnumIter)]
pub enum PrimTy {
    Int,
    Bool,
    String,
}
