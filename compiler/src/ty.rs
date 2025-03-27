use crate::interner::Symbol;

#[derive(Copy, Clone, PartialEq)]
pub struct TypeId(pub usize);

pub enum Ty {
    Var(TyVar),
    Constr(TyConstr),
}

/// A type constructor with paramaters to fill out
pub struct TyConstr {
    pub name: Symbol, // Figure out a way to intern this so it can be a `Symbol`
    pub id: TypeId,
    pub params: Vec<TyVar>,
}

#[derive(Copy, Clone)]
pub struct TyVar {
    pub name: Symbol,
    pub id: TypeId,
}

#[derive(strum::Display, strum::EnumIter)]
pub enum PTy {
    Int,
    Bool,
    String,
}
