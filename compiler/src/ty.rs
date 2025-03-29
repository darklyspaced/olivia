use crate::interner::Symbol;

#[derive(Copy, Clone, PartialEq)]
pub struct TypeId(pub usize);

pub enum Ty {
    Var(TyVar),
    Constr(TyConstr),
}

/// A type constructor with paramaters to fill out
pub struct TyConstr {
    pub name: Symbol,
    pub params: Vec<Ty>,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct TyVar(pub Symbol);

#[derive(strum::Display, strum::EnumIter)]
pub enum PTy {
    Int,
    Bool,
    String,
}
