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

impl PartialEq<Symbol> for Ty {
    fn eq(&self, other: &Symbol) -> bool {
        match self {
            Ty::Var(ty_var) => ty_var.0 == *other,
            Ty::Constr(ty_constr) => ty_constr.name == *other,
        }
    }
}
