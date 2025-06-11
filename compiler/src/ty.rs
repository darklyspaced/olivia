use std::collections::HashMap;

use crate::interner::Symbol;

#[derive(Copy, Clone, PartialEq)]
pub struct TypeId(pub usize);

pub enum Ty {
    Var(TyVar),
    Constr(TyConstr),
}

/// A type constructor with paramaters to fill out where params.last is the output type and the
/// constructor transforms params[..params.length()-2] -> params[params.length()-1]
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

#[derive(Clone)]
pub struct Value(usize);
#[derive(Clone)]
pub struct Use(usize);

pub enum VTyConstr {
    VBool,
    VFunc { arg: Vec<Use>, ret: Value },
    VStruct { fields: HashMap<Symbol, Value> },
}

pub enum UTyConstr {
    UBool,
    UFunc { arg: Vec<Value>, ret: Use },
    UStruct { fields: HashMap<Symbol, Value> },
}

impl PartialEq<Symbol> for Ty {
    fn eq(&self, other: &Symbol) -> bool {
        match self {
            Ty::Var(ty_var) => ty_var.0 == *other,
            Ty::Constr(ty_constr) => ty_constr.name == *other,
        }
    }
}
