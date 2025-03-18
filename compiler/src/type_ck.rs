use std::collections::HashMap;

use crate::{
    ast::Ast,
    interner::Symbol,
    ty::{Ty, TyId},
    union_set::DisjointSet,
};

struct TypedAst {
    env: HashMap<Symbol, Ty>,
    /// Keeps track of all the types and binds them as types are inferred
    disjoint_set: DisjointSet,
}

impl TypedAst {
    fn fresh(&mut self) -> TyId {
        self.disjoint_set.fresh()
    }

    /// Gives a concrete type `T` to the usage of a variable, specialising it from it's polymorphic
    /// type (which it implicitly already has) down to something that can be unified.
    fn var(&mut self) {
        // consume a let binding, giving the ident a type in the environment
    }

    /// Attempts to unify the type (e -> t) with (x). If this succeeds then it works.
    fn application() {}

    fn unify() {}
}

impl From<Ast> for TypedAst {
    fn from(value: Ast) -> Self {}
}

pub struct TyVar {
    pub kind: TyVarKind,
    pub id: TyId,
}

pub enum TyVarKind {
    /// Already canonical meaning that it is it's own representative
    Ty,
    /// Can be substituted for
    Var,
}
