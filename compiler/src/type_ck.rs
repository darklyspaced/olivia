use std::any::Any;

use crate::{ast::Ast, disjoint_set::DisjointSet, env::Env, ty::TypeId};

struct TypedAst {
    ast: Ast,
    env: Env,
    /// Keeps track of all the types and binds them as types are inferred
    disjoint_set: DisjointSet,
}

impl TypedAst {
    pub fn new(ast: Ast) -> Self {
        TypedAst {
            ast,
            env: Env::new(),
            disjoint_set: DisjointSet::default(),
        }
    }

    /// Type checks and annotates a specific Ast with types, returning any conflicts discovered
    pub fn type_ck(&mut self) {}

    pub fn infer(&mut self, ast: Ast) -> TypeId {
        match ast {
            Ast::Declaration(ident, expr) => match expr {
                Some(expr) => self.infer(Ast::Expr(expr)),
                None => {
                    let ty = self.fresh();
                    self.env.record(ident.0.name, ty);
                    return ty;
                }
            },
            Ast::Expr(expr) => expr.type_id(),
            Ast::Block(_) => unreachable!(),
            _ => todo!(),
        }
    }

    /// Generates a new type variable
    fn fresh(&mut self) -> TypeId {
        self.disjoint_set.fresh()
    }

    /// Gives a concrete fresh type `T` to the usage of a variable, specialising it from it's polymorphic
    /// type (which it implicitly already has) down to something that can be unified. It's
    /// polymorphic type is it's 'assumed' type that all let bindings have (in formal logic). In
    /// this case it would just been the type that the variable has before being assigned a fresh
    /// type (essentially anything because we have gleaned information on it yet).
    ///
    /// This is very much only a declaration with no value assigned to it
    fn var(&mut self) {
        // consume a let binding, giving the ident a type in the environment
    }

    /// Attempts to unify the type (e -> t) with (x).
    fn application() {}

    fn unify() {}
}

pub struct TyVar {
    pub kind: TyVarKind,
    pub id: TypeId,
}

pub enum TyVarKind {
    /// Already canonical meaning that it is it's own representative
    Ty,
    /// Can be substituted for
    Var,
}
