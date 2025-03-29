use strum::IntoEnumIterator;

use crate::{
    ast::{Ast, Expr, OpKind},
    disjoint_set::{DisjointSet, Elem},
    env::Env,
    error::source_map::SourceMap,
    interner::{Interner, Symbol},
    ty::{PTy, Ty, TyConstr, TyVar, TypeId},
};

struct TypedAst<'de> {
    ast: Ast,
    /// The environment for the current scope
    env: Env,
    /// Keeps track of all the types and binds them as types are inferred
    disjoint_set: DisjointSet,
    interner: &'de mut Interner,
    source_map: &'de SourceMap,
}

impl<'de> TypedAst<'de> {
    pub fn new(ast: Ast, interner: &'de mut Interner, source_map: &'de SourceMap) -> Self {
        TypedAst {
            ast,
            env: Env::new(),
            interner,
            source_map,
            disjoint_set: DisjointSet::default(),
        }
    }

    /// Get's the primitive `TyVar`
    fn get_tyvar(&mut self, prim_type: PTy) -> TyVar {
        let sym = self.interner.intern(&prim_type.to_string());
        TyVar(sym)
    }

    /// Initialises a fresh type variable and gives it a generated symbol
    fn fresh(&mut self) -> TypeId {
        let constr_sym = self
            .interner
            .intern(&format!("$#{}", self.disjoint_set.get_len()));
        self.disjoint_set.fresh(constr_sym)
    }

    /// Create all the type constructors that should already exist for
    /// 1. Predefined operators (+, &&, <, etc.)
    /// 2. Primitive types (Int, Bool, etc)
    /// and add them to the prelude.
    fn init_tyconsts(&mut self) {
        for elem in PTy::iter() {
            let sym = self.interner.intern(&elem.to_string());
            let id = self.fresh();
            self.env.rec_prelude(sym, id);
        }

        for elem in OpKind::iter() {
            let sym = self.interner.intern(&elem.to_string());

            let func = match elem {
                OpKind::And | OpKind::Or => {
                    let bool = self.get_tyvar(PTy::Bool);
                    TyConstr {
                        name: sym,
                        params: vec![Ty::Var(bool), Ty::Var(bool), Ty::Var(bool)],
                    }
                }
                OpKind::Greater | OpKind::GreaterEqual | OpKind::Less | OpKind::LessEqual => {
                    let (int, bool) = (self.get_tyvar(PTy::Int), self.get_tyvar(PTy::Bool));
                    TyConstr {
                        name: sym,
                        params: vec![Ty::Var(int), Ty::Var(int), Ty::Var(bool)],
                    }
                }
                OpKind::Add | OpKind::Mult | OpKind::Sub | OpKind::Div => {
                    let int = self.get_tyvar(PTy::Int);
                    TyConstr {
                        name: sym,
                        params: vec![Ty::Var(int), Ty::Var(int), Ty::Var(int)],
                    }
                }
            };

            let id = self.disjoint_set.new_ty(Ty::Constr(func));
            self.env.rec_prelude(sym, id);
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
            Ast::Expr(expr) => match expr {
                Expr::BinOp(op, expr, expr1) => todo!(),
                Expr::UnaryOp(op, expr) => todo!(),
                Expr::FnInvoc(fn_ident, vec) => todo!(),
                Expr::Ident(bind_ident) => todo!(),
                Expr::Atom(value) => todo!(),
            },
            Ast::Application { ident, params } => todo!(),
            Ast::Block(_) => unreachable!(),
            _ => todo!(),
        }
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

    /// Returns the representative of a `Symbol` in the current `Env`
    fn get_representative(&self, sym: Symbol) -> &Elem {
        let itself = self.env.get(&sym).unwrap();
        self.disjoint_set.find(*itself)
    }

    /// Pattern matches on the two possible input types based on the following scenarios and
    /// unifies them accordingly
    fn unify(&self, t1: &Ty, t2: &Ty) {
        match (t1, t2) {
            (Ty::Constr(t1), Ty::Constr(t2))
                if t1.name == t2.name && t1.params.len() == t2.params.len() =>
            {
                // conduct a pointwise unification of all the parameters because they should be of
                // the same type since they are the same function and have the same params
                for (ta, tb) in t1.params.iter().zip(t2.params.iter()) {
                    self.unify(&ta, &tb);
                }
            }

            (Ty::Var(v1), Ty::Var(v2)) if v1 == v2 => return,

            (Ty::Var(v), _) if self.is_bound(v.0) => {
                // v is bound so make a substitution for it and try again
                let representative = self.get_representative(v.0);
                self.unify(&representative.ty, t2);
            }

            (_, Ty::Var(v)) if self.is_bound(v.0) => {
                // the left one is it's own representative / is not bound so try the right one now
                let representative = self.get_representative(v.0);
                self.unify(t1, &representative.ty);
            }

            // the type variables are unbound here
            (Ty::Var(TyVar(sym)), other) | (other, Ty::Var(TyVar(sym))) => {
                if !self.occurs_in(*sym, other) {
                    let id = self.env.get(sym).unwrap();
                    let o_sym = match other {
                        Ty::Var(ty_var) => ty_var.0,
                        Ty::Constr(ty_constr) => ty_constr.name,
                    };

                    self.disjoint_set.union(*id, *self.env.get(&o_sym).unwrap());
                }
            }

            (_, _) => panic!("failed to unify the two types"),
        };
    }

    /// Returns whether the type associated with this `Symbol` is bound (has a representative other
    /// than itself) or not
    fn is_bound(&self, id: Symbol) -> bool {
        let itself = self.env.get(&id).unwrap();
        let representative = self.disjoint_set.find(*itself);
        representative.id != itself.0
    }

    /// Checks is `id` occurs in `ty` to prevent a recursion when binding `id` to `ty`. It achieves
    /// this by recursively checking the type by looking at it's representative if it has one or
    /// it's componenets if it's a constructor
    fn occurs_in(&self, id: Symbol, ty: &Ty) -> bool {
        // let id = self.env.get(&id).unwrap();
        match ty {
            Ty::Var(v) if self.is_bound(v.0) => {
                self.occurs_in(id, &self.get_representative(v.0).ty)
            }
            Ty::Var(v) => *ty == v.0,
            Ty::Constr(constr) => constr.params.iter().any(|ty| self.occurs_in(id, ty)),
        }
    }
}
