use std::iter;

use strum::IntoEnumIterator;

use crate::{
    ast::{Ast, Expr, OpKind},
    disjoint_set::{DisjointSet, Elem},
    env::Env,
    error::source_map::SourceMap,
    interner::{Interner, Symbol},
    ty::{PTy, Ty, TyConstr, TyVar, TypeId},
    value::ValueKind,
};

struct TypedAst<'de> {
    ast: Ast,
    /// The environment for the current scope
    env: Env,
    /// Keeps track of all the types and binds them as types are inferred
    disjoint_set: DisjointSet,
    interner: &'de mut Interner,
    source_map: &'de SourceMap,
    constraints: Vec<Constraint>,
}

enum Constraint {
    Eq(Ty, Ty),
}

impl<'de> TypedAst<'de> {
    pub fn new(
        ast: Ast,
        interner: &'de mut Interner,
        source_map: &'de SourceMap,
        disjoint_set: DisjointSet,
        env: Env,
    ) -> Self {
        TypedAst {
            ast,
            interner,
            source_map,
            env,
            disjoint_set,
            constraints: Vec::new(),
        }
    }

    /// Get's the primitive `TyVar`
    fn get_tyvar(&mut self, prim_type: PTy) -> TyVar {
        let sym = self.interner.intern(&prim_type.to_string());
        TyVar(sym)
    }

    /// Initialises a fresh type variable and gives it a generated symbol
    fn fresh(&mut self) -> (Symbol, TypeId) {
        let constr_sym = self
            .interner
            .intern(&format!("0x{}", self.disjoint_set.get_len()));
        (constr_sym, self.disjoint_set.fresh(constr_sym))
    }

    /// Create all the type constructors that should already exist for
    /// 1. Predefined operators (+, &&, <, etc.)
    /// 2. Primitive types (Int, Bool, etc)
    /// and add them to the prelude.
    fn init_tyconsts(&mut self) {
        for elem in PTy::iter() {
            let sym = self.interner.intern(&elem.to_string());
            let (_, id) = self.fresh();
            self.env.rec_prelude(sym, id);
        }

        for elem in OpKind::iter() {
            let sym = self.interner.intern(&elem.to_string());

            let func = match elem {
                OpKind::And | OpKind::Or => {
                    let bool = self.get_tyvar(PTy::Bool);
                    vec![TyConstr {
                        name: sym,
                        params: vec![Ty::Var(bool), Ty::Var(bool), Ty::Var(bool)],
                    }]
                }
                OpKind::Greater | OpKind::GreaterEqual | OpKind::Less | OpKind::LessEqual => {
                    let (int, bool) = (self.get_tyvar(PTy::Int), self.get_tyvar(PTy::Bool));
                    vec![TyConstr {
                        name: sym,
                        params: vec![Ty::Var(int), Ty::Var(int), Ty::Var(bool)],
                    }]
                }
                OpKind::Add | OpKind::Mult | OpKind::Sub | OpKind::Div => {
                    let int = self.get_tyvar(PTy::Int);
                    vec![TyConstr {
                        name: sym,
                        params: vec![Ty::Var(int), Ty::Var(int), Ty::Var(int)],
                    }]
                }
                OpKind::Equal => {
                    let mut constrs = vec![];
                    for pty in PTy::iter() {
                        let ty = self.get_tyvar(PTy::Int);
                        let sym = self.interner.intern(&format!(
                            "{}{}",
                            &elem.to_string(),
                            &pty.to_string(),
                        ));

                        let constr = TyConstr {
                            name: sym,
                            params: vec![Ty::Var(ty), Ty::Var(ty), Ty::Var(ty)],
                        };
                        constrs.push(constr);
                    }
                    constrs
                }
            };

            for x in func {
                let id = self.disjoint_set.new_ty(Ty::Constr(x));
                self.env.rec_prelude(sym, id);
            }
        }
    }

    /// Type checks and annotates a specific Ast with types, returning any conflicts discovered. It
    /// takes in an expected type to improve errors message and so forth. This ends up being a
    /// variation of constraint solving based Hindley-Milner that is bidirectional
    pub fn type_ck(&mut self) {}

    pub fn check() {}

    pub fn infer(&mut self, ast: Ast, expected: Ty) -> TypeId {
        match ast {
            Ast::Declaration(ident, ty, expr) => match expr {
                Some(expr) => {
                    let ty = match ty {
                        Some(t) => todo!(),
                        None => self.infer(Ast::Expr(expr), expected),
                    };
                    self.env.record(ident.0.name, ty);
                    ty
                }
                None => {
                    let (_, ty) = self.fresh();
                    self.env.record(ident.0.name, ty);
                    ty
                }
            },
            Ast::Expr(expr) => match expr {
                Expr::BinOp(op, expr, expr1) => {
                    // let sym = self.interner.intern(&op.to_string());
                    // let app_ty = self.env.get(&sym);
                    // let (t1, t2) = (self.infer(Ast::Expr(*expr)), self.infer(Ast::Expr(*expr1)));
                    // let ret = self.fresh();
                    todo!()
                }
                Expr::Ident(var) => {
                    let ty = self.env.get(&var.0.name).expect(&format!(
                        "attempted to access {:?} which doesn't exist",
                        var.0.name
                    ));
                    self.constraints
                        .push(Constraint::Eq(Ty::Var(TyVar(var.0.name)), expected));
                    todo!()
                }
                Expr::FnInvoc(ident, params) => {
                    // fresh ty variables with return type as expected var
                    let params_tys = params
                        .unwrap_or(vec![])
                        .iter()
                        .map(|_| Ty::Var(TyVar(self.fresh().0)))
                        .chain(iter::once(expected))
                        .collect::<Vec<Ty>>();

                    let fn_ty = Ty::Constr(TyConstr {
                        name: ident.0.name,
                        params: params_tys,
                    });

                    // saying here that we want the type of this blank function to be the one
                    // associated with the `ident` (hence to match the declaration). Essentially,
                    // we are binding all fresh type variables to what they should be, based on the
                    // declaration

                    // TODO: this should be replaced with a check as we are checking that it
                    // matches this type
                    let inferred_fn = self.infer(Ast::Expr(Expr::Ident(ident)), fn_ty);

                    // TODO: need to construct the environment during parse time so that all the
                    // function types can be constructed then this environment needs to be passed
                    // to the type checker which

                    todo!()
                }
                Expr::Atom(val) => match val.kind {
                    ValueKind::Integer(i) => {
                        self.constraints.push(Constraint::Eq((), ()));
                    }
                    ValueKind::Float(f) => {}
                },
                _ => todo!(),
            },
            Ast::Block(_) => unreachable!(),
            _ => todo!(),
        }
    }

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
                    self.unify(ta, tb);
                }
            }

            (Ty::Var(v1), Ty::Var(v2)) if v1 == v2 => (),

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
    fn occurs_in(&self, _id: Symbol, ty: &Ty) -> bool {
        // let id = self.env.get(&id).unwrap();
        match ty {
            Ty::Var(v) if self.is_bound(v.0) => {
                self.occurs_in(_id, &self.get_representative(v.0).ty)
            }
            Ty::Var(v) => *ty == v.0,
            Ty::Constr(constr) => constr.params.iter().any(|ty| self.occurs_in(_id, ty)),
        }
    }
}
