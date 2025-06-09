use std::iter;

use strum::IntoEnumIterator;

use crate::{
    ast::{self, Ast, Expr, OpKind},
    disjoint_set::{DisjointSet, Elem},
    env::Env,
    error::source_map::SourceMap,
    interner::{Interner, Symbol},
    ty::{PTy, Ty, TyConstr, TyVar, TypeId},
    value::ValueKind,
};

struct TyCk<'de> {
    ast: Ast,
    /// The environment for the current scope
    env: Env,
    /// Keeps track of all the types and binds them as types are inferred
    disjoint_set: DisjointSet,
    interner: &'de mut Interner,
    source_map: &'de SourceMap,
}

impl<'de> TyCk<'de> {
    pub fn new(
        ast: Ast,
        interner: &'de mut Interner,
        source_map: &'de SourceMap,
        disjoint_set: DisjointSet,
        env: Env,
    ) -> Self {
        TyCk {
            ast,
            interner,
            source_map,
            env,
            disjoint_set,
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

    /// Returns the representative of a `Symbol` in the current `Env`
    fn get_representative(&self, sym: Symbol) -> &Elem {
        let itself = self.env.get(&sym).unwrap();
        self.disjoint_set.find(*itself)
    }
}
