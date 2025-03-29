use strum::IntoEnumIterator;

use crate::{
    ast::{Ast, Expr, OpKind},
    disjoint_set::DisjointSet,
    env::Env,
    error::source_map::SourceMap,
    interner::Interner,
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

    /// Pattern matches on the two possible input types based on the following scenarios
    fn unify(&self, t1: &Ty, t2: &Ty) {
        match (t1, t2) {
            (Ty::Constr(t1), Ty::Constr(t2))
                if t1.name == t2.name && t1.params.len() == t2.params.len() =>
            {
                // conduct a pointwise unification of all the parameters because they should be of
                // the same type
                for (ta, tb) in t1.params.iter().zip(t2.params.iter()) {
                    self.unify(&ta, &tb);
                }
            }
            (Ty::Var(v1), Ty::Var(v2)) if v1 == v2 => return,
            (Ty::Var(v), other) | (other, Ty::Var(v)) => {
                let itself = self.env.get(&v.0).unwrap();
                let representative = self.disjoint_set.find(*itself);
                if representative.id != itself.0 {
                    // unify the representative with the other one, making a substiution
                    self.unify(&representative.ty, &other);
                } else {
                }
            }
            (_, _) => panic!("failed to unify the two types"),
        };
    }
}
