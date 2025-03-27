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

    fn get_tyvar(&mut self, prim_type: PTy) -> TyVar {
        let sym = self.interner.intern(&prim_type.to_string());
        let ty_id = self.env.get(&sym).unwrap();
        TyVar {
            name: sym,
            id: *ty_id,
        }
    }

    /// Create all the type constructors that should already exist for
    /// 1. Predefined operators (+, &&, <, etc.)
    /// 2. Primitive types (Int, Bool, etc)
    /// and add them to the prelude.
    fn init_tyconsts(&mut self) {
        // TODO: finish this function and make it so that they can be accessed from anywhere. This
        // may entail adding a 'prelude/global' environment that defines all these types. Probably
        // prelude since it isn't really an environment and we want types to be available file
        // wide
        for elem in PTy::iter() {
            let sym = self.interner.intern(&elem.to_string());
            let id = self.disjoint_set.fresh();
            self.env.rec_prelude(sym, id);
        }

        // TODO: use the `get_len()` method to generate sequential identifiers (symbols) using the
        // pattern `$#{len}` for each of the identifiers
        for elem in OpKind::iter() {
            let sym = self.interner.intern(&elem.to_string());

            let func = match elem {
                OpKind::And | OpKind::Or => {
                    let bool = self.get_tyvar(PTy::Bool);
                    TyConstr {
                        name: sym,
                        id: self.disjoint_set.fresh(),
                        params: vec![bool, bool, bool],
                    }
                }
                OpKind::Greater | OpKind::GreaterEqual | OpKind::Less | OpKind::LessEqual => {
                    let (int, bool) = (self.get_tyvar(PTy::Int), self.get_tyvar(PTy::Bool));
                    TyConstr {
                        name: sym,
                        id: self.disjoint_set.fresh(),
                        params: vec![int, int, bool],
                    }
                }
                OpKind::Add | OpKind::Mult | OpKind::Sub | OpKind::Div => {
                    let int = self.get_tyvar(PTy::Int);
                    TyConstr {
                        name: sym,
                        id: self.disjoint_set.fresh(),
                        params: vec![int, int, int],
                    }
                }
            };

            self.env.rec_prelude(sym, Ty::Constr(func));
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
