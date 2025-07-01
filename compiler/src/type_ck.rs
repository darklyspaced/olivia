use crate::{
    ast::{Ast, OpType},
    disjoint_set::{DisjointSet, Elem},
    env::Env,
    error::source_map::SourceMap,
    interner::{Interner, Symbol},
    ty::{PTy, TyVar, TypeId, Value},
    tyck_core::TyCkCore,
    value::LiteralKind,
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

    pub fn type_ck(&mut self, core: &mut TyCkCore, expr: Ast) -> Value {
        match expr {
            Ast::Declaration(ident, ty, expr) => todo!(),
            Ast::If {
                predicate,
                then,
                otherwise,
            } => {
                let pred_ty = self.type_ck(core, *predicate);
                let target_ty = core.bool_use();
                core.flow(pred_ty, target_ty);

                // We only want to perform the following if it's in a place where it being given a
                // type makes sense
                //
                // TODO: redo the syntax tree so that it supports having an if expression as a type
                // or even having a block as a type for example
                let then_ty = self.type_ck(core, *then);

                // TODO: need to this make this contigent on otherwise existing
                // let otherwise_ty = self.type_ck(core, otherwise);
                todo!()
            }
            Ast::Ident(ident) => self
                .env
                .get(&ident.0.name)
                .expect(&format!(
                    "Variable used before definition: {}",
                    self.interner.lookup(ident.0.name)
                ))
                .clone(),
            Ast::Atom(lit) => {
                use LiteralKind::*;
                match lit.kind {
                    Integer(_) => core.int(),
                    Float(_) => core.float(),
                    Boolean(_) => core.bool(),
                }
            }
            Ast::BinOp(op, lhs, rhs) => {
                let lhs_ty = self.type_ck(core, *lhs);
                let rhs_ty = self.type_ck(core, *rhs);

                match op.ty {
                    OpType::IntOp => {
                        let bound = core.int_use(); // takes in two ints
                        core.flow(lhs_ty, bound);
                        core.flow(rhs_ty, bound);
                        core.int() // emits an int
                    }
                    OpType::FloatOp => {
                        let bound = core.float_use();
                        core.flow(lhs_ty, bound);
                        core.flow(rhs_ty, bound);
                        core.float()
                    }
                    OpType::IntCmp => {
                        let bound = core.int_use();
                        core.flow(lhs_ty, bound);
                        core.flow(rhs_ty, bound);
                        core.float()
                    }
                    OpType::FloatCmp => {
                        let bound = core.float_use();
                        core.flow(lhs_ty, bound);
                        core.flow(rhs_ty, bound);
                        core.bool()
                    }
                    OpType::AnyCmp => core.bool(),
                }
            }
            // we need some way to be able to discern whether this has a return or not
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
    fn is_bound(&self, id: Symbol) -> bool {
        let itself = self.env.get(&id).unwrap();
        let representative = self.disjoint_set.find(*itself);
        representative.id != itself.0
    }
}
