use crate::{
    ast::{AstId, Ident, Op, Pass, Untyped},
    disjoint_set::DisjointSet,
    env::Env,
    interner::Interner,
    ty::{Ty, UntaggedTy},
    visitor::Visitor,
};

/// Defines all the types and variables into the `Env` and `DisjointSet`. However, it does not
/// perform any checking for unresolvable uses of variables / types. This is handled in `NameCk`.
///
/// The general strategy for types is to create a fresh type variable the first time an `Ident` is
/// observed and then use that until either a definition is found (at which point the type variable
/// is bound to that type) or the end of the program.
///
/// The strategy for name is to essentially just define them all in the enviorment normally.
struct ForwardRef<'de> {
    env: Env,
    interner: &'de mut Interner,
    disjoint_set: DisjointSet,
}

impl Visitor for ForwardRef<'_> {
    type P = Untyped;

    fn visit_block(&mut self) {
        self.env.enscope();
    }

    fn block_exited(&mut self) {
        self.env.descope();
    }

    fn visit_fun_declaration(
        &mut self,
        name: &Ident,
        params: &[<Self::P as Pass>::XArg],
        ret: &<Self::P as Pass>::XRet,
        id: AstId,
    ) {
        let ret = match ret {
            Some(ident) => match self.env.get(&ident.sym) {
                Some(ty) => self.disjoint_set.find(ty).ty_id(),
                None => {
                    let ty = self.disjoint_set.fresh(ident.sym);
                    self.env.record(ident.sym, ty);
                    ty
                }
            },
            None => Ty::Unit.ty_id(),
        };

        let params = params
            .iter()
            .map(|(name, ty)| {
                let ty = match self.env.get(&name.sym) {
                    Some(ty) => self.disjoint_set.find(ty).ty_id(),
                    None => {
                        let ty_id = self.disjoint_set.fresh(ty.sym);
                        self.env.record(name.sym, ty_id);
                        self.env.record(ty.sym, ty_id);
                        ty_id
                    }
                };
                self.env.record(name.sym, ty);
                ty
            })
            .collect();

        let fn_ty = self.disjoint_set.new_ty(UntaggedTy::Fn {
            name: name.sym,
            params,
            ret,
        });
        self.env.record(name.sym, fn_ty);
    }

    fn visit_for_loop(&mut self) {
        todo!()
    }

    fn visit_if(&mut self) {
        todo!()
    }

    fn visit_application(&mut self, name: &Ident) {
        todo!()
    }

    fn visit_declaration(&mut self, var: &<Self::P as Pass>::XVar) {
        todo!()
    }

    fn visit_assignment(&mut self, ident: &Ident) {
        todo!()
    }

    fn visit_bin_op(&mut self, op: &Op) {
        todo!()
    }

    fn visit_unary_op(&mut self, op: &Op) {
        todo!()
    }

    fn visit_fn_invoc(&mut self, bind_ident: &Ident) {
        todo!()
    }

    fn visit_ident(&mut self, ident: &Ident) {
        todo!()
    }

    fn visit_atom(&mut self, value: &crate::value::Value) {
        todo!()
    }
}
