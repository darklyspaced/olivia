use crate::{
    ast::{AstId, Ident, Op, Pass, Untyped},
    disjoint_set::DisjointSet,
    env::Env,
    interner::Interner,
    ty::Ty,
    visitor::Visitor,
};

struct NameResolution<'de> {
    env: Env,
    interner: &'de mut Interner,
    disjoint_set: DisjointSet,
}

impl NameResolution<'_> {
    // fn fresh(&mut self) -> (Symbol, TypeId) {
    //     let constr_sym = self
    //         .interner
    //         .intern(&format!("${}", self.disjoint_set.get_len()));
    //     (constr_sym, self.disjoint_set.fresh(constr_sym))
    // }
}

impl<'ast> Visitor<'ast> for NameResolution<'_> {
    type P = Untyped;

    fn visit_fun_declaration(
        &mut self,
        name: &Ident,
        params: &[<Self::P as Pass>::XArg<'ast>],
        ret: &<Self::P as Pass>::XRet<'ast>,
        id: AstId,
    ) {
        todo!();
        // name is registered in env
        params.iter().map(|(name, ty)| match ty {
            Some(t) => {
                // TODO: this absolutely should not be creating new types out of thin air??
                let ty = match self.env.get(&name.sym) {
                    Some(ty) => &self.disjoint_set.find(ty).ty,
                    None => {
                        let ty = self.disjoint_set.fresh(name.sym);
                        let Ty::Var(_, id) = ty else { unreachable!() };
                        self.env.record(name.sym, *id);
                        ty
                    }
                };
            }
            None => {}
        });
        // self.env.record(name.sym, Ty::Fn((), ()))
    }

    fn visit_struct(&mut self, name: &Ident, fields: &[(Ident, <Self::P as Pass>::XTy<'ast>)]) {
        todo!()
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

    fn visit_block(&mut self) {
        todo!()
    }

    fn visit_declaration(&mut self, var: &<Self::P as Pass>::XVar<'ast>) {
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
