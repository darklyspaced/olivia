use crate::{
    ast::{Ast, AstId, BindIdent, Ident, Op, Pass, TyIdent, Untyped, Value},
    env::Env,
    interner::Interner,
    ty::Ty,
    visitor::Visitor,
};

struct NameResolution<'de> {
    env: Env,
    interner: &'de mut Interner,
}

impl Visitor for NameResolution<'_> {
    type P = Untyped;

    fn visit_fun_declaration(
        &mut self,
        name: &Ident,
        params: &[<Self::P as Pass>::XArg],
        ret: &<Self::P as Pass>::XRet,
        id: AstId,
    ) {
        // name is registered in env
        params.iter().map(||);
        self.env.record(name.sym, Ty::Fn((), ()))
    }

    fn visit_struct(&mut self, name: &Ident, fields: &[(Ident, <Self::P as Pass>::XTy)]) {
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
