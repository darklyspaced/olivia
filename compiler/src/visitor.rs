use crate::{
    ast::{Ast, AstId, Ident, InnerAst, Op, Pass},
    value::Value,
};
/// Provides an implementation of a Visitor pattern so that other things have a standard way of
/// walking the AST. However this
pub trait Visitor {
    type P: Pass;
    fn walk(&mut self, ast: InnerAst<Self::P>) {
        match ast.inner {
            Ast::FunDeclaration {
                name,
                params,
                ret,
                block,
            } => {
                self.visit_fun_declaration(&name, &params, &ret, ast.id);
                self.walk(*block);
            }
            Ast::Struct { name, fields } => {
                self.visit_struct(&name, &fields);
            }
            Ast::ForLoop {
                decl,
                predicate,
                assignment,
                block,
            } => {
                self.visit_for_loop();
                self.walk(*decl);
                self.walk(*predicate);
                self.walk(*assignment);
                self.walk(*block);
            }
            Ast::If {
                predicate,
                then,
                otherwise,
            } => {
                self.visit_if();
                self.walk(*predicate);
                self.walk(*then);
                if let Some(other) = otherwise {
                    self.walk(*other);
                }
            }
            Ast::Application { name, params } => {
                self.visit_application(&name);
                for param in params {
                    self.walk(*param);
                }
            }
            Ast::Block(asts) => {
                self.visit_block();
                for stmt in asts {
                    self.walk(stmt);
                }
            }
            Ast::Declaration(var, ast) => {
                self.visit_declaration(&var);
                if let Some(ast) = ast {
                    self.walk(*ast);
                }
            }
            Ast::Assignment(ident, ast) => {
                self.visit_assignment(&ident);
                self.walk(*ast);
            }
            Ast::BinOp(op, lhs, rhs) => {
                self.visit_bin_op(&op);
                self.walk(*lhs);
                self.walk(*rhs);
            }
            Ast::UnaryOp(op, ast) => {
                self.visit_unary_op(&op);
                self.walk(*ast);
            }
            Ast::FnInvoc(bind_ident, args) => {
                self.visit_fn_invoc(&bind_ident);
                if let Some(args) = args {
                    for arg in args {
                        self.walk(arg);
                    }
                }
            }
            Ast::Ident(bind_ident) => {
                self.visit_ident(&bind_ident);
            }
            Ast::Atom(value) => {
                self.visit_atom(&value);
            }
        }
    }

    fn visit_fun_declaration(
        &mut self,
        name: &Ident,
        params: &[<Self::P as Pass>::XArg],
        ret: &<Self::P as Pass>::XRet,
        id: AstId,
    );
    fn visit_struct(&mut self, name: &Ident, fields: &[(Ident, <Self::P as Pass>::XTy)]);
    fn visit_for_loop(&mut self);
    fn visit_if(&mut self);
    fn visit_application(&mut self, name: &Ident);
    fn visit_block(&mut self);
    fn visit_declaration(&mut self, var: &<Self::P as Pass>::XVar);
    fn visit_assignment(&mut self, ident: &Ident);
    fn visit_bin_op(&mut self, op: &Op);
    fn visit_unary_op(&mut self, op: &Op);
    fn visit_fn_invoc(&mut self, bind_ident: &Ident);
    fn visit_ident(&mut self, ident: &Ident);
    fn visit_atom(&mut self, value: &Value);
}
