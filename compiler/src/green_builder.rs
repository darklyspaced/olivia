use crate::{
    ast::Ident,
    green_node::{self, Green, GreenTree},
    syntax::SyntaxKind,
    token::Token,
};

/// This essentially builds the green tree to match the structure of `Ast` except without being
/// strongly typed. It does this by providing helper methods and the parser essentially just adds
/// them into the correct places and tags each node with the proper format.
pub struct GreenBuilder<'de> {
    interner: green_node::Interner<'de>,
    stack: Vec<GreenTree<'de>>,
}

// HACK: this entire implementation makes me deeply sad
impl<'de> GreenBuilder<'de> {
    pub fn new() -> Self {
        Self {
            stack: vec![GreenTree::new(SyntaxKind::Source)],
            interner: green_node::Interner::new(),
        }
    }

    fn store(&mut self, green: Green<'de>) {
        let last = self
            .stack
            .last_mut()
            .expect("attempted to push token without node to add token to");
        last.width += green.width();
        last.children.push(green);
    }

    pub fn leaf(&mut self, token: Token<'de>) {
        let green = self.interner.intern_tok(token);
        self.store(green);
    }

    pub fn leaf_name(&mut self, name: Ident) {
        let green = Green::LeafN(name);
        self.store(green);
    }

    pub fn enter_node(&mut self, kind: SyntaxKind) {
        let node = GreenTree::new(kind);
        self.stack.push(node)
    }

    /// To be called everytime the parser is done adding to a node (like a SyntaxKind::Fn) for
    /// example
    ///
    /// We have to calculate width on the way back up and we have to intern here as well
    pub fn exit_node(&mut self) {
        assert!(self.stack.len() > 1);
        let last = self.stack.pop().unwrap();

        let exiting_node_width = last.width;
        let node = self.interner.intern_node(last);

        let last = self.stack.last_mut().expect("root node does not exist");
        last.children.push(node);
        last.width += exiting_node_width
    }

    pub fn end(mut self) -> GreenTree<'de> {
        assert!(self.stack.len() == 1); // should only be the root node
        self.stack.pop().unwrap()
    }
}

impl Default for GreenBuilder<'_> {
    fn default() -> Self {
        GreenBuilder::new()
    }
}
