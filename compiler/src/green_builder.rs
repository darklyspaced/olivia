use crate::{
    ast::Ident,
    green_node::{self, Green, GreenNode},
    syntax::SyntaxKind,
    token::Token,
};

/// This essentially builds the green tree to match the structure of `Ast` except without being
/// strongly typed. It does this by providing helper methods and the parser essentially just adds
/// them into the correct places and tags each node with the proper format.
pub struct GreenBuilder<'de> {
    interner: green_node::Interner<'de>,
    mode: StoreMode,
    cache: Vec<GreenNode<'de>>,
    stack: Vec<GreenNode<'de>>,
    depth: usize,
}

pub enum StoreMode {
    Direct,
    Cache,
}

impl<'de> GreenBuilder<'de> {
    pub fn new() -> Self {
        Self {
            stack: vec![],
            mode: StoreMode::Direct,
            cache: vec![GreenNode::new(SyntaxKind::Error)],
            interner: green_node::Interner::new(),
            depth: 0,
        }
    }

    pub fn error(&mut self) {
        while self.depth > 0 {
            self.exit_node();
        }

        self.enter_node(SyntaxKind::Error);
    }

    pub fn enter_func(&mut self) {
        self.depth = 0
    }

    pub fn set_mode(&mut self, mode: StoreMode) {
        self.mode = mode;
    }

    fn store(&mut self, green: Green<'de>) {
        use StoreMode::*;
        let target = match self.mode {
            Direct => &mut self.stack,
            Cache => &mut self.cache,
        };

        let last = target
            .last_mut()
            .expect("attempted to push token without node to add token to");
        last.width += green.width();
        last.children.push(green);
    }

    pub fn leaf(&mut self, token: Token<'de>) {
        let green = self.interner.intern_tok(token);
        self.store(green);
    }

    // HACK: there must be a better way than having a separate method for `Ident`s and `Token`s
    pub fn leaf_name(&mut self, name: Ident) {
        let green = Green::LeafN(name);
        self.store(green);
    }

    pub fn extend(&mut self, ext: Vec<Green<'de>>) {
        use StoreMode::*;
        let target = match self.mode {
            Direct => &mut self.stack,
            Cache => &mut self.cache,
        };

        target
            .last_mut()
            .expect("cache is empty")
            .children
            .extend(ext);
    }

    /// Flush the cache and return it to the user so they can store it
    pub fn flush(&mut self) -> Vec<Green<'de>> {
        std::mem::take(
            &mut self
                .cache
                .first_mut()
                .expect("root of cache doesn't exist")
                .children,
        )
    }

    /// To be called everytime the parser wants to create a new node
    pub fn enter_node(&mut self, kind: SyntaxKind) {
        self.depth += 1;
        use StoreMode::*;
        match self.mode {
            Direct => {
                let node = GreenNode::new(kind);
                self.stack.push(node)
            }
            Cache => {
                let node = GreenNode::new(kind);
                self.cache.push(node);
            }
        }
    }

    /// To be called everytime the parser is done adding to a node (like a SyntaxKind::Fn) for
    /// example
    ///
    /// We have to calculate width on the way back up and we have to intern here as well
    pub fn exit_node(&mut self) {
        assert!(self.stack.len() > 1);
        self.depth -= 1;
        let last = self.stack.pop().unwrap();

        let exiting_node_width = last.width;
        let node = self.interner.intern_node(last);

        let last = self.stack.last_mut().expect("root node does not exist");
        last.children.push(node);
        last.width += exiting_node_width
    }

    pub fn end(mut self) -> GreenNode<'de> {
        assert!(self.stack.len() == 1); // should only be the root node
        self.stack.pop().unwrap()
    }
}
