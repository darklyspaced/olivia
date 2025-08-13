use std::collections::HashMap;

use crate::{
    interner::Symbol,
    ty::{Ty, TypeId},
};

/// Stores all the structs that correspond to all the environments of the program. This is stored
/// in a spagetti tree structure that essentially functions like a stack except popping doesn't
/// actually pop so that all the symbols are kept in perpituity and not lost when the pointer
/// traverses the stack.
#[derive(Default)]
pub struct Env {
    /// Prelude of all the defined types (mostly for type checking purposes)
    prelude: Scope,
    /// Ptr to the active frame (at the bottom of the tree)
    frame: usize,
    /// The entire parent pointer tree
    stack: Vec<Scope>,
    // /// Where the Iterator is in relation to the stack
    // ptr: usize,
}

#[derive(Default)]
struct Scope {
    data: HashMap<Symbol, Ty>,
    parent: Option<usize>,
}

impl Scope {
    fn from(parent: usize) -> Self {
        Self {
            data: HashMap::new(),
            parent: Some(parent),
        }
    }
}

/// Returns `Scope`s in the order in which they were created. This is useful for walking an AST
/// from top to bottom when doing type inference since then all the items can be processed
/// linearly.
impl IntoIterator for Env {
    type Item = Scope;
    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.stack.into_iter()
    }
}

impl Env {
    pub fn new() -> Self {
        Self {
            prelude: Scope {
                data: HashMap::new(),
                parent: None,
            },
            frame: 0,
            stack: vec![Scope {
                data: HashMap::new(),
                parent: None,
            }],
        }
    }

    /// Enscope should only be called when new scopes are required. There is already a default
    /// scope that essentially functions as "global" for function declarations and such and is
    /// initialised when the program starts running
    pub fn enscope(&mut self) {
        let new_pos = self.stack.len();
        let new_scope = Scope::from(self.frame);
        self.frame = new_pos;
        self.stack.push(new_scope);
    }

    pub fn descope(&mut self) {
        match self.stack[self.frame].parent {
            Some(p) => self.frame = p,
            None => panic!("attempted to descope when at root environment!"),
        }
    }

    pub fn record(&mut self, symbol: Symbol, ty: Ty) {
        self.stack[self.frame].data.insert(symbol, ty);
    }

    pub fn rec_prelude(&mut self, symbol: Symbol, ty: Ty) {
        self.prelude.data.insert(symbol, ty);
    }

    pub fn get(&self, symbol: &Symbol) -> Option<&TypeId> {
        match self.prelude.data.get(symbol) {
            None => {
                let mut curr = &self.stack[self.frame];
                while curr.data.get(symbol).is_none() {
                    match curr.parent {
                        Some(p) => curr = &self.stack[p],
                        None => panic!(
                            "this should be an error saying that symbol is not declared / DNE in scope",
                        ),
                    }
                }
                self.stack[self.frame].data.get(symbol)
            }
            x => x,
        }
    }
}
