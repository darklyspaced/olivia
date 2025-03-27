use std::collections::HashMap;

use crate::{interner::Symbol, ty::TypeId};

/// Stores all the structs that correspond to all the environments of the program. This is stored
/// in a spagetti tree structure that essentially functions like a stack except popping doesn't
/// actually pop so that all the symbols are kept in perpituity and not lost when the pointer
/// traverses the stack.
#[derive(Default)]
pub struct Env {
    /// Prelude of all the defined types (mostly for type checking purposes)
    prelude: Scope,
    /// Ptr to the top of the active frame
    frame: usize,
    /// The entire parent pointer tree
    stack: Vec<Scope>,
}

#[derive(Default)]
struct Scope {
    data: HashMap<Symbol, TypeId>,
    parent: usize,
}

impl Scope {
    fn from(parent: usize) -> Self {
        Self {
            data: HashMap::new(),
            parent,
        }
    }
}

impl Env {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn enscope(&mut self) {
        let new_scope = Scope::from(self.frame);
        self.frame = self.stack.len();
        self.stack.push(new_scope);
    }

    pub fn descope(&mut self) {
        self.frame = self.stack[self.frame].parent
    }

    pub fn record(&mut self, symbol: Symbol, ty: TypeId) {
        self.stack[self.frame].data.insert(symbol, ty);
    }

    pub fn rec_prelude(&mut self, symbol: Symbol, ty: TypeId) {
        self.prelude.data.insert(symbol, ty);
    }

    pub fn get(&self, symbol: &Symbol) -> Option<&TypeId> {
        match self.prelude.data.get(symbol) {
            None => self.stack[self.frame].data.get(symbol),
            x => x,
        }
    }
}
