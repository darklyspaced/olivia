use std::collections::HashMap;

use crate::{interner::Symbol, ty::TypeId};

/// Stores all the structs that correspond to all the environments of the program. This is stored
/// in a spagetti tree structure that essentially functions like a stack except popping doesn't
/// actually pop so that all the symbols are kept in perpituity and not lost when the pointer
/// traverses the stack.
#[derive(Default)]
pub struct Env {
    /// Ptr to the top of the active frame
    frame: usize,
    /// The entire parent pointer tree
    stack: Vec<Scope>,
}

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
        let node = Scope {
            data: HashMap::new(),
            parent: 0,
        };
        Self {
            frame: 0,
            stack: vec![node],
        }
    }

    pub fn enscope(&mut self) {
        let new_scope = Scope::from(self.frame);
        self.frame = self.stack.len();
        self.stack.push(new_scope);
    }

    pub fn descope(&mut self) {
        self.frame = self.stack[self.frame].parent
    }
}
