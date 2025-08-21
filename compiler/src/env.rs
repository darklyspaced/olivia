use std::collections::HashMap;

use crate::{interner::Symbol, ty::TypeId};

/// A tree that respresents the namespace structure of a project. Each namespace has it's own Env
pub struct Namespace {
    name: Symbol,
    env: Env,
    children: Vec<Namespace>,
}

/// Stores all the structs that correspond to all the environments of the program. This is stored
/// in a spagetti tree structure that essentially functions like a stack except popping doesn't
/// actually pop so that all the symbols are kept in perpituity and not lost when the pointer
/// traverses the stack.
pub struct Env {
    /// Prelude of all the defined types (mostly for type checking purposes)
    prelude: Scope,
    /// Ptr to the active frame (at the bottom of the tree)
    frame: usize,
    /// The entire parent pointer tree
    stack: Vec<Scope>,
}

/// Relates `Symbol`s to `TypeId`s. These `TypeId`s can then be converted into `Ty`s via the
/// `DisjoinSet` that will convert it intos it's canonical form. We wouldn't want to store `Ty`s
/// directly here since their canonical form will change throughout type inference so we want to
/// refer to `DisjoinSet` so we always have the most up to date version.
#[derive(Default)]
pub struct Scope {
    data: HashMap<Symbol, TypeId>,
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

    /// Takes in a `Symbol` and `TypeId` and returns true if the item was recorded otherwise
    /// returns false if the `Symbol` already exists.
    pub fn record(&mut self, symbol: Symbol, ty: TypeId) -> bool {
        let scope = &mut self.stack[self.frame].data;
        match scope.get(&symbol) {
            Some(_) => false,
            None => {
                scope.insert(symbol, ty);
                true
            }
        }
    }

    pub fn rec_prelude(&mut self, symbol: Symbol, ty: TypeId) {
        self.prelude.data.insert(symbol, ty);
    }

    /// Inserts
    pub fn get_or_insert(&mut self, symbol: Symbol, ty: TypeId) -> TypeId {
        match self.get(&symbol) {
            Some(t) => t,
            None => {
                self.record(symbol, ty);
                ty
            }
        }
    }

    pub fn get(&self, symbol: &Symbol) -> Option<TypeId> {
        match self.prelude.data.get(symbol) {
            None => {
                let mut curr = &self.stack[self.frame];
                while !curr.data.contains_key(symbol) {
                    match curr.parent {
                        Some(p) => curr = &self.stack[p],
                        None => return None,
                    }
                }
                self.stack[self.frame].data.get(symbol).copied()
            }
            x => x.copied(),
        }
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}
