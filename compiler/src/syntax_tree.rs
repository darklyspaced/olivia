use std::{
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

use crate::token::{Token, TokenKind};

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
enum Either<N, L> {
    Node(N),
    Leaf(L),
}

/// Maintains only **relative** information about itself in the source file like it's width. Each
/// `Either<>` is interned via a `Rc<>` so that you don't need a table around to do useful thing
/// with the tree however, obviously, you need an interner to create one in the first place. This
/// also allows trees to share components like functions calls, strings etc.
///
/// This interner is created per compilation module and note that the Node is immutable hence the
/// use of `Rc<>`
///
/// These are also homogenous meaning that there is nothing inherently unique about any one of
/// them.
///
/// The derivation on clone for this should be fairly cheap considering Syntax Trees are usually
/// very shallow and sparse and we're mostly just copying pointers anyways.
#[derive(Debug, Eq, Hash, PartialEq, Clone)]
struct GreenNode<'de> {
    kind: TokenKind,
    width: usize,
    children: Vec<Either<Rc<GreenNode<'de>>, Rc<Token<'de>>>>,
}

struct Interner<'de> {
    node_table: HashMap<GreenNode<'de>, Rc<GreenNode<'de>>>,
    leaf_table: HashMap<Token<'de>, Rc<Token<'de>>>,
}

impl<'de> Interner<'de> {
    pub fn intern(
        &mut self,
        target: Either<GreenNode<'de>, Token<'de>>,
    ) -> Either<Rc<GreenNode<'de>>, Rc<Token<'de>>> {
        match target {
            Either::Node(n) => match self.node_table.entry(n) {
                Entry::Occupied(entry) => Either::Node(Rc::clone(entry.get())),
                Entry::Vacant(entry) => {
                    let ptr = Rc::new(entry.key().clone());
                    entry.insert(Rc::clone(&ptr));
                    Either::Node(ptr)
                }
            },
            Either::Leaf(l) => match self.leaf_table.entry(l) {
                Entry::Occupied(entry) => Either::Leaf(Rc::clone(entry.get())),
                Entry::Vacant(entry) => {
                    let ptr = Rc::new(entry.key().clone());
                    entry.insert(Rc::clone(&ptr));
                    Either::Leaf(ptr)
                }
            },
        }
    }
}
