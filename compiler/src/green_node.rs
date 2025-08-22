use std::{
    collections::{HashMap, hash_map::Entry},
    rc::Rc,
};

use crate::token::{Token, TokenKind};

#[derive(Debug, Eq, Hash, Clone)]
pub enum Green<'de> {
    Node(Rc<GreenNode<'de>>),
    Leaf(Rc<Token<'de>>),
}

impl PartialEq for Green<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Node(n1), Self::Node(n2)) => Rc::ptr_eq(n1, n2),
            (Self::Leaf(l1), Self::Leaf(l2)) => Rc::ptr_eq(l1, l2),
            _ => false,
        }
    }
}

impl Green<'_> {
    pub fn width(&self) -> usize {
        match self {
            Self::Node(n) => n.width,
            Self::Leaf(t) => t.lexeme.len(),
        }
    }

    pub fn children(&self) -> std::slice::Iter<'_, Green<'_>> {
        match self {
            Self::Node(n) => n.children(),
            Self::Leaf(_) => [].iter(),
        }
    }
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
pub struct GreenNode<'de> {
    kind: TokenKind,
    pub width: usize,
    children: Vec<Green<'de>>,
}

impl GreenNode<'_> {
    pub fn children(&self) -> std::slice::Iter<'_, Green<'_>> {
        self.children.iter()
    }
}

struct Interner<'de> {
    intern_table: HashMap<Green<'de>, Rc<Green<'de>>>,
}

impl<'de> Interner<'de> {
    pub fn intern(&mut self, target: Green<'de>) -> Rc<Green<'de>> {
        match self.intern_table.entry(target) {
            Entry::Occupied(entry) => Rc::clone(entry.get()),
            Entry::Vacant(entry) => {
                let ptr = Rc::new(entry.key().clone());
                entry.insert(Rc::clone(&ptr));
                ptr
            }
        }
    }
}
