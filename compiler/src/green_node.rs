use std::{
    collections::{HashMap, hash_map::Entry},
    fmt::Display,
    rc::Rc,
};

use crate::{ast::Ident, syntax::SyntaxKind, token::Token};

#[derive(Debug, Eq, Hash, Clone)]
pub enum Green<'de> {
    Node(Rc<GreenNode<'de>>),
    LeafT(Rc<Token<'de>>),
    LeafN(Ident),
}

impl PartialEq for Green<'_> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Node(n1), Self::Node(n2)) => Rc::ptr_eq(n1, n2),
            (Self::LeafT(l1), Self::LeafT(l2)) => Rc::ptr_eq(l1, l2),
            (Self::LeafN(l1), Self::LeafN(l2)) => l1.sym == l2.sym,
            _ => false,
        }
    }
}

impl Green<'_> {
    pub fn width(&self) -> usize {
        match self {
            Self::Node(n) => n.width,
            Self::LeafT(t) => t.lexeme.len(),
            Self::LeafN(t) => t.width,
        }
    }

    pub fn children(&self) -> std::slice::Iter<'_, Green<'_>> {
        match self {
            Self::Node(n) => n.children(),
            Self::LeafT(_) => [].iter(),
            Self::LeafN(_) => [].iter(),
        }
    }
}

/// Maintains only **relative** information about itself in the source file like it's width. Each
/// `Either<>` is interned via a `Rc<>` so that you don't need a table around to do useful thing
/// with the tree however, obviously, you need an interner to create one in the first place. This
/// also allows trees to share components like functions calls, strings etc. But also you need a
/// `crate::interner::Interner` still because ummm API and i cba. Might change this in the future
/// to rework string interning into the pipeline later but at least it's one interner instead of
/// two and string interning really should be done at parse time to minimise memory pressure.
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
    kind: SyntaxKind,
    pub width: usize,
    pub children: Vec<Green<'de>>,
}

impl GreenNode<'_> {
    pub fn children(&self) -> std::slice::Iter<'_, Green<'_>> {
        self.children.iter()
    }

    pub fn new(kind: SyntaxKind) -> Self {
        GreenNode {
            width: 0,
            children: vec![],
            kind,
        }
    }
}

pub struct Interner<'de> {
    node_table: HashMap<GreenNode<'de>, Rc<GreenNode<'de>>>,
    leaf_table: HashMap<Token<'de>, Rc<Token<'de>>>,
}

impl<'de> Interner<'de> {
    pub fn new() -> Self {
        Self {
            node_table: HashMap::new(),
            leaf_table: HashMap::new(),
        }
    }

    pub fn intern_tok(&mut self, target: Token<'de>) -> Green<'de> {
        match self.leaf_table.entry(target) {
            Entry::Occupied(entry) => Green::LeafT(Rc::clone(entry.get())),
            Entry::Vacant(entry) => {
                let ptr = Rc::new(entry.key().clone());
                entry.insert(Rc::clone(&ptr));
                Green::LeafT(ptr)
            }
        }
    }

    pub fn intern_node(&mut self, target: GreenNode<'de>) -> Green<'de> {
        match self.node_table.entry(target) {
            Entry::Occupied(entry) => Green::Node(Rc::clone(entry.get())),
            Entry::Vacant(entry) => {
                let ptr = Rc::new(entry.key().clone());
                entry.insert(Rc::clone(&ptr));
                Green::Node(ptr)
            }
        }
    }
}

// impl Display for GreenNode<'_> {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         for child in self.children {
//             writeln!(f, "{}", child)?
//         }
//
//         Ok(())
//     }
// }
