use std::cell::Cell;

use crate::{
    interner::Symbol,
    ty::{Ty, TypeId, UntaggedTy},
};

/// This is a data structure that contains many different disjoint sets that can be merged. This is
/// important because we create a bunch of fresh variables as we make our way through type
/// inference. Using this, we can collate these type variables into sets.
///
/// If two type variables occupy the same set, then they are the same and hence by `find`ing their
/// representative, they can be referred to as the same without having to do a linear traversal and
/// doing the substitution `S` immediately. This just keeps track of all the `S`s as we do one our
/// pass.
///
/// A type's `TyId` is its position in the array.
#[derive(Default)]
pub struct DisjointSet {
    /// The index in `forest` is one's `TyId`
    forest: Vec<Elem>,
    /// Which index in `forest` contains the parent for the `i`th `Elem`
    parents: Vec<usize>,
}

/// A node in the forest of a `DisjointSet`. It's rank approximates how deep the tree is so that
/// when unifying sets we can prevent trees from getting too deep.
///
/// `Cell` is needed here because holding mutable references whilst trying to update paths and
/// access grandparents simply isn't possible.
pub struct Elem {
    pub id: usize,
    pub ty: Ty,
    rank: Cell<usize>,
    parent: Cell<usize>,
}

impl DisjointSet {
    pub fn fresh(&mut self, name: Symbol) -> TypeId {
        let id = self.forest.len();
        let tyid = TypeId(id);
        self.forest.push(Elem {
            rank: Cell::new(0),
            ty: Ty::Var(name, tyid),
            parent: Cell::new(id),
            id,
        });
        tyid
    }

    /// Adds a new type that is known with the given name
    pub fn new_ty(&mut self, ty: UntaggedTy) -> TypeId {
        let id = self.forest.len();
        let ty = ty
            .tag(TypeId(id))
            .expect("called tag on a primitive type :(");
        self.forest.push(Elem {
            rank: Cell::new(0),
            ty,
            parent: Cell::new(id),
            id,
        });
        TypeId(id)
    }

    /// Standard find algorithm that uses path splitting by replacing every pointer on this path to
    /// a pointer to the node's grandparents. This ensures that next time we want to find something
    /// it becomes quicker and quicker!
    ///
    /// This takes a TypeId since we only want type variables to have representatives since other   
    pub fn find(&self, id: TypeId) -> &Elem {
        let mut curr = self.get_node(id.0);
        let parent = self.get_parent(curr);

        while curr.id != self.get_parent(curr).id {
            let grandparent = self.get_parent(parent);
            curr.set_parent(grandparent);

            curr = parent;
        }

        curr
    }

    /// Union algorithm that uses rank to make sure that the trees don't get too deep and
    /// essentially just sets the parent of one tree to the other so that they both end up having
    /// the same set representative.
    pub fn union(&self, x: TypeId, y: TypeId) {
        let mut x = self.find(x);
        let mut y = self.find(y);

        if x != y {
            if x.rank < y.rank {
                (x, y) = (y, x);
            }

            y.set_parent(x);
            if x.rank == y.rank {
                x.rank.set(x.rank.get() + 1);
            }
        }
    }

    /// Return all free fresh type variables in the environment
    pub fn free(&self) -> Vec<&Elem> {
        self.forest
            .iter()
            .enumerate()
            .filter(|(pos, x)| x.parent.get() == *pos)
            .map(|(_, x)| x)
            .collect::<Vec<_>>()
    }

    // pub fn transform(&mut self, id: TypeId, ty: Ty) {
    //     self.forest[id.0].ty = ty;
    // }

    fn get_node(&self, id: usize) -> &Elem {
        &self.forest[id]
    }

    /// Returns `Some` if `elem` has a parent, else `None`
    fn get_parent(&self, elem: &Elem) -> &Elem {
        self.get_node(elem.parent.get())
    }

    pub fn get_len(&self) -> usize {
        self.forest.len()
    }
}

impl PartialEq for Elem {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Elem {
    pub fn set_parent(&self, new_parent: &Elem) {
        self.parent.replace(new_parent.id);
    }
}
