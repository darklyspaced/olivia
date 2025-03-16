use std::ops::{Index, IndexMut};

use crate::{
    ty::TyId,
    type_ck::{TyVar, TyVarKind},
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
pub struct DisjointSet {
    /// The index in `forest` is one's `TyId`
    forest: Vec<Elem>,
    /// Which index in `forest` contains the parent for the `i`th `Elem`
    parents: Vec<usize>,
}

/// A node in the forest of a `DisjointSet`. It's rank approximates how deep the tree is so that
/// when unifying sets we can prevent trees from getting too deep.
pub struct Elem {
    rank: usize,
    ty_var: TyVar,
}

impl DisjointSet {
    pub fn fresh(&mut self) -> TyId {
        let id = TyId(self.forest.len());
        self.forest.push(Elem {
            rank: 0,
            ty_var: TyVar {
                kind: TyVarKind::Var,
                id,
            },
        });
        id
    }

    fn get_parent(&self, elem: &Elem) -> usize {
        self.parents[elem.ty_var.id.0]
    }

    pub fn find(&mut self, ty_var: TyVar) -> TyId {
        let mut curr = &mut self[&ty_var];
        while self.get_parent(curr) != curr.ty_var.id.0 {
            curr
        }

        todo!()
    }

    pub fn union(&mut self, x: Elem, y: Elem) {
        let x = self.find(x.ty_var);
        let y = self.find(y.ty_var);

        if x != y {
             if 
        }
    }
}

impl Index<&TyVar> for DisjointSet {
    type Output = Elem;

    fn index(&self, index: &TyVar) -> &Self::Output {
        self.forest.index(index.id.0)
    }
}

impl IndexMut<&TyVar> for DisjointSet {
    fn index_mut(&mut self, index: &TyVar) -> &mut Self::Output {
        self.forest.index_mut(index.id.0)
    }
}

impl Index<&TyId> for DisjointSet {
    type Output = Elem;

    fn index(&self, index: &TyId) -> &Self::Output {
        self.forest.index(index.0)
    }
}

impl IndexMut<&TyId> for DisjointSet {
    fn index_mut(&mut self, index: &TyId) -> &mut Self::Output {
        self.forest.index_mut(index.0)
    }
}
