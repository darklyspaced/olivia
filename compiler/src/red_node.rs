use std::rc::Rc;

use crate::green_node::{Green, GreenNode};

#[derive(Clone, Debug)]
/// Essentially a zipper over a purely function tree (the green tree)
struct RedNode<'de>(Rc<RedData<'de>>);

#[derive(Debug)]
struct RedData<'de> {
    offset: usize,
    parent: Option<RedNode<'de>>,
    green: Green<'de>,
}

impl<'de> RedNode<'de> {
    fn new_root(root: GreenNode<'de>) -> Self {
        Self(Rc::new(RedData {
            parent: None,
            offset: 0,
            green: Green::Node(Rc::new(root)),
        }))
    }

    fn parent(&self) -> Option<Self> {
        self.0.parent.clone()
    }

    /// The red tree is essentially only build as we traverse so the general strategy here is to
    /// compute the absolute offsets of the children using the widths of the green nodes as a basis
    /// assuming that the root is 0 offset obviously.
    fn children(&'de self) -> impl Iterator<Item = RedNode<'de>> {
        let mut offset = self.0.offset;
        let mut computed_children = vec![];

        for child in self.0.green.children() {
            let child_offset = offset;
            offset += child.width();
            let ptr = Rc::new(RedData {
                offset: child_offset,
                parent: Some(Self(Rc::clone(&self.0))),
                green: child.clone(),
            });
            computed_children.push(Self(Rc::clone(&ptr)));
        }

        computed_children.into_iter()
    }
}

impl PartialEq for RedNode<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.0.offset == other.0.offset && self.0.green == other.0.green
    }
}
