use crate::{
    ast::Ident, green_builder::GreenBuilder, green_node::GreenTree, parser::Parser,
    syntax::SyntaxKind, token::Token,
};

/// A flat stream of actions that describe how to build a tree. The parser produces this because
/// it is an ergonomic way to deal with constructing left recursive data structures.
///
/// Prior art: [rust-analyzer's parser](https://github.com/rust-lang/rust-analyzer/tree/master/crates/parser/src)
#[derive(Debug)]
pub enum Tree<'de> {
    /// Starts a sub-tree
    Start {
        kind: SyntaxKind,
        /// Relative forward jump to parent in Vec<Action>
        parent: Option<usize>,
    },
    /// Ends a sub-tree
    End,
    /// Adds a leaf to a sub-tree
    Token { tok: Token<'de> },
    /// Adds a leaf ident (interned) to a sub-tree
    Ident { ident: Ident },
    /// If the action has already been consumed by setting it to be the parent of something
    Consumed,
}

pub fn build(mut actions: Vec<Tree<'_>>) -> GreenTree<'_> {
    let mut builder = GreenBuilder::new();

    for idx in 0..actions.len() {
        match std::mem::replace(&mut actions[idx], Tree::Consumed) {
            Tree::Start { kind, parent } => {
                // essentially need to follow the chain of parents until we get this this
                // tree's root
                let mut parents = vec![kind];
                let (mut idx, mut curr_parent) = (idx, parent);
                while let Some(offset) = curr_parent {
                    parents.push(
                        match std::mem::replace(&mut actions[idx + offset], Tree::Consumed) {
                            Tree::Start { kind, parent } => {
                                curr_parent = parent;
                                kind
                            }
                            _ => unreachable!(),
                        },
                    );
                    idx += offset;
                }

                for tree in parents.into_iter().rev() {
                    builder.enter_node(tree);
                }
            }
            Tree::End => builder.exit_node(),

            Tree::Token { tok } => builder.leaf(tok),
            Tree::Ident { ident } => builder.leaf_name(ident),

            Tree::Consumed => continue,
        }
    }

    builder.end()
}

/// This tracks a specific sub-tree that has been created by the parser by essentially acting as an
/// index into its Vec<Event>.
#[derive(Debug)]
pub struct CompletedTreeIdx {
    /// The positions of `Action::Start {...}` and `Action::End` respectively
    start: usize,
    end: usize,
}

/// This tracks a specific sub-tree that is being created by a parser by acting as an index into
/// the parser's Vec<Event>.
pub struct TreeIdx {
    /// The positon of `Action::Start {..}`
    start: usize,
}

impl TreeIdx {
    /// End the subtree with the specified kind. It is better to choose the node at the end since
    /// the kind is usually known by this point due to parsing.
    pub fn end(&self, end_kind: SyntaxKind, p: &mut Parser<'_>) -> CompletedTreeIdx {
        p.actions.push(Tree::End);

        match &mut p.actions[self.start] {
            Tree::Start { kind, .. } => {
                *kind = end_kind;
            }
            _ => unreachable!(),
        }

        CompletedTreeIdx {
            start: self.start,
            end: p.actions.len() - 1,
        }
    }

    /// Abandon adding to the current sub-tree and let all children added instead be adopted by
    /// their grandparents
    pub fn abandon(&mut self, p: &mut Parser<'_>) {
        p.actions.remove(self.start);
    }
}

impl CompletedTreeIdx {
    /// Creates a new sub-tree and tells this completed sub-tree that its parent is actually the
    /// new sub-tree instead of its natural parent by setting the parent point (relatively) to the
    /// new sub-tree
    pub fn precede(&self, p: &mut Parser<'_>) -> TreeIdx {
        let t = p.start();
        match &mut p.actions[self.start] {
            Tree::Start { parent, .. } => {
                *parent = Some(t.start - self.start);
            }
            _ => unreachable!(),
        }

        t
    }

    /// Extends this completed sub-tree to include another sub-tree that is behind it
    pub fn extend_back(&self, t: TreeIdx, p: &mut Parser<'_>) {
        match &mut p.actions[t.start] {
            Tree::Start { parent, .. } => {
                *parent = Some(self.start - t.start);
            }
            _ => unreachable!(),
        }
    }
}

impl Parser<'_> {
    /// Start a new sub-tree. Every token that is consumed after this point will belong to this
    /// sub-tree until TreeIdx::end is called
    pub fn start(&mut self) -> TreeIdx {
        self.actions.push(Tree::Start {
            kind: SyntaxKind::Error, // starts off as error then is back-changed
            parent: None,
        });
        TreeIdx {
            start: self.actions.len() - 1,
        }
    }
}
