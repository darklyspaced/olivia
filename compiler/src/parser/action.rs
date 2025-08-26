use crate::{
    ast::Ident, green_builder::GreenBuilder, green_node::GreenTree, parser::Parser,
    syntax::SyntaxKind, token::Token,
};

/// A flat stream of actions that describe how to build a tree. The parser produces this because
/// it is an ergonomic way to deal with constructing left recursive data structures.
///
/// Prior art: [rust-analyzer's parser](https://github.com/rust-lang/rust-analyzer/tree/master/crates/parser/src)
#[derive(Debug)]
pub enum Action<'de> {
    Start {
        kind: SyntaxKind,
        /// Relative forward jump to parent in Vec<Action>
        parent: Option<usize>,
    },
    End,
    Token {
        tok: Token<'de>,
    },
    Ident {
        ident: Ident,
    },
    /// If the action has already been consumed by setting it to be the parent of something
    Consumed,
}

fn build(actions: Vec<Action>) -> GreenTree<'_> {
    let builder = GreenBuilder::new();

    for (idx, action) in actions.iter().enumerate() {
        match action {
            Action::Start { kind, parent } => {}
            _ => todo!(),
        }
    }

    todo!()
}

/// This tracks a specific sub-tree that has been created by the parser by essentially acting as an
/// index into its Vec<Event>.
pub struct Tracker {
    /// The positions of `Action::Start {...}` and `Action::End` respectively
    pos: (usize, usize),
}

impl Tracker {
    /// Creates a new sub-tree and tells this completed sub-tree that its parent is actually the
    /// new sub-tree instead of its natural parent by setting the parent point (relatively) to the
    /// new sub-tree
    pub fn precede(&self, p: &mut Parser<'_>, kind: SyntaxKind) {
        p.start(kind);
        let new_pos = p.actions.len() - 1;
        match &mut p.actions[self.pos.0] {
            Action::Start { parent, .. } => {
                *parent = Some(new_pos - self.pos.1);
            }
            _ => unreachable!(),
        }
    }
}

impl Parser<'_> {
    /// Start a new sub-tree. Every token that is consumed after this point will belong to this
    /// sub-tree
    pub fn start(&mut self, kind: SyntaxKind) {
        self.actions.push(Action::Start { kind, parent: None })
    }

    /// End the subtree
    pub fn end(&mut self) -> Tracker {
        self.actions.push(Action::End);

        Tracker {
            pos: (self.start_idx, self.actions.len() - 1),
        }
    }

    /// Abandon adding to the current sub-tree and let all children added instead be adopted by
    /// their grandparents
    pub fn abandon(&mut self) {
        self.actions.remove(self.start_idx);
    }
}
