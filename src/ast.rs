use crate::lexer::Token;

pub enum Expr {
    BinOp(Op, Box<Expr>, Box<Expr>),
    UnaryOp(Op, Box<Expr>),
    Unit(Type),
}

/// Built in primitive types
pub enum Type {
    Integer,
    Float,
}

pub enum Op {
    Add,
    Minus,
    Mult,
    Div,
    Pow,
}

pub struct Parser<'de, I: Iterator<Item = Token<'de>>> {
    toks: I,
}

impl<'de, I: Iterator<Item = Token<'de>>> Iterator for Parser<'de, I> {
    type Item = Expr;
    fn next(&mut self) -> Option<Self::Item> {
        match self.toks.next()?.kind {}
    }
}
