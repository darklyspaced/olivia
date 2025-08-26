mod action;
pub mod utils;

use std::iter::Peekable;

use crate::{
    ast::{Ast, AstId, Ident, InnerAst, Op, OpKind, OpType, Untyped},
    error::{
        self,
        parse_err::{ParseError, ParseErrorKind as PEKind},
        source_map::SourceMap,
    },
    interner::Interner,
    lexer::Lexer,
    parser::action::{CompletedTreeIdx, Tree},
    red_node::SyntaxTree,
    syntax::SyntaxKind,
    token::{Token, TokenKind},
    value::Value,
};

type Error = error::Error<ParseError>;

/// The state of the parser
pub enum State {
    /// Parse
    Parse,
    /// Recover until we see a statement boundary
    Recover,
    /// Return `None` forever
    Abort,
    /// Return `None` forever
    Finished,
}

pub struct Parser<'de> {
    state: State,
    interner: &'de mut Interner,
    source_map: &'de SourceMap,
    toks: Peekable<Lexer<'de>>,
    actions: Vec<Tree<'de>>,
    errors: Vec<Error>,
}

// TODO: need to add support for . notation for field access
// TODO: add support for use statements and paths to fully qualify names
impl<'de> Parser<'de> {
    pub fn new(iter: Lexer<'de>, source_map: &'de SourceMap, interner: &'de mut Interner) -> Self {
        Self {
            state: State::Parse,
            toks: iter.peekable(),
            actions: vec![],
            errors: vec![],
            source_map,
            interner,
        }
    }

    pub fn parse(&mut self) -> (SyntaxTree<'_>, Vec<Error>) {
        let errors = vec![];

        loop {
            match self.state {
                State::Parse => {
                    if self.toks.peek().is_none() {
                        self.state = State::Finished;
                        continue;
                    }
                    match self.stmt() {
                        Err(e) => {
                            panic!("oh dear");
                            errors.push(e)
                        }
                        Ok(()) => continue,
                    }
                }
                // TODO: need to fix error reporting by making it local cause rn it's completely
                // fucked yay
                State::Recover => {
                    panic!("pack it in");
                    while self
                        .toks
                        .next_if(|tok| {
                            tok.as_ref().is_ok_and(|x| {
                                matches!(
                                    x.kind,
                                    TokenKind::Semicolon
                                        | TokenKind::While
                                        | TokenKind::Equal
                                        | TokenKind::For
                                )
                            })
                        })
                        .is_some()
                    {}
                    self.state = State::Parse;
                }
                State::Abort => panic!("pack it in"),
                State::Finished => break,
            }
        }

        (
            SyntaxTree::new_root(action::build(std::mem::take(&mut self.actions))),
            errors,
        )
    }

    fn impl_block(&mut self) -> Result<(), Error> {
        let t = self.start();

        self.consume(); // impl
        self.ident(PEKind::ExpImplStructTargetFound)?; // target struct
        self.eat(TokenKind::LeftBrace, PEKind::ExpLBraceFound)?;

        let mut next = self.peek(PEKind::ExpRBraceFound)?.kind;

        while next != TokenKind::RightBrace {
            self.fn_decl()?;
            next = self.peek(PEKind::ExpRBraceFound)?.kind;
        }

        t.end(SyntaxKind::Impl, self);
        Ok(())
    }

    fn structure(&mut self) -> Result<(), Error> {
        let t = self.start();

        self.consume(); // struct
        self.ident(PEKind::ExpIdentFound)?;
        self.eat(TokenKind::LeftBrace, PEKind::ExpLBraceFound)?;

        while self
            .peek(|x| PEKind::ExpFound(vec![TokenKind::Ident, TokenKind::RightBrace], x))?
            .kind
            != TokenKind::RightBrace
        {
            self.ident(PEKind::ExpIdentFound)?; // field
            self.eat(TokenKind::Colon, PEKind::ExpTyAnnotationFound)?;
            self.ident(PEKind::ExpTyFound)?;
            self.eat(TokenKind::Comma, PEKind::ExpCommaFound)?;
        }

        self.consume(); // r_brace

        t.end(SyntaxKind::Struct, self);
        Ok(())
    }

    fn if_stmt(&mut self) -> Result<(), Error> {
        let t = self.start();

        self.consume(); // if
        self.eat(TokenKind::LeftParen, PEKind::ExpLParenFound)?;
        self.expression()?; // predicate
        self.eat(TokenKind::RightParen, PEKind::ExpRParenFound)?;

        self.block()?; // then

        if let Some(Ok(Token {
            kind: TokenKind::Else,
            ..
        })) = self.toks.peek()
        {
            self.consume();
            if let Some(Ok(Token {
                kind: TokenKind::If,
                ..
            })) = self.toks.peek()
            {
                self.if_stmt()?;
            } else {
                self.block()?;
            }
        }

        t.end(SyntaxKind::If, self);

        Ok(())
    }

    fn for_loop(&mut self) -> Result<(), Error> {
        self.eat(TokenKind::LeftParen, PEKind::ExpLParenFound)?;

        self.ident(PEKind::ExpIdentFound)?;

        if let Some(Ok(Token {
            kind: TokenKind::Colon,
            ..
        })) = self.toks.peek()
        {
            self.consume();
            self.ident(PEKind::ExpTyFound)?;
        }

        self.eat(TokenKind::Equal, |_| PEKind::IdxNotInitialised)?;

        self.expression()?; // value
        self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;

        self.expression()?; // predicate
        self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;

        self.assignment(false)?;

        self.eat(TokenKind::RightParen, PEKind::ExpRParenFound)?;
        self.block()?;

        Ok(())
    }

    fn fn_decl(&mut self) -> Result<(), Error> {
        let t = self.start();

        self.consume(); // fn
        self.ident(PEKind::ExpIdentFound)?;
        self.eat(TokenKind::LeftParen, PEKind::ExpLParenFound)?;

        let mut next =
            self.peek(|x| PEKind::ExpFound(vec![TokenKind::Ident, TokenKind::RightParen], x))?;
        if next.kind != TokenKind::RightParen {
            loop {
                self.ident(PEKind::ExpIdentFound)?;

                self.eat(TokenKind::Colon, PEKind::ExpTyAnnotationFound)?;
                self.ident(PEKind::ExpTyFound)?;

                next = self
                    .peek(|x| PEKind::ExpFound(vec![TokenKind::Comma, TokenKind::RightParen], x))?;

                match next.kind {
                    TokenKind::RightParen => {
                        self.consume();
                        break;
                    }
                    TokenKind::Comma => {
                        self.consume();
                        continue;
                    }
                    _ => {
                        return Err(self.make_err(|x| {
                            PEKind::ExpFound(vec![TokenKind::RightParen, TokenKind::Comma], x)
                        }));
                    }
                };
            }
        }

        let err = |x| PEKind::ExpFound(vec![TokenKind::LeftBrace, TokenKind::Arrow], x);
        let branch = self.peek(err)?;
        match branch.kind {
            TokenKind::Arrow => self.ident(PEKind::ExpIdentFound)?,
            TokenKind::LeftBrace => (),
            _ => return Err(self.make_err(err)),
        };

        self.block()?;

        t.end(SyntaxKind::FnDecl, self);

        Ok(())
    }

    fn block(&mut self) -> Result<(), Error> {
        let t = self.start();
        self.eat(TokenKind::LeftBrace, PEKind::ExpLParenFound)?;

        let mut next = self.peek(PEKind::ExpLBraceFound)?;
        while next.kind != TokenKind::RightBrace {
            self.stmt()?;
            next = self.peek(PEKind::ExpRBraceFound)?;
        }

        self.consume(); // r_brace

        t.end(SyntaxKind::Block, self);
        Ok(())
    }

    /// func | var | assignment | for | if | struct
    fn stmt(&mut self) -> Result<(), Error> {
        match self
            .peek(|x| PEKind::ExpFound(vec![TokenKind::Fn, TokenKind::Let], x))?
            .kind
        {
            TokenKind::Let => self.declaration(),
            TokenKind::Fn => self.fn_decl(),
            TokenKind::For => self.for_loop(),
            TokenKind::If => self.if_stmt(),
            TokenKind::Impl => self.impl_block(),
            TokenKind::Struct => self.structure(),
            _ => self.assignment(true),
        }
    }

    /// Parse an assignment of a value to an ident
    fn assignment(&mut self, semi: bool) -> Result<(), Error> {
        let t = self.start();
        self.ident(PEKind::ExpIdentFound)?; // bind
        self.eat(TokenKind::Equal, PEKind::ExpEqualFound)?;
        self.expression()?;
        if semi {
            self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;
        }

        t.end(SyntaxKind::Assignment, self);
        Ok(())
    }

    /// Parse a declaration
    fn declaration(&mut self) -> Result<(), Error> {
        let t = self.start();
        self.consume(); // let
        self.ident(PEKind::ExpIdentFound)?;

        let branch = self
            .peek(|x| PEKind::ExpFound(vec![TokenKind::Semicolon, TokenKind::Equal], x))?
            .kind;

        if branch == TokenKind::Colon {
            self.consume();
            self.ident(PEKind::ExpTyFound)?;
        }

        match branch {
            TokenKind::Semicolon => {
                self.consume();
            }
            TokenKind::Equal => {
                self.consume();
                self.expression()?;
                self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;
            }
            _ => {
                return Err(self.make_err(|tok| {
                    PEKind::ExpFound(vec![TokenKind::Semicolon, TokenKind::Equal], tok)
                }));
            }
        };

        t.end(SyntaxKind::Declaration, self);

        Ok(())
    }

    fn parse_params(&mut self) -> Result<(), Error> {
        let t = self.start();
        self.consume(); // l_paren

        let mut next = self.peek(PEKind::ExpExprFound)?;
        if next.kind != TokenKind::RightParen {
            loop {
                self.expression()?;

                let err = |x| PEKind::ExpFound(vec![TokenKind::Comma, TokenKind::RightParen], x);
                next = self.peek(err)?;
                match next.kind {
                    TokenKind::RightParen => {
                        break;
                    }
                    TokenKind::Comma => {
                        let _comma = self.consume();
                        continue;
                    }
                    _ => return Err(self.make_err(err)),
                };
            }
        }

        let _r_paren = self.consume();
        t.end(SyntaxKind::ParamList, self);
        Ok(())
    }

    /// Extract an expression, handling any errors that were raised
    fn expression(&mut self) -> Result<(), Error> {
        let res = self.expr(0);
        let Ok(_) = res else {
            self.state = State::Recover;
            return Err(res.unwrap_err());
        };
        Ok(())
    }

    /// An implementation of Pratt Parsing to deal with mathematical operations. All calls to this
    /// function from outside of itself must have `min_bp` = 0.
    fn expr(&mut self, min_bp: u8) -> Result<CompletedTreeIdx, Error> {
        // TODO: probably need a consume left until m marker so we do need trackers +
        // completed_trackers after all hahahah.....
        let t = self.start();

        let next = self.peek(PEKind::ExpExprFound)?;
        let mut lhs = match next.kind {
            TokenKind::Number | TokenKind::Float => {
                self.consume();
                t.end(SyntaxKind::AtomExpr, self)
            }
            TokenKind::Ident => {
                self.ident(PEKind::ExpIdentFound)?;

                let next = self.peek(PEKind::ExpSemicolonFound)?;
                match next.kind {
                    TokenKind::LeftParen => {
                        self.parse_params()?;
                        t.end(SyntaxKind::FnApp, self)
                    }
                    _ => t.end(SyntaxKind::AtomExpr, self),
                }
            }
            _ => {
                return Err(self.make_err(PEKind::ExpOperandFound));
            }
        };

        while self.toks.peek().is_some() {
            let tok = self.peek(PEKind::Unreachable)?;

            let Ok(op_kind) = OpKind::try_from(tok.kind) else {
                break;
            };

            let (l, r) = infix_binding_power(&op_kind);
            if l < min_bp {
                break; // fold to left
            }

            // only want to consume once we know that we're folding so that after
            // folding we can resume on the operator that had a lower BP to the left of
            // it
            let bin_expr = lhs.precede(self);
            self.consume(); // op

            self.expr(r)?; // rhs
            lhs = bin_expr.end(SyntaxKind::BinExpr, self);
        }

        Ok(lhs)
    }
}

fn infix_binding_power(op: &OpKind) -> (u8, u8) {
    match op {
        OpKind::Equal => (1, 2),
        OpKind::Or => (3, 4),
        OpKind::And => (5, 6),
        OpKind::Greater | OpKind::GreaterEqual | OpKind::Less | OpKind::LessEqual => (7, 8),
        OpKind::Add | OpKind::Sub => (9, 10),
        OpKind::Mult | OpKind::Div => (11, 12),
    }
}
