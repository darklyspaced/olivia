pub mod utils;

use std::iter::Peekable;

use crate::{
    ast::{OpKind, OpType},
    disjoint_set::DisjointSet,
    env::Env,
    error::{
        self,
        parse_err::{ParseError, ParseErrorKind as PEKind},
        source_map::SourceMap,
    },
    green_builder::{GreenBuilder, StoreMode},
    green_node::{Green, GreenNode},
    interner::Interner,
    lexer::Lexer,
    syntax::SyntaxKind,
    token::{Token, TokenKind},
};

type Error = error::Error<ParseError>;

// TODO: have an error sink instead of this Iterator pattern and relying on the consumer to pick up
// errors
impl Iterator for Parser<'_> {
    type Item = Result<(), Error>;
    /// How this function operates is dependant on `State`. If `State::Parse`, then we parse. If
    /// `State::Recover` then we must recover then change the state back to `State::Parse` and then
    /// return a token.
    ///
    /// If `State::Recover`, then we can assume that the last item returned from this iterater was
    /// `Error`
    ///
    /// Need some way to know where to recover until specific token?
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.state {
                State::Parse => {
                    if self.toks.peek().is_none() {
                        self.state = State::Finished;
                        continue;
                    }
                    return Some(self.stmt());
                }
                State::Recover => {
                    // TODO: implement error recovery on a per function basis where try maybe
                    // returns to an error sink instead of actually returning from parsing the
                    // function but i don't really care about error recovery for now
                    self.builder.error();
                    while self.next_if(|tok| {
                        tok.as_ref().is_some_and(|a| {
                            a.as_ref().is_ok_and(|b| {
                                matches!(
                                    b.kind,
                                    TokenKind::Semicolon
                                        | TokenKind::While
                                        | TokenKind::Equal
                                        | TokenKind::For
                                        | TokenKind::RightBrace,
                                )
                            })
                        })
                    }) {}
                    self.builder.exit_node();
                    self.state = State::Parse;
                }
                State::Abort | State::Finished => break None,
            }
        }
    }
}

/// The state of the parser
#[derive(PartialEq, Eq)]
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
    builder: GreenBuilder<'de>,
    output: Option<GreenNode<'de>>,
    toks: Peekable<Lexer<'de>>,
    disjoint_set: DisjointSet,
    env: Env,
    ids: usize,
}

// TODO: need to add support for . notation for field access
// TODO: add support for use statements and paths to fully qualify names
impl<'de> Parser<'de> {
    pub fn new(iter: Lexer<'de>, source_map: &'de SourceMap, interner: &'de mut Interner) -> Self {
        Self {
            state: State::Parse,
            toks: iter.peekable(),
            env: Env::new(),
            builder: GreenBuilder::new(),
            output: None,
            disjoint_set: DisjointSet::default(),
            source_map,
            interner,
            ids: 0,
        }
    }

    pub fn output(&mut self) -> GreenNode<'de> {
        if self.state == State::Finished {
            std::option::Option::take(&mut self.output).unwrap()
        } else {
            panic!("don't call before the parser returns None")
        }
    }

    fn impl_block(&mut self) -> Result<(), Error> {
        self.builder.enter_node(SyntaxKind::Impl);
        self.consume();

        self.ident(PEKind::ExpImplStructTargetFound)?;

        let mut next = self.peek(PEKind::ExpRBraceFound)?.kind;

        while next != TokenKind::RightBrace {
            self.fn_decl()?;
            next = self.peek(PEKind::ExpRBraceFound)?.kind;
        }

        self.builder.exit_node();
        Ok(())
    }

    fn structure(&mut self) -> Result<(), Error> {
        self.builder.enter_node(SyntaxKind::Struct);
        self.consume();

        let _struct_name = self.ident(PEKind::ExpIdentFound)?;

        self.builder.enter_node(SyntaxKind::FieldList);
        self.eat(TokenKind::LeftBrace, PEKind::ExpLBraceFound)?;

        while self
            .peek(|x| PEKind::ExpFound(vec![TokenKind::Ident, TokenKind::RightBrace], x))?
            .kind
            != TokenKind::RightBrace
        {
            let _field = self.ident(PEKind::ExpIdentFound)?;
            self.eat(TokenKind::Colon, PEKind::ExpTyAnnotationFound)?;
            let _ty = self.ident(PEKind::ExpTyFound)?;
            self.eat(TokenKind::Comma, PEKind::ExpCommaFound)?;
        }

        let _r_brace = self.consume();

        self.builder.exit_node();
        Ok(())
    }

    fn if_stmt(&mut self) -> Result<(), Error> {
        self.builder.enter_node(SyntaxKind::If);
        let _if = self.consume();

        self.builder.enter_node(SyntaxKind::IfPredicate);
        self.eat(TokenKind::LeftParen, PEKind::ExpLParenFound)?;
        let _predicate = self.expression()?;
        self.eat(TokenKind::RightParen, PEKind::ExpRParenFound)?;
        self.builder.exit_node();

        let _then = self.block()?;

        if let Some(Ok(Token {
            kind: TokenKind::Else,
            ..
        })) = self.peek_raw()
        {
            let _else = self.consume();
            let next = self.peek(PEKind::ExpIfOrBlockFound)?;
            if next.kind == TokenKind::If {
                // HACK: this might break typeck later i'm not sure (just letting if be parsed here
                // instead of special casing, that is)
                self.if_stmt();
            } else {
                self.block()?;
            }
        }

        self.builder.exit_node();
        Ok(())
    }

    fn for_loop(&mut self) -> Result<(), Error> {
        self.builder.enter_node(SyntaxKind::If);
        let _for_kw = self.consume();

        self.builder.enter_node(SyntaxKind::ForCommand);
        self.eat(TokenKind::LeftParen, PEKind::ExpLParenFound)?;

        self.eat(TokenKind::Let, PEKind::ExpVarDefFound)?;
        let _loop_counter = self.ident(PEKind::ExpIdentFound)?;

        if let Some(Ok(Token {
            kind: TokenKind::Colon,
            ..
        })) = self.peek_raw()
        {
            self.consume();
            let _loop_counter_ty = self.ident(PEKind::ExpTyFound)?;
        }

        self.eat(TokenKind::Equal, |_| PEKind::IdxNotInitialised)?;

        let _value = self.expression()?;
        self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;

        let _predicate = self.expression()?;
        self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;

        self.assignment(false)?;

        self.eat(TokenKind::RightParen, PEKind::ExpRParenFound)?;
        self.builder.exit_node();

        self.block()?;

        self.builder.exit_node();
        Ok(())
    }

    fn fn_decl(&mut self) -> Result<(), Error> {
        self.builder.enter_node(SyntaxKind::Fn);
        let _fn = self.consume();

        let _fn_name = self.ident(PEKind::ExpIdentFound)?;

        self.builder.enter_node(SyntaxKind::ParamList);
        self.eat(TokenKind::LeftParen, PEKind::ExpLParenFound)?;

        let mut next =
            self.peek(|x| PEKind::ExpFound(vec![TokenKind::Ident, TokenKind::RightParen], x))?;
        if next.kind != TokenKind::RightParen {
            loop {
                let _param_name = self.ident(PEKind::ExpIdentFound)?;

                self.eat(TokenKind::Colon, PEKind::ExpTyAnnotationFound)?;
                let _ty = self.ident(PEKind::ExpTyFound)?;

                next = self
                    .peek(|x| PEKind::ExpFound(vec![TokenKind::Comma, TokenKind::RightParen], x))?;

                match next.kind {
                    TokenKind::RightParen => {
                        let _r_paren = self.consume();
                        self.builder.exit_node();
                        break;
                    }
                    TokenKind::Comma => {
                        let _comma = self.consume();
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
            TokenKind::Arrow => {
                self.builder.enter_node(SyntaxKind::RetTy);
                let _arrow = self.consume();
                self.ident(PEKind::ExpIdentFound)?
            }
            TokenKind::LeftBrace => (),
            _ => return Err(self.make_err(err)),
        };

        self.block()?;

        self.builder.exit_node();
        Ok(())
    }

    fn block(&mut self) -> Result<(), Error> {
        self.builder.enter_node(SyntaxKind::Block);

        let _l_brace = self.eat(TokenKind::LeftBrace, PEKind::ExpLParenFound)?;

        let mut next = self.peek(PEKind::ExpLBraceFound)?;
        while next.kind != TokenKind::RightBrace {
            self.stmt()?;
            next = self.peek(PEKind::ExpRBraceFound)?;
        }

        let _r_brace = self.consume();

        self.builder.exit_node();
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
        self.builder.enter_node(SyntaxKind::Assignment);

        let _name = self.ident(PEKind::ExpIdentFound)?;
        self.eat(TokenKind::Equal, PEKind::ExpEqualFound)?;
        self.expression()?;
        if semi {
            self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;
        }

        self.builder.exit_node();
        Ok(())
    }

    /// Parse a declaration
    fn declaration(&mut self) -> Result<(), Error> {
        self.builder.enter_node(SyntaxKind::Declaration);
        let _let = self.consume();
        let _name = self.ident(PEKind::ExpIdentFound)?;

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
                let _semicolon = self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;
            }
            _ => {
                return Err(self.make_err(|tok| {
                    PEKind::ExpFound(vec![TokenKind::Semicolon, TokenKind::Equal], tok)
                }));
            }
        }

        self.builder.exit_node();
        Ok(())
    }

    /// Extract an expression, handling any errors that were raised
    fn expression(&mut self) -> Result<(), Error> {
        let res = self.expr(0);
        let Ok(expr) = res else {
            self.state = State::Recover;
            return Err(res.unwrap_err());
        };
        self.builder.set_mode(StoreMode::Direct);
        self.builder.extend(expr);
        Ok(())
    }

    /// An implementation of Pratt Parsing to deal with mathematical operations. All calls to this
    /// function from outside of itself must have `min_bp` = 0.
    ///
    /// The general strategy is to parse the LHS and then to have a look at the RHS. If the RHS
    /// has an operator to the right of it who can't bind it strong enough aka LHS `op1` RHS `op2`
    /// where `op2`.left < `op1`.right then we stop and then fold so we smash LHS and RHS
    /// together w `op1` and set that to LHS and resume from `op2` so state looks like LHS `op2`
    /// RHS `op3`.
    ///
    /// The problem with expressions for these are that they are created from the inside out not
    /// linearly so we have to special case this function
    fn expr(&mut self, min_bp: u8) -> Result<Vec<Green<'de>>, Error> {
        self.builder.set_mode(StoreMode::Cache);

        let next = self.peek(PEKind::ExpExprFound)?;

        let mut lhs = match next.kind {
            TokenKind::Number | TokenKind::Float => {
                let _val_tok = self.consume();
                self.builder.flush()
            }
            TokenKind::Ident => {
                let ident = self.ident_noreg(PEKind::ExpIdentFound)?;

                let next = self.peek(PEKind::ExpSemicolonFound)?;
                match next.kind {
                    TokenKind::LeftParen => {
                        self.builder.enter_node(SyntaxKind::FnApp); // this goes into cache
                        self.builder.leaf_name(ident);
                        self.builder.enter_node(SyntaxKind::ParamList);
                        let _l_paren = self.consume();

                        let mut next = self.peek(PEKind::ExpExprFound)?;
                        if next.kind != TokenKind::RightParen {
                            loop {
                                let _param = self.expression()?;

                                let err = |x| {
                                    PEKind::ExpFound(
                                        vec![TokenKind::Comma, TokenKind::RightParen],
                                        x,
                                    )
                                };
                                next = self.peek(err)?;
                                match next.kind {
                                    TokenKind::RightParen => {
                                        self.consume();
                                        self.builder.exit_node(); // param list
                                        self.builder.exit_node(); // fn app
                                        break;
                                    }
                                    TokenKind::Comma => {
                                        self.consume();
                                        continue;
                                    }
                                    _ => return Err(self.make_err(err)),
                                };
                            }
                        }

                        self.builder.flush()
                    }
                    _ => {
                        self.builder.leaf_name(ident);
                        self.builder.flush()
                    }
                }
            }
            _ => {
                return Err(self.make_err(PEKind::ExpOperandFound));
            }
        };

        // FIXME: probably doesn't play nicely with whitespace
        while self.peek_raw().is_some() {
            let tok = self.peek(PEKind::Unreachable)?;

            let Ok(op_kind) = OpKind::try_from(tok.kind) else {
                break;
            };

            let (l, r) = infix_binding_power(&op_kind);
            if l < min_bp {
                // at this point, we fold towards the left
                break;
            }

            // only want to consume once we know that we're folding so that after
            // folding we can resume on the operator that had a lower BP to the left of
            // it
            let _tok_op = self.consume();
            // let op = Op {
            //     ty,
            //     kind: op_kind,
            //     span: self.source_map.span_from_tok(&tok_op),
            // };
            let mut rhs = self.expr(r)?;
            rhs.extend(self.builder.flush()); // add the opp on

            self.builder.enter_node(SyntaxKind::BinExpr);
            self.builder.extend(lhs);
            self.builder.extend(rhs);
            self.builder.exit_node();

            lhs = self.builder.flush();
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

pub enum Kind {
    Atom,
    FnApp,
    Ident,
}
