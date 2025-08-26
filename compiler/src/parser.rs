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
    parser::action::Action,
    syntax::SyntaxKind,
    token::{Token, TokenKind},
    value::Value,
};

type Error = error::Error<ParseError>;

impl Iterator for Parser<'_> {
    type Item = Result<Ast<Untyped>, Error>;
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
                State::Abort | State::Finished => break None,
            }
        }
    }
}

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
    actions: Vec<Action<'de>>,
    /// The index of the most recent Action::Start {..} in actions
    start_idx: usize,
    ids: usize,
}

// TODO: need to add support for . notation for field access
// TODO: add support for use statements and paths to fully qualify names
impl<'de> Parser<'de> {
    pub fn new(iter: Lexer<'de>, source_map: &'de SourceMap, interner: &'de mut Interner) -> Self {
        Self {
            state: State::Parse,
            toks: iter.peekable(),
            actions: vec![],
            start_idx: 0,
            source_map,
            interner,
            ids: 0,
        }
    }

    fn impl_block(&mut self) -> Result<Ast<Untyped>, Error> {
        let _impl = self.toks.next();
        let _target_struct = self.ident(PEKind::ExpImplStructTargetFound);

        let mut next = self.peek(PEKind::ExpRBraceFound)?.kind;
        let mut fns = vec![];

        while next != TokenKind::RightBrace {
            let id = self.fresh_id();
            let fn_decl = self.fn_decl()?;

            fns.push(InnerAst { inner: fn_decl, id });

            next = self.peek(PEKind::ExpRBraceFound)?.kind;
        }

        Ok(Ast::ImplBlock(fns.into()))
    }

    fn structure(&mut self) -> Result<Ast<Untyped>, Error> {
        let _struct = self.toks.next();
        let name = self.ident(PEKind::ExpIdentFound)?;
        self.eat(TokenKind::LeftBrace, PEKind::ExpLBraceFound)?;
        let mut fields = vec![];

        while self
            .peek(|x| PEKind::ExpFound(vec![TokenKind::Ident, TokenKind::RightBrace], x))?
            .kind
            != TokenKind::RightBrace
        {
            let field = self.ident(PEKind::ExpIdentFound)?;
            self.eat(TokenKind::Colon, PEKind::ExpTyAnnotationFound)?;
            let ty = self.ident(PEKind::ExpTyFound)?;
            self.eat(TokenKind::Comma, PEKind::ExpCommaFound)?;

            fields.push((field, ty));
        }

        let _r_brace = self.toks.next();

        Ok(Ast::Struct { name, fields })
    }

    fn if_stmt(&mut self) -> Result<Ast<Untyped>, Error> {
        let _if = self.toks.next();
        self.eat(TokenKind::LeftParen, PEKind::ExpLParenFound)?;
        let predicate = self.expression()?;
        self.eat(TokenKind::RightParen, PEKind::ExpRParenFound)?;

        let then = self.block()?;

        if let Some(Ok(Token {
            kind: TokenKind::Else,
            ..
        })) = self.toks.peek()
        {
            let _else = self.toks.next();
            if let Some(Ok(Token {
                kind: TokenKind::If,
                ..
            })) = self.toks.peek()
            {
                let if_stmt = self.if_stmt()?;
                Ok(Ast::If {
                    predicate: self.tag(predicate),
                    then: self.tag(then),
                    otherwise: Some(self.tag(if_stmt)),
                })
            } else {
                let block = self.block()?;
                Ok(Ast::If {
                    predicate: self.tag(predicate),
                    then: self.tag(then),
                    otherwise: Some(self.tag(block)),
                })
            }
        } else {
            Ok(Ast::If {
                predicate: self.tag(predicate),
                then: self.tag(then),
                otherwise: None,
            })
        }
    }

    fn for_loop(&mut self) -> Result<Ast<Untyped>, Error> {
        self.eat(TokenKind::LeftParen, PEKind::ExpLParenFound)?;

        let ident = self.ident(PEKind::ExpIdentFound)?;
        let mut ty = None;

        if let Some(Ok(Token {
            kind: TokenKind::Colon,
            ..
        })) = self.toks.peek()
        {
            self.toks.next();
            ty = Some(self.ident(PEKind::ExpTyFound)?);
        }

        self.eat(TokenKind::Equal, |_| PEKind::IdxNotInitialised)?;

        let value = self.expression()?;
        self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;

        let expr = self.expression()?;
        let predicate = self.tag(expr);
        self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;

        let assignment = self.assignment(false)?;

        self.eat(TokenKind::RightParen, PEKind::ExpRParenFound)?;
        let block = self.block()?;

        let val = self.tag(value);
        Ok(Ast::ForLoop {
            decl: self.tag(Ast::Declaration((ident, ty), Some(val))),
            predicate,
            assignment: self.tag(assignment),
            block: self.tag(block),
        })
    }

    fn fn_decl(&mut self) -> Result<Ast<Untyped>, Error> {
        let _fn = self.toks.next();
        let ident = self.ident(PEKind::ExpIdentFound)?;
        self.eat(TokenKind::LeftParen, PEKind::ExpLParenFound)?;

        let mut params = vec![];

        let mut next =
            self.peek(|x| PEKind::ExpFound(vec![TokenKind::Ident, TokenKind::RightParen], x))?;
        if next.kind != TokenKind::RightParen {
            loop {
                let binding = self.ident(PEKind::ExpIdentFound)?;

                self.eat(TokenKind::Colon, PEKind::ExpTyAnnotationFound)?;
                let ty = self.ident(PEKind::ExpTyFound)?;
                params.push((binding, ty));

                next = self
                    .peek(|x| PEKind::ExpFound(vec![TokenKind::Comma, TokenKind::RightParen], x))?;

                match next.kind {
                    TokenKind::RightParen => {
                        let _r_paren = self.toks.next();
                        break;
                    }
                    TokenKind::Comma => {
                        let _comma = self.toks.next();
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
        let ret = match branch.kind {
            TokenKind::Arrow => Some(self.ident(PEKind::ExpIdentFound)?),
            TokenKind::LeftBrace => None,
            _ => return Err(self.make_err(err)),
        };

        let block = self.block()?;

        Ok(Ast::FunDeclaration {
            name: ident,
            params,
            ret,
            block: self.tag(block),
        })
    }

    fn block(&mut self) -> Result<Ast<Untyped>, Error> {
        let mut stmts = vec![];
        let _l_brace = self.eat(TokenKind::LeftBrace, PEKind::ExpLParenFound)?;

        let mut next = self.peek(PEKind::ExpLBraceFound)?;
        while next.kind != TokenKind::RightBrace {
            let stmt = self.stmt()?;

            let inner = InnerAst {
                inner: stmt,
                id: self.fresh_id(),
            };

            stmts.push(inner);
            next = self.peek(PEKind::ExpRBraceFound)?;
        }

        let _r_brace = self.toks.next();

        Ok(Ast::Block(stmts.into()))
    }

    /// func | var | assignment | for | if | struct
    fn stmt(&mut self) -> Result<Ast<Untyped>, Error> {
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
    fn assignment(&mut self, semi: bool) -> Result<Ast<Untyped>, Error> {
        let ident = self.ident(PEKind::ExpIdentFound)?;
        let _equal = self.eat(TokenKind::Equal, PEKind::ExpEqualFound)?;
        let expr = self.expression()?;
        if semi {
            let _semicolon = self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;
        }

        Ok(Ast::Assignment(ident, self.tag(expr)))
    }

    /// Parse a declaration
    fn declaration(&mut self) -> Result<Ast<Untyped>, Error> {
        let _let = self.toks.next();
        let ident = self.ident(PEKind::ExpIdentFound)?;
        let mut ty = None;

        let branch = self
            .peek(|x| PEKind::ExpFound(vec![TokenKind::Semicolon, TokenKind::Equal], x))?
            .kind;
        // actually used further on

        if branch == TokenKind::Colon {
            self.toks.next();
            ty = Some(self.ident(PEKind::ExpIdentFound)?);
        }

        match branch {
            TokenKind::Semicolon => {
                self.toks.next();

                Ok(Ast::Declaration((ident, ty), None))
            }
            TokenKind::Equal => {
                self.toks.next();

                let expr = self.expression()?;

                let _semicolon = self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;

                Ok(Ast::Declaration((ident, ty), Some(self.tag(expr))))
            }
            _ => Err(self.make_err(|tok| {
                PEKind::ExpFound(vec![TokenKind::Semicolon, TokenKind::Equal], tok)
            })),
        }
    }

    fn parse_params(&mut self, ident: Ident) -> Result<Ast<Untyped>, Error> {
        let mut params = vec![];
        let _l_param = self.toks.next();

        let mut next = self.peek(PEKind::ExpExprFound)?;
        if next.kind != TokenKind::RightParen {
            loop {
                self.ids += 1;
                let inner = InnerAst {
                    inner: self.expression()?,
                    id: AstId(self.ids),
                };
                params.push(inner);

                let err = |x| PEKind::ExpFound(vec![TokenKind::Comma, TokenKind::RightParen], x);

                next = self.peek(err)?;
                match next.kind {
                    TokenKind::RightParen => {
                        break;
                    }
                    TokenKind::Comma => {
                        let _comma = self.toks.next();
                        continue;
                    }
                    _ => return Err(self.make_err(err)),
                };
            }
        }

        let _r_paren = self.toks.next();

        Ok(Ast::FnInvoc(
            ident,
            if params.is_empty() {
                None
            } else {
                Some(params)
            },
        ))
    }

    /// Extract an expression, handling any errors that were raised
    fn expression(&mut self) -> Result<Ast<Untyped>, Error> {
        let res = self.expr(0);
        let Ok(expr) = res else {
            self.state = State::Recover;
            return Err(res.unwrap_err());
        };
        Ok(expr)
    }

    /// An implementation of Pratt Parsing to deal with mathematical operations. All calls to this
    /// function from outside of itself must have `min_bp` = 0.
    fn expr(&mut self, min_bp: u8) -> Result<Ast<Untyped>, Error> {
        // TODO: probably need a consume left until m marker so we do need trackers +
        // completed_trackers after all hahahah.....
        todo!();
        self.start(SyntaxKind::BinExpr);

        let next = self.peek(PEKind::ExpExprFound)?;
        match next.kind {
            TokenKind::Number | TokenKind::Float => {
                let val_tok = self.toks.next().unwrap().unwrap();
                // Ast::Atom(Value {
                //     kind: val_tok.val(),
                //     span: self.source_map.span_from_tok(&val_tok),
                // })
            }
            TokenKind::Ident => {
                self.start(SyntaxKind::FnApp);
                let ident = self.ident(PEKind::ExpIdentFound)?;

                let next = self.peek(PEKind::ExpSemicolonFound)?;
                match next.kind {
                    TokenKind::LeftParen => {
                        self.parse_params(ident)?;
                        self.end(); // close the FnApp
                    }
                    _ => self.abandon(), // abandon the FnApp
                }
            }
            _ => {
                return Err(self.make_err(PEKind::ExpOperandFound));
            }
        };

        if self.toks.peek().is_some() {
            while self.toks.peek().is_some() {
                let tok = self.peek(PEKind::Unreachable)?;

                let Ok(op_kind) = OpKind::try_from(tok.kind) else {
                    self.abandon();
                    break;
                };
                let _ty = OpType::from(tok.kind);

                let (l, r) = infix_binding_power(&op_kind);
                if l < min_bp {
                    // at this point, we fold towards the left
                    break;
                }

                // only want to consume once we know that we're folding so that after
                // folding we can resume on the operator that had a lower BP to the left of
                // it
                let _tok_op = self.toks.next().unwrap().unwrap();
                // let op = Op {
                //     ty,
                //     kind: op_kind,
                //     span: self.source_map.span_from_tok(&tok_op),
                // };

                let _rhs = self.expr(r)?;
                let bin_expr = self.end(); // end after op and rhs
                bin_expr.precede(self, SyntaxKind::BinExpr);
                // lhs = Ast::BinOp(op, self.tag(lhs), self.tag(rhs))
            }
        } else {
            // nothing to parse next so it cannot be a BinExpr
            self.abandon();
        }

        todo!();
        // Ok(())
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
