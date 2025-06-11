pub mod utils;

use std::iter::Peekable;

use crate::{
    ast::{Ast, BindIdent, Expr, Ident, Op, OpKind, TyIdent},
    disjoint_set::DisjointSet,
    env::Env,
    error::{
        self,
        parse_err::{ParseError, ParseErrorKind as PEKind},
        source_map::SourceMap,
    },
    interner::{Interner, Symbol},
    lexer::Lexer,
    token::{Token, TokenKind},
    ty::{Ty, TyConstr, TypeId},
    value::Literal,
};

type Error = error::Error<ParseError>;

impl Iterator for Parser<'_> {
    type Item = Result<Ast, Error>;
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
    disjoint_set: DisjointSet,
    env: Env,
}

impl<'de> Parser<'de> {
    pub fn new(iter: Lexer<'de>, source_map: &'de SourceMap, interner: &'de mut Interner) -> Self {
        Self {
            state: State::Parse,
            toks: iter.peekable(),
            env: Env::new(),
            disjoint_set: DisjointSet::default(),
            source_map,
            interner,
        }
    }

    fn structure(&mut self) -> Result<Ast, Error> {
        println!("gets to structure");
        let _struct = self.toks.next();
        let name = self.ident()?;
        self.eat(TokenKind::LeftBrace, PEKind::ExpLBraceFound)?;
        let mut fields = vec![];

        while self
            .peek(|x| PEKind::ExpFound(vec![TokenKind::Ident, TokenKind::RightBrace], x))?
            .kind
            != TokenKind::RightBrace
        {
            let field = BindIdent(self.ident()?);
            self.eat(TokenKind::Colon, PEKind::ExpTyAnnotationFound)?;
            let ty = TyIdent(self.ident()?);
            self.eat(TokenKind::Comma, PEKind::ExpCommaFound)?;

            fields.push((field, ty));
        }

        let _r_brace = self.toks.next();

        Ok(Ast::Struct { name, fields })
    }

    fn fn_app(&mut self, ident: Ident) -> Result<Ast, Error> {
        let _l_paren = self.toks.next();

        let mut next =
            self.peek(|x| PEKind::ExpFound(vec![TokenKind::Ident, TokenKind::RightParen], x))?;

        let mut params = vec![];

        if next.kind != TokenKind::RightParen {
            loop {
                let expr = self.expression()?;

                params.push(expr);

                let err = |x| PEKind::ExpFound(vec![TokenKind::Comma, TokenKind::RightParen], x);
                next = self.peek(err)?;
                match next.kind {
                    TokenKind::RightParen => {
                        let _r_paren = self.toks.next();
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

        Ok(Ast::Application {
            name: ident,
            params,
        })
    }

    fn if_stmt(&mut self) -> Result<Ast, Error> {
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
                    predicate,
                    then: Box::new(then),
                    otherwise: Some(Box::new(if_stmt)),
                })
            } else {
                let block = self.block()?;
                Ok(Ast::If {
                    predicate,
                    then: Box::new(then),
                    otherwise: Some(Box::new(block)),
                })
            }
        } else {
            Ok(Ast::If {
                predicate,
                then: Box::new(then),
                otherwise: None,
            })
        }
    }

    fn for_loop(&mut self) -> Result<Ast, Error> {
        self.eat(TokenKind::LeftParen, PEKind::ExpLParenFound)?;

        //let ty = TyIdent(self.ident()?); make this optional with ident: ty and match on `:`
        let ident = BindIdent(self.ident()?);
        let mut ty = None;

        if let Some(Ok(Token {
            kind: TokenKind::Colon,
            ..
        })) = self.toks.peek()
        {
            self.toks.next();
            ty = Some(TyIdent(self.ident()?));
        }

        self.eat(TokenKind::Equal, |_| PEKind::IdxNotInitialised)?;

        let value = self.expression()?;
        self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;

        let predicate = self.expression()?;
        self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;

        let assignment = self.assignment(false)?;

        self.eat(TokenKind::RightParen, PEKind::ExpRParenFound)?;
        let block = self.block()?;

        Ok(Ast::ForLoop {
            decl: Box::new(Ast::Declaration(ident, ty, Some(value))),
            predicate,
            assignment: Box::new(assignment),
            block: Box::new(block),
        })
    }

    fn fn_decl(&mut self) -> Result<Ast, Error> {
        let _fn = self.toks.next();
        let ident = self.ident()?;
        self.eat(TokenKind::LeftParen, PEKind::ExpLParenFound)?;

        let mut params = vec![];

        let mut next =
            self.peek(|x| PEKind::ExpFound(vec![TokenKind::Ident, TokenKind::RightParen], x))?;
        if next.kind != TokenKind::RightParen {
            loop {
                let binding = BindIdent(self.ident()?);

                let err = |x| PEKind::ExpFound(vec![TokenKind::Comma, TokenKind::RightParen], x);
                let colon = self.peek(err)?;
                if colon.kind == TokenKind::Colon {
                    let _colon = self.toks.next();
                    let ty = TyIdent(self.ident()?);
                    params.push((binding, Some(ty)));
                    next = self.peek(err)?;
                } else {
                    params.push((binding, None));
                    next = colon
                }

                match next.kind {
                    TokenKind::RightParen => {
                        let _r_paren = self.toks.next();
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

        let err = |x| PEKind::ExpFound(vec![TokenKind::LeftBrace, TokenKind::Arrow], x);
        let branch = self.peek(err)?;
        let ret = match branch.kind {
            TokenKind::Arrow => Some(TyIdent(self.ident()?)),
            TokenKind::LeftBrace => None,
            _ => return Err(self.make_err(err)),
        };

        // let params_ty = params.iter().map(|(_, ty)| if ty.is_none() {
        //     Ty::Var(TyVar(self.fresh().0))
        // } else {
        //         Ty::Var(
        //     })
        // self.env.record(
        //     ident,
        //     TyConstr {
        //         name: ident,
        //         params: params.push(ret),
        //     },
        // );

        let block = self.block()?;

        Ok(Ast::FunDeclaration {
            name: ident,
            params,
            ret,
            block: Box::new(block),
        })
    }

    /// Initialises a fresh type variable and gives it a generated symbol
    fn fresh(&mut self) -> (Symbol, TypeId) {
        let constr_sym = self
            .interner
            .intern(&format!("0x{}", self.disjoint_set.get_len()));
        (constr_sym, self.disjoint_set.fresh(constr_sym))
    }

    fn block(&mut self) -> Result<Ast, Error> {
        let mut stmts = vec![];
        let _l_brace = self.eat(TokenKind::LeftBrace, PEKind::ExpLParenFound)?;

        let mut next = self.peek(PEKind::ExpLBraceFound)?;
        while next.kind != TokenKind::RightBrace {
            stmts.push(self.stmt()?);
            next = self.peek(PEKind::ExpRBraceFound)?;
        }

        let _r_brace = self.toks.next();

        Ok(Ast::Block(stmts.into()))
    }

    /// func | var | assignment | for | if | struct
    fn stmt(&mut self) -> Result<Ast, Error> {
        match self
            .peek(|x| PEKind::ExpFound(vec![TokenKind::Fn, TokenKind::Let], x))?
            .kind
        {
            TokenKind::Let => self.declaration(),
            TokenKind::Fn => self.fn_decl(),
            TokenKind::For => self.for_loop(),
            TokenKind::If => self.if_stmt(),
            TokenKind::Struct => self.structure(),
            _ => self.assignment(true),
        }
    }

    /// Parse an assignment of a value to an ident
    fn assignment(&mut self, semi: bool) -> Result<Ast, Error> {
        let ident = self.ident()?;
        let _equal = self.eat(TokenKind::Equal, PEKind::ExpEqualFound)?;
        let expr = self.expression()?;
        if semi {
            let _semicolon = self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;
        }

        Ok(Ast::Assignment(ident, expr))
    }

    /// Parse a declaration
    fn declaration(&mut self) -> Result<Ast, Error> {
        let _let = self.toks.next();
        let ident = BindIdent(self.ident()?);
        let mut ty = None;

        let branch =
            self.peek(|x| PEKind::ExpFound(vec![TokenKind::Semicolon, TokenKind::Equal], x))?;
        let kind = branch.kind; // BORROW: signals to the compiler that the reference isn't
        // actually used further on

        if kind == TokenKind::Colon {
            self.toks.next();
            ty = Some(TyIdent(self.ident()?));
        }

        match kind {
            TokenKind::Semicolon => {
                self.toks.next();

                Ok(Ast::Declaration(ident, ty, None))
            }
            TokenKind::Equal => {
                self.toks.next();

                let expr = self.expression()?;

                let _semicolon = self.eat(TokenKind::Semicolon, PEKind::ExpSemicolonFound)?;

                Ok(Ast::Declaration(ident, ty, Some(expr)))
            }
            _ => Err(self.make_err(|tok| {
                PEKind::ExpFound(vec![TokenKind::Semicolon, TokenKind::Equal], tok)
            })),
        }
    }

    /// Extract an expression, handling any errors that were raised
    fn expression(&mut self) -> Result<Expr, Error> {
        let res = self.expr(0);
        let Ok(expr) = res else {
            self.state = State::Recover;
            return Err(res.unwrap_err());
        };
        Ok(expr)
    }

    /// An implementation of Pratt Parsing to deal with mathematical operations. All calls to this
    /// function from outside of itself must have `min_bp` = 0.
    fn expr(&mut self, min_bp: u8) -> Result<Expr, Error> {
        let next = self.peek(PEKind::ExpExprFound)?;
        let mut lhs = match next.kind {
            TokenKind::Number | TokenKind::Float => {
                let val_tok = self.toks.next().unwrap().unwrap();
                Expr::Atom(Literal {
                    kind: val_tok.val(),
                    span: self.source_map.span_from_tok(&val_tok),
                })
            }
            TokenKind::Ident => {
                let ident = self.ident()?;
                let mut params = vec![];

                let next = self.peek(PEKind::ExpSemicolonFound)?;
                match next.kind {
                    TokenKind::LeftParen => {
                        self.next();

                        let mut next = dbg!(self.peek(PEKind::ExpExprFound)?);
                        if next.kind != TokenKind::RightParen {
                            loop {
                                params.push(self.expression()?);

                                let err = |x| {
                                    PEKind::ExpFound(
                                        vec![TokenKind::Comma, TokenKind::RightParen],
                                        x,
                                    )
                                };

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

                        let _r_paren = self.next();

                        Expr::FnInvoc(
                            BindIdent(ident),
                            if params.is_empty() {
                                None
                            } else {
                                Some(params)
                            },
                        )
                    }
                    _ => Expr::Ident(BindIdent(ident)),
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
            let ty = OpTy::from(tok.kind);

            let (l, r) = infix_binding_power(&op_kind);
            if l < min_bp {
                // at this point, we fold towards the left
                break;
            }

            // only want to consume once we know that we're folding so that after
            // folding we can resume on the operator that had a lower BP to the left of
            // it
            let tok_op = self.toks.next().unwrap().unwrap();
            let op = Op {
                kind: op_kind,
                span: self.source_map.span_from_tok(&tok_op),
            };
            let rhs = self.expr(r)?;
            lhs = Expr::BinOp(op, Box::new(lhs), Box::new(rhs))
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
