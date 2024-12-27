use std::{fmt::Display, path::Path, str::Chars};

use crate::error::{Error, ErrorKind};

pub struct Lexer<'de> {
    path: &'de Path,
    source: &'de str,
    chars: Scanner<'de>,
}

impl<'de> Lexer<'de> {
    pub fn new(source: &'de str, path: &'de Path) -> Self {
        Self {
            path,
            source,
            chars: Scanner::new(source),
        }
    }
}

impl Iterator for Scanner<'_> {
    type Item = char;
    fn next(&mut self) -> Option<Self::Item> {
        let mut offset = |x: Option<Self::Item>| {
            x.inspect(|c| {
                self.offset += self.prev;
                self.prev = c.len_utf8();
            })
        };

        match self.peeked.take() {
            Some(x) => offset(x),
            None => offset(self.iter.next()),
        }
    }
}

#[derive(Debug)]
struct Scanner<'de> {
    iter: Chars<'de>,
    peeked: Option<Option<<Self as Iterator>::Item>>,
    prev: usize,
    offset: usize,
}

impl<'de> Scanner<'de> {
    pub fn new(source: &'de str) -> Self {
        Self {
            peeked: None,
            prev: 0,
            offset: 0,
            iter: source.chars(),
        }
    }

    pub fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        self.peeked.get_or_insert_with(|| self.iter.next()).as_ref()
    }

    pub fn next_if_eq<T>(&mut self, expected: &T) -> Option<<Self as Iterator>::Item>
    where
        T: ?Sized,
        <Self as Iterator>::Item: PartialEq<T>,
    {
        self.next_if(|next| next == expected)
    }

    pub fn next_if_neq<T>(&mut self, not_expected: &T) -> Option<<Self as Iterator>::Item>
    where
        T: ?Sized,
        <Self as Iterator>::Item: PartialEq<T>,
    {
        self.next_if(|next| next != not_expected)
    }

    fn next_if(
        &mut self,
        func: impl FnOnce(&<Self as Iterator>::Item) -> bool,
    ) -> Option<<Self as Iterator>::Item> {
        // it's a problem that it calls self.next... messes things up!
        match self.next() {
            Some(matched) if func(&matched) => Some(matched),
            other => {
                self.peeked = Some(other);
                None
            }
        }
    }
}

pub struct Token<'de> {
    pub kind: TokenKind,
    pub lexeme: &'de str,
    pub literal: Option<&'de str>,
}

#[derive(strum_macros::Display, Debug)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Comma,
    Plus,
    Minus,
    Star,
    Dot,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Slash,
    String,
    Number,
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let (c, start) = (self.chars.next()?, self.chars.offset);
        dbg!(start);

        let bare = |kind: TokenKind| {
            Some(Ok(Token {
                kind,
                lexeme: &self.source[start..start + c.len_utf8()],
                literal: None,
            }))
        };
        let mut branching = |single: TokenKind, predicate: char, branch: TokenKind| match self
            .chars
            .next_if_eq(&predicate)
        {
            Some(_) => Some(Ok(Token {
                kind: branch,
                lexeme: &self.source[start..self.chars.offset],
                literal: None,
            })),
            _ => bare(single),
        };
        let full = |kind: TokenKind, lexeme: &'de str, literal: &'de str| {
            Some(Ok(Token {
                kind,
                lexeme,
                literal: Some(literal),
            }))
        };

        match c {
            '(' => bare(TokenKind::LeftParen),
            ')' => bare(TokenKind::RightParen),
            '{' => bare(TokenKind::LeftBrace),
            '}' => bare(TokenKind::RightBrace),
            ';' => bare(TokenKind::Semicolon),
            ',' => bare(TokenKind::Comma),
            '+' => bare(TokenKind::Plus),
            '-' => bare(TokenKind::Minus),
            '*' => bare(TokenKind::Star),
            '.' => bare(TokenKind::Dot),
            '=' => branching(TokenKind::Equal, '=', TokenKind::EqualEqual),
            '!' => branching(TokenKind::Bang, '=', TokenKind::BangEqual),
            '>' => branching(TokenKind::Greater, '=', TokenKind::GreaterEqual),
            '<' => branching(TokenKind::Less, '=', TokenKind::LessEqual),
            '/' => match self.chars.peek() {
                Some('/') => {
                    let _ = self.chars.by_ref().skip_while(|c| *c != '\n');
                    Self::next(self)
                }
                _ => bare(TokenKind::Slash),
            },
            '"' => {
                while self.chars.next_if_neq(&'"').is_some() {}

                assert!(self.chars.next_if_eq(&'"').is_some());

                let side = '"'.len_utf8();
                let lexeme = &self.source[start..self.chars.offset];
                let literal = &lexeme[side..];

                dbg!(lexeme, literal);

                full(TokenKind::String, lexeme, literal)
            }
            x if x.is_whitespace() => Self::next(self),
            _ => Some(Err(Error {
                kind: ErrorKind::UnexpectedCharacter,
                path: self.path.to_path_buf(),
                source: self.source.to_string(),
                error: start..start + c.len_utf8(),
            })),
        }
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{} {} {}",
            self.kind,
            self.lexeme,
            self.literal.unwrap_or("")
        )
    }
}
