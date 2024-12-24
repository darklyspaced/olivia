use std::{
    fmt::Display,
    path::Path,
    str::{self, CharIndices, Chars},
};

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
        let mut offset = |x: Option<Self::Item>| x.inspect(|c| self.offset += c.len_utf8());

        match self.peeked.take() {
            Some(x) => offset(x),
            None => offset(self.iter.next()),
        }
    }
}

/// Needed because Peekable<> hides the essential methods required by
#[derive(Debug)]
struct Scanner<'de> {
    iter: Chars<'de>,
    peeked: Option<Option<<Self as Iterator>::Item>>,
    offset: usize,
}

impl<'de> Scanner<'de> {
    pub fn new(source: &'de str) -> Self {
        Self {
            peeked: None,
            offset: 0,
            iter: source.chars(),
        }
    }

    pub fn peek(&mut self) -> Option<&<Self as Iterator>::Item> {
        self.peeked.get_or_insert_with(|| self.iter.next()).as_ref()
    }

    pub fn offset(&self) -> usize {
        self.offset
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
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let (start, c) = (self.chars.offset, self.chars.next()?);

        let bare = |kind: TokenKind| {
            Some(Ok(Token {
                kind,
                lexeme: &self.source[start..start + c.len_utf8()],
                literal: None,
            }))
        };
        let mut branching =
        // TODO: rewrite this with peek method
            |single: TokenKind, predicate: char, branch: TokenKind| match self.chars.peek() {
                Some(x) if *x == predicate => {
                    let c = self.chars.next()?;
                    Some(Ok(Token {
                        kind: branch,
                        lexeme: &self.source[start..self.chars.offset + c.len_utf8()],
                        literal: None,
                    }))
                }
                _ => bare(single),
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
                dbg!(&self.chars);
                while let Some(x) = self.chars.next()
                    && x != '"'
                {}
                // if None, then we're fucked
                let literal = &self.source[dbg!(start..self.chars.offset)];
                None
            }
            c if c.is_whitespace() => Self::next(self),
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
