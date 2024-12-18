use std::{fmt::Display, str};

use crate::error::Error;

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
    offset: usize,
}

impl<'de> Lexer<'de> {
    pub fn new(source: &'de str) -> Self {
        Self {
            whole: source,
            rest: source,
            offset: 0,
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
    Less,
    Greater,
    Slash,
    Dot,
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.rest.chars();
        let c = chars.next()?;

        self.offset += c.len_utf8();
        let c_str = &self.rest[..c.len_utf8()];
        self.rest = chars.as_str();

        let bare = |kind: TokenKind| {
            Some(Ok(Token {
                kind,
                lexeme: c_str,
                literal: None,
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
            '<' => bare(TokenKind::Less),
            '>' => bare(TokenKind::Greater),
            '/' => bare(TokenKind::Slash),
            '.' => bare(TokenKind::Dot),
            '\n' => Self::next(self),
            _ => Some(Err(Error {
                source: self.whole.to_string(),
                error: self.offset - c.len_utf8()..self.offset,
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
            self.literal.unwrap_or("_")
        )
    }
}
