use std::{fmt::Display, str};

use crate::error::Error;

pub struct Lexer<'de> {
    whole: &'de str,
    rest: &'de str,
}

impl<'de> Lexer<'de> {
    pub fn new(source: &'de str) -> Self {
        Self {
            whole: source,
            rest: source,
        }
    }
}

#[derive(Debug)]
pub enum Token<'de> {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Comma,
    Plus,
    Minus,
    Star,
    BangEqual,
    EqualEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Slash,
    Dot,
    Literal(&'de str),
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let mut chars = self.rest.char_indices();
        let (offset, c) = chars.next()?;
        self.rest = chars.as_str();

        match c {
            '(' => Some(Ok(Token::LeftParen)),
            ')' => Some(Ok(Token::RightParen)),
            _ => Some(Err(Error {
                source: self.whole.to_string(),
                error: offset..(offset + c.len_utf8()),
            })),
        }
    }
}

impl<'de> Display for Token<'de> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LeftParen => writeln!(f, "LEFT_PAREN ( null"),
            Self::RightParen => writeln!(f, "RIGHT_PAREN ) null"),
            Self::LeftBrace => writeln!(f, "LEFT_BRACE {{ null"),
            Self::RightBrace => writeln!(f, "RIGHT_BRACE }} null"),
            Self::Semicolon => writeln!(f, "SEMICOLON ; null"),
            Self::Comma => writeln!(f, "COMMA , null"),
            Self::Plus => writeln!(f, "PLUS + null"),
            Self::Minus => writeln!(f, "MINUS - null"),
            Self::Star => writeln!(f, "STAR * null"),
            Self::EqualEqual => writeln!(f, "EQUAL_EQUAL == null"),
            Self::LessEqual => writeln!(f, "LESS_EQUAL <= null"),
            Self::GreaterEqual => writeln!(f, "GREATER_EQUAL >= null"),
            Self::BangEqual => writeln!(f, "BANG_EQUAL != null"),
            Self::Less => writeln!(f, "LESS < null"),
            Self::Greater => writeln!(f, "GREATER > null"),
            Self::Slash => writeln!(f, "SLASH / null"),
            Self::Dot => writeln!(f, "DOT . null"),
            Self::Literal(s) => writeln!(f, "LITERAL \"{s}\" {s}"),
        }
    }
}
