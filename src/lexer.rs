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
            // in this case, self.next() has already been called
            Some(x) => x,
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

    pub fn next_if_eq(&mut self, expected: &char) -> Option<<Self as Iterator>::Item> {
        self.next_if(|next| next == expected)
    }

    pub fn next_if_neq(&mut self, not_expected: &char) -> Option<<Self as Iterator>::Item> {
        self.next_if(|next| next != not_expected)
    }

    fn next_if(
        &mut self,
        func: impl FnOnce(&<Self as Iterator>::Item) -> bool,
    ) -> Option<<Self as Iterator>::Item> {
        match self.next() {
            Some(matched) if func(&matched) => Some(matched),
            other => {
                self.peeked = Some(other);
                None
            }
        }
    }

    fn offset(&self) -> usize {
        self.offset
            - match self.peeked {
                Some(Some(c)) => c.len_utf8(),
                _ => 0,
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
    While,
}

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        let (c, start) = (self.chars.next()?, self.chars.offset());

        macro_rules! token {
            ($kind:path) => {
                Some(Ok(Token {
                    kind: $kind,
                    lexeme: &self.source[start..=self.chars.offset()],
                    literal: None,
                }))
            };
            ($single:path, $pred:expr, $branch:path) => {
                match self.chars.next_if_eq(&$pred) {
                    Some(_) => Some(Ok(Token {
                        kind: $branch,
                        lexeme: &self.source[start..=self.chars.offset()],
                        literal: None,
                    })),
                    _ => token!($single),
                }
            };
            ($kind:path, $literal:expr) => {
                Some(Ok(Token {
                    kind: $kind,
                    lexeme: &self.source[start..=self.chars.offset()],
                    literal: Some($literal),
                }))
            };
        }

        match c {
            '(' => token!(TokenKind::LeftParen),
            ')' => token!(TokenKind::RightParen),
            '{' => token!(TokenKind::LeftBrace),
            '}' => token!(TokenKind::RightBrace),
            ';' => token!(TokenKind::Semicolon),
            ',' => token!(TokenKind::Comma),
            '+' => token!(TokenKind::Plus),
            '-' => token!(TokenKind::Minus),
            '*' => token!(TokenKind::Star),
            '.' => token!(TokenKind::Dot),
            '=' => token!(TokenKind::Equal, '=', TokenKind::EqualEqual),
            '!' => token!(TokenKind::Bang, '=', TokenKind::BangEqual),
            '>' => token!(TokenKind::Greater, '=', TokenKind::GreaterEqual),
            '<' => token!(TokenKind::Less, '=', TokenKind::LessEqual),
            '/' => match self.chars.peek() {
                Some('/') => {
                    while self.chars.next_if_neq(&'\n').is_some() {}
                    Self::next(self)
                }
                _ => token!(TokenKind::Slash),
            },
            '"' => {
                while self.chars.next_if_neq(&'"').is_some() {}

                if self.chars.next_if_eq(&'"').is_none() {
                    return Some(Err(Error {
                        kind: ErrorKind::UnterminatedStringLiteral,
                        path: self.path.to_path_buf(),
                        source: self.source.to_string(),
                        error: start..start + c.len_utf8(),
                    }));
                }

                let side = '"'.len_utf8();
                let literal = &self.source[start + side..=self.chars.offset() - side];

                token!(TokenKind::String, literal)
            }
            'a'..='z' | 'A'..='Z' => {
                while self.chars.next_if(|c| !c.is_whitespace()).is_some() {}
                let lexeme = &self.source[start..=self.chars.offset()];

                match lexeme {
                    "while" => token!(TokenKind::While),
                    _ => unimplemented!(),
                }
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
