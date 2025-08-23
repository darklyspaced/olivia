use std::{fmt::Display, str::Chars};

use crate::{
    error::{
        Error,
        lex_err::{LexError, LexErrorKind},
        source_map::SourceMap,
    },
    token::{Token, TokenKind},
};

pub struct Lexer<'de> {
    source: &'de str,
    source_map: &'de SourceMap,
    chars: Scanner<'de>,
}

impl<'de> Lexer<'de> {
    pub fn new(source_map: &'de SourceMap) -> Self {
        let source = source_map.source();
        Self {
            source_map,
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
    peeked: Option<Option<char>>,
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

    pub fn peek(&mut self) -> Option<&char> {
        self.peeked.get_or_insert_with(|| self.iter.next()).as_ref()
    }

    pub fn next_if_eq(&mut self, expected: &char) -> Option<char> {
        self.next_if(|next| next == expected)
    }

    pub fn next_if_neq(&mut self, not_expected: &char) -> Option<char> {
        self.next_if(|next| next != not_expected)
    }

    fn next_if(&mut self, func: impl FnOnce(&char) -> bool) -> Option<char> {
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

impl<'de> Iterator for Lexer<'de> {
    type Item = Result<Token<'de>, Error<LexError>>;

    fn next(&mut self) -> Option<Self::Item> {
        let (c, start);

        macro_rules! token {
            ($kind:path) => {
                return Some(Ok(Token {
                    kind: $kind,
                    lexeme: &self.source[start..=self.chars.offset()],
                    literal: None,
                }))
            };
            ($single:path, $pred:expr, $branch:path) => {
                return match self.chars.next_if_eq(&$pred) {
                    Some(_) => Some(Ok(Token {
                        kind: $branch,
                        lexeme: &self.source[start..=self.chars.offset()],
                        literal: None,
                    })),
                    _ => token!($single),
                }
            };
            ($kind:path, $literal:expr) => {
                return Some(Ok(Token {
                    kind: $kind,
                    lexeme: &self.source[start..=self.chars.offset()],
                    literal: Some($literal),
                }))
            };
        }

        macro_rules! error {
            ($kind:path) => {
                return Some(Err(LexError {
                    kind: $kind,
                    ctxt: Some(
                        self.source_map
                            .ctxt_from_range((start, self.chars.offset()))
                            .with(line!(), column!()),
                    ),
                }
                .into()))
            };
        }

        (c, start) = (self.chars.next()?, self.chars.offset());
        match c {
            '(' => token!(TokenKind::LeftParen),
            ')' => token!(TokenKind::RightParen),
            '{' => token!(TokenKind::LeftBrace),
            '}' => token!(TokenKind::RightBrace),
            ';' => token!(TokenKind::Semicolon),
            ',' => token!(TokenKind::Comma),
            '+' => token!(TokenKind::Plus),
            '*' => token!(TokenKind::Star),
            ':' => token!(TokenKind::Colon),
            '=' => token!(TokenKind::Equal, '=', TokenKind::EqualEqual),
            '!' => token!(TokenKind::Bang, '=', TokenKind::BangEqual),
            '>' => token!(TokenKind::Greater, '=', TokenKind::GreaterEqual),
            '<' => token!(TokenKind::Less, '=', TokenKind::LessEqual),
            '-' => token!(TokenKind::Minus, '>', TokenKind::Arrow),
            '&' => token!(TokenKind::Ampersand, '&', TokenKind::DoubleAmpersand),
            '|' => match self.chars.peek() {
                Some('|') => {
                    token!(TokenKind::DoublePipe)
                }
                _ => error!(LexErrorKind::UnexpectedCharacter),
            },
            '/' => match self.chars.peek() {
                Some('/') => {
                    while self.chars.next_if_neq(&'\n').is_some() {}
                    let literal = &self.source[start..=self.chars.offset()];
                    token!(TokenKind::Comment, literal)
                }
                _ => token!(TokenKind::Slash),
            },
            '"' => {
                while self.chars.next_if_neq(&'"').is_some() {}

                if self.chars.next_if_eq(&'"').is_none() {
                    error!(LexErrorKind::UnterminatedStringLiteral)
                }

                let side = '"'.len_utf8();
                let literal = &self.source[start + side..=self.chars.offset() - side];

                token!(TokenKind::String, literal)
            }
            'a'..='z' | 'A'..='Z' => {
                while self
                    .chars
                    .next_if(|c| !c.is_whitespace() && c.is_alphanumeric())
                    .is_some()
                {}

                match &self.source[start..=self.chars.offset()] {
                    "while" => token!(TokenKind::While),
                    "for" => token!(TokenKind::For),
                    "let" => token!(TokenKind::Let),
                    "fn" => token!(TokenKind::Fn),
                    "if" => token!(TokenKind::If),
                    "else" => token!(TokenKind::Else),
                    "impl" => token!(TokenKind::Impl),
                    _ => token!(TokenKind::Ident),
                }
            }
            '.' => match self.chars.peek() {
                Some('1'..='9') => {
                    while self.chars.next_if(|c| c.is_ascii_digit()).is_some() {}
                    // TODO: need a better way for the below
                    token!(TokenKind::Float, &self.source[start..=self.chars.offset()])
                }
                _ => token!(TokenKind::Dot),
            },
            '1'..='9' => {
                while self.chars.next_if(|c| c.is_ascii_digit()).is_some() {}

                match self.chars.next_if_eq(&'.') {
                    Some(_) => {
                        while self.chars.next_if(|c| c.is_ascii_digit()).is_some() {}
                        token!(TokenKind::Float, &self.source[start..=self.chars.offset()])
                    }
                    _ => token!(TokenKind::Number, &self.source[start..=self.chars.offset()]),
                }
            }
            x if x.is_whitespace() => {
                while self.chars.next_if(|x| x.is_whitespace()).is_some() {}

                token!(
                    TokenKind::Whitespace,
                    &self.source[start..=self.chars.offset()]
                )
            }
            _ => error!(LexErrorKind::UnexpectedCharacter),
        }
    }
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} \"{}\" {}",
            self.kind,
            self.lexeme,
            self.literal.unwrap_or("")
        )
    }
}
