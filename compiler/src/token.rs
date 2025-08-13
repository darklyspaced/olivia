use crate::value::ValueKind;

#[derive(Debug)]
pub struct Token<'de> {
    pub kind: TokenKind,
    pub lexeme: &'de str,
    pub literal: Option<&'de str>,
}

impl Token<'_> {
    /// Returns the populated value kind of the token
    pub fn val(&self) -> ValueKind {
        match self.kind {
            TokenKind::Number => self.literal.unwrap().parse::<u64>().unwrap().into(),
            _ => panic!("{} doesn't have a type, obviously.", self.kind),
        }
    }
}

#[derive(strum_macros::Display, Debug, PartialEq, Copy, Clone)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Comma,
    Plus,
    PlusDot,
    Minus,
    MinusDot,
    Arrow,
    Star,
    StarDot,
    Dot,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Greater,
    GreaterDot,
    GreaterEqual,
    GreaterEqualDot,
    Less,
    LessDot,
    LessEqual,
    LessEqualDot,
    Slash,
    String,
    Number,
    While,
    For,
    Ident,
    Integer,
    Float,
    Let,
    Fn,
    If,
    Else,
    DoublePipe,
    DoubleAmpersand,
    Ampersand,
    Colon,
    Struct,
}

impl TokenKind {
    pub fn to_string(&self) -> &str {
        match self {
            TokenKind::LeftParen => "(",
            TokenKind::RightParen => ")",
            TokenKind::LeftBrace => "{",
            TokenKind::RightBrace => "}",
            TokenKind::Semicolon => ";",
            TokenKind::Comma => ",",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Star => "*",
            TokenKind::Dot => ".",
            TokenKind::Equal => "=",
            TokenKind::EqualEqual => "==",
            TokenKind::Bang => "!",
            TokenKind::BangEqual => "!=",
            TokenKind::Greater => ">",
            TokenKind::GreaterEqual => ">=",
            TokenKind::Less => "<",
            TokenKind::LessEqual => "<=",
            TokenKind::Arrow => "->",
            TokenKind::Slash => "/",
            TokenKind::While => "while",
            TokenKind::For => "for",
            TokenKind::Let => "let",
            TokenKind::Fn => "fn",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::Struct => "struct",
            tk => unimplemented!("no String repr of {}", tk),
        }
    }
}
