use crate::value::ValueKind;

#[derive(Debug, Eq, Hash, PartialEq, Clone)]
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

#[derive(strum_macros::Display, Debug, PartialEq, Copy, Clone, Hash, Eq)]
pub enum TokenKind {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Semicolon,
    Comma,
    Plus,
    Minus,
    Arrow,
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
    While,
    For,
    Ident,
    // HACK: why do we have both Numbers and Integers and Floats etc
    Number,
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
    Impl,
    Error,
    Comment,
    Whitespace,
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
            TokenKind::While => "WHILE",
            TokenKind::For => "FOR",
            TokenKind::Let => "LET",
            TokenKind::Fn => "FN",
            TokenKind::If => "IF",
            TokenKind::Else => "ELSE",
            TokenKind::Struct => "STRUCT",
            TokenKind::Impl => "IMPL",
            TokenKind::Error => "ERROR",
            TokenKind::String => "STRING",
            TokenKind::Ident => "IDENT",
            TokenKind::Integer => "INT",
            TokenKind::Float => "FLOAT",
            TokenKind::DoublePipe => "||",
            TokenKind::DoubleAmpersand => "&&",
            TokenKind::Ampersand => "&&",
            TokenKind::Colon => ":",
            TokenKind::Number => "NUMBER",
            TokenKind::Whitespace => "WSPACE",
            TokenKind::Comment => "COMMENT",
        }
    }
}
