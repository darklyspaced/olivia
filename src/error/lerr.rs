use std::{fmt::Display, ops::Range, path::PathBuf};

use super::span::Span;

#[derive(Debug)]
pub struct LexErr {
    pub kind: LexErrorKind,
    pub path: PathBuf,
    pub source: String,
    /// Where the error actually is in the source
    pub error: Range<usize>,
}

#[derive(Debug)]
pub enum LexErrorKind {
    UnexpectedCharacter,
    UnterminatedStringLiteral,
}

impl LexErrorKind {
    pub fn annotation(&self) -> String {
        String::from(match self {
            Self::UnexpectedCharacter => "unexpected character here",
            Self::UnterminatedStringLiteral => "starts here",
        })
    }
}

impl Display for LexErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LexErrorKind::UnexpectedCharacter => writeln!(f, "unexpected character found"),
            LexErrorKind::UnterminatedStringLiteral => writeln!(f, "unterminated string literal"),
        }
    }
}

impl Display for LexErr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "ERROR: {}", self.path.display())?;

        let mut offset = 0;
        let line = self
            .source
            .lines()
            .enumerate()
            .find(|(_, line)| {
                offset += line.len();
                offset >= self.error.start
            })
            .expect("offset not in source");

        let line_display = format!("{}| {}", line.0 + 1, line.1);
        writeln!(f, "{}", line_display)?;
        for _c in 0..line_display.len() {
            write!(f, " ")?;
        }

        offset -= line.1.len();
        // the '\n's are removed so they need to be accounted for
        let end = self.error.end - offset - line.0;
        let start = self.error.start - offset - line.0;

        let mut left_pad = String::from("   ");

        for (pos, _) in line.1.char_indices() {
            if pos < start {
                left_pad += " ";
                write!(f, " ")?;
            } else if start <= pos && pos < end {
                write!(f, "^")?;
            }
        }
        write!(f, "\n{}{}", left_pad, self.kind)
    }
}
