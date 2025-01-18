use std::{fmt::Display, ops::Range, path::PathBuf};

#[derive(Debug)]
pub struct LexErr {
    pub kind: ErrorKind,
    pub path: PathBuf,
    pub source: String,
    /// Where the error actually is in the source
    pub error: Range<usize>,
}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedCharacter,
    UnterminatedStringLiteral,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::UnexpectedCharacter => writeln!(f, "unexpected character found"),
            ErrorKind::UnterminatedStringLiteral => writeln!(f, "unterminated string literal"),
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

impl std::error::Error for LexErr {}
