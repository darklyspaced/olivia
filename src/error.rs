use std::{fmt::Display, ops::Range, path::PathBuf};

#[derive(Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub path: PathBuf,
    pub source: String,
    /// Where the error actually is in the source
    pub error: Range<usize>,
}

#[derive(Debug)]
pub enum ErrorKind {
    UnexpectedCharacter,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorKind::UnexpectedCharacter => writeln!(f, "unexpected character found"),
        }
    }
}

impl Display for Error {
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

        writeln!(f, "{}| {}", line.0 + 1, line.1)?;
        write!(f, "   ")?; // on account of ln_num

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

impl std::error::Error for Error {}
