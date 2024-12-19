use std::{fmt::Display, ops::Range, path::PathBuf};

#[derive(Debug)]
pub struct Error {
    pub path: PathBuf,
    pub source: String,
    /// Where the error actually is in the source
    pub error: Range<usize>,
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

        for (pos, c) in line.1.char_indices() {
            if pos < start {
                if c != '\n' {
                    write!(f, " ")?;
                }
            } else if start <= pos && pos < end {
                write!(f, "^")?;
            }
        }

        Ok(())
    }
}

impl std::error::Error for Error {}
