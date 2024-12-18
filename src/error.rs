use std::{fmt::Display, ops::Range};

#[derive(Debug)]
pub struct Error {
    pub source: String,
    /// Where the error actually is in the source
    pub error: Range<usize>,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "ERROR:");
        let hi = self.source[..self.error.start]
            .chars()
            .rev()
            .take_while(|c| *c != '\n');
        let lo = self.source[self.error.end..]
            .chars()
            .take_while(|c| *c != '\n');
        let context = hi
            .chain(self.source[self.error.start..self.error.end].chars())
            .chain(lo)
            .collect::<String>();
        writeln!(f, "{}", context);
        Ok(())
    }
}

impl std::error::Error for Error {}
