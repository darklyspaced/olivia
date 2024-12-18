use std::{fmt::Display, ops::Range};

#[derive(Debug)]
pub struct Error {
    pub source: String,
    /// Where the error actually is in the source
    pub error: Range<usize>,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "ERROR:")?;

        let (mut left_pad, err_len) = (0, self.error.end - self.error.start);

        let hi = self.source[..self.error.start]
            .chars()
            .rev()
            .take_while(|c| *c != '\n')
            .inspect(|_| left_pad += 1);
        let lo = self.source[self.error.end..]
            .chars()
            .take_while(|c| *c != '\n');
        let context = hi
            .chain(self.source[self.error.start..self.error.end].chars())
            .chain(lo)
            .collect::<String>();

        writeln!(f, "{}", context)?;

        for _ in 0..left_pad {
            write!(f, " ")?;
        }
        for _ in 0..err_len {
            write!(f, "^")?;
        }
        Ok(())
    }
}

impl std::error::Error for Error {}
