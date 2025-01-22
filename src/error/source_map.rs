use std::fs::read_to_string;
use std::path::PathBuf;

use crate::lexer::Token;

pub struct SourceMap {
    source: String,
    path: PathBuf,
}

impl<'de> SourceMap {
    fn from(path: String) -> Self {
        let path = PathBuf::from(path);
        let source = read_to_string(&path).expect("failed to read file");
        Self { source, path }
    }

    /// Returns the line, and line number of the token
    pub fn line_from_tok(&'de self, tok: &Token<'de>) -> (usize, &'de str) {
        let start = tok.lexeme.as_ptr() as usize - self.source.as_ptr() as usize;

        self.line_from_offset(start)
    }

    /// Returns the line, and line number of the offset
    pub fn line_from_offset(&'de self, offset: usize) -> (usize, &'de str) {
        let mut prog = 0;
        let (num, line) = self
            .source
            .lines()
            .enumerate()
            .find(|(_, line)| {
                prog += line.len();
                prog >= offset
            })
            .expect("offset not in source");

        (num + 1, line)
    }

    pub fn source(&self) -> &str {
        &self.source
    }
}
