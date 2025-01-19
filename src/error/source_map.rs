use crate::lexer::Token;

pub struct SourceMap {
    source: String,
}

impl<'de> SourceMap {
    fn from(source: String) -> Self {
        Self { source }
    }

    /// Returns the line, and line number of the token
    fn ctxt_from_tok(&'de self, tok: &Token<'de>) -> (usize, &'de str) {
        let start = tok.lexeme.as_ptr() as usize - self.source.as_ptr() as usize;

        let mut offset = 0;
        let (num, line) = self
            .source
            .lines()
            .enumerate()
            .find(|(_, line)| {
                offset += line.len();
                offset >= start
            })
            .expect("offset not in source");

        (num + 1, line)
    }
}
