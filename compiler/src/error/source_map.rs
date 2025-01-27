use std::fs::read_to_string;
use std::ops::Range;
use std::path::PathBuf;

use crate::lexer::Token;

use super::reportable::RawCtxt;

pub struct SourceMap {
    source: String,
    path: PathBuf,
}

impl<'de> SourceMap {
    pub fn from(path: &str) -> Self {
        let path = PathBuf::from(path);
        let source = read_to_string(&path).expect("failed to read file");
        Self { source, path }
    }

    /// Returns the line, and line number of the token
    fn line_from_tok(&'de self, tok: &Token<'de>) -> (usize, &'de str) {
        let start = tok.lexeme.as_ptr() as usize - self.source.as_ptr() as usize;

        self.line_from_offset(start)
    }

    /// Returns the line, and line number of the offset
    fn line_from_offset(&'de self, offset: usize) -> (usize, &'de str) {
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

    /// Returns the last line and it's line number
    pub fn line_from_end(&'de self) -> RawCtxt {
        let mut lines = self.source.lines().rev();
        let line = lines.next().expect("should have been non-empty");
        let last_char = line.chars().next_back().map_or(0, |char| char.len_utf8());

        RawCtxt {
            path: self.path.to_string_lossy().to_string(),
            line: String::from(line),
            num: lines.count() + 1,
            annotation_range: (line.len() - last_char, line.len()),
            code_pos: None,
        }
    }

    /// Returns a RawCtxt without (line!(), column!()) which must then be added using the `with()`
    /// builder method in place so that it's accurate if the line and column are desired.
    ///
    /// The range is inclusive
    pub fn ctxt_from_range(&self, (start, end): (usize, usize)) -> RawCtxt {
        let (num, line) = self.line_from_offset(start);
        let line_offset = line.as_ptr() as usize - self.source.as_ptr() as usize;
        let (start, end) = (start - line_offset, end - line_offset);

        RawCtxt {
            path: self.path.to_string_lossy().to_string(),
            line: String::from(line),
            num,
            annotation_range: (start, end),
            code_pos: None,
        }
    }

    pub fn source(&self) -> &str {
        &self.source
    }
}
