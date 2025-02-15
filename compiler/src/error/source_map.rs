use std::fs::read_to_string;
use std::path::PathBuf;

use crate::token::Token;

use super::reportable::RawCtxt;
use super::span::Span;

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

    /// Used during parsing to attach spans to AST
    pub fn span_from_tok(&'de self, tok: &Token<'de>) -> Span {
        let start = tok.lexeme.as_ptr() as usize - self.source.as_ptr() as usize;
        let end = start + tok.lexeme.len();

        Span::from(start, end)
    }

    /// Returns gleaned context from token
    pub fn ctxt_from_tok(&'de self, tok: &Token<'de>) -> RawCtxt {
        let tok_offset = tok.lexeme.as_ptr() as usize - self.source.as_ptr() as usize;

        let (num, line) = self.line_from_offset(tok_offset);
        let line_offset = line.as_ptr() as usize - self.source.as_ptr() as usize;

        let rel_start = tok_offset - line_offset;
        let rel_end = rel_start + tok.lexeme.len();

        RawCtxt {
            path: self.path.to_string_lossy().to_string(),
            line: String::from(line),
            num,
            annotation_range: dbg!((rel_start, rel_end)),
            code_pos: None,
        }
    }

    /// Returns the last line and it's line number. This points to one past the last character on
    /// the last line since it assumes that it's an error to do with EOFs
    pub fn ctxt_from_end(&'de self) -> RawCtxt {
        let mut lines = self.source.lines().rev();
        let line = format!(
            "{} ",
            lines.next().expect("should have been non-empty").trim_end()
        ); // add a space so we can point to it to represent EOF

        let rel_end = line.len();
        let rel_start = rel_end - " ".len();

        RawCtxt {
            path: self.path.to_string_lossy().to_string(),
            line,
            num: lines.count() + 1,
            annotation_range: (rel_start, rel_end),
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

    /// Returns the line, and line number of the offset
    fn line_from_offset(&'de self, offset: usize) -> (usize, &'de str) {
        let mut prog = 0;
        let (num, line) = self
            .source
            .lines()
            .enumerate()
            .find(|(_, line)| {
                prog += line.len() + '\n'.len_utf8();
                prog > offset
            })
            .expect("offset not in source");

        (num + 1, line)
    }

    pub fn source(&self) -> &str {
        &self.source
    }
}
