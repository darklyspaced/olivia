use std::{
    collections::HashMap,
    fs::read_to_string,
    path::{Path, PathBuf},
};

use crate::{
    error::Error,
    lexer::{Lexer, Token},
};

pub struct SourceMap {
    /// Holds the copy of the string to which the rest of the program refers to and is alive for
    /// the length of the program
    map: HashMap<PathBuf, String>,
}

impl SourceMap {
    pub fn new(paths: Vec<PathBuf>) -> Self {
        let mut map = HashMap::new();
        for path in paths {
            let source = read_to_string(&path);
            map.insert(path, source.expect("failed to read source"));
        }

        Self { map }
    }

    pub fn get(&self, path: &Path) -> &str {
        self.map.get(path).expect("path has no source")
    }

    pub fn lex(&self) -> Vec<impl Iterator<Item = Result<Token, Error>>> {
        let mut lexers = vec![];
        for (path, source) in &self.map {
            lexers.push(Lexer::new(source, path, self))
        }

        lexers
    }
}
