use std::path::PathBuf;

use clap::{Parser, Subcommand};
use olivia::source::SourceMap;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Malatium {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Tokenises the input
    Tokenize {
        /// Source to tokenise
        filename: String,
    },
}

fn main() {
    let malatium = Malatium::parse();

    match &malatium.command {
        Commands::Tokenize { filename } => {
            let path = PathBuf::from(filename);
            let source_map = SourceMap::new(vec![path]);
            let lexed = source_map.lex();

            for lex in lexed {
                for res in lex {
                    match res {
                        Ok(tok) => {
                            println!("{}", tok);
                        }
                        Err(e) => {
                            println!("{}", e);
                        }
                    }
                }
            }
            println!("EOF  null");
        }
    }
}
