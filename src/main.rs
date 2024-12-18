use std::fs;

use clap::{Parser, Subcommand};
use malatium::lexer::Lexer;

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
            let source = fs::read_to_string(filename).unwrap();
            let lex = Lexer::new(&source);
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
            println!("EOF  null");
        }
    }
}
