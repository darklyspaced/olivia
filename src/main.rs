use std::{fs::read_to_string, path::Path};

use clap::{Parser, Subcommand};
use olivia::{ast::Parser as OParser, lexer::Lexer};

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
            let source = read_to_string(filename).expect("failed to read file");
            let lexer = Lexer::new(&source, Path::new(filename));
            //for res in lexer.into_iter() {
            //    match res {
            //        Ok(tok) => {
            //            println!("{}", tok);
            //        }
            //        Err(e) => {
            //            println!("{}", e);
            //        }
            //    }
            //}
            let mut parser = OParser::new(lexer);
            println!("{:?}", parser.parse(0));

            println!("EOF  null");
        }
    }
}
