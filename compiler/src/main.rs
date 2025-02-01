use clap::{Parser, Subcommand};
use compiler::{
    error::{report::Report, source_map::SourceMap},
    interner::Interner,
    lexer::Lexer,
    parser::Parser as OParser,
};

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
    /// Parses the input into an AST
    Parse {
        /// Source to tokenise, then parse
        filename: String,
    },
}

fn main() {
    let malatium = Malatium::parse();

    match &malatium.command {
        Commands::Parse { filename } => {
            let source_map = SourceMap::from(filename);

            let lexer = Lexer::new(&source_map);
            let mut interner = Interner::with_capacity(1024);

            let mut parser = OParser::new(lexer, &source_map, &mut interner);
            match parser.stmt() {
                Ok(x) => println!("{:?}", x),
                Err(e) => println!("{}", Report::from(e)),
            }
        }
        Commands::Tokenize { filename } => {
            let source_map = SourceMap::from(filename);

            let lexer = Lexer::new(&source_map);

            for res in lexer.into_iter() {
                match res {
                    Ok(tok) => {
                        println!("{}", tok);
                    }
                    Err(e) => {
                        println!("{}", Report::from(e));
                    }
                }
            }
            println!("EOF  null");
        }
    }
}
