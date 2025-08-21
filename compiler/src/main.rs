use clap::{Parser, Subcommand};
use colour::formatted::{Colour, Formatted};
use compiler::{
    error::{report::Report, source_map::SourceMap},
    interner::Interner,
    lexer::Lexer,
    parser::Parser as OParser,
};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Olivia {
    #[command(subcommand)]
    command: Commands,
    #[arg(short, long)]
    debug: bool,
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
    let olivia = Olivia::parse();

    match &olivia.command {
        Commands::Parse { filename } => {
            let source_map = SourceMap::from(filename);

            let lexer = Lexer::new(&source_map);
            let mut interner = Interner::with_capacity(1024);

            let parser = OParser::new(lexer, &source_map, &mut interner);
            for stmt in parser {
                match stmt {
                    Ok(s) => println!("{s:?}"),
                    Err(e) => {
                        if olivia.debug {
                            // SAFETY: this _shouldn't_ cause UB as long you don't run cargo
                            // elsewhere while also running this at the same time...
                            unsafe {
                                std::env::set_var("RUST_BACKTRACE", "full");
                            }
                            println!("{}", e.backtrace);
                        }
                        println!("{}", Report::from(e));
                    }
                }
            }
            if !olivia.debug {
                println!(
                    "{}",
                    Formatted::from(
                        String::from("To show backtraces for errors within the compiler itself, enable the `--debug` flag.")
                    ).colour(Colour::Yellow)
                );
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
