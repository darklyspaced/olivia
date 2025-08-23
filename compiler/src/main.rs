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
    Tokenise {
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
            let mut errored = false;

            let mut parser = OParser::new(lexer, &source_map, &mut interner);
            for stmt in parser {
                match stmt {
                    Ok(_) => (),
                    Err(e) => {
                        if !errored {
                            errored = true;
                        }

                        if olivia.debug {
                            println!("{}", e.backtrace);
                        }
                        println!("{}", Report::from(e));
                    }
                }
            }

            let output = parser.output();
            println!("{output:?}");
            // TODO: define Display for GreenNode<'de>

            if !olivia.debug && errored {
                println!(
                    "{}",
                    Formatted::from(
                        String::from("To show backtraces for errors within the compiler itself, enable the `--debug` flag.")
                    ).colour(Colour::Yellow)
                );
            }
        }
        Commands::Tokenise { filename } => {
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
