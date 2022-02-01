use clap::{Parser, Subcommand};
use mdbook::preprocess::{CmdPreprocessor, Preprocessor};
use reports::*;
use serde_json;
use std::{io, process};

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Cli {
    #[clap(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
enum Commands {
    /// Check whether a renderer in supported by this preprocessor
    Supports {
        #[clap(short, long)]
        renderer: Option<String>,
    },
}

fn main() {
    let cli = Cli::parse();

    let preprocessor = Processor::new();

    if let Err(e) = {
        match &cli.command {
            // e.g. cargo run --bin mdbook-reports -- supports --renderer html
            Some(Commands::Supports { renderer }) => handle_supports(&preprocessor, renderer),
            None => handle_preprocessing(&preprocessor),
        }
    } {
        eprintln!("{}", e);
        process::exit(1);
    }
}

fn handle_preprocessing(pre: &dyn Preprocessor) -> Result<(), mdbook::errors::Error> {
    let (ctx, book) = CmdPreprocessor::parse_input(io::stdin())?;

    let processed_book = pre.run(&ctx, book)?;
    serde_json::to_writer(io::stdout(), &processed_book)?;

    Ok(())
}

fn handle_supports(pre: &dyn Preprocessor, renderer: &Option<String>) -> ! {
    let renderer = renderer.as_ref().expect("Required argument");
    let supported = pre.supports_renderer(&renderer);

    // Signal whether the renderer is supported by exiting with 1 or 0
    if supported {
        println!("You're supported!");
        process::exit(0);
    } else {
        println!("You're not supported");
        process::exit(1);
    }
}
