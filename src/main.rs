use clap::{error::ErrorKind, Parser};
use toad::Engine;

fn main() {
    let engine = Engine::new();

    // Skip the executable name
    let args = std::env::args().skip(1).collect::<Vec<_>>();

    // eprintln!("ARGS: {args:?}");

    let mut arg_idx = args.len();
    let mut parsed_idx = 0;
    while parsed_idx < arg_idx {
        // eprint!("Attempting to parse [{parsed_idx}..{arg_idx}]: ");
        let slice = &args[parsed_idx..arg_idx];
        // eprintln!("Slice: {slice:?}");

        match toad::Cli::try_parse_from(slice) {
            Ok(cli) => {
                // eprintln!("CLAP ARGS: {cli:?}");
                engine.send_command(cli.command).unwrap();
                parsed_idx = arg_idx;
                arg_idx = args.len();
            }

            // Edge case: `--help` and `--version` are both "error" cases according to Clap
            Err(e)
                if matches!(e.kind(), ErrorKind::DisplayHelp)
                    || matches!(e.kind(), ErrorKind::DisplayVersion) =>
            {
                println!("{e}");
                parsed_idx = arg_idx;
                arg_idx = args.len();
            }

            Err(e) => {
                // TODO: Remove this print out once multi-command parsing is working
                eprintln!("ERROR on input {slice:?}:\n{e}");
                arg_idx -= 1;
            }
        }
    }

    if let Err(e) = engine.run() {
        eprintln!("{} encountered an error: {e}", env!("CARGO_PKG_NAME"));
    }
}
