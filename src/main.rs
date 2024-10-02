use toad::{parse_input, Engine};

/*
/// A UCI-compatible chess engine
#[derive(clap::Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    /// Name of the person to greet
    #[arg(short = 'x', long)]
    command: String,
}
 */

fn main() {
    // Print some metadata about the engine
    let name = env!("CARGO_PKG_NAME");
    let version = env!("CARGO_PKG_VERSION");
    let authors = env!("CARGO_PKG_AUTHORS").replace(':', ", "); // Split multiple authors by comma-space
    println!("{name} {version} by {authors}");

    let toad = Engine::new();

    // TODO: Use Clap with -x to send command(s) to engine
    // Skip the executable name
    let args = std::env::args().skip(1).collect::<Vec<_>>().join(" ");

    // Send input to engine
    if let Ok(cmd) = parse_input(&args) {
        toad.send_command(cmd).unwrap();
    }

    /*
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
     */

    if let Err(e) = toad.run() {
        eprintln!("{} encountered a fatal error: {e}", env!("CARGO_PKG_NAME"));
    }
}
