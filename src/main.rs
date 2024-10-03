/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use clap::Parser;
use toad::{Command, Engine};

/// A UCI-compatible chess engine
#[derive(Parser, Debug)]
#[command(version, about)]
struct Args {
    /// List of commands to send to the engine upon startup.
    ///
    /// These commands will be executed in the exact order that they are supplied.
    #[arg(short = 'c', long = "command", required = false)]
    commands: Vec<Command>,

    /// Exit the engine after the supplied command(s) have been executed.
    #[arg(short = 'e', long = "exit", default_value = "false")]
    exit: bool,

    /// Do not print package metadata (name, version, authors) on startup.
    #[arg(short = 'q', long = "quiet", default_value = "false")]
    quiet: bool,
}

fn main() {
    let args = Args::parse();

    // Print some metadata about the engine
    if !args.quiet {
        let name = env!("CARGO_PKG_NAME");
        let version = env!("CARGO_PKG_VERSION");
        let authors = env!("CARGO_PKG_AUTHORS").replace(':', ", "); // Split multiple authors by comma and space
        println!("{name} {version} by {authors}");
    }

    // Now create an engine and send commands, if necessary
    let toad = Engine::new();

    // Send commands to the engine, if they were provided.
    for cmd in args.commands {
        toad.send_command(cmd);
    }

    // Make the engine quit after executing all other commands, if requested.
    if args.exit {
        toad.send_command(Command::Quit { cleanup: true });
    }

    // Run the engine's main event loop
    if let Err(e) = toad.run() {
        eprintln!("{} encountered a fatal error: {e}", env!("CARGO_PKG_NAME"));
    }
}
