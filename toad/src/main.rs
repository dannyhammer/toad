/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use clap::error::ErrorKind;
use clap::Parser;
use toad::{Engine, EngineCommand};

#[derive(Debug, Parser)]
struct Cli {
    /// Execute command(s) upon engine startup.
    #[arg(short, long, required = false)]
    commands: Vec<EngineCommand>,

    /// Do not exit the engine after the startup commands (-c) have executed.
    #[arg(short, long, default_value = "false")]
    no_exit: bool,
}

fn main() {
    // Instantiate the engine
    let mut toad = Engine::new();

    // Attempt to parse command-line arguments, if applicable
    match Cli::try_parse() {
        // If successful, send any provided commands to the engine
        Ok(cli) => {
            // Whether we need to exit after executing the startup command(s)
            let send_quit = !cli.no_exit && !cli.commands.is_empty(); // I know DeMorgan's laws; this is easier to read.

            for cmd in cli.commands {
                toad.send_command(cmd);
            }

            if send_quit {
                toad.send_command(EngineCommand::Exit { cleanup: true });
            }
        }

        // If unsuccessful, try parsing the input as a singular engine command
        Err(cli_err) => {
            // If the error was to display the help/version, do that
            match cli_err.kind() {
                ErrorKind::DisplayHelp | ErrorKind::DisplayVersion => cli_err.exit(),
                _ => {}
            }

            // Skip the executable name
            let args = std::env::args().skip(1).collect::<Vec<_>>().join(" ");

            match args.parse() {
                // If this succeeds, send it to the engine
                Ok(cmd) => {
                    toad.send_command(cmd);
                    toad.send_command(EngineCommand::Exit { cleanup: true });
                }

                // If it fails, exit with the appropriate error
                Err(cmd_err) => {
                    // If the error was specific to the command, print it and exit
                    match cmd_err.kind() {
                        ErrorKind::InvalidSubcommand
                        | ErrorKind::UnknownArgument
                        | ErrorKind::DisplayHelp
                        | ErrorKind::DisplayVersion => cmd_err.exit(),

                        // Otherwise, just print the original error and exit
                        _ => cli_err.exit(),
                    }
                }
            }
        }
    }

    // Display banner and metadata
    println!(
        " (o)--(o)    {}\n/.______.\\   by {}\n\\________/",
        toad.name(),
        toad.authors()
    );

    // Run the engine's main event loop
    toad.run();
}
