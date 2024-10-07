/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use clap::Parser;
use toad::{Engine, EngineCommand};

fn main() {
    let mut toad = Engine::new();
    println!("{} by {}", toad.name(), toad.authors());

    // If a command was provided, send it and then exit
    if let Ok(cmd) = EngineCommand::try_parse_from(std::env::args_os().skip(1)) {
        toad.send_command(cmd);
        toad.send_command(EngineCommand::Exit { cleanup: true });
    }

    // Run the engine's main event loop
    if let Err(e) = toad.run() {
        eprintln!("{} encountered a fatal error: {e}", toad.name());
    }
}
