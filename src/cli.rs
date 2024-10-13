/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use chessie::Square;
use clap::Parser;
use uci_parser::UciCommand;

/// A command to be sent to the engine.
#[derive(Debug, Clone, Parser)]
#[command(multicall = true, about, rename_all = "lower")]
pub enum EngineCommand {
    /// Run a benchmark with the provided parameters.
    Bench {
        /// If set, the benchmarking results will be printed in a well-formatted table.
        #[arg(short, long, default_value = "false")]
        pretty: bool,

        /// Override the default benchmark depth.
        #[arg(short, long, required = false)]
        depth: Option<u8>,
    },

    /// Print a visual representation of the current board state.
    #[command(alias = "d")]
    Display,

    /// Print an evaluation of the current position.
    Eval {
        /// If set, extra information will be printed during evaluation.
        #[arg(short, long, default_value = "false")]
        pretty: bool,
    },

    /// Quit the engine.
    Exit {
        /// If set, the engine will await the completion of any search threads before exiting.
        #[arg(short, long, default_value = "false")]
        cleanup: bool,
    },

    /// Generate and print a FEN string for the current position.
    Fen,

    /// Flips the side-to-move. Equivalent to playing a nullmove.
    Flip,

    /// Display information about the current hash table(s) in the engine.
    #[command(aliases = ["tt", "ttable"])]
    HashInfo,

    /// Apply the provided move to the game, if possible.
    ///
    /// No enforcement of legality, so you can move a White piece twice in a row, if you want.
    MakeMove { mv_string: String },

    /// Shows all legal moves in the current position.
    ///
    /// If `square` is provided, it will display all available moves from that square.
    Moves { square: Option<Square> },

    /// Display the current value of the specified option.
    Option { name: String },

    /// Performs a perft on the current position at the supplied depth, printing total node count.
    Perft { depth: usize },

    /// Performs a split perft on the current position at the supplied depth.
    #[command(alias = "sperft")]
    Splitperft { depth: usize },

    /// Execute a UCI command on the engine.
    ///
    /// If you want to send a UCI command on engine startup, you must prefix it with `uci` like so: `./<engine> uci "go depth 5"`.
    ///
    /// During runtime, however, UCI commands are accessible normally, so you don't need to call this prefix.
    // #[command(skip)]
    // #[command(flatten)]
    // Uci(UciCommand),
    #[command(allow_external_subcommands = true)]
    Uci { cmd: UciCommand },
}
