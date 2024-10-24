/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::str::FromStr;

use chessie::{Piece, Square};
use clap::Parser;
use uci_parser::UciCommand;

/// A command to be sent to the engine.
#[derive(Debug, Clone, Parser)]
#[command(
    multicall = true,
    about,
    rename_all = "lower",
    override_usage("<ENGINE COMMAND> | <UCI COMMAND>")
)]
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
    Moves {
        square: Option<Square>,

        /// If set, a Bitboard of all possible moves will also be displayed
        #[arg(short, long, default_value = "false")]
        pretty: bool,
    },

    /// Display the current value of the specified option.
    Option { name: String },

    /// Performs a perft on the current position at the supplied depth, printing total node count.
    Perft { depth: usize },

    /// Outputs the Piece-Square table value for the provided piece at the provided square, scaled with the endgame weight.
    ///
    /// If no square was provided, the entire table(s) will be printed.
    /// If no endgame weight was provided, it will be computed from the current game state.
    #[command(aliases = ["psq", "pst"])]
    Psqt {
        /// The piece whose Piece-Square table value(s) to fetch.
        piece: Piece,

        /// Evaluate `piece` at `square`.
        square: Option<Square>,

        /// Evaluate `piece` at `square` with the provided endgame weight [0-100].
        endgame_weight: Option<i32>,
    },

    /// Performs a split perft on the current position at the supplied depth.
    #[command(alias = "sperft")]
    Splitperft { depth: usize },

    /// Wrapper over UCI commands sent to the engine.
    #[command(skip)]
    Uci { cmd: UciCommand },
}

impl FromStr for EngineCommand {
    type Err = clap::Error;
    /// Attempt to parse an [`EngineCommand`] from a string.
    ///
    /// If this fails, it will attempt to parse the string as a [`UciCommand`].
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match Self::try_parse_from(s.split_ascii_whitespace()) {
            Ok(cmd) => Ok(cmd),
            Err(e) => {
                // If parsing failed, attempt to parse as a UciCommand
                if let Ok(cmd) = UciCommand::new(s) {
                    Ok(Self::Uci { cmd })
                } else {
                    Err(e)
                }
            }
        }
    }
}
