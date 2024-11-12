/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

/// Commands to be sent to the engine, and how to parse them.
mod cli;
/// Code related to the engine's functionality, such as user input handling.
mod engine;
/// Evaluation of chess positions.
mod eval;
/// Piece-Square tables.
mod psqt;
/// Types and utilities for rating how good/bad a position is.
mod score;
/// Main engine logic; all search related code.
mod search;
/// Hash table for storing data during search.
mod ttable;
/// Constants and magic numbers that can be tuned to tweak engine performance.
mod tune;
/// Misc utility functions, constants, and types.
mod utils;

/// Board representation and move generation.
mod board {
    /// All things related to Bitboards.
    mod bitboard;
    /// High-level abstraction of the game of chess, including movable pieces, legality checks, game state, etc.
    mod game;
    /// All code related to generating moves (legal and pseudo-legal) for pieces on a board.
    mod movegen;
    /// Enums and structs for modeling the movement of a piece on a chessboard.
    mod moves;
    /// Utility function for performance testing.
    mod perft;
    /// Enums for piece kinds, colors, and a struct for a chess piece.
    mod piece;
    /// A chessboard, complete with piece placements, turn counters, and game state information.
    mod position;
    /// Pseudo-random number generation, written to be usable in `const` settings.
    ///
    /// Primarily for Zobrist hashing and magic generation.
    mod prng;
    /// Squares on a chessboard (including files and ranks).
    mod square;
    /// Misc utility functions and constants, as well as magic bitboard generation.
    mod utils;
    /// Zobrist keys for hashing chess positions.
    mod zobrist;

    pub use bitboard::*;
    pub use game::*;
    pub use movegen::*;
    pub use moves::*;
    pub use perft::*;
    pub use piece::*;
    pub use position::*;
    pub use prng::*;
    pub use square::*;
    pub use utils::*;
    pub use zobrist::*;
}

pub use board::*;
pub use cli::*;
pub use engine::*;
use eval::*;
use psqt::*;
use score::*;
use search::*;
use ttable::*;
use utils::*;
