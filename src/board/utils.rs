/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

/// FEN string for the starting position of chess.
pub const FEN_STARTPOS: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

/// A popular FEN string for debugging move generation.
pub const FEN_KIWIPETE: &str =
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";

/// Maximum possible number of moves in a given chess position.
///
/// Found [here](<https://www.chessprogramming.org/Chess_Position#cite_note-4>)
pub const MAX_NUM_MOVES: usize = 218;

/// Number of possible combinations of castling rights.
///
/// Used for Zobrist hashing.
pub const NUM_CASTLING_RIGHTS: usize = 16;
