/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{
    fmt,
    ops::{Deref, DerefMut},
};

use crate::{Color, Game, Piece, PieceKind, Score, SmallDisplayTable, Square, Table, Variant};

/// Piece-Square tables copied from [PeSTO](https://www.chessprogramming.org/PeSTO%27s_Evaluation_Function#Source_Code)
#[rustfmt::skip]
const PAWN_MG: Psqt = Psqt::new(PieceKind::Pawn, MG_PIECE_VALUES,[
      0,   0,   0,   0,   0,   0,  0,   0,
     98, 134,  61,  95,  68, 126, 34, -11,
     -6,   7,  26,  31,  65,  56, 25, -20,
    -14,  13,   6,  21,  23,  12, 17, -23,
    -27,  -2,  -5,  12,  17,   6, 10, -25,
    -26,  -4,  -4, -10,   3,   3, 33, -12,
    -35,  -1, -20, -23, -15,  24, 38, -22,
      0,   0,   0,   0,   0,   0,  0,   0,
]);

#[rustfmt::skip]
const PAWN_EG: Psqt = Psqt::new(PieceKind::Pawn, EG_PIECE_VALUES, [
      0,   0,   0,   0,   0,   0,   0,   0,
    178, 173, 158, 134, 147, 132, 165, 187,
     94, 100,  85,  67,  56,  53,  82,  84,
     32,  24,  13,   5,  -2,   4,  17,  17,
     13,   9,  -3,  -7,  -7,  -8,   3,  -1,
      4,   7,  -6,   1,   0,  -5,  -1,  -8,
     13,   8,   8,  10,  13,   0,   2,  -7,
      0,   0,   0,   0,   0,   0,   0,   0,
]);

#[rustfmt::skip]
const KNIGHT_MG: Psqt = Psqt::new(PieceKind::Knight, MG_PIECE_VALUES, [
    -167, -89, -34, -49,  61, -97, -15, -107,
     -73, -41,  72,  36,  23,  62,   7,  -17,
     -47,  60,  37,  65,  84, 129,  73,   44,
      -9,  17,  19,  53,  37,  69,  18,   22,
     -13,   4,  16,  13,  28,  19,  21,   -8,
     -23,  -9,  12,  10,  19,  17,  25,  -16,
     -29, -53, -12,  -3,  -1,  18, -14,  -19,
    -105, -21, -58, -33, -17, -28, -19,  -23,
]);

#[rustfmt::skip]
const KNIGHT_EG: Psqt = Psqt::new(PieceKind::Knight, EG_PIECE_VALUES, [
    -58, -38, -13, -28, -31, -27, -63, -99,
    -25,  -8, -25,  -2,  -9, -25, -24, -52,
    -24, -20,  10,   9,  -1,  -9, -19, -41,
    -17,   3,  22,  22,  22,  11,   8, -18,
    -18,  -6,  16,  25,  16,  17,   4, -18,
    -23,  -3,  -1,  15,  10,  -3, -20, -22,
    -42, -20, -10,  -5,  -2, -20, -23, -44,
    -29, -51, -23, -15, -22, -18, -50, -64,
]);

#[rustfmt::skip]
const BISHOP_MG: Psqt = Psqt::new(PieceKind::Bishop, MG_PIECE_VALUES, [
    -29,   4, -82, -37, -25, -42,   7,  -8,
    -26,  16, -18, -13,  30,  59,  18, -47,
    -16,  37,  43,  40,  35,  50,  37,  -2,
     -4,   5,  19,  50,  37,  37,   7,  -2,
     -6,  13,  13,  26,  34,  12,  10,   4,
      0,  15,  15,  15,  14,  27,  18,  10,
      4,  15,  16,   0,   7,  21,  33,   1,
    -33,  -3, -14, -21, -13, -12, -39, -21,
]);

#[rustfmt::skip]
const BISHOP_EG: Psqt = Psqt::new(PieceKind::Bishop, EG_PIECE_VALUES, [
    -14, -21, -11,  -8, -7,  -9, -17, -24,
     -8,  -4,   7, -12, -3, -13,  -4, -14,
      2,  -8,   0,  -1, -2,   6,   0,   4,
     -3,   9,  12,   9, 14,  10,   3,   2,
     -6,   3,  13,  19,  7,  10,  -3,  -9,
    -12,  -3,   8,  10, 13,   3,  -7, -15,
    -14, -18,  -7,  -1,  4,  -9, -15, -27,
    -23,  -9, -23,  -5, -9, -16,  -5, -17,
]);

#[rustfmt::skip]
const ROOK_MG: Psqt = Psqt::new(PieceKind::Rook, MG_PIECE_VALUES, [
     32,  42,  32,  51, 63,  9,  31,  43,
     27,  32,  58,  62, 80, 67,  26,  44,
     -5,  19,  26,  36, 17, 45,  61,  16,
    -24, -11,   7,  26, 24, 35,  -8, -20,
    -36, -26, -12,  -1,  9, -7,   6, -23,
    -45, -25, -16, -17,  3,  0,  -5, -33,
    -44, -16, -20,  -9, -1, 11,  -6, -71,
    -19, -13,   1,  17, 16,  7, -37, -26,
]);

#[rustfmt::skip]
const ROOK_EG: Psqt = Psqt::new(PieceKind::Rook, EG_PIECE_VALUES, [
    13, 10, 18, 15, 12,  12,   8,   5,
    11, 13, 13, 11, -3,   3,   8,   3,
     7,  7,  7,  5,  4,  -3,  -5,  -3,
     4,  3, 13,  1,  2,   1,  -1,   2,
     3,  5,  8,  4, -5,  -6,  -8, -11,
    -4,  0, -5, -1, -7, -12,  -8, -16,
    -6, -6,  0,  2, -9,  -9, -11,  -3,
    -9,  2,  3, -1, -5, -13,   4, -20,
]);

#[rustfmt::skip]
const QUEEN_MG: Psqt = Psqt::new(PieceKind::Queen, MG_PIECE_VALUES, [
    -28,   0,  29,  12,  59,  44,  43,  45,
    -24, -39,  -5,   1, -16,  57,  28,  54,
    -13, -17,   7,   8,  29,  56,  47,  57,
    -27, -27, -16, -16,  -1,  17,  -2,   1,
     -9, -26,  -9, -10,  -2,  -4,   3,  -3,
    -14,   2, -11,  -2,  -5,   2,  14,   5,
    -35,  -8,  11,   2,   8,  15,  -3,   1,
     -1, -18,  -9,  10, -15, -25, -31, -50,
]);

#[rustfmt::skip]
const QUEEN_EG: Psqt = Psqt::new(PieceKind::Queen, EG_PIECE_VALUES, [
     -9,  22,  22,  27,  27,  19,  10,  20,
    -17,  20,  32,  41,  58,  25,  30,   0,
    -20,   6,   9,  49,  47,  35,  19,   9,
      3,  22,  24,  45,  57,  40,  57,  36,
    -18,  28,  19,  47,  31,  34,  39,  23,
    -16, -27,  15,   6,   9,  17,  10,   5,
    -22, -23, -30, -16, -16, -23, -36, -32,
    -33, -28, -22, -43,  -5, -32, -20, -41,
]);

#[rustfmt::skip]
const KING_MG: Psqt = Psqt::new(PieceKind::King, MG_PIECE_VALUES, [
    -65,  23,  16, -15, -56, -34,   2,  13,
     29,  -1, -20,  -7,  -8,  -4, -38, -29,
     -9,  24,   2, -16, -20,   6,  22, -22,
    -17, -20, -12, -27, -30, -25, -14, -36,
    -49,  -1, -27, -39, -46, -44, -33, -51,
    -14, -14, -22, -46, -44, -30, -15, -27,
      1,   7,  -8, -64, -43, -16,   9,   8,
    -15,  36,  12, -54,   8, -28,  24,  14,
]);

#[rustfmt::skip]
const KING_EG: Psqt = Psqt::new(PieceKind::King, EG_PIECE_VALUES, [
    -74, -35, -18, -18, -11,  15,   4, -17,
    -12,  17,  14,  17,  17,  38,  23,  11,
     10,  17,  23,  15,  20,  45,  44,  13,
     -8,  22,  24,  27,  26,  33,  26,   3,
    -18,  -4,  21,  24,  27,  23,   9, -11,
    -19,  -3,  11,  21,  23,  16,   7,  -9,
    -27, -11,   4,  13,  14,   4,  -5, -17,
    -53, -34, -21, -11, -28, -14, -24, -43
]);

/// Piece values copied from [PeSTO](https://www.chessprogramming.org/PeSTO%27s_Evaluation_Function#Source_Code)
const MG_PIECE_VALUES: [i32; PieceKind::COUNT] = [82, 337, 365, 477, 1025, 0];
const EG_PIECE_VALUES: [i32; PieceKind::COUNT] = [94, 281, 297, 512, 936, 0];
const GAME_PHASE_INC: [i32; Piece::COUNT] = [
    0, 1, 1, 2, 4, 0, // White
    0, 1, 1, 2, 4, 0, // Black
];

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Evaluator {}

impl Evaluator {
    /// Evaluate the current position.
    ///
    /// Internally uses [PeSTO's evaluation function](https://www.chessprogramming.org/PeSTO%27s_Evaluation_Function#Source_Code).
    #[inline(always)]
    pub fn eval_for<V: Variant>(game: &Game<V>, color: Color) -> Score {
        let mut mg = [Score::DRAW; Color::COUNT];
        let mut eg = [Score::DRAW; Color::COUNT];
        let mut phase = 0;

        // Evaluate each piece
        for (square, piece) in game.iter() {
            let (mg_psqt, eg_psqt) = Psqt::evals(piece, square);
            mg[piece.color()] += mg_psqt;
            eg[piece.color()] += eg_psqt;
            phase += GAME_PHASE_INC[piece];
        }

        // Tapered eval
        let mg_score = mg[color] - mg[color.opponent()];
        let eg_score = eg[color] - eg[color.opponent()];
        let mg_phase = Score::from(phase.min(24)); // in case of early promotion
        let eg_phase = Score::from(24 - mg_phase);

        // Interpolate the score
        (mg_score * mg_phase + eg_score * eg_phase) / 24
    }

    /// Divides the original material value of the board by the current material value, yielding an `i32` in the range `[0, 100]`
    ///
    /// Lower numbers are closer to the beginning of the game. Higher numbers are closer to the end of the game.
    ///
    /// The King and Pawns are ignored when performing this calculation.
    #[inline(always)]
    pub fn endgame_weight<V: Variant>(game: &Game<V>) -> u8 {
        let mut phase = 0;
        for (_, piece) in game.iter() {
            phase += GAME_PHASE_INC[piece];
        }

        (100 - ((phase * 100 / 24 * 100) / 100)) as u8
    }

    /*
    /// Counts the material value of all pieces on the board
    ///
    /// The King is not included in this count
    #[inline(always)]
    pub fn material(&self) -> i32 {
        self.material[Color::White.index()] + self.material[Color::Black.index()]
    }
     */
}

/// A [Piece-Square Table](https://www.chessprogramming.org/Piece-Square_Tables) for use in evaluation.
#[derive(Debug, Clone, Copy)]
pub struct Psqt(Table<Score>);

impl Psqt {
    /// Fetch the mid-game and end-game evaluations for a `piece` at `square`.
    #[inline(always)]
    pub fn evals(piece: Piece, square: Square) -> (Score, Score) {
        let (mg, eg) = Self::get_tables_for(piece.kind());

        // Get the rank-relative square for this piece
        let square = square.rank_relative_to(piece.color());

        (mg.get(square), eg.get(square))
    }

    /// Fetch the Piece-Square Tables (middle-game and end-game) for the provided [`PieceKind`].
    #[inline(always)]
    pub fn get_tables_for<'a>(kind: PieceKind) -> (&'a Self, &'a Self) {
        match kind {
            PieceKind::Pawn => (&PAWN_MG, &PAWN_EG),
            PieceKind::Knight => (&KNIGHT_MG, &KNIGHT_EG),
            PieceKind::Bishop => (&BISHOP_MG, &BISHOP_EG),
            PieceKind::Rook => (&ROOK_MG, &ROOK_EG),
            PieceKind::Queen => (&QUEEN_MG, &QUEEN_EG),
            PieceKind::King => (&KING_MG, &KING_EG),
        }
    }

    /// Creates a new [`Psqt`] for the provided [`PieceKind`] and array of values.
    const fn new(
        kind: PieceKind,
        piece_values: [i32; PieceKind::COUNT],
        psqt: [i32; Square::COUNT],
    ) -> Self {
        let mut flipped = [Score::DRAW; Square::COUNT];

        let mut i = 0;
        while i < psqt.len() {
            // Flip the rank, not the file, so it can be used from White's perspective without modification
            // Also add in the value of this piece
            flipped[i] = Score::from_int(psqt[i ^ 56] + piece_values[kind.index()]);
            // flipped[i] = value_of(kind); // Functions like a material-only eval
            i += 1;
        }

        Self(Table::new(flipped))
    }

    /// Get the value of this PSQT at the provided square.
    #[inline(always)]
    pub const fn get(&self, square: Square) -> Score {
        *self.0.get(square)
    }

    /// Get the value of this PSQT at the provided square, relative to `color`.
    #[inline(always)]
    pub const fn get_relative(&self, square: Square, color: Color) -> Score {
        self.get(square.rank_relative_to(color))
    }
}

impl Deref for Psqt {
    type Target = Table<Score>;
    /// A [`Psqt`] dereferences to its inner [`Table`].
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Psqt {
    /// A [`Psqt`] dereferences to its inner [`Table`].
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl fmt::Display for Psqt {
    /// Printing a [`Psqt`] will display it in the same way it is written in the code (White's perspective).
    ///
    /// If the alternate formatter is used (`#`), it will print as if from Black's perspective.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let color = Color::from_bool(f.alternate());
        let table = SmallDisplayTable::from_fn(|sq| [self.get_relative(sq, color)]);
        write!(f, "{table:>3}")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_is_correct_for_colors() {
        // Over every possible square
        for square in Square::iter() {
            // For every piece
            for kind in PieceKind::all() {
                // Assert that White's PSQT eval is equal to Black's equivalent PSQT eval
                let white = Psqt::evals(Piece::new(Color::White, kind), square);
                let black = Psqt::evals(
                    Piece::new(Color::Black, kind),
                    square.rank_relative_to(Color::Black),
                );

                assert_eq!(
                    white,
                    black,
                    "{} on {square}: {white:?} (white) != {black:?} (black)",
                    kind.name()
                );
            }
        }
    }
}
