/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::fmt;

use chessie::{Board, Color, File, Game, PieceKind, Rank, Square};

use crate::Score;

/// Encapsulates the logic of scoring a chess position.
///
/// Generally, a high score is good for White, and a low score is good for Black.
/// However, during a negamax search, positions must be evaluated from the side-to-move's perspective.
/// That is, if it is Black's turn, a "good" evaluation for Black will be a positive number.
pub struct Evaluator<'a> {
    /// The game whose position to evaluate.
    game: &'a Game,
}

impl<'a> Evaluator<'a> {
    /// Construct a new [`Evaluator`], computing any important metadata.
    #[inline(always)]
    pub fn new(game: &'a Game) -> Self {
        Self { game }
    }

    /// Evaluate this position from the side-to-move's perspective.
    ///
    /// A positive/high number is good for the side-to-move, while a negative number is better for the opponent.
    /// A score of 0 is considered equal.
    #[inline(always)]
    pub fn eval(self) -> Score {
        let stm = self.game.side_to_move();
        self.eval_for(stm)
    }

    /// Evaluate this position from `color`'s perspective.
    ///
    /// A positive/high number is good for the `color`, while a negative number is better for the opponent.
    /// A score of 0 is considered equal.
    #[inline(always)]
    fn eval_for(self, color: Color) -> Score {
        let material = material_difference(self.game, color);
        Score(material)
    }

    #[inline(always)]
    fn value_at(&self, square: Square) -> Option<i32> {
        self.game
            .piece_at(square)
            .map(|p| value_of(p.kind()) * p.color().negation_multiplier() as i32)
    }

    #[inline(always)]
    fn normalize(&self, value: i32) -> f32 {
        // let max = Score::MATE.0 as f32;
        value as f32 / 100.0
    }
}

/// Returns a value of the provided `PieceKind`.
///
/// Values are obtained from here: <https://www.chessprogramming.org/Simplified_Evaluation_Function>
#[inline(always)]
pub const fn value_of(kind: PieceKind) -> i32 {
    match kind {
        PieceKind::Pawn => 100,
        PieceKind::Knight => 320,
        PieceKind::Bishop => 330,
        PieceKind::Rook => 500,
        PieceKind::Queen => 900,
        PieceKind::King => 20_000,
    }
}

/// Computes the difference in material on the board.
///
/// If positive, `color` has more material.
/// If negative, `color.opponent()` has more material.
/// If zero, both sides have equal material.
#[inline(always)]
pub fn material_difference(board: &Board, color: Color) -> i32 {
    let mut friendly = 0;
    let mut enemy = 0;
    let opponent = color.opponent();

    for kind in PieceKind::all_except_king() {
        friendly += count_material_of(board, color, kind);

        enemy += count_material_of(board, opponent, kind);
    }

    friendly - enemy
}

/// Counts the material value of the specified piece kind/color on the board.
#[inline(always)]
const fn count_material_of(board: &Board, color: Color, kind: PieceKind) -> i32 {
    let pieces = board.piece_parts(color, kind);

    (pieces.population() as i32) * value_of(kind)
}

impl fmt::Display for Evaluator<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ranks = Rank::iter().rev();

        write!(f, "  +")?;
        for _ in File::iter() {
            write!(f, "-----+")?;
        }
        writeln!(f)?;
        for rank in ranks {
            write!(f, "{rank} |")?;

            // Step 1: Write the piece char
            for file in File::iter() {
                let square = Square::new(file, rank);
                let piece = self.game.piece_at(square);
                let piece_char = piece.map(|p| p.char()).unwrap_or(' ');
                write!(f, "  {piece_char}  |")?;
            }
            writeln!(f)?;
            write!(f, "  |")?;

            // Step 2: Write the contribution of that piece
            for file in File::iter() {
                let square = Square::new(file, rank);
                let score = if let Some(val) = self.value_at(square) {
                    let s = if val > 0 {
                        format!("+{}", self.normalize(val))
                    } else {
                        format!("{}", self.normalize(val))
                    };

                    format!("{s:^5}")
                } else {
                    String::from("     ")
                };
                write!(f, "{score}|")?;
            }

            writeln!(f)?;

            write!(f, "  +")?;
            for _ in File::iter() {
                write!(f, "-----+")?;
            }
            writeln!(f)?;
        }
        for file in File::iter() {
            write!(f, "     {file}")?;
        }
        Ok(())
    }
}
