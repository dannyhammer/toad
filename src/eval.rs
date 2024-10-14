/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::fmt;

use chessie::{Board, Color, File, Game, PieceKind, Rank, Square};

use crate::{Psqt, Score};

/// Initial material value of all pieces in a standard setup.
const INITIAL_MATERIAL_VALUE: i32 = value_of(PieceKind::Pawn) * 16
    + value_of(PieceKind::Knight) * 4
    + value_of(PieceKind::Bishop) * 4
    + value_of(PieceKind::Rook) * 4
    + value_of(PieceKind::Queen) * 2;

/// Encapsulates the logic of scoring a chess position.
///
/// Generally, a high score is good for White, and a low score is good for Black.
/// However, during a negamax search, positions must be evaluated from the side-to-move's perspective.
/// That is, if it is Black's turn, a "good" evaluation for Black will be a positive number.
#[derive(Debug, Clone)]
pub struct Evaluator<'a> {
    /// The game whose position to evaluate.
    game: &'a Game,

    /// Percentage of game completion, in the range `[0, 100]`.
    ///
    /// Higher number means fewer pieces are on the board
    pub(crate) endgame_weight: i32,
}

impl<'a> Evaluator<'a> {
    /// Construct a new [`Evaluator`], computing any important metadata.
    #[inline(always)]
    pub fn new(game: &'a Game) -> Self {
        Self {
            game,
            endgame_weight: endgame_weight(game),
        }
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
    fn eval_for(&self, color: Color) -> Score {
        // By default, both scores are draws.
        let mut mg = Score::DRAW;
        let mut eg = Score::DRAW;

        // Iterate over every occupied square
        for (square, piece) in self.game.board() {
            let (mg_psqt_score, eg_psqt_score) = Psqt::evals(piece, square);

            // Flip scores appropriately to evaluate from `color`'s perspective
            if color == piece.color() {
                mg += mg_psqt_score;
                eg += eg_psqt_score;
            } else {
                mg -= mg_psqt_score;
                eg -= eg_psqt_score;
            }
        }

        mg.lerp(eg, self.endgame_weight)
    }

    /// Fetches the value for the piece on the specified square, if one exists.
    ///
    /// Only used when printing the evaluator
    #[inline(always)]
    fn value_at(&self, square: Square) -> Option<Score> {
        self.game.piece_at(square).map(|piece| {
            let (mg, eg) = Psqt::evals(piece, square);
            mg.lerp(eg, self.endgame_weight) * piece.color().negation_multiplier() as i32
        })
    }
}

impl fmt::Display for Evaluator<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let color = self.game.side_to_move();

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
                    let s = if val > Score::DRAW {
                        format!("+{}", val.normalize())
                    } else {
                        format!("{}", val.normalize())
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

        let score = self.eval_for(color);

        let winning_side = if score > Score::DRAW {
            Some(color)
        } else if score < Score::DRAW {
            Some(color.opponent())
        } else {
            None
        };

        writeln!(f, "\n\nEndgame: {}%", self.endgame_weight)?;
        writeln!(
            f,
            "Winning side: {}",
            winning_side.map(|c| c.name()).unwrap_or("N/A")
        )?;
        writeln!(f, "Score: {score}")?;

        Ok(())
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
        PieceKind::King => 0, // King is invaluable, but 0 is easier to work with in computations
    }
}

/*
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
 */

/// Counts the material value of all pieces on the board
///
/// Does NOT count the material of the King, as it cannot be removed from the board.
#[inline(always)]
fn material_remaining(board: &Board) -> i32 {
    PieceKind::all_except_king()
        .into_iter()
        .fold(0, |score, kind| {
            score + board.kind(kind).population() as i32 * value_of(kind)
        })
}

/*
/// Counts the material value of the specified piece kind/color on the board.
#[inline(always)]
const fn count_material_of(board: &Board, color: Color, kind: PieceKind) -> i32 {
    let pieces = board.piece_parts(color, kind);

    (pieces.population() as i32) * value_of(kind)
}
 */

/// Divides the original material value of the board by the current material value, yielding an `i32` in the range `[0, 100]`
///
/// Lower numbers are closer to the beginning of the game. Higher numbers are closer to the end of the game.
///
/// The King is ignored when performing this calculation.
#[inline(always)]
fn endgame_weight(board: &Board) -> i32 {
    let remaining = INITIAL_MATERIAL_VALUE - material_remaining(board);
    (remaining * 100 / INITIAL_MATERIAL_VALUE * 100) / 100
}
