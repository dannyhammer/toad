/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{fmt, ops::Deref};

use crate::{tune, Color, File, Game, Move, Piece, PieceKind, Rank, Square, Table, Variant};
/// Stores bonuses and penalties for moving a piece to a square.
///
/// Used to keep track of good/bad moves found during search.
#[derive(Debug)]
pub struct HistoryTable([Table<i16>; Piece::COUNT]);

impl HistoryTable {
    /// Clear the history table, removing all scores.
    #[inline(always)]
    pub fn clear(&mut self) {
        *self = Self::default();
    }

    /// Applies a bonus based on the history heuristic for the move.
    ///
    /// Uses the "history gravity" formula from <https://www.chessprogramming.org/History_Heuristic#History_Bonuses>
    #[inline(always)]
    pub fn update<V: Variant>(&mut self, game: &Game<V>, mv: &Move, bonus: i16) {
        // Safety: This is a move. There *must* be a piece at `from`.
        let piece = game.piece_at(mv.from()).unwrap();
        let to = mv.to();
        let current = self[piece][to];

        let max = tune::max_history_bonus!();
        let clamped = bonus.clamp(-max, max);

        // History gravity formula
        // Casting to i32 to prevent overflow
        let new = (current as i32 + clamped as i32
            - current as i32 * clamped.abs() as i32 / max as i32) as i16;

        self.0[piece].set(to, new);
    }
}

impl Default for HistoryTable {
    #[inline(always)]
    fn default() -> Self {
        Self([Table::splat(0); Piece::COUNT])
    }
}

impl Deref for HistoryTable {
    type Target = [Table<i16>; Piece::COUNT];
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl fmt::Display for HistoryTable {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use std::cmp::Ordering::*;
        const SPACER: char = '\t';

        for kind in PieceKind::all() {
            let white_piece = Piece::new(Color::White, kind);
            let black_piece = Piece::new(Color::Black, kind);
            // Table titles
            write!(f, "\t\t\t{:^5}\t\t", white_piece.name())?;
            write!(f, "{SPACER}")?;
            write!(f, "\t\t\t{:^5}", black_piece.name())?;
            writeln!(f)?;

            // Top line of a square
            write!(f, "  +")?;
            for _ in File::iter() {
                write!(f, "-----+")?;
            }
            write!(f, "{SPACER}")?;
            write!(f, "  +")?;
            for _ in File::iter() {
                write!(f, "-----+")?;
            }

            writeln!(f)?;
            for rank in Rank::iter().rev() {
                write!(f, "{rank} |")?;

                // Step 1: Write the sign of the score
                for file in File::iter() {
                    let square = Square::new(file, rank);
                    let sign = match self[white_piece][square].cmp(&0) {
                        Equal => ' ',
                        Greater => '+',
                        Less => '-',
                    };
                    write!(f, "{sign:^5}|")?;
                }
                write!(f, "{SPACER}")?;
                write!(f, "{rank} |")?;
                // Same for Black
                for file in File::iter() {
                    let square = Square::new(file, rank);
                    let sign = match self[black_piece][square].cmp(&0) {
                        Equal => ' ',
                        Greater => '+',
                        Less => '-',
                    };
                    write!(f, "{sign:^5}|")?;
                }
                writeln!(f)?;
                write!(f, "  |")?;

                // Step 2: Write the score
                for file in File::iter() {
                    let square = Square::new(file, rank);
                    let score = self[white_piece][square].abs();
                    let score = if score == 0 {
                        String::from("     ")
                    } else {
                        score.to_string()
                    };
                    write!(f, "{score:^5}|")?;
                }
                write!(f, "{SPACER}")?;
                write!(f, "  |")?;
                // Same for Black
                for file in File::iter() {
                    let square = Square::new(file, rank);
                    let score = self[black_piece][square].abs();
                    let score = if score == 0 {
                        String::from("     ")
                    } else {
                        score.to_string()
                    };
                    write!(f, "{score:^5}|")?;
                }

                writeln!(f)?;

                write!(f, "  +")?;
                for _ in File::iter() {
                    write!(f, "-----+")?;
                }
                write!(f, "{SPACER}")?;
                write!(f, "  +")?;
                for _ in File::iter() {
                    write!(f, "-----+")?;
                }
                writeln!(f)?;
            }
            for file in File::iter() {
                write!(f, "     {file}")?;
            }
            write!(f, "{SPACER}")?;
            for file in File::iter() {
                write!(f, "     {file}")?;
            }

            // Add an empty line between printing each table
            if kind != *PieceKind::all().last().unwrap() {
                writeln!(f, "\n")?;
            }
        }

        Ok(())
    }
}
