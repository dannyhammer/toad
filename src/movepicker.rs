/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use arrayvec::ArrayVec;

use crate::{Color, Move, MoveList, Piece, PieceKind, MAX_NUM_MOVES};

// enum Order {
//     HashMove,
//     Captures,
//     Quiets,
//     Done,
// }

pub struct MovePicker {
    moves: MoveList,
    scores: ArrayVec<i32, MAX_NUM_MOVES>,
    current: usize,
    // pub skip_quiets: bool,
}

impl MovePicker {
    pub fn new(
        moves: MoveList,
        // game: &Game<V>,
        // history: &HistoryTable,
        // tt_move: Option<Move>,
        score_fn: impl Fn(&Move) -> i32,
    ) -> Self {
        let mut scores = ArrayVec::default();

        for mv in moves.iter() {
            scores.push(-score_fn(mv));
        }

        /*
        // TT move should be looked at first, so assign it the best possible score and immediately exit.
        if tt_move.is_some_and(|tt_mv| tt_mv == *mv) {
            return Score::new(i32::MIN);
        }

        // Safe unwrap because we can't move unless there's a piece at `from`
        let piece = game.piece_at(mv.from()).unwrap();
        let to = mv.to();
        let mut score = Score::BASE_MOVE_SCORE;

        // Apply history bonus to quiets
        if mv.is_quiet() {
            score += self.history[piece][to];
        } else
        // Capturing a high-value piece with a low-value piece is a good idea
        if let Some(victim) = game.piece_at(to) {
            score += MVV_LVA[piece][victim];
        }

        -score // We're sorting, so a lower number is better
         */

        /*
        for (i, &mv) in moves.iter().enumerate() {
            // TT move should have highest priority
            if Some(mv) == tt_move {
                // scores[i] = -Score::new(i32::MIN);
                scores[i] = i32::MAX;
                best_index = i;
                continue;
            }

            // Safe unwrap because we can't move unless there's a piece at `from`
            let piece = game.piece_at(mv.from()).unwrap();
            let to = mv.to();

            // Apply history bonus to quiets
            if mv.is_quiet() {
                scores[i] += history[piece][to] as i32;
            } else
            // Capturing a high-value piece with a low-value piece is a good idea
            if let Some(victim) = game.piece_at(to) {
                scores[i] += MVV_LVA[piece][victim];
                // scores[i] += (10 * victim.kind().value() - piece.kind().value()) << 16
            }

            if scores[i] > scores[best_index] {
                best_index = i;
            }
        }

        if best_index > 0 {
            // Put the best move at the front of the list
            moves.swap(0, best_index);
            scores.swap(0, best_index);
        }
          */

        Self {
            moves,
            scores,
            current: 0,
            // skip_quiets: false,
        }
    }

    // pub fn moves_so_far(&self) -> &[Move] {
    //     &self.moves[..(self.current - 1)]
    // }
}

impl Iterator for MovePicker {
    type Item = (Move, i32);

    fn next(&mut self) -> Option<Self::Item> {
        // No more moves left
        if self.current >= self.moves.len() {
            return None;
        }

        // Fetch the current best
        let mut best_index = self.current;
        let mut best_score = self.scores[best_index];

        // Find the index of the next highest score
        for i in (self.current + 1)..self.moves.len() {
            if self.scores[i] > best_score {
                best_index = i;
                best_score = self.scores[i];
            }
        }

        // Swap, if necessary
        if best_index != self.current {
            self.moves.swap(self.current, best_index);
            self.scores.swap(self.current, best_index);
        }

        // Get the move/score at this index
        let mv = self.moves[self.current];
        let score = self.scores[self.current];

        // Increment for next call
        self.current += 1;

        Some((mv, score))
    }
}

/*
struct Picker<T, const CAP: usize> {
    things: ArrayVec<T, CAP>,
    scores: ArrayVec<i32, CAP>,
    current: usize,
}

impl<T, const CAP: usize> Picker<T, CAP> {
    fn new(mut things: ArrayVec<T, CAP>, mut scores: ArrayVec<i32, CAP>) -> Self {
        let mut best_index = 0;
        for i in 0..things.len() {
            if scores[i] > scores[best_index] {
                best_index = i;
            }
        }

        things.swap(best_index, 0);
        scores.swap(best_index, 0);

        Self {
            things,
            scores,
            current: 0,
        }
    }
}

impl<T: Copy + std::fmt::Debug, const CAP: usize> Iterator for Picker<T, CAP> {
    type Item = (T, i32);

    fn next(&mut self) -> Option<Self::Item> {
        if self.current == self.things.len() {
            return None;
        }
        // let index = self.current;
        let item = self.things[self.current];
        let score = self.scores[self.current];

        self.current += 1;
        for i in (self.current + 1)..self.things.len() {
            if self.scores[i] > self.scores[self.current] {
                self.things.swap(self.current, i);
                self.scores.swap(self.current, i);
            }
        }

        Some((item, score))
    }
}
 */

/// Values are obtained from here: <https://www.chessprogramming.org/Simplified_Evaluation_Function>
const MVV_LVA_PIECE_VALUES: [i32; PieceKind::COUNT] = [100, 320, 330, 500, 900, 0];

/// This table represents values for [MVV-LVA](https://www.chessprogramming.org/MVV-LVA) move ordering.
///
/// It is indexed by `[attacker][victim]`, and yields a "score" that is used when sorting moves.
///
/// The following table is produced:
/// ```text
///                     VICTIM
/// A       P     N     B     R     Q     K     
/// T    +---------------------------------+
/// T   P| 900   3100  3200  4900  8900  0     
/// A   N| 680   2880  2980  4680  8680  0     
/// C   B| 670   2870  2970  4670  8670  0     
/// K   R| 500   2700  2800  4500  8500  0     
/// E   Q| 100   2300  2400  4100  8100  0     
/// R   K| 1000  3200  3300  5000  9000  0     
/// ```
///
/// Note that the actual table is different, as it has size `12x12` instead of `6x6`
/// to account for the fact that castling is denoted as `KxR`.
/// The values are also all left-shifted by 16 bits, to ensure that captures are ranked above quiets in all cases.
///
/// See [`print_mvv_lva_table`] to display this table.
pub const MVV_LVA: [[i32; Piece::COUNT]; Piece::COUNT] = {
    let mut matrix = [[0; Piece::COUNT]; Piece::COUNT];
    let count = Piece::COUNT;

    let mut attacker = 0;
    while attacker < count {
        let mut victim = 0;
        let atk_color = Color::from_bool(attacker < PieceKind::COUNT);

        while victim < count {
            let atk = PieceKind::from_bits_unchecked(attacker as u8 % 6);
            let vtm = PieceKind::from_bits_unchecked(victim as u8 % 6);

            let vtm_color = Color::from_bool(victim < PieceKind::COUNT);

            // Remove scores for capturing the King and friendly pieces (KxR for castling)
            let can_capture = (atk_color.index() != vtm_color.index()) // Different colors
                && victim != count - 1 // Can't capture White King
                && victim != PieceKind::COUNT - 1; // Can't capture Black King

            // Rustic's way of doing things; Arbitrary increasing numbers for capturing pairs
            // bench: 27609398 nodes 5716479 nps
            // let score = (victim * 10 + (count - attacker)) as i32;

            // Default MVV-LVA except that the King is assigned a value of 0 if he is attacking
            // bench: 27032804 nodes 8136592 nps
            let score = 10 * MVV_LVA_PIECE_VALUES[vtm.index()] - MVV_LVA_PIECE_VALUES[atk.index()];

            // If the attacker is the King, the score is half the victim's value.
            // This encourages the King to attack, but not as strongly as other pieces.
            // bench: 27107011 nodes 5647285 nps
            // let score = if attacker == count - 1 {
            //     value_of(vtm) / 2
            // } else {
            //     // Standard MVV-LVA computation
            //     10 * value_of(vtm) - value_of(atk)
            // };

            // Shift the value by a large amount so that captures are always ranked very highly
            matrix[attacker][victim] = (score * can_capture as i32) << 16;
            victim += 1;
        }
        attacker += 1;
    }
    matrix
};

/// Utility function to print the MVV-LVA table
#[allow(dead_code)]
pub fn print_mvv_lva_table() {
    print!("\nX  ");
    for victim in Piece::all() {
        print!("{victim}     ");
    }
    print!("\n +");
    for _ in Piece::all() {
        print!("------");
    }
    println!("-+");
    for attacker in Piece::all() {
        print!("{attacker}| ");
        for victim in Piece::all() {
            let score = MVV_LVA[attacker][victim];
            print!("{score:<4}  ")
        }
        println!();
    }
}

/*
#[cfg(test)]
mod tests {

    use super::*;
    use crate::*;

    // #[test]
    // fn test_picker_order() {
    //     let things = ['c', 'b', 'a', 'd'];
    //     let scores = [3, 2, 1, 4];
    //     let picker = Picker::new(ArrayVec::from(things), scores.into());

    //     let list = picker.into_iter().collect::<Vec<_>>();
    //     assert_eq!(list, vec![('d', 4), ('c', 3), ('b', 2), ('a', 1)])
    // }

    /*
    #[test]
    fn test_capture_order() {
        let fen = "r3k2r/p1p1q1b1/bn3np1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
        let game: Game<Standard> = fen.parse().unwrap();

        let tt_move = Some(Move::new(Square::B2, Square::B3, MoveKind::Quiet));
        let history = HistoryTable::default();
        let moves = game.get_legal_moves();
        let mut picker = MovePicker::new(moves, &game, &history, tt_move);

        // Hash move comes first
        let (mv, score) = picker.next().unwrap();
        assert_eq!(mv, "b2b3", "Got score of {score}"); // BxB

        // MVV-LVA captures; will need to update once SEE is implemented
        // e2a6, f3f6, g2h3, e5g6, f3h3
        let (mv, score) = picker.next().unwrap();
        assert_eq!(mv, "e2a6", "Got score of {score}"); // BxB

        let (mv, score) = picker.next().unwrap();
        assert_eq!(mv, "f3f6", "Got score of {score}"); // QxN

        let (mv, score) = picker.next().unwrap();
        assert_eq!(mv, "g2h3", "Got score of {score}"); // PxP

        let (mv, score) = picker.next().unwrap();
        assert_eq!(mv, "e5g6", "Got score of {score}"); // NxP

        let (mv, score) = picker.next().unwrap();
        assert_eq!(mv, "f3h3", "Got score of {score}"); // QxP
    }

     */
}

 */
