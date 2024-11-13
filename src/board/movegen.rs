/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use super::{Bitboard, Color, Rank, Square};

// Include the pre-generated magics
include!("magics.rs");

/// A table indexed by two squares that returns a Bitboard of a ray of squares between (exclusive) the indices.
const RAY_BETWEEN: [[Bitboard; Square::COUNT]; Square::COUNT] = {
    let mut rays = [[Bitboard::EMPTY_BOARD; Square::COUNT]; Square::COUNT];

    let mut i = 0;
    while i < Square::COUNT {
        let from = Square::from_index_unchecked(i);
        let mut j = 0;
        while j < QUEEN_DELTAS.len() {
            let (df, dr) = QUEEN_DELTAS[j];
            let mut ray = Bitboard::EMPTY_BOARD; // Do not include `from`
            let mut to = from;

            while let Some(shifted) = to.offset(df, dr) {
                ray = ray.or(shifted.bitboard());
                to = shifted;
                // Do not include `to`
                rays[from.index()][to.index()] = ray.xor(to.bitboard());
            }

            j += 1;
        }

        i += 1;
    }

    rays
};

/// A table indexed by two squares that returns a Bitboard of a ray containing all squares between the indices.
const RAY_CONTAINING: [[Bitboard; Square::COUNT]; Square::COUNT] = {
    let mut rays = [[Bitboard::EMPTY_BOARD; Square::COUNT]; Square::COUNT];

    let mut i = 0;
    while i < Square::COUNT {
        let from = Square::from_index_unchecked(i);
        let mut j = 0;
        let fr = from.rank();
        let ff = from.file();
        while j < Square::COUNT {
            let to = Square::from_index_unchecked(j);
            let tr = to.rank();
            let tf = to.file();
            if from.inner() == to.inner() {
                rays[from.index()][to.index()] = Bitboard::from_square(from);
            } else if fr.inner() == tr.inner() {
                rays[from.index()][to.index()] = Bitboard::from_rank(fr);
            } else if ff.inner() == tf.inner() {
                rays[from.index()][to.index()] = Bitboard::from_file(ff);
            } else {
                // To check if these lie on a diagonal, compute (y1 - y2) / (x1 - x2)
                let file_diff = from.file().inner() as i32 - to.file().inner() as i32;
                let rank_diff = from.rank().inner() as i32 - to.rank().inner() as i32;

                // Checked division
                let diff = if rank_diff != 0 {
                    file_diff / rank_diff
                } else {
                    0
                };

                if diff == 1 {
                    rays[from.index()][to.index()] =
                        rays[from.index()][to.index()].or(from.bitboard());
                    // I'm too lazy to figure out the proper math, so I'm just going to cast rays in the diagonals
                    let mut tmp = from;
                    // First ray goes Northeast
                    while let Some(shifted) = tmp.offset(1, 1) {
                        rays[from.index()][to.index()] =
                            rays[from.index()][to.index()].or(shifted.bitboard());
                        tmp = shifted;
                    }
                    tmp = from;
                    // Second ray goes Southwest
                    // I'm intentionally not resetting tmp here, so that the square for `from` gets OR'd in
                    while let Some(shifted) = tmp.offset(-1, -1) {
                        rays[from.index()][to.index()] =
                            rays[from.index()][to.index()].or(shifted.bitboard());
                        tmp = shifted;
                    }
                } else if diff == -1 {
                    rays[from.index()][to.index()] =
                        rays[from.index()][to.index()].or(from.bitboard());
                    // Do it again, in the Northwest / Southeast directions
                    let mut tmp = from;
                    while let Some(shifted) = tmp.offset(-1, 1) {
                        rays[from.index()][to.index()] =
                            rays[from.index()][to.index()].or(shifted.bitboard());
                        tmp = shifted;
                    }
                    tmp = from;
                    while let Some(shifted) = tmp.offset(1, -1) {
                        rays[from.index()][to.index()] =
                            rays[from.index()][to.index()].or(shifted.bitboard());
                        tmp = shifted;
                    }
                }
            }
            j += 1;
        }
        i += 1;
    }

    rays
};

const KNIGHT_ATTACKS: [Bitboard; 64] = generate_knight_mobility();
const ROOK_ATTACKS: [Bitboard; 64] = generate_rook_mobility();
const BISHOP_ATTACKS: [Bitboard; 64] = generate_bishop_mobility();
const KING_ATTACKS: [Bitboard; 64] = generate_king_mobility();
const WHITE_PAWN_PUSHES: [Bitboard; 64] = generate_pawn_pushes(Color::White);
const BLACK_PAWN_PUSHES: [Bitboard; 64] = generate_pawn_pushes(Color::Black);
const WHITE_PAWN_ATTACKS: [Bitboard; 64] = generate_pawn_attacks(Color::White);
const BLACK_PAWN_ATTACKS: [Bitboard; 64] = generate_pawn_attacks(Color::Black);

/// Deltas for the movement of the Queen.
const QUEEN_DELTAS: [(i8, i8); 8] = [
    /* Rook */
    (1, 0),
    (0, -1),
    (-1, 0),
    (0, 1),
    /* Bishop */
    (1, 1),
    (1, -1),
    (-1, -1),
    (-1, 1),
];

/// Deltas for the movement of the Rook.
const ROOK_DELTAS: [(i8, i8); 4] = [
    QUEEN_DELTAS[0],
    QUEEN_DELTAS[1],
    QUEEN_DELTAS[2],
    QUEEN_DELTAS[3],
];

/// Deltas for the movement of the Bishop.
const BISHOP_DELTAS: [(i8, i8); 4] = [
    QUEEN_DELTAS[4],
    QUEEN_DELTAS[5],
    QUEEN_DELTAS[6],
    QUEEN_DELTAS[7],
];

/// Deltas for the movement of the Knight.
const KNIGHT_DELTAS: [(i8, i8); 8] = [
    (1, 2),
    (1, -2),
    (2, 1),
    (2, -1),
    (-1, 2),
    (-1, -2),
    (-2, 1),
    (-2, -1),
];

/*
/// Computes a [`Bitboard`] of all squares that `color` Pawns can push to.
///
/// These are *not* capture moves. Only forward pushes and double-pushes.
#[inline(always)]
pub fn pawn_push_map(board: &Board, color: Color) -> Bitboard {
    let pawns = board.pawns(color);
    let empty = board.empty();

    // By default, all pawns can push forward once if they are not blocked
    let single_pushes = pawns.forward_by(color, 1) & empty;

    // Any pawn that started on the 2nd rank and was able to push once can also push again, if unblocked
    let double_pushes = (Bitboard::third_rank(color) & single_pushes).forward_by(color, 1) & empty;

    single_pushes | double_pushes
}
 */

/*
/// Fetch the default, pseudo-legal attacks for `piece` at `square`, given `blockers`.
///
/// Note: For Pawns, this retrieves only the Pawn's _attacks_, not their pushes.
/// This is because we call this function internally when generating legal moves to create
/// an "attack map" of all attacks possible, indexable by a [`Square`].
/// This map is then used to determine if a square is "safe" for the King to move to.
/// If Pawn pushes were included in this "attack map", then the King would not be able to
/// move to squares otherwise be safe, as the move generator would think that a Pawn's
/// threat of pushing could check the King.
#[inline(always)]
pub const fn attacks_for(piece: Piece, square: Square, blockers: Bitboard) -> Bitboard {
    match piece.kind() {
        PieceKind::Pawn => pawn_attacks(square, piece.color()),
        PieceKind::Knight => knight_attacks(square),
        PieceKind::Bishop => bishop_attacks(square, blockers),
        PieceKind::Rook => rook_attacks(square, blockers),
        PieceKind::Queen => queen_attacks(square, blockers),
        PieceKind::King => king_attacks(square),
    }
}
  */

/// Fetches a [`Bitboard`] with all of the bits along the ray between `from` and `to` (exclusive) set to `1`.
///
/// # Example
/// ```
/// # use toad::*;
/// assert_eq!(ray_between(Square::A1, Square::A8), Bitboard::FILE_A ^ Square::A1 ^ Square::A8);
/// ```
#[inline(always)]
pub const fn ray_between(from: Square, to: Square) -> Bitboard {
    RAY_BETWEEN[from.index()][to.index()]
}

/// Fetches a [`Bitboard`] with all of the bits along the ray containing `from` and `to` set to `1`.
///
/// # Example
/// ```
/// # use toad::*;
/// assert_eq!(ray_containing(Square::A3, Square::A5), Bitboard::FILE_A);
/// ```
#[inline(always)]
pub const fn ray_containing(from: Square, to: Square) -> Bitboard {
    RAY_CONTAINING[from.index()][to.index()]
}

/// Computes the possible moves for a Rook at a given [`Square`] with the provided blockers.
///
/// This will yield a [`Bitboard`] that allows the Rook to capture the first blocker.
#[inline(always)]
pub const fn rook_attacks(square: Square, blockers: Bitboard) -> Bitboard {
    Bitboard::new(ROOK_MOVES[magic_index(&ROOK_MAGICS[square.index()], blockers)])
}

/// Computes the (unblocked) moves for a Rook at a given [`Square`].
#[inline(always)]
pub const fn rook_rays(square: Square) -> Bitboard {
    ROOK_ATTACKS[square.index()]
}

/// Computes the possible moves for a Bishop at a given [`Square`] with the provided blockers.
///
/// This will yield a [`Bitboard`] that allows the Bishop to capture the first blocker.
#[inline(always)]
pub const fn bishop_attacks(square: Square, blockers: Bitboard) -> Bitboard {
    Bitboard::new(BISHOP_MOVES[magic_index(&BISHOP_MAGICS[square.index()], blockers)])
}

/// Computes the (unblocked) moves for a Bishop at a given [`Square`].
#[inline(always)]
pub const fn bishop_rays(square: Square) -> Bitboard {
    BISHOP_ATTACKS[square.index()]
}

/// Computes the possible moves for a Queen at a given [`Square`] with the provided blockers.
///
/// This will yield a [`Bitboard`] that allows the Queen to capture the first blocker.
#[inline(always)]
pub const fn queen_attacks(square: Square, blockers: Bitboard) -> Bitboard {
    rook_attacks(square, blockers).or(bishop_attacks(square, blockers))
}

/// Fetch the raw, unblocked attacks for a knight on the provided square.
#[inline(always)]
pub const fn knight_attacks(square: Square) -> Bitboard {
    KNIGHT_ATTACKS[square.index()]
}

/// Fetch the raw, unblocked attacks for a king on the provided square.
#[inline(always)]
pub const fn king_attacks(square: Square) -> Bitboard {
    KING_ATTACKS[square.index()]
}

/// Fetch the raw, unblocked pushes for a pawn of the provided color on the provided square.
#[inline(always)]
pub const fn pawn_pushes(square: Square, color: Color) -> Bitboard {
    match color {
        Color::White => WHITE_PAWN_PUSHES[square.index()],
        Color::Black => BLACK_PAWN_PUSHES[square.index()],
    }
}

/// Fetch the raw, unblocked attacks for a pawn of the provided color on the provided square.
#[inline(always)]
pub const fn pawn_attacks(square: Square, color: Color) -> Bitboard {
    match color {
        Color::White => WHITE_PAWN_ATTACKS[square.index()],
        Color::Black => BLACK_PAWN_ATTACKS[square.index()],
    }
}

/// Generates the default push mobility for Pawns.
///
/// Pawns, by default, may push forward by one, except when pushing from their starting rank (rank 2 for White, rank 7 for Black), in which case they may push forward by two.
const fn generate_pawn_pushes(color: Color) -> [Bitboard; 64] {
    let mut boards = [Bitboard::EMPTY_BOARD; Square::COUNT];
    let mut i = 0;
    while i < Square::COUNT {
        let square = Square::from_index_unchecked(i);
        let bb = Bitboard::from_square(square);

        if square.rank().inner() == Rank::second(color).inner() {
            boards[i] = bb.forward_by(color, 1).or(bb.forward_by(color, 2));
        } else {
            boards[i] = bb.forward_by(color, 1);
        }

        i += 1;
    }
    boards
}

/// Generates the default attack mobility for Pawns.
///
/// Pawns, by default, may capture diagonally forward by one.
const fn generate_pawn_attacks(color: Color) -> [Bitboard; 64] {
    let mut boards = [Bitboard::EMPTY_BOARD; Square::COUNT];
    let mut i = 0;
    while i < Square::COUNT {
        let square = Square::from_index_unchecked(i);
        let bb = Bitboard::from_square(square);

        boards[i] = bb
            .forward_by(color, 1)
            .east()
            .or(bb.forward_by(color, 1).west());
        i += 1;
    }
    boards
}

/// Generates the moves from every location for the "Leaper" pieces.
/// Leapers may "leap" or "jump" to a square a specified distance away.
///
/// In standard chess, the Leapers are the King and Knight.
const fn generate_leaper_mobility(deltas: &[(i8, i8)]) -> [Bitboard; Square::COUNT] {
    // Represents all locations this piece can reach from that square/index.
    let mut mobility = [Bitboard::EMPTY_BOARD; Square::COUNT];

    let mut i = 0;
    while i < Square::COUNT {
        let square = Square::from_index_unchecked(i);
        let mut j = 0;
        // for square in Square::iter() {
        // All reachable locations from `square`.
        // This is empty because we cannot "move to" the square where we are currently.
        let mut movement = Bitboard::EMPTY_BOARD;

        // Loop over every pair of deltas
        while j < deltas.len() {
            let (df, dr) = deltas[j];
            // If shifting this location by the delta results in a valid position, add it to the movement mask.
            if let Some(shifted) = square.offset(df, dr) {
                movement = movement.or(shifted.bitboard());
            }

            j += 1;
        }

        // Store the mobility from this square.
        mobility[i] = movement;
        i += 1;
    }

    mobility
}

/// Generates the moves from every location for the "Rider" pieces.
/// Riders may "ride" or "slide" an unlimited number of squares in a direction.
///
/// In standard chess, the Riders are the Rook, Bishop, and Queen.
const fn generate_rider_mobility(deltas: &[(i8, i8)]) -> [Bitboard; Square::COUNT] {
    // Represents all locations this piece can reach from that square/index.
    let mut mobility = [Bitboard::EMPTY_BOARD; Square::COUNT];

    let mut i = 0;
    while i < Square::COUNT {
        let square = Square::from_index_unchecked(i);
        let mut j = 0;
        // All reachable locations from `square`.
        // This is empty because we cannot "move to" the square where we are currently.
        let mut movement = Bitboard::EMPTY_BOARD;

        // Loop over every pair of deltas
        while j < deltas.len() {
            // for (df, dr) in deltas {
            let (df, dr) = deltas[j];
            // Create a "ray" that represents movement in this direction.
            let mut ray = square;

            // Shift the ray and append it's movement, until we reach the end of the board.
            while let Some(shifted) = ray.offset(df, dr) {
                movement = movement.or(shifted.bitboard());
                ray = shifted;
            }

            j += 1;
        }

        // Store the mobility from this square.
        mobility[i] = movement;
        i += 1;
    }

    mobility
}

/// Generates the default mobility for the King.
#[inline(always)]
const fn generate_king_mobility() -> [Bitboard; 64] {
    generate_leaper_mobility(&QUEEN_DELTAS)
}

/// Generates the default mobility for the Knight.
#[inline(always)]
const fn generate_knight_mobility() -> [Bitboard; 64] {
    generate_leaper_mobility(&KNIGHT_DELTAS)
}

/// Generates the default mobility for the Rook.
#[inline(always)]
const fn generate_rook_mobility() -> [Bitboard; Square::COUNT] {
    generate_rider_mobility(&ROOK_DELTAS)
}

/// Generates the default mobility for the Bishop.
#[inline(always)]
const fn generate_bishop_mobility() -> [Bitboard; Square::COUNT] {
    generate_rider_mobility(&BISHOP_DELTAS)
}

/*
/// Generates the default mobility for the Queen.
fn generate_queen_mobility() -> [Bitboard; Square::COUNT] {
    generate_rider_mobility(&QUEEN_DELTAS)
}

/// Generates the default mobility for the Dragon (Queen + Knight).
fn generate_dragon_mobility() -> [Bitboard; Square::COUNT] {
    let mut dragon = generate_rider_mobility(&QUEEN_DELTAS);
    let knight = generate_leaper_mobility(&KNIGHT_DELTAS);

    for square in Square::iter() {
        dragon[square] |= knight[square];
    }

    dragon
}
 */

struct MagicEntry {
    mask: u64,
    magic: u64,
    shift: u8,
    offset: u32,
}

#[inline(always)]
const fn magic_index(entry: &MagicEntry, blockers: Bitboard) -> usize {
    let blockers = blockers.inner() & entry.mask;
    let hash = blockers.wrapping_mul(entry.magic);
    let index = (hash >> entry.shift) as usize;
    entry.offset as usize + index
}

#[cfg(test)]
mod test {
    use super::*;

    /// Checks if `moves` and `legal_moves` contain all the same elements, ignoring order
    fn lists_match(moves: Bitboard, legal_moves: &[Square]) {
        assert_eq!(
            moves.population() as usize,
            legal_moves.len(),
            "\nMoves: {:?}\nLegal: {:?}",
            moves.iter().collect::<Vec<_>>(),
            legal_moves
        );

        for mv in moves {
            assert!(
                legal_moves.contains(&mv),
                "{} not found in {:?}",
                mv,
                legal_moves
            );
        }
    }

    #[test]
    fn rook_blockers() {
        let legal_moves = [
            Square::D2,
            Square::D3,
            Square::D5,
            Square::D6,
            Square::A4,
            Square::B4,
            Square::C4,
            Square::E4,
            Square::F4,
            Square::G4,
            Square::H4,
        ];

        // . . . X . . . X
        // . . . . . . . .
        // . . . X . . . .
        // . . . . . . . .
        // . . . . . . . X
        // . . X . . . . .
        // . . . X . X . .
        // . . . . . . . .
        let blockers =
            Bitboard::new(0b1000100000000000000010000000000010000000000001000010100000000000);

        let moves = rook_attacks(Square::D4, blockers);

        lists_match(moves, &legal_moves);
    }
}
