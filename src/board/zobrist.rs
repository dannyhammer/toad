/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::fmt;

use crate::{Board, CastlingRights, Color, Piece, Position, Rank, Square, XoShiRo};

/// Stores Zobrist hash keys, for hashing [`Position`]s.
///
/// Initialized upon program startup with library-supplied keys that remain constant between compilations.
const ZOBRIST_TABLE: ZobristHashTable = ZobristHashTable::new();

/// Represents a key generated from a Zobrist Hash
#[derive(Default, PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Clone, Copy)]
pub struct ZobristKey(u64);

impl ZobristKey {
    /// Generates a new [`ZobristKey`] from the supplied [`Position`].
    ///
    /// This is equivalent to calling [`ZobristKey::from_parts`] with all of the [`Position`]'s relevant components.
    ///
    /// # Example
    /// ```
    /// # use toad::*;
    /// let pos = Position::default();
    /// let key= ZobristKey::new(&pos);
    /// assert_ne!(key.inner(), 0);
    /// ```
    #[inline(always)]
    pub fn new(position: &Position) -> Self {
        Self::from_parts(
            position.board(),
            position.ep_square(),
            position.castling_rights(),
            position.side_to_move(),
        )
    }

    /// Generates a [`ZobristKey`] from the provided components of a [`Position`].
    #[inline(always)]
    pub fn from_parts(
        board: &Board,
        ep_square: Option<Square>,
        castling_rights: &[CastlingRights; Color::COUNT],
        color: Color,
    ) -> Self {
        let mut key = Self::default();

        // Hash all pieces on the board
        for (square, piece) in board.iter() {
            key.hash_piece(square, piece);
        }

        // Hash the en passant square, if it exists
        key.hash_optional_ep_square(ep_square);

        // Hash the castling rights
        key.hash_castling_rights(castling_rights);

        // Hash the side-to-move
        key.hash_side_to_move(color);

        key
    }

    /// Return the inner `u64` of this key.
    ///
    /// # Example
    /// ```
    /// # use toad::*;
    /// let zero = ZobristKey::default();
    /// assert_eq!(zero.inner(), 0);
    /// ```
    #[inline(always)]
    pub fn inner(&self) -> u64 {
        self.0
    }

    /// Adds/removes `hash_key` to this [`ZobristKey`].
    ///
    /// This is done internally with the XOR operator.
    ///
    /// # Example
    /// ```
    /// # use toad::*;
    /// let mut zero = ZobristKey::default();
    /// assert_eq!(zero.inner(), 0);
    ///
    /// zero.hash(42);
    /// assert_ne!(zero.inner(), 0);
    ///
    /// // Calling again un-hashes it
    /// zero.hash(42);
    /// assert_eq!(zero.inner(), 0);
    /// ```
    #[inline(always)]
    pub fn hash(&mut self, hash_key: u64) {
        self.0 ^= hash_key;
    }

    /// Adds/removes the hash for the provided `piece` at `square` to this [`ZobristKey`].
    ///
    /// # Example
    /// ```
    /// # use toad::*;
    /// let zero = ZobristKey::default();
    ///
    /// let mut black_pawn_d7 = ZobristKey::default();
    /// black_pawn_d7.hash_piece(Square::D7, Piece::BLACK_PAWN);
    /// assert_ne!(black_pawn_d7, zero);
    ///
    /// // Pieces on different squares have different hashes
    /// let mut black_pawn_d5 = ZobristKey::default();
    /// black_pawn_d5.hash_piece(Square::D5, Piece::BLACK_PAWN);
    /// assert_ne!(black_pawn_d5, black_pawn_d7);
    ///
    /// // Different colored pieces have different hashes
    /// let mut white_pawn_d7 = ZobristKey::default();
    /// white_pawn_d7.hash_piece(Square::D7, Piece::WHITE_PAWN);
    /// assert_ne!(white_pawn_d7, black_pawn_d7);
    ///
    /// // Different kinds of pieces have different hashes
    /// let mut black_rook_d7 = ZobristKey::default();
    /// black_rook_d7.hash_piece(Square::D7, Piece::BLACK_ROOK);
    /// assert_ne!(black_rook_d7, black_pawn_d7);
    /// ```
    #[inline(always)]
    pub fn hash_piece(&mut self, square: Square, piece: Piece) {
        self.hash(ZOBRIST_TABLE.piece_keys[square][piece]);
    }

    /// Adds/removes the hash for the provided `ep_square` to this [`ZobristKey`].
    ///
    /// # Example
    /// ```
    /// # use toad::*;
    /// let zero = ZobristKey::default();
    ///
    ///
    /// let mut e3 = ZobristKey::default();
    /// e3.hash_ep_square(Square::E3);
    /// assert_ne!(e3, zero);
    ///
    /// let mut d6 = ZobristKey::default();
    /// d6.hash_ep_square(Square::D6);
    /// assert_ne!(d6, e3);
    ///
    /// // For simplicity, only valid en passant squares (ranks 3 and 6) have hash keys
    /// let mut e2 = ZobristKey::default();
    /// e2.hash_ep_square(Square::E2);
    /// assert_eq!(e2, zero);
    /// ```
    #[inline(always)]
    pub fn hash_ep_square(&mut self, ep_square: Square) {
        self.hash(ZOBRIST_TABLE.ep_keys[ep_square]);
    }

    /// Adds/removes the hash for the provided `ep_square` to this [`ZobristKey`].
    ///
    /// This is the same as calling [`ZobristKey::hash_ep_square`] with a safely-unwrapped `ep_square`.
    #[inline(always)]
    pub fn hash_optional_ep_square(&mut self, ep_square: Option<Square>) {
        // This works because all squares where EP isn't possible (including Square::default) have a hash value of 0
        self.hash_ep_square(ep_square.unwrap_or_default());
    }

    /// Adds/removes the hash for the provided `castling_rights` to this [`ZobristKey`].
    ///
    /// # Example
    /// ```
    /// # use toad::*;
    /// let zero = ZobristKey::default();
    ///
    ///
    /// // Default castling rights
    /// let white = CastlingRights::new(Some(Square::H1), Some(Square::A1));
    /// let black = CastlingRights::new(Some(Square::H8), Some(Square::A8));
    /// let rights = [white, black];
    ///
    /// let mut default_rights = ZobristKey::default();
    /// default_rights.hash_castling_rights(&rights);
    /// assert_ne!(default_rights, zero);
    /// ```
    #[inline(always)]
    pub fn hash_castling_rights(&mut self, castling_rights: &[CastlingRights; Color::COUNT]) {
        let w_index = castling_rights[Color::White].index();
        let b_index = castling_rights[Color::Black].index();
        let index = w_index << 2 | b_index;
        self.hash(ZOBRIST_TABLE.castling_keys[index]);
    }

    /// Adds/removes the hash for when the side-to-move is Black to this [`ZobristKey`].
    ///
    /// # Example
    /// ```
    /// # use toad::*;
    /// let zero = ZobristKey::default();
    ///
    /// // Only Black affects the key, as White is equivalent to 0
    /// let mut black = ZobristKey::default();
    /// black.hash_side_to_move(Color::Black);
    /// assert_ne!(black, zero);
    ///
    /// // White's side-to-move key is 0, intentionally
    /// let mut white = ZobristKey::default();
    /// white.hash_side_to_move(Color::White);
    /// assert_eq!(white, zero);
    /// ```
    #[inline(always)]
    pub fn hash_side_to_move(&mut self, color: Color) {
        self.hash(ZOBRIST_TABLE.color_key[color]);
    }
}

impl fmt::Display for ZobristKey {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Encapsulates the logic of Zobrist hashing.
#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
struct ZobristHashTable {
    /// One unique key for every possible piece and every possible square.
    piece_keys: [[u64; Piece::COUNT]; Square::COUNT],

    /// One unique key for every square where en passant is possible.
    ep_keys: [u64; Square::COUNT],

    /// One key for every possible combination of castling rights.
    castling_keys: [u64; CastlingRights::COUNT],

    /// One key for the side-to-move (only if side-to-move is Black- White's key is 0).
    color_key: [u64; Color::COUNT],
}

impl ZobristHashTable {
    /// Initialize this table, generating keys via the [`XoShiRo`] struct.
    ///
    /// This is only done once, at compilation, and is stored in the global `ZOBRIST_TABLE` constant.
    const fn new() -> Self {
        let mut piece_keys = [[0; Piece::COUNT]; Square::COUNT];
        let mut color_key = [0; Color::COUNT];
        let mut ep_keys = [0; Square::COUNT];
        let mut castling_keys = [0; CastlingRights::COUNT];

        let mut prng = XoShiRo::new();

        // Initialize keys for pieces and EP
        let mut i = 0;
        while i < Square::COUNT {
            let mut j = 0;
            // Initialize keys for pieces
            while j < Piece::COUNT {
                let key;
                (key, prng) = prng.get_next_const();
                piece_keys[i][j] = key;
                j += 1;
            }

            // Initialize keys for en passant squares
            let rank = Square::from_index_unchecked(i).rank();
            if rank.is(&Rank::THREE) || rank.is(&Rank::SIX) {
                // Since en passant can only happen on ranks 3 and 6, we only need to store hash keys for those ranks
                let key;
                (key, prng) = prng.get_next_const();
                ep_keys[i] = key;
            }

            i += 1;
        }

        // Initialize keys for castling rights
        i = 0;
        while i < CastlingRights::COUNT {
            let key;
            (key, prng) = prng.get_next_const();
            castling_keys[i] = key;
            i += 1;
        }

        // Initialize keys for side-to-move
        // Only Black has a key, since White's is just 0
        let (key, _) = prng.get_next_const();
        color_key[Color::Black.index()] = key;

        Self {
            piece_keys,
            ep_keys,
            castling_keys,
            color_key,
        }
    }
}
