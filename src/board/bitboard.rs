/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{
    fmt,
    ops::{Index, Not},
    str::FromStr,
};

use anyhow::{anyhow, bail};

use super::{Color, File, Rank, Square};

/// A [`Bitboard`] represents the game board as a set of bits.
/// They are used for various computations, such as fetching valid moves or computing move costs.
///
/// The internal representation is a 64-bit binary number, so the values will represent the entire board.
/// They are color-agnostic, with the low order bits representing the "lower" half of the board.
///
/// Bit index 0 is the least-significant bit (LSB = 2^0)
/// Bit index 63 is the most-significant bit (MSB = 2^63)
///
/// The internal encoding uses [Little-Endian Rank-File Mapping (LERF)](https://www.chessprogramming.org/Square_Mapping_Considerations#Little-Endian_Rank-File_Mapping),
/// so a bitboard of first Rank would look like this in binary:
/// ```text
/// 00000000
/// 00000000
/// 00000000
/// 00000000
/// 00000000
/// 00000000
/// 00000000
/// 11111111
/// ```
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Bitboard(pub(crate) u64);

impl Bitboard {
    pub const FILE_A: Self = Self(0x0101010101010101);
    pub const FILE_B: Self = Self(0x0202020202020202);
    pub const FILE_C: Self = Self(0x0404040404040404);
    pub const FILE_D: Self = Self(0x0808080808080808);
    pub const FILE_E: Self = Self(0x1010101010101010);
    pub const FILE_F: Self = Self(0x2020202020202020);
    pub const FILE_G: Self = Self(0x4040404040404040);
    pub const FILE_H: Self = Self(0x8080808080808080);
    pub const NOT_FILE_A: Self = Self(0xfefefefefefefefe);
    pub const NOT_FILE_H: Self = Self(0x7f7f7f7f7f7f7f7f);
    pub const RANK_1: Self = Self(0x00000000000000FF);
    pub const RANK_2: Self = Self(0x000000000000FF00);
    pub const RANK_3: Self = Self(0x0000000000FF0000);
    pub const RANK_4: Self = Self(0x00000000FF000000);
    pub const RANK_5: Self = Self(0x000000FF00000000);
    pub const RANK_6: Self = Self(0x0000FF0000000000);
    pub const RANK_7: Self = Self(0x00FF000000000000);
    pub const RANK_8: Self = Self(0xFF00000000000000);
    pub const A1_H8_DIAG: Self = Self(0x8040201008040201);
    pub const H1_A8_DIAG: Self = Self(0x0102040810204080);
    pub const LIGHT_SQUARES: Self = Self(0x55AA55AA55AA55AA);
    pub const DARK_SQUARES: Self = Self(0xAA55AA55AA55AA55);
    pub const EMPTY_BOARD: Self = Self(0x0000000000000000);
    pub const FULL_BOARD: Self = Self(0xFFFFFFFFFFFFFFFF);
    pub const EDGES: Self = Self(0xFF818181818181FF);
    pub const CORNERS: Self = Self(0x8100000000000081);
    pub const CENTER: Self = Self(0x0000001818000000);
    pub const BACK_RANKS: Self = Self(0xFF000000000000FF);
    pub const PAWN_RANKS: Self = Self(0x00FF00000000FF00);

    /// Constructs a new [`Bitboard`] from the provided bit pattern.
    ///
    /// # Example
    /// ```
    /// # use toad::Bitboard;
    /// let board = Bitboard::new(255);
    /// assert_eq!(board.to_hex_string(), "0x00000000000000FF");
    /// ```
    #[inline(always)]
    pub const fn new(bits: u64) -> Self {
        Self(bits)
    }

    /// Constructs a new [`Bitboard`] from the provided [`Square`].
    ///
    /// The resulting [`Bitboard`] will have only a single bit toggled on.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Square};
    /// let board = Bitboard::from_square(Square::H8);
    /// assert_eq!(board.to_hex_string(), "0x8000000000000000");
    /// ```
    #[inline(always)]
    pub const fn from_square(square: Square) -> Self {
        Self(1 << square.index())
    }

    /// Constructs a new [`Bitboard`] from the provided [`File`].
    ///
    /// The resulting [`Bitboard`] will have an entire column of bits toggled on.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, File};
    /// let board = Bitboard::from_file(File::F);
    /// assert_eq!(board.to_hex_string(), "0x2020202020202020");
    /// ```
    #[inline(always)]
    pub const fn from_file(file: File) -> Self {
        Self::new(Self::FILE_A.0 << file.0)
    }

    /// Constructs a new [`Bitboard`] from the provided [`Rank`].
    ///
    /// The resulting [`Bitboard`] will have an entire row of bits toggled on.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Rank};
    /// let board = Bitboard::from_rank(Rank::SEVEN);
    /// assert_eq!(board.to_hex_string(), "0x00FF000000000000");
    /// ```
    #[inline(always)]
    pub const fn from_rank(rank: Rank) -> Self {
        Self::new(Self::RANK_1.0 << (rank.0 * 8))
    }

    /// Returns [`Bitboard::FULL_BOARD`] if `true`, else [`Bitboard::EMPTY_BOARD`].
    ///
    /// # Example
    /// ```
    /// # use toad::Bitboard;
    ///
    /// assert_eq!(Bitboard::from_bool(true), Bitboard::FULL_BOARD);
    /// assert_eq!(Bitboard::from_bool(false), Bitboard::EMPTY_BOARD);
    /// ```
    #[inline(always)]
    pub const fn from_bool(value: bool) -> Self {
        Self((value as u64).wrapping_neg() & u64::MAX)
    }

    /// If `value` is `Some`, this converts the inner `T` using the appropriate [`Bitboard::from`] implementation.
    ///
    /// If `value` is `None`, this yields an empty bitboard.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Square};
    /// assert_eq!(Bitboard::from_option(Some(Square::A1)), Square::A1.bitboard());
    /// assert_eq!(Bitboard::from_option::<Square>(None), Bitboard::EMPTY_BOARD);
    /// ```
    #[inline(always)]
    pub fn from_option<T>(value: Option<T>) -> Self
    where
        Self: From<T>,
    {
        value.map(Self::from).unwrap_or_default()
    }

    /// Returns a [`Bitboard`] of this [`Color`]'s first rank.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Color};
    ///
    /// assert_eq!(Bitboard::first_rank(Color::White), Bitboard::RANK_1);
    /// assert_eq!(Bitboard::first_rank(Color::Black), Bitboard::RANK_8);
    /// ```
    #[inline(always)]
    pub const fn first_rank(color: Color) -> Self {
        [Self::RANK_1, Self::RANK_8][color.index()]
    }

    /// Returns a [`Bitboard`] of this [`Color`]'s second rank.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Color};
    ///
    /// assert_eq!(Bitboard::second_rank(Color::White), Bitboard::RANK_2);
    /// assert_eq!(Bitboard::second_rank(Color::Black), Bitboard::RANK_7);
    /// ```
    #[inline(always)]
    pub const fn second_rank(color: Color) -> Self {
        [Self::RANK_2, Self::RANK_7][color.index()]
    }

    /// Returns a [`Bitboard`] of this [`Color`]'s third rank.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Color};
    ///
    /// assert_eq!(Bitboard::third_rank(Color::White), Bitboard::RANK_3);
    /// assert_eq!(Bitboard::third_rank(Color::Black), Bitboard::RANK_6);
    /// ```
    #[inline(always)]
    pub const fn third_rank(color: Color) -> Self {
        [Self::RANK_3, Self::RANK_6][color.index()]
    }

    /// Returns a [`Bitboard`] of this [`Color`]'s seventh rank.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Color};
    ///
    /// assert_eq!(Bitboard::seventh_rank(Color::White), Bitboard::RANK_7);
    /// assert_eq!(Bitboard::seventh_rank(Color::Black), Bitboard::RANK_2);
    /// ```
    #[inline(always)]
    pub const fn seventh_rank(color: Color) -> Self {
        [Self::RANK_7, Self::RANK_2][color.index()]
    }

    /// Returns a [`Bitboard`] of this [`Color`]'s eighth (last) rank.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Color};
    ///
    /// assert_eq!(Bitboard::eighth_rank(Color::White), Bitboard::RANK_8);
    /// assert_eq!(Bitboard::eighth_rank(Color::Black), Bitboard::RANK_1);
    /// ```
    #[inline(always)]
    pub const fn eighth_rank(color: Color) -> Self {
        [Self::RANK_8, Self::RANK_1][color.index()]
    }

    /// Returns the inner `u64` of this [`Bitboard`].
    #[inline(always)]
    pub const fn inner(&self) -> u64 {
        self.0
    }

    /// Creates a [`Square`] from this [`Bitboard`] based on the lowest-index bit that is flipped.
    ///
    /// If this [`Bitboard`] contains more than a single flipped bit, it is converted into a [`Square`]
    /// based on the index of the lowest bit that is flipped.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Square};
    /// let board = Bitboard::from_square(Square::G2);
    /// assert_eq!(board.to_square_unchecked(), Square::G2);
    /// ```
    #[inline(always)]
    pub const fn to_square_unchecked(&self) -> Square {
        Square::from_index_unchecked(self.0.trailing_zeros() as usize)
    }

    /// Creates a [`Square`] from this [`Bitboard`] based on the lowest-index bit that is flipped.
    ///
    /// If this [`Bitboard`] contains more than a single flipped bit, it is converted into a [`Square`]
    /// based on the index of the lowest bit that is flipped.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Square};
    /// let board = Bitboard::from_square(Square::G2);
    /// assert_eq!(board.to_square(), Some(Square::G2));
    /// let invalid = Bitboard::RANK_1;
    /// assert_eq!(invalid.to_square(), None);
    /// ```
    #[inline(always)]
    pub const fn to_square(&self) -> Option<Square> {
        if self.population() == 1 {
            Some(self.to_square_unchecked())
        } else {
            None
        }
    }

    /// Reverse this [`Bitboard`], viewing it from the opponent's perspective.
    ///
    /// # Example
    /// ```
    /// # use toad::Bitboard;
    /// let board = Bitboard::RANK_7;
    /// assert_eq!(board.to_hex_string(), "0x00FF000000000000");
    ///
    /// let flipped = board.flipped();
    /// assert_eq!(flipped.to_hex_string(), "0x000000000000FF00");
    /// ```
    #[inline(always)]
    pub const fn flipped(&self) -> Self {
        Self(self.0.swap_bytes())
    }

    /// If `color` is Black, flips this [`Bitboard`].
    /// If `color` is White, does nothing.
    ///
    /// See [`Bitboard::flipped`] for more.
    ///
    /// # Example
    /// ```
    /// # use toad::{Color, Bitboard};
    /// assert_eq!(Bitboard::RANK_2.relative_to(Color::White), Bitboard::RANK_2);
    /// assert_eq!(Bitboard::RANK_2.relative_to(Color::Black), Bitboard::RANK_7);
    /// ```
    #[inline(always)]
    pub const fn relative_to(self, color: Color) -> Self {
        match color {
            Color::White => self,
            Color::Black => self.flipped(),
        }
    }

    /// Checks if this [`Bitboard`] is empty, meaning all bits are set to `0`.
    ///
    /// # Example
    /// ```
    /// # use toad::Bitboard;
    /// let board = Bitboard::new(0x0);
    /// assert!(board.is_empty());
    /// ```
    #[inline(always)]
    pub const fn is_empty(&self) -> bool {
        self.0 == 0
    }

    /// Checks if this [`Bitboard`] is NOT empty, or contains at least one set bit.
    ///
    /// # Example
    /// ```
    /// # use toad::Bitboard;
    /// let board = Bitboard::CORNERS;
    /// assert!(board.is_nonempty());
    /// ```
    #[inline(always)]
    pub const fn is_nonempty(&self) -> bool {
        self.0 != 0
    }

    /// Returns `true` if `self` contains *every* bit set in `other`.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Square};
    /// // Works on single squares
    /// let e4 = Bitboard::from_square(Square::E4);
    /// assert!(e4.is_superset(Square::E4));
    ///
    /// // ...multiple squares
    /// assert!(Bitboard::FULL_BOARD.is_superset(Bitboard::CORNERS));
    ///
    /// // ...and overlaps
    /// let rank_1 = Bitboard::RANK_1;
    /// let rank_5 = Bitboard::RANK_5;
    /// assert!(rank_1.is_superset(Square::E1));
    /// assert!(!rank_1.is_superset(rank_5));
    /// ```
    #[inline(always)]
    pub fn is_superset(&self, other: impl Into<Self>) -> bool {
        let other = other.into();
        (*self & other) == other
    }

    /// Returns `true` if `other` contains *every* bit set in `self`.
    ///
    /// Wrapper for `other.is_superset(self)`, since those are logically equivalent statements.
    #[inline(always)]
    pub fn is_subset(&self, other: impl Into<Self>) -> bool {
        other.into().is_superset(*self)
    }

    /// Returns `true` if `self` contains none of the bits set in `other`.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Square};
    /// // Works on single squares
    /// let e4 = Bitboard::from_square(Square::E4);
    /// assert!(e4.is_disjoint(Square::A1));
    ///
    /// // ...multiple squares
    /// assert!(Bitboard::EDGES.is_disjoint(Bitboard::CENTER));
    ///
    /// // ...and overlaps
    /// assert!(Bitboard::RANK_1.is_disjoint(Bitboard::RANK_5));
    /// assert!(!Bitboard::RANK_1.is_disjoint(Square::A1));
    /// ```
    #[inline(always)]
    pub fn is_disjoint(&self, other: impl Into<Self>) -> bool {
        (*self & other.into()).is_empty()
    }

    /// Returns `true` if `self` contains *any* of the bits set in `other`.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Square};
    /// // Works on single squares
    /// let e4 = Bitboard::from_square(Square::E4);
    /// assert!(e4.intersects(Square::E4));
    ///
    /// // ...multiple squares
    /// assert!(Bitboard::FILE_A.intersects(Square::A3));
    ///
    /// // ...and overlaps
    /// assert!(Bitboard::RANK_1.intersects(Bitboard::FILE_A));
    /// assert!(!Bitboard::RANK_1.intersects(Bitboard::RANK_5));
    /// ```
    #[inline(always)]
    pub fn intersects(&self, other: impl Into<Self>) -> bool {
        (*self & other.into()).is_nonempty()
    }

    /// Sets the bit(s) at the location(s) specified by `other` to `1` (on).
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Square};
    /// let mut board = Bitboard::EMPTY_BOARD;
    /// board.set(Square::G2);
    /// assert_eq!(board.to_hex_string(), "0x0000000000004000");
    /// ```
    #[inline(always)]
    pub fn set(&mut self, other: impl Into<Self>) {
        *self |= other.into()
    }

    /// Toggles (inverts/negates) the bit(s) at the location(s) specified by `other`.
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Square};
    /// let mut board = Bitboard::RANK_1;
    /// assert_eq!(board.to_hex_string(), "0x00000000000000FF");
    /// board.toggle(Square::C1);
    /// assert_eq!(board.to_hex_string(), "0x00000000000000FB");
    /// board.toggle(Square::C1);
    /// assert_eq!(board.to_hex_string(), "0x00000000000000FF");
    /// ```
    #[inline(always)]
    pub fn toggle(&mut self, other: impl Into<Self>) {
        *self ^= other.into()
    }

    /// Clears the bit(s) at the location(s) specified by `other` to `0` (off).
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Square};
    /// let mut board = Bitboard::RANK_1;
    /// board.clear(Square::C1);
    /// assert_eq!(board.to_hex_string(), "0x00000000000000FB");
    /// ```
    #[inline(always)]
    pub fn clear(&mut self, other: impl Into<Self>) {
        *self &= !other.into()
    }

    /// Returns the index of the lowest non-zero bit of this [`Bitboard`], as a [`Square`].
    ///
    /// If `self` is empty, this yields `None`,
    #[inline(always)]
    pub fn lsb(&self) -> Option<Square> {
        self.is_nonempty()
            .then(|| Square(self.0.trailing_zeros() as u8))
    }

    /// Returns the index of the lowest non-zero bit of this [`Bitboard`], as a [`Square`].
    ///
    /// It is undefined behavior to call this function when `self` is empty.
    #[inline(always)]
    pub fn lsb_unchecked(&self) -> Square {
        Square(self.0.trailing_zeros() as u8)
    }

    /// Pops and returns the index of the lowest non-zero bit of this [`Bitboard`], as a [`Square`].
    ///
    /// If `self` is empty, this yields `None`,
    #[inline(always)]
    pub fn pop_lsb(&mut self) -> Option<Square> {
        let lsb = self.lsb();
        self.clear_lsb();
        lsb
    }

    /// Clears the lowest non-zero bit from `self`, if there is a square to clear.
    #[inline(always)]
    pub fn clear_lsb(&mut self) {
        self.0 &= self.0.wrapping_sub(1);
    }

    /// Returns a [`BitboardIter`] to iterate over all of the set bits as [`Square`]s.
    #[inline(always)]
    pub const fn iter(&self) -> BitboardIter {
        BitboardIter { bitboard: *self }
    }

    /// Returns a [`BitboardSubsetIter`] to iterate over all of the subsets of this bitboard.
    #[inline(always)]
    pub const fn subsets(&self) -> BitboardSubsetIter {
        BitboardSubsetIter {
            bitboard: *self,
            subset: Self::EMPTY_BOARD,
            remaining: 2usize.pow(self.population() as u32),
        }
    }

    /// Yields the total number of `1`s in this [`Bitboard`].
    ///
    /// In other words, this function determines how many bits are activated.
    ///
    /// # Example
    /// ```
    /// # use toad::Bitboard;
    /// let board = Bitboard::RANK_1;
    /// assert_eq!(board.population(), 8);
    /// ```
    #[inline(always)]
    pub const fn population(&self) -> u8 {
        self.0.count_ones() as u8
    }

    /// Shifts this [`Bitboard`] forward by `n`, according to `color`.
    ///
    /// If `color` is White, this shifts `n` ranks up. If Black, it shifts by `n` rank down.
    ///
    /// Note: This can "wrap" by advancing beyond the end of the board, so be careful!
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Color};
    /// let rank4 = Bitboard::RANK_4;
    /// assert_eq!(rank4.forward_by(Color::White, 1), Bitboard::RANK_5);
    /// assert_eq!(rank4.forward_by(Color::Black, 1), Bitboard::RANK_3);
    /// // Wrapping
    /// assert_eq!(rank4.forward_by(Color::White, 5), Bitboard::RANK_1);
    /// ```
    #[inline(always)]
    pub const fn forward_by(self, color: Color, n: u32) -> Self {
        // Black magic: If `color` is White, this rotates left by 8, which is the same as "n ranks up"
        // If `color` is Black, this rotates left by 496, which is the same as rotating right by 8, or "n ranks down"
        Self(self.0.rotate_left(n * 8 * (1 + color as u32 * 62)))
    }

    /// Shifts this [`Bitboard`] backward by `n`, according to `color`.
    ///
    /// If `color` is White, this shifts `n` ranks up. If Black, it shifts by `n` ranks down.
    ///
    /// Note: This can "wrap" by advancing beyond the end of the board, so be careful!
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Color};
    /// let rank4 = Bitboard::RANK_4;
    /// assert_eq!(rank4.backward_by(Color::White, 1), Bitboard::RANK_3);
    /// assert_eq!(rank4.backward_by(Color::Black, 1), Bitboard::RANK_5);
    /// // Wrapping
    /// assert_eq!(rank4.backward_by(Color::Black, 5), Bitboard::RANK_1);
    /// ```
    #[inline(always)]
    pub const fn backward_by(self, color: Color, n: u32) -> Self {
        // Black magic: If `color` is White, this rotates right by 8, which is the same as "n ranks down"
        // If `color` is Black, this rotates right by 496, which is the same as rotating left by 8, or "n ranks up"
        Self(self.0.rotate_right(n * 8 * (1 + color as u32 * 62)))
    }

    /// Shifts this [`Bitboard`] by one rank up.
    ///
    /// If already at the final rank (8), returns an empty board.
    ///
    /// # Example
    /// ```
    /// # use toad::Bitboard;
    /// assert_eq!(Bitboard::RANK_4.north(), Bitboard::RANK_5);
    /// assert_eq!(Bitboard::RANK_8.north(), Bitboard::EMPTY_BOARD);
    /// ```
    #[inline(always)]
    pub const fn north(self) -> Self {
        Self(self.0 << 8)
    }

    // Rank down
    /// Shifts this [`Bitboard`] by one rank board.
    ///
    /// If already at the first rank (1), returns an empty board.
    ///
    /// # Example
    /// ```
    /// # use toad::Bitboard;
    /// assert_eq!(Bitboard::RANK_4.south(), Bitboard::RANK_3);
    /// assert_eq!(Bitboard::RANK_1.south(), Bitboard::EMPTY_BOARD);
    /// ```
    #[inline(always)]
    pub const fn south(self) -> Self {
        Self(self.0 >> 8)
    }

    /// Shifts this [`Bitboard`] by one [`File`] up.
    ///
    /// If already at the first file (a), returns an empty board.
    ///
    /// # Example
    /// ```
    /// # use toad::Bitboard;
    /// assert_eq!(Bitboard::FILE_C.east(), Bitboard::FILE_D);
    /// assert_eq!(Bitboard::FILE_H.east(), Bitboard::EMPTY_BOARD);
    /// ```
    #[inline(always)]
    pub const fn east(self) -> Self {
        // Post-shift mask
        Self((self.0 << 1) & Self::NOT_FILE_A.0)
    }

    /// Shifts this [`Bitboard`] by one [`File`] down.
    ///
    /// If already at the final file (h), returns an empty board.
    ///
    /// # Example
    /// ```
    /// # use toad::Bitboard;
    /// assert_eq!(Bitboard::FILE_C.west(), Bitboard::FILE_B);
    /// assert_eq!(Bitboard::FILE_A.west(), Bitboard::EMPTY_BOARD);
    /// ```
    #[inline(always)]
    pub const fn west(self) -> Self {
        // Post-shift mask
        Self((self.0 >> 1) & Self::NOT_FILE_H.0)
    }

    /// Combination of [`Bitboard::north`] and [`Bitboard::east`].
    ///
    /// If already at the edge, returns an empty board.
    ///
    /// This operation is faster than calling [`Bitboard::north`] and [`Bitboard::east`] separately.
    #[inline(always)]
    pub const fn northeast(self) -> Self {
        // Post-shift mask
        Self((self.0 << 9) & Self::NOT_FILE_A.0)
    }

    /// Combination of [`Bitboard::south`] and [`Bitboard::east`].
    ///
    /// If already at the edge, returns an empty board.
    ///
    /// This operation is faster than calling [`Bitboard::south`] and [`Bitboard::east`] separately.
    #[inline(always)]
    pub const fn southeast(self) -> Self {
        // Post-shift mask
        Self((self.0 >> 7) & Self::NOT_FILE_A.0)
    }

    /// Combination of [`Bitboard::north`] and [`Bitboard::west`].
    ///
    /// If already at the edge, returns an empty board.
    ///
    /// This operation is faster than calling [`Bitboard::north`] and [`Bitboard::west`] separately.
    #[inline(always)]
    pub const fn northwest(self) -> Self {
        // Post-shift mask
        Self((self.0 << 7) & Self::NOT_FILE_H.0)
    }

    /// Combination of [`Bitboard::south`] and [`Bitboard::west`].
    ///
    /// If already at the edge, returns an empty board.
    ///
    /// This operation is faster than calling [`Bitboard::south`] and [`Bitboard::west`] separately.
    #[inline(always)]
    pub const fn southwest(self) -> Self {
        // Post-shift mask
        Self((self.0 >> 9) & Self::NOT_FILE_H.0)
    }

    /*
    /// Creates a mask of all squares in front of `square` (according to `color`) that are either directly in front or on the adjacent files.
    pub fn passed_pawn_mask(square: Square, color: Color) -> Self {
        let rank = match color {
            Color::White => square.rank().increase(),
            Color::Black => square.rank().decrease(),
        };

        let forward_ranks = Self::FULL_BOARD << rank;

        let file = square.file();
        let left = Self::from(file.decrease());
        let right = Self::from(file.increase());
        let center = Self::from_file(file);

        let files = center | left | right;

        forward_ranks & files
    }
     */

    /// `const` analog of [`std::ops::BitAnd::bitand`].
    ///
    /// Returns the bitwise AND (`&`) of `self` and `other`.
    #[inline(always)]
    pub const fn and(self, other: Self) -> Self {
        Self(self.0 & other.0)
    }

    /// `const` analog of [`std::ops::BitOr::bitor`].
    ///
    /// Returns the bitwise OR (`|`) of `self` and `other`.
    #[inline(always)]
    pub const fn or(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    /// `const` analog of [`std::ops::BitXor::bitxor`].
    ///
    /// Returns the bitwise XOR (`^`) of `self` and `other`.
    #[inline(always)]
    pub const fn xor(self, other: Self) -> Self {
        Self(self.0 ^ other.0)
    }

    /// `const` analog of [`Not::not`].
    ///
    /// Returns the logical negation (`!`) of `self`, flipping all `1`'s to `0`'s and vice versa.
    #[inline(always)]
    pub const fn not(self) -> Self {
        Self(!self.0)
    }

    /// Formats this [`Bitboard`] as a hexadecimal string.
    #[inline(always)]
    pub fn to_hex_string(&self) -> String {
        format!("0x{:0>16X}", self.0)
    }
}

impl FromStr for Bitboard {
    type Err = anyhow::Error;
    /// Constructs a new [`Bitboard`] from the provided string.
    ///
    /// The string may be a binary or hexadecimal number, and may be proceeded with `0b` or `0x`.
    ///
    /// # Example
    /// ```
    /// # use toad::Bitboard;
    /// use std::str::FromStr;
    /// let board1 = Bitboard::from_str("0x00FF000000000000").unwrap();
    /// let board2 = Bitboard::from_str("00FF000000000000").unwrap();
    /// let board3 = Bitboard::from_str("0000000011111111000000000000000000000000000000000000000000000000").unwrap();
    /// let board4 = Bitboard::from_str("0b0000000011111111000000000000000000000000000000000000000000000000").unwrap();
    /// assert_eq!(board1, board2);
    /// assert_eq!(board1, board3);
    /// assert_eq!(board1, board4);
    /// assert_eq!(board1.to_hex_string(), "0x00FF000000000000");
    /// ```
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let bits = s.to_lowercase();

        if bits.len() == 64 || bits.len() == 66 {
            let bits = bits.trim_start_matches("0b");
            let bits = u64::from_str_radix(bits, 2).map_err(|_| {
                anyhow!("Invalid Bitboard string: Expected binary digits, got {bits}")
            })?;
            Ok(Self::new(bits))
        } else if bits.len() == 16 || bits.len() == 18 {
            let bits = bits.trim_start_matches("0x");
            let bits = u64::from_str_radix(bits, 16).map_err(|_| {
                anyhow!("Invalid Bitboard string: Expected hexadecimal digits, got {bits}")
            })?;
            Ok(Self::new(bits))
        } else {
            bail!("Invalid Bitboard string: Invalid length {}. Length must be either 64 (binary) or 16 (hexadecimal)", bits.len())
        }
    }
}

impl FromIterator<Square> for Bitboard {
    /// A [`Bitboard`] can be created from an iterator over [`Square`]s.
    fn from_iter<T: IntoIterator<Item = Square>>(iter: T) -> Self {
        iter.into_iter().fold(Self::default(), |bb, sq| bb | sq)
    }
}

macro_rules! impl_bitwise_op {
    // Impl op and op_assign for Self
    ($op:tt, $op_assign:tt, $func:ident, $func_assign:ident) => {
        impl<T> std::ops::$op<T> for Bitboard
        where
            Self: From<T>,
        {
            type Output = Self;
            #[inline(always)]
            fn $func(self, rhs: T) -> Self::Output {
                Self(self.0.$func(Self::from(rhs).0))
            }
        }

        impl<T> std::ops::$op_assign<T> for Bitboard
        where
            Self: From<T>,
        {
            #[inline(always)]
            fn $func_assign(&mut self, rhs: T) {
                self.0.$func_assign(Self::from(rhs).0);
            }
        }
    };
}

impl_bitwise_op!(BitAnd, BitAndAssign, bitand, bitand_assign);
impl_bitwise_op!(BitOr, BitOrAssign, bitor, bitor_assign);
impl_bitwise_op!(BitXor, BitXorAssign, bitxor, bitxor_assign);

impl Not for Bitboard {
    type Output = Self;
    #[inline(always)]
    fn not(self) -> Self::Output {
        Self(!self.0)
    }
}

impl<T: Into<Bitboard>> Index<T> for Bitboard {
    type Output = bool;

    /// Wrapper over [`Bitboard::intersects`].
    #[inline(always)]
    fn index(&self, index: T) -> &Self::Output {
        if self.intersects(index) {
            &true
        } else {
            &false
        }
    }
}

impl<T> From<Option<T>> for Bitboard
where
    Self: From<T>,
{
    /// Wrapper for [`Bitboard::from_option`].
    #[inline(always)]
    fn from(value: Option<T>) -> Self {
        Self::from_option(value)
    }
}

impl From<Square> for Bitboard {
    #[inline(always)]
    /// Wrapper for [`Bitboard::from_square`].
    fn from(value: Square) -> Self {
        Self::from_square(value)
    }
}

impl From<File> for Bitboard {
    #[inline(always)]
    /// Wrapper for [`Bitboard::from_file`].
    fn from(value: File) -> Self {
        Self::from_file(value)
    }
}

impl From<Rank> for Bitboard {
    #[inline(always)]
    /// Wrapper for [`Bitboard::from_rank`].
    fn from(value: Rank) -> Self {
        Self::from_rank(value)
    }
}

impl From<u64> for Bitboard {
    #[inline(always)]
    /// Wrapper for [`Bitboard::new`].
    fn from(value: u64) -> Self {
        Self::new(value)
    }
}

impl From<bool> for Bitboard {
    #[inline(always)]
    /// Wrapper for [`Bitboard::from_bool`].
    fn from(value: bool) -> Self {
        Self::from_bool(value)
    }
}

impl fmt::UpperHex for Bitboard {
    /// Formats this [`Bitboard`] as a 16-character uppercase hexadecimal string, not including the `0x` prefix.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0X{:0>16X}", self.0)
    }
}

impl fmt::LowerHex for Bitboard {
    /// Formats this [`Bitboard`] as a 16-character lowercase hexadecimal string, not including the `0x` prefix.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0x{:0>16x}", self.0)
    }
}

impl fmt::Binary for Bitboard {
    /// Formats this [`Bitboard`] as a 64-character binary string, not including the `0b` prefix.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "0b{:0>64b}", self.0)
    }
}

impl Default for Bitboard {
    #[inline(always)]
    fn default() -> Self {
        Self::EMPTY_BOARD
    }
}

impl fmt::Display for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Allocate just enough capacity
        let mut board = String::with_capacity(136);

        for rank in Rank::iter().rev() {
            for file in File::iter() {
                let square = Square::new(file, rank);
                let occupant = if self.intersects(square) { 'X' } else { '.' };

                board += &format!("{occupant} ");
            }
            board += "\n";
        }

        write!(f, "{board}")
    }
}

impl fmt::Debug for Bitboard {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Allocate just enough capacity
        let mut board = String::with_capacity(198);

        for rank in Rank::iter().rev() {
            board += &format!("{rank}| ");

            for file in File::iter() {
                let square = Square::new(file, rank);
                let occupant = if self.intersects(square) { 'X' } else { '.' };

                board += &format!("{occupant} ");
            }
            board += "\n";
        }
        board += " +";
        for _ in File::iter() {
            board += "--";
        }
        board += "\n   ";
        for file in File::iter() {
            board += &format!("{file} ");
        }

        write!(f, "{board}")
    }
}

/// An iterator over all set bits in a [`Bitboard`].
///
/// See [`Bitboard::iter`].
pub struct BitboardIter {
    bitboard: Bitboard,
}

impl Iterator for BitboardIter {
    type Item = Square;
    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        self.bitboard.pop_lsb()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.bitboard.population() as usize;
        (size, Some(size))
    }
}

impl ExactSizeIterator for BitboardIter {
    #[inline(always)]
    fn len(&self) -> usize {
        self.bitboard.population() as usize
    }
}

impl IntoIterator for Bitboard {
    type Item = Square;
    type IntoIter = BitboardIter;
    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        BitboardIter { bitboard: self }
    }
}

impl IntoIterator for &Bitboard {
    type Item = Square;
    type IntoIter = BitboardIter;
    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        BitboardIter { bitboard: *self }
    }
}

impl IntoIterator for &mut Bitboard {
    type Item = Square;
    type IntoIter = BitboardIter;
    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        BitboardIter { bitboard: *self }
    }
}

/// An iterator over all possible subsets of a [`Bitboard`].
///
/// See [`Bitboard::subsets`].
///
/// This is primarily used in magic bitboard generation, but may also be useful for other purposes, so it is made public.
pub struct BitboardSubsetIter {
    /// The original bitboard whose subsets to iterate over.
    bitboard: Bitboard,

    /// The current subset, which will be the result of `.next()`.
    subset: Bitboard,

    /// How many subsets we have left to iterate.
    remaining: usize,
}

impl Iterator for BitboardSubsetIter {
    type Item = Bitboard;
    fn next(&mut self) -> Option<Self::Item> {
        if self.remaining == 0 {
            None
        } else {
            // By saving and returning the original subset, we make the iterator return
            // an empty set as the first element and the full subset as the last.
            let subset = self.subset;

            // Performs a Carry-Rippler operation: https://www.chessprogramming.org/Traversing_Subsets_of_a_Set#All_Subsets_of_any_Set
            self.subset.0 = self.subset.0.wrapping_sub(self.bitboard.0) & self.bitboard.0;
            self.remaining -= 1;

            Some(subset)
        }
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        (self.remaining, Some(self.remaining))
    }
}

impl ExactSizeIterator for BitboardSubsetIter {
    #[inline(always)]
    fn len(&self) -> usize {
        self.remaining
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_bitboard_to_string() {
        let expected = ". . . . . . . X \n\
                              . . . . . . X . \n\
                              . . . . . X . . \n\
                              . . . . X . . . \n\
                              . . . X . . . . \n\
                              . . X . . . . . \n\
                              . X . . . . . . \n\
                              X . . . . . . . \n";
        assert_eq!(Bitboard::A1_H8_DIAG.to_string(), expected);

        let expected = ". . . . . . . . \n\
                              . . . . . . . . \n\
                              . . . . . . . . \n\
                              . . . . . . . . \n\
                              . . . . . . . . \n\
                              . . . . . . . . \n\
                              X X X X X X X X \n\
                              . . . . . . . . \n";
        assert_eq!(Bitboard::RANK_2.to_string(), expected);

        let board = Bitboard::RANK_2 | Bitboard::FILE_C;
        let expected = ". . X . . . . . \n\
                              . . X . . . . . \n\
                              . . X . . . . . \n\
                              . . X . . . . . \n\
                              . . X . . . . . \n\
                              . . X . . . . . \n\
                              X X X X X X X X \n\
                              . . X . . . . . \n";
        assert_eq!(board.to_string(), expected);
    }

    #[test]
    fn test_bitboard_masking() {
        let file_a = Bitboard::FILE_A;
        let full_board = Bitboard::FULL_BOARD;
        let expected = Bitboard::NOT_FILE_A;

        assert_eq!(file_a ^ full_board, expected);
    }

    #[test]
    fn test_bitboard_from_str() {
        let bits = "0x0101010101010101";
        let board = Bitboard::from_str(&bits).unwrap();
        assert_eq!(board, Bitboard::FILE_A);

        let bits = "0101010101010101";
        let board = Bitboard::from_str(&bits).unwrap();
        assert_eq!(board, Bitboard::FILE_A);

        let bits = "0b0000000100000001000000010000000100000001000000010000000100000001";
        let board = Bitboard::from_str(&bits).unwrap();
        assert_eq!(board, Bitboard::FILE_A);

        let bits = "0000000100000001000000010000000100000001000000010000000100000001";
        let board = Bitboard::from_str(&bits).unwrap();
        assert_eq!(board, Bitboard::FILE_A);

        let bits = "0b0000000200000002000000020000000200000002000000010000000100000001";
        let board = Bitboard::from_str(bits);
        assert!(board.is_err());

        let bits = "0000000200000002000000020000000200000002000000010000000100000001";
        let board = Bitboard::from_str(bits);
        assert!(board.is_err());

        let bits = "x0awdk";
        let board = Bitboard::from_str(bits);
        assert!(board.is_err());

        let bits = "";
        let board = Bitboard::from_str(bits);
        assert!(board.is_err());
    }

    #[test]
    fn test_bitboard_constructors() {
        assert_eq!(Bitboard::RANK_4, Bitboard::from_rank(Rank::FOUR));
    }
}
