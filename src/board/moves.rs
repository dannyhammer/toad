/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{fmt, str::FromStr};

use anyhow::{anyhow, Result};

use super::{File, Piece, PieceKind, Position, Rank, Square};

/// Maximum possible number of moves in a given chess position.
///
/// Found [here](<https://www.chessprogramming.org/Chess_Position#cite_note-4>)
pub const MAX_NUM_MOVES: usize = 218;

/// An alias for an [`arrayvec::ArrayVec`] containing at most [`MAX_NUM_MOVES`] moves.
pub type MoveList = arrayvec::ArrayVec<Move, MAX_NUM_MOVES>;

/// Represents the different kinds of moves that can be made during a chess game.
///
/// Internally, these are represented by bit flags, which allows a compact representation of the [`Move`] struct.
/// You do not need to know the bit flag values. They are only relevant internally.
/// If you care, though, they are fetched from the [chess programming wiki](https://www.chessprogramming.org/Encoding_Moves#From-To_Based).
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, PartialOrd, Ord)]
#[repr(u16)]
pub enum MoveKind {
    /// Involves only a single piece moving from one location to another, and does not change the quantity or kind of any pieces on the board.
    Quiet = 0 << Move::FLG_BITS,

    /// A special case on a Pawn's first move, wherein it can advance two squares forward.
    PawnDoublePush = 1 << Move::FLG_BITS,

    /// Involves the King and a Rook sliding past each other on the King's side of the board.
    ShortCastle = 2 << Move::FLG_BITS,

    /// Involves the King and a Rook sliding past each other on the Queen's side of the board.
    LongCastle = 3 << Move::FLG_BITS,

    /// Involves a piece moving onto a square occupied by an opponent's piece, removing it from the board.
    Capture = 4 << Move::FLG_BITS,

    /// A special variant of capturing that occurs when a Pawn executes En Passant.
    EnPassantCapture = 5 << Move::FLG_BITS,

    /// Involves a Pawn reaching the opponent's side of the board (rank 8 for White, rank 1 for Black) and becoming a [`PieceKind::Knight`].
    PromoteKnight = 8 << Move::FLG_BITS,

    /// Involves a Pawn reaching the opponent's side of the board (rank 8 for White, rank 1 for Black) and becoming a [`PieceKind::Bishop`].
    PromoteBishop = 9 << Move::FLG_BITS,

    /// Involves a Pawn reaching the opponent's side of the board (rank 8 for White, rank 1 for Black) and becoming a [`PieceKind::Rook`].
    PromoteRook = 10 << Move::FLG_BITS,

    /// Involves a Pawn reaching the opponent's side of the board (rank 8 for White, rank 1 for Black) and becoming a [`PieceKind::Queen`].
    PromoteQueen = 11 << Move::FLG_BITS,

    /// Involves a Pawn moving onto a square on the opponent's side of the board that is occupied by an opponent's piece, removing it from the board, and promoting this Pawn to a [`PieceKind::Knight`].
    CaptureAndPromoteKnight = 12 << Move::FLG_BITS,

    /// Involves a Pawn moving onto a square on the opponent's side of the board that is occupied by an opponent's piece, removing it from the board, and promoting this Pawn to a [`PieceKind::Bishop`].
    CaptureAndPromoteBishop = 13 << Move::FLG_BITS,

    /// Involves a Pawn moving onto a square on the opponent's side of the board that is occupied by an opponent's piece, removing it from the board, and promoting this Pawn to a [`PieceKind::Rook`].
    CaptureAndPromoteRook = 14 << Move::FLG_BITS,

    /// Involves a Pawn moving onto a square on the opponent's side of the board that is occupied by an opponent's piece, removing it from the board, and promoting this Pawn to a [`PieceKind::Queen`].
    CaptureAndPromoteQueen = 15 << Move::FLG_BITS,
}

impl MoveKind {
    /// Creates a new [`MoveKind`] that is a promotion to the provided [`PieceKind`].
    ///
    /// # Panics
    /// This function will panic if `promotion` is not a Knight, Bishop, Rook, or Queen.
    #[inline(always)]
    pub fn promotion(promotion: PieceKind) -> Self {
        match promotion {
            PieceKind::Knight => Self::PromoteKnight,
            PieceKind::Bishop => Self::PromoteBishop,
            PieceKind::Rook => Self::PromoteRook,
            PieceKind::Queen => Self::PromoteQueen,
            _ => unreachable!(),
        }
    }

    /// Creates a new [`MoveKind`] that is a capture and promotion to the provided [`PieceKind`].
    ///
    /// # Panics
    /// This function will panic if `promotion` is not a Knight, Bishop, Rook, or Queen.
    #[inline(always)]
    pub fn promotion_capture(promotion: PieceKind) -> Self {
        match promotion {
            PieceKind::Knight => Self::CaptureAndPromoteKnight,
            PieceKind::Bishop => Self::CaptureAndPromoteBishop,
            PieceKind::Rook => Self::CaptureAndPromoteRook,
            PieceKind::Queen => Self::CaptureAndPromoteQueen,
            _ => unreachable!(),
        }
    }

    /// Determines the appropriate [`MoveKind`] for moving the `piece` at `from` onto `to`, within the provided `position`.
    ///
    /// If `promotion` was provided and other parameters specify that this is a pawn moving to the eighth rank,
    /// this will yield a promotion variant that promotes to the [`PieceKind`] specified by `promotion`.
    ///
    /// # Example
    /// ```
    /// # use toad::{Position, MoveKind, Piece, PieceKind, Square};
    /// let pos = Position::default();
    /// let kind = MoveKind::new(Piece::WHITE_PAWN, Square::E2, Square::E4, &pos, None);
    /// assert_eq!(kind, MoveKind::PawnDoublePush);
    /// ```
    pub fn new(
        piece: Piece,
        from: Square,
        to: Square,
        position: &Position,
        promotion: Option<PieceKind>,
    ) -> Self {
        // By default, it's either a quiet or a capture.
        let victim = position.piece_at(to);
        let mut kind = if victim.is_some() {
            Self::Capture
        } else {
            Self::Quiet
        };

        // Pawns and Kings have special movement cases
        match piece.kind() {
            // Pawns are... complicated
            PieceKind::Pawn => {
                // If a promotion was provided, it's a promotion of some kind
                if let Some(promotion) = promotion {
                    // If this move also captures, it's a capture-promote
                    if kind == Self::Capture {
                        kind = Self::promotion_capture(promotion);
                    } else {
                        kind = Self::promotion(promotion);
                    }
                } else
                // If this pawn is moving to the en passant square, it's en passant
                if Some(to) == position.ep_square() {
                    kind = Self::EnPassantCapture;
                } else
                // If the Pawn is moving two ranks, it's a double push
                if from.rank().abs_diff(to.rank()) == 2 {
                    kind = Self::PawnDoublePush;
                }
            }

            // King moves are only special if castling
            PieceKind::King
                if victim
                    .is_some_and(|victim| victim.color() == piece.color() && victim.is_rook()) =>
            {
                if to.file() > from.file() {
                    kind = Self::ShortCastle;
                } else {
                    kind = Self::LongCastle;
                }
            }

            // All other pieces have no special moves
            _ => {}
        }

        kind
    }
}

impl fmt::Display for MoveKind {
    /// Displays a human-readable description for this [`MoveKind`].
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Self::Quiet => "Quiet",
            Self::PawnDoublePush => "Pawn Double Push",
            Self::EnPassantCapture => "En Passant Capture",
            Self::ShortCastle => "Short Castle",
            Self::LongCastle => "Long Castle",
            Self::Capture => "Capture",
            Self::PromoteQueen => "Promotion (Queen)",
            Self::PromoteKnight => "Promotion (Knight)",
            Self::PromoteRook => "Promotion (Rook)",
            Self::PromoteBishop => "Promotion (Bishop)",
            Self::CaptureAndPromoteQueen => "Capture and Promotion (Queen)",
            Self::CaptureAndPromoteKnight => "Capture and Promotion (Knight)",
            Self::CaptureAndPromoteRook => "Capture and Promotion (Rook)",
            Self::CaptureAndPromoteBishop => "Capture and Promotion (Bishop)",
        };

        write!(f, "{s}")
    }
}

/// Represents a move made on a chess board, including whether a piece is to be promoted.
///
/// Internally encoded using the following bit pattern:
/// ```text
///     0000 000000 000000
///      |     |      |
///      |     |      +- Source square of the move.
///      |     +- Target square of the move.
///      +- Special flags for promotion, castling, etc.
/// ```
///
/// Flags are fetched directly from the [Chess Programming Wiki](https://www.chessprogramming.org/Encoding_Moves#From-To_Based).
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Move(u16);

impl Move {
    /// Mask for the source ("from") bits.
    const SRC_MASK: u16 = 0b0000_0000_0011_1111;
    /// Mask for the destination ("to") bits.
    const DST_MASK: u16 = 0b0000_1111_1100_0000;
    /// Mask for the flag (promotions, captures, etc.) bits.
    const FLG_MASK: u16 = 0b1111_0000_0000_0000;
    /// Start index of destination bits.
    const DST_BITS: u16 = 6;
    /// Start index of flag bits.
    const FLG_BITS: u16 = 12;

    // const FLAG_QUIET: u16 = 0 << Self::FLG_BITS;
    const FLAG_PAWN_DOUBLE: u16 = 1 << Self::FLG_BITS;
    const FLAG_CASTLE_SHORT: u16 = 2 << Self::FLG_BITS;
    const FLAG_CASTLE_LONG: u16 = 3 << Self::FLG_BITS;
    const FLAG_CAPTURE: u16 = 4 << Self::FLG_BITS;
    const FLAG_EP_CAPTURE: u16 = 5 << Self::FLG_BITS;
    const FLAG_PROMO_KNIGHT: u16 = 8 << Self::FLG_BITS;
    const FLAG_PROMO_BISHOP: u16 = 9 << Self::FLG_BITS;
    const FLAG_PROMO_ROOK: u16 = 10 << Self::FLG_BITS;
    const FLAG_PROMO_QUEEN: u16 = 11 << Self::FLG_BITS;
    const FLAG_CAPTURE_PROMO_KNIGHT: u16 = 12 << Self::FLG_BITS;
    const FLAG_CAPTURE_PROMO_BISHOP: u16 = 13 << Self::FLG_BITS;
    const FLAG_CAPTURE_PROMO_ROOK: u16 = 14 << Self::FLG_BITS;
    const FLAG_CAPTURE_PROMO_QUEEN: u16 = 15 << Self::FLG_BITS;

    /// Creates a new [`Move`] from the given [`Square`]s and a [`MoveKind`].
    ///
    /// # Example
    /// ```
    /// # use toad::{Move, Square, MoveKind, PieceKind};
    /// let e2e4 = Move::new(Square::E2, Square::E4, MoveKind::PawnDoublePush);
    /// assert_eq!(e2e4.to_string(), "e2e4");
    ///
    /// let e7e8n = Move::new(Square::E7, Square::E8, MoveKind::promotion(PieceKind::Knight));
    /// assert_eq!(e7e8n.to_string(), "e7e8n");
    /// ```
    #[inline(always)]
    pub const fn new(from: Square, to: Square, kind: MoveKind) -> Self {
        Self(kind as u16 | (to.inner() as u16) << Self::DST_BITS | from.inner() as u16)
    }

    /// Creates an "illegal" [`Move`], representing moving a piece to and from the same [`Square`].
    ///
    /// Playing this move on a [`Position`] is *not* the same as playing a [null move](https://www.chessprogramming.org/Null_Move).
    ///
    /// # Example
    /// ```
    /// # use toad::Move;
    /// let illegal = Move::illegal();
    /// assert_eq!(illegal.to_string(), "a1a1");
    /// ```
    #[inline(always)]
    pub const fn illegal() -> Self {
        Self(0)
    }

    /// Fetches the source (or "from") part of this [`Move`], as a [`Square`].
    ///
    /// # Example
    /// ```
    /// # use toad::{Move, Square, MoveKind};
    /// let e2e4 = Move::new(Square::E2, Square::E4, MoveKind::PawnDoublePush);
    /// let from = e2e4.from();
    /// assert_eq!(from, Square::E2);
    /// ```
    #[inline(always)]
    pub const fn from(&self) -> Square {
        Square::from_bits_unchecked((self.0 & Self::SRC_MASK) as u8)
    }

    /// Fetches the destination (or "to") part of this [`Move`], as a [`Square`].
    ///
    /// # Example
    /// ```
    /// # use toad::{Move, Square, MoveKind};
    /// let e2e4 = Move::new(Square::E2, Square::E4, MoveKind::PawnDoublePush);
    /// let to = e2e4.to();
    /// assert_eq!(to, Square::E4);
    /// ```
    #[inline(always)]
    pub const fn to(&self) -> Square {
        Square::from_bits_unchecked(((self.0 & Self::DST_MASK) >> Self::DST_BITS) as u8)
    }

    /// Fetches the [`MoveKind`] part of this [`Move`].
    ///
    /// # Example
    /// ```
    /// # use toad::{Move, MoveKind, PieceKind, Square};
    /// let e7e8q = Move::new(Square::E7, Square::E8, MoveKind::promotion(PieceKind::Queen));
    /// assert_eq!(e7e8q.kind(), MoveKind::promotion(PieceKind::Queen));
    /// ```
    #[inline(always)]
    pub fn kind(&self) -> MoveKind {
        // Safety: Since a `Move` can ONLY be constructed through the public API,
        // any instance of a `Move` is guaranteed to have a valid bit pattern for its `MoveKind`.
        unsafe { std::mem::transmute(self.0 & Self::FLG_MASK) }
    }

    /// Returns `true` if this [`Move`] is a capture of any kind (capture, promotion-capture, en passant capture).
    ///
    /// # Example
    /// ```
    /// # use toad::{Move, Square, MoveKind, PieceKind, Game, FEN_KIWIPETE};
    /// let position = Game::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/Pp2P3/2N2Q1p/1PPBBPPP/R3K2R b KQkq a3 0 1").unwrap();
    ///
    /// let capture = Move::from_uci(&position, "b4c3").unwrap();
    /// assert_eq!(capture.is_capture(), true);
    ///
    /// let ep = Move::from_uci(&position, "b4a3").unwrap();
    /// assert_eq!(capture.is_capture(), true);
    /// ```
    #[inline(always)]
    pub const fn is_capture(&self) -> bool {
        self.0 & Self::FLAG_CAPTURE != 0
    }

    /// Returns `true` if this [`Move`] is a non-capture (quiet) move.
    ///
    /// # Example
    /// ```
    /// # use toad::{Move, Square, MoveKind, PieceKind, Game, FEN_KIWIPETE};
    /// let position = Game::from_fen(FEN_KIWIPETE).unwrap();
    ///
    /// let quiet = Move::from_uci(&position, "e1d1").unwrap();
    /// assert_eq!(quiet.is_quiet(), true);
    ///
    /// let castle = Move::from_uci(&position, "e1g1").unwrap();
    /// assert_eq!(castle.is_quiet(), true);
    ///
    /// let push = Move::from_uci(&position, "a2a4").unwrap();
    /// assert_eq!(push.is_quiet(), true);
    /// ```
    #[inline(always)]
    pub const fn is_quiet(&self) -> bool {
        self.0 & Self::FLAG_CAPTURE == 0
    }

    /// Returns `true` if this [`Move`] is en passant.
    #[inline(always)]
    pub const fn is_en_passant(&self) -> bool {
        (self.0 & Self::FLG_MASK) ^ Self::FLAG_EP_CAPTURE == 0
    }

    /// Returns `true` if this [`Move`] is a short (kingside) castle.
    #[inline(always)]
    pub const fn is_short_castle(&self) -> bool {
        (self.0 & Self::FLG_MASK) ^ Self::FLAG_CASTLE_SHORT == 0
    }

    /// Returns `true` if this [`Move`] is a long (queenside) castle.
    #[inline(always)]
    pub const fn is_long_castle(&self) -> bool {
        (self.0 & Self::FLG_MASK) ^ Self::FLAG_CASTLE_LONG == 0
    }

    // #[inline(always)]
    // pub const fn is_castle(&self) -> bool {
    //     self.is_short_castle() || self.is_long_castle()
    // }

    /// If this [`Move`] is a castling move, returns the [`File`]s of the destinations for the King and Rook, respectively.
    #[inline(always)]
    pub const fn castling_files(&self) -> Option<(File, File)> {
        if (self.0 & Self::FLG_MASK) ^ Self::FLAG_CASTLE_SHORT == 0 {
            Some((File::G, File::F))
        } else if (self.0 & Self::FLG_MASK) ^ Self::FLAG_CASTLE_LONG == 0 {
            Some((File::C, File::D))
        } else {
            None
        }
    }

    /// Returns `true` if this [`Move`] is a long (queenside) castle.
    ///
    /// # Example
    /// ```
    /// # use toad::{Move, Square, MoveKind};
    /// let e2e4 = Move::new(Square::E2, Square::E4, MoveKind::PawnDoublePush);
    /// assert_eq!(e2e4.is_pawn_double_push(), true);
    /// ```
    #[inline(always)]
    pub const fn is_pawn_double_push(&self) -> bool {
        (self.0 & Self::FLG_MASK) ^ Self::FLAG_PAWN_DOUBLE == 0
    }

    /*
    /// Returns `true` if this [`Move`] is a promotion of any kind.
    ///
    /// # Example
    /// ```
    /// # use toad::{Move, MoveKind, PieceKind, Square};
    /// let e7e8q = Move::new(Square::E7, Square::E8, MoveKind::promotion(PieceKind::Queen));
    /// assert_eq!(e7e8q.is_promotion(), true);
    ///
    /// let e7e8q = Move::new(Square::E7, Square::E8, MoveKind::promotion_capture(PieceKind::Queen));
    /// assert_eq!(e7e8q.is_promotion(), true);
    /// ```
    #[inline(always)]
    pub const fn is_promotion(&self) -> bool {
        // The flag bit for "promotion" is the most-significant bit.
        // Internally, FLAG_PROMO_KNIGHT has flag bits `1000`, so we can use it as a mask for promotions.
        self.0 & Self::FLAG_PROMO_KNIGHT != 0
    }
     */

    /// Returns `true` if this [`Move`] is a capture of any kind.
    ///
    /// # Example
    /// ```
    /// # use toad::{Move, Square, MoveKind, PieceKind, Game};
    /// // An sample test position for discovering promotion bugs.
    /// let position = Game::from_fen("n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ").unwrap();
    /// let b7c8b = Move::from_uci(&position, "b7c8b").unwrap();
    /// assert_eq!(b7c8b.promotion(), Some(PieceKind::Bishop));
    /// ```
    #[inline(always)]
    pub fn promotion(&self) -> Option<PieceKind> {
        match self.0 & Self::FLG_MASK {
            Self::FLAG_PROMO_QUEEN | Self::FLAG_CAPTURE_PROMO_QUEEN => Some(PieceKind::Queen),
            Self::FLAG_PROMO_KNIGHT | Self::FLAG_CAPTURE_PROMO_KNIGHT => Some(PieceKind::Knight),
            Self::FLAG_PROMO_ROOK | Self::FLAG_CAPTURE_PROMO_ROOK => Some(PieceKind::Rook),
            Self::FLAG_PROMO_BISHOP | Self::FLAG_CAPTURE_PROMO_BISHOP => Some(PieceKind::Bishop),
            _ => None,
        }
    }

    /// Creates a [`Move`] from a string, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation, extracting extra info from the provided [`Position`]
    ///
    /// Will return a [`anyhow::Error`] if the string is invalid in any way.
    ///
    /// # Example
    /// ```
    /// # use toad::*;
    /// let position = Game::from_fen("n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ").unwrap();
    /// let b7c8b = Move::from_uci(&position, "b7c8b");
    /// assert_eq!(b7c8b.unwrap(), Move::new(Square::B7, Square::C8, MoveKind::promotion_capture(PieceKind::Bishop)));
    ///
    /// // Automatically parses castling moves in standard notation to use the KxR format
    /// let position = Game::from_fen("r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1").unwrap();
    /// let e1c1 = Move::from_uci(&position, "e1c1");
    /// // Rook is on A1
    /// assert_eq!(e1c1.unwrap(), Move::new(Square::E1, Square::A1, MoveKind::LongCastle));
    /// ```
    pub fn from_uci(position: &Position, uci: &str) -> Result<Self> {
        // Extract the to/from squares
        let from = uci.get(0..2).ok_or(anyhow!(
            "Move str must contain a `from` square. Got {uci:?}"
        ))?;
        let to = uci
            .get(2..4)
            .ok_or(anyhow!("Move str must contain a `to` square. Got {uci:?}"))?;

        let from = Square::from_uci(from)?;
        let mut to = Square::from_uci(to)?;

        // Extract information about the piece being moved
        let piece = position.board().piece_at(from).ok_or(anyhow!(
            "No piece found at {from} when parsing {uci:?} on position {position}"
        ))?;

        // if piece.color() != position.side_to_move() {
        //     bail!("It is not {}'s turn to move", piece.color().name());
        // }

        // If there is a promotion char, attempt to convert it to a PieceKind
        let promotion = uci.get(4..5).map(PieceKind::from_str).transpose()?;

        // To potentially speed up `MoveKind::new`, we check for standard castling notation here, since it will only ever be important if we're parsing castling moves in standard notation.
        let color = position.side_to_move();
        let rank = Rank::first(color);

        // If this is a castle in UCI notation, change the destination square to the appropriate Rook
        if piece.is_king() && from == Square::new(File::E, rank) {
            if to == Square::new(File::G, rank) {
                to = position.castling_rights()[color].short.ok_or(anyhow!(
                    "Cannot castle {uci:?} because {} has no castling rights for that side",
                    color.name()
                ))?;
            } else if to == Square::new(File::C, rank) {
                to = position.castling_rights()[color].long.ok_or(anyhow!(
                    "Cannot castle {uci:?} because {} has no castling rights for that side",
                    color.name()
                ))?;
            }
        }

        // The MoveKind depends on what kind of piece is being moved and where
        let kind = MoveKind::new(piece, from, to, position, promotion);

        Ok(Self::new(from, to, kind))
    }

    /*
    pub fn from_san(position: &Position, san: &str) -> Result<Self> {
        // eprintln!("Parsing {san:?} for {}", position.to_fen());
        let color = position.side_to_move();
        let blockers = position.occupied();

        let mut from = Square::default();
        let to;
        let kind;

        // Handle castling first, since those are easy to parse
        if san == "O-O" {
            to = position.castling_rights()[color].short.unwrap();
            from = position.king(color).to_square_unchecked();
            return Ok(Self::new(from, to, MoveKind::ShortCastle));
        } else if san == "O-O-O" {
            to = position.castling_rights()[color].long.unwrap();
            from = position.king(color).to_square_unchecked();
            return Ok(Self::new(from, to, MoveKind::LongCastle));
        }

        let get_square = |s: &str, piece: Option<PieceKind>| -> Result<Square> {
            let square = s.parse()?;

            let mut origin = Err(anyhow!("No piece can move to {s:?}"));

            // Find the only piece that could move here.
            let kinds = if let Some(piece) = piece {
                &[piece][..]
            } else {
                &PieceKind::all()[..]
            };
            for kind in kinds {
                let piece = Piece::new(color, *kind);
                let attacks = attacks_for(piece, square, blockers);
                let overlap = attacks & position.piece(piece);
                if overlap.population() == 1 {
                    origin = Ok(overlap.to_square_unchecked());
                    eprintln!(
                        "{} on {} can move to {square}",
                        overlap.to_square_unchecked(),
                        piece.name()
                    );
                }
            }

            origin
        };

        // Now handle single squares
        if let Ok(square) = san.parse() {
            to = square;
            from = get_square(san, None)?;

            return Ok(Self::new(from, to, MoveKind::Quiet));
        }

        let mut chars = san.chars();

        let first_char = chars.next().ok_or(anyhow!("Cannot parse empty SAN move"))?;

        // If it's an uppercase char, find the piece associated with this move
        if first_char.is_uppercase() {
            let piece = Piece::new(color, PieceKind::from_uci(first_char)?);
            let bb = position.piece(piece);

            match bb.population() {
                0 => bail!("No {} found on the board", piece.name()),
                1 => from = bb.to_square_unchecked(),
                _ => {
                    //
                }
            }

            let next = chars
                .next()
                .ok_or(anyhow!("Expected more than just a single piece character"))?;

            if next == 'x' {
                kind = MoveKind::Capture;
            } else {
                kind = MoveKind::Quiet;
            }

            let to = Square::default();

            return Ok(Self::new(from, to, kind));
        }

        bail!("Could not parse SAN move {san:?}")
    }
     */

    /// Converts this [`Move`] to a string, according to the [Universal Chess Interface](https://en.wikipedia.org//wiki/Universal_Chess_Interface) notation.
    ///
    /// Promotions are capitalized by default, and castling is displayed in the standard `e1g1` and `e1c1` notation.
    ///
    /// # Example
    /// ```
    /// # use toad::{Move, Square, MoveKind, PieceKind};
    /// let e7e8Q = Move::new(Square::E7, Square::E8, MoveKind::promotion(PieceKind::Queen));
    /// assert_eq!(e7e8Q.to_uci(), "e7e8q");
    /// // Since castling is encoded as KxR, the `to` square is the Rook.
    /// let e1g1 = Move::new(Square::E1, Square::H1, MoveKind::ShortCastle);
    /// assert_eq!(e1g1.to_uci(), "e1g1")
    /// ```
    #[inline(always)]
    pub fn to_uci(&self) -> String {
        // Since castling is encoded internally as KxR, we need to adjust them for UCI notation
        let mv = self.into_standard_castle();
        if let Some(promote) = mv.promotion() {
            format!("{}{}{}", mv.from(), mv.to(), promote)
        } else {
            format!("{}{}", mv.from(), mv.to())
        }
    }

    /// Changes the `to` field of a castling move to align with the standard
    /// `e1g1` / `e1c1` / `e8g8` / `e8c8` notation.
    ///
    /// If `self` is not a castling move, this does not alter `self`.
    ///
    /// Internally, [`Move`] uses the Chess960 "King takes Rook" to represent castling.
    /// This is only ever an issue when you need to print moves,
    /// and luckily the [`fmt::Display`] implementation of [`Move`] allows you to toggle
    /// between standard and Chess960 notation with the alternate formatter (`#`).
    #[inline(always)]
    pub fn into_standard_castle(self) -> Self {
        if self.is_short_castle() {
            let new_to = Square::new(File::G, self.from().rank());
            Self::new(self.from(), new_to, MoveKind::ShortCastle)
        } else if self.is_long_castle() {
            let new_to = Square::new(File::C, self.from().rank());
            Self::new(self.from(), new_to, MoveKind::LongCastle)
        } else {
            // If this move isn't a castle, no modification needs to be done
            self
        }
    }
}

impl fmt::Display for Move {
    /// A [`Move`] is displayed in its UCI format.
    ///
    /// If the alternate format mode (`#`) was specified, this will print the castling moves in Chess960 notation.
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            if let Some(promote) = self.promotion() {
                write!(f, "{}{}{}", self.from(), self.to(), promote)
            } else {
                write!(f, "{}{}", self.from(), self.to())
            }
        } else {
            write!(f, "{}", self.to_uci())
        }
    }
}

impl fmt::Debug for Move {
    /// Debug formatting will call the [`fmt::Display`] implementation
    /// (taking into account the alternate formatter, if provided)
    /// and will also display it's [`MoveKind`] in a human-readable format.
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if f.alternate() {
            write!(f, "{self:#} ({})", self.kind())
        } else {
            write!(f, "{self} ({})", self.kind())
        }
    }
}

impl Default for Move {
    /// A "default" move is an illegal move. See [`Move::illegal`]
    ///
    /// This is mostly just to satisfy the compiler, and should never be used in a real scenario.
    #[inline(always)]
    fn default() -> Self {
        Self::illegal()
    }
}

impl<T: AsRef<str>> PartialEq<T> for Move {
    #[inline(always)]
    fn eq(&self, other: &T) -> bool {
        self.to_uci().eq(other.as_ref())
    }
}

#[cfg(test)]
mod test {

    use super::*;
    use crate::*;

    #[test]
    fn test_move_is_capture() {
        let (from, to) = (Square::A1, Square::H8);
        assert!(!Move::new(from, to, MoveKind::Quiet).is_capture());
        assert!(!Move::new(from, to, MoveKind::ShortCastle).is_capture());
        assert!(!Move::new(from, to, MoveKind::LongCastle).is_capture());
        assert!(!Move::new(from, to, MoveKind::PawnDoublePush).is_capture());
        assert!(Move::new(from, to, MoveKind::Capture).is_capture());
        assert!(Move::new(from, to, MoveKind::EnPassantCapture).is_capture());
        assert!(!Move::new(from, to, MoveKind::promotion(PieceKind::Queen)).is_capture());
        assert!(Move::new(from, to, MoveKind::promotion_capture(PieceKind::Queen)).is_capture());
    }

    #[test]
    fn test_move_is_quiet() {
        let (from, to) = (Square::A1, Square::H8);
        assert!(Move::new(from, to, MoveKind::Quiet).is_quiet());
        assert!(Move::new(from, to, MoveKind::ShortCastle).is_quiet());
        assert!(Move::new(from, to, MoveKind::LongCastle).is_quiet());
        assert!(Move::new(from, to, MoveKind::PawnDoublePush).is_quiet());
        assert!(!Move::new(from, to, MoveKind::Capture).is_quiet());
        assert!(!Move::new(from, to, MoveKind::EnPassantCapture).is_quiet());
        assert!(Move::new(from, to, MoveKind::promotion(PieceKind::Queen)).is_quiet());
        assert!(!Move::new(from, to, MoveKind::promotion_capture(PieceKind::Queen)).is_quiet());
    }

    #[test]
    fn test_move_is_en_passant() {
        let (from, to) = (Square::A1, Square::H8);
        assert!(!Move::new(from, to, MoveKind::Quiet).is_en_passant());
        assert!(!Move::new(from, to, MoveKind::ShortCastle).is_en_passant());
        assert!(!Move::new(from, to, MoveKind::LongCastle).is_en_passant());
        assert!(!Move::new(from, to, MoveKind::PawnDoublePush).is_en_passant());
        assert!(!Move::new(from, to, MoveKind::Capture).is_en_passant());
        assert!(Move::new(from, to, MoveKind::EnPassantCapture).is_en_passant());
        assert!(!Move::new(from, to, MoveKind::promotion(PieceKind::Queen)).is_en_passant());
        assert!(
            !Move::new(from, to, MoveKind::promotion_capture(PieceKind::Queen)).is_en_passant()
        );
    }

    #[test]
    fn test_move_is_short_castle() {
        let (from, to) = (Square::A1, Square::H8);
        assert!(!Move::new(from, to, MoveKind::Quiet).is_short_castle());
        assert!(Move::new(from, to, MoveKind::ShortCastle).is_short_castle());
        assert!(!Move::new(from, to, MoveKind::LongCastle).is_short_castle());
        assert!(!Move::new(from, to, MoveKind::PawnDoublePush).is_short_castle());
        assert!(!Move::new(from, to, MoveKind::Capture).is_short_castle());
        assert!(!Move::new(from, to, MoveKind::EnPassantCapture).is_short_castle());
        assert!(!Move::new(from, to, MoveKind::promotion(PieceKind::Queen)).is_short_castle());
        assert!(
            !Move::new(from, to, MoveKind::promotion_capture(PieceKind::Queen)).is_short_castle()
        );
    }

    #[test]
    fn test_move_is_long_castle() {
        let (from, to) = (Square::A1, Square::H8);
        assert!(!Move::new(from, to, MoveKind::Quiet).is_long_castle());
        assert!(!Move::new(from, to, MoveKind::ShortCastle).is_long_castle());
        assert!(Move::new(from, to, MoveKind::LongCastle).is_long_castle());
        assert!(!Move::new(from, to, MoveKind::PawnDoublePush).is_long_castle());
        assert!(!Move::new(from, to, MoveKind::Capture).is_long_castle());
        assert!(!Move::new(from, to, MoveKind::EnPassantCapture).is_long_castle());
        assert!(!Move::new(from, to, MoveKind::promotion(PieceKind::Queen)).is_long_castle());
        assert!(
            !Move::new(from, to, MoveKind::promotion_capture(PieceKind::Queen)).is_long_castle()
        );
    }

    /*
    #[test]
    fn test_move_is_castle() {
        let (from, to) = (Square::A1, Square::H8);
        assert!(!Move::new(from, to, MoveKind::Quiet).is_castle());
        assert!(Move::new(from, to, MoveKind::ShortCastle).is_castle());
        assert!(Move::new(from, to, MoveKind::LongCastle).is_castle());
        assert!(!Move::new(from, to, MoveKind::PawnDoublePush).is_castle());
        assert!(!Move::new(from, to, MoveKind::Capture).is_castle());
        assert!(!Move::new(from, to, MoveKind::EnPassantCapture).is_castle());
        assert!(!Move::new(from, to, MoveKind::promotion(PieceKind::Queen)).is_castle());
        assert!(!Move::new(from, to, MoveKind::promotion_capture(PieceKind::Queen)).is_castle());
    }
     */

    #[test]
    fn test_move_is_pawn_double_push() {
        let (from, to) = (Square::A1, Square::H8);
        assert!(!Move::new(from, to, MoveKind::Quiet).is_pawn_double_push());
        assert!(!Move::new(from, to, MoveKind::ShortCastle).is_pawn_double_push());
        assert!(!Move::new(from, to, MoveKind::LongCastle).is_pawn_double_push());
        assert!(Move::new(from, to, MoveKind::PawnDoublePush).is_pawn_double_push());
        assert!(!Move::new(from, to, MoveKind::Capture).is_pawn_double_push());
        assert!(!Move::new(from, to, MoveKind::EnPassantCapture).is_pawn_double_push());
        assert!(!Move::new(from, to, MoveKind::promotion(PieceKind::Queen)).is_pawn_double_push());
        assert!(
            !Move::new(from, to, MoveKind::promotion_capture(PieceKind::Queen))
                .is_pawn_double_push()
        );
    }

    /*
    #[test]
    fn test_move_is_promotion() {
        let (from, to) = (Square::A1, Square::H8);
        assert!(!Move::new(from, to, MoveKind::Quiet).is_promotion());
        assert!(!Move::new(from, to, MoveKind::ShortCastle).is_promotion());
        assert!(!Move::new(from, to, MoveKind::LongCastle).is_promotion());
        assert!(!Move::new(from, to, MoveKind::PawnDoublePush).is_promotion());
        assert!(!Move::new(from, to, MoveKind::Capture).is_promotion());
        assert!(!Move::new(from, to, MoveKind::EnPassantCapture).is_promotion());
        assert!(Move::new(from, to, MoveKind::promotion(PieceKind::Queen)).is_promotion());
        assert!(Move::new(from, to, MoveKind::promotion_capture(PieceKind::Queen)).is_promotion());
    }
     */

    /// Helper function to assert that the `uci` move is parsed as `expected` on the position created from `fen`.
    fn test_move_parse(fen: &str, uci: &str, expected: Move) {
        let pos = fen.parse::<Game>().unwrap();

        let mv = Move::from_uci(&pos, uci);
        assert!(mv.is_ok(), "{}", mv.unwrap_err());
        let mv = mv.unwrap();
        assert_eq!(mv, expected, "{mv:?} is incorrect for {fen}");
    }

    #[test]
    fn test_move_parsing() {
        // We can test all moves except castling with Pawns
        let pawn_fen = "2n1k3/1P6/8/5pP1/5n2/2P1P3/P7/4K3 w - f6 0 1";

        // Pawn single push
        let mv = Move::new(Square::A2, Square::A3, MoveKind::Quiet);
        test_move_parse(pawn_fen, "a2a3", mv);

        // Pawn double push
        let mv = Move::new(Square::A2, Square::A4, MoveKind::PawnDoublePush);
        test_move_parse(pawn_fen, "a2a4", mv);

        // Pawn capture
        let mv = Move::new(Square::E3, Square::F4, MoveKind::Capture);
        test_move_parse(pawn_fen, "e3f4", mv);

        // Pawn en passant capture
        let mv = Move::new(Square::G5, Square::F6, MoveKind::EnPassantCapture);
        test_move_parse(pawn_fen, "g5f6", mv);

        // Pawn promotion to queen
        let mv = Move::new(
            Square::B7,
            Square::B8,
            MoveKind::promotion(PieceKind::Queen),
        );
        test_move_parse(pawn_fen, "b7b8Q", mv);

        // Pawn promotion to Knight
        let mv = Move::new(
            Square::B7,
            Square::B8,
            MoveKind::promotion(PieceKind::Knight),
        );
        test_move_parse(pawn_fen, "b7b8n", mv);

        // Pawn capture promotion to queen
        let mv = Move::new(
            Square::B7,
            Square::C8,
            MoveKind::promotion_capture(PieceKind::Queen),
        );
        test_move_parse(pawn_fen, "b7c8q", mv);

        // Pawn capture promotion to Knight
        let mv = Move::new(
            Square::B7,
            Square::C8,
            MoveKind::promotion_capture(PieceKind::Knight),
        );
        test_move_parse(pawn_fen, "b7c8N", mv);

        // Now test castling
        let king_fen = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1";

        // Kingside (short) castling (White)
        let mv = Move::new(Square::E1, Square::H1, MoveKind::ShortCastle);
        test_move_parse(king_fen, "e1g1", mv);

        // Queenside (long) castling (White)
        let mv = Move::new(Square::E1, Square::A1, MoveKind::LongCastle);
        test_move_parse(king_fen, "e1c1", mv);

        // Same fen, just Black's turn to move
        let king_fen = "r3k2r/8/8/8/8/8/8/R3K2R b KQkq - 0 1";

        // Kingside (short) castling (Black)
        let mv = Move::new(Square::E8, Square::H8, MoveKind::ShortCastle);
        test_move_parse(king_fen, "e8g8", mv);

        // Queenside (long) castling (Black)
        let mv = Move::new(Square::E8, Square::A8, MoveKind::LongCastle);
        test_move_parse(king_fen, "e8c8", mv);
    }

    /*
        fn test_parse_san() {
            let fen = "r2qkb1r/ppp2ppp/2n1pn2/8/2PP4/2NbBN2/PP3PPP/R2QK2R w KQkq - 0 1";
            let mut pos = Game::from_fen(fen).unwrap();
            let mut moves = "
    Qxd3 Bd6
    Rd1  O-O
    c5   Be7
    O-O  h6
    h3   Re8
    a4   a5
    Rfe1 Rc8
    Qb5  Nb4
    Qxa5 Nc2
    Re2  b6
    Qb5  Nxe3
    Rxe3 bxc5
    dxc5 c6
    Rxd8 cxb5
    Rxc8 Rxc8"
                .split_ascii_whitespace();

            for san in moves {
                let mv = Move::from_san(&pos, san).unwrap();
                pos.make_move(mv);
            }
        }
         */
}
