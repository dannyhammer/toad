/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{
    fmt::{self, Debug, Write},
    ops::{Deref, Index, IndexMut},
    str::FromStr,
};

use anyhow::{anyhow, bail, Result};

use crate::{
    bishop_attacks, bishop_rays, king_attacks, knight_attacks, pawn_attacks, pawn_pushes,
    queen_attacks, ray_between, ray_containing, rook_attacks, rook_rays, Bitboard, Color,
    Evaluator, File, Move, MoveKind, MoveList, Piece, PieceKind, Rank, Score, SmallDisplayTable,
    Square, ZobristKey,
};

use super::Table;

/// FEN string for the starting position of chess.
pub const FEN_STARTPOS: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

/// A popular FEN string for debugging move generation.
pub const FEN_KIWIPETE: &str =
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";

/// Variant of chess being played by the Engine.
#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Copy)]
pub enum GameVariant {
    /// Standard chess.
    #[default]
    Standard,

    /// Fischer Random chess.
    ///
    /// The home ranks of each color are shuffled into one of 960 possible starting positions.
    Chess960,
    /*
    /// Horde chess.
    ///
    /// Black plays as normal, while White hahs 36 Pawns and no other pieces.
    /// Black must eliminate all of White's pieces, while White must checkmate Black's King.
    ///
    Horde,
    */
}

/// Abstraction over the specific chess variant being played.
///
/// Different chess variants have slightly different rules.
/// For example, castling moves are printed differently in Chess960 than in standard chess.
pub trait Variant
where
    Self: Copy + Send + 'static,
{
    /// Initial material value of all pieces in a standard setup.
    const INITIAL_MATERIAL_VALUE: i32;

    /// Formats a [`Move`] according to this variant's notation semantics.
    ///
    /// Calls the [`fmt::Display`] implementation.
    #[inline(always)]
    fn fmt_move(mv: Move) -> String {
        format!("{mv}")
    }

    /// Formats a [`Move`] according to this variant's notation semantics.
    ///
    /// Calls the [`fmt::Debug`] implementation.
    #[inline(always)]
    fn dbg_move(mv: Move) -> String {
        format!("{mv:?}")
    }

    /// Fetch the [`GameVariant`] value of this variant.
    fn variant() -> GameVariant;

    /// Fetch the FEN string of the starting position of this variant.
    fn fen_startpos() -> &'static str;

    /// Formats the castling rights of this variant into a string.
    #[inline(always)]
    fn fmt_castling_rights(rights: &[CastlingRights; Color::COUNT]) -> String {
        let mut castling = String::with_capacity(4);
        if rights[Color::White].short.is_some() {
            castling.push('K');
        }
        if rights[Color::White].long.is_some() {
            castling.push('Q');
        }
        if rights[Color::Black].short.is_some() {
            castling.push('k');
        }
        if rights[Color::Black].long.is_some() {
            castling.push('q');
        }

        // If no side can castle, use a hyphen
        if castling.is_empty() {
            castling.push('-');
        }
        castling
    }
}

/// Marker type for standard chess.
#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Standard;
impl Variant for Standard {
    const INITIAL_MATERIAL_VALUE: i32 = PieceKind::Pawn.value() * 16
        + PieceKind::Knight.value() * 4
        + PieceKind::Bishop.value() * 4
        + PieceKind::Rook.value() * 4
        + PieceKind::Queen.value() * 2;

    #[inline(always)]
    fn variant() -> GameVariant {
        GameVariant::Standard
    }

    #[inline(always)]
    fn fen_startpos() -> &'static str {
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
    }
}

/// Marker type for chess 960.
#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Chess960;
impl Variant for Chess960 {
    const INITIAL_MATERIAL_VALUE: i32 = PieceKind::Pawn.value() * 16
        + PieceKind::Knight.value() * 4
        + PieceKind::Bishop.value() * 4
        + PieceKind::Rook.value() * 4
        + PieceKind::Queen.value() * 2;

    #[inline(always)]
    fn fmt_move(mv: Move) -> String {
        format!("{mv:#}")
    }

    #[inline(always)]
    fn dbg_move(mv: Move) -> String {
        format!("{mv:#?}")
    }

    #[inline(always)]
    fn variant() -> GameVariant {
        GameVariant::Chess960
    }

    /// This uses the standard chess starting position, just with castling rights formatted as Files, rather than `K`/`Q`.
    #[inline(always)]
    fn fen_startpos() -> &'static str {
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w HAha - 0 1"
    }

    #[inline(always)]
    fn fmt_castling_rights(rights: &[CastlingRights; Color::COUNT]) -> String {
        let mut castling = String::with_capacity(4);

        if let Some(sq) = rights[Color::White].short {
            castling.push(sq.file().char().to_ascii_uppercase());
        }

        if let Some(sq) = rights[Color::White].long {
            castling.push(sq.file().char().to_ascii_uppercase());
        }

        if let Some(sq) = rights[Color::Black].short {
            castling.push(sq.file().char());
        }

        if let Some(sq) = rights[Color::Black].long {
            castling.push(sq.file().char());
        }

        // If no side can castle, use a hyphen
        if castling.is_empty() {
            castling.push('-');
        }
        castling
    }
}

/*
/// Marker type for horde chess.
#[derive(Debug, Default, PartialEq, Eq, Hash, Clone, Copy)]
pub struct Horde;
impl Variant for Horde {
    const INITIAL_MATERIAL_VALUE: [i32; Color::COUNT] = [PieceKind::Pawn.value() * 32,
        PieceKind::Pawn.value() * 8
        + PieceKind::Knight.value() * 2
        + PieceKind::Bishop.value() * 2
        + PieceKind::Rook.value() * 2
        + PieceKind::Queen.value() * 1;
    ];

    #[inline(always)]
    fn variant() -> GameVariant {
        GameVariant::Horde
    }

    #[inline(always)]
    fn fen_startpos() -> &'static str {
        "rnbqkbnr/pppppppp/8/1PP2PP1/PPPPPPPP/PPPPPPPP/PPPPPPPP/PPPPPPPP w kq - 0 1"
    }
}
 */

/// A game of chess.
///
/// This type encapsulates a [`Position`] and adds metadata about piece movements, such as all pieces currently checking the side-to-move's King.
/// It is the primary type for working with a chess game, and is suitable for use in engines.
///
/// The basic methods you're probably looking for are [`Game::<Standard>::from_fen`], [`Game::make_move`], and [`Game::get_legal_moves`].
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Game<V: Variant> {
    /// The current [`Position`] of the game, including piece layouts, castling rights, turn counters, etc.
    position: Position,

    /// All squares whose pieces are attacking the side-to-move's King.
    checkers: Bitboard,

    /// If `self.checkers` is empty, this is a bitboard of all squares that are not occupied by friendly pieces.
    /// Otherwise, it is the path from every checker to the side-to-move's King.
    /// Because, if we're in check, we must either capture or block the checker.
    checkmask: Bitboard,

    /// All pieces that are the sole blocker between the King and an enemy slider.
    pinned: Bitboard,

    /// All squares (pseudo-legally) attacked by a specific color.
    attacks_by_color: [Bitboard; Color::COUNT],

    /*
    /// Pseudo-legal moves (including Pawn pushes and castles) from every given square on the board.
    // mobility_at: [Bitboard; Square::COUNT],
     */
    /// The square where the side-to-move's King resides.
    king_square: Square,

    /// Responsible for evaluating the current position into a [`Score`].
    evaluator: Evaluator<V>,
}

/// Implementation details specific to standard chess.
impl Game<Standard> {
    //
}

/// Implementation details specific to chess 960.
impl Game<Chess960> {
    //
}

/*
/// Implementation details specific to horde chess.
impl Game<Horde> {
    //
}
 */

impl<V: Variant> Game<V> {
    /// Creates a new, empty [`Game`] with the following properties:
    /// * No pieces on the board
    /// * White moves first
    /// * No castling rights
    /// * No en passant square available
    /// * Halfmove counter set to 0
    /// * Fullmove counter set to 1
    ///
    /// # Example
    /// ```
    /// # use toad::*;
    /// let empty = Game::<Standard>::new();
    /// assert_eq!(empty.to_fen(), "8/8/8/8/8/8/8/8 w - - 0 1");
    /// ```
    #[inline(always)]
    pub fn new() -> Self {
        Self {
            king_square: Square::default(),
            position: Position::new(),
            checkers: Bitboard::EMPTY_BOARD,
            checkmask: Bitboard::EMPTY_BOARD,
            pinned: Bitboard::EMPTY_BOARD,
            attacks_by_color: [Bitboard::EMPTY_BOARD; Color::COUNT],
            evaluator: Evaluator::new(),
        }
    }

    /// Creates a new [`Game`] from the provided FEN string.
    #[inline(always)]
    pub fn from_fen(fen: &str) -> Result<Self> {
        let mut game = Self::new();

        let mut split = fen.trim().split(' ');
        let Some(placements) = split.next() else {
            bail!("FEN string must have piece placements");
        };

        // Check if the placements string is the correct length
        if placements.matches('/').count() != 7 {
            bail!("FEN must have piece placements for all 8 ranks");
        }

        let mut king_files = [File::E; Color::COUNT];
        // Need to reverse this so that White pieces are at the "bottom" of the board
        for (rank, placements) in placements.split('/').rev().enumerate() {
            let mut file = 0;
            let rank = rank as u8;

            for piece_char in placements.chars() {
                // If the next char is a piece, we need to update the relevant Bitboards
                if let Ok(piece) = Piece::from_uci(piece_char) {
                    // Place the appropriate piece at this square
                    let square = Square::new(File::new_unchecked(file), Rank::new_unchecked(rank));

                    game.place(piece, square);

                    // Keep track of the Kings, for castling rights in (D)FRC
                    if piece.is_king() {
                        king_files[piece.color()] = File::new_unchecked(file);
                    }

                    file += 1;
                } else {
                    // If the next char was not a piece, increment our File counter, checking for errors along the way
                    let Some(empty) = piece_char.to_digit(10) else {
                        bail!(
                            "FEN placements must contain piece chars or digits. Got {piece_char:?}"
                        );
                    };
                    file += empty as u8
                }
            }
        }

        let active_color = split.next().unwrap_or("w");
        game.position.side_to_move = Color::from_str(active_color)?;

        // Castling is a bit more complicated; especially for Chess960
        let castling = split.next().unwrap_or("-");
        let mut rights = [CastlingRights::default(); Color::COUNT];
        // Parse castling rights, if they exist
        for c in castling.replace("-", "").chars() {
            let color = Color::from_case(c);

            // If we can parse the char into a file, use it
            let file = if let Ok(file) = File::from_char(c) {
                file
            } else
            // Otherwise, check if it's in the default notation for short ("kingside") castling
            if c.to_ascii_lowercase() == 'k' {
                File::H
            } else
            // Same for long ("queenside")
            if c.to_ascii_lowercase() == 'q' {
                File::A
            } else {
                bail!("Castling chars must be either valid files or [K, Q, k, q]. Got {c:?}");
            };

            // Files higher than the one on which the King resides are "short" castles. (g > e)
            if file > king_files[color] {
                rights[color].short = Some(Square::new(file, Rank::first(color)));
            } else {
                rights[color].long = Some(Square::new(file, Rank::first(color)));
            }
        }
        game.position.castling_rights = rights;

        let en_passant_target = split.next().unwrap_or("-");
        game.position.ep_square = match en_passant_target {
            "-" => None,
            square => Some(Square::from_uci(square)?),
        };

        let halfmove = split.next().unwrap_or("0");
        game.position.halfmove = halfmove.parse().or(Err(anyhow!(
            "FEN string must have valid halfmove counter. Got {halfmove:?}"
        )))?;

        let fullmove = split.next().unwrap_or("1");
        game.position.fullmove = fullmove.parse().or(Err(anyhow!(
            "FEN string must have valid fullmove counter. Got {fullmove:?}"
        )))?;

        game.position.key = ZobristKey::new(game.position());

        game.recompute_legal_masks();
        Ok(game)
    }

    /// Generates a FEN string from this [`Position`].
    ///
    /// The `is_chess960` parameter will determine whether to print castling rights in Chess960 notation.
    ///
    /// # Example
    /// ```
    /// # use toad::*;
    /// let state = Game::<Standard>::default();
    /// assert_eq!(state.to_fen(), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    ///
    /// let state = Game::<Chess960>::default();
    /// assert_eq!(state.to_fen(), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w HAha - 0 1");
    /// ```
    pub fn to_fen(&self) -> String {
        let placements = self.board().to_fen();
        let active_color = self.side_to_move();
        let castling = V::fmt_castling_rights(self.castling_rights());
        let en_passant = self
            .ep_square()
            .map(|ep| ep.to_string())
            .unwrap_or(String::from("-"));
        let halfmove = self.halfmove;
        let fullmove = self.fullmove;

        format!("{placements} {active_color} {castling} {en_passant} {halfmove} {fullmove}")
    }

    /// Copies `self` and returns a [`Game`] after having applied the provided [`Move`].
    #[inline(always)]
    pub fn with_move_made(&self, mv: Move) -> Self {
        let mut copied = *self;
        copied.make_move(mv);
        copied
    }

    /// Copies `self` and returns a [`Game`] after having applied a nullmove
    #[inline(always)]
    pub fn with_nullmove_made(&self) -> Self {
        let mut copied = *self;
        copied.make_nullmove();
        copied
    }

    /*
    /// Returns `true` if the game is in a position that is identical to a position it has been in before.
    ///
    /// This is useful for checking repetitions.
    ///
    ///
    /// # Example
    /// ```
    /// # use toad::{Game, Move};
    /// let mut game = Game::default();
    /// game.make_move(Move::from_uci(&game, "b1a3").unwrap());
    /// assert_eq!(game.is_repetition(), false);
    /// game.make_move(Move::from_uci(&game, "b8a6").unwrap());
    /// assert_eq!(game.is_repetition(), false);
    /// game.make_move(Move::from_uci(&game, "a3b1").unwrap());
    /// assert_eq!(game.is_repetition(), false);
    /// game.make_move(Move::from_uci(&game, "a6b8").unwrap());
    /// assert_eq!(game.is_repetition(), true);
    /// ```
    pub fn is_repetition(&self) -> bool {
        for prev in self.history.iter().rev().skip(1).step_by(2) {
            if *prev == self.key() {
                return true;
            }
        }

        false
    }
     */

    /// Toggles the side to move of the current position.
    ///
    /// This is equivalent to playing a nullmove.
    ///
    /// # Example
    /// ```
    /// # use toad::*;
    /// let mut game = Game::<Standard>::default();
    /// assert_eq!(game.to_fen(), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    /// game.toggle_side_to_move();
    /// assert_eq!(game.to_fen(), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1");
    /// ```
    #[inline(always)]
    pub fn toggle_side_to_move(&mut self) {
        self.position.toggle_side_to_move();
        self.recompute_legal_masks();
    }

    /// Returns `true` if the side-to-move is currently in check.
    #[inline(always)]
    pub const fn is_in_check(&self) -> bool {
        self.checkers.population() > 0
    }

    /// Returns a [`Bitboard`] of all squares attacked by `color`.
    #[inline(always)]
    pub const fn attacks_by(&self, color: Color) -> Bitboard {
        self.attacks_by_color[color.index()]
    }

    /// Evaluate this position from the side-to-move's perspective.
    ///
    /// A positive/high number is good for the side-to-move, while a negative number is better for the opponent.
    /// A score of 0 is considered equal.
    #[inline(always)]
    pub fn eval(&self) -> Score {
        let stm = self.side_to_move();
        self.evaluator().eval_for(stm)
    }

    /// Applies the provided [`Move`]. No enforcement of legality.
    #[inline(always)]
    pub fn make_move(&mut self, mv: Move) {
        // Remove the piece from it's previous location, exiting early if there is no piece there
        let Some(mut piece) = self.take(mv.from()) else {
            return;
        };

        let color = piece.color();
        let mut to = mv.to();
        let from = mv.from();

        // Un-hash the side-to-move
        self.position.key.hash_side_to_move(self.side_to_move());

        // Clear the EP square from the last move (and un-hash it)
        if let Some(ep_square) = self.position.ep_square.take() {
            self.position.key.hash_ep_square(ep_square);
        }

        // Increment move counters
        self.position.halfmove += 1; // This is reset if a capture occurs or a pawn moves
        self.position.fullmove += self.side_to_move().bits();

        // First, deal with special cases like captures and castling
        if mv.is_capture() {
            // If this move was en passant, the piece we captured isn't at `to`, it's one square behind
            let victim_square = if mv.is_en_passant() {
                // Safety: En passant cannot occur on the first or eighth rank, so this is guaranteed to have a square behind it.
                unsafe { to.backward_by(color, 1).unwrap_unchecked() }
            } else {
                to
            };

            // Safety: This is a capture; there *must* be a piece at the destination square.
            // let victim = self.take(victim_square).unwrap();
            let Some(victim) = self.take(victim_square) else {
                panic!(
                    "No piece to capture at {victim_square} in move {mv} on {:?}",
                    self.to_fen()
                );
            };
            // let victim = unsafe { self.take(victim_square).unwrap_unchecked() };
            let victim_color = victim.color();

            // If the capture was on a rook's starting square, disable that side's castling.
            let rights = self.castling_rights_for(victim_color);
            if rights.long.is_some_and(|sq| victim_square == sq) {
                self.position.clear_long_castling_rights(victim_color);
            } else if rights.short.is_some_and(|sq| victim_square == sq) {
                self.position.clear_short_castling_rights(victim_color);
            }

            // Reset halfmove counter, since a capture occurred
            self.position.halfmove = 0;
        } else if mv.is_pawn_double_push() {
            // Double pawn push, so set the EP square
            self.position.ep_square = from.forward_by(color, 1);
            self.position.key.hash_optional_ep_square(self.ep_square());
        } else if mv.is_short_castle() {
            // Safety; This is a castle. There *must* be a Rook at `to`.
            // let rook = unsafe { self.take(to).unwrap_unchecked() };
            let rook = self.take(to).unwrap();
            self.place(rook, Square::rook_short_castle(color));

            // The King doesn't actually move to the Rook, so update the destination square
            to = Square::king_short_castle(color);

            // Disable castling
            self.position.clear_castling_rights(color);
        } else if mv.is_long_castle() {
            // Safety; This is a castle. There *must* be a Rook at `to`.
            // let rook = unsafe { self.take(to).unwrap_unchecked() };
            let rook = self.take(to).unwrap();
            self.place(rook, Square::rook_long_castle(color));

            // The King doesn't actually move to the Rook, so update the destination square
            to = Square::king_long_castle(color);

            // Disable castling
            self.position.clear_castling_rights(color);
        }

        // Next, handle special cases for Pawn (halfmove), Rook, and King (castling)
        match piece.kind() {
            PieceKind::Pawn => self.position.halfmove = 0,

            // Disable castling if a rook moved for the first time
            PieceKind::Rook => {
                let rights = self.castling_rights_for(color);
                if rights.long.is_some_and(|sq| from == sq) {
                    self.position.clear_long_castling_rights(color);
                } else if rights.short.is_some_and(|sq| from == sq) {
                    self.position.clear_short_castling_rights(color);
                }
            }

            PieceKind::King => self.position.clear_castling_rights(color),

            _ => {}
        }

        // Now we check for promotions, since all special cases for Pawns and Rooks have been dealt with
        if let Some(promotion) = mv.promotion() {
            piece = piece.promoted(promotion);
        }

        // Place the piece in it's new position
        self.place(piece, to);

        // Next player's turn
        self.toggle_side_to_move();

        // Toggle the hash of the current player
        self.position.key.hash_side_to_move(self.side_to_move());

        // Now update movegen metadata
        self.recompute_legal_masks();
    }

    /// Applies a nullmove to the board.
    ///
    ///
    /// This toggles the side-to-move, recomputes legal movegen data,
    /// and increments the halfmove (for draw detection).
    /// It does **NOT** increment the fullmove counter.
    #[inline(always)]
    pub fn make_nullmove(&mut self) {
        // Clear the EP square from the last move (and un-hash it)
        if let Some(ep_square) = self.position.ep_square.take() {
            self.position.key.hash_ep_square(ep_square);
        }

        // A nullmove just changes the side-to-move, that's all
        self.position.key.hash_side_to_move(self.side_to_move());
        self.toggle_side_to_move();
        self.position.key.hash_side_to_move(self.side_to_move());

        // Also increment the halfmove counter,
        self.position.halfmove += 1;

        // Now update movegen metadata
        self.recompute_legal_masks();
    }

    /// Makes a move in UCI notation on the board, if possible.
    ///
    /// Will return `Err` if `mv_str` is not valid for the current position.
    #[inline(always)]
    pub fn make_move_uci(&mut self, mv_str: &str) -> Result<()> {
        let mv = Move::from_uci(self, mv_str)?;
        self.make_move(mv);
        Ok(())
    }

    /// Fetch the internal [`Position`] of this [`Game`].
    #[inline(always)]
    pub const fn position(&self) -> &Position {
        &self.position
    }

    /// Fetch a [`Bitboard`] of all squares currently putting the side-to-move's King in check.
    #[inline(always)]
    pub const fn checkers(&self) -> Bitboard {
        self.checkers
    }

    /// Fetch a [`Bitboard`] of all squares that a piece can legally move to.
    #[inline(always)]
    pub const fn checkmask(&self) -> Bitboard {
        self.checkmask
    }

    /// Fetch a [`Bitboard`] of all squares occupied by pinned pieces.
    #[inline(always)]
    pub const fn pinned(&self) -> Bitboard {
        self.pinned
    }

    /// Fetch a reference to this game's [`Evaluator`].
    #[inline(always)]
    pub fn evaluator(&self) -> &Evaluator<V> {
        &self.evaluator
    }

    /// Checks if playing the provided [`Move`] is legal on the current position.
    ///
    /// This assumes the move is pseudo-legal. i.e. not capturing friendly pieces,
    /// not moving Pawns like Rooks, etc.
    ///
    /// This aims to be faster than playing the move and recalculating checkmasks and
    /// whatnot by manually moving pieces around and recalculating enemy attacks.
    pub fn is_legal(&self, mv: Move) -> bool {
        let from = mv.from();
        // If there isn't a piece here, this move isn't legal
        let Some(piece) = self.piece_at(from) else {
            return false;
        };

        let to = mv.to();
        let color = piece.color();
        let opponent = color.opponent();

        // Castling requires the path to be safe and clear
        if let Some((king_file, rook_file)) = mv.castling_files() {
            // Get the destinations for the King and Rook
            let rank = from.rank();
            let king_dst = Square::new(king_file, rank);
            let rook_dst = Square::new(rook_file, rank);

            // The King and Rook are not considered blockers, because they move through each other
            let blockers = self.occupied() ^ from ^ to;

            // All squares between the King and his destination (inclusive) must be empty
            if (ray_between(from, king_dst) | king_dst).intersects(blockers) {
                // eprintln!("{mv}: King {from} -> {king_dst} is not empty");
                return false;
            }

            // All squares between the Rook and its destination (inclusive) must be empty
            if (ray_between(to, rook_dst) | rook_dst).intersects(blockers) {
                // eprintln!("{mv}: Rook {to} -> {rook_dst} is not empty");
                return false;
            }

            // All squares between the King (inclusive) and his destination (inclusive) must not be attacked
            let enemy_attacks = self.attacks_by(opponent);
            if (ray_between(from, king_dst) | king_dst | from).intersects(enemy_attacks) {
                // eprintln!("{mv}: {from} -> {king_dst} is unsafe");
                return false;
            }

            // The Rook cannot be pinned (in Chess960)
            if self.pinned().intersects(to) {
                // eprintln!("{mv}: Rook on {to} is pinned");
                return false;
            }

            // Castling is legal
            return true;
        } else
        // After performing en passant, is our King in check?
        if mv.is_en_passant() {
            // Cant perform en passant if it's not available
            let Some(ep_square) = self.ep_square() else {
                // eprintln!("{mv}: No en passant square available");
                return false;
            };

            // If this Pawn isn't on an adjacent file and the same rank as the enemy Pawn that caused en passant to be possible, it can't perform en passant
            if from.distance_chebyshev(ep_square) != 1 {
                // eprintln!("{mv}: Pawn on {from} cannot perform en passant (too far away)");
                return false;
            }

            let ep_bb = ep_square.bitboard();
            let ep_target_bb = ep_bb.backward_by(color, 1);
            let blockers_after_ep = (self.occupied() ^ ep_target_bb ^ from) | ep_bb;

            // If, after performing EP, any sliders can attack our King, EP is not legal
            return bishop_attacks(self.king_square, blockers_after_ep)
                .is_disjoint(self.diagonal_sliders(opponent))
                && rook_attacks(self.king_square, blockers_after_ep)
                    .is_disjoint(self.orthogonal_sliders(opponent));
        } else
        // Must move to either an empty square or one occupied by a non-King enemy piece.
        if (self.enemy_or_empty(color) ^ self.king(opponent)).is_disjoint(to) {
            // eprintln!("{mv}: captures a friendly piece or enemy King");
            return false;
        }

        // If the King is moving, make sure he is moving somewhere safe
        if piece.is_king() {
            // Safe squares are ones not attacked by the enemy or part of a discoverable check
            return (self.attacks_by(opponent) | self.generate_discoverable_checks_bitboard(color))
                .is_disjoint(to);
        }

        // So long as we're not in double check, a piece can legally move within its pinmask and the checkmask
        if self.checkers().population() < 2 {
            let mut legal_squares = self.checkmask();

            // If we're pinned, we must not leave the ray on which we are pinned
            if self.pinned().intersects(from) {
                legal_squares &= ray_containing(from, self.king_square)
            }

            legal_squares.intersects(to)
        } else {
            // If we ARE in double-check, only the King can move
            // Since we already handled King movement, this move is illegal
            false
        }

        // Catch-all: Make the move, then see if the enemy can attack our King
        // let new = self.with_move_made(mv);
        // return new
        //     .attacks_by(color.opponent())
        //     .is_disjoint(new.king(color));
    }

    /// Generate all legal moves from the current position.
    ///
    /// If you need all legal moves for the position, use this method.
    #[inline(always)]
    pub fn get_legal_moves(&self) -> MoveList {
        let friendlies = self.color(self.side_to_move());
        self.get_legal_moves_from(friendlies)
    }

    /// Generate all legal moves from the current position that originate from squares in `mask`.
    ///
    /// Important note: If you call this method on a position that is in double-check with a
    /// mask that does _not_ include the King who is in check, no moves will be generated.
    /// This is intentional behavior, as only the King can move when in double-check.
    ///
    /// # Example
    /// ```
    /// use toad::*;
    /// let game = Game::<Standard>::default();
    /// let mask = game.knight(Color::White);
    /// let mut knight_moves = game.get_legal_moves_from(mask).into_iter();
    ///
    /// assert_eq!(knight_moves.next().unwrap(), "b1a3");
    /// assert_eq!(knight_moves.next().unwrap(), "b1c3");
    /// assert_eq!(knight_moves.next().unwrap(), "g1f3");
    /// assert_eq!(knight_moves.next().unwrap(), "g1h3");
    /// assert!(knight_moves.next().is_none());
    /// ```
    #[inline(always)]
    pub fn get_legal_moves_from(&self, mask: impl Into<Bitboard>) -> MoveList {
        let mut moves = MoveList::default();
        let mask = mask.into();
        match self.checkers().population() {
            0 => self.generate_all_moves::<false>(mask, &mut moves),
            1 => self.generate_all_moves::<true>(mask, &mut moves),
            // If we're in double check, we can only move the King
            _ => self.generate_king_moves::<true>(mask, &mut moves),
        }
        moves
    }

    /// Recomputes legal metadata (checkers, checkmask, pinmask, etc.).
    #[inline(always)]
    fn recompute_legal_masks(&mut self) {
        let color = self.side_to_move();
        let opponent = color.opponent();
        let occupied = self.occupied();

        // Find the King
        self.king_square = self.king(color).to_square_unchecked();

        // Reset the pinmask and checkmask
        self.pinned = Bitboard::EMPTY_BOARD;
        // Sanity check; no move can capture the enemy King, so his square is removed
        self.checkmask = self.enemy_or_empty(color) ^ self.king(opponent);

        // Starting off, the easiest checkers to find are Knights and Pawns; just the overlap of their attacks from the King and themselves.
        self.checkers = self.knight(opponent) & knight_attacks(self.king_square)
            | self.pawn(opponent) & pawn_attacks(self.king_square, color);

        // By pretending that there is a Rook/Bishop at our King that can attack without blockers,
        //  we can find all possible sliding attacks *to* the King,
        //  which lets us figure out who our checkers are and what pieces are pinned.
        let enemy_sliding_attacks = rook_rays(self.king_square) & self.orthogonal_sliders(opponent)
            | bishop_rays(self.king_square) & self.diagonal_sliders(opponent);

        // Examine every square that this Rook/Bishop can attack, so that we can figure out if it's a checker or if there are any pinned pieces.
        for attacker in enemy_sliding_attacks {
            // Get a ray between this square and the attacker square, excluding both pieces
            let ray = ray_between(self.king_square, attacker);

            // Whether the piece is a checker or pinned depends on how many pieces are in the ray
            match (ray & occupied).population() {
                // There are no pieces between the attacker and the King, so the attacker is a checker
                0 => self.checkers |= attacker,

                // The piece is not (necessarily) adjacent, but is on a ray to the King, so it is pinned
                1 => self.pinned |= ray & self.color(color), // Enemy pieces can't be pinned!

                // Since we can't move two pieces off of the same ray in the same turn, we don't care about populations higher than 1
                _ => {}
            }
        }

        // If there are any checkers, we need to update the checkmask
        if self.checkers.is_nonempty() {
            // Start with the checkers so they are included in the checkmask (since there is no ray between a King and a Knight)
            self.checkmask = self.checkers ^ self.king(opponent);

            // There is *usually* less than two checkers, so this rarely loops.
            for checker in self.checkers {
                self.checkmask |= ray_between(self.king_square, checker);
            }
        }

        // Recompute attack/defend maps
        for color in Color::all() {
            self.attacks_by_color[color] = self.compute_attacks_by(color);
        }
    }

    /// Wrapper for all of the `generate_x_moves` methods.
    #[inline(always)]
    fn generate_all_moves<const IN_CHECK: bool>(&self, mask: Bitboard, moves: &mut MoveList) {
        self.generate_pawn_moves::<IN_CHECK>(mask, moves);
        self.generate_knight_moves::<IN_CHECK>(mask, moves);
        self.generate_bishop_moves::<IN_CHECK>(mask, moves);
        self.generate_rook_moves::<IN_CHECK>(mask, moves);
        self.generate_king_moves::<IN_CHECK>(mask, moves);
    }

    /// Creates and appends a [`Move`] that is either a quiet or capture.
    #[inline(always)]
    fn serialize_normal_move(&self, to: Square, from: Square, moves: &mut MoveList) {
        let kind = if self.has(to) {
            MoveKind::Capture
        } else {
            MoveKind::Quiet
        };

        moves.push(Move::new(from, to, kind));
    }

    /// Generates and serializes all legal Pawn moves.
    fn generate_pawn_moves<const IN_CHECK: bool>(&self, mask: Bitboard, moves: &mut MoveList) {
        let color = self.side_to_move();
        for from in self.pawn(color) & mask {
            let mobility = self.generate_legal_pawn_mobility::<IN_CHECK>(color, from);

            for to in mobility {
                let mut kind = if self.has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                if to.rank() == Rank::eighth(color) {
                    // If this move also captures, it's a capture-promote
                    if kind == MoveKind::Capture {
                        moves.push(Move::new(from, to, MoveKind::CaptureAndPromoteKnight));
                        moves.push(Move::new(from, to, MoveKind::CaptureAndPromoteBishop));
                        moves.push(Move::new(from, to, MoveKind::CaptureAndPromoteRook));
                        kind = MoveKind::CaptureAndPromoteQueen;
                    } else {
                        moves.push(Move::new(from, to, MoveKind::PromoteKnight));
                        moves.push(Move::new(from, to, MoveKind::PromoteBishop));
                        moves.push(Move::new(from, to, MoveKind::PromoteRook));
                        kind = MoveKind::PromoteQueen;
                    }
                }
                // If this pawn is moving to the en passant square, it's en passant
                else if Some(to) == self.ep_square() {
                    kind = MoveKind::EnPassantCapture;
                }
                // If the Pawn is moving two ranks, it's a double push
                else if from.rank().abs_diff(to.rank()) == 2 {
                    kind = MoveKind::PawnDoublePush;
                }

                let mv = Move::new(from, to, kind);
                moves.push(mv);
            }
        }
    }

    /// Generates and serializes all legal Knight moves.
    fn generate_knight_moves<const IN_CHECK: bool>(&self, mask: Bitboard, moves: &mut MoveList) {
        let color = self.side_to_move();
        for from in self.knight(color) & mask {
            let attacks = knight_attacks(from);
            let mobility = self.generate_legal_normal_piece_mobility::<IN_CHECK>(from, attacks);

            for to in mobility {
                self.serialize_normal_move(to, from, moves);
            }
        }
    }

    /// Generates and serializes all legal Bishop moves.
    fn generate_bishop_moves<const IN_CHECK: bool>(&self, mask: Bitboard, moves: &mut MoveList) {
        let color = self.side_to_move();
        let blockers = self.occupied();
        for from in self.diagonal_sliders(color) & mask {
            let attacks = bishop_attacks(from, blockers);
            let mobility = self.generate_legal_normal_piece_mobility::<IN_CHECK>(from, attacks);

            for to in mobility {
                self.serialize_normal_move(to, from, moves);
            }
        }
    }

    /// Generates and serializes all legal Rook moves.
    fn generate_rook_moves<const IN_CHECK: bool>(&self, mask: Bitboard, moves: &mut MoveList) {
        let color = self.side_to_move();
        let blockers = self.occupied();
        for from in self.orthogonal_sliders(color) & mask {
            let attacks = rook_attacks(from, blockers);
            let mobility = self.generate_legal_normal_piece_mobility::<IN_CHECK>(from, attacks);

            for to in mobility {
                self.serialize_normal_move(to, from, moves);
            }
        }
    }

    /// Generates and serializes all legal King moves.
    fn generate_king_moves<const IN_CHECK: bool>(&self, mask: Bitboard, moves: &mut MoveList) {
        // If the mask doesn't contain the King, then don't generate the King's moves
        if mask.is_disjoint(self.king_square) {
            return;
        }

        let from = self.king_square;
        let color = self.side_to_move();
        for to in self.generate_legal_king_mobility::<IN_CHECK>(color, from) {
            let kind = if let Some(victim) = self.piece_at(to) {
                // If the victim is friendly, this is a castling move (KxR).
                if victim.color() == color {
                    if to.file() > from.file() {
                        MoveKind::ShortCastle
                    } else {
                        MoveKind::LongCastle
                    }
                } else {
                    MoveKind::Capture
                }
            } else {
                MoveKind::Quiet
            };

            let mv = Move::new(from, to, kind);
            moves.push(mv);
        }
    }

    /// Generates a [`Bitboard`] of all legal moves for a Pawn at `square`.
    fn generate_legal_pawn_mobility<const IN_CHECK: bool>(
        &self,
        color: Color,
        square: Square,
    ) -> Bitboard {
        let blockers = self.occupied();

        // Pinned pawns are complicated:
        // - A pawn pinned horizontally cannot move. At all.
        // - A pawn pinned vertically can only push forward, not capture.
        // - A pawn pinned diagonally can only capture it's pinner.
        let is_pinned = self.pinned.intersects(square);
        let pinmask = Bitboard::from_bool(!is_pinned) | ray_containing(square, self.king_square);

        // If en passant can be performed, check its legality.
        // If not, default to an empty bitboard.
        let ep_bb = self
            .ep_square()
            .map(|ep_square| self.generate_ep_bitboard(color, square, ep_square))
            .unwrap_or_default();

        // Get a mask for all possible pawn double pushes.
        let all_but_this_pawn = blockers ^ square;
        let double_push_mask = all_but_this_pawn | all_but_this_pawn.forward_by(color, 1);
        let pushes = pawn_pushes(square, color) & !double_push_mask & !blockers;

        // Attacks are only possible on enemy occupied squares, or en passant.
        let enemies = self.color(color.opponent());
        let attacks = pawn_attacks(square, color) & (enemies | ep_bb);

        // Pseudo-legal      ---------------Legal--------------
        (pushes | attacks) & (self.checkmask() | ep_bb) & pinmask
    }

    /// Generate a [`Bitboard`] for the legality of performing an en passant capture with the Pawn at `square`.
    ///
    /// If en passant is legal, the returned bitboard will have a single bit set, representing a legal capture for the Pawn at `square`.
    /// If en passant is not legal, the returned bitboard will be empty.
    #[inline(always)]
    fn generate_ep_bitboard(&self, color: Color, square: Square, ep_square: Square) -> Bitboard {
        // If this Pawn isn't on an adjacent file and the same rank as the enemy Pawn that caused en passant to be possible, it can't perform en passant
        if square.distance_ranks(ep_square) != 1 || square.distance_files(ep_square) != 1 {
            return Bitboard::EMPTY_BOARD;
        }

        // Compute a blockers bitboard as if EP was performed.
        let ep_bb = ep_square.bitboard();
        let ep_target_bb = ep_bb.backward_by(color, 1);
        let blockers_after_ep = (self.occupied() ^ ep_target_bb ^ square) | ep_bb;

        // If, after performing EP, any sliders can attack our King, EP is not legal
        let enemy_ortho_sliders = self.orthogonal_sliders(color.opponent());
        if rook_attacks(self.king_square, blockers_after_ep).intersects(enemy_ortho_sliders) {
            return Bitboard::EMPTY_BOARD;
        }

        let enemy_diag_sliders = self.diagonal_sliders(color.opponent());
        if bishop_attacks(self.king_square, blockers_after_ep).intersects(enemy_diag_sliders) {
            return Bitboard::EMPTY_BOARD;
        }

        // Otherwise, it is safe to perform EP
        ep_bb
    }

    /// Generates a [`Bitboard`] of all legal moves for the King at `square`.
    fn generate_legal_king_mobility<const IN_CHECK: bool>(
        &self,
        color: Color,
        square: Square,
    ) -> Bitboard {
        let attacks = king_attacks(square);
        let enemy_attacks = self.attacks_by(color.opponent());

        // If in check, we cannot castle- we can only attack with the default movement of the King.
        let castling = if IN_CHECK {
            Bitboard::EMPTY_BOARD
        } else {
            // Otherwise, compute castling availability like normal
            let short = self.castling_rights_for(color).short.map(|rook_start| {
                let king_end = Square::king_short_castle(color);
                let rook_end = Square::rook_short_castle(color);
                self.generate_castling_bitboard(rook_start, rook_end, king_end, enemy_attacks)
            });

            let long = self.castling_rights_for(color).long.map(|rook_start| {
                let king_end = Square::king_long_castle(color);
                let rook_end = Square::rook_long_castle(color);
                self.generate_castling_bitboard(rook_start, rook_end, king_end, enemy_attacks)
            });

            short.unwrap_or_default() | long.unwrap_or_default()
        };

        // Safe squares are ones not attacked by the enemy or part of a discoverable check
        let safe_squares = !(enemy_attacks | self.generate_discoverable_checks_bitboard(color));

        // All legal attacks that are safe and not on friendly squares, as well as castling
        (attacks & self.enemy_or_empty(color) & safe_squares) | castling
    }

    /// Generate a bitboard for `color`'s ability to castle with the Rook on `rook_square`, which will place the King on `dst_square`.
    #[inline(always)]
    fn generate_castling_bitboard(
        &self,
        rook_src: Square,
        rook_dst: Square,
        king_dst: Square,
        enemy_attacks: Bitboard,
    ) -> Bitboard {
        // The King and Rook don't count as blockers, since they're moving through each other
        let blockers = self.occupied() ^ self.king_square ^ rook_src;

        // All squares between the King and his destination must be empty
        if (ray_between(self.king_square, king_dst) | king_dst).intersects(blockers) {
            return Bitboard::EMPTY_BOARD;
        }

        // All squares between the Rook and its destination must be empty
        if (ray_between(rook_src, rook_dst) | rook_dst).intersects(blockers) {
            return Bitboard::EMPTY_BOARD;
        }

        // All squares between the King and his destination (inclusive) must not be attacked
        if (ray_between(self.king_square, king_dst) | king_dst).intersects(enemy_attacks) {
            return Bitboard::EMPTY_BOARD;
        }

        // If the Rook is pinned, we can't castle (Chess960)
        if self.pinned().intersects(rook_src) {
            return Bitboard::EMPTY_BOARD;
        }

        // Castling is safe to perform
        Bitboard::from_square(rook_src)
    }

    /// These are the rays containing the King and his Checkers.
    /// They are used to prevent the King from retreating along a line he is checked on.
    /// Note: A pawn can't generate a discoverable check, as it can only capture 1 square away.
    #[inline(always)]
    fn generate_discoverable_checks_bitboard(&self, color: Color) -> Bitboard {
        let mut discoverable = Bitboard::EMPTY_BOARD;

        for checker in self.checkers & self.sliders(color.opponent()) {
            // Need to XOR because capturing the checker is legal
            discoverable |= ray_containing(self.king_square, checker) ^ checker;
        }

        discoverable
    }

    /// Generates a [`Bitboard`] of all legal moves for a non-Pawn and non-King piece at `square`.
    #[inline(always)]
    fn generate_legal_normal_piece_mobility<const IN_CHECK: bool>(
        &self,
        square: Square,
        default_attacks: Bitboard,
    ) -> Bitboard {
        // Check if this piece is pinned along any of the pinmasks
        let legal_squares = if self.pinned().intersects(square) {
            self.checkmask() & ray_containing(square, self.king_square)
        } else {
            self.checkmask()
        };

        // Pseudo-legal attacks that are within the check/pin mask and attack non-friendly squares
        default_attacks & legal_squares
    }

    /// Places a piece at the provided square, updating Zobrist hash information.
    #[inline(always)]
    pub fn place(&mut self, piece: Piece, square: Square) {
        self.position.board.place(piece, square);
        self.position.key.hash_piece(square, piece);
        self.evaluator.piece_placed(piece, square);
    }

    /// Removes and returns a piece on the provided square, updating Zobrist hash information.
    #[inline(always)]
    pub fn take(&mut self, square: Square) -> Option<Piece> {
        let piece = self.position.board.take(square)?;

        self.position.key.hash_piece(square, piece);
        self.evaluator.piece_taken(piece, square);

        Some(piece)
    }
}

impl<V: Variant> Deref for Game<V> {
    type Target = Position;
    /// A [`Game`] immutably dereferences to a [`Position`], for simplicity.
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.position
    }
}

impl<V: Variant> FromStr for Game<V> {
    type Err = anyhow::Error;
    /// Wrapper for [`Game::<Standard>::from_fen`]
    #[inline(always)]
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Self::from_fen(s)
    }
}

impl<V: Variant> Default for Game<V> {
    /// Standard starting position for Chess.
    #[inline(always)]
    fn default() -> Self {
        // Safety: The FEN for startpos is always valid
        // unsafe { Self::from_fen(FEN_STARTPOS).unwrap_unchecked() }
        Self::from_fen(V::fen_startpos()).unwrap()
    }
}

impl<V: Variant> fmt::Display for Game<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ranks = Rank::iter().rev();

        let squares_to_string = |bb: Bitboard| {
            bb.into_iter()
                .map(Square::to_uci)
                .collect::<Vec<_>>()
                .join(", ")
        };

        for rank in ranks {
            write!(f, "{rank}")?;
            write!(f, "|")?;
            for file in File::iter() {
                let piece = self.board().piece_at(file * rank);
                let piece_char = piece.map(|p| p.char()).unwrap_or('.');
                write!(f, " {piece_char}")?;
            }

            if rank == Rank::EIGHT {
                write!(f, "    Variant: {:?}", V::variant())?;
            } else if rank == Rank::SEVEN {
                write!(f, "        FEN: {}", self.to_fen())?;
            } else if rank == Rank::SIX {
                write!(f, "        Key: {}", self.key())?;
            } else if rank == Rank::FIVE {
                write!(f, "   Checkers: {}", squares_to_string(self.checkers()))?;
            } else if rank == Rank::FOUR {
                write!(f, "     Pinned: {}", squares_to_string(self.pinned()))?;
            } else if rank == Rank::THREE {
                write!(
                    f,
                    "   Material: {} (white) {} (black)",
                    self.evaluator().material[Color::White],
                    self.evaluator().material[Color::Black]
                )?;
            } else if rank == Rank::TWO {
                let (mg, eg) = self.evaluator().evals();
                write!(
                    f,
                    "       Eval: {} (mg={mg}, eg={eg}, %={})",
                    self.eval(),
                    self.evaluator().endgame_weight(),
                )?;
                // } else if rank == Rank::ONE {
            }
            writeln!(f)?;
        }
        write!(f, " +")?;
        for _ in File::iter() {
            write!(f, "--")?;
        }
        write!(f, "\n   ")?;
        for file in File::iter() {
            write!(f, "{file}")?;
            write!(f, " ")?;
        }

        Ok(())
    }
}

impl<V: Variant> fmt::Debug for Game<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let format = |to_fmt: &[(Bitboard, &str)]| {
            let strings = to_fmt
                .iter()
                .map(|(b, s)| (b.to_string(), s))
                .collect::<Vec<_>>();

            let splits = strings
                .iter()
                .map(|(b, _)| b.split('\n').collect::<Vec<_>>())
                .collect::<Vec<_>>();

            let labels = strings.iter().fold(String::new(), |mut acc, (_, s)| {
                _ = write!(acc, "{s:10}\t\t");
                acc
            });

            let boards = (0..8).fold(String::new(), |mut acc, i| {
                _ = writeln!(
                    acc,
                    "{}",
                    (0..splits.len()).fold(String::new(), |mut output, j| {
                        _ = write!(output, "{}\t", splits[j][i]);
                        output
                    })
                );
                acc
            });

            format!("{labels}\n{boards}")
        };

        let color = self.position.side_to_move();

        let check_data = format(&[
            (self.checkers, "Checkers"),
            (self.checkmask, "Checkmask"),
            (self.pinned, "Pinned"),
            (
                self.generate_discoverable_checks_bitboard(color),
                "Disc. Checks",
            ),
        ]);

        let mobility_data = format(&[
            (self.position.color(color), "Friendlies"),
            (self.position.color(color.opponent()), "Enemies"),
        ]);

        write!(
            f,
            "Position:\n{:?}\n\n{check_data}\n\n{mobility_data}",
            self.position
        )
    }
}

/// Represents the castling rights of a single player
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
pub struct CastlingRights {
    /// If a right is `Some(square)`, then `square` is the *Rook*'s location
    pub(crate) short: Option<Square>,
    pub(crate) long: Option<Square>,
}

impl CastlingRights {
    /// Number of possible combinations of castling rights.
    ///
    /// Used for Zobrist hashing.
    pub const COUNT: usize = 16;

    /// Creates a new [`CastlingRights`] with the provided values.
    #[inline(always)]
    pub const fn new(short: Option<Square>, long: Option<Square>) -> Self {
        Self { short, long }
    }

    /// Creates a `usize` for indexing into lists of 4 elements.
    ///
    /// Only used internally for Zobrist hashing.
    #[inline(always)]
    pub(crate) const fn index(&self) -> usize {
        (self.short.is_some() as usize) | (self.long.is_some() as usize) << 1
    }
}

/// Represents the current state of the game, including move counters.
///
/// This is analogous to a FEN string, and possesses no way to move pieces on the board.
/// If you are looking for a type to include in an engine, use [`crate::Game`].
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Position {
    /// Bitboard representation of the game board.
    board: Board,

    /// The [`Color`] of the current player.
    side_to_move: Color,

    /// Castling rights for each player.
    castling_rights: [CastlingRights; Color::COUNT],

    /// Optional attack square for en passant.
    ep_square: Option<Square>,

    /// Used to enforce the fifty-move rule.
    ///
    /// - Incremented after each move.
    /// - Reset after a capture or a pawn moves.
    halfmove: u8,

    /// Number of moves since the beginning of the game.
    ///
    /// A fullmove is a complete turn by white and then by black.
    fullmove: u8,

    /// Zobrist hash key of this position
    key: ZobristKey,
}

impl Position {
    /// Creates a new, empty [`Position`] with the following properties:
    /// * No pieces on the board
    /// * White moves first
    /// * No castling rights
    /// * No en passant square available
    /// * Halfmove counter set to 0
    /// * Fullmove counter set to 1
    ///
    /// # Example
    /// ```
    /// # use toad::Position;
    /// let state = Position::new();
    /// assert_eq!(state.to_fen(false), "8/8/8/8/8/8/8/8 w - - 0 1");
    /// ```
    #[inline(always)]
    pub fn new() -> Self {
        let board = Board::new();
        let castling_rights = [CastlingRights::default(); Color::COUNT];
        let side_to_move = Color::White;
        let ep_square = None;

        let key = ZobristKey::from_parts(&board, ep_square, &castling_rights, side_to_move);

        Self {
            board,
            side_to_move,
            castling_rights,
            ep_square,
            halfmove: 0,
            fullmove: 1,
            key,
        }
    }

    /// Generates a FEN string from this [`Position`].
    ///
    /// The `is_chess960` parameter will determine whether to print castling rights in Chess960 notation.
    ///
    /// # Example
    /// ```
    /// # use toad::Position;
    /// let state = Position::default();
    /// assert_eq!(state.to_fen(false), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    /// assert_eq!(state.to_fen(true), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w HAha - 0 1");
    /// ```
    pub fn to_fen(&self, is_chess960: bool) -> String {
        let placements = self.board().to_fen();
        let active_color = self.side_to_move();

        let castling = if is_chess960 {
            self.castling_rights_960()
        } else {
            self.castling_rights_uci()
        };

        let en_passant_target = if let Some(square) = self.ep_square {
            square.to_string()
        } else {
            String::from("-")
        };

        let halfmove = self.halfmove;
        let fullmove = self.fullmove;

        format!("{placements} {active_color} {castling} {en_passant_target} {halfmove} {fullmove}")
    }

    /// Returns the current player as a [`Color`].
    #[inline(always)]
    pub const fn side_to_move(&self) -> Color {
        self.side_to_move
    }

    /// If en passant can be performed, returns the en passant [`Square`].
    #[inline(always)]
    pub const fn ep_square(&self) -> Option<Square> {
        self.ep_square
    }

    /// Returns the half-move counter of the current position.
    #[inline(always)]
    pub const fn halfmove(&self) -> u8 {
        self.halfmove
    }

    /// Returns the full-move counter of the current position.
    #[inline(always)]
    pub const fn fullmove(&self) -> u8 {
        self.fullmove
    }

    /// Fetch the Zobrist hash key of this position.
    #[inline(always)]
    pub fn key(&self) -> ZobristKey {
        self.key
    }

    /// Fetches this position's [`Board`]
    #[inline(always)]
    pub const fn board(&self) -> &Board {
        &self.board
    }

    /// Returns the [`CastlingRights`] of the current position.
    #[inline(always)]
    pub const fn castling_rights(&self) -> &[CastlingRights; Color::COUNT] {
        &self.castling_rights
    }

    /// Returns the [`CastlingRights`] for `color` in the current position.
    #[inline(always)]
    pub const fn castling_rights_for(&self, color: Color) -> &CastlingRights {
        &self.castling_rights[color.index()]
    }

    /// Returns the [`CastlingRights`] of the current position in standard UCI notation.
    pub fn castling_rights_uci(&self) -> String {
        let mut castling = String::with_capacity(4);

        if self.castling_rights()[Color::White].short.is_some() {
            castling.push('K');
        }
        if self.castling_rights()[Color::White].long.is_some() {
            castling.push('Q');
        }
        if self.castling_rights()[Color::Black].short.is_some() {
            castling.push('k');
        }
        if self.castling_rights()[Color::Black].long.is_some() {
            castling.push('q');
        }

        // If no side can castle, use a hyphen
        if castling.is_empty() {
            castling = String::from("-");
        }
        castling
    }

    /// Returns the [`CastlingRights`] of the current position in Chess960 notation.
    pub fn castling_rights_960(&self) -> String {
        let mut castling = String::with_capacity(4);

        if let Some(sq) = self.castling_rights()[Color::White].short {
            castling.push(sq.file().char().to_ascii_uppercase());
        }

        if let Some(sq) = self.castling_rights()[Color::White].long {
            castling.push(sq.file().char().to_ascii_uppercase());
        }

        if let Some(sq) = self.castling_rights()[Color::Black].short {
            castling.push(sq.file().char());
        }

        if let Some(sq) = self.castling_rights()[Color::Black].long {
            castling.push(sq.file().char());
        }

        // If no side can castle, use a hyphen
        if castling.is_empty() {
            castling = String::from("-");
        }
        castling
    }

    /// Returns `true` if the half-move counter is 100 or greater.
    ///
    /// Since "half-move" increases with ply, the 50-move rule takes effect at 100 ply.
    #[inline(always)]
    pub const fn can_draw_by_fifty(&self) -> bool {
        self.halfmove() >= 100
    }

    /// Returns `true` if there is insufficient material on the board to cause a checkmate.
    ///
    /// According to the [FIDE rules on draw conditions](https://handbook.fide.com/chapter/E012023):
    /// >  The game is drawn when a position has arisen in which neither player can checkmate the opponent’s king with any series of legal moves. The game is said to end in a ‘dead position’. This immediately ends the game, provided that the move producing the position was in accordance with Article 3 and Articles 4.2 – 4.7.
    ///
    /// # Example
    /// ```
    /// # use toad::*;
    /// // Lone Kings
    /// let kk: Game<Standard> = "8/4k3/8/8/3K4/8/8/8 w - - 0 1".parse().unwrap();
    /// assert!(kk.can_draw_by_insufficient_material());
    ///
    /// // A single Bishop (either color)
    /// let kbk: Game<Standard> = "8/4k3/8/8/3K4/8/5B2/8 w - - 0 1".parse().unwrap();
    /// assert!(kbk.can_draw_by_insufficient_material());
    ///
    /// // A single Knight
    /// let knk: Game<Standard> = "8/4k3/2n5/8/3K4/8/8/8 w - - 0 1".parse().unwrap();
    /// assert!(knk.can_draw_by_insufficient_material());
    ///
    /// // Opposing Bishops on the same color square
    /// let same_square_bishops: Game<Standard> = "8/2b1k3/8/8/3K4/8/5B2/8 w - - 0 1".parse().unwrap();
    /// assert!(same_square_bishops.can_draw_by_insufficient_material());
    ///
    /// // Opposing Bishops on different color squares
    /// let diff_square_bishops: Game<Standard> = "8/3bk3/8/8/3K4/8/5B2/8 w - - 0 1".parse().unwrap();
    /// assert!(!diff_square_bishops.can_draw_by_insufficient_material());
    /// ```
    #[inline(always)]
    pub fn can_draw_by_insufficient_material(&self) -> bool {
        // If either side has a Queen, Rook, or Pawn, there remains sufficient material
        if (self.kind(PieceKind::Queen) | self.kind(PieceKind::Rook) | self.kind(PieceKind::Pawn))
            .is_nonempty()
        {
            return false;
        }

        // Get all of the minor pieces
        let wb = self.bishop(Color::White);
        let wn = self.knight(Color::White);
        let bb = self.bishop(Color::Black);
        let bn = self.knight(Color::Black);

        // Match on all possible combinations
        match (
            wb.population(),
            wn.population(),
            bb.population(),
            bn.population(),
        ) {
            // Lone kings...
            (0, 0, 0, 0) |
            // ...or a single bishop...
            (1, 0, 0, 0) | (0, 0, 1, 0) |
            // ...or a single knight...
            (0, 1, 0, 0) | (0, 0, 0, 1) => true,

            // ...or if each King has a single Bishop...
            (1, 0, 1, 0) => {
                // ...but only if the bishops are on the SAME color!
                wb.to_square_unchecked().color()
                    == bb.to_square_unchecked().color()
            }

            // All other cases have sufficient material, even if checkmate requires coercion.
            _ => false,
        }
    }

    /// Toggles the current player from White to Black (or vice versa).
    ///
    /// This is equivalent to playing a null move.
    #[inline(always)]
    pub fn toggle_side_to_move(&mut self) {
        self.side_to_move = self.side_to_move.opponent();
    }

    /*
    /// Checks if the provided move is pseudo-legal to perform.
    ///
    /// If `Ok()`, the move is legal.
    /// If `Err(msg)`, then `msg` will be a reason as to why it's not legal.
    pub fn check_pseudo_legality_of(&self, mv: Move) -> Result<()> {
        let (from, to, kind) = mv.parts();

        // If there's no piece here, illegal move
        let Some(piece) = self.board().piece_at(from) else {
            bail!("No piece here to move");
        };

        // If it's not this piece's color's turn, illegal move
        if piece.color() != self.side_to_move() {
            bail!("Tried to move a piece that wasn't yours");
        }

        // If this move captures a piece, handle those cases
        if let Some(to_capture) = self.board().piece_at(to) {
            // Can't capture own pieces
            if to_capture.color() == piece.color() {
                bail!("Tried to capture your own piece");
            }

            // Can't capture king
            if to_capture.is_king() {
                bail!("Tried to capture enemy king");
            }

            // Ensure that the move is a capture or en passant, and that it captures the correct piece
            if !mv.is_capture() {
                bail!("Captured on a non-capture move");
            }
        }

        match kind {
            // If the move is pawn-specific, ensure it's a pawn moving
            MoveKind::EnPassantCapture | MoveKind::PawnDoublePush => {
                if !piece.is_pawn() {
                    bail!("Tried to do a pawn move (EP, Push 2, Promote) with a piece that isn't a pawn");
                }
            }
            // If castling, ensure we have the right to
            MoveKind::ShortCastle => {
                if self.castling_rights[piece.color()].short.is_none() {
                    bail!("Tried to castle (short) without rights");
                }
            }
            // If castling, ensure we have the right to
            MoveKind::LongCastle => {
                if self.castling_rights[piece.color()].long.is_none() {
                    bail!("Tried to castle (long) without rights");
                }
            }
            // Quiet moves are fine
            _ => {}
        }

        Ok(())
    }
     */

    /*
    /// Checks if playing the provided [`Move`] is pseudo-legal on the current position.
    ///
    /// A pseudo-legal move obeys all rules of chess except that the side-to-move's King
    /// may be left in Check after applying the move.
    pub fn is_pseudo_legal(&self, mv: Move) -> bool {
        let from = mv.from();
        // If there isn't a piece here, this move isn't legal
        let Some(piece) = self.piece_at(from) else {
            return false;
        };

        let color = piece.color();
        // If the piece is the wrong color, this move isn't legal
        if color != self.side_to_move() {
            return false;
        }
        let to = mv.to();
        let opponent = color.opponent();

        // Ensure the destination square is appropriate for the piece type
        // match piece.kind() {
        //     PieceKind::Pawn => {}
        // }

        return true;
    }
     */

    /// Generate all pseudo-legal moves from the current position.
    ///
    /// Pseudo-legal moves are consistent with the current board representation,
    /// but may leave the side-to-move's King in check after being made.
    #[inline(always)]
    pub fn get_pseudo_legal_moves(&self) -> MoveList {
        let friendlies = self.color(self.side_to_move());
        self.get_pseudo_legal_moves_from(friendlies)
    }

    /// Generate all pseudo-legal moves from the current position that originate from squares in `mask`.
    ///
    /// Pseudo-legal moves are consistent with the current board representation,
    /// but may leave the side-to-move's King in check after being made.
    #[inline(always)]
    pub fn get_pseudo_legal_moves_from(&self, mask: impl Into<Bitboard>) -> MoveList {
        let mut moves = MoveList::default();
        let color = self.side_to_move();
        let opponent = color.opponent();
        let blockers = self.occupied();
        // Cannot capture enemy king, so remove him from the possible target squares
        let target_squares = self.enemy_or_empty(color) ^ self.king(opponent);
        // Ensure the mask ONLY contains the side-to-move's pieces
        let mask = mask.into() & self.color(color);

        let pawns = self.pawn(color) & mask;
        let king = self.king(color) & mask;
        let normal_pieces = (blockers ^ pawns ^ king) & mask;

        // King first
        for from in king {
            // Castling is handled separately from regular attacks
            if let Some(rook) = self.castling_rights_for(color).short {
                moves.push(Move::new(from, rook, MoveKind::ShortCastle));
            }

            if let Some(rook) = self.castling_rights_for(color).long {
                moves.push(Move::new(from, rook, MoveKind::LongCastle));
            }

            // Attacks are either quiet or captures
            for to in king_attacks(from) & target_squares {
                let kind = if self.has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                moves.push(Move::new(from, to, kind));
            }
        }

        // All non-Pawns next
        for (from, piece) in self.iter_for(normal_pieces) {
            let attacks = match piece.kind() {
                PieceKind::Knight => knight_attacks(from),
                PieceKind::Bishop => bishop_attacks(from, blockers),
                PieceKind::Rook => rook_attacks(from, blockers),
                PieceKind::Queen => queen_attacks(from, blockers),
                _ => unreachable!("Pawn and King moves already handled"),
            } & target_squares;

            for to in attacks {
                // If the destination is occupied, it's a capture. Otherwise, it's a quiet.
                let kind = if self.has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                moves.push(Move::new(from, to, kind));
            }
        }

        // Pawns can attack opponent pieces or the EP square, but cannot capture the enemy King.
        let pawn_targets = self.color(opponent) ^ self.king(opponent)
            | self.ep_square().map(|sq| sq.bitboard()).unwrap_or_default();
        // Pawns last
        for from in pawns {
            let attacks = pawn_attacks(from, color) & pawn_targets;

            let all_but_this_pawn = blockers ^ from;
            let double_push_mask = all_but_this_pawn | all_but_this_pawn.forward_by(color, 1);
            let pushes = pawn_pushes(from, color) & !double_push_mask & !blockers;

            for to in attacks | pushes {
                // Captures can be normal, en passant, or promotions
                let mut kind = if self.has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                if to.rank() == Rank::eighth(color) {
                    // If this move also captures, it's a capture-promote
                    if kind == MoveKind::Capture {
                        moves.push(Move::new(from, to, MoveKind::CaptureAndPromoteKnight));
                        moves.push(Move::new(from, to, MoveKind::CaptureAndPromoteBishop));
                        moves.push(Move::new(from, to, MoveKind::CaptureAndPromoteRook));
                        kind = MoveKind::CaptureAndPromoteQueen;
                    } else {
                        moves.push(Move::new(from, to, MoveKind::PromoteKnight));
                        moves.push(Move::new(from, to, MoveKind::PromoteBishop));
                        moves.push(Move::new(from, to, MoveKind::PromoteRook));
                        kind = MoveKind::PromoteQueen;
                    }
                } else
                // If this pawn is moving to the en passant square, it's en passant
                if Some(to) == self.ep_square() {
                    kind = MoveKind::EnPassantCapture;
                } else
                // If the Pawn is moving two ranks, it's a double push
                if from.distance_ranks(to) == 2 {
                    kind = MoveKind::PawnDoublePush;
                }

                moves.push(Move::new(from, to, kind));
            }
        }

        moves
    }

    /// Clears the castling rights of `color`
    #[inline(always)]
    fn clear_castling_rights(&mut self, color: Color) {
        self.key.hash_castling_rights(&self.castling_rights);
        self.castling_rights[color].short = None;
        self.castling_rights[color].long = None;
        self.key.hash_castling_rights(&self.castling_rights);
    }

    /// Clears the short/kingside castling rights of `color`
    #[inline(always)]
    fn clear_short_castling_rights(&mut self, color: Color) {
        self.key.hash_castling_rights(&self.castling_rights);
        self.castling_rights[color].short = None;
        self.key.hash_castling_rights(&self.castling_rights);
    }

    /// Clears the long/queenside castling rights of `color`
    #[inline(always)]
    fn clear_long_castling_rights(&mut self, color: Color) {
        self.key.hash_castling_rights(&self.castling_rights);
        self.castling_rights[color].long = None;
        self.key.hash_castling_rights(&self.castling_rights);
    }
}

impl Deref for Position {
    type Target = Board;
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        self.board()
    }
}

impl Default for Position {
    #[inline(always)]
    fn default() -> Self {
        // Safety: The FEN for startpos is always valid
        unsafe {
            Game::<Standard>::from_fen(FEN_STARTPOS)
                .unwrap_unchecked()
                .position
        }
    }
}

impl fmt::Display for Position {
    /// Display this position's FEN string.
    ///
    /// If the alternate format mode (`#`) was specified, this will print the castling rights in Chess960 format.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_fen(f.alternate()))
    }
}

impl fmt::Debug for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ranks = Rank::iter().rev();

        for rank in ranks {
            write!(f, "{rank}")?;
            write!(f, "|")?;
            for file in File::iter() {
                let square = Square::new(file, rank);
                let piece = self.board().piece_at(square);
                let piece_char = piece.map(|p| p.char()).unwrap_or('.');
                write!(f, " {piece_char}")?;
            }

            if rank == Rank::SEVEN {
                write!(f, "           FEN: {}", self.to_fen(f.alternate()))?;
            } else if rank == Rank::SIX {
                write!(f, "          Side: {}", self.side_to_move())?;
            } else if rank == Rank::FIVE {
                write!(f, "      Castling: {}", self.castling_rights_uci())?;
            } else if rank == Rank::FOUR {
                let ep = self
                    .ep_square()
                    .map(|t| t.to_uci())
                    .unwrap_or(String::from("-"));
                write!(f, "            EP: {ep}")?;
            } else if rank == Rank::THREE {
                write!(f, "     Half-move: {}", self.halfmove())?;
            } else if rank == Rank::TWO {
                write!(f, "     Full-move: {}", self.fullmove())?;
            } else if rank == Rank::ONE {
                write!(f, "     Key: {}", self.key())?;
            }
            writeln!(f)?;
        }
        write!(f, " +")?;
        for _ in File::iter() {
            write!(f, "--")?;
        }
        write!(f, "\n   ")?;
        for file in File::iter() {
            write!(f, "{file}")?;
            write!(f, " ")?;
        }

        Ok(())
    }
}

/// Represents all pieces and their locations on a chess board.
///
/// Has no knowledge of castling rights, en passant, or move counters. If you need those, see [`Position`].
///
/// Internally uses a collection of [`Bitboard`]s to keep track of piece/color locations.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Board {
    /// All squares occupied by a specific color.
    colors: [Bitboard; Color::COUNT],

    /// All squares occupied by a specific piece kind.
    pieces: [Bitboard; PieceKind::COUNT],

    /// Redundant mailbox to speed up the [`Board::piece_at`] functions.
    mailbox: Table<Option<Piece>>,
}

impl Board {
    /// Creates a new, empty [`Board`] containing no pieces.
    #[inline(always)]
    pub const fn new() -> Self {
        Self {
            colors: [Bitboard::EMPTY_BOARD; Color::COUNT],
            pieces: [Bitboard::EMPTY_BOARD; PieceKind::COUNT],
            mailbox: Table::splat(None),
        }
    }

    /// Returns `true` if there is a piece at the given [`Square`], else `false`.
    ///
    /// # Example
    /// ```
    /// # use toad::{Board, Square};
    /// let board = Board::default();
    /// assert_eq!(board.has(Square::B1), true);
    /// ```
    #[inline(always)]
    pub const fn has(&self, square: Square) -> bool {
        self.mailbox.get(square).is_some()
    }

    /// Places the provided [`Piece`] and the supplied [`Square`].
    ///
    /// If another piece occupies this square, this does *not* remove that piece.
    /// Use [`Board::take`] first.
    ///
    /// # Example
    /// ```
    /// # use toad::{Board, Piece, PieceKind, Color, Square};
    /// let white_knight = Piece::new(Color::White, PieceKind::Knight);
    /// let mut board = Board::new();
    /// board.place(white_knight, Square::C4);
    /// assert_eq!(board.to_fen(), "8/8/8/8/2N5/8/8/8");
    /// ```
    #[inline(always)]
    pub fn place(&mut self, piece: Piece, square: Square) {
        self[piece.color()].set(square);
        self[piece.kind()].set(square);
        self.mailbox[square] = Some(piece);
    }

    /// Takes the [`Piece`] from a given [`Square`], if there is one present.
    #[inline(always)]
    pub fn take(&mut self, square: Square) -> Option<Piece> {
        // Take the piece from the mailbox, exiting early if there is none
        let piece = self.mailbox[square].take()?;

        // If there was a piece, clear the internal bitboards.
        self.colors[piece.color()].clear(square);
        self.pieces[piece.kind()].clear(square);

        Some(piece)
    }

    /// Fetches the [`Piece`] of the piece at the provided [`Square`], if there is one.
    ///
    /// # Example
    /// ```
    /// # use toad::{Board, PieceKind, Color, Square};
    /// let mut board = Board::default();
    /// assert_eq!(board.piece_at(Square::A2).unwrap().kind(), PieceKind::Pawn);
    /// assert_eq!(board.piece_at(Square::A2).unwrap().color(), Color::White);
    /// assert!(board.piece_at(Square::E4).is_none());
    /// ```
    #[inline(always)]
    pub const fn piece_at(&self, square: Square) -> Option<Piece> {
        *self.mailbox.get(square)
    }

    /// Fetches the [`Bitboard`] corresponding to the supplied [`PieceKind`].
    ///
    /// The returned [`Bitboard`] will hold the locations of every occurrence of each [`Piece`] matching the supplied [`PieceKind`].
    ///
    /// # Example
    /// ```
    /// # use toad::{Board, PieceKind, Bitboard};
    /// let board = Board::default();
    /// let pawns = board.kind(PieceKind::Pawn);
    /// assert_eq!(pawns, Bitboard::RANK_2 | Bitboard::RANK_7);
    /// ```
    #[inline(always)]
    pub const fn kind(&self, kind: PieceKind) -> Bitboard {
        self.pieces[kind.index()]
    }

    /// Fetches the [`Bitboard`] corresponding to the supplied [`Color`].
    ///
    /// The returned [`Bitboard`] will hold the locations of every occurrence each [`Piece`] matching the supplied [`Color`].
    ///
    /// # Example
    /// ```
    /// # use toad::{Board, Color, Piece, Bitboard};
    /// let board = Board::default();
    /// let white_pieces = board.color(Color::White);
    /// assert_eq!(white_pieces, Bitboard::RANK_1 | Bitboard::RANK_2);
    /// ```
    #[inline(always)]
    pub const fn color(&self, color: Color) -> Bitboard {
        self.colors[color.index()]
    }

    /// Fetches a [`Bitboard`] of all occupied squares on the board.
    #[inline(always)]
    pub const fn occupied(&self) -> Bitboard {
        self.color(Color::White).or(self.color(Color::Black))
    }

    /// Fetch a [`Bitboard`] of all pieces of `kind` and `color`.
    #[inline(always)]
    pub const fn piece_parts(&self, color: Color, kind: PieceKind) -> Bitboard {
        self.color(color).and(self.kind(kind))
    }

    /// Fetches the [`Bitboard`] for the Pawns of the provided color.
    #[inline(always)]
    pub const fn pawn(&self, color: Color) -> Bitboard {
        self.piece_parts(color, PieceKind::Pawn)
    }

    /// Fetches the [`Bitboard`] for the Knights of the provided color.
    #[inline(always)]
    pub const fn knight(&self, color: Color) -> Bitboard {
        self.piece_parts(color, PieceKind::Knight)
    }

    /// Fetches the [`Bitboard`] for the Bishops of the provided color.
    #[inline(always)]
    pub const fn bishop(&self, color: Color) -> Bitboard {
        self.piece_parts(color, PieceKind::Bishop)
    }

    /// Fetches the [`Bitboard`] for the Rooks of the provided color.
    #[inline(always)]
    pub const fn rook(&self, color: Color) -> Bitboard {
        self.piece_parts(color, PieceKind::Rook)
    }

    /// Fetches the [`Bitboard`] for the Queen(s) of the provided color.
    #[inline(always)]
    pub const fn queen(&self, color: Color) -> Bitboard {
        self.piece_parts(color, PieceKind::Queen)
    }

    /// Fetches the [`Bitboard`] for the King of the provided color.
    #[inline(always)]
    pub const fn king(&self, color: Color) -> Bitboard {
        self.piece_parts(color, PieceKind::King)
    }

    /// Fetches a [`Bitboard`] containing the locations of all orthogonal sliding pieces (Rook, Queen).
    #[inline(always)]
    pub fn orthogonal_sliders(&self, color: Color) -> Bitboard {
        (self.kind(PieceKind::Rook) | self.kind(PieceKind::Queen)) & self.color(color)
    }

    /// Fetches a [`Bitboard`] containing the locations of all diagonal sliding pieces (Bishop, Queen).
    #[inline(always)]
    pub fn diagonal_sliders(&self, color: Color) -> Bitboard {
        (self.kind(PieceKind::Bishop) | self.kind(PieceKind::Queen)) & self.color(color)
    }

    /// Fetches a [`Bitboard`] containing the locations of all sliding pieces (Rook, Bishop, Queen).
    #[inline(always)]
    pub fn sliders(&self, color: Color) -> Bitboard {
        (self.kind(PieceKind::Rook) | self.kind(PieceKind::Bishop) | self.kind(PieceKind::Queen))
            & self.color(color)
    }

    /// Get all squares that are either empty or occupied by the enemy
    ///
    /// # Example
    /// ```
    /// # use toad::{Bitboard, Board, Color};
    /// let board = Board::default();
    /// let not_white = board.enemy_or_empty(Color::White);
    /// assert_eq!(not_white.to_hex_string(), "0xFFFFFFFFFFFF0000");
    /// ```
    #[inline(always)]
    pub const fn enemy_or_empty(&self, color: Color) -> Bitboard {
        self.color(color).not()
    }

    /// Computes a [`Bitboard`] of all squares attacked by `color`.
    ///
    /// This is a raw attack map, meaning the squares on this map are unsafe for the friendly King to occupy.
    #[inline(always)]
    pub fn compute_attacks_by(&self, color: Color) -> Bitboard {
        let blockers = self.occupied();

        let mut attacks = self.pawn_attack_map(color);
        for square in self.knight(color) {
            attacks |= knight_attacks(square);
        }
        for square in self.diagonal_sliders(color) {
            attacks |= bishop_attacks(square, blockers);
        }
        for square in self.orthogonal_sliders(color) {
            attacks |= rook_attacks(square, blockers);
        }
        for square in self.king(color) {
            attacks |= king_attacks(square);
        }

        attacks
    }

    /// Computes a [`Bitboard`] of all squares attacked by `color` Pawns, excluding En Passant for convenience.
    #[inline(always)]
    pub fn pawn_attack_map(&self, color: Color) -> Bitboard {
        let pushes = self.pawn(color).forward_by(color, 1);
        pushes.east() | pushes.west()
    }

    /// Creates a [`BoardIter`] to iterate over all occupied [`Square`]s in this [`Board`].
    #[inline(always)]
    pub const fn iter(&self) -> BoardIter<'_> {
        BoardIter {
            board: self,
            occupancy: self.occupied(),
        }
    }

    /// Returns an iterator over all of the pieces in `mask` on this board along with their corresponding locations.
    #[inline(always)]
    pub const fn iter_for(&self, mask: Bitboard) -> BoardIter<'_> {
        BoardIter {
            board: self,
            occupancy: mask,
        }
    }

    /// Generates a [FEN](https://www.chess.com/terms/fen-chess) string of this [`Board`].
    pub fn to_fen(&self) -> String {
        let mut placements: [String; 8] = Default::default();

        for rank in Rank::iter() {
            let mut empty_spaces = 0;
            for file in File::iter() {
                if let Some(piece) = self.piece_at(file * rank) {
                    if empty_spaces != 0 {
                        placements[rank.index()] += &empty_spaces.to_string();
                        empty_spaces = 0;
                    }
                    placements[rank.index()] += piece.as_ref();
                } else {
                    empty_spaces += 1;
                }
            }

            if empty_spaces != 0 {
                placements[rank.index()] += &empty_spaces.to_string();
            }
        }
        placements.reverse();

        placements.join("/")
    }
}

impl Default for Board {
    #[inline(always)]
    fn default() -> Self {
        // Safety: The FEN for startpos is always valid
        unsafe {
            Game::<Standard>::from_fen(FEN_STARTPOS)
                .unwrap_unchecked()
                .board
        }
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(
            &SmallDisplayTable::from_fn(|sq| {
                [self
                    .piece_at(sq)
                    .map(|p| p.to_string())
                    .unwrap_or(String::from("."))]
            }),
            f,
        )
    }
}

impl From<[Option<Piece>; 64]> for Board {
    fn from(value: [Option<Piece>; 64]) -> Self {
        let mut board = Self::new();

        for (i, piece) in value.into_iter().enumerate() {
            if let Some(piece) = piece {
                board.place(piece, Square::from_index(i).unwrap())
            }
        }

        board
    }
}

impl Index<PieceKind> for Board {
    type Output = Bitboard;
    #[inline(always)]
    fn index(&self, index: PieceKind) -> &Self::Output {
        &self.pieces[index]
    }
}

impl IndexMut<PieceKind> for Board {
    #[inline(always)]
    fn index_mut(&mut self, index: PieceKind) -> &mut Self::Output {
        &mut self.pieces[index]
    }
}

impl Index<Color> for Board {
    type Output = Bitboard;
    #[inline(always)]
    fn index(&self, index: Color) -> &Self::Output {
        &self.colors[index]
    }
}

impl IndexMut<Color> for Board {
    #[inline(always)]
    fn index_mut(&mut self, index: Color) -> &mut Self::Output {
        &mut self.colors[index]
    }
}

impl Index<Square> for Board {
    type Output = Option<Piece>;
    #[inline(always)]
    fn index(&self, index: Square) -> &Self::Output {
        &self.mailbox[index]
    }
}

impl IndexMut<Square> for Board {
    #[inline(always)]
    fn index_mut(&mut self, index: Square) -> &mut Self::Output {
        &mut self.mailbox[index]
    }
}

impl<'a> IntoIterator for &'a Board {
    type IntoIter = BoardIter<'a>;
    type Item = <BoardIter<'a> as Iterator>::Item;
    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a> IntoIterator for &'a mut Board {
    type IntoIter = BoardIter<'a>;
    type Item = <BoardIter<'a> as Iterator>::Item;
    #[inline(always)]
    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl fmt::Debug for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let format = |to_fmt: &[(Bitboard, &str)]| {
            let strings = to_fmt
                .iter()
                .map(|(b, s)| (b.to_string(), s))
                .collect::<Vec<_>>();

            let splits = strings
                .iter()
                .map(|(b, _)| b.split('\n').collect::<Vec<_>>())
                .collect::<Vec<_>>();

            let labels = strings.iter().fold(String::new(), |mut acc, (_, s)| {
                _ = write!(acc, "{s:10}\t\t");
                acc
            });

            let boards = (0..8).fold(String::new(), |mut acc, i| {
                _ = writeln!(
                    acc,
                    "{}",
                    (0..splits.len()).fold(String::new(), |mut output, j| {
                        _ = write!(output, "{}\t", splits[j][i]);
                        output
                    })
                );
                acc
            });

            format!("{labels}\n{boards}")
        };

        let pieces = format(&[
            (self.pieces[PieceKind::Pawn], "Pawn"),
            (self.pieces[PieceKind::Knight], "Knight"),
            (self.pieces[PieceKind::Bishop], "Bishop"),
            (self.pieces[PieceKind::Rook], "Rook"),
            (self.pieces[PieceKind::Queen], "Queen"),
            (self.pieces[PieceKind::King], "King"),
        ]);

        let metadata = format(&[
            (self.occupied(), "Occupied"),
            (!self.occupied(), "Empty"),
            (self.colors[Color::White], "White"),
            (self.colors[Color::Black], "Black"),
        ]);

        write!(f, "Bitboards:\n{pieces}\n\n{metadata}")
    }
}

/// An iterator over a set of squares on a [`Board`].
///
/// Calls to [`Iterator::next`] will yield a tuple of a [`Square`] and a [`Piece`].
pub struct BoardIter<'a> {
    /// The board to retrieve pieces from.
    board: &'a Board,

    /// The list of squares to iterate over.
    occupancy: Bitboard,
}

impl Iterator for BoardIter<'_> {
    type Item = (Square, Piece);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        let square = self.occupancy.pop_lsb()?;

        // Safety: Because we early return when calling `pop_lsb` above, there is guaranteed to be a piece at `square`.
        let piece = unsafe { self.board.piece_at(square).unwrap_unchecked() };
        Some((square, piece))
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.occupancy.population() as usize;
        (size, Some(size))
    }
}

impl ExactSizeIterator for BoardIter<'_> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zobrist_key_side_to_move() {
        let fen = "r3k2r/pppp1ppp/8/4p3/8/8/PPPPPPPP/R3K2R w KQkq e6 0 1";
        let pos = Game::<Standard>::from_fen(fen).unwrap();

        let fen_black = "r3k2r/pppp1ppp/8/4p3/8/8/PPPPPPPP/R3K2R b KQkq - 0 1";
        let pos_black = Game::<Standard>::from_fen(fen_black).unwrap();

        assert_ne!(pos.key(), pos_black.key());
    }

    #[test]
    fn test_zobrist_key_ep() {
        let fen = "r3k2r/pppp1ppp/8/4p3/8/8/PPPPPPPP/R3K2R w KQkq e6 0 1";
        let pos = Game::<Standard>::from_fen(fen).unwrap();

        let fen_without_ep = "r3k2r/pppp1ppp/8/4p3/8/8/PPPPPPPP/R3K2R w KQkq - 0 1";
        let pos_without_ep = Game::<Standard>::from_fen(fen_without_ep).unwrap();

        assert_ne!(pos.key(), pos_without_ep.key());
    }

    #[test]
    fn test_zobrist_key_castling() {
        let fen = "r3k2r/pppp1ppp/8/4p3/8/8/PPPPPPPP/R3K2R w KQkq e6 0 1";
        let pos = Game::<Standard>::from_fen(fen).unwrap();

        let fen_without_k = "r3k2r/pppp1ppp/8/4p3/8/8/PPPPPPPP/R3K2R w KQq - 0 1";
        let pos_without_k = Game::<Standard>::from_fen(fen_without_k).unwrap();

        assert_ne!(pos.key(), pos_without_k.key());
    }

    #[test]
    fn test_zobrist_key_updates_on_quiet_moves() {
        let mut pos = Game::<Standard>::default();
        let original_key = pos.key();
        assert_ne!(original_key.inner(), 0);

        pos.make_move(Move::from_uci(&pos, "b1a3").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_eq!(pos.key(), ZobristKey::new(&pos));

        pos.make_move(Move::from_uci(&pos, "b8a6").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_eq!(pos.key(), ZobristKey::new(&pos));

        pos.make_move(Move::from_uci(&pos, "a3b1").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_eq!(pos.key(), ZobristKey::new(&pos));

        // After returning to the original position, they keys should be equal again
        pos.make_move(Move::from_uci(&pos, "a6b8").unwrap());
        assert_eq!(pos.key(), original_key);
        assert_eq!(pos.key(), ZobristKey::new(&pos));
    }

    // There are four cases in which castling rights can be lost:
    //  1. The King was moved
    //  2. A Rook was moved
    //  3. A Rook was captured
    //  4. Castling was performed
    //
    // I am also littering in some assertions on the Zobrist keys, just to be safe.

    #[test]
    fn test_castling_rights_update_on_king_move() {
        /***********************************/
        /* Test case 1: The King was moved */
        /***********************************/
        let fen = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1";
        let mut pos = Game::<Standard>::from_fen(fen).unwrap();
        let original_key = pos.key();
        let original_rights = pos.castling_rights().clone();

        // Moving the White King should remove White's castling rights
        pos.make_move(Move::from_uci(&pos, "e1d1").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_ne!(pos.castling_rights(), &original_rights);
        assert_eq!(pos.castling_rights_uci(), "kq");

        // Same for Black
        pos.make_move(Move::from_uci(&pos, "e8f8").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_ne!(pos.castling_rights(), &original_rights);
        assert_eq!(pos.castling_rights_uci(), "-");

        // Moving the White King back should NOT restore castling rights
        pos.make_move(Move::from_uci(&pos, "d1e1").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_ne!(pos.castling_rights(), &original_rights);
        assert_eq!(pos.castling_rights_uci(), "-");

        // Same for Black
        pos.make_move(Move::from_uci(&pos, "f8e8").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_eq!(pos.key(), ZobristKey::new(&pos));
        assert_ne!(pos.castling_rights(), &original_rights);
        assert_eq!(pos.castling_rights_uci(), "-");
    }

    #[test]
    fn test_castling_rights_update_on_rook_move() {
        /*********************************/
        /* Test case 2: A Rook was moved */
        /*********************************/
        let fen = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1";
        let mut pos = Game::<Standard>::from_fen(fen).unwrap();
        let original_key = pos.key();
        let original_rights = pos.castling_rights().clone();

        // Moving a Rook should disable castling for that side
        pos.make_move(Move::from_uci(&pos, "a1b1").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_ne!(pos.castling_rights(), &original_rights);
        assert_eq!(pos.castling_rights_uci(), "Kkq");

        // Same for Black
        pos.make_move(Move::from_uci(&pos, "a8b8").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_ne!(pos.castling_rights(), &original_rights);
        assert_eq!(pos.castling_rights_uci(), "Kk");

        // Moving the Rook back should NOT re-enable castling for that side
        pos.make_move(Move::from_uci(&pos, "b1a1").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_ne!(pos.castling_rights(), &original_rights);
        assert_eq!(pos.castling_rights_uci(), "Kk");

        // Same for Black
        pos.make_move(Move::from_uci(&pos, "b8a8").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_ne!(pos.castling_rights(), &original_rights);
        assert_eq!(pos.castling_rights_uci(), "Kk");
    }

    #[test]
    fn test_castling_rights_update_on_rook_captured() {
        /************************************/
        /* Test case 3: A Rook was captured */
        /************************************/
        let fen = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1";
        let mut pos = Game::<Standard>::from_fen(fen).unwrap();
        let original_key = pos.key();
        let original_rights = pos.castling_rights().clone();

        // Capturing a Rook should disable castling on that side for the side that lost the Rook
        pos.make_move(Move::from_uci(&pos, "a1a8").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_eq!(pos.key(), ZobristKey::new(&pos));
        assert_ne!(pos.castling_rights(), &original_rights);
        assert_eq!(pos.castling_rights_uci(), "Kk"); // White used it's H1 Rook to capture, so they lose their rights on that side as well

        // Same for Black, on the other side
        pos.make_move(Move::from_uci(&pos, "h8h1").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_eq!(pos.key(), ZobristKey::new(&pos));
        assert_ne!(pos.castling_rights(), &original_rights);
        assert_eq!(pos.castling_rights_uci(), "-");
    }

    #[test]
    fn test_castling_rights_update_on_castling_performed() {
        /***************************************/
        /* Test case 3: Castling was performed */
        /***************************************/
        let fen = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1";
        let mut pos = Game::<Standard>::from_fen(fen).unwrap();
        let original_key = pos.key();
        let original_rights = pos.castling_rights().clone();

        // Performing castling should remove that side's rights altogether
        pos.make_move(Move::from_uci(&pos, "e1g1").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_eq!(pos.key(), ZobristKey::new(&pos));
        assert_ne!(pos.castling_rights(), &original_rights);
        assert_eq!(pos.castling_rights_uci(), "kq");

        // Same for Black, on the other side
        pos.make_move(Move::from_uci(&pos, "e8c8").unwrap());
        assert_ne!(pos.key(), original_key);
        assert_eq!(pos.key(), ZobristKey::new(&pos));
        assert_ne!(pos.castling_rights(), &original_rights);
        assert_eq!(pos.castling_rights_uci(), "-");
    }

    #[test]
    fn test_castling_rights_update_on_promote_to_rook() {
        // Now for a more complicate scenario:
        // Black will capture White's H1 Rook,
        //  but White will promote a Pawn to a Rook,
        //  then capture Black's Rook on H1.
        //
        // Queenside/Long castling rights for White should NOT be restored!

        let fen = "4k2r/P7/8/8/r7/8/8/RB2K2R b KQk - 0 1";
        let mut pos = Game::<Standard>::from_fen(fen).unwrap();
        let original_key = pos.key();
        let original_rights = pos.castling_rights().clone();
        assert_eq!(pos.castling_rights_uci(), "KQk");

        // Black captures White's H1 Rook
        pos.make_move(Move::from_uci(&pos, "a4a1").unwrap());
        assert_eq!(pos.castling_rights_uci(), "Kk");

        // White promotes a Pawn to a Rook
        pos.make_move(Move::from_uci(&pos, "a7a8r").unwrap());
        assert_eq!(pos.castling_rights_uci(), "Kk");

        // Black moves it's King out of Check
        pos.make_move(Move::from_uci(&pos, "e8e7").unwrap());
        assert_eq!(pos.castling_rights_uci(), "K");

        // White captures Black's Rook on H1
        pos.make_move(Move::from_uci(&pos, "a8a1").unwrap());
        assert_eq!(pos.castling_rights_uci(), "K");

        // Despite having a Rook back on H1, White should NOT be able to queenside/long castle
        assert_ne!(pos.key(), original_key);
        assert_ne!(pos.castling_rights(), &original_rights);
    }

    #[test]
    fn test_nullmove_works() {
        let mut game = Game::<Standard>::from_fen(FEN_KIWIPETE).unwrap();
        assert_eq!(game.side_to_move(), Color::White);
        game.make_move_uci("a2a4").unwrap();
        assert_eq!(game.side_to_move(), Color::Black);
        assert_eq!(game.ep_square(), Some(Square::A3));
        let key_before_nullmove = game.key();

        game.make_nullmove();
        assert_eq!(game.side_to_move(), Color::White);
        assert!(game.ep_square().is_none());
        assert_ne!(game.key(), key_before_nullmove);
    }
}
