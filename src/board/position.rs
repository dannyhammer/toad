/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{
    fmt::{self, Write},
    ops::{Deref, Index, IndexMut},
    str::FromStr,
};

use anyhow::{anyhow, bail, Result};

use crate::{king_attacks, pawn_attacks, pawn_pushes};

use super::{
    attacks_for, Bitboard, Color, File, Move, MoveKind, MoveList, Piece, PieceKind, Rank, Square,
    ZobristKey, FEN_STARTPOS,
};

/// Represents the castling rights of a single player
#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash, Default)]
pub struct CastlingRights {
    /// If a right is `Some(square)`, then `square` is the *Rook*'s location
    pub(crate) short: Option<Square>,
    pub(crate) long: Option<Square>,
}

impl CastlingRights {
    /// Creates a new [`CastlingRights`] that permits castling with a Rook on the provided squares.
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
    pub(crate) board: Board,

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
    halfmove: usize,

    /// Number of moves since the beginning of the game.
    ///
    /// A fullmove is a complete turn by white and then by black.
    fullmove: usize,

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
    /// assert_eq!(state.to_fen(), "8/8/8/8/8/8/8/8 w - - 0 1");
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

    /// Creates a new [`Position`] from the provided FEN string.
    pub fn from_fen(fen: &str) -> Result<Self> {
        let mut pos = Self::new();
        let mut split = fen.trim().split(' ');
        let placements = split
            .next()
            .ok_or(anyhow!("FEN string must have piece placements."))?;
        pos.board = Board::from_fen(placements)?;

        let active_color = split.next().unwrap_or("w");
        pos.side_to_move = Color::from_str(active_color)?;

        // Castling is a bit more complicated; especially for Chess960
        let castling = split.next().unwrap_or("-");
        if castling.contains(['K', 'k', 'Q', 'q']) {
            pos.castling_rights[Color::White].short = castling.contains('K').then_some(Square::H1);
            pos.castling_rights[Color::White].long = castling.contains('Q').then_some(Square::A1);
            pos.castling_rights[Color::Black].short = castling.contains('k').then_some(Square::H8);
            pos.castling_rights[Color::Black].long = castling.contains('q').then_some(Square::A8);
        } else if castling.chars().any(|c| File::from_char(c).is_ok()) {
            for c in castling.chars() {
                let color = Color::from_bool(c.is_ascii_lowercase());
                let rook_file = File::from_char(c)?;
                let rook_square = Square::new(rook_file, Rank::first(color));

                let king_file = pos.board.king(color).to_square_unchecked().file();
                if rook_file > king_file {
                    pos.castling_rights[color].short = Some(rook_square);
                } else {
                    pos.castling_rights[color].long = Some(rook_square);
                }
            }
        }

        let en_passant_target = split.next().unwrap_or("-");
        pos.ep_square = match en_passant_target {
            "-" => None,
            square => Some(Square::from_uci(square)?),
        };

        let halfmove = split.next().unwrap_or("0");
        pos.halfmove = halfmove.parse().or(Err(anyhow!(
            "FEN string must have valid halfmove counter. Got {halfmove:?}"
        )))?;

        let fullmove = split.next().unwrap_or("1");
        pos.fullmove = fullmove.parse().or(Err(anyhow!(
            "FEN string must have valid fullmove counter. Got {fullmove:?}"
        )))?;

        pos.key = ZobristKey::new(&pos);

        Ok(pos)
    }

    /// Converts a [Scharnagl Number](https://en.wikipedia.org/wiki/Fischer_random_chess_numbering_scheme#Direct_derivation) to a Chess960 starting position.
    ///
    /// # Panics
    ///
    /// If `n >= 960`, as there are only 960 valid starting positions.
    ///
    /// # Examples
    /// ```
    /// # use toad::*;
    /// // 518 is the Scharnagl number for startpos
    /// let startpos = Position::from_960(518);
    /// assert_eq!(startpos, Position::from_fen(FEN_STARTPOS).unwrap());
    /// ```
    #[inline(always)]
    pub fn from_960(n: usize) -> Self {
        Self::from_d960(n, n)
    }

    /// Converts a pair of [Scharnagl Numbers](https://en.wikipedia.org/wiki/Fischer_random_chess_numbering_scheme#Direct_derivation) to a Double Fischer Random Chess starting position.
    ///
    /// # Panics
    ///
    /// If either Scharnagl number `>= 960`, as there are only 960 valid starting positions.
    pub fn from_d960(white_scharnagl: usize, black_scharnagl: usize) -> Self {
        // Fetch the starting arrangement
        let start_positions = if white_scharnagl != black_scharnagl {
            [
                Self::scharnagl_to_placements(white_scharnagl),
                Self::scharnagl_to_placements(black_scharnagl),
            ]
        } else {
            // Small time saver to avoid re-computing if the numbers are equal
            [Self::scharnagl_to_placements(white_scharnagl); 2]
        };

        // Actually place the pieces
        let mut pos = Self::new();
        for color in Color::all() {
            for (file, kind) in start_positions[color]
                .into_iter()
                .enumerate()
                .map(|(f, kind)| (File::new_unchecked(f as u8), kind))
            {
                // Place the piece
                let piece = Piece::new(color, kind);
                pos.place(piece, Square::new(file, Rank::first(color)));

                // Place the Pawn
                let pawn = Piece::new(color, PieceKind::Pawn);
                pos.place(pawn, Square::new(file, Rank::second(color)));
            }

            // Assign castling rights
            for file in File::iter() {
                let square = Square::new(file, Rank::first(color));
                if let Some(PieceKind::Rook) = pos.kind_at(square) {
                    pos.castling_rights[color].long = Some(square);
                    break;
                }
            }

            // There's probably a way to do this in a single loop, but this works, too.
            for file in File::iter().rev() {
                let square = Square::new(file, Rank::first(color));
                if let Some(PieceKind::Rook) = pos.kind_at(square) {
                    pos.castling_rights[color].short = Some(square);
                    break;
                }
            }
        }

        // Make sure to update the Zobrist key
        pos.key = ZobristKey::new(&pos);

        pos
    }

    /// Copies `self` and returns a [`Position`] after having applied the provided [`Move`].
    #[inline(always)]
    pub fn with_move_made(&self, mv: Move) -> Self {
        let mut copied = *self;
        copied.make_move(mv);
        copied
    }

    /// Generates a FEN string from this [`Position`].
    ///
    /// # Example
    /// ```
    /// # use toad::Position;
    /// let state = Position::default();
    /// assert_eq!(state.to_fen(), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    /// ```
    pub fn to_fen(&self) -> String {
        format!("{self}")
    }

    /// Generates a FEN string from this [`Position`] with castling rights in Chess960 format.
    ///
    /// # Example
    /// ```
    /// # use toad::Position;
    /// let state = Position::default();
    /// assert_eq!(state.to_960_fen(), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w HAha - 0 1");
    /// ```
    pub fn to_960_fen(&self) -> String {
        format!("{self:#}")
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

    /// If en passant can be performed, returns the destination of a pawn that would perform en passant.
    #[inline(always)]
    pub fn ep_target_square(&self) -> Option<Square> {
        self.ep_square()
            .map(|ep_square| ep_square.backward_by(self.side_to_move(), 1).unwrap())
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

    /// Returns the half-move counter of the current position.
    #[inline(always)]
    pub const fn halfmove(&self) -> usize {
        self.halfmove
    }

    /// Returns the full-move counter of the current position.
    #[inline(always)]
    pub const fn fullmove(&self) -> usize {
        self.fullmove
    }

    /// Fetch the Zobrist hash key of this position.
    #[inline(always)]
    pub fn key(&self) -> ZobristKey {
        self.key
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
    /// let kk: Position = "8/4k3/8/8/3K4/8/8/8 w - - 0 1".parse().unwrap();
    /// assert!(kk.can_draw_by_insufficient_material());
    ///
    /// // A single Bishop (either color)
    /// let kbk: Position = "8/4k3/8/8/3K4/8/5B2/8 w - - 0 1".parse().unwrap();
    /// assert!(kbk.can_draw_by_insufficient_material());
    ///
    /// // A single Knight
    /// let knk: Position = "8/4k3/2n5/8/3K4/8/8/8 w - - 0 1".parse().unwrap();
    /// assert!(knk.can_draw_by_insufficient_material());
    ///
    /// // Opposing Bishops on the same color square
    /// let same_square_bishops: Position = "8/2b1k3/8/8/3K4/8/5B2/8 w - - 0 1".parse().unwrap();
    /// assert!(same_square_bishops.can_draw_by_insufficient_material());
    ///
    /// // Opposing Bishops on different color squares
    /// let diff_square_bishops: Position = "8/3bk3/8/8/3K4/8/5B2/8 w - - 0 1".parse().unwrap();
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
        let wb = self.bishops(Color::White);
        let wn = self.knights(Color::White);
        let bb = self.bishops(Color::Black);
        let bn = self.knights(Color::Black);

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
    #[inline(always)]
    pub fn toggle_side_to_move(&mut self) {
        self.side_to_move = self.side_to_move.opponent();
    }

    /// Fetches this position's [`Board`]
    #[inline(always)]
    pub const fn board(&self) -> &Board {
        &self.board
    }

    /// Mutably fetches this position's [`Board`]
    #[inline(always)]
    pub fn board_mut(&mut self) -> &mut Board {
        &mut self.board
    }

    /// Returns `true` if `color` can castle (either short or long).
    #[inline(always)]
    pub const fn can_castle(&self, color: Color) -> bool {
        self.castling_rights()[color.index()].short.is_some()
            || self.castling_rights()[color.index()].long.is_some()
    }

    /// According to [FIDE](https://en.wikipedia.org/wiki/Threefold_repetition#Statement_of_the_rule) rules,
    /// two positions are considered the same if they share the same piece layout, castling rights, and en passant square.
    /// Fullmove and Halfmove clocks are ignored.
    ///
    /// This does _not_ check the [`ZobristKey`] of each [`Position`].
    #[inline(always)]
    pub fn is_same_as(&self, other: &Self) -> bool {
        self.side_to_move() == other.side_to_move()
            && self.ep_square() == other.ep_square()
            && self.castling_rights() == other.castling_rights()
            && self.board() == other.board()
    }

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

    /// Apply the provided `moves` to the board. No enforcement of legality.
    #[inline(always)]
    pub fn make_moves(&mut self, moves: impl IntoIterator<Item = Move>) {
        for mv in moves {
            self.make_move(mv);
        }
    }

    /// Applies the move. No enforcement of legality
    pub fn make_move(&mut self, mv: Move) {
        // Remove the piece from it's previous location, exiting early if there is no piece there
        let Some(mut piece) = self.take(mv.from()) else {
            return;
        };

        let color = piece.color();
        let mut to = mv.to();
        let from = mv.from();

        // Un-hash the side-to-move
        self.key.hash_side_to_move(self.side_to_move());

        // Clear the EP square from the last move (and un-hash it)
        if let Some(ep_square) = self.ep_square.take() {
            self.key.hash_ep_square(ep_square);
        }

        // Increment move counters
        self.halfmove += 1; // This is reset if a capture occurs or a pawn moves
        self.fullmove += self.side_to_move().index();

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
            let victim = self.take(victim_square).unwrap();
            // let victim = unsafe { self.take(victim_square).unwrap_unchecked() };
            let victim_color = victim.color();

            // If the capture was on a rook's starting square, disable that side's castling.
            if self.castling_rights[victim_color]
                .long
                .is_some_and(|sq| victim_square == sq)
            {
                self.clear_long_castling_rights(victim_color);
            } else if self.castling_rights[victim_color]
                .short
                .is_some_and(|sq| victim_square == sq)
            {
                self.clear_short_castling_rights(victim_color);
            }

            // Reset halfmove counter, since a capture occurred
            self.halfmove = 0;
        } else if mv.is_pawn_double_push() {
            // Double pawn push, so set the EP square
            self.ep_square = from.forward_by(color, 1);
            self.key.hash_optional_ep_square(self.ep_square());
        } else if mv.is_short_castle() {
            // Safety; This is a castle. There *must* be a Rook at `to`.
            // let rook = unsafe { self.take(to).unwrap_unchecked() };
            let rook = self.take(to).unwrap();
            self.place(rook, Square::rook_short_castle(color));

            // The King doesn't actually move to the Rook, so update the destination square
            to = Square::king_short_castle(color);

            // Disable castling
            self.clear_castling_rights(color);
        } else if mv.is_long_castle() {
            // Safety; This is a castle. There *must* be a Rook at `to`.
            // let rook = unsafe { self.take(to).unwrap_unchecked() };
            let rook = self.take(to).unwrap();
            self.place(rook, Square::rook_long_castle(color));

            // The King doesn't actually move to the Rook, so update the destination square
            to = Square::king_long_castle(color);

            // Disable castling
            self.clear_castling_rights(color);
        }

        // Next, handle special cases for Pawn (halfmove), Rook, and King (castling)
        match piece.kind() {
            PieceKind::Pawn => self.halfmove = 0,

            // Disable castling if a rook moved for the first time
            PieceKind::Rook => {
                if self.castling_rights[color]
                    .long
                    .is_some_and(|sq| from == sq)
                {
                    self.clear_long_castling_rights(color);
                } else if self.castling_rights[color]
                    .short
                    .is_some_and(|sq| from == sq)
                {
                    self.clear_short_castling_rights(color);
                }
            }

            PieceKind::King => self.clear_castling_rights(color),

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
        self.key.hash_side_to_move(self.side_to_move());
    }

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
    pub fn get_pseudo_legal_moves_from(&self, mask: Bitboard) -> MoveList {
        let mut moves = MoveList::default();
        let color = self.side_to_move();
        let opponent = color.opponent();
        let blockers = self.occupied();
        // Cannot capture enemy king, so remove him from the possible target squares
        let enemy_or_empty = self.enemy_or_empty(color) ^ self.king(opponent);
        // Ensure the mask ONLY contains the side-to-move's pieces
        let mask = mask & self.color(color);

        let pawns = self.pawns(color) & mask;
        let king = self.king(color) & mask;
        let normal_pieces = (blockers ^ pawns ^ king) & mask;

        // Pawns first
        for from in pawns {
            let attacks = pawn_attacks(from, color)
                & (self.color(opponent)
                    | self.ep_square().map(|sq| sq.bitboard()).unwrap_or_default());

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

        // King next
        for from in king {
            // Castling is handled separately from regular attacks
            if let Some(rook) = self.castling_rights_for(color).short {
                moves.push(Move::new(from, rook, MoveKind::ShortCastle));
            }

            if let Some(rook) = self.castling_rights_for(color).long {
                moves.push(Move::new(from, rook, MoveKind::LongCastle));
            }

            // Attacks are either quiet or captures
            for to in king_attacks(from) & enemy_or_empty {
                let kind = if self.has(to) {
                    MoveKind::Capture
                } else {
                    MoveKind::Quiet
                };

                moves.push(Move::new(from, to, kind));
            }
        }

        // All remaining pieces
        for (from, piece) in self.iter_for(normal_pieces) {
            let attacks = attacks_for(piece, from, blockers) & enemy_or_empty;

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

        moves
    }

    /// Places a piece at the provided square, updating Zobrist hash information.
    #[inline(always)]
    fn place(&mut self, piece: Piece, square: Square) {
        self.board_mut().place(piece, square);
        self.key.hash_piece(square, piece);
    }

    /// Removes and returns a piece on the provided square, updating Zobrist hash information.
    #[inline(always)]
    fn take(&mut self, square: Square) -> Option<Piece> {
        let piece = self.board_mut().take(square)?;
        self.key.hash_piece(square, piece);
        Some(piece)
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

    /// Internal function to convert a [Scharnagl Number](https://en.wikipedia.org/wiki/Fischer_random_chess_numbering_scheme#Direct_derivation) to a Chess960 starting position.
    ///
    /// This function returns an array of [`PieceKind`]s in the order they should be placed in White's home rank.
    ///
    /// # Panics
    /// If `n >= 960`, as there are only 960 valid starting positions.
    fn scharnagl_to_placements(n: usize) -> [PieceKind; File::COUNT] {
        assert!(n < 960, "Must be [0, 960)");

        // Start with Pawns and replace them as we go
        let mut startpos = [PieceKind::Pawn; File::COUNT];

        // Divide N by 4, yielding quotient N2 and remainder B1.
        // Place a Bishop upon the bright square corresponding to B1 (0=b, 1=d, 2=f, 3=h).
        let (n2, b1) = (n / 4, n % 4);
        startpos[1 + b1 * 2] = PieceKind::Bishop;

        // Divide N2 by 4 again, yielding quotient N3 and remainder B2.
        // Place a second Bishop upon the dark square corresponding to B2 (0=a, 1=c, 2=e, 3=g).
        let (n3, b2) = (n2 / 4, n2 % 4);
        startpos[b2 * 2] = PieceKind::Bishop;

        // Divide N3 by 6, yielding quotient N4 and remainder Q.
        // Place the Queen according to Q, where 0 is the first free square starting from a, 1 is the second, etc.
        let (n4, q) = (n3 / 6, n3 % 6);
        let mut num_empty = 0;
        for kind in &mut startpos {
            if matches!(kind, PieceKind::Pawn) {
                if num_empty == q {
                    *kind = PieceKind::Queen;
                }
                num_empty += 1;
            }
        }

        //  N4 will be a single digit, 0 ... 9.
        // Ignoring Bishops and Queen, find the positions of two Knights within the remaining five spaces.
        // Place the Knights according to its value by consulting the following N5N table:
        let (kn1, kn2) = match n4 {
            0 => (0, 0),
            1 => (0, 1),
            2 => (0, 2),
            3 => (0, 3),
            4 => (1, 0),
            5 => (1, 1),
            6 => (1, 2),
            7 => (2, 0),
            8 => (2, 1),
            9 => (3, 0),
            _ => unreachable!(),
        };

        let mut num_empty = 0;
        let mut first_knight = 0;
        for (i, kind) in startpos.iter_mut().enumerate() {
            if matches!(kind, PieceKind::Pawn) {
                if num_empty == kn1 {
                    *kind = PieceKind::Knight;
                    first_knight = i;
                    num_empty = 0;
                    break;
                }
                num_empty += 1;
            }
        }

        // Now for the second Knight
        for kind in startpos.iter_mut().skip(first_knight) {
            if matches!(kind, PieceKind::Pawn) {
                if num_empty == kn2 {
                    *kind = PieceKind::Knight;
                    break;
                }
                num_empty += 1;
            }
        }

        // There are three blank squares remaining; place a Rook in each of the outer two and the King in the middle one.
        let remaining = [PieceKind::Rook, PieceKind::King, PieceKind::Rook];
        let mut j = 0;
        for kind in &mut startpos {
            if matches!(kind, PieceKind::Pawn) {
                *kind = remaining[j];
                j += 1;
            }
        }

        startpos
    }
}

impl FromStr for Position {
    type Err = anyhow::Error;
    #[inline(always)]
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Self::from_fen(s)
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
        unsafe { Self::from_fen(FEN_STARTPOS).unwrap_unchecked() }
    }
}

impl fmt::Display for Position {
    /// Display this position's FEN string.
    ///
    /// If the alternate format mode (`#`) was specified, this will print the castling rights in Chess960 format.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let placements = self.board().to_fen();
        let active_color = self.side_to_move();

        let castling = if f.alternate() {
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

        write!(
            f,
            "{placements} {active_color} {castling} {en_passant_target} {halfmove} {fullmove}"
        )
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
                write!(f, "           FEN: {}", self.to_fen())?;
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
    mailbox: [Option<Piece>; Square::COUNT],
}

impl Board {
    /// Creates a new, empty [`Board`] containing no pieces.
    ///
    /// # Example
    /// ```
    /// # use toad::Board;
    /// let board = Board::new();
    /// assert_eq!(board.to_fen(), "8/8/8/8/8/8/8/8");
    /// ```
    #[inline(always)]
    pub const fn new() -> Self {
        Self {
            colors: [Bitboard::EMPTY_BOARD; Color::COUNT],
            pieces: [Bitboard::EMPTY_BOARD; PieceKind::COUNT],
            mailbox: [None; Square::COUNT],
        }
    }

    /// Constructs a [`Board`] from the provided FEN string, ignoring castling/ep/move counters.
    pub fn from_fen(fen: &str) -> Result<Self> {
        let mut board = Self::new();

        // If this FEN string contains more than just the initial placements, extract the placements
        let placements = if fen.contains(' ') {
            fen.split(' ').next().unwrap()
        } else {
            fen
        };

        // Check if the placements string is the correct length
        if placements.matches('/').count() != 7 {
            bail!("FEN must have piece placements for all 8 ranks");
        }

        // Need to reverse this so that White pieces are at the "bottom" of the board
        for (rank, placements) in placements.split('/').rev().enumerate() {
            let mut file = 0;
            let rank = rank as u8;

            for piece_char in placements.chars() {
                // If the next char is a piece, we need to update the relevant Bitboards
                if let Ok(piece) = Piece::from_uci(piece_char) {
                    // Firstly, create a square and set the "Occupied" board at this location.
                    let square = Square::new(File::new_unchecked(file), Rank::new_unchecked(rank));

                    board.place(piece, square);

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

        Ok(board)
    }

    /*
    /// Returns an instance of this [`Board`] that has the additional bits specified by `mask` set, according to the [`Piece`] supplied.
    ///
    /// If `mask` contains only 1 square, use [`Board::with`] instead, as it is likely to be faster.
    pub const fn with(self, mask: Bitboard, piece: Piece) -> Self {
        let (color, kind) = piece.parts();

        let mut colors = self.colors;
        colors[color.index()] = colors[color.index()].or(mask);

        let mut pieces = self.pieces;
        pieces[kind.index()] = pieces[kind.index()].or(mask);

        Self { colors, pieces }
    }
     */

    /*
    /// Returns an instance of this [`Board`] that has all bits specified by `mask` cleared.
    pub fn without(self, mask: Bitboard) -> Self {
        let not_mask = !mask;

        let mut colors = self.colors;
        for color in Color::all() {
            colors[color] &= not_mask;
        }

        let mut pieces = self.pieces;
        for kind in PieceKind::all() {
            pieces[kind] &= not_mask;
        }

        Self { colors, pieces }
    }
      */

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
        self.mailbox[square.index()].is_some()
    }

    /// Places the provided [`Piece`] and the supplied [`Square`].
    ///
    /// If another piece occupies this square, this does *not* remove that piece.
    /// Use [`Board::clear`] first.
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

    /// Clears the supplied [`Square`] of any pieces.
    ///
    /// # Example
    /// ```
    /// # use toad::{Board, Square};
    /// let mut board = Board::from_fen("k7/8/8/8/2N5/8/8/7K").unwrap();
    /// board.clear(Square::C4);
    /// assert_eq!(board.to_fen(), "k7/8/8/8/8/8/8/7K");
    /// ```
    #[inline(always)]
    pub fn clear(&mut self, square: Square) {
        self.take(square);
    }

    /// Takes the [`Piece`] from a given [`Square`], if there is one present.
    ///
    /// # Example
    /// ```
    /// # use toad::{Board, Piece, PieceKind, Color, Square};
    /// let mut board = Board::from_fen("k7/8/8/8/2N5/8/8/7K").unwrap();
    /// let white_knight = Piece::new(Color::White, PieceKind::Knight);
    /// let taken = board.take(Square::C4);
    /// assert_eq!(board.to_fen(), "k7/8/8/8/8/8/8/7K");
    /// assert_eq!(taken, Some(white_knight));
    /// ```
    #[inline(always)]
    pub fn take(&mut self, square: Square) -> Option<Piece> {
        // Take the piece from the mailbox, exiting early if there is none
        let piece = self.mailbox[square].take()?;

        // If there was a piece, clear the internal bitboards.
        self.colors[piece.color()].clear(square);
        self.pieces[piece.kind()].clear(square);

        Some(piece)
    }

    /// Clears the entire board, removing all pieces.
    ///
    /// # Example
    /// ```
    /// # use toad::Board;
    /// let mut board = Board::default();
    /// board.clear_all();
    /// assert_eq!(board.to_fen(), "8/8/8/8/8/8/8/8");
    /// ```
    #[inline(always)]
    pub fn clear_all(&mut self) {
        *self = Self::new();
    }

    /// Fetches the [`Color`] of the piece at the provided [`Square`], if there is one.
    ///
    /// # Example
    /// ```
    /// # use toad::{Board, Color, Square};
    /// let board = Board::default();
    /// assert_eq!(board.color_at(Square::A2), Some(Color::White));
    /// assert_eq!(board.color_at(Square::E8), Some(Color::Black));
    /// assert!(board.color_at(Square::E4).is_none());
    /// ```
    #[inline(always)]
    pub fn color_at(&self, square: Square) -> Option<Color> {
        self.mailbox[square].map(|piece| piece.color())
    }

    /// Fetches the [`PieceKind`] of the piece at the provided [`Square`], if there is one.
    ///
    /// # Example
    /// ```
    /// # use toad::{Board, PieceKind, Square};
    /// let mut board = Board::default();
    /// assert_eq!(board.kind_at(Square::A2), Some(PieceKind::Pawn));
    /// assert!(board.kind_at(Square::E4).is_none());
    /// ```
    #[inline(always)]
    pub fn kind_at(&self, square: Square) -> Option<PieceKind> {
        self.mailbox[square].map(|piece| piece.kind())
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
        self.mailbox[square.index()]
    }

    /// Fetches the [`Piece`] of the piece at the provided [`Square`], without checking if one is there.
    ///
    /// The primary use case of this function is to get the piece at a [`Move`]'s `to` field.
    ///
    /// It is undefined behavior to call this function on a square that has no piece.
    #[inline(always)]
    pub fn piece_at_unchecked(&self, square: Square) -> Piece {
        unsafe { self.piece_at(square).unwrap_unchecked() }
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

    /// Fetches a [`Bitboard`] of all non-occupied squares on the board.
    #[inline(always)]
    pub const fn empty(&self) -> Bitboard {
        self.occupied().not()
    }

    /// Fetches the [`Bitboard`] corresponding to the supplied [`Piece`].
    ///
    /// The returned [`Bitboard`] will hold the locations of every occurrence of the supplied [`Piece`].
    ///
    /// # Example
    /// ```
    /// # use toad::{Board, PieceKind, Color, Piece, Bitboard};
    /// let board = Board::default();
    /// let white_pawn = Piece::new(Color::White, PieceKind::Pawn);
    /// let white_pawns = board.piece(white_pawn);
    /// assert_eq!(white_pawns, Bitboard::RANK_2);
    /// ```
    #[inline(always)]
    pub const fn piece(&self, piece: Piece) -> Bitboard {
        self.piece_parts(piece.color(), piece.kind())
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

    /// Analogous to [`Board::piece`] with a [`Piece`]'s individual components.
    ///
    /// If you have a [`PieceKind`] and a [`Color`] already, this is likely to be *slightly*
    /// faster that constructing a [`Piece`] and calling [`Board::piece`].
    #[inline(always)]
    pub const fn piece_parts(&self, color: Color, kind: PieceKind) -> Bitboard {
        self.color(color).and(self.kind(kind))
    }

    /// Fetches the [`Bitboard`] for the Pawns of the provided color.
    #[inline(always)]
    pub const fn pawns(&self, color: Color) -> Bitboard {
        self.piece_parts(color, PieceKind::Pawn)
    }

    /// Fetches the [`Bitboard`] for the Knights of the provided color.
    #[inline(always)]
    pub const fn knights(&self, color: Color) -> Bitboard {
        self.piece_parts(color, PieceKind::Knight)
    }

    /// Fetches the [`Bitboard`] for the Bishops of the provided color.
    #[inline(always)]
    pub const fn bishops(&self, color: Color) -> Bitboard {
        self.piece_parts(color, PieceKind::Bishop)
    }

    /// Fetches the [`Bitboard`] for the Rooks of the provided color.
    #[inline(always)]
    pub const fn rooks(&self, color: Color) -> Bitboard {
        self.piece_parts(color, PieceKind::Rook)
    }

    /// Fetches the [`Bitboard`] for the Queen(s) of the provided color.
    #[inline(always)]
    pub const fn queens(&self, color: Color) -> Bitboard {
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
        unsafe { Self::from_fen(FEN_STARTPOS).unwrap_unchecked() }
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Allocate just enough capacity
        let mut board = String::with_capacity(198);

        for rank in Rank::iter().rev() {
            board += &format!("{rank}| ");

            for file in File::iter() {
                let square = Square::new(file, rank);
                let occupant = if let Some(piece) = self.piece_at(square) {
                    piece.to_string()
                } else {
                    // String::from(if square.is_light() { "#" } else { "-" })
                    String::from(".")
                };

                board += &format!("{occupant} ");
            }

            board += "\n"
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
            (self.empty(), "Empty"),
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

impl<'a> Iterator for BoardIter<'a> {
    type Item = (Square, Piece);

    #[inline(always)]
    fn next(&mut self) -> Option<Self::Item> {
        let square = self.occupancy.pop_lsb()?;

        // Safety: Because we early return when calling `pop_lsb` above, there is guaranteed to be a piece at `square`.
        let piece = self.board.piece_at_unchecked(square);
        Some((square, piece))
    }

    #[inline(always)]
    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.occupancy.population() as usize;
        (size, Some(size))
    }
}

impl<'a> ExactSizeIterator for BoardIter<'a> {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_zobrist_key_side_to_move() {
        let fen = "r3k2r/pppp1ppp/8/4p3/8/8/PPPPPPPP/R3K2R w KQkq e6 0 1";
        let pos = Position::from_fen(fen).unwrap();

        let fen_black = "r3k2r/pppp1ppp/8/4p3/8/8/PPPPPPPP/R3K2R b KQkq - 0 1";
        let pos_black = Position::from_fen(fen_black).unwrap();

        assert_ne!(pos.key(), pos_black.key());
    }

    #[test]
    fn test_zobrist_key_ep() {
        let fen = "r3k2r/pppp1ppp/8/4p3/8/8/PPPPPPPP/R3K2R w KQkq e6 0 1";
        let pos = Position::from_fen(fen).unwrap();

        let fen_without_ep = "r3k2r/pppp1ppp/8/4p3/8/8/PPPPPPPP/R3K2R w KQkq - 0 1";
        let pos_without_ep = Position::from_fen(fen_without_ep).unwrap();

        assert_ne!(pos.key(), pos_without_ep.key());
    }

    #[test]
    fn test_zobrist_key_castling() {
        let fen = "r3k2r/pppp1ppp/8/4p3/8/8/PPPPPPPP/R3K2R w KQkq e6 0 1";
        let pos = Position::from_fen(fen).unwrap();

        let fen_without_k = "r3k2r/pppp1ppp/8/4p3/8/8/PPPPPPPP/R3K2R w KQq - 0 1";
        let pos_without_k = Position::from_fen(fen_without_k).unwrap();

        assert_ne!(pos.key(), pos_without_k.key());
    }

    #[test]
    fn test_zobrist_key_updates_on_quiet_moves() {
        let mut pos = Position::default();
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
        let mut pos = Position::from_fen(fen).unwrap();
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
        let mut pos = Position::from_fen(fen).unwrap();
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
        let mut pos = Position::from_fen(fen).unwrap();
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
        let mut pos = Position::from_fen(fen).unwrap();
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
        let mut pos = Position::from_fen(fen).unwrap();
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
}
