/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{
    fmt::{self, Write},
    ops::Deref,
    str::FromStr,
};

use anyhow::Result;

use super::{
    bishop_attacks, bishop_rays, compute_attacks_by, king_attacks, knight_attacks, pawn_attacks,
    pawn_pushes, perft, ray_between, ray_containing, rook_attacks, rook_rays, Bitboard, Color,
    File, Move, MoveKind, MoveList, PieceKind, Position, Rank, Square,
};

/// A game of chess.
///
/// This type encapsulates a [`Position`] and adds metadata about piece movements, such as all pieces currently checking the side-to-move's King.
/// It is the primary type for working with a chess game, and is suitable for use in engines.
///
/// The basic methods you're probably looking for are [`Game::from_fen`], [`Game::make_move`], and [`Game::get_legal_moves`].
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Game {
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

    /// Pseudo-legal attacks from every given square on the board.
    // attacks_by_square: [Bitboard; Square::COUNT],

    /// The square where the side-to-move's King resides.
    king_square: Square,
}

impl Game {
    /// Creates a new [`Game`] from  the provided [`Position`].
    #[inline(always)]
    pub fn new(position: Position) -> Self {
        let mut game = Self {
            position,
            checkers: Bitboard::EMPTY_BOARD,
            checkmask: Bitboard::EMPTY_BOARD,
            pinned: Bitboard::EMPTY_BOARD,
            attacks_by_color: [Bitboard::EMPTY_BOARD; Color::COUNT],
            // attacks_by_square,
            king_square: Square::default(),
        };

        game.recompute_legal_masks();
        game
    }

    /// Creates a new [`Game`] from the provided FEN string.
    #[inline(always)]
    pub fn from_fen(fen: &str) -> Result<Self> {
        Ok(Self::new(Position::from_fen(fen)?))
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
    /// let startpos = Game::from_960(518);
    /// assert_eq!(startpos, Game::from_fen(FEN_STARTPOS).unwrap());
    /// ```
    #[inline(always)]
    pub fn from_960(n: usize) -> Self {
        Self::new(Position::from_d960(n, n))
    }

    /// Converts a pair of [Scharnagl Numbers](https://en.wikipedia.org/wiki/Fischer_random_chess_numbering_scheme#Direct_derivation) to a Double Fischer Random Chess starting position.
    ///
    /// # Panics
    ///
    /// If either Scharnagl number `>= 960`, as there are only 960 valid starting positions.
    #[inline(always)]
    pub fn from_d960(white_scharnagl: usize, black_scharnagl: usize) -> Self {
        Self::new(Position::from_d960(white_scharnagl, black_scharnagl))
    }

    /// Copies `self` and returns a [`Game`] after having applied the provided [`Move`].
    #[inline(always)]
    pub fn with_move_made(&self, mv: Move) -> Self {
        let mut copied = *self;
        copied.make_move(mv);
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
    /// let mut game = Game::default();
    /// assert_eq!(game.to_fen(), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    /// game.toggle_side_to_move();
    /// assert_eq!(game.to_fen(), "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR b KQkq - 0 1");
    /// ```
    #[inline(always)]
    pub fn toggle_side_to_move(&mut self) {
        self.position.toggle_side_to_move();
        self.recompute_legal_masks();
    }

    /// Applies the move, if it is legal to make. If it is not legal, returns an `Err` explaining why.
    #[inline(always)]
    pub fn make_move_checked(&mut self, mv: Move) -> Result<()> {
        self.check_pseudo_legality_of(mv)?;
        self.make_move(mv);
        Ok(())
    }

    /// Returns `true` if the side-to-move is currently in check.
    #[inline(always)]
    pub const fn is_in_check(&self) -> bool {
        self.checkers.population() > 0
    }

    /// Returns `true` if the side-to-move is currently in double check (in check by more than one piece).
    #[inline(always)]
    pub const fn is_in_double_check(&self) -> bool {
        self.checkers.population() > 1
    }

    /// Returns a [`Bitboard`] of all squares attacked by `color`.
    #[inline(always)]
    pub const fn attacks_by(&self, color: Color) -> Bitboard {
        self.attacks_by_color[color.index()]
    }

    /// Recursively make all legal moves available until the supplied depth is reached, returning the total number of positions reachable.
    ///
    /// This is just a convenience method.
    /// See [`perft()`] for more.
    #[inline(always)]
    pub fn perft(&self, depth: usize) -> u64 {
        perft(self, depth)
    }

    /// Converts the provided string to a [`Move`], if possible, and applies it to the game.
    ///
    /// Equivalent to calling [`Move::from_uci`] and [`Game::make_move`].
    #[inline(always)]
    pub fn make_move_uci(&mut self, mv_str: &str) -> Result<()> {
        let mv = Move::from_uci(self, mv_str)?;
        self.make_move(mv);
        Ok(())
    }

    /// Applies the provided [`Move`]. No enforcement of legality.
    #[inline(always)]
    pub fn make_move(&mut self, mv: Move) {
        // Actually make the move
        self.position.make_move(mv);

        // Now update movegen metadata
        self.recompute_legal_masks();
    }

    /// Applies the provided [`Move`]s. No enforcement of legality.
    #[inline(always)]
    pub fn make_moves(&mut self, moves: impl IntoIterator<Item = Move>) {
        for mv in moves {
            self.make_move(mv);
        }
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
        if mv.is_castle() {
            // Get the destinations for the King and Rook
            let (king_dst, rook_dst) = {
                let (king_file, rook_file) = if to.file() > from.file() {
                    (File::G, File::F)
                } else {
                    (File::C, File::D)
                };
                let rank = from.rank();
                (Square::new(king_file, rank), Square::new(rook_file, rank))
            };

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
    /// If you want to incrementally generated moves one-by-one, use [`MoveGenIter`].
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
    /// use toad::{Game, Color};
    /// let game = Game::default();
    /// let mask = game.knights(Color::White);
    /// let mut knight_moves = game.get_legal_moves_from(mask).into_iter();
    ///
    /// assert_eq!(knight_moves.next().unwrap(), "b1a3");
    /// assert_eq!(knight_moves.next().unwrap(), "b1c3");
    /// assert_eq!(knight_moves.next().unwrap(), "g1f3");
    /// assert_eq!(knight_moves.next().unwrap(), "g1h3");
    /// assert!(knight_moves.next().is_none());
    /// ```
    #[inline(always)]
    pub fn get_legal_moves_from(&self, mask: Bitboard) -> MoveList {
        let mut moves = MoveList::default();
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

        self.king_square = self.king(color).to_square_unchecked();

        // Reset the pinmask and checkmask
        self.pinned = Bitboard::EMPTY_BOARD;
        // Sanity check; no move can capture the enemy King, so his square is removed
        self.checkmask = self.enemy_or_empty(color) ^ self.king(opponent);

        // Starting off, the easiest checkers to find are Knights and Pawns; just the overlap of their attacks from the King and themselves.
        self.checkers = self.knights(opponent) & knight_attacks(self.king_square)
            | self.pawns(opponent) & pawn_attacks(self.king_square, color);

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
            self.attacks_by_color[color] = compute_attacks_by(self.board(), color);
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
        for from in self.pawns(color) & mask {
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

    // fn generate_pawn_moves<const IN_CHECK: bool>(&self, moves: &mut MoveList) {
    //     let color = self.side_to_move();

    //     // Any pawn can push forward once, so long as there's nothing blocking it & it's not horizontally pinned
    //     let pawns_that_can_push = self.pawns(color);
    //     let single_pushes = pawns_that_can_push.advance_by(color, 1);
    // }
    /*
    // TODO: https://github.com/dannyhammer/brogle/issues/9
    fn compute_pawn_moves(
        &self,
        color: Color,
        checkmask: Bitboard,
        moves: &mut MoveList,
    ) {
        // Fetch all pinned and unpinned pawns
        let pinned_pawns = self.pawns(color) & self.pinmask();
        let unpinned_pawns = self.pawns(color) & !self.pinmask();
        // eprintln!("PINNED PAWNS:\n{pinned_pawns:?}");
        // eprintln!("UNPINNED PAWNS:\n{unpinned_pawns:?}");

        // Pinned pawns may push along their pin ray
        let pinned_pushes = pinned_pawns.advance_by(color, 1) & self.pinmask();
        // Unpinned pawns may push normally
        let unpinned_pushes = unpinned_pawns.advance_by(color, 1);
        let pushes = pinned_pushes | unpinned_pushes;
        // eprintln!("PUSHES:\n{pushes:?}");

        // Cannot push outside of checkmask or into an occupied spot
        let legal_push_mask = !self.occupied() & checkmask;
        let single_pushes = pushes & legal_push_mask;
        // If it can push once, check if it's on the third rank. If so, it can push again.
        let third_rank = Bitboard::third_rank(color);
        let double_pushes = (single_pushes & third_rank).advance_by(color, 1) & legal_push_mask;

        // eprintln!("DOUBLE PUSHES:\n{double_pushes:?}");

        // Cannot capture outside of checkmask or into an empty or friendly spot
        let legal_enemies = self.color(color.opponent()) & checkmask;
        let east_captures = self.pawns(color).advance_by(color, 1).east() & legal_enemies;
        let west_captures = self.pawns(color).advance_by(color, 1).west() & legal_enemies;

        // Now generate the moves for these
        for to in single_pushes {
            let from = to.backward_by(color, 1).unwrap();
            if to.rank() == Rank::eighth(color) {
                moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Knight)));
                moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Bishop)));
                moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Rook)));
                moves.push(Move::new(from, to, MoveKind::Promote(PieceKind::Queen)));
            } else {
                moves.push(Move::new(from, to, MoveKind::Quiet));
            }
        }

        for to in double_pushes {
            let from = to.backward_by(color, 2).unwrap();
            moves.push(Move::new(from, to, MoveKind::Quiet));
        }

        for to in west_captures {
            let from = to.backward_by(color, 1).unwrap().left_by(color, 1).unwrap();

            if to.rank() == Rank::eighth(color) {
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Knight)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Bishop)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Rook)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Queen)));
            } else {
                moves.push(Move::new(from, to, MoveKind::Quiet));
            }
        }

        for to in east_captures {
            let from = to
                .backward_by(color, 1)
                .unwrap()
                .right_by(color, 1)
                .unwrap();
            if to.rank() == Rank::eighth(color) {
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Knight)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Bishop)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Rook)));
                moves.push(Move::new(from, to, MoveKind::PromoCapt(PieceKind::Queen)));
            } else {
                moves.push(Move::new(from, to, MoveKind::Quiet));
            }
        }
    }
    */

    /// Generates and serializes all legal Knight moves.
    fn generate_knight_moves<const IN_CHECK: bool>(&self, mask: Bitboard, moves: &mut MoveList) {
        let color = self.side_to_move();
        for from in self.knights(color) & mask {
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
        if (rook_attacks(self.king_square, blockers_after_ep) & enemy_ortho_sliders).is_nonempty() {
            return Bitboard::EMPTY_BOARD;
        }

        let enemy_diag_sliders = self.diagonal_sliders(color.opponent());
        if (bishop_attacks(self.king_square, blockers_after_ep) & enemy_diag_sliders).is_nonempty()
        {
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

            (short.unwrap_or_default() | long.unwrap_or_default())
            // Can only castle to friendly Rooks
                & self.piece_parts(color, PieceKind::Rook)
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
        rook_square: Square,
        rook_dst_square: Square,
        king_dst_square: Square,
        enemy_attacks: Bitboard,
    ) -> Bitboard {
        // The King and Rook don't count as blockers, since they're moving through each other
        let blockers = self.occupied() ^ self.king_square ^ rook_square;

        // All squares between the King and his destination must be empty
        let king_to_dst = ray_between(self.king_square, king_dst_square) | king_dst_square;
        // All squares between the Rook and its destination must be empty
        let rook_to_dst = ray_between(rook_square, rook_dst_square) | rook_dst_square;
        let squares_are_empty =
            (rook_to_dst & blockers).is_empty() && (king_to_dst & blockers).is_empty();

        // All squares between the King and his destination (inclusive) must not be attacked
        let squares_that_must_be_safe = ray_between(self.king_square, king_dst_square)
            | king_dst_square
            | (self.pinned() & rook_square); // If the Rook is pinned, we can't castle
        let squares_are_safe = (squares_that_must_be_safe & enemy_attacks).is_empty();

        Bitboard::from_square(rook_square)
            & Bitboard::from_bool(squares_are_empty && squares_are_safe)
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
        let mut legal_squares = self.checkmask();

        // Check if this piece is pinned along any of the pinmasks
        if self.pinned().intersects(square) {
            legal_squares &= ray_containing(square, self.king_square);
        }

        // Pseudo-legal attacks that are within the check/pin mask and attack non-friendly squares
        default_attacks & legal_squares
    }
}

impl Deref for Game {
    type Target = Position;
    /// A [`Game`] immutably dereferences to a [`Position`], for simplicity.
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.position
    }
}

impl FromStr for Game {
    type Err = anyhow::Error;
    /// Wrapper for [`Game::from_fen`]
    #[inline(always)]
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Self::from_fen(s)
    }
}

impl Default for Game {
    /// Standard starting position for Chess.
    #[inline(always)]
    fn default() -> Self {
        Self::new(Position::default())
    }
}

impl fmt::Display for Game {
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

            if rank == Rank::SEVEN {
                if f.alternate() {
                    write!(f, "        FEN: {:#}", self.position())?;
                } else {
                    write!(f, "        FEN: {}", self.position())?;
                }
            } else if rank == Rank::SIX {
                write!(f, "        Key: {}", self.key())?;
            } else if rank == Rank::FIVE {
                write!(f, "   Checkers: {}", squares_to_string(self.checkers()))?;
            } else if rank == Rank::FOUR {
                write!(f, "     Pinned: {}", squares_to_string(self.pinned()))?;
                // } else if rank == Rank::THREE {
                // } else if rank == Rank::TWO {
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

impl fmt::Debug for Game {
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
