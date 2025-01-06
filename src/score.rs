/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::fmt;

use uci_parser::UciScore;

use crate::{tune, Color, Ply};

/// Internal representation for a [`Score`].
type ScoreInner = i16;

/// Internal type that is larger than [`ScoreInner`].
///
/// This is used during linear interpolation between scores.
type ScoreInnerLarger = i32;

/// A numerical representation of the evaluation of a position / move, in units of ["centipawns"](https://www.chessprogramming.org/Score).
///
/// This value is internally capped at [`Self::INF`].
#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Score(ScoreInner);

impl Score {
    /// Largest possible score ever achievable.
    pub const INF: Self = Self(ScoreInner::MAX);

    /// Score of mate in the current position.
    pub const MATE: Self = Self(Self::INF.0 - 1);

    /// Score of a draw.
    pub const DRAW: Self = Self(0);

    /// Lowest possible score for mate.
    ///
    /// This is only obtainable if mate is possible in [`Ply::MAX`] moves.
    pub const LOWEST_MATE: Self = Self(Self::MATE.0 - Ply::MAX.plies() as ScoreInner);

    /// Maximum bonus to apply to moves via history heuristic.
    pub const MAX_HISTORY: Self = Self(tune::max_history_bonus!());

    /// The base value of a move, used when ordering moves during search.
    pub const BASE_MOVE_SCORE: Self = Self(tune::base_move_score!());

    /// Value to multiply depth by when computing razoring margin.
    pub const RAZORING_MULTIPLIER: Self = Self(tune::razoring_multiplier!());

    /// Value to subtract from alpha bound when computing a razoring margin.
    pub const RAZORING_OFFSET: Self = Self(tune::razoring_offset!());

    /// Constructs a new [`Score`] instance.
    #[inline(always)]
    pub const fn new(score: ScoreInner) -> Self {
        Self(score)
    }

    /// Constructs a new [`Score`] instance that represents *being* checkmated in `n` plies.
    #[inline(always)]
    pub const fn mated_in(n: Ply) -> Self {
        Self(-Self::MATE.0 + n.plies() as ScoreInner)
    }

    /// Constructs a new [`Score`] instance that represents *giving* checkmate in `n` plies.
    #[inline(always)]
    pub const fn mate_in(n: Ply) -> Self {
        Self(Self::MATE.0 - n.plies() as ScoreInner)
    }

    /// Constructs a new [`Score`] instance from an integer.
    #[inline(always)]
    pub const fn from_int(score: i32) -> Self {
        Self(score as ScoreInner)
    }

    /// Returns the inner value of this [`Score`].
    #[inline(always)]
    pub const fn inner(self) -> ScoreInner {
        self.0
    }

    /// Returns `true` if the score is a mate score.
    #[inline(always)]
    pub const fn is_mate(&self) -> bool {
        self.abs().0 >= Self::LOWEST_MATE.0
    }

    /// Returns `true` if the score represents *being* checkmated.
    #[inline(always)]
    pub const fn mated(&self) -> bool {
        self.0 <= -Self::LOWEST_MATE.0
    }

    /// Returns `true` if the score represents *giving* checkmated.
    #[inline(always)]
    pub const fn mating(&self) -> bool {
        self.0 >= Self::LOWEST_MATE.0
    }

    /// Converts this [`Score`] into a [`UciScore`],
    /// determining whether it is a centipawns score or a mate score.
    ///
    /// Used when sending the `info score` message.
    #[inline(always)]
    pub fn into_uci(self) -> UciScore {
        if self.is_mate() {
            UciScore::mate(self.moves_to_mate())
        } else {
            UciScore::cp(self.0 as i32)
        }
    }

    /// Returns the number of plies (half moves) this score is from mate.
    #[inline(always)]
    pub const fn plies_to_mate(&self) -> i32 {
        (Self::MATE.0 - self.0.abs()) as i32
    }

    /// Returns the number of moves (full moves) this score is from mate.
    #[inline(always)]
    pub const fn moves_to_mate(&self) -> i32 {
        let plies = self.plies_to_mate();

        // If this score is in favor of the side-to-move, it will be positive
        // so we add 1 (because we need to make the current move in order for it's score to take effect).
        // Otherwise, the score is for our opponent, so we need to negate it.
        let relative_to_side = if self.0 > 0 { plies + 1 } else { -plies };

        // Divide by 2 to obtain the number of moves (1 move = 2 ply)
        relative_to_side / 2
    }

    /// Returns the absolute value of this [`Score`].
    #[inline(always)]
    pub const fn abs(self) -> Self {
        Self(self.0.abs())
    }

    /// If this [`Score`] is a mate score, adjust it by 1 to propagate up the search tree.
    ///
    /// This helps with mate score adjustments, ensuring that a mate score is always relative to the current ply.
    #[inline(always)]
    pub const fn adjust_mate(&mut self) {
        self.0 -= self.0.signum() * self.is_mate() as ScoreInner
    }

    /// "Normalizes" a score so that it can be printed as a float.
    ///
    /// Presently, this just divides by 100, since a score represents a centipawn value.
    #[inline(always)]
    pub fn normalize(&self) -> f32 {
        // let max = Score::MATE.0 as f32;
        self.0 as f32 / 100.0
    }

    /// Performs linear interpolation between `self` and `other` by `t` where `t` is `[0, 100]`.
    #[inline(always)]
    pub const fn lerp(self, other: Self, t: u8) -> Self {
        // Cast these to larger types to prevent overflow during the arithmetic.
        // TODO: Is there a nicer way of doing this? Perhaps with some of the std saturating/overflowing functions?
        let overflow = self.0 as ScoreInnerLarger
            + (other.0 as ScoreInnerLarger - self.0 as ScoreInnerLarger) * t as ScoreInnerLarger
                / 100;

        Self(overflow as ScoreInner)
    }

    /// Wrapper for `saturating_add` on a [`Score`]'s internal representation.
    #[inline(always)]
    pub const fn saturating_add(&self, other: Self) -> Self {
        Self(self.0.saturating_add(other.0))
    }

    /// Wrapper for `saturating_sub` on a [`Score`]'s internal representation.
    #[inline(always)]
    pub const fn saturating_sub(&self, other: Self) -> Self {
        Self(self.0.saturating_sub(other.0))
    }

    /// Wrapper for `saturating_mul` on a [`Score`]'s internal representation.
    #[inline(always)]
    pub const fn saturating_mul(&self, other: Self) -> Self {
        Self(self.0.saturating_mul(other.0))
    }
}

impl From<Score> for UciScore {
    #[inline(always)]
    fn from(value: Score) -> Self {
        value.into_uci()
    }
}

impl From<i32> for Score {
    #[inline(always)]
    fn from(value: i32) -> Self {
        Self(value as ScoreInner)
    }
}

macro_rules! impl_binary_op {
    ($trait:tt, $fn:ident) => {
        impl std::ops::$trait for Score {
            type Output = Self;

            #[inline(always)]
            fn $fn(self, rhs: Self) -> Self::Output {
                Self(self.0.$fn(rhs.0))
            }
        }

        impl std::ops::$trait<ScoreInner> for Score {
            type Output = Self;

            #[inline(always)]
            fn $fn(self, rhs: ScoreInner) -> Self::Output {
                Self(self.0.$fn(rhs))
            }
        }

        impl std::ops::$trait<Score> for ScoreInner {
            type Output = Score;

            #[inline(always)]
            fn $fn(self, rhs: Score) -> Self::Output {
                Score(self.$fn(rhs.0))
            }
        }

        impl std::ops::$trait<Ply> for Score {
            type Output = Self;

            #[inline(always)]
            fn $fn(self, rhs: Ply) -> Self::Output {
                Self(self.0.$fn(rhs.plies() as ScoreInner))
            }
        }

        impl std::ops::$trait<Score> for Ply {
            type Output = Score;

            #[inline(always)]
            fn $fn(self, rhs: Score) -> Self::Output {
                Score((self.plies() as ScoreInner).$fn(rhs.0))
            }
        }
    };
}

macro_rules! impl_binary_op_assign {
    ($trait:tt, $fn:ident) => {
        impl std::ops::$trait for Score {
            #[inline(always)]
            fn $fn(&mut self, rhs: Self) {
                self.0.$fn(rhs.0);
            }
        }

        impl std::ops::$trait<ScoreInner> for Score {
            #[inline(always)]
            fn $fn(&mut self, rhs: ScoreInner) {
                self.0.$fn(rhs);
            }
        }
    };
}

impl_binary_op!(Add, add);
impl_binary_op!(Sub, sub);
impl_binary_op!(Mul, mul);
impl_binary_op!(Div, div);

impl_binary_op_assign!(AddAssign, add_assign);
impl_binary_op_assign!(SubAssign, sub_assign);

impl std::ops::Neg for Score {
    type Output = Self;

    #[inline(always)]
    fn neg(self) -> Self::Output {
        Self(self.0.neg())
    }
}

impl std::ops::Mul<bool> for Score {
    type Output = Self;

    #[inline(always)]
    fn mul(self, rhs: bool) -> Self::Output {
        Self(self.0 * rhs as ScoreInner)
    }
}

impl std::ops::Mul<Color> for Score {
    type Output = Self;

    #[inline(always)]
    fn mul(self, rhs: Color) -> Self::Output {
        Self(self.0 * rhs.negation_multiplier() as ScoreInner)
    }
}

impl PartialEq<ScoreInner> for Score {
    #[inline(always)]
    fn eq(&self, other: &ScoreInner) -> bool {
        self.0.eq(other)
    }
}

impl PartialOrd<ScoreInner> for Score {
    #[inline(always)]
    fn partial_cmp(&self, other: &ScoreInner) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(other)
    }
}

impl fmt::Display for Score {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl fmt::Debug for Score {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.is_mate() {
            write!(
                f,
                "{} (mate in {} plies {} moves)",
                self.0,
                self.plies_to_mate(),
                self.moves_to_mate()
            )
        } else {
            write!(f, "{self} ({})", self.0)
        }
    }
}

/*
// TODO: https://discord.com/channels/719576389245993010/719576389690589244/1304177247124455535
#[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
struct MoveScore(u32);

impl MoveScore {
    pub fn hash(score: i32) -> Self {
        debug_assert!(score.unsigned_abs() < (1 << 29));
        Self((score + (1 << 29) + (1 << 31)) as u32)
    }

    pub fn capture(score: i32) -> Self {
        debug_assert!(score.unsigned_abs() < (1 << 29));
        Self((score + (1 << 29) + (1 << 30)) as u32)
    }

    pub fn history(score: i32) -> Self {
        debug_assert!(score.unsigned_abs() < (1 << 29));
        Self((score + (1 << 29)) as u32)
    }
}
 */

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_plies_to_mate() {
        let plies = Ply::new(3);

        // Plies to mate
        let our_mate = Score::MATE - plies;
        assert_eq!(our_mate.plies_to_mate(), plies.plies());

        let their_mate = -(Score::MATE - plies);
        assert_eq!(their_mate.plies_to_mate(), plies.plies());
    }

    #[test]
    fn test_lerp() {
        assert_eq!(Score::new(0).lerp(Score::new(0), 0), Score::new(0));
        assert_eq!(Score::new(0).lerp(Score::new(100), 0), Score::new(0));
        assert_eq!(Score::new(0).lerp(Score::new(100), 50), Score::new(50));
        assert_eq!(Score::new(0).lerp(Score::new(100), 65), Score::new(65));
        assert_eq!(Score::new(10).lerp(Score::new(20), 50), Score::new(15));

        assert_eq!(
            Score::new(0).lerp(Score::new(ScoreInner::MAX), 0),
            Score::new(0)
        );

        assert_eq!(
            Score::new(0).lerp(Score::new(ScoreInner::MAX), 100),
            Score::new(ScoreInner::MAX)
        );

        assert_eq!(
            Score::new(ScoreInner::MAX).lerp(Score::new(ScoreInner::MAX), 100),
            Score::new(ScoreInner::MAX)
        );

        assert_eq!(
            Score::INF.lerp(Score::new(ScoreInner::MAX), 100),
            Score::INF
        );
    }
}
