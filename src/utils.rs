/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::fmt;

use uci_parser::UciScore;

/// A numerical representation of the evaluation of a position / move.
///
/// This value is internally capped at [`Self::INF`].
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Score(pub(crate) i32);

impl Score {
    /// Largest possible score ever achievable.
    pub const INF: Self = Self(i16::MAX as i32);

    /// Score of mate in 1 ply.
    pub const MATE: Self = Self(Self::INF.0 - 1);

    /// Score of a draw.
    pub const DRAW: Self = Self(0);

    /// Lowest possible score for mate.
    ///
    /// This is only obtainable if mate is possible in [`MAX_DEPTH`] moves.
    pub const LOWEST_MATE: Self = Self(Self::MATE.0 - MAX_DEPTH as i32);

    /// Returns `true` if the score is a mate score.
    #[inline(always)]
    pub fn is_mate(&self) -> bool {
        self.abs() >= Self::LOWEST_MATE
    }

    /// Converts this [`Score`] into a [`UciScore`],
    /// determining whether it is a centipawns score or a mate score.
    ///
    /// Used when sending the `info score` message.
    #[inline(always)]
    pub fn into_uci(self) -> UciScore {
        if self.is_mate() {
            // distance to mate (in plies)
            let plies = self.mate_dist();

            // If this score is in favor of the side-to-move, it will be positive
            // so we add 1 (because we need to make the current move in order for it's score to take effect).
            // Otherwise, the score is for our opponent, so we need to negate it.
            let moves = if self > Self::DRAW { plies + 1 } else { -plies } / 2;

            UciScore::mate(moves)
        } else {
            UciScore::cp(self.0)
        }
    }

    /// Returns the number of plies this score is from mate.
    pub const fn mate_dist(&self) -> i32 {
        Self::MATE.0 - self.0.abs()
    }

    /*
    /// Normalize the score to the provided ply.
    ///
    /// Score will be relative to `ply`.
    pub const fn relative(self, ply: i32) -> Self {
        if self.is_mate() {
            Self(self.0 + ply)
        } else {
            self
        }
    }

    /// De-normalize the score from the provided ply.
    ///
    /// Score will be relative to root (0 ply).
    pub const fn absolute(self, ply: i32) -> Self {
        if self.is_mate() {
            Self(self.0 - ply)
        } else {
            self
        }
    }
     */

    /// Returns the absolute value of this [`Score`].`
    #[inline(always)]
    pub const fn abs(self) -> Self {
        Self(self.0.abs())
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

        impl std::ops::$trait<i32> for Score {
            type Output = Self;

            #[inline(always)]
            fn $fn(self, rhs: i32) -> Self::Output {
                Self(self.0.$fn(rhs))
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

        impl std::ops::$trait<i32> for Score {
            #[inline(always)]
            fn $fn(&mut self, rhs: i32) {
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

impl fmt::Display for Score {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Maximum depth that can be searched
pub const MAX_DEPTH: usize = 255;
