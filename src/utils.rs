use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Score(pub(crate) i32);

impl Score {
    /// Largest possible score ever achievable.
    pub const INF: Self = Self(i16::MAX as i32);

    /// Score of mate in 1 move.
    pub const MATE: Self = Self(Self::INF.0 - 1);

    /// Score of a draw.
    pub const DRAW: Self = Self(0);

    /*
    /// Lowest possible score for mate.
    ///
    /// This is only obtainable if mate is possible in [`MAX_DEPTH`] moves.
    pub const LOWEST_MATE: Self = Self(Self::MATE.0 - MAX_DEPTH as i32);

    /// Returns `true` if the score is a mate score.
    pub const fn is_mate(&self) -> bool {
        self.0.abs() >= Self::LOWEST_MATE.0
    }

    /*
    /// Returns the number of plies this score is from mate.
    pub const fn mate_dist(&self) -> i32 {
        Self::MATE.0 - self.0.abs()
    }
     */

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

    pub fn abs(&self) -> Self {
        Self(self.0.abs())
    }
     */
}

macro_rules! impl_binary_op {
    ($trait:tt, $fn:ident) => {
        impl std::ops::$trait for Score {
            type Output = Self;

            fn $fn(self, rhs: Self) -> Self::Output {
                Self(self.0.$fn(rhs.0))
            }
        }

        impl std::ops::$trait<i32> for Score {
            type Output = Self;

            fn $fn(self, rhs: i32) -> Self::Output {
                Self(self.0.$fn(rhs))
            }
        }
    };
}

macro_rules! impl_binary_op_assign {
    ($trait:tt, $fn:ident) => {
        impl std::ops::$trait for Score {
            fn $fn(&mut self, rhs: Self) {
                self.0.$fn(rhs.0);
            }
        }

        impl std::ops::$trait<i32> for Score {
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

    fn neg(self) -> Self::Output {
        Self(self.0.neg())
    }
}

impl fmt::Display for Score {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

/// Maximum depth that can be searched
pub const MAX_DEPTH: usize = 255;
