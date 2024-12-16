/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{fmt, ops::AddAssign};

/// A representation of the vertical distance between nodes in a search tree.
///
/// Internally this value is upper-bounded by [`Ply::MAX`] and uses
/// [fractional plies](https://www.chessprogramming.org/Ply#Fractional_Plies)
/// with a granularity defined internally.
#[derive(Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(transparent)]
pub struct Ply(i32);

impl Ply {
    /// Maximum depth reachable during a search.
    pub const MAX: Self = Self::new((u8::MAX / 2) as i32);

    /// A depth of zero plies.
    pub const ZERO: Self = Self::new(0);

    /// A depth of one ply.
    pub const ONE: Self = Self::new(1);

    /// Granularity of a single ply.
    ///
    /// The higher this number is, the finer the grain of fractional depth.
    const GRAIN: i32 = 100;

    /// Constructs a new [`Ply`] instance that is `n` plies deep.
    #[inline(always)]
    pub const fn new(n: i32) -> Self {
        Self(n * Self::GRAIN)
    }

    /// Returns the number of plies this depth represents, truncating any fractional depth.
    #[inline(always)]
    pub const fn plies(&self) -> i32 {
        self.0 / Self::GRAIN
    }

    /// Returns the number of plies this depth represents, rounding to the nearest whole depth.
    #[inline(always)]
    pub fn rounded(&self) -> i32 {
        (self.0 as f32 / Self::GRAIN as f32).round() as i32
    }
}

macro_rules! impl_binary_op {
    ($trait:tt, $fn:ident) => {
        impl std::ops::$trait for Ply {
            type Output = Self;

            #[inline(always)]
            fn $fn(self, rhs: Self) -> Self::Output {
                Self(self.0.$fn(rhs.0))
            }
        }
    };
}

impl_binary_op!(Add, add);
impl_binary_op!(Sub, sub);
impl_binary_op!(Mul, mul);
impl_binary_op!(Div, div);

impl std::ops::Add<i32> for Ply {
    type Output = Ply;
    #[inline(always)]
    fn add(self, rhs: i32) -> Self::Output {
        Self(self.0.add(Self::new(rhs).0))
    }
}

impl std::ops::Sub<i32> for Ply {
    type Output = Ply;
    #[inline(always)]
    fn sub(self, rhs: i32) -> Self::Output {
        Self(self.0.sub(Self::new(rhs).0))
    }
}

impl std::ops::Mul<i32> for Ply {
    type Output = Ply;
    #[inline(always)]
    fn mul(self, rhs: i32) -> Self::Output {
        Self(self.0.mul(rhs))
    }
}

impl std::ops::Div<i32> for Ply {
    type Output = Ply;
    #[inline(always)]
    fn div(self, rhs: i32) -> Self::Output {
        Self(self.0.div(rhs))
    }
}

impl AddAssign for Ply {
    #[inline(always)]
    fn add_assign(&mut self, rhs: Self) {
        self.0.add_assign(rhs.0);
    }
}

impl std::ops::AddAssign<i32> for Ply {
    #[inline(always)]
    fn add_assign(&mut self, rhs: i32) {
        self.0.add_assign(&Self::new(rhs).0);
    }
}

impl std::ops::SubAssign for Ply {
    #[inline(always)]
    fn sub_assign(&mut self, rhs: Self) {
        self.0.sub_assign(rhs.0);
    }
}

impl std::ops::SubAssign<i32> for Ply {
    #[inline(always)]
    fn sub_assign(&mut self, rhs: i32) {
        self.0.sub_assign(&Self::new(rhs).0);
    }
}

impl PartialEq<i32> for Ply {
    #[inline(always)]
    fn eq(&self, other: &i32) -> bool {
        self.0.eq(&Self::new(*other).0)
    }
}

impl PartialOrd<i32> for Ply {
    #[inline(always)]
    fn partial_cmp(&self, other: &i32) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&(other * Self::GRAIN))
    }
}

impl fmt::Display for Ply {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.plies().fmt(f)
    }
}

impl fmt::Debug for Ply {
    #[inline(always)]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self} ({})", self.0)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_math() {
        let mut depth = Ply::ZERO;
        assert_eq!(depth, Ply::new(0));

        depth += 1;
        assert_eq!(depth, Ply::new(1));
        assert_eq!(depth, Ply::ONE);

        depth = depth + 5;
        assert_eq!(depth, Ply::new(6));

        depth = depth * 2;
        assert_eq!(depth, Ply::new(12));

        depth = depth / 3;
        assert_eq!(depth, Ply::new(4));

        depth = depth - 3;
        assert_eq!(depth, Ply::new(1));
    }

    #[test]
    fn test_fractional() {
        // Ensure that fractional depths truncate properly after mathematical operations.
        let mut depth = Ply(50);
        assert_eq!(depth.plies(), 0);

        depth += 1;
        assert_eq!(depth.plies(), 1);

        // Ensure that fractional depths are truncated/rounded appropriately
        let half = Ply::GRAIN / 2;
        for i in 0..Ply::GRAIN {
            let depth = Ply(i);
            assert_eq!(depth.plies(), 0, "Depth {depth:?} did not truncate to 0");

            if i < half {
                assert_eq!(depth.rounded(), 0, "Depth {depth:?} did not round to 0");
            } else {
                assert_eq!(depth.rounded(), 1, "Depth {depth:?} did not round to 1");
            }
        }
    }

    #[test]
    fn test_negative() {
        let mut depth = Ply::ONE;
        depth -= 1;
        assert_eq!(depth, Ply(0));
        assert_eq!(depth.plies(), 0);

        depth -= 1;
        assert_eq!(depth, Ply::new(-1));
        assert_eq!(depth, Ply(-Ply::GRAIN));
        assert_eq!(depth.plies(), -1);
    }
}
