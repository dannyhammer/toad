/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

/// Four random u64 values.
const SEEDS: [u64; 4] = [
    0b1001000111000101101010110011110011101011111111010101101001110001,
    0b0000011010111010001001010011101110011101110110001001011111001101,
    0b1000000000010101101101011110010110011100110000100111010111101001,
    0b1111100011110100001001111111110001010100000100011101111001010011,
];

/// A pseudo-random number generator using the "xoshiro" algorithm.
///
/// Source code copied from <https://prng.di.unimi.it/xoshiro256starstar.c>
pub struct XoShiRo([u64; 4]);

impl XoShiRo {
    /// Construct a new pseudo-random number generator from the library's seeds.
    #[inline(always)]
    pub const fn new() -> Self {
        Self::from_seeds(SEEDS)
    }

    /// Construct a new pseudo-random number generator from your own seeds.
    #[inline(always)]
    pub const fn from_seeds(seeds: [u64; 4]) -> Self {
        Self(seeds)
    }

    /// `const` analog of [`XoShiRo::get_next`], returning `(next, Self)`.
    #[inline(always)]
    pub const fn get_next_const(self) -> (u64, Self) {
        let (result, s) = Self::xoshiro(self.0);
        (result, Self(s))
    }

    /// Inner function for computing the next pseudo-random number in the sequence.
    const fn xoshiro(mut s: [u64; 4]) -> (u64, [u64; 4]) {
        let result = s[1].wrapping_mul(5).rotate_left(7).wrapping_mul(9);

        let t = s[1] << 17;

        s[2] ^= s[0];
        s[3] ^= s[1];
        s[1] ^= s[2];
        s[0] ^= s[3];

        s[2] ^= t;

        s[3] = s[3].rotate_left(45);
        (result, s)
    }
}

impl Default for XoShiRo {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}
