/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{
    fmt,
    ops::{Index, IndexMut},
};

use crate::{File, Rank, Square};

/// A generic container of 64 elements.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Table<T>([T; Square::COUNT]);

impl<T> Table<T> {
    /// Create a new [`Table`] by applying `f` to each [`Square`].
    #[inline(always)]
    pub fn from_fn<F>(mut f: F) -> Self
    where
        F: FnMut(Square) -> T,
    {
        Self(std::array::from_fn(|i| f(Square(i as u8))))
    }

    /// Create a new [`Table`] with the provided values.
    #[inline(always)]
    pub const fn new(values: [T; Square::COUNT]) -> Self {
        Self(values)
    }

    /// Create a new [`Table`] with every value set to `value`.
    #[inline(always)]
    pub const fn splat(value: T) -> Self
    where
        T: Copy,
    {
        Self([value; Square::COUNT])
    }

    /// Get the value of this [`Table`] at the index of `square`.
    #[inline(always)]
    pub const fn get(&self, square: Square) -> &T {
        &self.0[square.index()]
    }

    /// Set the value of this [`Table`] at the index of `square`.
    #[inline(always)]
    pub fn set(&mut self, square: Square, value: T) {
        self.0[square] = value;
    }
}

impl<T> Default for Table<T>
where
    T: Default + Copy,
{
    #[inline(always)]
    fn default() -> Self {
        Self::splat(T::default())
    }
}

impl<T, Idx> Index<Idx> for Table<T>
where
    [T; Square::COUNT]: Index<Idx, Output = T>,
{
    type Output = T;
    #[inline(always)]
    fn index(&self, index: Idx) -> &Self::Output {
        &self.0[index]
    }
}

impl<T, Idx> IndexMut<Idx> for Table<T>
where
    [T; Square::COUNT]: IndexMut<Idx, Output = T>,
{
    #[inline(always)]
    fn index_mut(&mut self, index: Idx) -> &mut Self::Output {
        &mut self.0[index]
    }
}
/// Used to display an arbitrary-sized [`Table`].
pub type DisplayTable<T, const N: usize> = Table<[T; N]>;

/// A [`Table`] that will be printed with each square is printed inside of 1 line of chars.
pub type SmallDisplayTable<T> = DisplayTable<T, 1>;

/// A [`Table`] that will be printed with each square is printed inside of 2 lines of chars.
pub type MediumDisplayTable<T> = DisplayTable<T, 2>;

impl<T, const N: usize> fmt::Display for DisplayTable<T, N>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use fmt::Alignment::*;
        let precision = f.precision().unwrap_or(2 * N + 1);
        let width = f.width().unwrap_or(2 * N + 1);
        let range = 0..N;
        let alignment = f.align().unwrap_or(Center);

        // Special case: Small tables can be printed without borders
        if N == 1 && !f.alternate() {
            // Small tables need a smaller width, since we don't have file dividers.
            // Also, if an entry takes up more than 1 char of space, we need a wider table.
            let width = f.width().unwrap_or(self.0[0][0].to_string().len());

            for rank in Rank::iter().rev() {
                write!(f, "{rank}| ")?;

                for file in File::iter() {
                    let square = Square::new(file, rank);
                    let value = &self.0[square][0];
                    match alignment {
                        Center => write!(f, "{value:^width$.*} ", precision)?,
                        Left => write!(f, "{value:<width$.*} ", precision)?,
                        Right => write!(f, "{value:>width$.*} ", precision)?,
                    }
                }
                writeln!(f)?;
            }
            write!(f, " +")?;
            for _ in File::iter() {
                write!(f, "--{}", "-".repeat(width - 1))?;
            }
            write!(f, "\n   ")?;
            for file in File::iter() {
                write!(f, "{file} {}", " ".repeat(width - 1))?;
            }
        } else {
            // Write the top edge of the board
            write!(f, "  +")?;
            for _ in File::iter() {
                write!(f, "-{}+", "-".repeat(width - 1))?;
            }
            writeln!(f)?;

            // Now write each row
            for rank in Rank::iter().rev() {
                write!(f, "{rank} |")?;

                // Write each value in each row
                for i in range.clone() {
                    for file in File::iter() {
                        let square = Square::new(file, rank);
                        let value = &self.0[square][i];
                        match alignment {
                            Center => write!(f, "{value:^width$.*}|", precision)?,
                            Left => write!(f, "{value:<width$.*}|", precision)?,
                            Right => write!(f, "{value:>width$.*}|", precision)?,
                        }
                    }

                    if i + 1 < range.end {
                        write!(f, "\n  |")?; // End of the first row & start of the second
                    }
                }

                write!(f, "\n  +")?; // Print separation between rows
                for _ in File::iter() {
                    write!(f, "-{}+", "-".repeat(width - 1))?;
                }
                writeln!(f)?;
            }

            for file in File::iter() {
                write!(f, " {}{file}", " ".repeat(width - 1))?;
            }
        }

        Ok(())
    }
}
