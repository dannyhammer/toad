/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

// #[macro_export]
// macro_rules! tunable {
//     ($default:tt, $min:tt, $max:tt, $step: tt) => {
//         $default
//     };
// }

/// Divisor for computing the soft timeout of a search.
macro_rules! soft_timeout_divisor {
    () => {
        20
        // super::tunable!(20, 1, 100, 1)
    };
}
pub(crate) use soft_timeout_divisor;

/// Divisor for computing the hard timeout of a search.
macro_rules! hard_timeout_divisor {
    () => {
        5
    };
}
pub(crate) use hard_timeout_divisor;

/// Divisor for computing how much of the time increment to use.
macro_rules! time_inc_divisor {
    () => {
        2
    };
}
pub(crate) use time_inc_divisor;

/// Initial Aspiration Window size
macro_rules! initial_aspiration_window_size {
    () => {
        25
    };
}
pub(crate) use initial_aspiration_window_size;

/// Minimum depth to incorporate Aspiration Windows into the Iterative Deepening search.
macro_rules! min_aspiration_window_depth {
    () => {
        1
    };
}
pub(crate) use min_aspiration_window_depth;
