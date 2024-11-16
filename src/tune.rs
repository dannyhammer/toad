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
macro_rules! initial_aspiration_window_delta {
    () => {
        25
    };
}
pub(crate) use initial_aspiration_window_delta;

/// Initial Aspiration Window size
macro_rules! min_aspiration_window_delta {
    () => {
        10
    };
}
pub(crate) use min_aspiration_window_delta;

/// Minimum depth to incorporate Aspiration Windows into the Iterative Deepening search.
macro_rules! min_aspiration_window_depth {
    () => {
        1
    };
}
pub(crate) use min_aspiration_window_depth;

/// Maximum bonus to apply to a move via History Heuristic.
macro_rules! max_history_bonus {
    () => {
        16_384
    };
}
pub(crate) use max_history_bonus;

/// Base value of a move when ordering moves.
///
/// Negative offset is to prevent history moves from interfering with moves of higher priority,
/// such as captures and hash moves.
macro_rules! base_move_score {
    () => {
        0
    };
}
pub(crate) use base_move_score;

/// Value to multiply depth by when computing history scores.
macro_rules! history_multiplier {
    () => {
        300
    };
}
pub(crate) use history_multiplier;

/// Value to subtract from a history score at a given depth.
macro_rules! history_offset {
    () => {
        250
    };
}
pub(crate) use history_offset;

/// Minimum depth at which null move pruning can be applied.
macro_rules! min_nmp_depth {
    () => {
        3
    };
}
pub(crate) use min_nmp_depth;

/// Value to subtract from `depth` when applying null move pruning.
macro_rules! nmp_reduction_value {
    () => {
        3
    };
}
pub(crate) use nmp_reduction_value;
