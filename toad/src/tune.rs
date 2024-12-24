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
        3
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
        300
    };
}
pub(crate) use min_nmp_depth;

/// Value to subtract from `depth` when applying null move pruning.
macro_rules! nmp_reduction {
    () => {
        300
    };
}
pub(crate) use nmp_reduction;

/// Maximum depth at which to apply reverse futility pruning.
macro_rules! max_rfp_depth {
    () => {
        500
    };
}
pub(crate) use max_rfp_depth;

/// Safety margin when applying reverse futility pruning.
macro_rules! rfp_margin {
    () => {
        75
    };
}
pub(crate) use rfp_margin;

/// Minimum depth at which to apply late move pruning.
macro_rules! min_lmp_depth {
    () => {
        300
    };
}
pub(crate) use min_lmp_depth;

/// Minimum depth at which to apply late move reductions.
macro_rules! min_lmr_depth {
    () => {
        300
    };
}
pub(crate) use min_lmr_depth;

/// Minimum moves that must be made before late move reductions can be applied.
macro_rules! min_lmr_moves {
    () => {
        5
    };
}
pub(crate) use min_lmr_moves;

/// Base value in the LMR formula.
macro_rules! lmr_offset {
    () => {
        0.8
    };
}
pub(crate) use lmr_offset;

/// Divisor in the LMR formula.
macro_rules! lmr_divisor {
    () => {
        2.35
    };
}
pub(crate) use lmr_divisor;

/// Value to multiply depth by when computing razoring margin.
macro_rules! razoring_multiplier {
    () => {
        128
    };
}
pub(crate) use razoring_multiplier;

/// Value to subtract from alpha bound when computing a razoring margin.
macro_rules! razoring_offset {
    () => {
        256
    };
}
pub(crate) use razoring_offset;

/// Depth to extend by for check extensions.
macro_rules! check_extensions_depth {
    () => {
        100
    };
}
pub(crate) use check_extensions_depth;

/// Minimum depth at which razoring can be applied.
macro_rules! min_razoring_depth {
    () => {
        200
    };
}
pub(crate) use min_razoring_depth;

/// Value to multiply depth by when performing futility pruning.
macro_rules! fp_multiplier {
    () => {
        75
    };
}
pub(crate) use fp_multiplier;

/// Value to offset depth by when performing futility pruning.
macro_rules! fp_offset {
    () => {
        75
    };
}
pub(crate) use fp_offset;

/// Minimum depth at which futility pruning can be applied.
macro_rules! min_fp_depth {
    () => {
        300
    };
}

pub(crate) use min_fp_depth;
/// Multiplier for the LMP formula.
macro_rules! lmp_multiplier {
    () => {
        1
    };
}
pub(crate) use lmp_multiplier;

/// Divisor for the LMP formula.
macro_rules! lmp_divisor {
    () => {
        3
    };
}
pub(crate) use lmp_divisor;

/// Minimum depth at which IIR can be performed
macro_rules! min_iir_depth {
    () => {
        500
    };
}
pub(crate) use min_iir_depth;

/// Minimum depth at which IID can be performed
macro_rules! min_iid_depth {
    () => {
        400
    };
}
pub(crate) use min_iid_depth;

/// Offset to subtract from depth during IID.
macro_rules! iid_offset {
    () => {
        200
    };
}
pub(crate) use iid_offset;
