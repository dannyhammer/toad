/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

/// Code related to the engine's functionality, such as user input handling.
mod engine;

/// Evaluation of chess positions.
mod eval;

/// Main engine logic; all search related code.
mod search;

/// Misc utility functions, constants, and types.
mod utils;

pub use engine::*;
use eval::*;
use search::*;
use utils::*;
