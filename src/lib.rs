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

/// Types and utilities for rating how good/bad a position is.
mod score;

/// Commands to be sent to the engine, and how to parse them.
mod cli;

pub use cli::*;
pub use engine::*;
use eval::*;
use score::*;
use search::*;
use utils::*;
