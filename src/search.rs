/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{
    fmt,
    marker::PhantomData,
    ops::Neg,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};

use arrayvec::ArrayVec;
use thiserror::Error;
use uci_parser::{UciInfo, UciResponse, UciSearchOptions};

use crate::{
    tune, Color, Game, HistoryTable, LogLevel, Move, MoveList, Piece, PieceKind, Ply, Position,
    ProbeResult, Score, TTable, TTableEntry, Variant, ZobristKey, MAX_NUM_MOVES,
};

/// Reasons that a search can be cancelled.
#[derive(Error, Debug, Clone, Copy, PartialEq, Eq)]
enum SearchCancelled {
    /// Search ran out of time.
    ///
    /// Contains the amount of time since the timeout was exceeded.
    #[error("Exceeded hard timeout of {0:?} by {1:?}")]
    HardTimeout(Duration, Duration),

    /// Met or exceeded the maximum number of nodes allowed.
    ///
    /// Contains the number of nodes past the allowance that were searched.
    #[error("Exceeded node allowance by {0}")]
    MaxNodes(u64),

    /// Stopped by an external factor
    #[error("Atomic flag was flipped")]
    Stopped,
}

/// A marker trait for the types of nodes encountered during search.
///
/// Credit to Cosmo, author of Viridithas,
/// for the idea of using a const generic trait for this.
trait NodeType {
    /// Is this node the first searched?
    const ROOT: bool;

    /// Is this node a PV node?
    const PV: bool;
}

/// First node searched.
struct RootNode;
impl NodeType for RootNode {
    const ROOT: bool = true;
    const PV: bool = true;
}

/// A node on the principal variation, searched with a non-null window.
struct PvNode;
impl NodeType for PvNode {
    const ROOT: bool = false;
    const PV: bool = true;
}

/// A node not on the principal variation, searched with a null window.
struct NonPvNode;
impl NodeType for NonPvNode {
    const ROOT: bool = false;
    const PV: bool = false;
}

/// Represents the best sequence of moves found during a search.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct PrincipalVariation(ArrayVec<Move, { Ply::MAX.plies() as usize }>);

impl PrincipalVariation {
    /// An empty PV.
    const EMPTY: Self = Self(ArrayVec::new_const());

    /// Clears the moves of `self`.
    #[inline(always)]
    fn clear(&mut self) {
        self.0.clear();
    }

    /// Extend the contents of `self` with `mv` and the contents of `other`.
    ///
    /// # Panics
    ///
    /// Will panic if `mv` and `other` are longer than `self`'s capacity.
    #[inline(always)]
    fn extend(&mut self, mv: Move, other: &Self) {
        self.clear();
        self.0.push(mv);
        self.0
            .try_extend_from_slice(&other.0)
            .unwrap_or_else(|err| {
                panic!(
                    "{err}: Attempted to exceed PV capacity of {} pushing {mv:?} and {:?}",
                    Ply::MAX,
                    &other.0
                );
            });
    }
}

impl Default for PrincipalVariation {
    #[inline(always)]
    fn default() -> Self {
        Self::EMPTY
    }
}

/// Bounds within an alpha-beta search.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SearchBounds {
    /// Lower bound.
    ///
    /// We are guaranteed a score that is AT LEAST `alpha`.
    /// During search, if no move can raise `alpha`, we are said to have "failed low."
    ///
    /// On a fail-low, we do not have a "best move."
    pub alpha: Score,

    /// Upper bound.
    ///
    /// Our opponent is guaranteed a score that is AT MOST `beta`.
    /// During search, if a move scores higher than `beta`, we are said to have "failed high."
    ///
    /// On a fail-high, the branch is pruned, since our opponent has a better move to play earlier in the tree,
    /// which would make this position unreachable for us.
    pub beta: Score,
}

impl SearchBounds {
    /// Create a new [`SearchBounds`] from the provided `alpha` and `beta` values.
    #[inline(always)]
    const fn new(alpha: Score, beta: Score) -> Self {
        Self { alpha, beta }
    }

    /// Create a "null window" around `alpha`.
    #[inline(always)]
    fn null_alpha(self) -> Self {
        Self::new(self.alpha, self.alpha + 1)
    }

    /// Create a "null window" around `beta`.
    #[inline(always)]
    fn null_beta(self) -> Self {
        Self::new(self.beta - 1, self.beta)
    }
}

impl Neg for SearchBounds {
    type Output = Self;
    /// Negating a [`SearchBounds`] swaps the `alpha` and `beta` fields and negates them both.
    #[inline(always)]
    fn neg(self) -> Self::Output {
        Self {
            alpha: -self.beta,
            beta: -self.alpha,
        }
    }
}

impl Default for SearchBounds {
    /// Default [`SearchBounds`] are a `(-infinity, infinity)`.
    #[inline(always)]
    fn default() -> Self {
        Self::new(-Score::INF, Score::INF)
    }
}

/// Represents a window around a search result to act as our a/b bounds.
#[derive(Debug)]
struct AspirationWindow {
    /// Bounds of this search window
    bounds: SearchBounds,

    /// Number of times that a score has been returned above beta.
    beta_fails: i32,

    /// Number of times that a score has been returned below alpha.
    alpha_fails: i32,
}

impl AspirationWindow {
    /// Returns a delta value to change window's size.
    ///
    /// The value will differ depending on `depth`, with higher depths producing narrower windows.
    #[inline(always)]
    fn delta(depth: Ply) -> Score {
        let initial_delta = tune::initial_aspiration_window_delta!();

        let min_delta = tune::min_aspiration_window_delta!();

        // Gradually decrease the window size from `8*init` to `min`
        Score::new(((initial_delta << 3) / depth.plies()).max(min_delta))
    }

    /// Creates a new [`AspirationWindow`] centered around `score`.
    #[inline(always)]
    fn new(score: Score, depth: Ply) -> Self {
        // If the score is mate, we expect search results to fluctuate, so set the windows to infinite.
        // Also, we only want to use aspiration windows after certain depths, so check that, too.
        let bounds = if depth < tune::min_aspiration_window_depth!() || score.is_mate() {
            SearchBounds::default()
        } else {
            // Otherwise we build a window around the provided score.
            let delta = Self::delta(depth);
            SearchBounds::new(
                (score - delta).max(-Score::INF),
                (score + delta).min(Score::INF),
            )
        };

        Self {
            bounds,
            alpha_fails: 0,
            beta_fails: 0,
        }
    }

    /// Widens the window's `alpha` bound, expanding it downwards.
    ///
    /// This also resets the `beta` bound to `(alpha + beta) / 2`
    #[inline(always)]
    fn widen_down(&mut self, score: Score, depth: Ply) {
        // Compute a gradually-increasing delta
        let delta = Self::delta(depth) * (1 << (self.alpha_fails + 1));

        // By convention, we widen both bounds on a fail low.
        self.bounds.beta = ((self.bounds.alpha + self.bounds.beta) / 2).min(Score::INF);
        self.bounds.alpha = (score - delta).max(-Score::INF);

        // Increase number of failures
        self.alpha_fails += 1;
    }

    /// Widens the window's `beta` bound, expanding it upwards.
    #[inline(always)]
    fn widen_up(&mut self, score: Score, depth: Ply) {
        // Compute a gradually-increasing delta
        let delta = Self::delta(depth) * (1 << (self.beta_fails + 1));

        // Widen the beta bound
        self.bounds.beta = (score + delta).min(Score::INF);

        // Increase number of failures
        self.beta_fails += 1;
    }

    /// Returns `true` if `score` fails low, meaning it is below `alpha` and the window must be expanded downwards.
    #[inline(always)]
    fn fails_low(&self, score: Score) -> bool {
        self.bounds.alpha != -Score::INF && score <= self.bounds.alpha
    }

    /// Returns `true` if `score` fails high, meaning it is above `beta` and the window must be expanded upwards.
    #[inline(always)]
    fn fails_high(&self, score: Score) -> bool {
        self.bounds.beta != Score::INF && score >= self.bounds.beta
    }
}

/// The result of a search, containing the best move found, score, and total nodes searched.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SearchResult {
    /// Number of nodes searched.
    pub nodes: u64,

    /// Evaluation of the position after `bestmove` is made.
    pub score: Score,

    /// The depth of the search that produced this result.
    pub depth: Ply,

    /// The maximum depth (ply) reached during this search.
    pub seldepth: Ply,

    /// Principal variation during this search.
    ///
    /// The first entry of this field represents the "best move" found during the search.
    pub pv: PrincipalVariation,
}

impl SearchResult {
    /// Fetch the first move in this PV, it one exists.
    #[inline(always)]
    fn bestmove(&self) -> Option<Move> {
        self.pv.0.first().copied()
    }
}

impl Default for SearchResult {
    /// A default search result should initialize to a *very bad* value,
    /// since there isn't a move to play.
    #[inline(always)]
    fn default() -> Self {
        Self {
            nodes: 0,
            score: -Score::INF,
            depth: Ply::ONE,
            seldepth: Ply::ZERO,
            pv: PrincipalVariation::EMPTY,
        }
    }
}

/// Configuration variables for executing a [`Search`].
#[derive(Debug, Clone, Copy)]
pub struct SearchConfig {
    /// Maximum depth to execute the search.
    pub max_depth: Ply,

    /// Node allowance.
    ///
    /// If the search exceeds this many nodes, it will exit as quickly as possible.
    pub max_nodes: u64,

    /// Start time of the search.
    pub starttime: Instant,

    /// Soft limit on search time.
    ///
    /// During iterative deepening, if a search concludes and this timeout is exceeded,
    /// the entire search will exit, since there probably isn't enough time remaining
    /// to conduct a search at a deeper depth.
    pub soft_timeout: Duration,

    /// Hard limit on search time.
    ///
    /// During *any* point in the search, if this limit is exceeded, the search will cancel.
    pub hard_timeout: Duration,
}

impl SearchConfig {
    /// Constructs a new [`SearchConfig`] from the provided UCI options and game.
    ///
    /// The [`Game`] is used to determine side to move, and other factors when computing the soft/hard timeouts.
    pub fn new<V: Variant>(options: UciSearchOptions, game: &Game<V>) -> Self {
        let mut config = Self::default();

        // If supplied, set the max depth / node allowance
        if let Some(depth) = options.depth {
            config.max_depth = Ply::new(depth as i32);
        }

        if let Some(nodes) = options.nodes {
            config.max_nodes = nodes as u64;
        }

        // If `movetime` was supplied, search that long.
        if let Some(movetime) = options.movetime {
            config.hard_timeout = movetime;
            config.soft_timeout = movetime;
        } else {
            // Otherwise, search based on time remaining and increment
            let (remaining, inc) = if game.side_to_move().is_white() {
                (options.wtime, options.winc)
            } else {
                (options.btime, options.binc)
            };

            // Only calculate timeouts if a time was provided
            if let Some(remaining) = remaining {
                let inc = inc.unwrap_or(Duration::ZERO) / tune::time_inc_divisor!();

                // Don't exceed time limit with increment.
                config.soft_timeout =
                    remaining.min(remaining / tune::soft_timeout_divisor!() + inc);
                config.hard_timeout = remaining / tune::hard_timeout_divisor!();
            }
        }

        config
    }
}

impl Default for SearchConfig {
    /// A default [`SearchConfig`] will permit an "infinite" search.
    ///
    /// The word "infinite" is quoted here because the actual defaults are the `::MAX` values for each field.
    #[inline(always)]
    fn default() -> Self {
        Self {
            max_depth: Ply::MAX,
            max_nodes: u64::MAX,
            starttime: Instant::now(),
            soft_timeout: Duration::MAX,
            hard_timeout: Duration::MAX,
        }
    }
}

/// Parameters for the various features used to enhance the efficiency of a search.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct SearchParameters {
    /// Minium depth at which null move pruning can be applied.
    min_nmp_depth: Ply,

    /// Value to subtract from `depth` when applying null move pruning.
    nmp_reduction: Ply,

    /// Maximum depth at which to apply reverse futility pruning.
    max_rfp_depth: Ply,

    /// Maximum depth at which to apply late move pruning.
    max_lmp_depth: Ply,

    /// Minimum depth at which to apply late move reductions.
    min_lmr_depth: Ply,

    /// Minimum moves that must be made before late move reductions can be applied.
    min_lmr_moves: usize,

    /// Base value in the LMR formula.
    lmr_offset: f32,

    /// Divisor in the LMR formula.
    lmr_divisor: f32,

    /// Value to multiply depth by when computing history scores.
    history_multiplier: Score,

    /// Value to subtract from a history score at a given depth.
    history_offset: Score,

    /// Safety margin when applying reverse futility pruning.
    rfp_margin: Score,

    /// Depth to extend by if the position is in check.
    check_extensions_depth: Ply,

    /// Maximum depth at which razoring can be performed.
    max_razoring_depth: Ply,

    /// Multiplier for the LMP formula.
    lmp_multiplier: usize,

    /// Divisor for the LMP formula.
    lmp_divisor: usize,

    /// Minimum depth at which IIR can be applied.
    min_iir_depth: Ply,

    /// Minimum depth at which IID can be applied.
    min_iid_depth: Ply,

    /// Offset to subtract from depth during IID.
    iid_offset: Ply,

    /// Pre-computed table for Late Move Reduction values.
    lmr_table: [[i32; MAX_NUM_MOVES]; Ply::MAX.plies() as usize + 1],
}

impl Default for SearchParameters {
    fn default() -> Self {
        let lmr_offset = tune::lmr_offset!();
        let lmr_divisor = tune::lmr_divisor!();

        // Initialize the table for Late Move Reductions, so that we don't need redo the floating-point arithmetic constantly.
        let mut lmr_table = [[0; MAX_NUM_MOVES]; Ply::MAX.plies() as usize + 1];
        for (depth, entry) in lmr_table.iter_mut().enumerate().skip(1) {
            for (moves_made, reduction) in entry.iter_mut().enumerate() {
                let d = (depth as f32).ln();
                let m = (moves_made as f32).ln();
                let r = lmr_offset + d * m / lmr_divisor;
                *reduction = r as i32;

                // eprintln!(
                //     "D: {depth:width$}, M: {moves_made:width$} := {r}",
                //     width = 3
                // );
                // assert!(!d.is_nan() && d.is_finite(), "{depth} produced {d}");
                // assert!(!m.is_nan() && m.is_finite(), "{moves_made} produced {m}");
                // assert!(
                //     !r.is_nan() && m.is_finite(),
                //     "{depth} x {moves_made} produced {r}"
                // );
            }
        }

        Self {
            min_nmp_depth: Ply::from_raw(tune::min_nmp_depth!()),
            nmp_reduction: Ply::from_raw(tune::nmp_reduction!()),
            max_rfp_depth: Ply::from_raw(tune::max_rfp_depth!()),
            max_lmp_depth: Ply::from_raw(tune::max_lmp_depth!()),
            min_lmr_depth: Ply::from_raw(tune::min_lmr_depth!()),
            min_lmr_moves: tune::min_lmr_moves!(),
            lmr_offset,
            lmr_divisor,
            history_multiplier: Score::new(tune::history_multiplier!()),
            history_offset: Score::new(tune::history_offset!()),
            rfp_margin: Score::new(tune::rfp_margin!()),
            check_extensions_depth: Ply::from_raw(tune::check_extensions_depth!()),
            max_razoring_depth: Ply::from_raw(tune::max_razoring_depth!()),
            lmp_multiplier: tune::lmp_multiplier!(),
            lmp_divisor: tune::lmp_divisor!(),
            min_iir_depth: Ply::from_raw(tune::min_iir_depth!()),
            min_iid_depth: Ply::from_raw(tune::min_iid_depth!()),
            iid_offset: Ply::from_raw(tune::iid_offset!()),
            lmr_table,
        }
    }
}

/// Executes a search on a game of chess.
pub struct Search<'a, Log, V> {
    /// An atomic flag to determine if the search should be cancelled at any time.
    ///
    /// If this is ever `false`, the search must exit as soon as possible.
    is_searching: Arc<AtomicBool>,

    /// Configuration variables for this instance of the search.
    config: SearchConfig,

    /// Information collected that is returned at the conclusion of the search.
    result: SearchResult,

    /// Previous positions encountered during search.
    prev_positions: Vec<Position>,

    /// Transposition table used to cache information during search.
    ttable: &'a mut TTable,

    /// Storage for moves that cause a beta-cutoff during search.
    history: &'a mut HistoryTable,

    /// Parameters for search features like pruning, extensions, etc.
    params: SearchParameters,

    /// Marker for what variant of Chess is being played.
    variant: PhantomData<V>,

    /// Marker for the level of logging to print.
    log: PhantomData<Log>,
}

impl<'a, Log: LogLevel, V: Variant> Search<'a, Log, V> {
    /// Construct a new [`Search`] instance to execute.
    #[inline(always)]
    pub fn new(
        is_searching: Arc<AtomicBool>,
        config: SearchConfig,
        prev_positions: Vec<Position>,
        ttable: &'a mut TTable,
        history: &'a mut HistoryTable,
        params: SearchParameters,
    ) -> Self {
        Self {
            is_searching,
            config,
            prev_positions,
            ttable,
            history,
            params,
            result: SearchResult::default(),
            variant: PhantomData,
            log: PhantomData,
        }
    }

    /// Start the search on the supplied [`Game`], returning a [`SearchResult`].
    ///
    /// This is the entrypoint of the search, and prints UCI info before starting iterative deepening.
    /// and concluding by sending the `bestmove` message and exiting.
    #[inline(always)]
    pub fn start(mut self, game: &Game<V>) -> SearchResult {
        if Log::DEBUG {
            self.send_string(format!("Starting search on {:?}", game.to_fen()));

            let soft = self.config.soft_timeout;
            let hard = self.config.hard_timeout;
            let nodes = self.config.max_nodes;
            let depth = self.config.max_depth;

            if soft < Duration::MAX {
                self.send_string(format!("Soft timeout := {soft:?}"));
            }
            if hard < Duration::MAX {
                self.send_string(format!("Hard timeout := {hard:?}"));
            }
            if nodes < u64::MAX {
                self.send_string(format!("Max nodes := {nodes} nodes"));
            }
            if depth < Ply::MAX {
                self.send_string(format!("Max depth := {depth}"));
            }
        }

        // Get the legal moves at the root, so we can ensure that there is at least one move we can play.
        let moves = game.get_legal_moves();

        // Get the search result, exiting early if possible.
        match moves.len() {
            // If no legal moves available, the game is over, so return immediately.
            0 => {
                // It's either a draw or a checkmate
                self.result.score = -Score::MATE * game.is_in_check();
                self.result.nodes += 1;

                if Log::DEBUG {
                    self.send_string(format!(
                        "Position {:?} has no legal moves available, evaluated at {}",
                        game.to_fen(),
                        self.result.score.into_uci(),
                    ));
                }
            }

            /*
            // If only 1 legal move available, it is forced, so don't waste time on a full search.
            1 => {
                // Get a quick, albeit poor, evaluation of the position.
                // TODO: Replace this with a call to qsearch?
                self.result.score = game.eval();
                self.result.nodes += 1;

                // Append the only legal move to the PV
                let bestmove = moves[0];
                self.result.pv.0.push(bestmove);

                if Log::DEBUG {
                    self.send_string(format!(
                        "Position {:?} has only one legal move available ({bestmove}), evaluated at {}",
                        game.to_fen(),
                        self.result.score.into_uci(),
                    ));
                }
            }
             */
            // Otherwise, start a search like normal.
            _ => self.iterative_deepening(game),
        }

        // Debug info about the termination of the search.
        if Log::DEBUG {
            if let Err(reason) = self.search_cancelled() {
                if let Some(bestmove) = self.result.bestmove() {
                    self.send_string(format!(
                        "Search cancelled during depth {} while evaluating {} with score {}. Reason: {reason}",
                        self.result.depth,
                        V::fmt_move(bestmove),
                        self.result.score,
                    ));
                } else {
                    self.send_string(format!(
                        "Search cancelled during depth {} with score {} and no bestmove. Reason: {reason}",
                        self.result.depth, self.result.score,
                    ));
                }
            }

            let hits = self.ttable.hits;
            let reads = self.ttable.reads;
            let writes = self.ttable.writes;
            let hit_rate = (hits as f32 / reads as f32 * 100.0).min(0.0);
            let collisions = self.ttable.collisions;
            let info = format!("TT stats: {hits} hits / {reads} reads ({hit_rate:.2}% hit rate), {writes} writes, {collisions} collisions");
            self.send_string(info);
        }

        // Sanity check: If no bestmove, but there is a legal move, update bestmove.
        if self.result.bestmove().is_none() {
            if let Some(first) = moves.first().copied() {
                self.result.pv.0.push(first);
                self.result.score = game.eval();
            }
        }

        // Search has ended; send bestmove
        if Log::INFO {
            self.send_search_info(); // UCI spec states to send one last `info` before `bestmove`.

            // TODO: On a `go infinite` search, we should *only* send `bestmove` after `stop` is received, regardless of whether the search has concluded
            self.send_response(UciResponse::BestMove {
                bestmove: self.result.bestmove().map(V::fmt_move),
                ponder: None,
            });
        }

        // Search has concluded, alert other thread(s) that we are no longer searching
        self.is_searching.store(false, Ordering::Relaxed);

        self.result
    }

    /// Sends a [`UciResponse`] to `stdout`.
    #[inline(always)]
    fn send_response<T: fmt::Display>(&self, response: UciResponse<T>) {
        println!("{response}");
    }

    /// Sends a [`UciInfo`] to `stdout`.
    #[inline(always)]
    fn send_info(&self, info: UciInfo) {
        let resp = UciResponse::info(info);
        self.send_response(resp);
    }

    /// Helper to send a [`UciInfo`] containing only a `string` message to `stdout`.
    #[inline(always)]
    fn send_string<T: fmt::Display>(&self, string: T) {
        self.send_response(UciResponse::info_string(string));
    }

    /// Sends UCI info about the conclusion of a search.
    ///
    /// This is sent at the end of each new search in the iterative deepening loop.
    #[inline(always)]
    fn send_search_info(&self) {
        let elapsed = self.config.starttime.elapsed();

        self.send_info(
            UciInfo::new()
                .depth(self.result.depth)
                .seldepth(self.result.seldepth)
                .nodes(self.result.nodes)
                .score(self.result.score)
                .nps((self.result.nodes as f32 / elapsed.as_secs_f32()).trunc())
                .time(elapsed.as_millis())
                .pv(self.result.pv.0.iter().map(|&mv| V::fmt_move(mv))),
        );
    }

    /// Performs [iterative deepening](https://www.chessprogramming.org/Iterative_Deepening) (ID) on the Search's position.
    ///
    /// ID is a basic time management strategy for engines.
    /// It involves performing a search at depth `n`, then, if there is enough time remaining, performing a search at depth `n + 1`.
    /// On it's own, ID does not improve performance, because we are wasting work by re-running searches at low depth.
    /// However, with features such as move ordering, a/b pruning, and aspiration windows, ID enhances performance.
    ///
    /// After each iteration, we check if we've exceeded our `soft_timeout` and, if we haven't, we run a search at a greater depth.
    fn iterative_deepening(&mut self, game: &Game<V>) {
        /****************************************************************************************************
         * Iterative Deepening: https://www.chessprogramming.org/Iterative_Deepening
         *
         * Since we don't know how much time a search will take, we perform a series of searches as increasing
         * depths until we run out of time.
         ****************************************************************************************************/
        'iterative_deepening: while self.config.starttime.elapsed() < self.config.soft_timeout
            && self.is_searching.load(Ordering::Relaxed)
            && self.result.depth <= self.config.max_depth
        {
            /****************************************************************************************************
             * Aspiration Windows: https://www.chessprogramming.org/Aspiration_Windows
             *
             * If our search is stable, the result of a search from the next depth should be similar to our
             * current result. Therefore, we can use the current result to initialize our alpha/beta bounds.
             ****************************************************************************************************/

            // Create a new aspiration window for this search
            let mut window = AspirationWindow::new(self.result.score, self.result.depth);
            let mut pv = PrincipalVariation::EMPTY;

            // Get a score from the a/b search while using aspiration windows
            let score = 'aspiration_window: loop {
                // Start a new search at the current depth, exiting the ID loop if we've ran out of time
                let Ok(score) = self.negamax::<RootNode>(
                    game,
                    self.result.depth,
                    Ply::ZERO,
                    window.bounds,
                    &mut pv,
                ) else {
                    break 'iterative_deepening;
                };

                // If the score fell outside of the aspiration window, widen it gradually
                if window.fails_low(score) {
                    window.widen_down(score, self.result.depth);
                } else if window.fails_high(score) {
                    window.widen_up(score, self.result.depth);
                } else {
                    // Otherwise, the window is OK and we can use the score
                    break 'aspiration_window score;
                }
            };

            /****************************************************************************************************
             * Update current best score
             ****************************************************************************************************/

            // We need to update our bestmove and score, since this iteration's search completed without timeout.
            self.result.score = score;
            self.result.pv = pv;

            // Hack; if we're on the last iteration, don't send an `info` line, as it gets sent just before `bestmove` anyway.
            if self.result.depth == self.config.max_depth {
                break;
            }

            // Send search info to the GUI
            if Log::INFO {
                self.send_search_info();
            }

            // Increase the depth for the next iteration
            self.result.depth += 1;
        }
    }

    /// Primary location of search logic.
    ///
    /// Uses the [negamax](https://www.chessprogramming.org/Negamax) algorithm in a [fail soft](https://www.chessprogramming.org/Alpha-Beta#Negamax_Framework) framework.
    fn negamax<Node: NodeType>(
        &mut self,
        game: &Game<V>,
        mut depth: Ply,
        ply: Ply,
        mut bounds: SearchBounds,
        pv: &mut PrincipalVariation,
    ) -> Result<Score, SearchCancelled> {
        self.search_cancelled()?; // Exit early if search is terminated.

        // Don't bother searching drawn positions, unless we're at the root node.
        if !Node::ROOT && self.is_draw(game) {
            return Ok(Score::DRAW);
        }

        /****************************************************************************************************
         * Quiescence Search: https://www.chessprogramming.org/Quiescence_Search
         *
         * In order to avoid the horizon effect, we don't stop searching at a depth of 0. Instead, we
         * continue searching all "noisy" moves until we reach a "quiet" (quiescent) position.
         ****************************************************************************************************/
        if depth <= 0 {
            return self.quiescence::<Node>(game, ply, bounds, pv);
        }

        // Record the max max height / max ply / seldepth
        self.result.seldepth = self.result.seldepth.max(ply) * !Node::ROOT as i32;

        // Declare a local principal variation for nodes found during this search.
        let mut local_pv = PrincipalVariation::default();
        // Clear any nodes in this PV, since we're searching from a new position
        pv.clear();

        // Initial node inspection, such as mate-distance pruning, draws, etc.
        if !Node::ROOT {
            /****************************************************************************************************
             * Mate-Distance Pruning: https://www.chessprogramming.org/Mate_Distance_Pruning
             *
             * If we've found a mate-in-n, prune all other branches that are not mate-in-m where m < n.
             * This doesn't really affect playing strength, since it only occurs when the game result is certain,
             * but helps avoid searching useless nodes.
             ****************************************************************************************************/
            // Clamp the bounds to a mate-in-`ply` score, if possible.
            bounds.alpha = bounds.alpha.max(Score::mated_in(ply));
            bounds.beta = bounds.beta.min(Score::mate_in(ply + 1));

            // Prune this node if no shorter mate has been found.
            if bounds.alpha >= bounds.beta {
                return Ok(bounds.alpha);
            }
        }

        // Probe the TT to see if we can return early or use an existing bestmove.
        if Log::DEBUG {
            self.ttable.reads += 1;
        }
        let tt_move = match self.ttable.probe(game.key(), depth, bounds) {
            /****************************************************************************************************
             * TT Cutoffs: https://www.chessprogramming.org/Transposition_Table#Transposition_Table_Cutoffs
             *
             * If we've already evaluated this position before at a higher depth, we can avoid re-doing a lot of
             * work by just returning the evaluation stored in the transposition table. However, we must be sure
             * that we are not in a PV node.
             ****************************************************************************************************/
            ProbeResult::Cutoff(tt_score) if !Node::PV => return Ok(tt_score),

            // Entry was found, but could not be used to perform a cutoff
            ProbeResult::Hit(tt_entry) => tt_entry.bestmove,

            // Miss or otherwise unusable result
            _ => {
                /****************************************************************************************************
                 * Internal Iterative Deepening: https://www.chessprogramming.org/Internal_Iterative_Deepening
                 *
                 * If we're in a PV node and there was no TT hit, this is likely to be a costly search, due to poor
                 * move ordering. So, we perform a shallower search in order to get a TT move and to populate the
                 * hash tables.
                 ****************************************************************************************************/
                if Node::PV && depth >= self.params.min_iid_depth {
                    let iid_depth = depth - self.params.min_iid_depth + self.params.iid_offset;
                    self.negamax::<Node>(game, iid_depth, ply, bounds, &mut local_pv)?;
                    local_pv.0.first().copied() // Return the bestmove found during the reduced search
                } else {
                    None
                }
            }
        };

        /****************************************************************************************************
         * Internal Iterative Reductions: https://www.chessprogramming.org/Internal_Iterative_Reductions
         *
         * Also known as Transposition Table Reductions. If no bestmove was found when probing the TT, we are
         * likely to spend a lot of time on this search, due to poor move ordering. It is also likely that
         * this node isn't *that* important, since it wasn't already in the TT. So, we perform a reduced-depth
         * search to speed things up and hopefully deliver better results.
         ****************************************************************************************************/
        if tt_move.is_none() && depth >= self.params.min_iir_depth {
            depth -= 1;
        }

        // If we CAN prune this node by means other than the TT, do so.
        if let Some(score) =
            self.node_pruning_score::<Node>(game, depth, ply, bounds, pv, &mut local_pv)?
        {
            return Ok(score);
        }

        // If there are no legal moves, it's either mate or a draw.
        let mut moves = game.get_legal_moves();
        if moves.is_empty() {
            return Ok(-Score::MATE * game.is_in_check());
        }

        // Sort moves so that we look at "promising" ones first
        moves.sort_by_cached_key(|mv| self.score_move(game, mv, tt_move));

        // Start with a *really bad* initial score
        let mut best = -Score::INF;
        let mut bestmove = tt_move; // Ensures we don't overwrite TT entry's bestmove with `None` if one already existed.
        let original_alpha = bounds.alpha;

        /****************************************************************************************************
         * Primary move loop
         ****************************************************************************************************/

        for (i, mv) in moves.iter().enumerate() {
            /****************************************************************************************************
             * Move-Loop Pruning techniques
             ****************************************************************************************************/
            if !Node::PV && !best.mated() {
                /****************************************************************************************************
                 * Late Move Pruning: https://www.chessprogramming.org/Futility_Pruning#MoveCountBasedPruning
                 *
                 * We assume our move ordering is so good and that the moves ordered last are so bad that we should
                 * not even bother searching them.
                 ****************************************************************************************************/
                let min_lmp_moves =
                    self.params.lmp_multiplier * moves.len() / self.params.lmp_divisor;
                if depth <= self.params.max_lmp_depth && i >= min_lmp_moves {
                    break;
                }
            }

            // Copy-make the new position
            let new = game.with_move_made(*mv);
            let mut score = Score::DRAW;

            // The local PV is different for every node search after this one, so we must reset it in between recursive calls.
            local_pv.clear();

            /****************************************************************************************************
             * Recursion of the search
             ****************************************************************************************************/
            // Append this position onto our stack, so we can detect repetitions
            self.prev_positions.push(*new.position());

            let new_depth = depth - 1 + self.extension_value(&new);

            // If this node can be reduced, search it with a reduced window.
            if let Some(lmr_reduction) = self.reduction_value::<Node>(depth, &new, i) {
                // Reduced depth should never exceed `new_depth` and should never be less than `1`.
                let reduced_depth = (new_depth - lmr_reduction).max(Ply::ONE).min(new_depth);

                // Search at a reduced depth with a null window
                score = -self.negamax::<NonPvNode>(
                    &new,
                    reduced_depth,
                    ply + 1,
                    -bounds.null_alpha(),
                    &mut local_pv,
                )?;

                // If that failed *high* (raised alpha), re-search at the full depth with the null window
                if score > bounds.alpha && reduced_depth < new_depth {
                    score = -self.negamax::<NonPvNode>(
                        &new,
                        new_depth,
                        ply + 1,
                        -bounds.null_alpha(),
                        &mut local_pv,
                    )?;
                }
            } else if !Node::PV || i > 0 {
                // All non-PV nodes get searched with a null window
                score = -self.negamax::<NonPvNode>(
                    &new,
                    new_depth,
                    ply + 1,
                    -bounds.null_alpha(),
                    &mut local_pv,
                )?;
            }

            /****************************************************************************************************
             * Principal Variation Search: https://en.wikipedia.org/wiki/Principal_variation_search#Pseudocode
             *
             * We assume our move ordering is so good that the first move searched is then best available. So,
             * for every other move, we search with a null window and thus prune nodes easier. If we find
             * something that beats the null window, we have to do a costly re-search. However, this happens so
             * infrequently in practice that it ends up being an overall speedup.
             ****************************************************************************************************/
            // If searching the PV, or if a reduced search failed *high*, we search with a full depth and window
            if Node::PV && (i == 0 || score > bounds.alpha) {
                score =
                    -self.negamax::<PvNode>(&new, new_depth, ply + 1, -bounds, &mut local_pv)?;
            }

            self.search_cancelled()?; // Exit early if search is terminated.

            // We've now searched this node
            self.result.nodes += 1;

            // Pop the move from the history
            self.prev_positions.pop();

            /****************************************************************************************************
             * Score evaluation & bounds adjustments
             ****************************************************************************************************/

            // If we've found a better move than our current best, update the results
            if score > best {
                best = score;

                // PV found
                if score > bounds.alpha {
                    bounds.alpha = score;
                    bestmove = Some(*mv);

                    // Only extend the PV if we're in a PV node
                    if Node::PV {
                        // assert_pv_is_legal(game, *mv, &local_pv);
                        pv.extend(*mv, &local_pv);
                    }
                }

                // Fail high
                if score >= bounds.beta {
                    /****************************************************************************************************
                     * History Heuristic
                     *
                     * If a quiet move fails high, it is probably a good move. Therefore we want to look at it early on
                     * in future searches. We also penalize previously-searched quiets, since they are clearly not as good
                     * as this one (as they did not cause a beta cutoff).
                     ****************************************************************************************************/
                    // Simple bonus based on depth
                    let bonus = self.params.history_multiplier * depth - self.params.history_offset;

                    // Only update quiet moves
                    if mv.is_quiet() {
                        self.history.update(game, mv, bonus);
                    }

                    // Apply a penalty to all quiets searched so far
                    for mv in moves[..i].iter().filter(|mv| mv.is_quiet()) {
                        self.history.update(game, mv, -bonus);
                    }
                    break;
                }
            }
        }

        // Adjust mate score by 1 ply, since we're returning up the call stack
        if best.is_mate() {
            best -= best.signum();
        }

        // Save this node to the TTable.
        self.save_to_tt(
            game.key(),
            bestmove,
            best,
            SearchBounds::new(original_alpha, bounds.beta),
            depth,
        );

        Ok(best)
    }

    /// Quiescence Search (QSearch)
    ///
    /// A search that looks at only possible captures and capture-chains.
    /// This is called when [`Search::negamax`] reaches a depth of 0, and has no recursion limit.
    fn quiescence<Node: NodeType>(
        &mut self,
        game: &Game<V>,
        ply: Ply,
        mut bounds: SearchBounds,
        pv: &mut PrincipalVariation,
    ) -> Result<Score, SearchCancelled> {
        self.search_cancelled()?; // Exit early if search is terminated.

        // Don't bother searching drawn positions, unless we're at the root node.
        if !Node::ROOT && self.is_draw(game) {
            return Ok(Score::DRAW);
        }

        // Record the max max height / max ply / seldepth
        self.result.seldepth = self.result.seldepth.max(ply) * !Node::ROOT as i32;

        // Declare a local principal variation for nodes found during this search.
        let mut local_pv = PrincipalVariation::default();
        // Clear any nodes in this PV, since we're searching from a new position
        pv.clear();

        // Evaluate the current position, to serve as our baseline
        let static_eval = game.eval();

        // Beta cutoff; this position is "too good" and our opponent would never let us get here
        if static_eval >= bounds.beta {
            return Ok(static_eval); // fail-soft
        } else if static_eval > bounds.alpha {
            bounds.alpha = static_eval;
        }

        // Probe the TT to see if we can return early or use an existing bestmove.
        let tt_move = match self.ttable.probe(game.key(), Ply::ZERO, bounds) {
            /****************************************************************************************************
             * TT Cutoffs: https://www.chessprogramming.org/Transposition_Table#Transposition_Table_Cutoffs
             *
             * If we've already evaluated this position before at a higher depth, we can avoid re-doing a lot of
             * work by just returning the evaluation stored in the transposition table. However, we must be sure
             * that we are not in a PV node.
             ****************************************************************************************************/
            ProbeResult::Cutoff(tt_score) if !Node::PV => return Ok(tt_score),

            // Entry was found, but could not be used to perform a cutoff
            ProbeResult::Hit(tt_entry) => tt_entry.bestmove,

            // Miss or otherwise unusable result
            _ => None,
        };

        // Generate only the legal captures
        // TODO: Is there a more concise way of doing this?
        // The `game.into_iter().only_captures()` doesn't cover en passant...
        let mut moves = game
            .get_legal_moves()
            .into_iter()
            .filter(Move::is_capture)
            .collect::<MoveList>();

        // Can't check for mates in normal qsearch, since we're not looking at *all* moves.
        // So, if there are no captures available, just return the current evaluation.
        if moves.is_empty() {
            return Ok(static_eval);
        }

        moves.sort_by_cached_key(|mv| self.score_move(game, mv, tt_move));

        let mut best = static_eval;
        let mut bestmove = tt_move; // Ensures we don't overwrite TT entry's bestmove with `None` if one already existed.
        let original_alpha = bounds.alpha;

        /****************************************************************************************************
         * Primary move loop
         ****************************************************************************************************/

        for mv in moves {
            // The local PV is different for every node search after this one, so we must reset it in between recursive calls.
            local_pv.clear();

            // Copy-make the new position
            let new = game.with_move_made(mv);

            /****************************************************************************************************
             * Recursion of the search
             ****************************************************************************************************/
            // Append this position onto our stack, so we can detect repetitions
            self.prev_positions.push(*new.position());

            let score = -self.quiescence::<Node>(&new, ply + 1, -bounds, &mut local_pv)?;

            self.search_cancelled()?; // Exit early if search is terminated.

            self.result.nodes += 1; // We've now searched this node

            self.prev_positions.pop();

            /****************************************************************************************************
             * Score evaluation & bounds adjustments
             ****************************************************************************************************/
            // If we've found a better move than our current best, update our result
            if score > best {
                best = score;

                // PV found
                if score > bounds.alpha {
                    bounds.alpha = score;
                    bestmove = Some(mv);

                    // Only extend the PV if we're in a PV node
                    if Node::PV {
                        // assert_pv_is_legal(game, mv, &local_pv);
                        pv.extend(mv, &local_pv);
                    }
                }

                // Fail high
                if score >= bounds.beta {
                    break;
                }
            }
        }

        // Adjust mate score by 1 ply, since we're returning up the call stack
        if best.is_mate() {
            best -= best.signum();
        }

        // Save this node to the TTable.
        self.save_to_tt(
            game.key(),
            bestmove,
            best,
            SearchBounds::new(original_alpha, bounds.beta),
            Ply::ZERO,
        );

        Ok(best) // fail-soft
    }

    /// Checks if we've exceeded any conditions that would warrant the search to end.
    ///
    /// This method returns an `Err` of [`SearchCancelled`] if the search must end prematurely.
    /// While a termination isn't truly an error, this API allows us to cleanly leverage the `?` operator.
    #[inline(always)]
    fn search_cancelled(&self) -> Result<(), SearchCancelled> {
        // Only check for timeouts every 1024 nodes, because searches are fast and 1k nodes doesn't take too long..
        if self.result.nodes % 1024 == 0 {
            // We've exceeded the hard limit of our allotted search time
            if let Some(diff) = self
                .config
                .starttime
                .elapsed()
                .checked_sub(self.config.hard_timeout)
            {
                return Err(SearchCancelled::HardTimeout(self.config.hard_timeout, diff));
            }
        }

        // We've exceeded the maximum amount of nodes we're allowed to search
        if let Some(diff) = self.result.nodes.checked_sub(self.config.max_nodes) {
            return Err(SearchCancelled::MaxNodes(diff));
        }

        // The search was stopped by an external factor, like the `stop` command
        if !self.is_searching.load(Ordering::Relaxed) {
            return Err(SearchCancelled::Stopped);
        }

        // No conditions met; we can continue searching
        Ok(())
    }

    /// Checks if `game` is a repetition, comparing it to previous positions
    #[inline(always)]
    fn is_repetition(&self, game: &Game<V>) -> bool {
        // We can skip the previous position, because there's no way it can be a repetition.
        // We also only need to look check at most `halfmove` previous positions.
        let n = game.halfmove();
        for prev in self.prev_positions.iter().rev().take(n).skip(1).step_by(2) {
            if prev.key() == game.key() {
                return true;
            } else
            // The halfmove counter only resets on irreversible moves (captures, pawns, etc.) so it can't be a repetition.
            if prev.halfmove() == 0 {
                return false;
            }
        }

        false
    }

    /// Returns `true` if `game` can be claimed as a draw
    #[inline(always)]
    fn is_draw(&self, game: &Game<V>) -> bool {
        self.is_repetition(game)
            || game.can_draw_by_fifty()
            || game.can_draw_by_insufficient_material()
    }

    /// Saves the provided data to an entry in the TTable.
    #[inline(always)]
    fn save_to_tt(
        &mut self,
        key: ZobristKey,
        bestmove: Option<Move>,
        score: Score,
        bounds: SearchBounds,
        depth: Ply,
    ) {
        let entry = TTableEntry::new(key, bestmove, score, bounds, depth);
        let old = self.ttable.store(entry);

        if Log::DEBUG {
            // If a previous entry existed and had a *different* key, this was a collision
            if old.is_some_and(|old| old.key != key) {
                self.ttable.collisions += 1;
            }

            // This was a write, regardless.
            self.ttable.writes += 1;
        }
    }

    /// Applies a score to the provided move, intended to be used when ordering moves during search.
    #[inline(always)]
    fn score_move(&self, game: &Game<V>, mv: &Move, tt_move: Option<Move>) -> Score {
        // TT move should be looked at first, so assign it the best possible score and immediately exit.
        if tt_move.is_some_and(|tt_mv| tt_mv == *mv) {
            return Score::new(i32::MIN);
        }

        // Safe unwrap because we can't move unless there's a piece at `from`
        let piece = game.piece_at(mv.from()).unwrap();
        let to = mv.to();
        let mut score = Score::BASE_MOVE_SCORE;

        // Apply history bonus to quiets
        if mv.is_quiet() {
            score += self.history[piece][to];
        } else
        // Capturing a high-value piece with a low-value piece is a good idea
        if let Some(victim) = game.piece_at(to) {
            score += MVV_LVA[piece][victim];
        }

        -score // We're sorting, so a lower number is better
    }

    /// If we can prune the provided node, this function returns a score to return upon pruning.
    ///
    /// If we cannot prune the node, this function returns `None`.
    #[inline]
    fn node_pruning_score<Node: NodeType>(
        &mut self,
        game: &Game<V>,
        depth: Ply,
        ply: Ply,
        bounds: SearchBounds,
        pv: &mut PrincipalVariation,
        local_pv: &mut PrincipalVariation,
    ) -> Result<Option<Score>, SearchCancelled> {
        // Cannot prune anything in a PV node or if we're in check
        if Node::PV || game.is_in_check() {
            return Ok(None);
        }

        // Static evaluation of the current position is used in multiple pruning techniques.
        let static_eval = game.eval();

        /****************************************************************************************************
         * Razoring: https://www.chessprogramming.org/Razoring
         *
         * If the static eval of our position is low enough, check if a qsearch can beat alpha.
         * If it can't, we can prune this node.
         ****************************************************************************************************/
        let razoring_margin = Score::RAZORING_OFFSET + Score::RAZORING_MULTIPLIER * depth;
        if depth <= self.params.max_razoring_depth && static_eval + razoring_margin < bounds.alpha {
            let score = self.quiescence::<Node>(game, ply, bounds.null_alpha(), pv)?;
            // If we can't beat alpha (without mating), we can prune.
            if score < bounds.alpha && !score.is_mate() {
                return Ok(Some(score)); // fail-soft
            }
        }

        /****************************************************************************************************
         * Reverse Futility Pruning: https://www.chessprogramming.org/Reverse_Futility_Pruning
         *
         * If our static eval is too good (better than beta), we can prune this branch. Multiplying our
         * margin by depth makes this pruning process less risky for higher depths.
         ****************************************************************************************************/
        let rfp_score = static_eval - self.params.rfp_margin * depth;
        if depth <= self.params.max_rfp_depth && rfp_score >= bounds.beta {
            return Ok(Some(rfp_score));
        }

        /****************************************************************************************************
         * Null Move Pruning: https://www.chessprogramming.org/Null_Move_Pruning
         *
         * If we can afford to skip our turn and give our opponent two moves in a row while maintaining a high
         * enough score, we can prune this branch as our opponent would likely never let us reach it anyway.
         ****************************************************************************************************/
        // If the last move did not increment the fullmove, but *did* increment the halfmove, it was a nullmove
        let last_move_was_nullmove = self.prev_positions.last().is_some_and(|pos| {
            pos.fullmove() == game.fullmove() && pos.halfmove() == game.halfmove() + 1
        });

        // All pieces that are not Kings or Pawns
        let non_king_pawn_material =
            game.occupied() ^ game.kind(PieceKind::Pawn) ^ game.kind(PieceKind::King);

        let can_perform_nmp = depth >= self.params.min_nmp_depth // Can't play nullmove under a certain depth
        && !last_move_was_nullmove // Can't play two nullmoves in a row
        && non_king_pawn_material.is_nonempty(); // Can't play nullmove if insufficient material (only Kings and Pawns)

        if can_perform_nmp {
            let null_game = game.with_nullmove_made();
            // Record this position in our stack, for repetition detection
            self.prev_positions.push(*null_game.position());

            // Search at a reduced depth with a zero-window
            let nmp_depth = depth - self.params.nmp_reduction;
            let score = -self.negamax::<Node>(
                &null_game,
                nmp_depth,
                ply + 1,
                -bounds.null_beta(),
                local_pv,
            )?;

            self.prev_positions.pop();

            // If making the nullmove produces a cutoff, we can assume that a full-depth search would also produce a cutoff
            if score >= bounds.beta {
                return Ok(Some(score));
            }
        }

        // If no pruning technique was possible, return no score
        Ok(None)
    }

    /// Compute a reduction value (`R`) to apply to a given node's search depth, if possible.
    #[inline(always)]
    fn reduction_value<Node: NodeType>(
        &self,
        depth: Ply,
        game: &Game<V>,
        moves_made: usize,
    ) -> Option<i32> {
        /****************************************************************************************************
         * Late Move Reductions: https://www.chessprogramming.org/Late_Move_Reductions
         *
         * We assume our move ordering will let us search the best moves first. Thus, the last moves are
         * likely to be the worst moves. We can save some time by searching these at a lower depth and with
         * a null window. If this fails, however, we must perform a costly re-search.
         ****************************************************************************************************/
        (depth >= self.params.min_lmr_depth
            && moves_made >= self.params.min_lmr_moves + Node::PV as usize)
            .then(|| {
                // Base LMR reduction increases as we go higher in depth and/or make more moves
                let mut lmr_reduction = self.params.lmr_table[depth.plies() as usize][moves_made];
                // let mut lmr_reduction = (self.params.lmr_offset
                //     + (depth.plies() as f32).ln() * (moves_made as f32).ln()
                //         / self.params.lmr_divisor) as i32;

                // Increase/decrease the reduction based on current conditions
                // lmr_reduction += something;
                lmr_reduction -= game.is_in_check() as i32;

                lmr_reduction
            })
    }

    /// Compute an extension value to apply to a given node's search depth.
    #[inline(always)]
    fn extension_value(&self, game: &Game<V>) -> Ply {
        let mut extension = Ply::ZERO;

        /****************************************************************************************************
         * Check Extensions: https://www.chessprogramming.org/Check_Extensions
         *
         * If we're in check, we should extend the search a bit, in hopes to find a good way to escape.
         ****************************************************************************************************/
        if game.is_in_check() {
            extension += self.params.check_extensions_depth
        }

        extension
    }
}

/// Utility function to assert that the PV is legal for the provided game.
#[allow(dead_code)]
fn assert_pv_is_legal<V: Variant>(game: &Game<V>, mv: Move, local_pv: &PrincipalVariation) {
    let fen = game.to_fen();
    let mut game = game.with_move_made(mv);

    for local_pv_mv in &local_pv.0 {
        assert!(
            game.is_legal(*local_pv_mv),
            "Illegal PV move {local_pv_mv} found on {fen}\nFull PV: {}\nResulting FEN: {}",
            [&mv]
                .into_iter()
                .chain(local_pv.0.iter())
                .map(|m| m.to_string())
                .collect::<Vec<_>>()
                .join(" "),
            game.to_fen()
        );
        game.make_move(*local_pv_mv);
    }
}

/// This table represents values for [MVV-LVA](https://www.chessprogramming.org/MVV-LVA) move ordering.
///
/// It is indexed by `[attacker][victim]`, and yields a "score" that is used when sorting moves.
///
/// The following table is produced:
/// ```text
///                     VICTIM
/// A       P     N     B     R     Q     K     
/// T    +---------------------------------+
/// T   P| 900   3100  3200  4900  8900  0     
/// A   N| 680   2880  2980  4680  8680  0     
/// C   B| 670   2870  2970  4670  8670  0     
/// K   R| 500   2700  2800  4500  8500  0     
/// E   Q| 100   2300  2400  4100  8100  0     
/// R   K| 1000  3200  3300  5000  9000  0     
/// ```
///
/// Note that the actual table is different, as it has size `12x12` instead of `6x6`
/// to account for the fact that castling is denoted as `KxR`.
/// The values are also all left-shifted by 16 bits, to ensure that captures are ranked above quiets in all cases.
///
/// See [`print_mvv_lva_table`] to display this table.
const MVV_LVA: [[i32; Piece::COUNT]; Piece::COUNT] = {
    let mut matrix = [[0; Piece::COUNT]; Piece::COUNT];
    let count = Piece::COUNT;

    let mut attacker = 0;
    while attacker < count {
        let mut victim = 0;
        let atk_color = Color::from_bool(attacker < PieceKind::COUNT);

        while victim < count {
            let atk = PieceKind::from_bits_unchecked(attacker as u8 % 6);
            let vtm = PieceKind::from_bits_unchecked(victim as u8 % 6);

            let vtm_color = Color::from_bool(victim < PieceKind::COUNT);

            // Remove scores for capturing the King and friendly pieces (KxR for castling)
            let can_capture = (atk_color.index() != vtm_color.index()) // Different colors
                && victim != count - 1 // Can't capture White King
                && victim != PieceKind::COUNT - 1; // Can't capture Black King

            // Rustic's way of doing things; Arbitrary increasing numbers for capturing pairs
            // bench: 27609398 nodes 5716479 nps
            // let score = (victim * 10 + (count - attacker)) as i32;

            // Default MVV-LVA except that the King is assigned a value of 0 if he is attacking
            // bench: 27032804 nodes 8136592 nps
            let score = 10 * vtm.value() - atk.value();

            // If the attacker is the King, the score is half the victim's value.
            // This encourages the King to attack, but not as strongly as other pieces.
            // bench: 27107011 nodes 5647285 nps
            // let score = if attacker == count - 1 {
            //     value_of(vtm) / 2
            // } else {
            //     // Standard MVV-LVA computation
            //     10 * value_of(vtm) - value_of(atk)
            // };

            // Shift the value by a large amount so that captures are always ranked very highly
            matrix[attacker][victim] = (score * can_capture as i32) << 16;
            victim += 1;
        }
        attacker += 1;
    }
    matrix
};

/// Utility function to print the MVV-LVA table
#[allow(dead_code)]
pub fn print_mvv_lva_table() {
    print!("\nX  ");
    for victim in Piece::all() {
        print!("{victim}     ");
    }
    print!("\n +");
    for _ in Piece::all() {
        print!("------");
    }
    println!("-+");
    for attacker in Piece::all() {
        print!("{attacker}| ");
        for victim in Piece::all() {
            let score = MVV_LVA[attacker][victim];
            print!("{score:<4}  ")
        }
        println!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::*;

    fn run_search(fen: &str, config: SearchConfig) -> SearchResult {
        let is_searching = Arc::new(AtomicBool::new(true));
        let game = fen.parse().unwrap();

        let mut ttable = Default::default();
        let mut history = Default::default();
        Search::<LogNone, Standard>::new(
            is_searching,
            config,
            Default::default(),
            &mut ttable,
            &mut history,
            Default::default(),
        )
        .start(&game)
    }

    fn ensure_is_mate_in(fen: &str, config: SearchConfig, moves: i32) -> SearchResult {
        let res = run_search(fen, config);
        assert!(
            res.score.is_mate(),
            "Search on {fen:?} with config {config:#?} produced result that is not mate.\nResult: {res:#?}"
        );
        assert_eq!(
            res.score.moves_to_mate(),
            moves,
            "Search on {fen:?} with config {config:#?} produced result not mate in {moves}.\nResult: {res:#?}"
        );
        res
    }

    #[test]
    fn test_white_mate_in_1() {
        let fen = "k7/8/KQ6/8/8/8/8/8 w - - 0 1";
        let config = SearchConfig {
            max_depth: Ply::new(2),
            ..Default::default()
        };

        let res = ensure_is_mate_in(fen, config, 1);
        assert_eq!(res.bestmove().unwrap(), "b6a7", "Result: {res:#?}");
    }

    #[test]
    fn test_black_mated_in_1() {
        let fen = "2k5/7Q/8/2K5/8/8/8/6Q1 b - - 0 1";
        let config = SearchConfig {
            max_depth: Ply::new(3),
            ..Default::default()
        };

        let res = ensure_is_mate_in(fen, config, -1);
        assert!(["c8b8", "c8d8"].contains(&res.bestmove().unwrap().to_string().as_str()));
    }

    #[test]
    fn test_stalemate() {
        let fen = "k7/8/KQ6/8/8/8/8/8 b - - 0 1";
        let config = SearchConfig::default();

        let res = run_search(fen, config);
        assert!(res.bestmove().is_none());
        assert_eq!(res.score, Score::DRAW);
    }

    #[test]
    fn test_obvious_capture_promote() {
        // Pawn should take queen and also promote to queen
        let fen = "3q1n2/4P3/8/8/8/8/k7/7K w - - 0 1";
        let config = SearchConfig {
            max_depth: Ply::new(1),
            ..Default::default()
        };

        let res = run_search(fen, config);
        assert_eq!(res.bestmove().unwrap(), "e7d8q");
    }

    #[test]
    fn test_quick_search_finds_move() {
        // If *any* legal move is available, it should be found, regardless of how much time was given.
        let fen = FEN_STARTPOS;
        let config = SearchConfig {
            soft_timeout: Duration::from_nanos(1),
            hard_timeout: Duration::from_nanos(1),
            ..Default::default()
        };

        let res = run_search(fen, config);
        assert!(res.bestmove().is_some());
    }

    #[test]
    fn test_go_nodes() {
        let fen = FEN_KIWIPETE;

        let node_limits = [0, 1, 10, 17, 126, 192, 1748, 182048, 1928392];

        for max_nodes in node_limits {
            let config = SearchConfig {
                max_nodes,
                ..Default::default()
            };

            let res = run_search(fen, config);

            assert_eq!(res.nodes, max_nodes);
            assert!(res.depth < Ply::MAX); // Ensure the ID didn't loop forever.
        }
    }

    #[test]
    fn test_go_nodes_cutoff_search_still_gives_good_result() {
        // d5e6 is a good capture, but will lead to mate on the next iteration.
        let fen = "k6r/8/4q3/3P4/8/8/PP6/K7 w - - 0 1";

        // Loop until we reach a depth that does NOT think d5e6 is the best move
        let mut max_depth = Ply::ONE;
        let max_nodes = loop {
            let config = SearchConfig {
                max_depth,
                ..Default::default()
            };

            let res = run_search(fen, config);
            if res.bestmove().unwrap() != "d5e6" {
                break res.nodes;
            }

            max_depth += 1;
        };

        // Now run a search on that position, stopping *just* before we would have found a move better than d5e6
        let config = SearchConfig {
            max_nodes: max_nodes - 1,
            ..Default::default()
        };
        let res = run_search(fen, config);
        assert_eq!(res.bestmove().unwrap(), "d5e6");

        // Do the same, but stop just *after* the node count that lets us find a better move than d5e6
        let config = SearchConfig {
            max_nodes: max_nodes + 1,
            ..Default::default()
        };
        let res = run_search(fen, config);
        assert_ne!(res.bestmove().unwrap(), "d5e6");
    }
}
