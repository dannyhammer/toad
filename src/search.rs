/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{
    fmt::{self, Debug},
    marker::PhantomData,
    ops::Deref,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};

use uci_parser::{UciInfo, UciResponse, UciSearchOptions};

use crate::{
    tune, Color, Game, LogLevel, LoggingLevel, Move, MoveList, Piece, PieceKind, Position, Score,
    Square, TTable, TTableEntry, Variant, ZobristKey,
};

/// Maximum depth that can be searched
pub const MAX_DEPTH: u8 = u8::MAX / 2;

/// Minium depth at which null move pruning can be applied.
const MIN_NMP_DEPTH: u8 = tune::min_nmp_depth!();

/// Value to subtract from `depth` when applying null move pruning.
const NMP_REDUCTION_VALUE: u8 = tune::nmp_reduction_value!();

/// MAximum depth at which to apply reverse futility pruning.
const MAX_RFP_DEPTH: u8 = tune::max_rfp_depth!();

/// Minimum depth at which to apply late move reductions.
const MIN_LMR_DEPTH: u8 = tune::min_lmr_depth!();

/// Minimum moves that must be made before late move reductions can be applied.
const MIN_LMR_MOVES: usize = tune::min_lmr_moves!();

/// Represents a window around a search result to act as our a/b bounds.
#[derive(Debug)]
struct AspirationWindow {
    /// Lower bound of the window
    alpha: Score,

    /// Upper bound of the window
    beta: Score,

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
    fn delta(depth: u8) -> Score {
        let initial_delta = tune::initial_aspiration_window_delta!();

        let min_delta = tune::min_aspiration_window_delta!();

        // Gradually decrease the window size from `8*init` to `min`
        let delta = ((initial_delta << 3) / depth as i32).max(min_delta);

        Score(delta)
    }

    /// Creates a new [`AspirationWindow`] centered around `score`.
    #[inline(always)]
    fn new(score: Score, depth: u8) -> Self {
        // If the score is mate, we expect search results to fluctuate, so set the windows to infinite.
        // Also, we only want to use aspiration windows after certain depths, so check that, too.
        let (alpha, beta) = if depth < tune::min_aspiration_window_depth!() || score.is_mate() {
            (-Score::INF, Score::INF)
        } else {
            // Otherwise we build a window around the provided score.
            let delta = Self::delta(depth);
            (
                (score - delta).max(-Score::INF),
                (score + delta).min(Score::INF),
            )
        };

        Self {
            alpha,
            beta,
            alpha_fails: 0,
            beta_fails: 0,
        }
    }

    /// Widens the window's `alpha` bound, expanding it downwards.
    ///
    /// This also resets the `beta` bound to `(alpha + beta) / 2`
    #[inline(always)]
    fn widen_down(&mut self, score: Score, depth: u8) {
        // Compute a gradually-increasing delta
        let delta = Self::delta(depth) * (1 << (self.alpha_fails + 1));

        // By convention, we widen both bounds on a fail low.
        self.beta = ((self.alpha + self.beta) / 2).min(Score::INF);
        self.alpha = (score - delta).max(-Score::INF);

        // Increase number of failures
        self.alpha_fails += 1;
    }

    /// Widens the window's `beta` bound, expanding it upwards.
    #[inline(always)]
    fn widen_up(&mut self, score: Score, depth: u8) {
        // Compute a gradually-increasing delta
        let delta = Self::delta(depth) * (1 << (self.beta_fails + 1));

        // Widen the beta bound
        self.beta = (score + delta).min(Score::INF);

        // Increase number of failures
        self.beta_fails += 1;
    }

    /// Returns `true` if `score` fails low, meaning it is below `alpha` and the window must be expanded downwards.
    #[inline(always)]
    fn fails_low(&self, score: Score) -> bool {
        self.alpha != -Score::INF && score <= self.alpha
    }

    /// Returns `true` if `score` fails high, meaning it is above `beta` and the window must be expanded upwards.
    #[inline(always)]
    fn fails_high(&self, score: Score) -> bool {
        self.beta != Score::INF && score >= self.beta
    }
}

/// The result of a search, containing the best move found, score, and total nodes searched.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SearchResult {
    /// Number of nodes searched.
    pub nodes: u64,

    /// Best move found during the search.
    pub bestmove: Option<Move>,

    /// Evaluation of the position after `bestmove` is made.
    pub score: Score,

    // The depth of the search that produced this result.
    pub depth: u8,
}

impl Default for SearchResult {
    /// A default search result should initialize to a *very bad* value,
    /// since there isn't a move to play.
    #[inline(always)]
    fn default() -> Self {
        Self {
            nodes: 0,
            bestmove: None,
            score: -Score::INF,
            depth: 1,
        }
    }
}

/// Configuration variables for executing a [`Search`].
#[derive(Debug, Clone, Copy)]
pub struct SearchConfig {
    /// Maximum depth to execute the search.
    pub max_depth: u8,

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
            config.max_depth = depth as u8;
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
            let (time, inc) = if game.side_to_move().is_white() {
                (options.wtime, options.winc)
            } else {
                (options.btime, options.binc)
            };

            // Only calculate timeouts if a time was provided
            if let Some(time) = time {
                let inc = inc.unwrap_or(Duration::ZERO) / tune::time_inc_divisor!();

                config.soft_timeout = time / tune::soft_timeout_divisor!() + inc;
                config.hard_timeout = time / tune::hard_timeout_divisor!() + inc;
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
            max_depth: MAX_DEPTH,
            max_nodes: u64::MAX,
            starttime: Instant::now(),
            soft_timeout: Duration::MAX,
            hard_timeout: Duration::MAX,
        }
    }
}

/// Stores bonuses and penalties for moving a piece to a square.
///
/// Used to keep track of good/bad moves found during search.
#[derive(Debug)]
pub struct HistoryTable([[Score; Square::COUNT]; Piece::COUNT]);

impl HistoryTable {
    /// Clear the history table, removing all scores.
    #[inline(always)]
    pub fn clear(&mut self) {
        *self = Self::default();
    }

    /// Applies a bonus based on the history heuristic for the move.
    ///
    /// Uses the "history gravity" formula from <https://www.chessprogramming.org/History_Heuristic#History_Bonuses>
    #[inline(always)]
    fn update<V: Variant>(&mut self, game: &Game<V>, mv: &Move, bonus: Score) {
        // Safety: This is a move. There *must* be a piece at `from`.
        let piece = game.piece_at(mv.from()).unwrap();
        let to = mv.to();
        let current_score = self.0[piece][to];
        let clamped_bonus = bonus.clamp(-Score::MAX_HISTORY, Score::MAX_HISTORY);

        // History gravity formula
        self.0[piece][to] +=
            clamped_bonus - current_score * clamped_bonus.abs() / Score::MAX_HISTORY;
    }
}

impl Default for HistoryTable {
    #[inline(always)]
    fn default() -> Self {
        Self([[Score::BASE_MOVE_SCORE; Square::COUNT]; Piece::COUNT])
    }
}

impl Deref for HistoryTable {
    type Target = [[Score; Square::COUNT]; Piece::COUNT];
    #[inline(always)]
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

/// Executes a search on the provided game at a specified depth.
pub struct Search<'a, const LOG: u8, V> {
    /// Number of nodes searched.
    nodes: u64,

    /// An atomic flag to determine if the search should be cancelled at any time.
    ///
    /// If this is ever `false`, the search must exit as soon as possible.
    is_searching: Arc<AtomicBool>,

    /// Configuration variables for this instance of the search.
    config: SearchConfig,

    /// Previous positions encountered during search.
    prev_positions: Vec<Position>,

    /// Transposition table used to cache information during search.
    ttable: &'a mut TTable,

    /// Storage for moves that cause a beta-cutoff during search.
    history: &'a mut HistoryTable,

    /// Marker for what variant of Chess is being played
    variant: PhantomData<&'a V>,
}

impl<'a, const LOG: u8, V: Variant> Search<'a, LOG, V> {
    /// Construct a new [`Search`] instance to execute.
    #[inline(always)]
    pub fn new(
        is_searching: Arc<AtomicBool>,
        config: SearchConfig,
        prev_positions: Vec<Position>,
        ttable: &'a mut TTable,
        history: &'a mut HistoryTable,
    ) -> Self {
        Self {
            nodes: 0,
            is_searching,
            config,
            prev_positions,
            ttable,
            history,
            variant: PhantomData,
        }
    }

    /// Start the search on the supplied [`Game`], returning a [`SearchResult`].
    ///
    /// This is the entrypoint of the search, and prints UCI info before starting iterative deepening.
    /// and concluding by sending the `bestmove` message and exiting.
    #[inline(always)]
    pub fn start(mut self, game: &Game<V>) -> SearchResult {
        if LOG.allows(LogLevel::Debug) {
            self.send_string(format!("Starting search on {:?}", game.to_fen()));

            let soft = self.config.soft_timeout.as_millis();
            let hard = self.config.hard_timeout.as_millis();
            let nodes = self.config.max_nodes;
            let depth = self.config.max_depth;

            if soft < Duration::MAX.as_millis() {
                self.send_string(format!("Soft timeout := {soft}ms"));
            }
            if hard < Duration::MAX.as_millis() {
                self.send_string(format!("Hard timeout := {hard}ms"));
            }
            if nodes < u64::MAX {
                self.send_string(format!("Max nodes := {nodes} nodes"));
            }
            if depth < MAX_DEPTH {
                self.send_string(format!("Max depth := {depth}"));
            }
        }

        let res = self.iterative_deepening(game);

        if LOG.allows(LogLevel::Debug) {
            let hits = self.ttable.hits;
            let accesses = self.ttable.accesses;
            let hit_rate = hits as f32 / accesses as f32 * 100.0;
            let collisions = self.ttable.collisions;
            let info = format!("TT stats: {hits} hits / {accesses} accesses ({hit_rate:.2}% hit rate), {collisions} collisions");
            self.send_string(info);
        }

        // Search has ended; send bestmove
        if LOG.allows(LogLevel::Info) {
            self.send_response(UciResponse::BestMove {
                bestmove: res.bestmove.map(V::fmt_move),
                ponder: None,
            });
        }

        // Search has concluded, alert other thread(s) that we are no longer searching
        self.is_searching.store(false, Ordering::Relaxed);

        res
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

    /// Sends UCI info about the conclusion of this search.
    #[inline(always)]
    fn send_end_of_search_info(&self, result: &SearchResult) {
        let elapsed = self.config.starttime.elapsed();

        self.send_info(
            UciInfo::new()
                .depth(result.depth)
                .nodes(self.nodes)
                .score(result.score)
                .nps((self.nodes as f32 / elapsed.as_secs_f32()).trunc())
                .time(elapsed.as_millis())
                .pv(result.bestmove.map(V::fmt_move)),
        );
    }

    /// Helper to send a [`UciInfo`] containing only a `string` message to `stdout`.
    #[inline(always)]
    fn send_string<T: fmt::Display>(&self, string: T) {
        self.send_response(UciResponse::info_string(string));
    }

    /// Performs [iterative deepening](https://www.chessprogramming.org/Iterative_Deepening) (ID) on the Search's position.
    ///
    /// ID is a basic time management strategy for engines.
    /// It involves performing a search at depth `n`, then, if there is enough time remaining, performing a search at depth `n + 1`.
    /// On it's own, ID does not improve performance, because we are wasting work by re-running searches at low depth.
    /// However, with features such as move ordering, a/b pruning, and aspiration windows, ID enhances performance.
    ///
    /// After each iteration, we check if we've exceeded our `soft_timeout` and, if we haven't, we run a search at a greater depth.
    fn iterative_deepening(&mut self, game: &Game<V>) -> SearchResult {
        // Initialize `bestmove` to the first move available
        let mut result = SearchResult {
            bestmove: game.get_legal_moves().first().copied(),
            ..Default::default()
        };

        /****************************************************************************************************
         * Iterative Deepening loop
         ****************************************************************************************************/

        // The actual Iterative Deepening loop
        'iterative_deepening: while self.config.starttime.elapsed() < self.config.soft_timeout
            && self.is_searching.load(Ordering::Relaxed)
            && result.depth <= self.config.max_depth
        {
            // Create a new aspiration window for this search
            let mut window = AspirationWindow::new(result.score, result.depth);

            // Get a score from the a/b search while using aspiration windows
            let score = 'aspiration_window: loop {
                // Start a new search at the current depth
                let score = self.negamax::<true>(game, result.depth, 0, window.alpha, window.beta);

                // If the score fell outside of the aspiration window, widen it gradually
                if window.fails_low(score) {
                    window.widen_down(score, result.depth);
                } else if window.fails_high(score) {
                    window.widen_up(score, result.depth);
                } else {
                    // Otherwise, the window is OK and we can use the score
                    break 'aspiration_window score;
                }

                // If we've ran out of time, we shouldn't update the score, because the last search iteration was forcibly cancelled.
                // Instead, we should break out of the ID loop, using the result from the previous iteration
                if self.search_cancelled() {
                    if LOG.allows(LogLevel::Debug) {
                        if let Some(bestmove) = self.get_tt_bestmove(game.key()) {
                            self.send_string(format!(
                                "Search cancelled during depth {} while evaluating {} with score {score}",
                                result.depth,
                                V::fmt_move(bestmove),
                                ));
                        } else {
                            self.send_string(format!(
                            "Search cancelled during depth {} with score {score} and no bestmove",
                            result.depth,
                        ));
                        }
                    }
                    break 'iterative_deepening;
                }
            };

            /****************************************************************************************************
             * Update current best score
             ****************************************************************************************************/

            // Otherwise, we need to update the "best" result with the results from the new search
            result.score = score;

            // Get the bestmove from the TTable
            result.bestmove = self.ttable.get(&game.key()).map(|entry| entry.bestmove);

            // Send search info to the GUI
            if LOG.allows(LogLevel::Info) {
                self.send_end_of_search_info(&result);
            }

            // Increase the depth for the next iteration
            result.depth += 1;
        }

        // Transfer the node count
        result.nodes += self.nodes;

        // ID loop has concluded (either by finishing or timing out),
        // so we return the result from the last successfully-completed search.
        result
    }

    /// Primary location of search logic.
    ///
    /// Uses the [negamax](https://www.chessprogramming.org/Negamax) algorithm in a [fail soft](https://www.chessprogramming.org/Alpha-Beta#Negamax_Framework) framework.
    fn negamax<const PV: bool>(
        &mut self,
        game: &Game<V>,
        depth: u8,
        ply: i32,
        mut alpha: Score,
        beta: Score,
    ) -> Score {
        // If we've reached a terminal node, evaluate the current position
        if depth == 0 {
            return self.quiescence(game, ply, alpha, beta);
        }

        // If there are no legal moves, it's either mate or a draw.
        let mut moves = game.get_legal_moves();
        if moves.is_empty() {
            let score = if game.is_in_check() {
                // Offset by ply to prefer earlier mates
                -Score::MATE + ply
            } else {
                // Drawing is better than losing
                Score::DRAW
            };

            return score;
        }

        // If we CAN prune this node, do so
        if let Some(score) = self.node_pruning_score::<PV>(game, depth, ply, beta) {
            return score;
        }

        // Sort moves so that we look at "promising" ones first
        let tt_move = self.get_tt_bestmove(game.key());
        moves.sort_by_cached_key(|mv| self.score_move(game, mv, tt_move));

        // Start with a *really bad* initial score
        let mut best = -Score::INF;
        let mut bestmove = moves[0]; // Safe because we guaranteed `moves` to be nonempty above
        let original_alpha = alpha;

        /****************************************************************************************************
         * Primary move loop
         ****************************************************************************************************/

        for (i, mv) in moves.iter().enumerate() {
            // Copy-make the new position
            let new = game.with_move_made(*mv);
            let mut score = Score::DRAW;

            if !self.is_draw(&new) {
                // Append the move onto the history
                self.prev_positions.push(*new.position());

                let new_depth = depth - 1;

                /****************************************************************************************************
                 * Late Move Reductions: https://www.chessprogramming.org/Late_Move_Reductions
                 ****************************************************************************************************/
                if depth >= MIN_LMR_DEPTH && i >= MIN_LMR_MOVES {
                    // By default, LMR reduces by a depth of 2
                    let mut lmr_reduction = 2;

                    // Increase/decrease the reduction based on current conditions
                    // lmr_reduction += something;
                    lmr_reduction -= new.is_in_check() as u8;

                    // Reduced depth should never exceed `new_depth` and should never be less than `1`.
                    let reduced_depth = (new_depth - lmr_reduction).max(1).min(new_depth);

                    // Search at a reduced depth with a null window
                    score =
                        -self.negamax::<false>(&new, reduced_depth, ply + 1, -alpha - 1, -alpha);

                    // If that failed *high* (raised alpha), re-search at the full depth with the null window
                    if score > alpha && reduced_depth < new_depth {
                        score =
                            -self.negamax::<false>(&new, new_depth, ply + 1, -alpha - 1, -alpha);
                    }
                } else if !PV || i > 0 {
                    // All non-PV nodes get searched with a null window
                    score = -self.negamax::<false>(&new, new_depth, ply + 1, -alpha - 1, -alpha);
                }

                /****************************************************************************************************
                 * Principal Variation Search: https://en.wikipedia.org/wiki/Principal_variation_search#Pseudocode
                 ****************************************************************************************************/
                // If searching the PV, or if a reduced search failed *high*, we search with a full depth and window
                if PV && (i == 0 || score > alpha) {
                    score = -self.negamax::<PV>(&new, new_depth, ply + 1, -beta, -alpha);
                }

                // We've now searched this node
                self.nodes += 1;

                // Pop the move from the history
                self.prev_positions.pop();
            }

            /****************************************************************************************************
             * Score evaluation & bounds adjustments
             ****************************************************************************************************/

            // If we've found a better move than our current best, update the results
            if score > best {
                best = score;

                if score > alpha {
                    alpha = score;
                    // PV found
                    bestmove = *mv;
                }

                // Fail soft beta-cutoff.
                if score >= beta {
                    /****************************************************************************************************
                     * History Heuristic
                     ****************************************************************************************************/
                    // Simple bonus based on depth
                    let bonus = Score::HISTORY_MULTIPLIER * depth as i32 - Score::HISTORY_OFFSET;

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

            // Check if we can continue searching
            if self.search_cancelled() {
                break;
            }
        }

        // Save this node to the TTable
        self.save_to_tt(game.key(), bestmove, best, original_alpha, beta, depth, ply);

        best
    }

    /// Quiescence Search (QSearch)
    ///
    /// A search that looks at only possible captures and capture-chains.
    /// This is called when [`Search::negamax`] reaches a depth of 0, and has no recursion limit.
    fn quiescence(&mut self, game: &Game<V>, _ply: i32, mut alpha: Score, beta: Score) -> Score {
        // Evaluate the current position, to serve as our baseline
        let stand_pat = game.eval();

        // Beta cutoff; this position is "too good" and our opponent would never let us get here
        if stand_pat >= beta {
            return beta;
        } else if stand_pat > alpha {
            alpha = stand_pat;
        }

        // Generate only the legal captures
        // TODO: Is there a more concise way of doing this?
        // The `game.into_iter().only_captures()` doesn't cover en passant...
        let mut captures = game
            .get_legal_moves()
            .into_iter()
            .filter(Move::is_capture)
            .collect::<MoveList>();

        // Can't check for mates in normal qsearch, since we're not looking at *all* moves.
        // So, if there are no captures available, just return the current evaluation.
        if captures.is_empty() {
            return stand_pat;
        }

        let tt_move = self.get_tt_bestmove(game.key());
        captures.sort_by_cached_key(|mv| self.score_move(game, mv, tt_move));

        let mut best = stand_pat;
        // let mut bestmove = captures[0]; // Safe because we ensured `captures` is not empty
        // let original_alpha = alpha;

        /****************************************************************************************************
         * Primary move loop
         ****************************************************************************************************/

        for mv in captures {
            // Copy-make the new position
            let new = game.with_move_made(mv);
            let score;

            // Normally, repetitions can't occur in QSearch, because captures are irreversible.
            // However, some QSearch extensions (quiet TT moves, all moves when in check, etc.) may be reversible.
            if self.is_draw(&new) {
                score = Score::DRAW;
            } else {
                self.prev_positions.push(*new.position());

                score = -self.quiescence(&new, _ply + 1, -beta, -alpha);
                self.nodes += 1; // We've now searched this node

                self.prev_positions.pop();
            }

            /****************************************************************************************************
             * Score evaluation & bounds adjustments
             ****************************************************************************************************/
            // If we've found a better move than our current best, update our result
            if score > best {
                best = score;

                if score > alpha {
                    alpha = score;

                    // PV found
                    // bestmove = mv;
                }

                // Fail soft beta-cutoff.
                if score >= beta {
                    break;
                }
            }

            // Check if we can continue searching
            if self.search_cancelled() {
                break;
            }
        }

        // Save this node to the TTable
        // self.save_to_tt(game.key(), bestmove, best, original_alpha, beta, 0, ply);

        best // fail-soft
    }

    /// Checks if we've exceeded any conditions that would warrant the search to end.
    #[inline(always)]
    fn search_cancelled(&self) -> bool {
        // Condition 1: We've exceeded the hard limit of our allotted search time
        self.config.starttime.elapsed() >= self.config.hard_timeout ||
        // Condition 2: The search was stopped by an external factor, like the `stop` command
        !self.is_searching.load(Ordering::Relaxed) ||
        // Condition 3: We've exceeded the maximum amount of nodes we're allowed to search
        self.nodes >= self.config.max_nodes
    }

    /// Checks if `game` is a repetition, comparing it to previous positions
    #[inline(always)]
    fn is_repetition(&self, game: &Game<V>) -> bool {
        // We can skip the previous position, because there's no way it can be a repetition
        for prev in self.prev_positions.iter().rev().skip(1) {
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
    #[allow(clippy::too_many_arguments)]
    fn save_to_tt(
        &mut self,
        key: ZobristKey,
        bestmove: Move,
        score: Score,
        alpha: Score,
        beta: Score,
        depth: u8,
        ply: i32,
    ) {
        let entry = TTableEntry::new(key, bestmove, score, alpha, beta, depth, ply);
        let old = self.ttable.store(entry);

        if LOG.allows(LogLevel::Debug) {
            // If a previous entry existed and had a *different* key, this was a collision
            if old.is_some_and(|old| old.key != key) {
                self.ttable.collisions += 1;
            }
        }
    }

    /// Gets the bestmove for the provided position from the TTable, if it exists.
    #[inline(always)]
    fn get_tt_bestmove(&mut self, key: ZobristKey) -> Option<Move> {
        let mv = self.ttable.get(&key).map(|entry| entry.bestmove);

        if LOG.allows(LogLevel::Debug) {
            // Regardless whether this was a hit, it was still an access
            self.ttable.accesses += 1;

            // If a move was found, this was a hit
            if mv.is_some() {
                self.ttable.hits += 1;
            }
        }

        mv
    }

    /// Applies a score to the provided move, intended to be used when ordering moves during search.
    #[inline(always)]
    fn score_move(&self, game: &Game<V>, mv: &Move, tt_move: Option<Move>) -> Score {
        // TT move should be looked at first, so assign it the best possible score and immediately exit.
        if tt_move.is_some_and(|tt_mv| tt_mv == *mv) {
            return Score(i32::MIN);
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
    fn node_pruning_score<const PV: bool>(
        &mut self,
        game: &Game<V>,
        depth: u8,
        ply: i32,
        beta: Score,
    ) -> Option<Score> {
        // Cannot prune anything in a PV node or if we're in check
        if PV || game.is_in_check() {
            return None;
        }

        /****************************************************************************************************
         * Reverse Futility Pruning: https://www.chessprogramming.org/Reverse_Futility_Pruning
         ****************************************************************************************************/
        // If our static eval is too good (better than beta), we can prune this branch.
        // Multiplying our margin by depth makes this pruning process less risky for higher depths.
        let rfp_score = game.eval() - Score::RFP_MARGIN * depth as i32;
        if depth <= MAX_RFP_DEPTH && rfp_score >= beta {
            return Some(rfp_score);
        }

        /****************************************************************************************************
         * Null Move Pruning: https://www.chessprogramming.org/Null_Move_Pruning
         ****************************************************************************************************/
        // If the last move did not increment the fullmove, but *did* increment the halfmove, it was a nullmove
        let last_move_was_nullmove = self.prev_positions.last().is_some_and(|pos| {
            pos.fullmove() == game.fullmove() && pos.halfmove() == game.halfmove() + 1
        });

        // All pieces that are not Kings or Pawns
        let non_king_pawn_material =
            game.occupied() ^ game.kind(PieceKind::Pawn) ^ game.kind(PieceKind::King);

        let can_perform_nmp = depth >= MIN_NMP_DEPTH // Can't play nullmove under a certain depth
        && !last_move_was_nullmove // Can't play two nullmoves in a row
        && non_king_pawn_material.is_nonempty(); // Can't play nullmove if insufficient material (only Kings and Pawns)

        if can_perform_nmp {
            let null_game = game.with_nullmove_made();
            self.prev_positions.push(*null_game.position());

            // Search at a reduced depth with a zero-window
            let nmp_depth = depth - NMP_REDUCTION_VALUE;
            let score = -self.negamax::<PV>(&null_game, nmp_depth, ply + 1, -beta, -beta + 1);

            self.prev_positions.pop();

            // If making the nullmove produces a cutoff, we can assume that a full-depth search would also produce a cutoff
            if score >= beta {
                return Some(score);
            }
        }

        // If no pruning technique was possible, return no score
        None
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
        Search::<{ LogLevel::None as u8 }, Standard>::new(
            is_searching,
            config,
            Default::default(),
            &mut ttable,
            &mut history,
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
            max_depth: 2,
            ..Default::default()
        };

        let res = ensure_is_mate_in(fen, config, 1);
        assert_eq!(res.bestmove.unwrap(), "b6a7")
    }

    #[test]
    fn test_black_mated_in_1() {
        let fen = "1k6/8/KQ6/2Q5/8/8/8/8 b - - 0 1";
        let config = SearchConfig {
            max_depth: 3,
            ..Default::default()
        };

        let res = ensure_is_mate_in(fen, config, -1);
        assert_eq!(res.bestmove.unwrap(), "b8a8")
    }

    #[test]
    fn test_stalemate() {
        let fen = "k7/8/KQ6/8/8/8/8/8 b - - 0 1";
        let config = SearchConfig::default();

        let res = run_search(fen, config);
        assert!(res.bestmove.is_none());
        assert_eq!(res.score, Score::DRAW);
    }

    #[test]
    fn test_obvious_capture_promote() {
        // Pawn should take queen and also promote to queen
        let fen = "3q1n2/4P3/8/8/8/8/k7/7K w - - 0 1";
        let config = SearchConfig {
            max_depth: 1,
            ..Default::default()
        };

        let res = run_search(fen, config);
        assert_eq!(res.bestmove.unwrap(), "e7d8q");
    }

    #[test]
    fn test_quick_search_finds_move() {
        // If *any* legal move is available, it should be found, regardless of how much time was given.
        let fen = FEN_STARTPOS;
        let config = SearchConfig {
            soft_timeout: Duration::from_millis(0),
            hard_timeout: Duration::from_millis(0),
            ..Default::default()
        };

        let res = run_search(fen, config);
        assert!(res.bestmove.is_some());
    }
}
