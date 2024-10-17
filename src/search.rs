/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{
    fmt,
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};

use chessie::{Game, Move, MoveList, PieceKind, Position, ZobristKey};
use uci_parser::{UciInfo, UciResponse, UciSearchOptions};

use crate::{tune, value_of, Evaluator, Score, TTable, TTableEntry};

/// Maximum depth that can be searched
pub const MAX_DEPTH: u8 = u8::MAX / 2;

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
    pub fn new(options: UciSearchOptions, game: &Game) -> Self {
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

/// Executes a search on the provided game at a specified depth.
pub struct Search<'a> {
    /// Number of nodes searched.
    nodes: u64,

    /// An atomic flag to determine if the search should be cancelled at any time.
    ///
    /// If this is ever `false`, the search must exit as soon as possible.
    is_searching: Arc<AtomicBool>,

    /// Configuration variables for this instance of the search.
    config: SearchConfig,

    /// Previous positions encountered during search.
    history: Vec<Position>,

    /// Transposition table used to cache information during search.
    ttable: &'a mut TTable,
}

impl<'a> Search<'a> {
    /// Construct a new [`Search`] instance to execute.
    #[inline(always)]
    pub fn new(
        is_searching: Arc<AtomicBool>,
        config: SearchConfig,
        history: Vec<Position>,
        ttable: &'a mut TTable,
    ) -> Self {
        Self {
            nodes: 0,
            is_searching,
            config,
            history,
            ttable,
        }
    }

    /// Start the search on the supplied [`Game`], returning a [`SearchResult`].
    ///
    /// This is the entrypoint of the search, and prints UCI info before calling [`Self::iterative_deepening`],
    /// and concluding by sending the `bestmove` message and exiting.
    #[inline(always)]
    pub fn start<const DEBUG: bool>(mut self, game: &Game) -> SearchResult {
        if DEBUG {
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

        let res = self.iterative_deepening::<DEBUG>(game);

        if DEBUG {
            let hits = self.ttable.hits;
            let accesses = self.ttable.accesses;
            let hit_rate = hits as f32 / accesses as f32 * 100.0;
            let collisions = self.ttable.collisions;
            let info = format!("TT stats: {hits} hits / {accesses} accesses ({hit_rate:.2}% hit rate), {collisions} collisions");
            self.send_string(info);
        }

        // Search has ended; send bestmove
        self.send_response(UciResponse::BestMove {
            bestmove: res.bestmove,
            ponder: None,
        });

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
                .pv(result.bestmove),
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
    fn iterative_deepening<const DEBUG: bool>(&mut self, game: &Game) -> SearchResult {
        // Initialize `bestmove` to the first move available
        let mut result = SearchResult {
            bestmove: game.into_iter().next(),
            ..Default::default()
        };

        // Start at depth 1 because a search at depth 0 makes no sense
        let alpha = -Score::INF;
        let beta = Score::INF;

        /****************************************************************************************************
         * Iterative Deepening loop
         ****************************************************************************************************/

        // The actual Iterative Deepening loop
        while self.config.starttime.elapsed() < self.config.soft_timeout
            && self.is_searching.load(Ordering::Relaxed)
            && result.depth <= self.config.max_depth
        {
            // Start a new search at the current depth
            let score = self.negamax::<DEBUG, true>(game, result.depth, 0, alpha, beta);

            // If we've ran out of time, we shouldn't update the score, because the last search iteration was forcibly cancelled.
            // Instead, we should break out of the ID loop, using the result from the previous iteration
            if self.search_cancelled() {
                if DEBUG {
                    if let Some(bestmove) = self.get_tt_bestmove::<false>(game.key()) {
                        self.send_string(format!(
                                "Search cancelled during depth {} while evaluating {bestmove} with score {score}",
                                result.depth,
                                ));
                    } else {
                        self.send_string(format!(
                            "Search cancelled during depth {} with score {score} and no bestmove",
                            result.depth,
                        ));
                    }
                }
                break;
            }

            /****************************************************************************************************
             * Update current best score
             ****************************************************************************************************/

            // Otherwise, we need to update the "current" result with the results from the new search
            result.score = score;

            // Get the bestmove from the TTable
            // This is guaranteed to be a cache hit, so don't log it as such
            result.bestmove = self.get_tt_bestmove::<false>(game.key());

            // Send search info to the GUI
            self.send_end_of_search_info(&result);

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
    fn negamax<const DEBUG: bool, const PV: bool>(
        &mut self,
        game: &Game,
        depth: u8,
        ply: i32,
        mut alpha: Score,
        beta: Score,
    ) -> Score {
        // Regardless of how long we stay here, we've searched this node, so increment the counter.
        // TODO: Move this below the recursive call, so that PVS re-searches don't cause it to increment (and do the same for qsearch)
        self.nodes += 1;

        // If we've reached a terminal node, evaluate the current position
        if depth == 0 {
            return self.quiescence::<DEBUG>(game, ply, alpha, beta);
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

        // Sort moves so that we look at "promising" ones first
        let tt_move = self.get_tt_bestmove::<DEBUG>(game.key());
        moves.sort_by_cached_key(|mv| score_move(game, mv, tt_move));

        // Start with a *really bad* initial score
        let mut best = -Score::INF;
        let mut bestmove = moves[0]; // Safe because we guaranteed `moves` to be nonempty above
        let original_alpha = alpha;

        /****************************************************************************************************
         * Primary move loop
         ****************************************************************************************************/

        for (i, mv) in moves.into_iter().enumerate() {
            // Copy-make the new position
            let new = game.with_move_made(mv);
            let mut score;

            // Determine the score of making this move
            if self.is_draw(&new) {
                score = Score::DRAW;
            } else {
                // Append the move onto the history
                self.history.push(*new.position());

                /****************************************************************************************************
                 * Principal Variation Search: https://en.wikipedia.org/wiki/Principal_variation_search#Pseudocode
                 ****************************************************************************************************/
                if i == 0 {
                    // Recurse on the principle variation
                    score = -self.negamax::<DEBUG, PV>(&new, depth - 1, ply + 1, -beta, -alpha);
                } else {
                    // Search with a null window
                    score =
                        -self.negamax::<DEBUG, false>(&new, depth - 1, ply + 1, -alpha - 1, -alpha);

                    // If it failed, perform a full re-search with the full a/b bounds
                    if alpha < score && score < beta {
                        score = -self.negamax::<DEBUG, PV>(&new, depth - 1, ply + 1, -beta, -alpha);
                    }
                };

                // Pop the move from the history
                self.history.pop();
            };

            /****************************************************************************************************
             * Score evaluation & bounds adjustments
             ****************************************************************************************************/

            // If we've found a better move than our current best, update the results
            if score > best {
                best = score;

                if score > alpha {
                    alpha = score;
                    // PV found
                    bestmove = mv;
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
        self.save_to_tt::<DEBUG>(game.key(), bestmove, best, original_alpha, beta, depth, ply);

        best
    }

    /// Quiescence Search (QSearch)
    ///
    /// A search that looks at only possible captures and capture-chains.
    /// This is called when [`Search::negamax`] reaches a depth of 0, and has no recursion limit.
    fn quiescence<const DEBUG: bool>(
        &mut self,
        game: &Game,
        ply: i32,
        mut alpha: Score,
        beta: Score,
    ) -> Score {
        // Evaluate the current position, to serve as our baseline
        let stand_pat = Evaluator::new(game).eval();

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

        // Everything before this point is the same as if we called `eval()` in Negamax
        // If we made it here, then we're examining this node
        self.nodes += 1;

        let tt_move = self.get_tt_bestmove::<DEBUG>(game.key());
        captures.sort_by_cached_key(|mv| score_move(game, mv, tt_move));

        let mut best = stand_pat;
        let mut bestmove = captures[0]; // Safe because we ensured `captures` is not empty
        let original_alpha = alpha;

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
                self.history.push(*new.position());

                score = -self.quiescence::<DEBUG>(&new, ply + 1, -beta, -alpha);

                self.history.pop();
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
                    bestmove = mv;
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

        // Save this node to the TTable if there isn't an entry here or the entry was found in another QSearch
        let tt_entry = self.ttable.get(&game.key());
        if tt_entry.is_none() || tt_entry.is_some_and(|entry| entry.depth > 0) {
            self.save_to_tt::<DEBUG>(game.key(), bestmove, best, original_alpha, beta, 0, ply);
        }

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
    fn is_repetition(&self, game: &Game) -> bool {
        // We can skip the previous position, because there's no way it can be a repetition
        for prev in self.history.iter().rev().skip(1) {
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
    fn is_draw(&self, game: &Game) -> bool {
        self.is_repetition(game)
            || game.can_draw_by_fifty()
            || game.can_draw_by_insufficient_material()
    }

    /// Saves the provided data to an entry in the TTable.
    #[allow(clippy::too_many_arguments)]
    fn save_to_tt<const DEBUG: bool>(
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

        if DEBUG {
            // If a previous entry existed and had a *different* key, this was a collision
            if old.is_some_and(|old| old.key != key) {
                self.ttable.collisions += 1;
            }
        }
    }

    /// Gets the bestmove for the provided position from the TTable, if it exists.
    fn get_tt_bestmove<const DEBUG: bool>(&mut self, key: ZobristKey) -> Option<Move> {
        let mv = self.ttable.get(&key).map(|entry| entry.bestmove);

        if DEBUG {
            // Regardless whether this was a hit, it was still an access
            self.ttable.accesses += 1;

            // If a move was found, this was a hit
            if mv.is_some() {
                self.ttable.hits += 1;
            }
        }

        mv
    }
}

/// Applies a score to the provided move, intended to be used when ordering moves during search.
#[inline(always)]
fn score_move(game: &Game, mv: &Move, tt_move: Option<Move>) -> Score {
    // TT move should be looked at first!
    if tt_move.is_some_and(|tt_mv| tt_mv == *mv) {
        return -Score::INF;
    }

    // Safe unwrap because we can't move unless there's a piece at `from`
    let kind = game.kind_at(mv.from()).unwrap();
    let mut score = Score(0);

    // Capturing a high-value piece with a low-value piece is a good idea
    if let Some(victim) = game.kind_at(mv.to()) {
        score += MVV_LVA[kind][victim];
    }

    -score // We're sorting, so a lower number is better
}

/// This table represents values for [MVV-LVA](https://www.chessprogramming.org/MVV-LVA) move ordering.
///
/// It is indexed by `[attacker][victim]`, and yields a "score" that is used when sorting moves.
///
/// The following table is produced:
/// ```text
///
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
const MVV_LVA: [[i32; PieceKind::COUNT]; PieceKind::COUNT] = {
    let mut matrix = [[0; PieceKind::COUNT]; PieceKind::COUNT];
    let count = PieceKind::COUNT;

    let mut attacker = 0;
    while attacker < count {
        let mut victim = 0;

        // The -1 here is to remove scores for capturing the King
        while victim < count - 1 {
            let atk = PieceKind::from_bits_unchecked(attacker as u8);
            let vtm = PieceKind::from_bits_unchecked(victim as u8);

            // Rustic's way of doing things; Arbitrary increasing numbers for capturing pairs
            // bench: 27609398 nodes 5716479 nps
            // let score = (victim * 10 + (count - attacker)) as i32;

            // Default MVV-LVA except that the King is assigned a value of 0 if he is attacking
            // bench: 27032804 nodes 8136592 nps
            let score = 10 * value_of(vtm) - value_of(atk);

            // If the attacker is the King, the score is half the victim's value.
            // This encourages the King to attack, but not as strongly as other pieces.
            // bench: 27107011 nodes 5647285 nps
            // let score = if attacker == count - 1 {
            //     value_of(vtm) / 2
            // } else {
            //     // Standard MVV-LVA computation
            //     10 * value_of(vtm) - value_of(atk)
            // };

            matrix[attacker][victim] = score;
            victim += 1;
        }
        attacker += 1;
    }
    matrix
};

/// Utility function to print the MVV-LVA table
#[allow(dead_code)]
pub fn print_mvv_lva_table() {
    print!("\nX   ");
    for victim in PieceKind::iter() {
        let v = victim.char().to_ascii_uppercase();

        print!("{v:<6}");
    }
    print!("\n +");
    for _ in PieceKind::iter() {
        print!("------");
    }
    println!("-+");
    for attacker in PieceKind::iter() {
        let a = attacker.char().to_ascii_uppercase();

        print!("{a}| ");
        for victim in PieceKind::iter() {
            let score = MVV_LVA[attacker][victim];
            print!("{score:<4}  ")
        }
        println!();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_search(fen: &str, config: SearchConfig) -> SearchResult {
        let is_searching = Arc::new(AtomicBool::new(true));
        let game = fen.parse().unwrap();

        let mut ttable = Default::default();
        let search = Search::new(is_searching, config, Default::default(), &mut ttable);

        search.start::<false>(&game)
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
        let fen = chessie::FEN_STARTPOS;
        let config = SearchConfig {
            soft_timeout: Duration::from_millis(0),
            hard_timeout: Duration::from_millis(0),
            ..Default::default()
        };

        let res = run_search(fen, config);
        assert!(res.bestmove.is_some());
    }
}
