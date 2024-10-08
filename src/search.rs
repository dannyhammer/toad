/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    time::{Duration, Instant},
};

use anyhow::{bail, Result};
use chessie::{Game, Move};
use uci_parser::{UciInfo, UciResponse, UciSearchOptions};

use crate::{Evaluator, Score};

/// Maximum depth that can be searched
pub const MAX_DEPTH: usize = 255;

/// The result of a search, containing the best move found, score, and total nodes searched.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SearchResult {
    /// Number of nodes searched.
    pub nodes: u64,

    /// Best move found during the search.
    pub bestmove: Option<Move>,

    /// Evaluation of the position after `bestmove` is made.
    pub score: Score,
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
        }
    }
}

/// Configuration variables for executing a [`Search`].
#[derive(Debug, Clone, Copy)]
pub struct SearchConfig {
    /// Maximum depth to execute the search.
    pub max_depth: usize,

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
            config.max_depth = depth as usize;
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

            let (time, inc) = (time.unwrap_or(Duration::MAX), inc.unwrap_or(Duration::ZERO));

            config.soft_timeout = time / 20 + inc / 2; // Soft Timeout: 5% of time remaining + 50% time increment
            config.hard_timeout = time / 5 + inc / 2; // Hard Timeout: 20% of time remaining + 50% time increment
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
    /// The game to search on.
    ///
    /// This game will be copied when moves are applied to it.
    game: &'a Game,

    /// The result of the search, updated as-needed during search.
    result: SearchResult,

    /// An atomic flag to determine if the search should be cancelled at any time.
    ///
    /// If this is ever `false`, the search will exit as soon as possible.
    is_searching: Arc<AtomicBool>,

    /// Configuration variables for this instance of the search.
    config: SearchConfig,
}

impl<'a> Search<'a> {
    /// Construct a new [`Search`] instance to execute on the provided [`Game`].
    #[inline(always)]
    pub fn new(game: &'a Game, is_searching: Arc<AtomicBool>, config: SearchConfig) -> Self {
        let result = SearchResult {
            // Initialize `bestmove` to the first move available
            bestmove: game.into_iter().next(),
            ..Default::default()
        };

        Self {
            game,
            result,
            is_searching,
            config,
        }
    }

    /// Start the search, returning its results if the search was successful.
    ///
    /// This is the entrypoint of the search, and prints UCI info before calling [`Self::iterative_deepening`],
    /// and concluding by sending the `bestmove` message and exiting.
    pub fn start(mut self) -> SearchResult {
        self.send_info(
            UciInfo::new().string(format!("Starting search on {:?}", self.game.to_fen(),)),
        );
        // println!(
        //     "info string Soft timeout {}ms",
        //     self.config.soft_timeout.as_millis()
        // );
        // println!(
        //     "info string Hard timeout {}ms",
        //     self.config.hard_timeout.as_millis()
        // );

        let res = self.iterative_deepening();
        // let res = self.random_move();

        // Search has ended; send bestmove
        let response = UciResponse::BestMove {
            bestmove: res.bestmove,
            ponder: None,
        };

        println!("{response}");

        // Search has concluded, alert other threads that we are no longer searching
        self.is_searching.store(false, Ordering::Relaxed);

        res
    }

    #[inline(always)]
    fn send_info(&self, info: UciInfo) {
        let resp = UciResponse::<String>::Info(Box::new(info));
        println!("{resp}");
    }

    /// Performs [iterative deepening](https://www.chessprogramming.org/Iterative_Deepening) (ID) on the Search's position.
    ///
    /// ID is a basic time management strategy for engines.
    /// It involves performing a search at depth `n`, then, if there is enough time remaining, performing a search at depth `n + 1`.
    /// On it's own, ID does not improve performance, because we are wasting work by re-running searches at low depth.
    /// However, with features such as move ordering, a/b pruning, and aspiration windows, ID enhances performance.
    ///
    /// After each iteration, we check if we've exceeded our `soft_timeout` and, if we haven't, we run a search at a greater depth.
    fn iterative_deepening(&mut self) -> SearchResult {
        // Start at depth 1 because a search at depth 0 makes no sense
        let mut depth = 1;

        // The actual Iterative Deepening loop
        while self.config.starttime.elapsed() < self.config.soft_timeout
            && self.is_searching.load(Ordering::Relaxed)
            && depth <= self.config.max_depth
        {
            // Reset score after each search, as there is no way to know what the bounds are.
            // We can use the score from the previous depth's search in Aspiration Windows: https://www.chessprogramming.org/Aspiration_Windows
            self.result.score = -Score::INF;

            // If the search returned an error, it was cancelled, so exit the iterative deepening loop.
            match self.negamax(*self.game, depth, 0, -Score::INF, Score::INF) {
                // A new result was found, so update our best so far
                Ok((bestmove, score)) => {
                    self.result.bestmove = bestmove;
                    self.result.score = score;
                }

                // The search was cancelled, so exit the ID loop.
                Err(e) => {
                    self.send_info(UciInfo::new().string(format!(
                        "Search cancelled during depth {depth} while evaluating {} with score {}: {e}",
                        self.result.bestmove.unwrap_or_default(),
                        self.result.score
                    )));

                    self.send_info(UciInfo::new().string(format!(
                        "Falling back to result from depth {}: {} with score {}",
                        depth - 1,
                        self.result.bestmove.unwrap_or_default(),
                        self.result.score,
                    )));

                    break;
                }
            }

            // Send search info to the GUI
            let elapsed = self.config.starttime.elapsed();
            self.send_info(
                UciInfo::new()
                    .depth(depth)
                    .nodes(self.result.nodes)
                    .score(self.result.score.into_uci())
                    .nps((self.result.nodes as f32 / elapsed.as_secs_f32()).trunc())
                    .time(elapsed.as_millis()),
            );

            // Increase the depth for the next iteration
            depth += 1;
        }

        // ID loop has concluded (either by finishing or timing out),
        //  so we return the result from the last successfully-completed search.
        self.result
    }

    /// Primary location of search logic.
    ///
    /// Uses the [negamax](https://www.chessprogramming.org/Negamax) algorithm in a [fail soft](https://www.chessprogramming.org/Alpha-Beta#Negamax_Framework) framework.
    fn negamax(
        &mut self,
        game: Game,
        depth: usize,
        ply: i32,
        mut alpha: Score,
        beta: Score,
    ) -> Result<(Option<Move>, Score)> {
        // Regardless of how long we stay here, we've searched this node, so increment the counter.
        self.result.nodes += 1;

        // If we've reached a terminal node, evaluate the position
        if depth == 0 {
            return Ok((None, Evaluator::new(&game).eval()));
        }

        // If there are no legal moves, it's either mate or a draw.
        let moves = game.get_legal_moves();
        if moves.is_empty() {
            let score = if game.is_in_check() {
                // Offset by ply to prefer earlier mates
                -Score::MATE + ply
            } else {
                // Drawing is better than losing
                Score::DRAW
            };

            return Ok((None, score));
        }

        // Start with a *really bad* initial score
        let mut best = -Score::INF;
        let mut bestmove = moves.first().copied(); // Safe because we guaranteed `moves` to be nonempty above

        for mv in moves {
            // We need to check several conditions periodically while searching, to make sure we can continue
            // Condition 1: We've exceeded the hard limit of our allotted search time
            if self.config.starttime.elapsed() >= self.config.hard_timeout {
                let ms = self.config.hard_timeout.as_millis();
                bail!("exceeded hard timeout of {ms}ms",);
            } else
            // Condition 2: The search was stopped by an external factor, like the `stop` command
            if !self.is_searching.load(Ordering::Relaxed) {
                bail!("cancelled by external command");
            } else
            // Condition 3: We've exceeded the maximum amount of nodes we're allowed to search
            if self.result.nodes >= self.config.max_nodes {
                let nodes = self.config.max_nodes;
                bail!("exceeded node allowance of {nodes} nodes");
            }

            // Copy-make the new position
            let new_game = game.with_move_made(mv);

            // Recurse
            let score = -self.negamax(new_game, depth - 1, ply + 1, -beta, -alpha)?.1;

            // If we've found a better move than our current best, update the results
            if score > best {
                best = score;

                if score > alpha {
                    alpha = score;
                    // PV found
                    bestmove = Some(mv);
                }

                // Fail soft beta-cutoff.
                if score >= beta {
                    break;
                }
            }
        }

        Ok((bestmove, best))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn ensure_is_mate_in(fen: &str, config: SearchConfig, moves: i32) -> SearchResult {
        let is_searching = Arc::new(AtomicBool::new(true));
        let game = fen.parse().unwrap();

        let search = Search::new(&game, is_searching, config);

        let res = search.start();
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

        let is_searching = Arc::new(AtomicBool::new(true));
        let game = fen.parse().unwrap();

        let search = Search::new(&game, is_searching, config);

        let res = search.start();
        assert!(res.bestmove.is_none());
        assert_eq!(res.score, Score::DRAW,);
    }

    #[test]
    fn test_obvious_capture_promote() {
        // Pawn should take queen and also promote to queen
        let fen = "3q1n2/4P3/8/8/8/8/k7/7K w - - 0 1";
        let config = SearchConfig {
            max_depth: 1,
            ..Default::default()
        };

        let is_searching = Arc::new(AtomicBool::new(true));
        let game = fen.parse().unwrap();

        let search = Search::new(&game, is_searching, config);

        let res = search.start();
        assert_eq!(res.bestmove.unwrap(), "e7d8q");
    }
}
