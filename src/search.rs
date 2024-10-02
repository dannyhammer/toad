use anyhow::{bail, Result};
use chessie::{Game, Move};
use std::{
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::Sender,
        Arc,
    },
    time::{Duration, Instant},
    u64,
};
use uci_parser::{UciCommand, UciSearchOptions};

use crate::{EngineCommand, Evaluator, Score, MAX_DEPTH};

/// The result of a search, containing the best move found, score, and total nodes searched.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SearchResult {
    pub nodes: u64,
    pub bestmove: Option<Move>,
    pub score: Score,
}

impl Default for SearchResult {
    /// A default search result should initialize to a *very bad* value.
    fn default() -> Self {
        Self {
            nodes: 0,
            bestmove: None,
            score: Score(0),
        }
    }
}

/// Configuration variables for executing a [`Search`].
#[derive(Debug, Clone, Copy)]
pub struct SearchConfig {
    pub max_depth: usize,
    pub max_nodes: u64,
    pub starttime: Instant,
    pub soft_timeout: Duration,
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

impl From<UciSearchOptions> for SearchConfig {
    /// Convert a [`UciSearchOptions`] to a [`SearchConfig`] by using a default [`Game`].
    fn from(options: UciSearchOptions) -> Self {
        Self::new(options, &Game::default())
    }
}

impl Default for SearchConfig {
    /// A default [`SearchConfig`] will permit an "infinite" search.
    ///
    /// The word "infinite" is quoted here because the actual defaults are the `::MAX` values for each field.
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
    game: &'a Game,
    result: SearchResult,
    sender: Sender<EngineCommand>,
    is_searching: Arc<AtomicBool>,
    config: SearchConfig,
}

impl<'a> Search<'a> {
    /// Construct a new [`Search`] instance to execute on the provided [`Game`].
    pub fn new(
        game: &'a Game,
        sender: Sender<EngineCommand>,
        is_searching: Arc<AtomicBool>,
        config: SearchConfig,
    ) -> Self {
        let mut result = SearchResult::default();
        // Initialize `bestmove` to the first move available
        result.bestmove = game.into_iter().next();
        Self {
            game,
            result,
            sender,
            is_searching,
            config,
        }
    }

    /// Start the search, returning its results if the search was successful.
    ///
    /// This is the entrypoint of the search, and begins by performing iterative deepening.
    pub fn start(mut self) -> SearchResult {
        println!("info string Starting search on {:?}", self.game.to_fen());
        // println!(
        //     "info string Soft timeout {}ms",
        //     self.config.soft_timeout.as_millis()
        // );
        // println!(
        //     "info string Hard timeout {}ms",
        //     self.config.hard_timeout.as_millis()
        // );

        let mut depth = 1;

        // Save the original result
        let mut res = self.result;

        // Iterative Deepening
        // In order to obey time constraints, we run searches one-by-one with increasing depths
        // until we run out of time.
        while self.config.starttime.elapsed() < self.config.soft_timeout
            && self.is_searching.load(Ordering::Relaxed)
            && depth <= self.config.max_depth
        {
            // TODO: Reset score after each search?
            // self.result.score = -Score::INF;

            if let Err(e) = self.negamax_root(*self.game, depth) {
                println!("info string Search cancelled at depth {depth}: {e}");
                break;
            }

            // If the search at the next depth succeeded, update the result.
            res = self.result;

            // TODO: UCI response
            println!(
                "info depth {depth} nodes {} score cp {}",
                res.nodes, res.score,
            );

            // Increase the depth for the next iteration
            depth += 1;
        }

        // Search has concluded, send a message to stop the search.
        self.sender
            .send(EngineCommand::UciCommand(UciCommand::Stop))
            .unwrap();

        res
    }

    fn negamax_root(&mut self, game: Game, depth: usize) -> Result<()> {
        let moves = game.get_legal_moves();

        // If there are no legal moves, it's either mate or a draw.
        if moves.is_empty() {
            if game.is_in_check() {
                self.result.score = Score::MATE;
            } else {
                self.result.score = Score::DRAW;
            };
            return Ok(());
        }

        self.result.score = -Score::INF;
        self.result.bestmove = moves.first().copied();

        for mv in moves {
            let new_game = game.with_move_made(mv);

            let new_score = self.negamax(new_game, depth - 1)?;
            self.result.nodes += 1;

            if new_score > self.result.score {
                self.result.score = new_score;
                self.result.bestmove = Some(mv);
            }
        }

        Ok(())
    }

    fn negamax(&mut self, game: Game, depth: usize) -> Result<Score> {
        // If we've reached a terminal node, evaluate the position
        if depth == 0 {
            return Ok(Evaluator::new(&game).eval());
        }

        let moves = game.get_legal_moves();

        // If there are no legal moves, it's either mate or a draw.
        if moves.is_empty() {
            let score = if game.is_in_check() {
                Score::MATE
            } else {
                Score::DRAW
            };
            return Ok(score);
        }

        // Start with a *really bad* initial score
        let mut score = -Score::INF;

        for mv in moves {
            // We need to check several conditions periodically while searching, to make sure we can continue
            // Condition 1: We've exceeded the hard limit of our allotted search time
            if self.config.starttime.elapsed() >= self.config.hard_timeout {
                let ms = self.config.hard_timeout.as_millis();
                bail!("exceeded hard timeout of {ms}ms",);
            } else
            // Condition 2: The search was stopped by an external factor, like the `stop` command
            if !self.is_searching.load(Ordering::Relaxed) {
                bail!("cancelled by `stop` command");
            } else
            // Condition 3: We've exceeded the maximum amount of nodes we're allowed to search
            if self.result.nodes >= self.config.max_nodes {
                let nodes = self.config.max_nodes;
                bail!("exceeded node allowance of {nodes} nodes");
            }

            // Copy-make the new position
            let new_game = game.with_move_made(mv);

            // Recurse
            let new_score = self.negamax(new_game, depth - 1)?;
            self.result.nodes += 1;

            // If the new score is better than the current score, update it.
            score = score.max(new_score);
        }

        Ok(score)
    }
}
