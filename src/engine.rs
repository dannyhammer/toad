/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use core::fmt;
use std::{
    io,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::{channel, Receiver, Sender},
        Arc, Mutex,
    },
    thread::{self, JoinHandle},
};

use anyhow::{bail, Context, Result};
use uci_parser::{UciCommand, UciInfo, UciOption, UciParseError, UciResponse};

use crate::{
    perft, splitperft, Bitboard, Chess960, EngineCommand, Evaluator, Game, GameVariant,
    HistoryTable, LogLevel, Move, Piece, Position, Psqt, Search, SearchConfig, SearchResult,
    Square, Standard, TTable, Variant, BENCHMARK_FENS,
};

/// Default depth at which to run the benchmark searches.
const BENCH_DEPTH: u8 = 7;

/// The Toad chess engine.
#[derive(Debug)]
pub struct Engine {
    /// The current state of the chess board, as known to the engine.
    ///
    /// This is modified whenever moves are played or new positions are given,
    /// and is reset whenever the engine is told to start a new game.
    game: Game,

    /// All previous positions of `self.game`, including the current position.
    ///
    /// Updated when the engine makes a move or receives `position ... moves [move list]`.
    prev_positions: Vec<Position>,

    /// One half of a channel, responsible for sending commands to the engine to execute.
    sender: Sender<EngineCommand>,

    /// One half of a channel, responsible for receiving commands for the engine to execute.
    receiver: Receiver<EngineCommand>,

    /// Atomic flag to determine whether a search is currently running
    is_searching: Arc<AtomicBool>,

    /// Handle to the currently-running search thread, if one exists.
    search_thread: Option<JoinHandle<SearchResult>>,

    /// Transposition table used to cache information found during search.
    ttable: Arc<Mutex<TTable>>,

    /// History table for keeping track of good/bad moves during search.
    history: Arc<Mutex<HistoryTable>>,

    /// Whether to display extra information during execution.
    debug: bool,

    /// Chess variant being played
    variant: GameVariant,
}

impl Engine {
    /// Constructs a new [`Engine`] instance to be executed with [`Engine::run`].
    #[inline(always)]
    pub fn new() -> Self {
        // Construct a channel for communication and threadpool for parallel tasks
        let (sender, receiver) = channel();

        Self {
            game: Game::default(),
            prev_positions: Vec::with_capacity(512),
            sender,
            receiver,
            is_searching: Arc::default(),
            search_thread: None,
            ttable: Arc::default(),
            history: Arc::default(),
            debug: false,
            variant: Default::default(),
        }
    }

    /// Returns a string of the engine's name and current version.
    #[inline(always)]
    pub fn name(&self) -> String {
        format!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"))
    }

    /// Returns a string of all authors of this engine.
    #[inline(always)]
    pub fn authors(&self) -> String {
        // Split multiple authors by comma-space
        env!("CARGO_PKG_AUTHORS").replace(':', ", ").to_string()
    }

    /// Sends an [`EngineCommand`] to the engine to be executed.
    #[inline(always)]
    pub fn send_command(&self, command: EngineCommand) {
        // Safe unwrap: `send` can only fail if it's corresponding receiver doesn't exist,
        //  and the only way our engine's `Receiver` can no longer exist is when our engine
        //  doesn't exist either, so this is always safe.
        self.sender.send(command).unwrap();
    }

    /// Execute the main event loop for the engine.
    ///
    /// This function spawns a thread to handle input from `stdin` and waits on received commands.
    pub fn run(&mut self) -> Result<()> {
        // Spawn a separate thread for handling user input
        let sender = self.sender.clone();
        thread::spawn(|| {
            if let Err(err) = input_handler(sender) {
                eprintln!("Input handler thread stopping after fatal error: {err:#}");
            }
        });

        // Loop on user input
        while let Ok(cmd) = self.receiver.recv() {
            if self.debug {
                println!("info string Received command {cmd:?}");
            }
            match cmd {
                EngineCommand::Await => {
                    self.stop_search();
                }

                EngineCommand::Bench { depth, pretty } => self.bench(depth, pretty)?,

                EngineCommand::Display => self.display(),

                EngineCommand::Eval { pretty } => self.eval(pretty),

                EngineCommand::Exit { cleanup } => {
                    // If requested, await the completion of any ongoing search threads
                    if cleanup {
                        self.stop_search();
                    }

                    // Exit the loop so the engine can quit
                    break;
                }

                EngineCommand::Fen => println!("{}", self.game.to_fen()),

                EngineCommand::Flip => self.game.toggle_side_to_move(),

                EngineCommand::MakeMove { mv_string } => {
                    match Move::from_uci(&self.game, &mv_string) {
                        Ok(mv) => self.make_move(mv),
                        Err(e) => eprintln!("{e:#}"),
                    }
                }

                EngineCommand::Moves {
                    square,
                    pretty,
                    debug,
                    sort,
                } => self.moves(square, pretty, debug, sort),

                EngineCommand::Option { name } => {
                    if let Some(value) = self.get_option(&name) {
                        println!("Option {name:?} := {value}");
                    } else {
                        println!("{} has no option {name:?}", self.name());
                    }
                }

                EngineCommand::Perft { depth } => println!("{}", perft(&self.game, depth)),

                EngineCommand::Psqt {
                    piece,
                    square,
                    endgame_weight: weight,
                } => self.psqt(piece, square, weight),

                EngineCommand::Splitperft { depth } => {
                    println!("{}", splitperft(&self.game, depth))
                }

                EngineCommand::HashInfo => self.hash_info(),

                EngineCommand::Uci { cmd } => {
                    // Keep running, even on error
                    if let Err(e) = self.handle_uci_command(cmd) {
                        eprintln!("Error: {e:#}");
                    }
                }
            };
        }

        Ok(())
    }

    /// Handle the execution of a single [`UciCommand`].
    fn handle_uci_command(&mut self, uci: UciCommand) -> Result<()> {
        use UciCommand::*;
        match uci {
            Uci => self.uci(),

            Debug(status) => self.debug = status,

            IsReady => println!("{}", UciResponse::<&str>::ReadyOk),

            SetOption { name, value } => self.set_option(&name, value)?,

            Register { name: _, code: _ } => println!("{} requires no registration", self.name()),

            UciNewGame => self.new_game(),

            Position { fen, moves } => self.position(fen, moves)?,

            Go(options) => {
                if let Some(depth) = options.perft {
                    println!("{}", splitperft(&self.game, depth as usize));
                    return Ok(());
                }

                let config = SearchConfig::new(options, &self.game);
                self.search_thread = match (self.variant, self.debug) {
                    (GameVariant::Standard, true) => {
                        self.start_search::<{ LogLevel::Debug as u8 }, Standard>(config)
                    }
                    (GameVariant::Standard, false) => {
                        self.start_search::<{ LogLevel::Info as u8 }, Standard>(config)
                    }

                    (GameVariant::Chess960, true) => {
                        self.start_search::<{ LogLevel::Debug as u8 }, Chess960>(config)
                    }
                    (GameVariant::Chess960, false) => {
                        self.start_search::<{ LogLevel::Info as u8 }, Chess960>(config)
                    }
                };
            }

            Stop => self.set_is_searching(false),

            // PonderHit => self.ponderhit(),
            Quit => self.send_command(EngineCommand::Exit { cleanup: false }),

            _ => bail!("{} does not support UCI command {uci:?}", self.name()),
        }

        Ok(())
    }

    /// Execute the `bench` command, running a benchmark of a fixed search on a series of positions and displaying the results.
    fn bench(&mut self, depth: Option<u8>, pretty: bool) -> Result<()> {
        // Set up the benchmarking config
        let config = SearchConfig {
            max_depth: depth.unwrap_or(BENCH_DEPTH),
            ..Default::default()
        };

        let benches = BENCHMARK_FENS;
        let num_tests = benches.len();
        let mut nodes = 0;

        // Run a fixed search on each position
        for (i, epd) in benches.into_iter().enumerate() {
            // Parse the FEN and the total node count

            let fen = epd.split(';').next().unwrap();
            println!("Benchmark position {}/{}: {fen}", i + 1, num_tests);

            // Set up the game and start the search
            self.position(Some(fen), [])?;
            self.search_thread = self.start_search::<{ LogLevel::None as u8 }, Standard>(config);

            // Await the search, appending the node count once concluded.
            let res = self.stop_search().unwrap();
            nodes += res.nodes;

            // Bench is on different positions, so hash tables are not likely to contain useful info.
            self.clear_hash_tables();
        }

        // Compute results
        let elapsed = config.starttime.elapsed();
        let nps = (nodes as f32 / elapsed.as_secs_f32()) as u64;
        let m_nps = nodes as f32 / elapsed.as_secs_f32() / 1_000_000.0;
        let ms = elapsed.as_millis();

        if pretty {
            // Display the results in a nice table
            println!();
            println!("+--- Benchmark Complete ---+");
            println!("| time (ms)  : {ms:<12}|");
            println!("| nodes      : {nodes:<12}|");
            println!("| nps        : {nps:<12}|");
            println!("| Mnps       : {m_nps:<12.2}|");
            println!("+--------------------------+");
        } else {
            println!("{nodes} nodes {nps} nps");
        }

        // Re-set the internal game state.
        self.new_game();

        Ok(())
    }

    /// Executes the `display` command, printing the current position.
    fn display(&self) {
        if self.variant.is_chess960() {
            println!("{:#}", self.game);
        } else {
            println!("{}", self.game);
        }
    }

    /// Executes the `eval` command, printing an evaluation of the current position.
    fn eval(&self, pretty: bool) {
        let evaluator = Evaluator::new(&self.game);

        if pretty {
            println!("{evaluator}");
        } else {
            println!("{}", evaluator.eval());
        }
    }

    /// Display info about the internal hash table(s)
    fn hash_info(&self) {
        let ttable = self.ttable();

        let size = ttable.size();
        let num = ttable.num_entries();
        let cap = ttable.capacity();
        let percent = num as f32 / cap as f32 * 100.0;
        println!("TT info: {size}mb @ {num}/{cap} entries ({percent:.2}% full)");
    }

    /// Clears all hash tables in the engine.
    ///
    /// Called in between games.
    #[inline(always)]
    fn clear_hash_tables(&mut self) {
        self.ttable().clear();
        self.history().clear();
    }

    /// Makes the supplied move on the current position.
    #[inline(always)]
    fn make_move(&mut self, mv: Move) {
        self.prev_positions.push(*self.game.position());
        self.game.make_move(mv);
    }

    /// Executes the `moves` command, displaying all available moves on the board, or for the given square.
    fn moves(&self, square: Option<Square>, pretty: bool, debug: bool, sort: bool) {
        // Get the legal moves
        let moves = if let Some(square) = square {
            self.game.get_legal_moves_from(square)
        } else {
            self.game.get_legal_moves()
        };

        // If there are none, print "(none)"
        if moves.is_empty() {
            println!("(none)")
        } else {
            // Join by comma-space
            // TODO: I don't love how this code is laid out, but it's UI code, so it doesn't *need* to be fast.
            let string = {
                let mut s = moves
                    .iter()
                    .map(|mv| match (debug, self.variant.is_chess960()) {
                        (true, true) => format!("{mv:#?}"),
                        (true, false) => format!("{mv:?}"),
                        (false, true) => format!("{mv:#}"),
                        (false, false) => format!("{mv}"),
                    })
                    .collect::<Vec<_>>();

                // Sort alphabetically, if necessary
                if sort {
                    s.sort();
                }

                s.join(", ")
            };

            // If pretty-printing, also display a Bitboard of all possible destinations
            if pretty {
                let bb = moves.iter().map(|mv| mv.to()).collect::<Bitboard>();
                println!("{bb:?}\n\nmoves: {string}");
            } else {
                println!("{string}");
            }
        }
    }

    /// Resets the engine's internal game state.
    ///
    /// This clears all internal caches and hash tables, as well as search history.
    /// It also cancels any ongoing searches, ignoring their results.
    #[inline(always)]
    fn new_game(&mut self) {
        self.set_is_searching(false);
        self.game = Game::default();
        self.prev_positions.clear();
        self.clear_hash_tables();
    }

    /// Set the position to the supplied FEN string (defaults to the standard startpos if not supplied),
    /// and then apply `moves` one-by-one to the position.
    fn position<T: AsRef<str>>(
        &mut self,
        fen: Option<T>,
        moves: impl IntoIterator<Item = T>,
    ) -> Result<()> {
        // Set the new position
        if let Some(fen) = fen {
            self.game = fen.as_ref().parse()?;
        } else {
            self.game = Game::default();
        }

        // Since this is a new position, it has a new history
        self.prev_positions.clear();

        // Apply the provided moves
        for mv_str in moves {
            let mv = Move::from_uci(&self.game, mv_str.as_ref())?;
            self.make_move(mv);
        }

        Ok(())
    }

    /// Executes the `psqt` command, printing the piece-square table info for the provided piece.
    fn psqt(&self, piece: Piece, square: Option<Square>, endgame_weight: Option<i32>) {
        // Compute the current endgame weight, if it wasn't provided
        let weight = endgame_weight.unwrap_or(Evaluator::new(&self.game).endgame_weight);
        // Fetch the middle-game and end-game tables
        let (mg, eg) = Psqt::get_tables_for(piece.kind());

        // If there was a square provided, print the eval for that square
        if let Some(square) = square {
            let (mg_value, eg_value) = Psqt::evals(piece, square);
            let value = mg_value.lerp(eg_value, weight);
            println!("[{mg_value}, {eg_value}] := {value}");
        } else {
            // Otherwise, print both the middle-game and end-game tables
            let name = piece.name();

            // If the piece is Black, flip the tables when printing
            let f = |psqt| {
                if piece.is_white() {
                    format!("{psqt}")
                } else {
                    format!("{psqt:#}")
                }
            };

            println!("Mid-game table for {name}:\n{}", f(mg));
            println!();
            println!("End-game table for {name}:\n{}", f(eg));
        }
    }

    /// Sets the search flag to signal that the engine is starting/stopping a search.
    #[inline(always)]
    fn set_is_searching(&mut self, status: bool) {
        self.is_searching.store(status, Ordering::Relaxed);
    }

    /// Returns `true` if the engine is currently executing a searching.
    #[inline(always)]
    fn is_searching(&self) -> bool {
        self.is_searching.load(Ordering::Relaxed)
    }

    /// Starts a search on the current position, given the parameters in `config`.
    fn start_search<const LOG: u8, V: Variant>(
        &mut self,
        config: SearchConfig,
    ) -> Option<JoinHandle<SearchResult>> {
        // Cannot start a search if one is already running
        if self.is_searching() {
            Self::send_string("A search is already running");
            return None;
        }
        self.set_is_searching(true);

        // Clone the parameters that will be sent into the thread
        let game = self.game;
        let is_searching = Arc::clone(&self.is_searching);
        let mut prev_positions = self.prev_positions.clone();
        // Cloning a vec doesn't clone its capacity, so we need to do that manually
        prev_positions.reserve(self.prev_positions.capacity());
        prev_positions.push(*game.position());
        let ttable = Arc::clone(&self.ttable);
        let history = Arc::clone(&self.history);

        // Spawn a thread to conduct the search
        let handle = thread::spawn(move || {
            // Lock the hash tables at the start of the search so that only the search thread may modify them
            let mut ttable = ttable.lock().unwrap();
            let mut history = history.lock().unwrap();

            // Start the search, returning the result when completed.
            Search::<LOG, V>::new(
                is_searching,
                config,
                prev_positions,
                &mut ttable,
                &mut history,
            )
            .start(&game)
        });

        Some(handle)
    }

    /// Awaits the current search thread, blocking until it finishes and returning its result.
    fn stop_search(&mut self) -> Option<SearchResult> {
        // Can't stop a search if there aren't any threads searching!
        let handle = self.search_thread.take()?;

        // Attempt to join the thread handle to retrieve the result
        let id = handle.thread().id();
        let Ok(res) = handle.join() else {
            Self::send_string(format!("Failed to join on thread {id:?}"));
            return None;
        };

        // Flip the search flag so that any active threads will (hopefully) begin to clean themselves up.
        self.set_is_searching(false);

        Some(res)
    }

    /// Called when the engine receives the `uci` command.
    ///
    /// Prints engine's ID, version, and authors, and lists all UCI options.
    fn uci(&self) {
        println!("id name {}\nid author {}\n", self.name(), self.authors());

        // Print all UCI options
        for opt in self.options() {
            println!("{}", UciResponse::Option(opt));
        }

        // We're ready to go!
        println!("{}", UciResponse::<&str>::UciOk)
    }

    /// Convenience function to return an iterator over all UCI options this engine supports.
    fn options(&self) -> impl Iterator<Item = UciOption> {
        [
            UciOption::button("Clear Hash"),
            UciOption::spin(
                "Hash",
                TTable::DEFAULT_SIZE as i32,
                TTable::MIN_SIZE as i32,
                TTable::MAX_SIZE as i32,
            ),
            UciOption::spin("Threads", 1, 1, 1),
            UciOption::check("UCI_Chess960", false),
        ]
        .into_iter()
    }

    /// Handles the `setoption` command, setting option `name` to `value`, or toggling it if `value` is None.
    ///
    /// Will return an error if `name` isn't a valid option or `value` is not a valid value for that option.
    fn set_option(&mut self, name: &str, value: Option<String>) -> Result<()> {
        match name {
            // Clear all hash tables
            "Clear Hash" => self.clear_hash_tables(),

            // Re-size the hash table
            "Hash" => {
                let Some(value) = value.as_ref() else {
                    bail!("usage: setoption name {name} value <value>");
                };

                let mb = value
                    .parse::<usize>()
                    .context(format!("expected integer. got {value:?}"))?;

                // Ensure the value is within bounds
                if mb < TTable::MIN_SIZE {
                    bail!("Minimum value for Hash is {}mb", TTable::MIN_SIZE);
                }
                if mb > TTable::MAX_SIZE {
                    bail!("Maximum value for Hash is {}mb", TTable::MAX_SIZE);
                }

                *self.ttable() = TTable::new(mb);
            }

            // Set the number of search threads
            "Threads" => bail!("{} currently supports only 1 thread", self.name()),

            "UCI_Chess960" => {
                let Some(value) = value.as_ref() else {
                    bail!("usage: setoption name {name} value <true / false>");
                };

                let enabled = value
                    .parse::<bool>()
                    .context(format!("expected bool. got {value:?}"))?;

                if enabled {
                    self.variant = GameVariant::Chess960;
                } else {
                    self.variant = Default::default();
                }
            }

            _ => {
                if let Some(value) = value.as_ref() {
                    bail!("Unrecognized option {name:?} with value {value:?}")
                } else {
                    bail!("Unrecognized option {name:?}")
                }
            }
        }

        if self.debug {
            let info = if let Some(value) = value.as_ref() {
                format!("Option {name} set to {value}")
            } else {
                format!("Option {name} toggled")
            };
            Self::send_string(info);
        }

        Ok(())
    }

    /// Returns the current value of the option `name`, if it exists on this engine.
    fn get_option(&self, name: &str) -> Option<String> {
        let value = match name {
            "Clear Hash" => String::default(),

            "Hash" => format!("{}", self.ttable().size()),

            "Threads" => String::from("1"),

            "UCI_Chess960" => format!("{}", self.variant.is_chess960()),

            _ => return None,
        };

        Some(value)
    }

    /// Helper to send a [`UciInfo`] containing only a `string` message to `stdout`.
    #[inline(always)]
    fn send_string<T: fmt::Display>(info: T) {
        let resp = UciResponse::<String>::Info(Box::new(UciInfo::new().string(info)));
        println!("{resp}");
    }

    /// Helper function to fetch the TTable, panicking if impossible.
    #[inline(always)]
    fn ttable(&self) -> std::sync::MutexGuard<'_, TTable> {
        self.ttable
            .lock()
            .expect("A thread holding the TTable panicked")
    }

    /// Helper function to fetch the History table, panicking if impossible.
    #[inline(always)]
    fn history(&self) -> std::sync::MutexGuard<'_, HistoryTable> {
        self.history
            .lock()
            .expect("A thread holding the History table panicked")
    }
}

impl Default for Engine {
    #[inline(always)]
    fn default() -> Self {
        Self::new()
    }
}

/// Loops endlessly to await input via `stdin`, sending all successfully-parsed commands through the supplied `sender`.
fn input_handler(sender: Sender<EngineCommand>) -> Result<()> {
    let mut buffer = String::with_capacity(2048); // Seems like a good amount of space to pre-allocate

    loop {
        // Clear the buffer, read input, and trim the trailing newline
        buffer.clear();
        let bytes = io::stdin()
            .read_line(&mut buffer)
            .context("Failed to read line when parsing UCI commands")?;

        // For ctrl + d
        if 0 == bytes {
            // Send the Quit command and exit this function
            sender
                .send(EngineCommand::Exit { cleanup: false })
                .context("Failed to send 'quit' command after receiving empty input")?;

            bail!("Engine received input of 0 bytes and is quitting");
        }

        // Trim any leading/trailing whitespace
        let buf = buffer.trim();

        // Ignore empty lines
        if buf.is_empty() {
            continue;
        }

        // Attempt to parse the input as a UCI command first, since that's the primary use case of the engine
        match UciCommand::new(buf) {
            Ok(cmd) => sender
                .send(EngineCommand::Uci { cmd })
                .context("Failed to send UCI command to engine")?,

            // If it's not a UCI command, check if it's an engine-specific command
            Err(UciParseError::UnrecognizedCommand { cmd: _ }) => {
                match buf.parse() {
                    Ok(cmd) => sender
                        .send(cmd)
                        .context("Failed to send command to engine")?,

                    // If it wasn't a custom command, either, print an error.
                    Err(err) => err.print()?,
                }
            }

            // If it was a UCI command, print a usage message.
            Err(uci_err) => eprintln!("{uci_err:#}"),
        }

        /*
        // This would work if UciCommand was clap-friendly
        match EngineCommand::try_parse_from(buf.split_ascii_whitespace()) {
            // If successful, send the command to the engine
            Ok(cmd) => sender
                .send(cmd)
                .context("Failed to send command {buffer:?} to engine")?,

            // If an invalid command was received, just print the error and continue running
            Err(err) => println!("{err}"),
        }
         */
    }
}
