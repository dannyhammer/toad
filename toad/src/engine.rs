/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{
    fmt,
    io::{self, Write},
    ops::ControlFlow,
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
    perft, splitperft, Bitboard, Chess960, EngineCommand, Game, GameVariant, HistoryTable,
    LogDebug, LogInfo, LogLevel, LogNone, MediumDisplayTable, Move, Piece, Ply, Position, Psqt,
    Score, Search, SearchConfig, SearchResult, Square, Standard, TTable, Variant, BENCHMARK_FENS,
};

/// Default depth at which to run the benchmark searches.
const BENCH_DEPTH: u8 = 9;

/// The Toad chess engine.
#[derive(Debug)]
pub struct Engine {
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
}

impl Engine {
    /// Constructs a new [`Engine`] instance to be executed with [`Engine::run`].
    #[inline(always)]
    pub fn new() -> Self {
        // Construct a channel for communication and threadpool for parallel tasks
        let (sender, receiver) = channel();

        Self {
            prev_positions: Vec::with_capacity(512),
            sender,
            receiver,
            is_searching: Arc::default(),
            search_thread: None,
            ttable: Arc::default(),
            history: Arc::default(),
            debug: false,
        }
    }

    /// Returns a string of the engine's name and current version.
    #[inline(always)]
    pub fn name(&self) -> String {
        format!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"))

        // This could be done with `concat!`, but I'm keeping it consistent with `Engine::authors`
        // concat!(env!("CARGO_PKG_NAME"), " ", env!("CARGO_PKG_VERSION"))
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
        self.sender
            .send(command)
            .expect("Failed to send a command to the engine via channels.");
    }

    /// Entrypoint of the engine.
    ///
    /// This function first spawns a new thread that handles user input from `stdin`.
    /// It then loops on commands received by the engine, executing them in the order received.
    /// If the [`EngineCommand::ChangeVariant`] command is received,
    /// it exits the event loop and spawns a new one,
    /// changing the generic parameter to the appropriate type representing the requested chess variant.
    pub fn run(&mut self) {
        // Spawn a separate thread for handling user input
        let sender = self.sender.clone();
        thread::spawn(|| {
            if let Err(err) = input_handler(sender) {
                eprintln!("Input handler thread stopping after fatal error: {err:#}");
            }
        });

        // By default, we're playing standard chess
        let mut variant = GameVariant::Standard;

        // Run the event loop on the current variant.
        while let ControlFlow::Continue(new_variant) = match variant {
            GameVariant::Standard => self.run_variant::<Standard>(),
            GameVariant::Chess960 => self.run_variant::<Chess960>(),
            // GameVariant::Horde => self.run_variant::<Horde>(),
        } {
            variant = new_variant;

            if self.debug {
                println!("Switching to chess variant: {new_variant:?}");
            }
        }
    }

    /// Execute the main event loop for the engine for a specific variant of chess.
    fn run_variant<V: Variant>(&mut self) -> ControlFlow<(), GameVariant> {
        // Since we're starting the engine for a new variant, create a new game.
        let mut game = self.new_game::<V>();

        // When we exit the event loop, we will, by default, not spawn another instance of it
        let mut status = ControlFlow::Break(());

        // Execute commands as they are received
        while let Ok(cmd) = self.receiver.recv() {
            // if self.debug {
            //     println!("info string Received command {cmd:?}");
            // }

            match cmd {
                EngineCommand::Bench { depth, pretty } => self.bench(depth, pretty),

                EngineCommand::ChangeVariant { variant } => {
                    // If a new variant was provided, change the current variant, if necessary.
                    if let Some(variant) = variant {
                        // If we're already in this variant, there's nothing to change
                        if variant != V::variant() {
                            // Otherwise, kill this event loop with a signal to spawn a new one.
                            status = ControlFlow::Continue(variant);
                            break;
                        }
                    } else {
                        println!("Current variant: {:?}", V::variant());
                    }
                }

                EngineCommand::Display => println!("{game}"),

                EngineCommand::Eval { pretty } => self.eval(&game, pretty),

                EngineCommand::Exit { cleanup } => {
                    // If requested, await the completion of any ongoing search threads
                    if cleanup {
                        self.stop_search();
                    }

                    // Exit the loop so the engine can quit
                    break;
                }

                EngineCommand::Fen => println!("{}", game.to_fen()),

                EngineCommand::Flip => game.toggle_side_to_move(),

                EngineCommand::HashInfo => self.hash_info(),

                EngineCommand::MakeMove { mv_string } => match Move::from_uci(&game, &mv_string) {
                    Ok(mv) => self.make_move(&mut game, mv),
                    Err(e) => eprintln!("{e:#}"),
                },

                EngineCommand::Moves {
                    square,
                    pretty,
                    debug,
                    sort,
                } => self.moves(&game, square, pretty, debug, sort),

                EngineCommand::Option { name } => {
                    if let Some(value) = self.get_option::<V>(&name) {
                        println!("Option {name:?} := {value}");
                    } else {
                        println!("{} has no option {name:?}", self.name());
                    }
                }

                EngineCommand::Perft { depth } => println!("{}", perft(&game, depth)),

                EngineCommand::Place { piece, square } => {
                    game.place(piece, square);
                    if self.debug {
                        println!("Placed {piece} at {square}");
                    }
                }

                EngineCommand::Psqt {
                    piece,
                    square,
                    endgame_weight: weight,
                } => self.psqt(&game, piece, square, weight),

                EngineCommand::Splitperft { depth } => {
                    println!("{}", splitperft(&game, depth))
                }

                EngineCommand::Take { square } => {
                    if let Some(piece) = game.take(square) {
                        if self.debug {
                            println!("Removed {piece} at {square}");
                        }
                    }
                }

                EngineCommand::Uci { cmd } => {
                    // UCI spec states to continue execution if an error occurs
                    if let Err(e) = self.handle_uci_command(cmd, &mut game) {
                        eprintln!("Error: {e:#}");
                    }
                }

                EngineCommand::Wait => _ = self.stop_search(),
            };
        }

        // Return whether to spawn another event loop
        status
    }

    /// Handle the execution of a single [`UciCommand`].
    fn handle_uci_command<V: Variant>(
        &mut self,
        uci: UciCommand,
        game: &mut Game<V>,
    ) -> Result<()> {
        use UciCommand::*;
        match uci {
            Uci => self.uci(),

            Debug(status) => self.debug = status,

            IsReady => println!("{}", UciResponse::<&str>::ReadyOk),

            SetOption { name, value } => self.set_option(&name, value)?,

            Register { name: _, code: _ } => println!("{} requires no registration", self.name()),

            UciNewGame => *game = self.new_game(),

            Go(options) => {
                if let Some(depth) = options.perft {
                    println!("{}", splitperft(game, depth as usize));
                    return Ok(());
                }

                let config = SearchConfig::new(options, game);
                self.search_thread = if self.debug {
                    self.start_search::<LogDebug, V>(*game, config)
                } else {
                    self.start_search::<LogInfo, V>(*game, config)
                };
            }

            Position { fen, moves } => {
                *game = self.position(fen, moves)?;
            }

            Stop => self.set_is_searching(false),

            // PonderHit => self.ponderhit(),
            Quit => self.send_command(EngineCommand::Exit { cleanup: false }),

            _ => bail!("{} does not support UCI command {uci:?}", self.name()),
        }

        Ok(())
    }

    /// Execute the `bench` command, running a benchmark of a fixed search on a series of positions and displaying the results.
    fn bench(&mut self, depth: Option<u8>, pretty: bool) {
        // Set up the benchmarking config
        let config = SearchConfig {
            max_depth: Ply::new(depth.unwrap_or(BENCH_DEPTH) as i32),
            ..Default::default()
        };

        let benches = BENCHMARK_FENS;
        let mut nodes = 0;

        // Padding for printing FENs
        let width = benches.iter().map(|fen| fen.len()).max().unwrap();

        println!(
            "Running fixed-depth search (d={}) on {} positions",
            config.max_depth,
            benches.len()
        );

        // Run a fixed search on each position
        for (i, fen) in benches.into_iter().enumerate() {
            print!("{:>2}/{:>2}: {fen:<width$} := ", i + 1, benches.len());
            std::io::stdout().lock().flush().unwrap(); // flush stdout so the node count will appear on the same line after search concludes

            // Set up the game and start the search
            let game = self.position(Some(fen), []).unwrap();
            self.search_thread = self.start_search::<LogNone, Standard>(game, config);

            // Await the search, appending the node count once concluded.
            let Some(res) = self.stop_search() else {
                panic!("Search thread panicked while running benchmarks on fen {fen}");
            };
            nodes += res.nodes;
            println!("{}", res.nodes);

            // Each bench is essentially a new game, so reset hash tables, etc.
            self.new_game::<Standard>();
        }

        // Compute results
        let elapsed = config.starttime.elapsed();
        let nps = (nodes as f32 / elapsed.as_secs_f32()) as u64;
        let m_nps = nodes as f32 / elapsed.as_secs_f32() / 1_000_000.0;
        let ms = elapsed.as_millis();

        if pretty {
            // Display the results in a nice table
            println!();
            println!("+-- Benchmark Complete --+");
            println!("| time (ms)  {ms:<12}|");
            println!("|     nodes  {nodes:<12}|");
            println!("|       nps  {nps:<12}|");
            println!("|      Mnps  {m_nps:<12.2}|");
            println!("+------------------------+");
        } else {
            println!("{nodes} nodes / {elapsed:?} := {nps} nps");
        }

        // Re-set the internal game state.
        self.new_game::<Standard>();
    }

    /// Executes the `eval` command, printing an evaluation of the current position.
    fn eval<V: Variant>(&self, game: &Game<V>, pretty: bool) {
        use std::cmp::Ordering::*;
        if pretty {
            let color = game.side_to_move();
            let endgame_weight = game.evaluator().endgame_weight();

            let table = MediumDisplayTable::from_fn(|sq| {
                game.piece_at(sq)
                    .map(|piece| {
                        let (mg, eg) = Psqt::evals(piece, sq);
                        let score = mg.lerp(eg, endgame_weight)
                            * piece.color().negation_multiplier() as i32;

                        [piece.to_string(), format!("{:+}", score.normalize())]
                    })
                    .unwrap_or_default()
            });

            let score = game.evaluator().eval_for(color);

            let winning = match score.cmp(&Score::DRAW) {
                Greater => color.name(),
                Less => color.opponent().name(),
                Equal => "N/A",
            };

            println!("{table}");
            println!("Endgame: {endgame_weight}%");
            println!("Winning side: {winning}",);
            println!("Score: {score}");
        } else {
            println!("{}", game.eval());
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

        println!("History Tables:\n{}", self.history());
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
    fn make_move<V: Variant>(&mut self, game: &mut Game<V>, mv: Move) {
        self.prev_positions.push(*game.position());
        game.make_move(mv);
    }

    /// Executes the `moves` command, displaying all available moves on the board, or for the given square.
    fn moves<V: Variant>(
        &self,
        game: &Game<V>,
        square: Option<Square>,
        pretty: bool,
        debug: bool,
        sort: bool,
    ) {
        // Get the legal moves
        let moves = if let Some(square) = square {
            game.get_legal_moves_from(square)
        } else {
            game.get_legal_moves()
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
                    .map(|mv| {
                        if debug {
                            V::dbg_move(*mv)
                        } else {
                            V::fmt_move(*mv)
                        }
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
    fn new_game<V: Variant>(&mut self) -> Game<V> {
        self.set_is_searching(false);
        self.prev_positions.clear();
        self.clear_hash_tables();
        Game::default()
    }

    /// Set the position to the supplied FEN string (defaults to the standard startpos if not supplied),
    /// and then apply `moves` one-by-one to the position.
    fn position<T: AsRef<str>, V: Variant>(
        &mut self,
        fen: Option<T>,
        moves: impl IntoIterator<Item = T>,
    ) -> Result<Game<V>> {
        // Set the new position
        let mut game = if let Some(fen) = fen {
            fen.as_ref().parse()?
        } else {
            Game::default()
        };

        // Since this is a new position, it has a new history
        self.prev_positions.clear();

        // Apply the provided moves
        for mv_str in moves {
            let mv = Move::from_uci(&game, mv_str.as_ref())?;
            self.make_move(&mut game, mv);
        }

        Ok(game)
    }

    /// Executes the `psqt` command, printing the piece-square table info for the provided piece.
    fn psqt<V: Variant>(
        &self,
        game: &Game<V>,
        piece: Piece,
        square: Option<Square>,
        endgame_weight: Option<i32>,
    ) {
        // Compute the current endgame weight, if it wasn't provided
        let weight = endgame_weight.unwrap_or(game.evaluator().endgame_weight());
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
    fn start_search<Log: LogLevel, V: Variant>(
        &mut self,
        game: Game<V>,
        config: SearchConfig,
    ) -> Option<JoinHandle<SearchResult>> {
        // Cannot start a search if one is already running
        if self.is_searching() {
            Self::send_string("A search is already running");
            return None;
        }
        self.set_is_searching(true);

        // Clone the parameters that will be sent into the thread
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
            let mut ttable = ttable
                .lock()
                .expect("Failed to acquire Transposition Table at the start of search.");
            let mut history = history
                .lock()
                .expect("Failed to acquire History Table at the start of search.");

            // Start the search, returning the result when completed.
            Search::<Log, V>::new(
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

                let Ok(mb) = value.parse() else {
                    bail!("expected integer. got {value:?}");
                };

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

                let Ok(enabled) = value.parse() else {
                    bail!("expected bool. got {value:?}");
                };

                let v = if enabled {
                    GameVariant::Chess960
                } else {
                    GameVariant::Standard
                };

                self.send_command(EngineCommand::ChangeVariant { variant: Some(v) });
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
    fn get_option<V: Variant>(&self, name: &str) -> Option<String> {
        let value = match name {
            "Clear Hash" => String::default(),

            "Hash" => format!("{}", self.ttable().size()),

            "Threads" => String::from("1"),

            "UCI_Chess960" => format!("{}", V::variant() == GameVariant::Chess960),
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
