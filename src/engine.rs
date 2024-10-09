/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{
    io,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::{channel, Receiver, Sender},
        Arc,
    },
    thread::{self, JoinHandle},
};

use anyhow::{bail, Context, Result};
use chessie::{print_perft, Game, Move};
use clap::Parser;
use uci_parser::{UciCommand, UciOption, UciParseError, UciResponse};

use crate::{EngineCommand, Evaluator, Search, SearchConfig, SearchResult, BENCHMARK_FENS};

/// Default depth at which to run the benchmark searches.
const BENCH_DEPTH: usize = 7;

/// The Toad chess engine.
#[derive(Debug)]
pub struct Engine {
    /// The current state of the chess board, as known to the engine.
    ///
    /// This is modified whenever moves are played or new positions are given,
    /// and is reset whenever the engine is told to start a new game.
    game: Game,

    /// One half of a channel, responsible for sending commands to the engine to execute.
    sender: Sender<EngineCommand>,

    /// One half of a channel, responsible for receiving commands for the engine to execute.
    receiver: Receiver<EngineCommand>,

    /// Atomic flag to determine whether a search is currently running
    is_searching: Arc<AtomicBool>,

    /// Handle to the currently-running search thread, if one exists.
    search_thread: Option<JoinHandle<SearchResult>>,
}

impl Engine {
    /// Constructs a new [`Engine`] instance to be executed with [`Engine::run`].
    pub fn new() -> Self {
        // Construct a channel for communication and threadpool for parallel tasks
        let (sender, receiver) = channel();

        Self {
            game: Game::default(),
            sender,
            receiver,
            is_searching: Arc::default(),
            search_thread: None,
        }
    }

    /// Returns a string of the engine's name and current version.
    pub fn name(&self) -> String {
        format!("{} {}", env!("CARGO_PKG_NAME"), env!("CARGO_PKG_VERSION"))
    }

    /// Returns a string of all authors of this engine.
    pub fn authors(&self) -> String {
        // Split multiple authors by comma-space
        env!("CARGO_PKG_AUTHORS").replace(':', ", ").to_string()
    }

    /// Sends an [`EngineCommand`] to the engine to be executed.
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
                eprintln!("Input handler thread stopping after fatal error: {err}");
            }
        });

        // Loop on user input
        while let Ok(cmd) = self.receiver.recv() {
            match cmd {
                EngineCommand::Bench { depth, pretty } => self.bench(depth, pretty)?,

                EngineCommand::Display => self.display(),

                EngineCommand::Eval { pretty } => self.eval(pretty),

                EngineCommand::Fen => println!("{}", self.game.to_fen()),

                EngineCommand::Flip => self.game.toggle_side_to_move(),

                EngineCommand::Moves { square } => {
                    // Get the legal moves
                    let moves = if let Some(square) = square {
                        self.game.get_legal_moves_from(square.into())
                    } else {
                        self.game.get_legal_moves()
                    };

                    // If there are none, print "(none)"
                    let moves_string = if moves.is_empty() {
                        String::from("(none)")
                    } else {
                        // Otherwise, join them by comma-space
                        moves
                            .into_iter()
                            .map(|mv| mv.to_string())
                            .collect::<Vec<_>>()
                            .join(", ")
                    };
                    println!("{moves_string}");
                }

                EngineCommand::Option { name } => {
                    if let Some(value) = self.get_option(&name) {
                        println!("{name} := {value}");
                    } else {
                        println!("{} has no option {name:?}", self.name());
                    }
                }

                EngineCommand::Perft { depth } => {
                    print_perft::<false, false>(&self.game, depth);
                }

                EngineCommand::Splitperft { depth } => {
                    print_perft::<false, true>(&self.game, depth);
                }

                EngineCommand::Exit { cleanup } => {
                    // If requested, await the completion of any ongoing search threads
                    if cleanup {
                        self.stop_search();
                    }

                    // Exit the loop so the engine can quit
                    break;
                }

                EngineCommand::Uci { cmd } => {
                    // Keep running, even on error
                    if let Err(e) = self.handle_uci_command(cmd) {
                        eprintln!("Error: {e}");
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

            // Debug(status) => self.debug.store(status, Ordering::Relaxed),
            IsReady => println!("{}", UciResponse::<&str>::ReadyOk),

            SetOption { name, value } => self.set_option(&name, value)?,

            // Register { name, code } => {}
            UciNewGame => self.new_game(),

            Position { fen, moves } => self.position(fen, moves)?,

            Go(options) => {
                if let Some(depth) = options.perft {
                    print_perft::<false, true>(&self.game, depth as usize);
                    return Ok(());
                }

                self.search_thread = self.start_search(SearchConfig::new(options, &self.game));
            }

            Stop => self.set_is_searching(false),

            // PonderHit => self.ponderhit(),
            Quit => self.send_command(EngineCommand::Exit { cleanup: false }),

            _ => bail!(
                "{} does not support UCI command {uci:?}",
                env!("CARGO_PKG_NAME")
            ),
        }

        Ok(())
    }

    /// Execute the `bench` command, running a benchmark of a fixed search on a series of positions and displaying the results.
    fn bench(&mut self, depth: Option<usize>, pretty: bool) -> Result<()> {
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
            self.game = Game::from_fen(fen)?;
            self.search_thread = self.start_search(config);

            // Await the search, appending the node count once concluded.
            let res = self.stop_search().unwrap();
            nodes += res.nodes;
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
        println!("{}", self.game);
    }

    /// Executes the `eval` command, printing an evaluation of the current position.
    fn eval(&self, pretty: bool) {
        let evaluator = Evaluator::new(&self.game);
        if pretty {
            print!("{evaluator}\n\nScore: ");
        }

        println!("{}", evaluator.eval());
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

        // Apply the provided moves
        for mv_str in moves {
            let mv = Move::from_uci(&self.game, mv_str.as_ref())?;
            self.game.make_move(mv);
        }

        Ok(())
    }

    /// Resets the engine's internal game state.
    ///
    /// This clears all internal caches and hash tables, as well as search history.
    /// It also cancels any ongoing searches, ignoring their results.
    fn new_game(&mut self) {
        self.set_is_searching(false);
        self.game = Game::default();
    }

    /// Sets the search flag to signal that the engine is starting/stopping a search.
    fn set_is_searching(&mut self, status: bool) {
        self.is_searching.store(status, Ordering::Relaxed);
    }

    /// Returns `true` if the engine is currently executing a searching.
    fn is_searching(&self) -> bool {
        self.is_searching.load(Ordering::Relaxed)
    }

    /// Starts a search on the current position, given the parameters in `config`.
    fn start_search(&mut self, config: SearchConfig) -> Option<JoinHandle<SearchResult>> {
        // Cannot start a search if one is already running
        if self.is_searching() {
            eprintln!("A search is already running");
            return None;
        }
        self.set_is_searching(true);

        // Clone the parameters that will be sent into the thread
        let game = self.game;
        let is_searching = Arc::clone(&self.is_searching);

        // Spawn a thread to conduct the search
        let handle = thread::spawn(move || {
            // Launch the search, performing iterative deepening, negamax, a/b pruning, etc.
            Search::new(&game, is_searching.clone(), config).start()
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
            eprintln!("Failed to join on thread {id:?}");
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
    fn options(&self) -> impl Iterator<Item = UciOption<&str>> {
        [
            UciOption::spin("Threads", 1, 1, 1),
            UciOption::spin("Hash", 1, 1, 1),
        ]
        .into_iter()
    }

    /// Handles the `setoption` command, setting option `name` to `value`, or toggling it if `value` is None.
    ///
    /// Will return an error if `name` isn't a valid option or `value` is not a valid value for that option.
    fn set_option(&mut self, name: &str, _value: Option<String>) -> Result<()> {
        match name {
            "Hash" => bail!("{} currently has no hash tables", self.name()),
            "Threads" => bail!("{} currently supports only 1 thread", self.name()),
            _ => bail!("{} has no option named {name:?}", self.name()),
        }

        // if let Some(value) = value.as_ref() {
        //     eprintln!("Option {name} has been set to {value}")
        // } else {
        //     eprintln!("Option {name} has been set")
        // }

        // Ok(())
    }

    /// Returns the current value of the option `name`, if it exists on this engine.
    fn get_option(&self, name: &str) -> Option<String> {
        let opt = self.options().find(|opt| opt.name == name)?;
        let value = match opt.name {
            "Hash" => String::from("1"),
            "Threads" => String::from("1"),
            _ => unreachable!(),
        };

        Some(value)
    }

    // fn send_info_string<T: fmt::Display>(&self, info: T) {
    //     let resp = UciResponse::<T>::Info(Box::new(UciInfo::new().string(info)));
    //     println!("{resp}");
    // }
}

impl Default for Engine {
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
                match EngineCommand::try_parse_from(buf.split_ascii_whitespace()) {
                    Ok(cmd) => sender
                        .send(cmd)
                        .context("Failed to send command to engine")?,

                    // If it wasn't a custom command, either, print an error.
                    Err(err) => eprintln!("{err}"),
                }
            }

            // If it was a UCI command, print a usage message.
            Err(uci_err) => eprintln!("{uci_err}"),
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
