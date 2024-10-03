/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use std::{
    io,
    str::FromStr,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::{channel, Receiver, Sender},
        Arc,
    },
    thread::JoinHandle,
};

use anyhow::{anyhow, bail, Context, Result};
use chessie::{print_perft, Game, Move};
use std::thread;
use uci_parser::{UciCommand, UciResponse};

use crate::{Evaluator, Search, SearchConfig, SearchResult};

/// Default depth at which to run the benchmark searches.
const BENCH_DEPTH: usize = 4;

/// Default file for benchmarking.
const DEFAULT_BENCH_FILE: &str = "benches/standard.epd";

/// The Toad chess engine.
#[derive(Debug)]
pub struct Engine {
    /// The current state of the chess board, as known to the engine.
    ///
    /// This is modified whenever moves are played or new positions are given,
    /// and is reset whenever the engine is told to start a new game.
    game: Game,

    /// One half of a channel, responsible for sending commands to the engine to execute.
    sender: Sender<Command>,

    /// One half of a channel, responsible for receiving commands for the engine to execute.
    receiver: Receiver<Command>,

    /// Atomic flag to determine whether a search is currently running
    is_searching: Arc<AtomicBool>,

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

    /// Sends an [`Command`] to the engine to be executed.
    pub fn send_command(&self, command: Command) {
        // Safe unwrap: `send` can only fail if it's corresponding receiver doesn't exist,
        //  and the only way our engine's `Receiver` can no longer exist is when our engine
        //  doesn't exist either, so this is always safe.
        self.sender.send(command).unwrap();
    }

    /// Execute the main event loop for the engine.
    ///
    /// This function spawns a thread to handle input from `stdin` and waits on received commands.
    pub fn run(mut self) -> Result<()> {
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
                Command::Bench { depth, file } => self.bench(depth, file)?,

                Command::Display => self.display(),

                Command::Eval { pretty } => self.eval(pretty),

                Command::Fen => println!("{}", self.game.to_fen()),

                Command::Flip => self.game.toggle_side_to_move(),

                Command::Perft { depth } => {
                    print_perft::<false, false>(&self.game, depth);
                }

                Command::Quit { cleanup } => {
                    // If requested, await the completion of any ongoing search threads
                    if cleanup {
                        self.stop_search();
                    }

                    // Exit the loop so the engine can quit
                    break;
                }

                Command::Uci(uci) => {
                    // Keep running, even on error
                    if let Err(e) = self.handle_uci_command(uci) {
                        eprintln!("Error: {e}");
                    }
                } // Command::UciResp(resp) => println!("{resp}"),
            };
        }

        Ok(())
    }

    /// Handle the execution of a single [`UciCommand`].
    fn handle_uci_command(&mut self, uci: UciCommand) -> Result<()> {
        use UciCommand::*;
        match uci {
            Uci => println!("{}", UciResponse::<&str>::UciOk),

            // Debug(status) => self.debug.store(status, Ordering::Relaxed),
            IsReady => println!("{}", UciResponse::<&str>::ReadyOk),

            // SetOption { name, value } => {}

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
            Quit => self.send_command(Command::Quit { cleanup: false }),

            _ => bail!(
                "{} does not support UCI command {uci:?}",
                env!("CARGO_PKG_NAME")
            ),
        }

        Ok(())
    }

    /// Execute the `bench` command, running a benchmark of a fixed search on a series of positions and displaying the results.
    fn bench(&mut self, depth: Option<usize>, file: Option<String>) -> Result<()> {
        // Set up the benchmarking config
        let config = SearchConfig {
            max_depth: depth.unwrap_or(BENCH_DEPTH),
            ..Default::default()
        };

        let file = file.unwrap_or(String::from(DEFAULT_BENCH_FILE));
        let Ok(benches) = std::fs::read_to_string(&file) else {
            bail!("Could not read benchmark file {file:?}")
        };
        let num_tests = benches.chars().filter(|&c| c == '\n').count();
        let mut possible_nodes = 0;
        let mut nodes = 0;

        // Run a fixed search on each position
        for (i, epd) in benches.lines().enumerate() {
            // Parse the FEN and the total node count
            let fen_end = epd.find(";").unwrap();
            let (fen, tests) = epd.split_at(fen_end);
            let tests = tests
                .split(";")
                .filter(|s| !s.is_empty())
                .collect::<Vec<_>>();

            // Count up the total number of nodes reachable from this position
            for i in 1..=config.max_depth {
                // Parse the node count, which is located after the depth value
                // TODO: This will fail if depth > 6 (the highest depth in the default file, standard.epd)
                let new_nodes = tests[0..i].iter().fold(0, |acc, n| {
                    acc + n.get(2..).unwrap().trim().parse::<u64>().unwrap()
                });

                possible_nodes += new_nodes + 1; // Add 1 per depth because we search the root node, too!
            }

            println!("Benchmark position {}/{}: {fen:?}", i + 1, num_tests + 1);

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
        let ms = elapsed.as_millis();
        let prune = ((possible_nodes - nodes) as f32 / possible_nodes as f32) * 100.0;

        // Display the results in a nice table
        println!();
        println!("+----- Benchmark Complete -----+");
        println!("| time (ms)      : {ms:<12}|");
        println!("| nodes          : {nodes:<12}|");
        println!("| nps            : {nps:<12}|");
        println!("| prune rate (%) : {prune:<12.2}|");
        println!("+------------------------------+");

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
            eprintln!("Failed to join on thread {id:?}",);
            return None;
        };

        // Flip the search flag so that any active threads will (hopefully) begin to clean themselves up.
        self.set_is_searching(false);

        Some(res)
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

/// A command to be sent to the engine.
#[derive(Debug, Clone)]
pub enum Command {
    /// Run a benchmark with the provided parameters.
    Bench {
        depth: Option<usize>,
        file: Option<String>,
    },

    /// Print a visual representation of the current board state.
    Display,

    /// Print an evaluation of the current position.
    Eval { pretty: bool },

    /// Generate and print a FEN string for the current position.
    Fen,

    /// Flips the side-to-move.
    Flip,

    /// Performs a perft on the current position at the supplied depth, printing total node count.
    Perft { depth: usize },

    /// Quit the program.
    Quit { cleanup: bool },

    /// Wrapper for UCI commands from the GUI to the Engine.
    Uci(UciCommand),
}

impl Command {
    /// Attempts to parse user input into an [`Command`].
    fn new(input: &str) -> Result<Self> {
        // Split by command and args, if possible.
        let (cmd, args) = input.split_once(' ').unwrap_or((input, ""));

        match cmd {
            "b" | "bench" => Self::parse_bench(args),
            "d" | "display" => Ok(Self::Display),
            "e" | "eval" => Self::parse_eval(args),
            "fen" => Ok(Self::Fen),
            "flip" => Ok(Self::Flip),
            "perft" => Self::parse_perft(args),
            "quiet" | "exit" => Self::parse_quit(args),
            _ => UciCommand::new(input)
                .map_err(|_| anyhow!("Unknown command: {input:?}"))
                .map(Self::Uci),
        }
    }

    /// Attempts to parse the arguments to [`Command::Eval`].
    fn parse_bench(args: &str) -> Result<Self> {
        let mut depth = None;
        let mut file = None;

        let mut args = args.split_ascii_whitespace();
        while let Some(arg) = args.next() {
            match arg {
                "depth" => {
                    depth = Some(
                        args.next()
                            .ok_or(anyhow!("usage: bench depth <n>"))?
                            .parse()?,
                    )
                }
                "file" => {
                    file = Some(String::from(
                        args.next().ok_or(anyhow!("usage: bench file <path>"))?,
                    ))
                }
                _ => bail!("usage: bench [depth <n>] [file <path>]"),
            }
        }

        Ok(Self::Bench { depth, file })
    }

    /// Attempts to parse the arguments to [`Command::Eval`].
    fn parse_eval(args: &str) -> Result<Self> {
        let pretty = if args.is_empty() {
            false
        } else if args == "pretty" {
            true
        } else {
            bail!("usage: eval [pretty]")
        };

        Ok(Self::Eval { pretty })
    }

    /// Attempts to parse the arguments to [`Command::Perft`].
    fn parse_perft(args: &str) -> Result<Self> {
        let Ok(depth) = args.parse() else {
            bail!("usage: perft <depth>")
        };

        Ok(Self::Perft { depth })
    }

    /// Attempts to parse the arguments to [`Command::Quit`].
    fn parse_quit(args: &str) -> Result<Self> {
        let Ok(cleanup) = args.parse() else {
            bail!("usage: quit [ true | false ]")
        };

        Ok(Self::Quit { cleanup })
    }
}

impl FromStr for Command {
    type Err = anyhow::Error;
    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        Self::new(s)
    }
}

/// Loops endlessly to await input via `stdin`, sending all successfully-parsed commands through the supplied `sender`.
fn input_handler(sender: Sender<Command>) -> Result<()> {
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
                .send(Command::Quit { cleanup: false })
                .context("Failed to send 'quit' command after receiving empty input")?;

            bail!("Engine received input of 0 bytes and is quitting");
        }

        // Trim any leading/trailing whitespace
        let buf = buffer.trim();

        // Ignore empty lines
        if buf.is_empty() {
            continue;
        }

        // Attempt to parse the user input
        match buf.parse() {
            // If successful, send the command to the engine
            Ok(cmd) => sender
                .send(cmd)
                .context("Failed to send command {buffer:?} to engine")?,

            // If an invalid command was received, just print the error and continue running
            Err(err) => eprintln!("{err}"),
        }
    }
}
