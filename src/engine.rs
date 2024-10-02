use std::{
    io,
    path::Path,
    sync::{
        atomic::{AtomicBool, Ordering},
        mpsc::{self, Receiver, Sender},
        Arc,
    },
};

use anyhow::{anyhow, bail, Context, Result};
use chessie::{print_perft, Game, Move};
use std::thread;
use uci_parser::UciCommand;

use crate::{Evaluator, Search, SearchConfig, MAX_DEPTH};

const BENCH_DEPTH: usize = 3;
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
    sender: Sender<EngineCommand>,

    /// One half of a channel, responsible for receiving commands for the engine to execute.
    receiver: Receiver<EngineCommand>,

    /// Atomic flag to determine whether a search is currently running
    is_searching: Arc<AtomicBool>,
}

impl Engine {
    /// Constructs a new [`Engine`] instance to be ran with [`Engine::run`].
    pub fn new() -> Self {
        // Construct a channel for communication and threadpool for parallel tasks
        let (sender, receiver) = mpsc::channel();

        Self {
            game: Game::default(),
            sender,
            receiver,
            is_searching: Arc::default(),
        }
    }

    /// Sends an [`EngineCommand`] to the engine to be executed.
    pub fn send_command(&self, command: EngineCommand) -> Result<()> {
        self.sender
            .send(command)
            .map_err(|err| anyhow!("Failed to send {:?} to engine", err.0))
    }

    /// Execute the main event loop for the engine.
    ///
    /// This function spawns a thread to handle input from `stdin` and waits on received commands.
    pub fn run(mut self) -> Result<()> {
        // Spawn a separate thread for handling user input
        let sender = self.sender.clone();
        thread::spawn(|| {
            if let Err(err) = input_handler(sender) {
                eprintln!("{err}");
            }
        });

        // Loop on user input
        while let Ok(cmd) = self.receiver.recv() {
            match cmd {
                EngineCommand::Display => self.display(),

                EngineCommand::Eval(pretty) => self.eval(pretty),

                EngineCommand::Fen => println!("{}", self.game.to_fen()),
                EngineCommand::Flip => self.game.make_move(Move::nullmove()),

                EngineCommand::Perft(depth) => {
                    print_perft::<false, false>(&self.game, depth);
                }

                // Exit the loop so the engine can quit
                EngineCommand::Quit | EngineCommand::UciCommand(UciCommand::Quit) => break,

                EngineCommand::UciCommand(uci) => {
                    // Keep running, even on error
                    if let Err(e) = self.handle_uci_command(uci) {
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
            Uci => println!("uciok"),

            // Debug(status) => self.debug.store(status, Ordering::Relaxed),
            IsReady => println!("readyok"),

            // SetOption { name, value } => {}

            // Register { name, code } => {}
            UciNewGame => self.new_game(),

            Position { fen, moves } => self.set_position(fen, moves)?,

            Go(options) => {
                if let Some(depth) = options.perft {
                    print_perft::<false, true>(&self.game, depth as usize);
                    return Ok(());
                }

                self.start_search::<false>(SearchConfig::from(options));
            }

            Stop => self.set_is_searching(false),

            // PonderHit => self.ponderhit(),
            Quit => unreachable!("UCI command `Quit` is handled in main event pump"),

            Bench(options) => self.bench(SearchConfig::from(options)),

            _ => eprintln!(
                "{} does not support UCI command {uci:?}",
                env!("CARGO_PKG_NAME")
            ),
        }

        Ok(())
    }

    /// Execute the `bench` command, running a benchmark of a fixed search on a series of positions and displaying the results.
    fn bench(&mut self, mut config: SearchConfig) {
        // TODO: Find a better way to check this
        if config.max_depth == MAX_DEPTH {
            config.max_depth = BENCH_DEPTH;
        }

        let file = Path::new(DEFAULT_BENCH_FILE);
        eprintln!("Running bench suite on {file:?} with config: {config:?}\n");

        let mut possible_nodes = 0;
        let mut nodes = 0;
        let benches = std::fs::read_to_string(file).expect("Failed to read bench file");
        let num_positions = benches.chars().filter(|&c| c == '\n').count();

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
                // TODO: This will fail if depth > 6.
                let new_nodes = tests[0..i].iter().fold(0, |acc, n| {
                    acc + n.get(2..).unwrap().trim().parse::<u64>().unwrap()
                });

                possible_nodes += new_nodes;
            }

            println!("Benchmark position {}/{num_positions}: {fen:?}", i + 1);
            self.game = Game::from_fen(fen).unwrap();
            nodes += self.start_search::<true>(config);
        }

        // Compute results
        let elapsed = config.starttime.elapsed();
        let nps = (nodes as f32 / elapsed.as_secs_f32()) as u64;
        let ms = elapsed.as_millis();
        let prune = ((possible_nodes - nodes) as f32 / possible_nodes as f32) * 100.0;

        // Display the results in a nice table
        println!("");
        println!("+----- Benchmark Complete -----+");
        println!("| time (ms)      : {ms:<12}|");
        println!("| nodes          : {nodes:<12}|");
        println!("| nps            : {nps:<12}|");
        println!("| prune rate (%) : {prune:<12.2}|");
        println!("+------------------------------+");

        self.set_is_searching(false);
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
    fn set_position<T: AsRef<str>>(
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
    fn new_game(&mut self) {
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

    /// Starts a search on the current position
    fn start_search<const BENCH: bool>(&mut self, config: SearchConfig) -> u64 {
        // Cannot start a search if one is already running
        if self.is_searching() {
            eprintln!("A search is already running");
            return 0;
        }
        self.set_is_searching(true);

        // Clone the parameters that will be sent into the thread
        let game = self.game;
        let sender = self.sender.clone();
        let is_searching = Arc::clone(&self.is_searching);

        // Spawn a thread to conduct the search
        let search_thread = thread::spawn(move || {
            let search = Search::new(&game, sender, is_searching, config);

            // Launch the search, performing iterative deepening, negamax, etc.
            let res = search.start();

            // Search has ended; send bestmove
            // TODO: Proper UCI types for bestmove in `uci-parser`
            if let Some(mv) = res.bestmove {
                println!("bestmove {mv}");
            } else {
                println!("bestmove (none)");
            }

            res.nodes
        });

        // If this is a benchmark, we need to wait until the search is completed, then return the node count
        let nodes = if BENCH {
            let nodes = search_thread.join().expect("Benchmarking can never fail");
            self.set_is_searching(false);
            nodes
        } else {
            0
        };

        nodes
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

/// A command to be sent to the engine.
#[derive(Debug)]
pub enum EngineCommand {
    /// Print an evaluation of the current position.
    Eval(bool),

    /// Generate and print a FEN string for the current position.
    Fen,

    /// Flips the side-to-move.
    Flip,

    /// Performs a perft on the current position at the supplied depth, printing total node count.
    Perft(usize),

    /// Quit the program.
    Quit,

    /// Print a visual representation of the current board state.
    Display,

    /// Wrapper for UCI commands from the GUI to the Engine.
    UciCommand(UciCommand),
}

/// Attempts to parse user input into an [`EngineCommand`].
pub fn parse_input(input: &str) -> Result<EngineCommand> {
    // Split by command and args, if possible.
    let (cmd, args) = input.split_once(' ').unwrap_or((input, ""));

    match cmd {
        "d" | "display" => Ok(EngineCommand::Display),
        "e" | "eval" => parse_eval(args),
        "fen" => Ok(EngineCommand::Fen),
        "flip" => Ok(EngineCommand::Flip),
        "perft" => parse_perft(args),
        "quit" | "exit" => Ok(EngineCommand::Quit),
        _ => UciCommand::new(input)
            .map_err(|_| anyhow!("Unknown command: {input:?}"))
            .map(EngineCommand::UciCommand),
    }
}

/// Attempts to parse the arguments to [`EngineCommand::Eval`].
fn parse_eval(args: &str) -> Result<EngineCommand> {
    let pretty = if args.is_empty() {
        false
    } else if args == "pretty" {
        true
    } else {
        bail!("usage: eval [pretty]")
    };

    Ok(EngineCommand::Eval(pretty))
}

/// Attempts to parse the arguments to [`EngineCommand::Perft`].
fn parse_perft(args: &str) -> Result<EngineCommand> {
    let Ok(depth) = args.parse() else {
        bail!("usage: perft <depth>")
    };

    Ok(EngineCommand::Perft(depth))
}

/// Loops endlessly to await input via `stdin`, sending all successfully-parsed commands through the supplied `sender`.
fn input_handler(sender: Sender<EngineCommand>) -> Result<()> {
    let mut buffer = String::with_capacity(2048);

    loop {
        // Clear the buffer, read input, and trim the trailing newline
        buffer.clear();
        let bytes = io::stdin()
            .read_line(&mut buffer)
            .context("Failed to read line when parsing UCI commands")?;

        // For ctrl + d
        if 0 == bytes {
            eprintln!("Engine received input of 0 bytes and is quitting");
            sender
                .send(EngineCommand::Quit)
                .context("Failed to send 'quit' command after receiving empty input")?;
            return Ok(());
        }

        // Trim any leading/trailing whitespace
        let buf = buffer.trim();

        // Ignore empty lines
        if buf.is_empty() {
            continue;
        }

        // Attempt to parse the user input
        match parse_input(buf) {
            // If successful, send the command to the engine
            Ok(cmd) => sender
                .send(cmd)
                .context("Failed to send command {buffer:?} to engine")?,

            // If an invalid command was received, we want to continue running
            Err(err) => eprintln!("{err}"),
        }
    }
}
