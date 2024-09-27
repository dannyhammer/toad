use std::{
    io::{self, Write},
    sync::mpsc::{self, Receiver, Sender},
};

use anyhow::{bail, Context, Result};
use chessie::{print_perft, Game, Move};
use clap::{ArgAction, Parser, Subcommand, ValueEnum};
use threadpool::ThreadPool;

use super::Evaluator;

#[derive(Debug)]
pub struct Engine {
    /// The current state of the chess board, as known to the engine.
    ///
    /// This is modified whenever moves are played or new positions are given,
    /// and is reset whenever the engine is told to start a new game.
    game: Game,

    /// Threadpool for executing tasks in parallel.
    ///
    /// A thread is spawned for handling command-line input.
    pool: ThreadPool,

    /// One half of a channel, responsible for sending commands to the engine to execute.
    sender: Sender<EngineCommand>,

    /// One half of a channel, responsible for receiving commands for the engine to execute.
    receiver: Receiver<EngineCommand>,
}

impl Engine {
    pub fn new() -> Self {
        // Construct a channel for communication and threadpool for parallel tasks
        let (sender, receiver) = mpsc::channel();
        let pool = ThreadPool::with_name(
            format!("{} input / search thread(s)", env!("CARGO_PKG_NAME")),
            num_cpus::get(),
        );

        Self {
            game: Game::default(),
            pool,
            sender,
            receiver,
        }
    }

    pub fn send_command(&self, command: EngineCommand) -> Result<()> {
        self.sender.send(command)?;
        Ok(())
    }

    pub fn run(mut self) -> Result<()> {
        // Print some metadata about the engine
        let name = env!("CARGO_PKG_NAME");
        let version = env!("CARGO_PKG_VERSION");
        let authors = env!("CARGO_PKG_AUTHORS").replace(':', ", "); // Split multiple authors by comma-space
        println!("{name} {version} by {authors}");

        // Spawn a separate thread for handling user input
        let sender = self.sender.clone();
        self.pool.execute(|| {
            if let Err(err) = input_handler(sender) {
                eprintln!("{err}");
            }
        });

        // Loop on user input
        while let Ok(cmd) = self.receiver.recv() {
            match cmd {
                EngineCommand::Eval => println!("{}", Evaluator::new(&self.game).eval()),

                EngineCommand::Fen => println!("{}", self.game.to_fen()),

                EngineCommand::Perft { depth, split } => {
                    if split {
                        print_perft::<false, true>(&self.game, depth);
                    } else {
                        print_perft::<false, false>(&self.game, depth);
                    }
                }

                EngineCommand::Position {
                    startpos,
                    game,
                    moves,
                } => {
                    // eprintln!("REST: {rest:?}");
                    // self.game = fen;
                    // for mv_str in moves {
                    //     let mv = Move::from_uci(&self.game, &mv_str)?;
                    //     self.game.make_move(mv);
                    // }

                    eprintln!("Successfully set position to {startpos} and applied {moves:?}");
                }

                EngineCommand::Show => println!("{:?}", self.game.position()),

                // Exit the loop so the engine can quit
                EngineCommand::Quit => break,
            }
        }

        Ok(())
    }
}

#[derive(Debug, Parser)]
#[command(multicall = true, version, long_about = None)]
pub struct Cli {
    #[command(subcommand)]
    pub command: EngineCommand,
}

/// A command to be sent to the engine.
#[derive(Debug, Subcommand)]
#[command(about = "Send a command to the engine")]
pub enum EngineCommand {
    /// Print an evaluation of the current position.
    Eval,

    /// Generate a FEN string for the current position.
    Fen,

    /// Run a perft on the current position.
    Perft {
        /// Maximum depth of the perft.
        #[arg(default_value = "1")]
        depth: usize,

        /// Whether to display results as a splitperft.
        #[arg(short = 's', long = "split", default_value = "false")]
        split: bool,
    },

    /// Set the current position of the engine.
    // #[command(external_subcommand)]
    // Position(Vec<String>),
    #[command(allow_external_subcommands = true)]
    Position {
        /// The position of the game
        #[arg(requires_if("fen", "fen"))]
        startpos: String,

        #[arg(value_parser = parse_fen)]
        game: Option<Game>,

        // #[arg(required_if_eq("other", "special"))]
        // is_startpos: bool,
        #[arg(default_value = "", trailing_var_arg = true)]
        #[arg(num_args = 1..)]
        moves: Vec<String>,
    },

    /// Quit the program.
    #[command(alias = "exit")]
    Quit,

    /// Print a visual representation of the current board state.
    Show,
}

fn parse_fen(args: &str) -> Result<Game> {
    eprintln!("Parsing POSITION command arguments: {args:?}");
    if args.to_ascii_lowercase() == "startpos" {
        Ok(Game::default())
    } else {
        Game::from_fen(args)
    }
}

#[derive(ValueEnum, Clone)]
enum PositionCmdTypes {
    // #[value(alias = "")]
    Fen,

    #[value(alias = "fen rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1")]
    Startpos,
}

fn input_handler(sender: Sender<EngineCommand>) -> Result<()> {
    // eprintln!("Awaiting user input...");
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

        // Trim whitespace and split args appropriately
        let args = shlex::split(&buffer).context("Invalid quoting")?;

        // Ignore empty lines
        if args.is_empty() {
            continue;
        }

        // Attempt to parse the user input
        let cmd = match Cli::try_parse_from(args) {
            Ok(cmd) => cmd.command,

            // If an invalid command was received, we want to continue running
            Err(err) => {
                // eprintln!("Unknown command: {:?}", buffer.trim());
                // eprintln!("Full error:\n{err}");
                eprintln!("{err}");
                continue;
            }
        };

        sender
            .send(cmd)
            .context("Failed to send command {buffer:?} to engine")?;
    }
}
