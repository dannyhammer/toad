# Toad 🐸 A UCI-compatible toy chess engine

Toad is a work-in-progress [chess engine](https://en.wikipedia.org/wiki/Chess_engine), and serves as my personal excuse to write fun code in Rust.
Development progress is recorded automatically in the [changelog](./CHANGELOG.md).
All progression/non-regression testing is done through [OpenBench](https://github.com/AndyGrant/OpenBench) instance hosted [here](https://pyronomy.pythonanywhere.com/index/).
Strength of the latest version can be found on the [CCRL pages](https://computerchess.org.uk/ccrl/404/cgi/compare_engines.cgi?family=Toad&print=Rating+list&print=Results+table&print=LOS+table&print=Ponder+hit+table&print=Eval+difference+table&print=Comopp+gamenum+table&print=Overlap+table&print=Score+with+common+opponents).

Up for a game? Play against Toad on [Lichess](https://lichess.org/@/toad-bot)!

## Overview

Being a chess engine, Toad is CLI application that, by default, will await commands via `stdin`.
Toad does not have a GUI.
There are some [commands](#custom-commands) you can use to view the board state and make moves, but Toad's primary use case is to be paired with a GUI or match runner like [en croissant](https://encroissant.org/) or [cutechess](https://github.com/cutechess/cutechess).
For convenience, you can run any of Toad's commands on startup and Toad will exit immediately after that command's execution.
To run multiple commands on startup, pass them in with the `-c "<command>"` flag.
You can pass in the `--no-exit` flag to continue execution after the command(s) have finished executing.
Run the engine and execute the `help` command to see a list of available commands, and `--help` to view all CLI flags and arguments.

## Running

To run Toad, head over to the [releases](https://github.com/dannyhammer/toad/releases) page to grab the latest pre-compiled release for your platform.
Alternatively, you can build from source:

1. Ensure you have [Rust](https://www.rust-lang.org/) and [Cargo](https://doc.rust-lang.org/cargo/) installed.
2. Clone this repository:
    ```sh
    git clone git@github.com:dannyhammer/toad.git
    ```
3. Build and run!
    ```sh
    cargo run --release
    ```
    or run `make` to generate an executable named `toad-<version>`.
    ```sh
    make
    ./toad-<version>
    ```

**Note**: Development has primarily been done on Linux (Ubuntu 22.04), with minimal testing on Windows 10 and MacOS.

## Features

-   Core:
    -   [Bitboard representation](https://www.chessprogramming.org/Bitboards).
    -   [Magic Bitboards](https://www.chessprogramming.org/Magic_Bitboards) for sliding piece attacks.
    -   [Repetition](https://www.chessprogramming.org/Repetitions) detection through [Zobrist Hashing](https://www.chessprogramming.org/Zobrist_Hashing).
    -   [Chess960](https://en.wikipedia.org/wiki/Fischer_random_chess) support via the `UCI_Chess960` option or `changevariant` command.
-   Search:
    -   Based on the [Negamax](https://www.chessprogramming.org/Negamax) algorithm.
    -   [Alpha-Beta Pruning](https://www.chessprogramming.org/Alpha-Beta#Negamax_Framework) in a fail soft framework.
    -   [Time Management](https://www.chessprogramming.org/Time_Management) with soft and hard timeouts.
    -   [Quiescence Search](https://www.chessprogramming.org/Quiescence_Search) in a fail soft framework.
    -   [Draw detection](https://www.chessprogramming.org/Draw) through insufficient material, 2-fold repetition, and the 50-move rule.
    -   [Transposition Table](https://www.chessprogramming.org/Transposition_Table) for move ordering and [cutoffs](https://www.chessprogramming.org/Transposition_Table#Transposition_Table_Cutoffs).
    -   [Principal Variation Search](https://www.chessprogramming.org/Principal_Variation_Search).
    -   [Aspiration Windows](https://www.chessprogramming.org/Aspiration_Windows) with [gradual widening](https://www.chessprogramming.org/Aspiration_Windows#Gradual_Widening).
    -   [Null Move Pruning](https://www.chessprogramming.org/Null_Move_Pruning).
    -   [Reverse Futility Pruning](https://www.chessprogramming.org/Reverse_Futility_Pruning).
    -   [Late Move Reductions](https://www.chessprogramming.org/Late_Move_Reductions).
    -   [Check Extensions](https://www.chessprogramming.org/Check_Extensions).
    -   [Razoring](https://www.chessprogramming.org/Razoring).
    -   [Internal Iterative Reductions (Transposition Table Reductions)](https://www.chessprogramming.org/Internal_Iterative_Reductions).
    -   [Internal Iterative Deepening](https://www.chessprogramming.org/Internal_Iterative_Deepening).
    -   [Late Move Pruning](https://www.chessprogramming.org/Futility_Pruning#MoveCountBasedPruning).
    -   [Mate Distance Pruning](https://www.chessprogramming.org/Mate_Distance_Pruning).
    -   Support for [fractional plies](https://www.chessprogramming.org/Depth#Fractional_Plies).
    -   Move Ordering:
        -   [MVV-LVA](https://www.chessprogramming.org/MVV-LVA) with relative piece values `K < P < N < B < R < Q`, so `KxR` is ordered before `PxR`.
        -   [Hash moves](https://www.chessprogramming.org/Hash_Move).
        -   [History Heuristic](https://www.chessprogramming.org/History_Heuristic).
-   Evaluation:
    -   [Hand-Crafted Evaluation (HCE)](https://www.chessprogramming.org/Evaluation).
        -   [Material difference](https://www.chessprogramming.org/Material).
        -   [Piece-Square Tables](https://www.chessprogramming.org/Piece-Square_Tables) with initial values from [PeSTO](https://www.chessprogramming.org/PeSTO%27s_Evaluation_Function#Source_Code).
        -   [Tapered Evaluation](https://www.chessprogramming.org/Tapered_Eval).
        -   [Incrementally-updated board evaluation](https://www.chessprogramming.org/Incremental_Updates).

More features will be added as development continues! You can see most of my future plans in the [backlog](https://github.com/dannyhammer/toad/issues).

### UCI Commands

Toad abides (mostly) by the [Universal Chess Interface](https://backscattering.de/chess/uci/) protocol, and communicates through `stdin` and `stdout`.
The parsing of UCI commands and responses is handled by my [`uci-parser`](https://crates.io/crates/uci-parser) crate.

The following UCI commands (and arguments) are supported:

-   `uci`
-   `debug [ on | off ]`
-   `isready`
-   `setoption name <x> [value <y>]`
-   `ucinewgame`
-   `position [fen <fenstring> | startpos] [moves <move_1> ... <move_i>]`
    -   Extended to include [`position kiwipete`](https://www.chessprogramming.org/Perft_Results#Position_2).
-   `go wtime <x> btime <x> winc <x> binc <x> depth <x> nodes <x> movetime <x> infinite`
    -   Extended to include `go perft <x>`
-   `stop`
-   `quit`

### Custom Commands

In addition to the above UCI commands, Toad also supports the following custom commands:

```
Commands:
  bench          Run a benchmark with the provided parameters
  changevariant  Change the variant of chess being played, or display the current variant
  display        Print a visual representation of the current board state
  eval           Print an evaluation of the current position
  exit           Quit the engine
  fen            Generate and print a FEN string for the current position
  flip           Flips the side-to-move. Equivalent to playing a nullmove
  hashinfo       Display information about the current hash table(s) in the engine
  makemove       Apply the provided move to the game, if possible
  moves          Shows all legal moves in the current position, or for a specific piece
  option         Display the current value of the specified option
  perft          Performs a perft on the current position at the supplied depth, printing total node count
  place          Place a piece on the provided square
  psqt           Outputs the Piece-Square table value for the provided piece at the provided square, scaled with the endgame weight
  splitperft     Performs a split perft on the current position at the supplied depth
  take           Remove the piece at the provided square
  wait           Await the current search, blocking until it completes
  help           Print this message or the help of the given subcommand(s)
```

For specifics on how a command works, run `toad <COMMAND> --help`

### UCI Options

| Name           | Values          | Default | Description                                                                       |
| -------------- | --------------- | ------- | --------------------------------------------------------------------------------- |
| `Clear Hash`   |                 |         | Clear the hash table(s)                                                           |
| `Hash`         | `1..=1024`      | `16`    | Set the size (in MB) of the hash table(s)                                         |
| `Threads`      | `1..=1`         | `1`     | Only implemented for use with [OpenBench](https://github.com/AndyGrant/OpenBench) |
| `UCI_Chess960` | `true`, `false` | `false` | Enable support for [Chess960](https://en.wikipedia.org/wiki/Fischer_random_chess) |

## Acknowledgements

More people have helped me on this journey than I can track, but I'll name a few notable resources/people here:

-   [Sebastian Lague](https://www.youtube.com/@SebastianLague), for his [chess programming series](https://www.youtube.com/watch?v=_vqlIPDR2TU&list=PLFt_AvWsXl0cvHyu32ajwh2qU1i6hl77c) on YouTube, which was the original inspiration for this project.
-   The [Chess Programming Wiki](https://www.chessprogramming.org/), and all those who contribute to free, open-source knowledge.
-   The folks over at the [Engine Programming Discord](https://discord.com/invite/F6W6mMsTGN), for their patience with my silly questions and invaluable help overall.
-   [Analog-Hors](https://github.com/analog-hors), for an excellent [article on magic bitboards](https://analog-hors.github.io/site/magic-bitboards/)
-   The authors of [viridithas](https://github.com/cosmobobak/viridithas/) and [Stormphrax](https://github.com/Ciekce/Stormphrax), for allowing their engines to be open source and for answering all my silly questions.
-   [Andrew Grant](https://github.com/AndyGrant/) for creating [OpenBench](https://github.com/AndyGrant/OpenBench) and being willing to help me with its setup and use.
-   All those in the engine testing community, with special thanks for those who manage and host the [CCRL pages](https://computerchess.org.uk/ccrl/).
-   The authors of [Yukari](https://github.com/yukarichess/yukari) for motivation through friendly competition.
-   [Paul T](https://github.com/DeveloperPaul123), for feedback on my [`uci-parser`](https://crates.io/crates/uci-parser) crate.
-   All of the friendly devs over at [PyroBench](https://pyronomy.pythonanywhere.com), both for the friendly camaraderie and for sharing compute for testing.
