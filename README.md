# Toad - A UCI-compatible toy chess engine

Toad is a work-in-progress [chess engine](https://en.wikipedia.org/wiki/Chess_engine), and serves as my personal excuse to write fun code in Rust.
It is built upon my [`chessie`](https://crates.io/crates/chessie) crate, which is the "core" library that handles move generation and all other rules of chess.

Up for a game? Play against Toad on [Lichess](https://lichess.org/@/toad-bot)!

## Overview

By default, running Toad will cause it to print its version and authors and await input via `stdin`.
For convenience, you can run any of Toad's commands on startup and Toad will exit immediately after that command's execution.
To run multiple commands on startup, pass them in with the `-c "<command>"` flag.
You can pass in the `--no-exit` flag to continue execution after the command(s) have finished executing.
Run the engine and execute the `help` command to see a list of available commands, and `--help` to view all CLI flags and arguments.

### UCI

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
  bench       Run a benchmark with the provided parameters
  display     Print a visual representation of the current board state
  eval        Print an evaluation of the current position
  exit        Quit the engine
  fen         Generate and print a FEN string for the current position
  flip        Flips the side-to-move. Equivalent to playing a nullmove
  hashinfo    Display information about the current hash table(s) in the engine
  makemove    Apply the provided move to the game, if possible
  moves       Shows all legal moves in the current position
  option      Display the current value of the specified option
  perft       Performs a perft on the current position at the supplied depth, printing total node count
  psqt        Outputs the Piece-Square table value for the provided piece at the provided square, scaled with the endgame weight
  splitperft  Performs a split perft on the current position at the supplied depth
  help        Print this message or the help of the given subcommand(s)
```

For specifics on how a command works, run `toad <COMMAND> --help`

### UCI Options

|     Name     | Values      | Default | Description                               |
| :----------: | ----------- | ------- | ----------------------------------------- |
| `Clear Hash` |             |         | Clear the hash table(s)                   |
|    `Hash`    | `[1, 1024]` | `16`    | Set the size (in MB) of the hash table(s) |

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

**Note**: At this time, development and testing has been done solely on Linux (Ubuntu).
If you are willing to test the installation and execution of Toad on other operating systems, please provide feedback!

## Features

-   [Bitboard representation](https://www.chessprogramming.org/Bitboards).
-   [Magic Bitboards](https://www.chessprogramming.org/Magic_Bitboards) for sliding piece attacks.
-   Search:
    -   Based on the [Negamax](https://www.chessprogramming.org/Negamax) algorithm.
    -   [Alpha-Beta Pruning](https://www.chessprogramming.org/Alpha-Beta#Negamax_Framework) in a fail soft framework.
    -   [Time Management](https://www.chessprogramming.org/Time_Management) with soft and hard timeouts.
    -   [Quiescence Search](https://www.chessprogramming.org/Quiescence_Search) in a fail soft framework.
    -   [Draw detection](https://www.chessprogramming.org/Draw) through insufficient material, 2-fold repetition, and the 50-move rule.
    -   [Transposition Table](https://www.chessprogramming.org/Transposition_Table)
    -   [Principal Variation Search](https://www.chessprogramming.org/Principal_Variation_Search)
    -   [Aspiration Windows](https://www.chessprogramming.org/Aspiration_Windows) with [gradual widening](https://www.chessprogramming.org/Aspiration_Windows#Gradual_Widening)
    -   Move Ordering:
        -   [MVV-LVA](https://www.chessprogramming.org/MVV-LVA) with relative piece values `K < P < N < B < R < Q`, so `KxR` is ordered before `PxR`.
        -   [Hash moves](https://www.chessprogramming.org/Hash_Move)
-   Evaluation:
    -   [Hand-Crafted Evaluation (HCE)](https://www.chessprogramming.org/Evaluation)
        -   [Material difference](https://www.chessprogramming.org/Material)
        -   [Piece-Square Tables](https://www.chessprogramming.org/Piece-Square_Tables) with initial values from [PeSTO](https://www.chessprogramming.org/PeSTO%27s_Evaluation_Function#Source_Code)
        -   [Tapered Evaluation](https://www.chessprogramming.org/Tapered_Eval)

More features will be added as development continues! You can see most of my future plans in the [backlog](https://github.com/dannyhammer/toad/issues).

## Acknowledgements

More people have helped me on this journey than I can track, but I'll name a few notable resources/people here:

-   [Sebastian Lague](https://www.youtube.com/@SebastianLague), for his [chess programming series](https://www.youtube.com/watch?v=_vqlIPDR2TU&list=PLFt_AvWsXl0cvHyu32ajwh2qU1i6hl77c) on YouTube that ultimate inspired me to do this project.
-   The [Chess Programming Wiki](https://www.chessprogramming.org/), and all those who contribute to free, open-source knowledge.
-   The folks over at the [Engine Programming Discord](https://discord.com/invite/F6W6mMsTGN), for their patience with my silly questions and invaluable help overall.
-   [Analog-Hors](https://github.com/analog-hors), for an excellent [article on magic bitboards](https://analog-hors.github.io/site/magic-bitboards/)
-   The authors of [viridithas](https://github.com/cosmobobak/viridithas/) and [Stormphrax](https://github.com/Ciekce/Stormphrax), for allowing their engines to be open source and for answering all my silly questions.
-   [Andrew Grant](https://github.com/AndyGrant/) for creating [OpenBench](https://github.com/AndyGrant/OpenBench) and being willing to help me with its setup and use.
