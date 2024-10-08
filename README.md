# Toad - A UCI chess engine, written in Rust.

Toad is a work-in-progress [chess engine](https://en.wikipedia.org/wiki/Chess_engine), and serves as my personal excuse to write fun code in Rust.
It is built upon [`chessie`](https://crates.io/crates/chessie), my chess crate which handles move generation and all other rules of chess.

## Overview

By default, Toad will print its version and authors and await input via `stdin`.
For convenience, you can run any of Toad's commands on startup and Toad will exit immediately after that command's execution.
Run the engine and execute the `help` command to see a list of available commands.

### UCI

Toad abides (mostly) by the [Universal Chess Interface](https://backscattering.de/chess/uci/) protocol, and communicates through `stdin` and `stdout`.
Code for parsing UCI commands and responses is handled by a crate of mine, [`uci-parser`](https://crates.io/crates/uci-parser).

The following UCI commands (and arguments) are supported:

-   `uci`
-   `isready`
-   `setoption name <x> [value <y>]`
-   `ucinewgame`
-   `position [fen <fenstring> | startpos] [moves <move_1> ... <move_i>]`
    -   Extended to include [`position kiwipete`](https://www.chessprogramming.org/Perft_Results#Position_2).
-   `go wtime <x> btime <x> winc <x> binc <x> depth <x> nodes <x> movetime <x> infinite perft <x>`
-   `stop`
-   `quit`

### Custom Commands

In addition to the above UCI commands, Toad also supports the following custom commands:

-   `bench`: Execute a benchmark, running a fixed-depth search (`default=5`) on the positions specified by the `file` argument (`default=benches/standard.epd`).
-   `display` (alias `d`): Display an ASCII representation of the current board state, including FEN string, Zobrist key, pinned pieces and checkers.
-   `eval`: Print an evaluation of the current board state. Additional flag(s) available to display more information about how the evaluation was computed.
-   `exit`: Quits the program as quickly as possible. If the `cleanup` flag is supplied, it will await the completion of any active search threads before exiting.
-   `fen`: Generate a FEN string of the current board state.
-   `flip`: Toggles the side-to-move. Equivalent to playing a null move.
-   `moves`: Show all legal moves on the current position, or for a given square.
-   `option`: Display the current value of the specified option.
-   `perft`: Execute a perft on the current position at a supplied depth, printing the total nodes.
-   `splitperft`: Execute a splitperft on the current position at a supplied depth, printing the total nodes.

### UCI Options

None, yet!

## Running

To run Toad on your own platform, head over to the [releases](https://github.com/dannyhammer/toad/releases) page to grab the latest release.
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

Toad uses several state-of-the-art techniques in [chess programming](https://www.chessprogramming.org/Main_Page) to achieve its strength:

-   [Bitboard representation](https://www.chessprogramming.org/Bitboards).
-   [Magic Bitboards](https://www.chessprogramming.org/Magic_Bitboards) for sliding piece attacks.
-   Search:
    -   Based on the [Negamax](https://www.chessprogramming.org/Negamax) algorithm.
    -   [Alpha-Beta Pruning](https://www.chessprogramming.org/Alpha-Beta#Negamax_Framework) in a fail soft framework.
-   Evaluation:
    -   [Material difference](https://www.chessprogramming.org/Material)

More features will be added as development continues!

## Acknowledgements

More people have helped me on this journey than I can track, but I'll name a few notable resources/people here:

-   [Sebastian Lague](https://www.youtube.com/@SebastianLague), for his [chess programming series](https://www.youtube.com/watch?v=_vqlIPDR2TU&list=PLFt_AvWsXl0cvHyu32ajwh2qU1i6hl77c) on YouTube that ultimate inspired me to do this project.
-   The [Chess Programming Wiki](https://www.chessprogramming.org/), and all those who contribute to free, open-source knowledge.
-   The folks over at the [Engine Programming Discord](https://discord.com/invite/F6W6mMsTGN), for their patience with my silly questions and invaluable help overall.
-   [Analog-Hors](https://github.com/analog-hors), for an excellent [article on magic bitboards](https://analog-hors.github.io/site/magic-bitboards/)
-   The authors of [viridithas](https://github.com/cosmobobak/viridithas/) and [Stormphrax](https://github.com/Ciekce/Stormphrax), for allowing their engines to be open source and for answering all my silly questions.
