# Toad - A UCI chess engine, written in Rust.

Toad is a work-in-progress [chess engine](https://en.wikipedia.org/wiki/Chess_engine), and serves as my personal excuse to write fun code in Rust.
It is built upon [`chessie`](https://crates.io/crates/chessie), my chess crate which handles move generation and all other rules of chess.

## Overview

By default, Toad will print its version and authors and await input via `stdin`.
For convenience, there are a few command-line flags you can specify to alter Toad's behavior when running.
To see them all, run Toad with `--help`.
Here are some useful ones:

-   `-c <command>`: Specify a command to be executed upon startup. Multi-word commands must be quoted, and multiple commands can be specified with multiple `-c` flags.
-   `-e`: Exit the program as soon as all commands have finished execution (this includes searches).

### UCI

Toad abides (mostly) by the [Universal Chess Interface](https://backscattering.de/chess/uci/) protocol, and communicates through `stdin` and `stdout`.
Code for parsing UCI commands and responses is handled by a crate of mine, [`uci-parser`](https://crates.io/crates/uci-parser).

The following UCI commands (and arguments) are supported:

-   `uci`
-   `isready`
-   `ucinewgame`
-   `position [fen <fenstring> | startpos] [moves <move_1> ... <move_i>]`
-   `go wtime <x> btime <x> winc <x> binc <x> depth <x> nodes <x> movetime <x> infinite perft <x>`
-   `stop`
-   `quit`

### Custom Commands

In addition to the above UCI commands, Toad also supports the following custom commands:

-   `bench [depth <n>] [file <path>]`: Execute a benchmark, running a fixed-depth search (`default=5`) on the positions specified by `file` (`default=benches/standard.epd`).
-   `display` (alias `d`): Display an ASCII representation of the current board state, including FEN string, Zobrist key, pinned pieces and checkers.
-   `eval [pretty]`: Print an evaluation of the current board state. If `pretty` is supplied, it will display more information about how the evaluation was computed.
-   `fen`: Generate a FEN string of the current board state.
-   `flip`: Toggles the side-to-move. Equivalent to playing a null move.
-   `perft <depth>`: Execute a perft on the current position at the supplied depth, printing the total nodes.
    -   `go perft <depth>` will do the same, but will print a split perft instead.
-   `quit [true | false]`(alias `exit`): Quits the program as quickly as possible. If `true` is supplied, it will await the completion of any active search threads before exiting.
-   `position kiwipete`: Extension on the UCI `position` command that sets the current position to the [Kiwipete FEN](https://www.chessprogramming.org/Perft_Results#Position_2).

### UCI Options

None, yet!

## Running

To run Toad on your own platform, head over to the [releases](https://github.com/dannyhammer/toad/releases) page.
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

## Features

Toad uses several state-of-the-art techniques in [chess programming](https://www.chessprogramming.org/Main_Page) to achieve its strength:

-   [Bitboard representation](https://www.chessprogramming.org/Bitboards)
-   [Magic Bitboards](https://www.chessprogramming.org/Magic_Bitboards) for sliding piece attacks
-   Search:
    -   Based on the [Negamax](https://www.chessprogramming.org/Negamax) algorithm
-   Evaluation:
    -   [Material difference](https://www.chessprogramming.org/Material)

More features will be added as development continues!

## Acknowledgements

More people have helped me on this journey than I can track, but I'll name a few notable resources/people here:

-   [Sebastian Lague](https://www.youtube.com/@SebastianLague), for his [chess programming series](https://www.youtube.com/watch?v=_vqlIPDR2TU&list=PLFt_AvWsXl0cvHyu32ajwh2qU1i6hl77c) on YouTube that ultimate inspired me to do this project.
-   The [Chess Programming Wiki](https://www.chessprogramming.org/), and all those who contribute to free, open-source knowledge.
-   The folks over at the [Engine Programming Discord](https://discord.com/invite/F6W6mMsTGN), for their patience with my silly questions and invaluable help overall.
-   [Analog-Hors](https://github.com/analog-hors), for an excellent [article on magic bitboards](https://analog-hors.github.io/site/magic-bitboards/)
