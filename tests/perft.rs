/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use toad::{perft_generic, Game, Standard};

fn test_perft_fen_nodes(depth: usize, fen: &str, expected: u64) {
    let position = Game::from_fen(fen).unwrap();
    let res = perft_generic::<false, false, Standard>(&position, depth);
    assert_eq!(res, expected, "PERFT(depth) failed on {fen}");
}

/*
fn test_epd(epd_file: &str, max_depth: usize) -> anyhow::Result<()> {
    let contents = std::fs::read_to_string(epd_file)?;

    for (i, entry) in contents.lines().enumerate() {
        let mut parts = entry.split(';');

        let fen = parts.next().unwrap().trim();

        for perft_data in parts {
            let depth = usize::from_str_radix(perft_data.get(1..2).unwrap().trim(), 10)?;
            if depth > max_depth {
                break;
            }
            let expected = u64::from_str_radix(perft_data.get(3..).unwrap().trim(), 10)?;

            let mut position = Game::from_fen(fen)?;

            let nodes = perft_generic::<false, false>(&mut position, depth);

            assert_eq!(
                nodes, expected,
                "\n{i} Perft({depth}, \"{fen}\") failed\nExpected: {expected}\nGot: {nodes}"
            );
        }
    }

    Ok(())
}

#[test]
fn test_standard_epd() {
    test_epd("tests/standard.epd", 6).unwrap();
}

#[test]
fn test_fischer_epd() {
    // Depth 6 takes a *very* long time
    test_epd("tests/fischer.epd", 5).unwrap();
}
 */

#[cfg(test)]
mod promotion_perft {
    use crate::test_perft_fen_nodes;

    #[test]
    fn test_promotion_perft_1() {
        test_perft_fen_nodes(1, "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ", 24);
    }
    #[test]
    fn test_promotion_perft_2() {
        test_perft_fen_nodes(2, "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ", 496);
    }
    #[test]
    fn test_promotion_perft_3() {
        test_perft_fen_nodes(3, "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ", 9483);
    }
    #[test]
    fn test_promotion_perft_4() {
        test_perft_fen_nodes(4, "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ", 182838);
    }
    #[test]
    fn test_promotion_perft_5() {
        test_perft_fen_nodes(5, "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ", 3605103);
    }

    #[test]
    fn test_promotion_perft_6() {
        test_perft_fen_nodes(6, "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1 ", 71179139);
    }
}

/// https://www.chessprogramming.net/perfect-perft/
#[cfg(test)]
mod simple_perfts {
    use super::*;

    #[test]
    fn test_simple_perft_1() {
        test_perft_fen_nodes(
            6,
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            119060324,
        );
    }

    #[test]
    fn test_simple_perft_2() {
        test_perft_fen_nodes(
            5,
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            193690690,
        );
    }

    #[test]
    fn test_simple_perft_3() {
        test_perft_fen_nodes(7, "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -", 178633661);
    }

    #[test]
    fn test_simple_perft_4() {
        test_perft_fen_nodes(
            6,
            "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1",
            706045033,
        );
    }

    #[test]
    fn test_simple_perft_5() {
        test_perft_fen_nodes(5, "1k6/1b6/8/8/7R/8/8/4K2R b K - 0 1", 1063513);
    }
}

/// https://www.chessprogramming.net/perfect-perft/
#[cfg(test)]
mod special_perfts {
    use super::*;

    #[test]
    fn test_special_perft_illegal_ep_move_1() {
        test_perft_fen_nodes(6, "3k4/3p4/8/K1P4r/8/8/8/8 b - - 0 1", 1134888);
    }

    #[test]
    fn test_special_perft_illegal_ep_move_2() {
        test_perft_fen_nodes(6, "8/8/4k3/8/2p5/8/B2P2K1/8 w - - 0 1", 1015133);
    }

    #[test]
    fn test_special_perft_ep_capture_checks_opponent() {
        test_perft_fen_nodes(6, "8/8/1k6/2b5/2pP4/8/5K2/8 b - d3 0 1", 1440467);
    }

    #[test]
    fn test_special_perft_short_castling_gives_check() {
        test_perft_fen_nodes(6, "5k2/8/8/8/8/8/8/4K2R w K - 0 1", 661072);
    }

    #[test]
    fn test_special_perft_long_castling_gives_check() {
        test_perft_fen_nodes(6, "3k4/8/8/8/8/8/8/R3K3 w Q - 0 1", 803711);
    }

    #[test]
    fn test_special_perft_castling_rights() {
        test_perft_fen_nodes(4, "r3k2r/1b4bq/8/8/8/8/7B/R3K2R w KQkq - 0 1", 1274206);
    }

    #[test]
    fn test_special_perft_castling_prevented() {
        test_perft_fen_nodes(4, "r3k2r/8/3Q4/8/8/5q2/8/R3K2R b KQkq - 0 1", 1720476);
    }

    #[test]
    fn test_special_perft_promote_out_of_check() {
        test_perft_fen_nodes(6, "2K2r2/4P3/8/8/8/8/8/3k4 w - - 0 1", 3821001);
    }

    #[test]
    fn test_special_perft_discovered_check() {
        test_perft_fen_nodes(5, "8/8/1P2K3/8/2n5/1q6/8/5k2 b - - 0 1", 1004658);
    }

    #[test]
    fn test_special_perft_promote_to_give_check() {
        test_perft_fen_nodes(6, "4k3/1P6/8/8/8/8/K7/8 w - - 0 1", 217342);
    }

    #[test]
    fn test_special_perft_under_promote_to_give_check() {
        test_perft_fen_nodes(6, "8/P1k5/K7/8/8/8/8/8 w - - 0 1", 92683);
    }

    #[test]
    fn test_special_perft_self_stalemate() {
        test_perft_fen_nodes(6, "K1k5/8/P7/8/8/8/8/8 w - - 0 1", 2217);
    }

    #[test]
    fn test_special_perft_stalemate_and_checkmate_1() {
        test_perft_fen_nodes(7, "8/k1P5/8/1K6/8/8/8/8 w - - 0 1", 567584);
    }

    #[test]
    fn test_special_perft_stalemate_and_checkmate_2() {
        test_perft_fen_nodes(4, "8/8/2k5/5q2/5n2/8/5K2/8 b - - 0 1", 23527);
    }

    #[test]
    fn test_cannot_castle_when_rook_pinned() {
        test_perft_fen_nodes(
            1,
            "Qr2kqbr/2bpp1pp/pn3p2/2p5/6P1/P1PP4/1P2PP1P/NRNBK1BR b HBhb - 0 9",
            34,
        );
    }
}

// The following test cases are from: https://github.com/kz04px/rawr/blob/master/tests/perft_extra.rs

// Helper function for the test cases below
fn do_perft(fen: &str, results: &[u64]) {
    println!("{fen}");
    let pos = Game::from_fen(fen).unwrap();
    for (depth, result) in results.iter().enumerate() {
        // let nodes = pos.perft_generic::<false, false>(idx as u8);
        let nodes = perft_generic::<false, false, Standard>(&pos, depth);
        assert_eq!(nodes, *result, "PERFT({depth}) failed on {fen}");
    }
}

#[test]
fn perft_enpassant() {
    let tests = [
        // EP
        ("8/8/8/8/1k1PpN1R/8/8/4K3 b - d3 0 1", vec![1, 9, 193]),
        ("8/8/8/8/1k1Ppn1R/8/8/4K3 b - d3 0 1", vec![1, 17, 220]),
        ("4k3/8/8/2PpP3/8/8/8/4K3 w - d6 0 1", vec![1, 9, 47, 376]),
        ("4k3/8/8/8/2pPp3/8/8/4K3 b - d3 0 1", vec![1, 9, 47, 376]),
        // EP - pinned diagonal
        ("4k3/b7/8/2Pp4/8/8/8/6K1 w - d6 0 1", vec![1, 5, 45]),
        ("4k3/7b/8/4pP2/8/8/8/1K6 w - e6 0 1", vec![1, 5, 45]),
        ("6k1/8/8/8/2pP4/8/B7/3K4 b - d3 0 1", vec![1, 5, 45]),
        ("1k6/8/8/8/4Pp2/8/7B/4K3 b - e3 0 1", vec![1, 5, 45]),
        ("4k3/b7/8/1pP5/8/8/8/6K1 w - b6 0 1", vec![1, 6, 52]),
        ("4k3/7b/8/5Pp1/8/8/8/1K6 w - g6 0 1", vec![1, 6, 51]),
        ("6k1/8/8/8/1Pp5/8/B7/4K3 b - b3 0 1", vec![1, 6, 52]),
        ("1k6/8/8/8/5pP1/8/7B/4K3 b - g3 0 1", vec![1, 6, 51]),
        ("4k3/K7/8/1pP5/8/8/8/6b1 w - b6 0 1", vec![1, 6, 66]),
        ("4k3/7K/8/5Pp1/8/8/8/1b6 w - g6 0 1", vec![1, 6, 60]),
        ("6B1/8/8/8/1Pp5/8/k7/4K3 b - b3 0 1", vec![1, 6, 66]),
        ("1B6/8/8/8/5pP1/8/7k/4K3 b - g3 0 1", vec![1, 6, 60]),
        ("4k3/b7/8/2Pp4/3K4/8/8/8 w - d6 0 1", vec![1, 5, 44]),
        ("4k3/8/1b6/2Pp4/3K4/8/8/8 w - d6 0 1", vec![1, 6, 59]),
        ("4k3/8/b7/1Pp5/2K5/8/8/8 w - c6 0 1", vec![1, 6, 49]),
        ("4k3/8/7b/5pP1/5K2/8/8/8 w - f6 0 1", vec![1, 6, 49]),
        ("4k3/7b/8/4pP2/4K3/8/8/8 w - e6 0 1", vec![1, 5, 44]),
        ("4k3/8/6b1/4pP2/4K3/8/8/8 w - e6 0 1", vec![1, 6, 53]),
        ("4k3/8/3K4/1pP5/8/q7/8/8 w - b6 0 1", vec![1, 5, 114]),
        ("7k/4K3/8/1pP5/8/q7/8/8 w - b6 0 1", vec![1, 8, 171]),
        // EP - double check
        ("4k3/2rn4/8/2K1pP2/8/8/8/8 w - e6 0 1", vec![1, 4, 75]),
        // EP - pinned horizontal
        ("4k3/8/8/K2pP2r/8/8/8/8 w - d6 0 1", vec![1, 6, 94]),
        ("4k3/8/8/K2pP2q/8/8/8/8 w - d6 0 1", vec![1, 6, 130]),
        ("4k3/8/8/r2pP2K/8/8/8/8 w - d6 0 1", vec![1, 6, 87]),
        ("4k3/8/8/q2pP2K/8/8/8/8 w - d6 0 1", vec![1, 6, 129]),
        ("8/8/8/8/1k1Pp2R/8/8/4K3 b - d3 0 1", vec![1, 8, 125]),
        ("8/8/8/8/1R1Pp2k/8/8/4K3 b - d3 0 1", vec![1, 6, 87]),
        // EP - pinned vertical
        ("k7/8/4r3/3pP3/8/8/8/4K3 w - d6 0 1", vec![1, 5, 70]),
        ("k3K3/8/8/3pP3/8/8/8/4r3 w - d6 0 1", vec![1, 6, 91]),
        // EP - in check
        ("4k3/8/8/4pP2/3K4/8/8/8 w - e6 0 1", vec![1, 9, 49]),
        ("8/8/8/4k3/5Pp1/8/8/3K4 b - f3 0 1", vec![1, 9, 50]),
        // EP - block check
        ("4k3/8/K6r/3pP3/8/8/8/8 w - d6 0 1", vec![1, 6, 109]),
        ("4k3/8/K6q/3pP3/8/8/8/8 w - d6 0 1", vec![1, 6, 151]),
    ];

    for (fen, results) in tests {
        do_perft(fen, &results);
    }
}

#[test]
fn perft_double_checked() {
    let tests = [
        ("4k3/8/4r3/8/8/8/3p4/4K3 w - - 0 1", [1, 4, 80, 320]),
        ("4k3/8/4q3/8/8/8/3b4/4K3 w - - 0 1", [1, 4, 143, 496]),
    ];

    for (fen, results) in tests {
        do_perft(fen, &results);
    }
}

#[test]
fn perft_pins() {
    let tests = [
        ("4k3/8/8/8/1b5b/8/3Q4/4K3 w - - 0 1", [1, 3, 54, 1256]),
        ("4k3/8/8/8/1b5b/8/3R4/4K3 w - - 0 1", [1, 3, 54, 836]),
        ("4k3/8/8/8/1b5b/2Q5/5P2/4K3 w - - 0 1", [1, 6, 98, 2274]),
        ("4k3/8/8/8/1b5b/2R5/5P2/4K3 w - - 0 1", [1, 4, 72, 1300]),
        ("4k3/8/8/8/1b2r3/8/3Q4/4K3 w - - 0 1", [1, 3, 66, 1390]),
        ("4k3/8/8/8/1b2r3/8/3QP3/4K3 w - - 0 1", [1, 6, 119, 2074]),
    ];

    for (fen, results) in tests {
        do_perft(fen, &results);
    }
}

#[test]
fn perft_dfrc() {
    let tests = [
        (
            "2r1kr2/8/8/8/8/8/8/1R2K1R1 w GBfc - 0 1",
            [1, 22, 501, 11459],
        ),
        ("rkr5/8/8/8/8/8/8/5RKR w HFca - 0 1", [1, 22, 442, 10217]),
        ("5rkr/8/8/8/8/8/8/RKR5 w CAhf - 0 1", [1, 22, 442, 10206]),
        ("3rkr2/8/8/8/8/8/8/R3K2R w HAfd - 0 1", [1, 20, 452, 9873]),
        ("4k3/8/8/8/8/8/8/4KR2 w F - 0 1", [1, 14, 47, 781]),
        ("4kr2/8/8/8/8/8/8/4K3 w f - 0 1", [1, 3, 42, 246]),
        ("4k3/8/8/8/8/8/8/2R1K3 w C - 0 1", [1, 16, 71, 1277]),
        ("2r1k3/8/8/8/8/8/8/4K3 w c - 0 1", [1, 5, 80, 448]),
    ];

    for (fen, results) in tests {
        do_perft(fen, &results);
    }
}
