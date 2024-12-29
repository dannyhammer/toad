use std::{
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
    str::FromStr,
};

use anyhow::{bail, Result};
use chrono::Utc;
use toad::*;

use croak::*;

type Float = f64;

const K: Float = 1.0;

#[derive(Debug)]
enum GameResult {
    BlackWin,
    Draw,
    WhiteWin,
}

impl GameResult {
    fn value(&self) -> Float {
        match self {
            Self::BlackWin => 0.0,
            Self::Draw => 0.5,
            Self::WhiteWin => 1.0,
        }
    }
}

impl FromStr for GameResult {
    type Err = anyhow::Error;
    fn from_str(mut s: &str) -> std::result::Result<Self, Self::Err> {
        // Remove leading/trailing brackets, if present
        s = s.trim_start_matches('[');
        s = s.trim_end_matches(']');

        match s {
            "0.0" | "0-0" => Ok(Self::BlackWin),
            "0.5" | "1/2-1/2" => Ok(Self::Draw),
            "1.0" | "1-0" => Ok(Self::WhiteWin),
            _ => bail!("Could not parse {s:?} into GameResult"),
        }
    }
}

/*
// A TTuple is used for all coefficients that are non-zero or specific to the Safety or
// Complexity evaluations. An array of TTuples acts as a Skip-Vector or Skip-Matrix. In
// practice, smaller data types are used than integers to save Memory
#[derive(Debug)]
struct TTuple {
    index: usize,
    coefficients: [Float; Color::COUNT],
}

#[derive(Debug)]
struct TEntry {
    // A Static Evaluation, used to compute the optimal K value
    static_eval: i32,
    // The phase coefficient rho that is defined in (1.3)
    phase: i32,
    // Side to move of the position. Not used in this paper.
    side_to_move: Color,

    // A evaluation of the position before applying complexity written as (mg, eg)
    eval: (i32, i32),
    // The result of the game this position came from. [0.0, 0.5, 1.0]
    result: GameResult,
    // The scale factor xi used to adjust the Endgame evaluation
    scale_factor: Float,
    // The rho_mg and rho_eg that are defined in (3.1)
    p_factors: (Float, Float),

    // Array of TTuples for all needed coefficients
    tuples: Vec<TTuple>,
}
 */

/*
fn init_tuner_entries(tuning_file: &str) -> Result<Vec<TEntry>> {
    let entries = Vec::default();

    Ok(entries)
}

fn loss_function(list: impl ExactSizeIterator<Item = (Float, Float)>) -> Float {
    let n = list.len() as Float;

    let sum: Float = list.map(|(r_i, q_i)| (r_i - sigmoid(q_i, K)).powi(2)).sum();

    sum / n
}
 */

// fn sigmoid(s: Float, k: Float) -> Float {
//     1.0 / (1.0 + Float::powf(10.0, (-k * s) / 400.0))
// }

fn sigmoid(e: Float, k: Float) -> Float {
    1.0 / (1.0 + (-k * e).exp())
}

/*
fn mean_squared_error(weights: &[i32], k: Float) -> Float {
    let n = weights.len() as Float;

    let sum: Float = weights
        .iter()
        .map(|(game, result)| {
            let r_i = (game.eval().inner() as Float) / 100.0;
            let q_i = result.value();
            (r_i - sigmoid(q_i, k)).powi(2)
        })
        .sum();

    sum / n
}
 */

fn dot(a: &[i32], b: &[i32]) -> i32 {
    a.iter().zip(b.iter()).map(|(x, y)| x * y).sum()
}

fn extract_features<V: Variant>(game: &Game<V>) -> Vec<i32> {
    let mut features = vec![0; 12];

    // Counting material values
    let mut i = 0;
    for kind in PieceKind::all() {
        let val = game.piece_parts(Color::White, kind).population() as i32
            - game.piece_parts(Color::Black, kind).population() as i32;

        // Coefficients are equal for midgame and endgame
        features[i] += val;
        features[i + 1] += val;
        i += 2;
    }

    features
}

fn foo<V: Variant>(fen: &str, _result: GameResult, weights: &[i32]) -> i32 {
    let game: Game<V> = Game::from_fen(fen).unwrap();
    // let weight = game.evaluator().endgame_weight();
    let features = extract_features(&game);
    let mg_dot = dot(weights, &features);
    let eg_dot = dot(weights, &features);
    eprintln!("Features in {fen}:\n{features:?}");
    eprintln!("Dot product: {mg_dot} (mg) {eg_dot} (eg)");
    mg_dot
}

fn main() {
    println!("Starting tuner");
    let init_params = Parameters::default();
    let weights = init_params.into_iter().collect::<Vec<_>>();
    println!("Tuning {} parameters:\n{weights:?}\n", weights.len());

    foo::<Standard>(FEN_STARTPOS, GameResult::Draw, &weights);
    println!();
    foo::<Standard>(
        "rnb1k1nr/pppp1ppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
        GameResult::WhiteWin,
        &weights,
    );
    println!();
    foo::<Standard>(
        "rnbqkbnr/pppppppp/8/8/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 1",
        GameResult::BlackWin,
        &weights,
    );

    let mut tuner = Tuner::<Standard>::new("./croak/datasets/sample.txt", weights).unwrap();
    // max FEN length
    let width = tuner
        .inputs
        .iter()
        .map(|v| v.0.to_fen().len())
        .max()
        .unwrap_or(85);
    for (game, _features, res) in tuner.inputs.iter() {
        println!("{:<width$} {res:?}", game.to_fen());
    }

    tuner.tune(K).unwrap();

    // let path = Path::new(".").join("tuned_eval_params.rs");
    // std::fs::write(&path, tuned.to_string()).unwrap();
}

struct Tuner<V: Variant> {
    inputs: Vec<(Game<V>, Vec<i32>, GameResult)>,
    weights: Vec<i32>,
}

impl<V: Variant> Tuner<V> {
    /// Parses the file at `path` into a list of positions, feature sets, and game results.
    fn new(path: impl AsRef<Path>, weights: Vec<i32>) -> Result<Self> {
        let mut inputs = vec![];
        let reader = BufReader::new(File::open(path)?);

        for line in reader.lines() {
            let line = line?;

            let (fen, result) = line
                .split_once('[')
                .expect("Did not find '[' while parsing position");

            let game: Game<V> = fen.parse()?;
            inputs.push((game, extract_features(&game), result.parse()?));
        }

        Ok(Self { inputs, weights })
    }

    /// Evaluates the provided feature set against the weights stored in this tuner.
    fn eval(&self, feature_set: &[i32]) -> i32 {
        dot(&self.weights, feature_set)
    }

    fn weights(&self) -> &[i32] {
        &self.weights
    }
    fn weights_mut(&mut self) -> &mut [i32] {
        &mut self.weights
    }

    fn update_weights(&mut self, weights: &[i32]) {
        self.weights_mut()[..weights.len()].copy_from_slice(weights);
    }
    fn store_weights(&self, path: impl AsRef<Path>) -> Result<()> {
        println!("Writing weights to {:?}", path.as_ref());
        // let path = Path::new("tune-outputs").join(path);
        // std::fs::write(path, format!("{:?}", self.weights()))?;
        Ok(())
    }

    fn tune(&mut self, k: Float) -> Result<()> {
        // Get the evaluation parameters
        let mut best_params = self.weights().to_vec();
        let n = best_params.len();

        // Initial mean squared error
        let mut best_mse = self.mean_squared_error(k);

        // Loop until MSE is minimized
        'improving: loop {
            for i in 0..n {
                // Copy params
                let mut new_params = best_params.to_vec();

                // First try incrementing, then try decrementing.
                for adjust in [1, -2] {
                    // Adjust current parameter
                    new_params[i] += adjust;

                    // Update evaluation parameters
                    self.update_weights(&new_params);

                    // Recalculate MSE with update params
                    let new_mse = self.mean_squared_error(k);
                    let sign = if adjust > 0 { '+' } else { '-' };
                    println!("Tuning params({sign}) {i}/{n} with MSE := {new_mse}");

                    // If param adjustment reduced the error, record that.
                    if new_mse < best_mse {
                        best_mse = new_mse;
                        best_params = new_params;
                        println!("Found better params({sign}): {best_mse}");
                        _ = best_params;
                        self.store_weights("tuning_weights.txt")?;
                        break 'improving;
                    }
                }
            }
            let now = Utc::now().format("%Y-%m-%d_%H-%M-%S");
            self.store_weights(format!("session_weights_{now}"))?;
            // break;
        }

        self.store_weights("final_weights.txt")
    }

    fn mean_squared_error(&self, k: Float) -> Float {
        let n = self.inputs.len() as Float;

        let sum: Float = self
            .inputs
            .iter()
            .map(|(_, feature_set, result)| {
                let r_i = (self.eval(feature_set) as Float) / 100.0;
                let q_i = result.value();
                (r_i - sigmoid(q_i, k)).powi(2)
            })
            .sum();

        sum / n
    }
}
