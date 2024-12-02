use std::{
    fs::{self, File},
    io::{BufRead, BufReader},
    path::Path,
    str::FromStr,
};

use anyhow::{bail, Result};
use chrono::Utc;
use toad::*;

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

fn main() {
    let mut tuner: Tuner<Standard> =
        Tuner::new("./croak/datasets/sample.txt", vec![1, 2, 3]).unwrap();
    for (game, res) in tuner.inputs.iter() {
        println!(
            "{:<width$} {res:?}",
            game.to_fen(),
            width = 85 /* max FEN length */
        );
    }

    tuner.tune(K).unwrap();
}

struct Tuner<V: Variant> {
    inputs: Vec<(Game<V>, GameResult)>,
    weights: Vec<i32>,
}

impl<V: Variant> Tuner<V> {
    /// Parses the file at `path` into a list of positions and game results.
    fn new(path: impl AsRef<Path>, weights: Vec<i32>) -> Result<Self> {
        let mut inputs = vec![];
        let reader = BufReader::new(File::open(path)?);

        for line in reader.lines() {
            let line = line?;

            let (fen, result) = line
                .split_once('[')
                .expect("Did not find '[' while parsing position");

            inputs.push((fen.parse()?, result.parse()?));
        }

        Ok(Self { inputs, weights })
    }

    fn weights(&self) -> &[i32] {
        &self.weights
    }
    fn weights_mut(&mut self) -> &mut [i32] {
        &mut self.weights
    }

    fn update_weights(&mut self, weights: &[i32]) {
        self.weights_mut()[..weights.len()].copy_from_slice(&weights[..]);
    }
    fn store_weights(&self, path: impl AsRef<Path>) -> Result<()> {
        println!("Writing weights to {:?}", path.as_ref());
        let path = Path::new("tune-outputs").join(path);
        fs::write(path, format!("{:?}", self.weights()))?;
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
                        println!("Found better params({sign})");
                        self.store_weights("tuning_weights.txt")?;
                        break 'improving;
                    }
                }
            }
            let now = Utc::now().format("%Y-%m-%d_%H-%M-%S");
            self.store_weights(format!("session_weights_{now}"))?;
            break;
        }

        self.store_weights("final_weights.txt")
    }

    fn mean_squared_error(&self, k: Float) -> Float {
        let n = self.inputs.len() as Float;

        let sum: Float = self
            .inputs
            .iter()
            .map(|(game, result)| {
                let r_i = (game.eval().inner() as f64) / 100.0;
                let q_i = result.value();
                (r_i - sigmoid(q_i, k)).powi(2)
            })
            .sum();

        sum / n
    }
}
