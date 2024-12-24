/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 */

use crate::{Move, Ply, Score, SearchBounds, ZobristKey};

/// Number of bytes in a megabyte
const BYTES_IN_MB: usize = 1024 * 1024;

/// Result of probing the [`TTable`].
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ProbeResult<'a> {
    /// An entry was found and can be used to perform a cutoff.
    Cutoff(Score),

    /// An entry was found, but it could not be used to perform a cutoff.
    Hit(&'a TTableEntry),

    /// No entry was found for the provided key.
    Miss,
}

/// Type of node encountered during search.
///
/// See [CPW](https://www.chessprogramming.org/Node_Types) for more.
#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub enum NodeType {
    /// The score is exact.
    Pv,

    /// The score is less than alpha (upper bound).
    All,

    /// The score is greater than or equal to beta (lower bound).
    Cut,
}

impl NodeType {
    /// Creates a new [`NodeType`] based on the parameters as follows:
    ///
    /// ```text
    /// if score <= alpha:
    ///     UPPERBOUND
    /// else if score >= beta:
    ///     LOWERBOUND
    /// else:
    ///     EXACT
    /// ```
    #[inline(always)]
    pub fn new(score: Score, bounds: SearchBounds) -> Self {
        if score <= bounds.alpha {
            Self::All
        } else if score >= bounds.beta {
            Self::Cut
        } else {
            Self::Pv
        }
    }
}

/// An entry into a hash table
#[derive(PartialEq, Eq, Clone, Debug)]
pub struct TTableEntry {
    /// Key of the node this entry represents.
    pub key: ZobristKey,

    /// Depth at which the data for this entry was found.
    pub depth: u8,

    /// Best move found for this position.
    pub bestmove: Option<Move>,

    /// Best score found for this position.
    pub score: Score,

    /// Node type of this entry.
    pub node_type: NodeType,
}

impl TTableEntry {
    /// Creates a new [`TTableEntry`] from the provided parameters.
    ///
    /// This will generate a node type through [`NodeType::new`] and
    /// will adjust `score` by `ply` if it was a mate score.
    #[inline(always)]
    pub fn new(
        key: ZobristKey,
        bestmove: Option<Move>,
        score: Score,
        bounds: SearchBounds,
        depth: Ply,
        ply: Ply,
    ) -> Self {
        // Determine what kind of node this is first, *before* score adjustment
        let node_type = NodeType::new(score, bounds);

        Self {
            key,
            bestmove,
            // Adjust the score (if it was mate) to the ply at which we found it
            score: score.absolute(ply),
            depth: depth.plies() as u8,
            node_type,
        }
    }

    /// Fetch the depth associated with this entry.
    #[inline(always)]
    pub fn depth(&self) -> Ply {
        Ply::new(self.depth as i32)
    }
}

/// Transposition Table.
///
/// Used during a search to keep track of previous search results on positions,
/// avoiding unnecessary re-computations.
#[derive(Debug)]
pub struct TTable {
    /// Internal cache of the TTable.
    cache: Vec<Option<TTableEntry>>,

    /// Number of collisions that have occurred since last clearing.
    pub(crate) collisions: usize,

    /// Number of reads that have occurred since last clearing.
    pub(crate) reads: usize,

    /// Number of writes that have occurred since last clearing.
    pub(crate) writes: usize,

    /// Number of hits that have occurred since last clearing.
    pub(crate) hits: usize,
}

impl TTable {
    /// Default size of the Transposition Table, in megabytes.
    pub const DEFAULT_SIZE: usize = 16;

    /// Minimum size of the Transposition Table, in megabytes.
    pub const MIN_SIZE: usize = 1;

    /// Maximum size of the Transposition Table, in megabytes.
    pub const MAX_SIZE: usize = 1_024;

    /// Create a new [`TTable`] that is `size` megabytes.
    ///
    /// Its size will be `size_of::<TTableEntry>() * capacity`
    #[inline(always)]
    pub fn new(size: usize) -> Self {
        Self::from_capacity((size * BYTES_IN_MB) / size_of::<TTableEntry>())
    }

    /// Create a new [`TTable`] that can hold `capacity` entries.
    #[inline(always)]
    pub fn from_capacity(capacity: usize) -> Self {
        Self {
            cache: vec![None; capacity],
            collisions: 0,
            reads: 0,
            writes: 0,
            hits: 0,
        }
    }

    /// Clears the entries of this [`TTable`].
    ///
    /// Also resets all collected stats.
    #[inline(always)]
    pub fn clear(&mut self) {
        self.cache.iter_mut().for_each(|entry| *entry = None);
        self.collisions = 0;
        self.reads = 0;
        self.writes = 0;
        self.hits = 0;
    }

    /// Returns the number of entries that can fit within this [`TTable`]
    #[inline(always)]
    pub fn capacity(&self) -> usize {
        self.cache.len()
    }

    /// Returns the size of this [`TTable`], in megabytes.
    #[inline(always)]
    pub fn size(&self) -> usize {
        self.cache.len() * size_of::<TTableEntry>() / BYTES_IN_MB
    }

    /// Returns the number of `Some` entries in this [`TTable`].
    #[inline(always)]
    pub fn num_entries(&self) -> usize {
        self.cache.iter().filter(|entry| entry.is_some()).count()
    }

    /// Map `key` to an index into this [`TTable`].
    #[inline(always)]
    pub fn index(&self, key: &ZobristKey) -> usize {
        // key.inner() as usize % self.capacity()
        // Not sure if this is guaranteed to work, actually
        key.inner() as usize & (self.capacity() - 1)
    }

    /// Get the entry if and only if it matches the provided key
    #[inline(always)]
    pub fn get(&self, key: &ZobristKey) -> Option<&TTableEntry> {
        self.entry(key).filter(|e| &e.key == key)
    }

    /*
    /// Mutably get the entry if and only if it matches the provided key
    #[inline(always)]
    pub fn get_mut(&mut self, key: &ZobristKey) -> Option<&mut TTableEntry> {
        self.entry_mut(key).filter(|e| &e.key == key)
    }
     */

    /// Get the entry, without regards for whether it matches the provided key
    #[inline(always)]
    fn entry(&self, key: &ZobristKey) -> Option<&TTableEntry> {
        // We can safely index as we've initialized this ttable to be non-empty
        self.cache[self.index(key)].as_ref()
    }

    /*
    /// Mutably get the entry, without regards for whether it matches the provided key
    #[inline(always)]
    fn entry_mut(&mut self, key: &ZobristKey) -> Option<&mut TTableEntry> {
        let index = self.index(key);
        self.cache[index].as_mut()
    }
     */

    /// Store `entry` in the table at `entry.key`, overriding and returning whatever was there.
    #[inline(always)]
    pub fn store(&mut self, entry: TTableEntry) -> Option<TTableEntry> {
        let index = self.index(&entry.key);
        self.cache[index].replace(entry)
    }

    /// Probes the [`TTable`] for an entry at the provided `key`, returning that entry's score, if appropriate.
    ///
    /// If an entry is found from a greater depth than `depth`, its score is returned if and only if:
    ///     1. The entry is exact.
    ///     2. The entry is an upper bound and its score is `<= alpha`.
    ///     3. The entry is a lower bound and its score is `>= beta`.
    ///
    /// See [`TTableEntry::try_score`] for more.
    #[inline(always)]
    pub fn probe(
        &self,
        key: ZobristKey,
        depth: Ply,
        ply: Ply,
        bounds: SearchBounds,
    ) -> ProbeResult {
        // if-let chains are set to be stabilized in Rust 2024 (1.85.0): https://rust-lang.github.io/rfcs/2497-if-let-chains.html
        if let Some(entry) = self.get(&key) {
            // Can only cut off if the existing entry came from a greater depth.
            if entry.depth() >= depth {
                // Adjust mate scores to be relative to current ply
                let score = entry.score.relative(ply);

                // If we can cutoff, do so
                if entry.node_type == NodeType::Pv
                    || ((entry.node_type == NodeType::All && score <= bounds.alpha)
                        || (entry.node_type == NodeType::Cut && score >= bounds.beta))
                {
                    return ProbeResult::Cutoff(score);
                }
            }

            // No cutoff was possible, but there was still an entry found.
            return ProbeResult::Hit(entry);
        }

        ProbeResult::Miss
    }
}

impl Default for TTable {
    #[inline(always)]
    fn default() -> Self {
        Self::new(Self::DEFAULT_SIZE)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::*;

    #[test]
    fn test_ttable() {
        // Create two positions whose Zobrist keys are equal mod 2
        let pos1 = Game::<Standard>::default();
        let mut pos2 = Game::<Standard>::from_fen(FEN_KIWIPETE).unwrap();

        // Ensure that the two positions have Zobrist keys that are both odd/even
        while pos1.key().inner() % 2 != pos2.key().inner() % 2 {
            let mv = pos2.get_legal_moves()[0];
            pos2 = pos2.with_move_made(mv);
        }

        // Create Zobrist keys for both positions
        let key1 = pos1.key();
        let key2 = pos2.key();

        // Create entries for both positions
        let entry1 = TTableEntry {
            key: key1,
            bestmove: None,
            score: Score::DRAW,
            depth: 0,
            node_type: NodeType::Pv,
        };

        let entry2 = TTableEntry {
            key: key2,
            bestmove: None,
            score: Score::MATE,
            depth: 0,
            node_type: NodeType::Pv,
        };

        // Create a TTable that can hold two elements.
        // This is important as both elements will need to map to the same index
        let mut tt = TTable::from_capacity(2);
        assert_eq!(
            tt.num_entries(),
            0,
            "TTable should initialize to being empty"
        );

        tt.store(entry1.clone());
        assert_eq!(
            tt.num_entries(),
            1,
            "After storing one entry, TTable should only have 1 entry"
        );
        assert_eq!(
            tt.entry(&key1),
            Some(&entry1),
            "Getting an entry by key returns the appropriate entry"
        );

        tt.store(entry2.clone());
        assert_eq!(tt.num_entries(), 1, "After storing another entry that overwrites a previous one, TTable should only have 1 entry");

        assert_ne!(
            tt.entry(&key1),
            Some(&entry1),
            "Cannot get an entry that has been overridden"
        );

        assert!(
            tt.get(&key1).is_none(),
            "Cannot get an entry that has been overridden"
        );

        assert_eq!(
            tt.get(&key2),
            Some(&entry2),
            "Getting an entry by key returns the appropriate entry"
        );
    }
}
