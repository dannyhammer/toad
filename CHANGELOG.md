# Changelog

## [0.3.0](https://github.com/dannyhammer/toad/compare/v0.2.0...v0.3.0) (2024-10-08)


### Features

* remodelled command structure to be compatible with OpenBench ([7cf15a6](https://github.com/dannyhammer/toad/commit/7cf15a6ccbd26cd8ab800f94ff33d4249e98caaa))

## [0.2.0](https://github.com/dannyhammer/toad/compare/v0.1.0...v0.2.0) (2024-10-03)


### Features

* added more utilities to the Score type ([1484b8f](https://github.com/dannyhammer/toad/commit/1484b8fd8df2c6e3d9fa32a152a6ce15c14ec7eb))


### Bug Fixes

* fixed bug causing quit command to not parse properly ([1484b8f](https://github.com/dannyhammer/toad/commit/1484b8fd8df2c6e3d9fa32a152a6ce15c14ec7eb))

## 0.1.0 (2024-10-03)


### Features

* added basic iterative deepening for time management ([a82e06d](https://github.com/dannyhammer/toad/commit/a82e06d57d62d688cf27f3ea4b2026a618e4de41))
* added basic negamax search routine ([a82e06d](https://github.com/dannyhammer/toad/commit/a82e06d57d62d688cf27f3ea4b2026a618e4de41))
* added command-line flags for passing in engine commands on startup and exiting when done ([763766c](https://github.com/dannyhammer/toad/commit/763766cac60283b5416b6bd8d611b693cb43a19e))
* added more utilities to the Score type ([f97c7ef](https://github.com/dannyhammer/toad/commit/f97c7efc2a47992e4c4c6f55e143b793e32e6950))
* added support for benchmarking ([a82e06d](https://github.com/dannyhammer/toad/commit/a82e06d57d62d688cf27f3ea4b2026a618e4de41))
* added well-typed Uci responses ([763766c](https://github.com/dannyhammer/toad/commit/763766cac60283b5416b6bd8d611b693cb43a19e))
* engine now runs with basic functionality ([a82e06d](https://github.com/dannyhammer/toad/commit/a82e06d57d62d688cf27f3ea4b2026a618e4de41))
* finalized search skeleton ([763766c](https://github.com/dannyhammer/toad/commit/763766cac60283b5416b6bd8d611b693cb43a19e))


### Bug Fixes

* fixed bug causing mate distance to be calculated incorrecltly for Black ([763766c](https://github.com/dannyhammer/toad/commit/763766cac60283b5416b6bd8d611b693cb43a19e))
* fixed bug causing quit command to not parse properly ([f97c7ef](https://github.com/dannyhammer/toad/commit/f97c7efc2a47992e4c4c6f55e143b793e32e6950))