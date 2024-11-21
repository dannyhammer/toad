# Changelog

## [1.4.0](https://github.com/dannyhammer/toad/compare/v1.3.0...v1.4.0) (2024-11-21)


### Features

* lmr, check extensions, tt cutoffs ([#85](https://github.com/dannyhammer/toad/issues/85)) ([8664bcf](https://github.com/dannyhammer/toad/commit/8664bcf77ed40696c5f26cf43790fe596c559d5a))
* reverse futility pruning ([#82](https://github.com/dannyhammer/toad/issues/82)) ([85abda0](https://github.com/dannyhammer/toad/commit/85abda00317639e9ff77bc48da662954f8d80af5))

## [1.4.0](https://github.com/dannyhammer/toad/compare/v1.3.0...v1.4.0) (2024-11-17)


### Features

* reverse futility pruning ([#82](https://github.com/dannyhammer/toad/issues/82)) ([85abda0](https://github.com/dannyhammer/toad/commit/85abda00317639e9ff77bc48da662954f8d80af5))

## [1.3.0](https://github.com/dannyhammer/toad/compare/v1.2.0...v1.3.0) (2024-11-16)


### Features

* null move pruning ([#80](https://github.com/dannyhammer/toad/issues/80)) ([001500d](https://github.com/dannyhammer/toad/commit/001500d8e284619bf03dcdbf5b73a7ff9946f37f))

## [1.2.0](https://github.com/dannyhammer/toad/compare/v1.1.0...v1.2.0) (2024-11-10)


### Features

* implement History Heuristic ([#67](https://github.com/dannyhammer/toad/issues/67)) ([730a787](https://github.com/dannyhammer/toad/commit/730a787415f18bc91b7a8e3f34319e2dfa50f67e))


### Bug Fixes

* set `Score::BASE_MOVE_SCORE` to `0` (from `-32768`) ([#71](https://github.com/dannyhammer/toad/issues/71)) ([0bd13f2](https://github.com/dannyhammer/toad/commit/0bd13f2a2b71798a7eaa3ed8c0ff53b5df276096))

## [1.1.0](https://github.com/dannyhammer/toad/compare/v1.0.0...v1.1.0) (2024-11-02)


### Features

* added support for Chess960 and Double Chess960 ([#63](https://github.com/dannyhammer/toad/issues/63)) ([1f868c2](https://github.com/dannyhammer/toad/commit/1f868c28f8df18d9681466c32f516ab73159a36d))


### Bug Fixes

* fixed bug causing `make` to fail when `sed` not present on Windows ([#61](https://github.com/dannyhammer/toad/issues/61)) ([ea8104c](https://github.com/dannyhammer/toad/commit/ea8104ce240acd65fa2205ec29008edb51741618))

## [1.0.0](https://github.com/dannyhammer/toad/compare/v0.9.2...v1.0.0) (2024-10-22)


### ⚠ BREAKING CHANGES

* 1.0 release

### Features

* 1.0 release ([c1564e2](https://github.com/dannyhammer/toad/commit/c1564e2af8150f886fb0285170b38ba8d838b917))
* if debug enabled, engine prints command received ([c1564e2](https://github.com/dannyhammer/toad/commit/c1564e2af8150f886fb0285170b38ba8d838b917))

## [0.9.2](https://github.com/dannyhammer/toad/compare/v0.9.1...v0.9.2) (2024-10-22)


### Bug Fixes

* added toolchain version ([fc63c3d](https://github.com/dannyhammer/toad/commit/fc63c3d5069ae07a930e64ccd71ceaba9a99e59c))

## [0.9.1](https://github.com/dannyhammer/toad/compare/v0.9.0...v0.9.1) (2024-10-22)

### Bug Fixes

-   removed toolchain version ([82f3fd4](https://github.com/dannyhammer/toad/commit/82f3fd4154047b31d9711839fdb634c471cb827d))

## [0.9.0](https://github.com/dannyhammer/toad/compare/v0.8.0...v0.9.0) (2024-10-22)

### Bug Fixes

-   toolchain version ([b4ce3fa](https://github.com/dannyhammer/toad/commit/b4ce3fa71e1905a84ec61217257617bea6ef3c86))

## [0.8.0](https://github.com/dannyhammer/toad/compare/v0.7.0...v0.8.0) (2024-10-22)

### Features

-   added Aspiration Windows with gradual widening ([#50](https://github.com/dannyhammer/toad/issues/50)) ([42528d5](https://github.com/dannyhammer/toad/commit/42528d55213a5fe0524f7915209fbee67791f13a))
-   added release pipeline through GitHub Actions ([#52](https://github.com/dannyhammer/toad/issues/52)) ([0a16347](https://github.com/dannyhammer/toad/commit/0a16347afa2a7ed37bee1bdff1fca079fcadcf9c))
-   QSearch now stores moves in TT ([4224004](https://github.com/dannyhammer/toad/commit/4224004e434c6ee72fcc9f5025768069f8984504))

### Bug Fixes

-   fixed incorrect operator precedence in AW ([b13b0a6](https://github.com/dannyhammer/toad/commit/b13b0a6056d9dfd2931a64e52aa6aa833a7b28d5))

## [0.7.0](https://github.com/dannyhammer/toad/compare/v0.6.0...v0.7.0) (2024-10-15)

### Features

-   added support to send multiple commands on startup ([9f1ff3b](https://github.com/dannyhammer/toad/commit/9f1ff3b8c2bcd2d861b733ee2268310055e761c1))
-   added support to send UCI commands on startup ([9f1ff3b](https://github.com/dannyhammer/toad/commit/9f1ff3b8c2bcd2d861b733ee2268310055e761c1))
-   implemented PVS ([16f4f47](https://github.com/dannyhammer/toad/commit/16f4f4790c2d72042ec05c2accd077f09bb93f40))
-   promotions are given a bonus when scoring moves ([2e591de](https://github.com/dannyhammer/toad/commit/2e591de877f64355fa3311b07836115fe38bddde))

## [0.6.0](https://github.com/dannyhammer/toad/compare/v0.5.0...v0.6.0) (2024-10-14)

### Features

-   added `psqt` command to view Piece-Square table value for a given piece/square ([5a11b2f](https://github.com/dannyhammer/toad/commit/5a11b2f8ec6a185003347777a965f33b81f08f50))
-   added 50-move-rule draw detection in search ([6dfc16e](https://github.com/dannyhammer/toad/commit/6dfc16e92fd972a0fd8071c2d2bdf81768b4e091))
-   added draw by insufficient material check ([e54d937](https://github.com/dannyhammer/toad/commit/e54d9376ec0c6abdfa6051946996b16010b96354))
-   added Piece-Square tables (PeSTO's values) ([5a11b2f](https://github.com/dannyhammer/toad/commit/5a11b2f8ec6a185003347777a965f33b81f08f50))
-   added quiescence search ([041a95d](https://github.com/dannyhammer/toad/commit/041a95d4d9a24513e23f53cdd8abe22f8048eae8))
-   added repetition checks to qsearch ([e54d937](https://github.com/dannyhammer/toad/commit/e54d9376ec0c6abdfa6051946996b16010b96354))
-   added support for `debug` command ([533f7e9](https://github.com/dannyhammer/toad/commit/533f7e991657b546831f00dfd2e2c8165c88082d))
-   added TTable and TT move ordering ([fa94f94](https://github.com/dannyhammer/toad/commit/fa94f940279360fe639abe26d8c3d7512d3d5260))

### Bug Fixes

-   fixed bug causing history to not be reset on ucinewgame ([ccfc15f](https://github.com/dannyhammer/toad/commit/ccfc15f339e2fae70ffffba1a15e3c888030d5bc))
-   fixed bug causing unit tests to fail and scores to not display ([3410c50](https://github.com/dannyhammer/toad/commit/3410c501b83c526f134fad96c8346454efcbf5dd))
-   fixed typo causing wrong variable to be used ([16590c3](https://github.com/dannyhammer/toad/commit/16590c3ba68a1caf38e31a4eaceb4cde698dc4c8))
-   hopefully fixed bug with is_repetition ([30ac9f7](https://github.com/dannyhammer/toad/commit/30ac9f7c40653a6d3e2775982d480d2e8ea8828a))

## [0.5.0](https://github.com/dannyhammer/toad/compare/v0.4.0...v0.5.0) (2024-10-10)

### Features

-   added `option` command ([3e39d6d](https://github.com/dannyhammer/toad/commit/3e39d6d9227d9a50fc3f58b93294446c2b2a7ac6))
-   added MVV-LVA move ordering ([1ce7206](https://github.com/dannyhammer/toad/commit/1ce7206bdefec0a6c6fd14c9e4a9b9ca1facbb4f))
-   adjusted King's value to be 10, so that MVV-LVA orders captures with King before captures with Pawn ([253d7f4](https://github.com/dannyhammer/toad/commit/253d7f4caf81cd78cfa1e374bb10135f045fb131))
-   integrated alpha-beta pruning to search ([3791a29](https://github.com/dannyhammer/toad/commit/3791a29c0235586b76b640c2f1a8c46d43d9f57e))

## [0.4.0](https://github.com/dannyhammer/toad/compare/v0.3.0...v0.4.0) (2024-10-08)

### Features

-   added `moves [square]` command to display legal moves (at a given square) ([11dcccb](https://github.com/dannyhammer/toad/commit/11dcccbbb9ee202851b18eb922d361c02b812d77))

### Bug Fixes

-   fixed bug causing search to give weird results on even/odd depths ([11dcccb](https://github.com/dannyhammer/toad/commit/11dcccbbb9ee202851b18eb922d361c02b812d77))

## [0.3.0](https://github.com/dannyhammer/toad/compare/v0.2.0...v0.3.0) (2024-10-08)

### Features

-   remodelled command structure to be compatible with OpenBench ([7cf15a6](https://github.com/dannyhammer/toad/commit/7cf15a6ccbd26cd8ab800f94ff33d4249e98caaa))

## [0.2.0](https://github.com/dannyhammer/toad/compare/v0.1.0...v0.2.0) (2024-10-03)

### Features

-   added more utilities to the Score type ([1484b8f](https://github.com/dannyhammer/toad/commit/1484b8fd8df2c6e3d9fa32a152a6ce15c14ec7eb))

### Bug Fixes

-   fixed bug causing quit command to not parse properly ([1484b8f](https://github.com/dannyhammer/toad/commit/1484b8fd8df2c6e3d9fa32a152a6ce15c14ec7eb))

## 0.1.0 (2024-10-03)

### Features

-   added basic iterative deepening for time management ([a82e06d](https://github.com/dannyhammer/toad/commit/a82e06d57d62d688cf27f3ea4b2026a618e4de41))
-   added basic negamax search routine ([a82e06d](https://github.com/dannyhammer/toad/commit/a82e06d57d62d688cf27f3ea4b2026a618e4de41))
-   added command-line flags for passing in engine commands on startup and exiting when done ([763766c](https://github.com/dannyhammer/toad/commit/763766cac60283b5416b6bd8d611b693cb43a19e))
-   added more utilities to the Score type ([f97c7ef](https://github.com/dannyhammer/toad/commit/f97c7efc2a47992e4c4c6f55e143b793e32e6950))
-   added support for benchmarking ([a82e06d](https://github.com/dannyhammer/toad/commit/a82e06d57d62d688cf27f3ea4b2026a618e4de41))
-   added well-typed Uci responses ([763766c](https://github.com/dannyhammer/toad/commit/763766cac60283b5416b6bd8d611b693cb43a19e))
-   engine now runs with basic functionality ([a82e06d](https://github.com/dannyhammer/toad/commit/a82e06d57d62d688cf27f3ea4b2026a618e4de41))
-   finalized search skeleton ([763766c](https://github.com/dannyhammer/toad/commit/763766cac60283b5416b6bd8d611b693cb43a19e))

### Bug Fixes

-   fixed bug causing mate distance to be calculated incorrecltly for Black ([763766c](https://github.com/dannyhammer/toad/commit/763766cac60283b5416b6bd8d611b693cb43a19e))
-   fixed bug causing quit command to not parse properly ([f97c7ef](https://github.com/dannyhammer/toad/commit/f97c7efc2a47992e4c4c6f55e143b793e32e6950))
