# Zeta

Zeta is a C++ card-game engine and solver project.

## Disclaimer

Zeta is an independent research and software project developed for
educational and experimental purposes. It is not intended to provide
financial, investment, gambling, or other professional advice. The
software is provided "as is" without warranty.

## Generic card game engine

The core library provides the generic card representation, deck traits, suit/rank types, and bit-mask operations that
can be used to implement trick taking card games. The Texas Hold'em module has its own representations and algorithms.

- [Core structures](doc/core/core_structures.md)

## Texas Hold'em

The Hold'em module currently includes a lookup-based native 7-card evaluator and supporting card/board structures.

- [Core structures](doc/holdem/core_structures.md)
- [Core algorithms](doc/holdem/core_algorithms.md)
- [Lookup-based 7-card evaluator](doc/holdem/post_flop_hand_evaluator.md)
- [PokerStove-compatible range parser](doc/holdem/range_parser.md)
- [River terminal evaluator](doc/holdem/terminal_evaluator.md)
- [zeta-solve CLI](doc/holdem/cli_usage.md)
- [Hold'em solver UI user guide](doc/holdem/ui/user_guide.md)

### GTO postflop analyser

Zeta includes a multiway postflop CFR solver for **flop**, **turn**, and **river** Hold'em
analysis, exposed through both `zeta-solve` CLI and the solver UI. River leaves are
evaluated by the terminal evaluator documented in
[River terminal evaluator](doc/holdem/terminal_evaluator.md), which currently
implements:

- board-specialized river caches (`river_terminal_cache`)
- per-player reach indices with blocker-corrected mass accounting
- dedicated heads-up evaluator with exact showdown and fold kernels
- exact N-way terminal-state evaluation for explicit showdown/fold states
- sampled N-way showdown value estimation for scalable multi-player contexts
- side-pot and rake-aware terminal-state payoff distribution
- thread-local terminal workspaces for solver traversal

#### Benchmark baseline (CFR runtime and scheduler)

Latest Google Benchmark summary from `zeta-bench-holdem-cfr` on Release WSL Clang (`setarch x86_64 -R`):

| Benchmark | Throughput | CPU time | Purpose |
|---|---:|---:|---|
| `BM_CFRIterationMedium/1/real_time` | `edges/s=54.7146M/s` | `17.205 us` | Medium graph single-worker CFR iteration baseline |
| `BM_CFRIterationMedium/4/real_time` | `edges/s=162.42M/s` | `58.487 us` | Medium graph CFR scaling at 4 workers |
| `BM_CFRIterationMedium/8/real_time` | `edges/s=186.081M/s` | `128.162 us` | Medium graph CFR scaling at 8 workers |
| `BM_CFRIterationLarge/1/real_time` | `edges/s=53.1613M/s` | `26.777 us` | Large graph single-worker CFR iteration baseline |
| `BM_CFRIterationLarge/4/real_time` | `edges/s=151.907M/s` | `93.186 us` | Large graph CFR scaling at 4 workers |
| `BM_CFRIterationLarge/12/real_time` | `edges/s=223.395M/s` | `336.141 us` | Large graph CFR scaling at 12 workers |
| `BM_CFRIterationLargeRealistic/8/real_time` | `edges/s=168.171M/s` | `210.954 us` | Large realistic scheduling profile (chunk size 64) |
| `BM_CFRIterationLargeChunkSize/12/1/real_time` | `edges/s=225.67M/s` | `316.873 us` | Chunk-size sweep best point on large graph (12 workers) |
| `BM_CFRIterationLargeChunkSize/12/64/real_time` | `edges/s=160.533M/s` | `308.604 us` | Chunk-size sweep larger chunks on same workload |
| `BM_CFRIterationLargeChunkSize/12/128/real_time` | `edges/s=118.043M/s` | `263.516 us` | Chunk-size sweep upper bound (over-chunked) |
| `BM_BoardPartitionSchedulerRealistic/12/real_time` | `actions/s=6.19082G/s, tasks/s=154.884M/s` | `197.977 us` | Dynamic board-partition scheduler realistic workload |
| `BM_BoardPartitionStaticRangeRealistic/12/real_time` | `actions/s=8.44126G/s` | `205.034 us` | Static-range scheduler comparison point |

### Evaluator benchmark summary

Recent release benchmark results for the chunked restricted-quinary evaluator:

| Environment | Random 7-card median | Adversarial median | Table data |
|---|---:|---:|---:|
| WSL Clang release | ~7.12 ns/eval | ~7.41 ns/eval | ~337 KiB |
| MSVC release | ~9.19 ns/eval | ~9.39 ns/eval | ~337 KiB |

The evaluator uses a direct flush lookup plus a dense restricted-quinary non-flush table, avoiding 21-subset 5-card enumeration.

Latest Google Benchmark summary from `zeta-bench-holdem-evaluator` on Release WSL Clang:

| Benchmark | Throughput | CPU time | Purpose |
|---|---:|---:|---|
| `BM_DenseTableLookup` | ~674.0M/s | ~73.0 us / 49,205 slots | Full dense non-flush rank-table scan |
| `BM_FullEvaluateRandom` | ~133.5M/s | ~7.49 ns/eval | Random 7-card evaluator path |
| `BM_FullEvaluateAdversarial` | ~144.4M/s | ~6.93 ns/eval | Repeated adversarial 7-card patterns |
| `BM_EvaluateFromMasksRandom` | ~154.7M/s | ~6.46 ns/eval | Evaluator from precomputed suit/rank masks |
| `BM_IsolatedQuinaryIndex` | ~187.2M/s | ~5.34 ns/index | Non-flush quinary index from masks |
| `BM_QuinaryIndexFromLayers` | ~265.8M/s | ~3.76 ns/index | Quinary index from precomputed rank layers |
| `BM_MasksOnly` | ~621.7M/s | ~1.61 ns/hand | Card mask to suit/rank masks |
| `BM_MasksFlushCheck` | ~472.6M/s | ~2.12 ns/hand | Mask construction plus flush detection |
| `BM_MasksFlushIndex` | ~153.7M/s | ~6.51 ns/hand | Mask construction plus flush/non-flush indexing |
| `BM_DenseLookupOnly` | ~668.4M/s | ~1.50 ns/lookup | Dense non-flush table lookup only |
| `BM_EvaluateAllSevenCards` | ~156.1M/s | ~6.41 ns/eval | Exhaustive 52 choose 7 evaluator run |

These numbers show the hot path is the non-flush restricted-quinary index, not the dense rank-table lookup.

## Build profiles

Use the CLion CMake profiles for local builds:

- `Debug-WSL (Clang)`
- `Release-WSL (Clang)`
- `Debug-Visual Studio`
- `Release-Visual Studio`
