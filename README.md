# Zeta

Zeta is a C++ card-game engine and solver project.

## Generic card game engine

The core library provides the generic card representation, deck traits, suit/rank types, and bit-mask operations used by higher-level games.

- [Core structures](doc/core/core_structures.md)
- [Glossary and math notes](doc/glossary.md)

## Texas Hold'em

The Hold'em module currently includes a lookup-based native 7-card evaluator and supporting card/board structures.

- [Lookup-based 7-card evaluator](doc/holdem/post_flop_hand_evaluator.md)
- [PokerStove range parser](doc/holdem/range_parser.md)
- [River terminal evaluator](doc/holdem/terminal_evaluator.md)

### GTO postflop analyser (multi-player capable)

Zeta is a **multi-player capable** postflop GTO analyser, evolving beyond the
heads-up-only TexasSolver model to support 2–6 player games. The architecture is
built to scale from fast heads-up evaluation to general N-way sampling without
compromising either performance tier.

#### Implemented (river terminal evaluator)

- ✅ **Board-specialized cache layer** (`river_terminal_cache`) — immutable,
  shared, zero allocation, built once per river
- ✅ **Player-neutral data structures** — templated on `N` (compile-time constant):
  `terminal_context<N>`, `terminal_values<N>`, `terminal_result<N>`
- ✅ **Generic compile-time dispatch** — `evaluate_showdown<N>`, `evaluate_fold_values<N>`
  with `static_assert(N == 2)` guard (hard error if N-way called before implementation)
- ✅ **Hand-tuned heads-up kernel** — two-stream rank-sweep algorithm in
  `evaluate_showdown_heads_up` (O(H+V), branch-predictable, allocation-free)
- ✅ **Reach index abstraction** — `river_reach_index` per player, supports exact
  traversal, blocker correction, and future sampling views
- ✅ **Documentation and tests** — see `terminal_nway_plan.md` for the multi-kernel
  architecture roadmap; 76 test cases + benchmarks confirm zero regression

#### Required next (the multi-kernel engine)

The near-term roadmap lifts heads-up optimization to support N-way evaluation
without touching the fast path:

1. **Range infrastructure** — `range_data` with three views:
   - `exact_view` (current `river_reach_index`) — hot path for heads-up
   - `sampling_view` (alias tables, CDFs, strata) — for N-way sampling
   - `blocker_view` (card-to-combo lookup) — for multiway blocker filtering

2. **Sparse N-way kernel** (3-player, narrow ranges) — exact enumeration with
   card-availability pruning and incremental blocker maintenance

3. **Sampled N-way kernel** (3–6 players) — the primary multiway route:
   - stratified sampling (premium hands vs. air)
   - importance sampling (high-impact outcomes)
   - quasi-random sequences (Sobol/Halton)
   - adaptive confidence intervals

4. **Payoff/side-pot kernel** — separate hand-rank evaluation from pot accounting:
   - `pot_structure` for unequal all-in stacks
   - win/tie accounting per pot
   - rake distribution

5. **`terminal_engine` dispatch** — route by player count, range mass, and accuracy:
   ```
   Players == 2           → heads_up_exact (current kernel)
   Players == 3, sparse   → sparse_exact (opportunistic)
   otherwise              → sampled (general multiway route)
   ```

6. **Parallel execution** — board-first parallelism with thread-local workspaces:
   - read-only shared cache
   - thread-local reach indices and sampling state
   - no synchronisation inside evaluator (single-threaded kernel)

7. **Turn / flop evaluators** — separate siblings to the river evaluator (different
   problems: C(44,1) unknown rivers on turn; C(45,2) on flop)

8. **Abstraction layers** — bucketed ranges feeding enumeration/sampled kernels

#### Benchmark baseline (heads-up river showdown)

Current performance on dense 1081-combo heads-up evaluation (WSL Clang Release):

| Benchmark | Throughput | Latency | Outcome |
|---|---:|---:|---|
| Dense showdown | 122.3M/s | ~141 µs | Two rank-buckets merged |
| Sparse (50 combos) | 84.2M/s | ~9.5 µs | Per-card blocker correction |
| Sparse (100 combos) | 91.7M/s | ~17.4 µs | Scaling typical game states |

The 40G matchups/sec throughput on dense evaluation demonstrates the architecture
is not the bottleneck; competitive differentiation will come from **systems
engineering** (memory layout, cache efficiency, parallelism), **sampling quality**
(variance reduction, adaptive refinement), and **abstraction** (bucket hierarchies,
compression).

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
