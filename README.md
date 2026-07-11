# Zeta

Zeta is a C++ card-game engine and solver project.

## Generic card game engine

The core library provides the generic card representation, deck traits, suit/rank types, and bit-mask operations used by higher-level games.

- [Core structures](doc/core/core_structures.md)

## Texas Hold'em

The Hold'em module currently includes a lookup-based native 7-card evaluator and supporting card/board structures.

- [Lookup-based 7-card evaluator](doc/holdem/post_flop_hand_evaluator.md)

### GTO postflop analyser

The intended direction is a TexasSolver-style postflop GTO analyser. The next major layer is a full postflop game model with river-first CFR execution:

- range and combo representation
- postflop action tree
- board-specialized showdown evaluation
- CFR/CFR+ regret and average-strategy storage
- exploitability / best-response measurement
- turn and flop chance-node rollout after the river path is correct

### Evaluator benchmark summary

Recent release benchmark results for the chunked restricted-quinary evaluator:

| Environment | Random 7-card median | Adversarial median | Table data |
|---|---:|---:|---:|
| WSL Clang release | ~7.35 ns/eval | ~6.73 ns/eval | ~337 KiB |
| MSVC release | ~9.19 ns/eval | ~9.39 ns/eval | ~337 KiB |

The evaluator uses a direct flush lookup plus a dense restricted-quinary non-flush table, avoiding 21-subset 5-card enumeration.

## Build presets

The project includes CMake presets for MSVC and WSL/Clang release builds, with PEXT/PDEP variants for benchmarking:

```bash
cmake --preset msvc-release
cmake --build --preset msvc-release --target zeta-bench-holdem-probes

cmake --preset msvc-release-pext
cmake --build --preset msvc-release-pext --target zeta-bench-holdem-probes
```

The equivalent WSL/Clang presets are `wsl-clang-release` and `wsl-clang-release-pext`.
