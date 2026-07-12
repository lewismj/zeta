# River Terminal Evaluator

## Purpose

The terminal evaluator computes river-node utilities for the active players on a
fixed river board from:
1. a fixed river board
2. one reach vector per active player (heads-up: two — OOP and IP)
3. pot/rake/contribution context

It provides:
- fold terminal values
- showdown terminal values
- showdown summary (`EV`, wins/ties/loss mass)

The implementation is allocation-free in hot paths and deterministic.

> The current implementation provides an optimized **heads-up** (2-player)
> evaluator. The cache layout and reach-index abstractions are player-neutral so
> that alternative multiplayer evaluators may reuse the same infrastructure in the
> future. See `terminal_nway_plan.md` for the N-way generalization design.

---

## Design: fixed heads-up evaluation vs. the generic N-way boundary

This evaluator is deliberately split into two conceptual halves: a **generic,
player-neutral boundary** (types, caches, reach indices, dispatch) and a
**specialized, hand-tuned heads-up kernel** that does the actual work today. The
guiding principle is that generalizing the *interface* must never slow down the
*implementation* of the one case that is currently implemented.

```
                       generic interface
                              |
                              v
                +---------------------------+
                | compile-time dispatch     |   evaluate_showdown<N>
                |   evaluate_showdown<N>    |   static_assert(N == 2)
                +---------------------------+
                       /              \
                      /                \
                 N == 2               N > 2
                    |                    |
                    v                    v
          evaluate_showdown_heads_up   (not implemented —
          two-stream rank sweep         hard compile error today;
          O(H + V), no allocation       future sampled / sparse-exact
          the "sacred" fast path        kernels plug in here)
```

### What is *fixed* to heads-up (the specialized kernel)

The heads-up showdown algorithm is fundamentally a **two-stream merge** of two
rank-ordered bucket streams (OOP and IP). Its core concepts only make sense with a
*single* opponent and are intentionally **not** generalized:

- **lower / equal / higher opponent mass** decomposition per hero rank
- **prefix mass accumulation** (running opponent lower-rank mass, per-card too)
- **compatible-opponent mass** via card-blocker subtraction against one opponent
- **exact-same-combo** correction in the equal-rank term

These live in `evaluate_showdown_heads_up` / `evaluate_fold_values_heads_up`. They
run in `O(H + V)` in the active hand counts, allocation-free, branch-predictable,
and single-threaded by design. This kernel is treated as *sacred*: the N-way work
must not wrap it in a generic loop or add runtime player-count branches to it.

### What is *player-neutral* (reusable infrastructure)

Everything around the kernel is templated on the player count `N` (a compile-time
constant) or is inherently per-player, so a future multiplayer kernel reuses it
without a redesign:

| Layer | Type | Neutrality |
| --- | --- | --- |
| Public API | `evaluate_showdown<N>`, `evaluate_fold_values<N>` | generic, compile-time `N` |
| Context | `terminal_context<N>` (per-seat `contribution[]`) | generic |
| Results | `terminal_values<N>`, `terminal_result<N>` | generic SoA |
| Summary | `terminal_summary<N>` | primary `static_assert`; `<2>` specialized |
| Board cache | `river_terminal_cache` | player-neutral (knows nothing of players) |
| Reach index | `river_reach_index` | inherently one-per-player |

`std::array<T, N>` (not a runtime-sized span) is used for the reach set so callers
need no dynamic allocation and the compiler unrolls/inlines the `N == 2` path to
exactly two register-resident reach indices.

### How dispatch stays zero-cost

`evaluate_showdown<N>` is a single function template that `static_assert(N == 2)`s
and then `if constexpr (N == 2)` forwards to `evaluate_showdown_heads_up`. For the
only instantiated case (`N == 2`) this collapses to a direct call with no extra
branches — the generated code is essentially identical to calling the kernel
directly. Any `N != 2` instantiation is a **hard compile error**, so an accidental
multiway call can never silently link a slow or missing evaluator.

### Why this split (and what is deferred)

Heads-up and N-way are not the same problem scaled up: exact N-way showdown cost
explodes combinatorially (`~1000^N`), so the roadmap makes *sampling* the primary
multiway route and keeps exact enumeration opportunistic (sparse 3-way only). The
board cache and reach index are already the correct shared substrate for those
future kernels. The multi-kernel `terminal_engine`, `range_data` sampling views,
payoff/side-pot kernel, and parallelism model are **deferred** — see
`terminal_nway_plan.md`. This evaluator is intentionally **river-only**; turn and
flop are different problems that warrant sibling evaluators, not a merged one.

---

## Core data structures

### `terminal_pot`, `terminal_context<N>`, `terminal_payoff`

Scalar accounting inputs and outputs:
- `gross_pot`, `rake`
- `terminal_context<N>` holds player-neutral per-seat `contribution[seat]`
  (heads-up: `contribution[0]=oop`, `contribution[1]=ip`); the player count is a
  compile-time constant
- `terminal_pot` remains the heads-up pot-accounting struct used by the payoff
  helpers; `make_heads_up_context(...)` and `heads_up_pot(...)` convert between
  the two
- per-outcome payoffs (`oop`, `ip`) for OOP win, IP win, tie, and fold

### `terminal_values<N>`, `terminal_summary<N>`, `terminal_result<N>`

- `terminal_values<N>`: templated structure-of-arrays — one contiguous per-combo
  utility array per active player. `terminal_values<2>` is the heads-up layout.
  Access by `player` enum (heads-up) or by seat index (generic).
- `terminal_summary<N>`: aggregate accounting, templated on player count. The
  primary template is intentionally unimplemented (`static_assert(N == 2)`) because
  a summary's *shape* is kernel-specific — heads-up exposes a lower/equal/higher
  decomposition that has no direct N-way analogue. The `terminal_summary<2>`
  specialization stores `oop_ev`/`ip_ev`, `oop_wins`/`ip_wins`, `ties`, and
  `matchup_weight`.
- `terminal_result<N>`: `{ terminal_values<N> values; terminal_summary<N> summary; }`.
  Fully templated so a future N-way kernel returns `terminal_result<N>` rather than
  being forced through a permanently two-seat shape. Heads-up work uses
  `terminal_result<2>`.

### `river_terminal_cache`

Board-specialized immutable cache:
- board identity (`board_hash`)
- combo masks/cards/live bitset
- evaluated ranks grouped by rank key
- `rank_order`: board-live combos sorted by `(rank, combo_index)`

This is built once per river board.

### `river_reach_index`

Range-specialized index built from `(cache, reach_vector)`:
- `weights[combo]` for direct combo lookup
- `active_indices` in cache rank order (no extra sort)
- `mass_by_card[52]` and `total_live_mass`
- rank buckets over active combos
- sparse per-bucket card masses (`bucket_card_masses`) for blocker correction

Live-only arrays are sized to `river_live_combination_count` (`C(47,2)=1081`) for better footprint.

---

## Algorithms

## 1) Cache construction: `make_river_terminal_cache`

For each of 1326 combos:
1. skip board-overlapping combos
2. evaluate 7-card hand rank for live combos
3. sort live combos by `(rank asc, combo_index asc)`
4. assign compact `rank_key` IDs and `unique_ranks`

Result: stable rank ordering and deterministic tie-breaking.

## 2) Reach indexing: `make_river_reach_index`

Single linear scan over `cache.rank_order`:
1. skip weights `<= 0`
2. append to `active_indices`
3. accumulate total mass and per-card mass
4. build rank buckets and sparse bucket card masses

No sort is performed in this phase.

## 3) Compatible mass helper

Given hero cards `(c1,c2)` and opponent mass aggregates:

`compatible = total - mass(c1) - mass(c2) + exact_same_combo_weight`

The exact-same-combo term is only relevant in equal-rank handling.

## 4) Heads-up showdown evaluation: rank sweep

This is the current, hand-tuned **heads-up** algorithm (the fast special case).
`evaluate_showdown_heads_up(...)` walks OOP and IP bucket streams in rank order as
a two-stream merge:
- tracks opponent lower-rank prefix mass (+ per-card prefix mass)
- resolves current rank as:
  - lower mass (hero win component)
  - equal mass (tie component, with exact-same-combo correction)
  - higher mass (loss component)

Per active hero combo value:

`value = lower * win + equal * tie + higher * loss`

This computes combo values and summary accumulation in one traversal.

## 5) Fold evaluation

`evaluate_fold_values(...)` computes:

`hero_value[combo] = compatible_opponent_mass(combo) * fold_payoff_component`

for each active combo on each side.

---

## API shape

Generic entry point (player count is a compile-time constant):
- `evaluate_showdown<N>(cache, std::array<river_reach_index, N>, terminal_context<N>) -> terminal_result<N>`
- `evaluate_fold_values<N>(cache, std::array<river_reach_index, N>, terminal_context<N>, folded) -> terminal_values<N>`

Both primary templates `static_assert(N == 2)`: an N-way call is a hard compile
error until a multiplayer kernel exists. For `N == 2` they dispatch to the
hand-tuned heads-up kernels:
- `evaluate_showdown_heads_up(cache, oop_index, ip_index, context)`
- `evaluate_fold_values_heads_up(cache, oop_index, ip_index, context, folded)`

Heads-up convenience overloads (forward to the kernels; keep existing call sites
working):
- `evaluate_showdown(cache, oop_reach, ip_reach, context) -> terminal_result<2>`
- `evaluate_showdown(cache, oop_index, ip_index, context) -> terminal_result<2>`
- `evaluate_fold_values(cache, oop_index_or_reach, ..., context, folded) -> terminal_values<2>`

Context helpers:
- `make_heads_up_context(gross_pot, rake, oop_contribution, ip_contribution) -> terminal_context<2>`
- `heads_up_pot(terminal_context<2>) -> terminal_pot`

Convenience wrappers:
- `evaluate_showdown_values(...) -> terminal_values<2>`
- `summarize_showdown(...) -> terminal_summary<2>`

Wrappers reuse showdown traversal outputs; they do not implement alternate logic.

---

## Determinism and safety

- deterministic tie-breaking by combo index
- assertions guard invalid board/accounting/index-hash mismatch states
- negative/zero reach weights are ignored (not normalized)
- terminal code is value-based and `noexcept`-oriented

