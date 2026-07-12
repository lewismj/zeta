# River Terminal Evaluator — N-Way Generalization Plan

## Objective

Extend the river terminal evaluator beyond heads-up to support N active players
on a fixed river board, **without losing any of the specialization that makes the
2-player (heads-up) path fast.**

The evaluator computes river-node utilities for the active players on a fixed
river board from:
1. a fixed river board (immutable, player-neutral cache)
2. one reach vector per active player
3. pot / rake / per-player contribution context

The guiding principle: **generic interface, specialized implementation, selected
at compile time via template specialization** so heads-up remains as fast as
today while multiplayer evaluators can reuse the same cache, indexing, and API
layers.

```
                    generic interface
                           |
                           v
              +--------------------------+
              | compile-time dispatch    |
              +--------------------------+
                    /              \
                   /                \
              N == 2                N > 2
                  |                    |
                  v                    v
        hand-tuned rank sweep    future algorithm
```

The player count is a **compile-time constant** throughout: CFR solvers almost
always know it statically. Prefer `std::array<T, N>` (no allocation, size known
to the compiler) over dynamically-sized containers or fixed-extent spans at the
public boundary.

---

## Design philosophy

| Layer                     | Player-awareness | Change required |
|---------------------------|------------------|-----------------|
| Public interface          | Player-neutral   | Reword / `std::array<T,N>` |
| Generic infrastructure    | Player-neutral   | Minor           |
| Heads-up sweep (algorithm)| Specialized (2)  | Isolate, keep   |
| `river_terminal_cache`    | Player-neutral   | **None**        |
| `river_reach_index`       | Player-neutral (per-player) | **None** |
| Current implementation    | Heads-up only    | Preserve        |

**Generic infrastructure** (reusable by any `N`):
- rank buckets
- active indices
- per-card mass accounting
- compatible mass accounting

**Heads-up-only algorithm** (do *not* imply this is reusable):
- two-stream rank sweep
- lower / equal / higher mass decomposition

Estimated scope: roughly **10–15%** of the surrounding code/documentation needs
to change to make the design future-proof. The hot heads-up algorithm itself is
untouched.

---

## What must NOT change

The following are **fundamentally heads-up** concepts and must not be abstracted
now — doing so would reduce clarity and almost certainly slow the hot loop:

- lower prefix mass
- equal mass
- higher mass

These rely on there being exactly **one** opponent. The two-stream rank sweep in
`evaluate_showdown` is the fast specialization and should remain a distinct,
hand-tuned code path.

Note the distinction from *generic* infrastructure: **compatible mass
accounting**, rank buckets, active indices, and per-card mass are reusable by any
`N`. It is specifically the *lower / equal / higher decomposition against a
single opponent* (the two-stream sweep) that is heads-up-only.

Also leave completely alone:
- `river_terminal_cache` — already player-neutral; it knows nothing about players.
- `river_reach_index` — a reach index is inherently for one player; already correct.
- `make_river_terminal_cache`, `make_river_reach_index` — no change.

---

## Target architecture

```
Public interface   ── player-neutral (std::array<river_reach_index, N>)
Generic infra      ── player-neutral (rank buckets, active indices, card/compat mass)
Caches             ── player-neutral (river_terminal_cache)
Reach indices      ── player-neutral (river_reach_index, one per player)
Heads-up algorithm ── specialized (evaluate_showdown<2> = two-stream sweep)
Current impl       ── heads-up only (2-player specialization provided)
```

The generic entry point dispatches at **compile time** to a specialization. Use
`std::array<T, N>` (not a fixed-extent span) so callers need no pre-built
contiguous buffer and the size is a compile-time constant:

```cpp
// Primary generic template: fails loudly until multiplayer is implemented.
template <std::size_t N>
terminal_result<N> evaluate_showdown(
    const river_terminal_cache& cache,
    const std::array<river_reach_index, N>& reach,
    const terminal_context<N>& context) noexcept {
    static_assert(N == 2, "N-way showdown evaluator not implemented");
}

// Fast, hand-tuned heads-up specialization (current algorithm).
template <>
terminal_result<2> evaluate_showdown<2>(
    const river_terminal_cache& cache,
    const std::array<river_reach_index, 2>& reach,
    const terminal_context<2>& context) noexcept;
```

`N == 2` resolves to the existing optimized two-stream sweep with zero runtime
branching. The primary template `static_assert`s so an N-way call **fails to
compile** rather than silently linking a slow or missing evaluator. Other `N`
can be added later as an explicit specialization without touching the heads-up
path.

---

## Commercial multi-kernel architecture

Do **not** build a single monolithic "N-way terminal evaluator." Build a
`terminal_engine` that dispatches to **regime-specific kernels** over a shared
cache/range layer. The heads-up evaluator becomes the fastest special case.

```
                      terminal_engine
                             |
        +--------------------+--------------------+
        |                    |                    |
        v                    v                    v
   Heads-up exact      Sparse exact         Sampled
   two-stream sweep    (3-way, narrow)      Monte Carlo / CFR sampling
     <2 players>       <3, sparse only>       <3–6 players, general>
```

**Complexity reality check.** N-way showdown is not merely "more players" — exact
enumeration cost explodes. Heads-up is `~1000 × 1000 = 10^6` comparisons; three
players is `~1000^3 ≈ 10^9` and six players `~1000^6`, which is intractable. So
exact enumeration is only viable for very small player counts **and** sparse
ranges. Do **not** promise "3–4 exact" as a general capability — sampling is the
primary N-way route. Realistic regimes:

- **2 players** → exact rank sweep (current kernel)
- **3 players, sparse ranges** → limited exact enumeration (opportunistic only)
- **3–6 players (general)** → sampled evaluator (the main multiway path)
- **6+ players** → stratified / quasi-random Monte Carlo

The kernel is selected explicitly, not by an ad-hoc chain of `if`s:

```cpp
enum class terminal_kernel { heads_up_exact, sparse_exact, sampled };
```

Dispatch decision inputs:
- number of players
- range sizes (active combo counts / total range mass)
- requested accuracy
- CFR iteration mode (exact vs sampled)

A possible top-level API:

```cpp
enum class terminal_mode { exact, sampled };

template <std::size_t Players>
terminal_result<Players> evaluate_terminal(
    const river_terminal_cache&           cache,
    const std::array<range_data, Players>& ranges,
    const terminal_context<Players>&      context,   // carries pot_structure
    terminal_options                      options) noexcept;
```

Internal routing:

```
Players == 2                         -> heads_up_exact (current kernel)
Players == 3 && range_mass < thresh  -> sparse_exact (opportunistic)
otherwise                            -> sampled evaluator
```

### Layer 1 — Shared board-specialized infrastructure

Already strong in the current design. `river_terminal_cache` is the "terminal
database" layer: built once, shared by every thread.

```
river_terminal_cache
  board_hash
  combo_cards / combo_masks          // cards
  hand_rank[1081] / rank_key[1081]   // evaluator output
  rank_order                         // ordering
  rank_buckets                       // grouping
  card_to_combos                     // blocker support (additive)
```

The only additive item vs today is an explicit `card_to_combos` (card → combos
containing it) to accelerate blocker/availability queries in the multiway
kernels. Heads-up does not require it.

**Do not rename `river_terminal_cache` now.** It is already implemented,
benchmarked, and understood; a rename is pure churn. It *is* conceptually a
fixed-board cache — document that, and if flop/turn caches later appear, revisit
with an alias rather than a rename:

```cpp
// Optional, later — no churn to existing code:
using board_terminal_cache = river_terminal_cache;
```

### Layer 2 — Player range representation (`range_data` with distinct views)

A commercial solver keeps **multiple views** of a range, but they should be
**separate structures**, not one overloaded object — the requirements differ and
the hot path must not be contaminated by sampling metadata:

```
range_data
  ├─ exact_view      // hot path: weight[1081], active_indices, rank buckets
  ├─ sampling_view   // alias_table / CDF / strata (sampled kernel only)
  └─ blocker_view    // card_mass[52], card availability (multiway only)
```

`river_reach_index` (current) **is** the `exact_view` — leave it lean and
unchanged. The heads-up kernel touches only `exact_view`. The `sampling_view`
(alias table, CDF, strata) and richer `blocker_view` are additive, built only
when a sampled/enumeration kernel needs them, so they never bloat the two-stream
sweep's working set.

### Layer 3 — Exact heads-up kernel (`<2>`)

The current algorithm, unchanged and maximally optimized:

```
OOP rank buckets ---- linear sweep ---- IP rank buckets
```

### Layer 3 — Exact heads-up kernel (`<2>`), kept sacred

The current algorithm, unchanged and maximally optimized. It is fundamentally a
**two-stream merge** of the two players' rank buckets:

```
OOP rank buckets ---- linear sweep ---- IP rank buckets
```

Complexity `O(H + V)` in active hands. This is the reference-quality fast path.

**Do not** dissolve it into a generic loop with a `players == 2` special case:

```cpp
// ANTI-PATTERN — do not do this:
for (auto& p : players) { ... }   // then: if (players == 2) ...
```

That is exactly how high-performance code gets slower. The `<2>` specialization
must stay a distinct, hand-written two-stream merge — never a degenerate case of
a generic N-way loop.

### Layer 4 — Exact small-multiway kernel (3 players, narrow ranges)

Only realistic for **3 players with narrow ranges** (see the complexity check
above — general 4-way exact is intractable). Recursive enumeration across player
ranges:

```
player 0 range
  └─ player 1 range
       └─ player 2 range
            └─ evaluate
```

Key optimizations:
- **Card availability pruning** — never generate impossible (blocked) branches.
- **Range ordering** — enumerate strongest / most likely combos first.
- **Incremental blockers** — maintain a running `used_cards` mask instead of
  rebuilding masks per branch.

### Layer 5 — Large-multiway kernel (4+ players, sampled)

Where commercial solvers differentiate — a serious sampling engine, not naive
"random opponent hand":

- **Stratified sampling** — split ranges into strata (premium / middle / air) and
  sample within strata for variance reduction.
- **Importance sampling** — bias toward high-EV-impact states; correct with
  `EV = Σ payoff / sampling_probability`.
- **Quasi-random sequences** — Sobol / Halton instead of plain PRNG for lower
  variance.
- **Adaptive sampling** — spend more samples where per-board EV confidence is low;
  stop early on high-confidence boards.

### Layer 5b — Payoff kernel (separate from hand evaluation)

Multiway pots make payoff computation a **distinct concern** from ranking hands.
Keep the pipeline layered:

```
showdown evaluator          // who wins / ties (rank comparison)
        |
        v
hand outcome generator      // per-player win/tie/loss classification
        |
        v
payoff calculator           // side pots, folded players, rake, splits
```

The hand evaluator must **not** own pot logic. Multiway introduces side pots from
unequal all-in contributions — e.g. A all-in 100, B all-in 50, C all-in 200 →
main pot + two side pots. Model this in the context, not the evaluator:

```cpp
template <std::size_t N>
struct terminal_context {
    utility               rake;
    std::array<utility,N> contribution;
    pot_structure         pots;        // main + side pots
};
```

Heads-up degenerates to a single pot, so `terminal_context<2>` stays trivially
cheap; the payoff kernel only does real work for multiway.

### Layer 6 — Parallel execution priority

Parallelize the **outer** dimensions first; sampling parallelism is a last resort
because sampling is cheap relative to board/subtree work:

1. **Boards** (best) — independent, no communication.
2. **CFR traversal chunks** — subtree-local accumulation.
3. **Samples** (last resort).

### Memory model (unchanged shape, one addition)

```
SHARED READ-ONLY   game graph, river caches, lookup tables
THREAD-LOCAL       RNG, sampling state, scratch, temporary EV
GLOBAL             regrets, strategy sums
REDUCTION          batch updates
```

The additive thread-local state is the **RNG and sampling scratch** required by
the sampled kernel. Heads-up traversals never touch it.

### Abstraction layer (multiway prerequisite)

Commercial multiplayer solvers do not solve raw cards; they bucket:

```
raw cards -> hand buckets -> information sets
```

(e.g. `AA` on a dry vs wet board need not be the same infoset.) The terminal
evaluator must therefore also accept **abstracted ranges** — `range_index` should
be able to represent bucketed mass, not only raw 1081-combo weights. This mainly
affects the sampled/enumeration kernels; heads-up exact remains raw-combo.

### Where the real advantage comes from

A cleverer showdown evaluator alone will not beat established solvers. The edge
comes from:
1. **Systems engineering** — cache-friendly SoA, no allocations, deterministic
   reductions (already the current direction).
2. **Better sampling** — variance reduction, adaptive sampling, SIMD/GPU rollouts.
3. **Memory architecture** — compressed infosets, compact regret storage,
   NUMA-aware scheduling, huge pages.
4. **Parallelism** — many-core CPUs, GPU terminal rollouts, distributed solving.

### Scope note

Layers 4–6, the abstraction layer, and `range_index`'s sampling views are
**future, additive work**. The near-term deliverable remains: player-neutral
interface + isolated heads-up `<2>` kernel over the shared cache/range layer.
This document's implementation sequence below covers only that near-term scope;
the multi-kernel layers are the roadmap they plug into.

---

## Changes by section

### 1. Objective wording

Replace "computes river-node utilities for **both players**" with
"computes river-node utilities for **the active players** on a fixed river
board." Equally true for 2 or 6 players. (Applied above and to
`terminal_evaluator.md`.)

### 2. Player-neutral public API (remove OOP/IP from signatures)

Instead of:

```cpp
evaluate_showdown(cache, oop_index, ip_index, context);
```

expose a static-`N` array API (CFR solvers know the player count at compile
time — no dynamic allocation, size known to the compiler):

```cpp
template <std::size_t N>
terminal_result<N> evaluate_showdown(
    const river_terminal_cache& cache,
    const std::array<river_reach_index, N>& reach,
    const terminal_context<N>& context) noexcept;
```

Prefer `std::array<river_reach_index, N>` over `std::span<const T, N>`: a span
requires the caller to already own a contiguous N-element buffer, which is
awkward at the public boundary. Heads-up callers pass
`std::array<river_reach_index, 2>`; six-player callers pass `…, 6`.

Internally the heads-up specialization immediately binds `reach[0]` / `reach[1]`
(the compiler places these in registers). **Do not** reintroduce `oop_index` /
`ip_index` names inside the implementation — index by seat so the terminology
does not leak back. Keep the existing `(oop_index, ip_index)` overload only as a
thin forwarding wrapper that builds a 2-array and calls `evaluate_showdown<2>`.

### 3. Player-neutral, compile-time-sized `terminal_context<N>`

The context is passed everywhere, so it is part of the hot API. Avoid a
`std::span` member (pointer + size + possible dangling). Template the context on
the player count instead:

```cpp
template <std::size_t N>
struct terminal_context {
    utility gross_pot = 0.0;
    utility rake      = 0.0;
    std::array<utility, N> contribution{}; // per-seat contributions
};
```

`terminal_context<2>` is literally `gross_pot`, `rake`, and two floats — no
pointer, no size, no dangling risk, and just as cache-friendly as today.
`terminal_context<N>` is the clean generalization for the future path. Provide a
small helper to build a heads-up context from `(oop_contribution,
ip_contribution)` for source compatibility.

### 4. Generalize `terminal_values` (templated SoA — never nested vectors)

Make "one value array per active player" explicit — **without** falling back to
`std::vector<std::vector<float>>`, which would undo the SoA design. Template on
the player count and keep contiguous per-seat arrays:

```cpp
using value_array = std::array<terminal_value, combination_count>; // compact SoA

template <std::size_t N>
struct terminal_values {
    std::array<value_array, N> player_values{};
};
```

`terminal_values<2>` is exactly the current two-array layout. Index by
seat/player index in the generic form rather than an `oop`/`ip` enum. The
heads-up specialization keeps its existing storage unchanged.

Size is not a concern even at 6 players: `6 × 1081` floats ≈ 26 KB — trivial, no
need for compression or nesting.

### 5. Generalize `terminal_summary`

Current names (`oop_ev`, `ip_ev`, `oop_wins`, `ip_wins`, `ties`) are heads-up
specific. Describe the summary **conceptually** in docs:

- aggregate EV (per active player)
- win / tie accounting
- matchup weight

The heads-up implementation may retain `oop_ev` / `ip_ev` internally; the
documentation and generic interface need not expose those names. For the generic
form, prefer per-player arrays (`ev[seat]`, `wins[seat]`) plus shared `ties` /
`matchup_weight`.

### 6. Reach terminology

Replace "two reach vectors (OOP/IP)" with "**one reach vector per active
player**." For heads-up: two reach vectors. No behavioural change.

### 7–8. Cache and reach index — leave alone

`river_terminal_cache` and `river_reach_index` are already the correct
player-neutral abstractions. No changes.

### 9. Document the specialization explicitly

Add to `terminal_evaluator.md`:

> The current implementation provides an optimized heads-up evaluator. The cache
> layout and reach-index abstractions are player-neutral so that alternative
> multiplayer evaluators may reuse the same infrastructure in the future.

### 10. Separate algorithm from infrastructure in docs

Rename the "Showdown evaluation" section to **"Heads-up showdown evaluation"**
(or "Current heads-up algorithm") so it does not imply it is the only possible
implementation.

### 11. Keep the fast specialization visible

Document that the generic interface is backed by a specialized implementation,
e.g. `evaluate_showdown_heads_up(...)` or `template<> evaluate_showdown<2>(...)`.
Communicates clearly: **interface is generic, implementation is specialized** —
the standard structure for high-performance libraries.

### 12. Fold evaluator follows the same pattern

Fold is much easier to generalize than showdown (it is compatible mass ×
constant payoff), so template it the same way:

```cpp
template <std::size_t N>
terminal_values<N> evaluate_fold_values(
    const river_terminal_cache& cache,
    const std::array<river_reach_index, N>& reach,
    const terminal_context<N>& context,
    /* folded / active players */ ...) noexcept;
```

Express the folded/active set explicitly (e.g. a `folded_player` seat index or an
`active_players` mask) rather than the heads-up `player folded` enum. Provide a
`<2>` path matching current behaviour; a generic `N` fold path can follow because
the math generalizes cleanly.

### 13. Player count is a solver property

Do not let `evaluate_showdown<N>` be the *only* place that knows the player
count. Thread it through the solver so heads-up and multiplayer share one knob:

```cpp
template <std::size_t Players>
class solver {
    terminal_evaluator<Players> evaluator;
    regret_table                regrets;
    traversal<Players>          traversal;
};
```

Heads-up is `solver<2>`; six-player is `solver<6>`. This keeps the compile-time
player count consistent from the terminal evaluator up to the solver.

---

## Implementation sequence

1. **Introduce `terminal_context<N>`.** Template the context on player count with
   a `std::array<utility, N> contribution` member; add a heads-up builder helper
   that maps `(oop_contribution, ip_contribution)` for source compatibility.
   **✅ Done** — `terminal_context<N>` + `make_heads_up_context` / `heads_up_pot`
   helpers in `terminal.h`; all heads-up evaluate/summary APIs take
   `terminal_context<2>`; tests + benchmark migrated; 74 test cases pass.
2. **Add the `std::array<T,N>` public API.** Add
   `evaluate_showdown<N>(cache, std::array<river_reach_index, N>, context)` and
   the matching fold entry point, with a primary-template `static_assert(N == 2)`
   so non-heads-up calls fail to compile until implemented.
   **✅ Done** — generic `evaluate_showdown<N>` / `evaluate_fold_values<N>`
   templates added with `static_assert(N == 2, ...)`; verified a `<3>` call is a
   hard compile error. Parity tests added
   (`holdem_terminal_showdown_array_api_matches_index_api`,
   `holdem_terminal_fold_array_api_matches_index_api`).
3. **Extract the heads-up algorithm** into an explicitly named specialization
   (`evaluate_showdown<2>` / `evaluate_showdown_heads_up`). No logic change —
   pure move/rename so the fast path is isolated and named. Bind `reach[0]` /
   `reach[1]` internally; do not reintroduce `oop`/`ip` names.
   **✅ Done** — two-stream kernel is now `evaluate_showdown_heads_up`
   (fold: `evaluate_fold_values_heads_up`); the generic `<2>` path binds
   `reach[0]`/`reach[1]` and forwards. Algorithm bytes unchanged.
4. **Keep existing overloads** as thin forwarding wrappers (build a 2-array and
   call the `<2>` specialization) so current call sites and benchmarks compile
   unchanged.
   **✅ Done** — index-pair and `reach_vector` overloads retained as forwarders;
   all pre-existing call sites/tests unchanged (76 test cases pass).
5. **Generalize the value/summary containers** as templated SoA
   (`terminal_values<N>`, seat-indexed summary) while the heads-up specialization
   keeps its 2-array storage.
   **✅ Done** — `terminal_values<N>` (SoA `std::array<value_array, N>`) with both
   `player`-enum and seat-index accessors; `terminal_values<2>` is the unchanged
   heads-up layout. `terminal_summary<N>` and `terminal_result<N>` are now fully
   templated too (the primary `terminal_summary` `static_assert`s; a `<2>`
   specialization holds the heads-up lower/equal/higher fields), so a future N-way
   kernel returns `terminal_result<N>` rather than a permanently two-seat shape.
   Seat-vs-enum access covered by tests (76 pass).
6. **Update documentation** (`terminal_evaluator.md`) per sections 1, 5, 6, 9, 10.
   **✅ Done** — objective reworded to "active players"; specialization note added;
   `terminal_context<N>` / `terminal_values<N>` / summary described conceptually;
   showdown section retitled "Heads-up showdown evaluation"; API shape documents
   the generic `<N>` entry points, `static_assert` guard, HU kernels, and context
   helpers.
7. **(Future, separate work)** Add a correctness-first generic `N`-way showdown
   evaluator as its own specialization. Do **not** modify the heads-up path to
   accommodate it.

Each step is source-compatible and independently testable. Steps 1–6 are the
"future-proofing" work (≈10–15%); step 7 is deferred multiplayer work.

### Future roadmap (the multi-kernel engine)

Beyond the near-term refactor, the tiered `terminal_engine` grows in this order:

8. **`range_data` split views** — introduce `sampling_view` (`alias_table` / CDF
   / strata) and richer `blocker_view` as *separate* structures alongside the
   existing lean `exact_view` (`river_reach_index`). Do not overload the hot path.
9. **`card_to_combos`** in the cache for fast blocker/availability queries.
10. **Payoff kernel + `pot_structure`** — separate showdown ranking from side-pot
    payoff computation; add `pot_structure` to `terminal_context<N>`.
11. **3-player narrow-range exact enumeration kernel** with card-availability
    pruning, range ordering, and incremental `used_cards` blockers.
12. **Sampled 4+ player kernel**: stratified + importance + quasi-random
    (Sobol/Halton) + adaptive sampling.
13. **`terminal_engine` dispatch** (`evaluate_terminal`) routing on player count,
    range sizes, accuracy, and CFR mode.
14. **Abstraction support** — bucketed ranges feeding the enumeration/sampled
    kernels.
15. **Parallelism & memory** — board-first parallelism, NUMA-aware scheduling,
    compact regret/infoset storage, optional SIMD/GPU terminal rollouts.

**Additional architectural guidance (review round 4):**

- **Keep the evaluator single-threaded.** Do *not* parallelize inside a showdown
  (rank-bucket sweeps synchronize badly and thrash cache). Parallelize the *outer*
  dimension instead: CFR traversal / board batches call the evaluator with a
  read-only cache and a thread-local workspace. The kernel stays cache-friendly
  and branch-predictable.
- **Reach indices are scratch, not persistent.** `river_terminal_cache` is the
  persistent shared object; a `river_reach_index` is ~129 KB per player and must
  not be retained per CFR node. Eventually wrap them in a thread-local
  `terminal_workspace { std::array<river_reach_index, N> players; scratch; }` that
  is reused across evaluations.
- **One evaluator per street — do not unify.** Keep this a *river-only* evaluator.
  Turn (`C(44,1)` unknown rivers) and flop (`C(45,2)` unknown turn+river) are
  different problems; a single all-streets evaluator destroys the specialization.
  The engine gains sibling `turn_evaluator` / `flop_evaluator`, not a merged one.
- **Sampling is the primary N-way route.** Reaffirmed: exact 3-way is opportunistic
  (sparse ranges only); a sampled kernel is the general multiway path.

The heads-up `<2>` kernel remains untouched throughout; each new kernel is
additive and shares Layers 1–2.

---

## Testing requirements

- **Parity:** `std::array<T,2>` API produces bit-identical results to the current
  `(oop_index, ip_index)` API for a range of boards and ranges.
- **Context equivalence:** `terminal_context<2>.contribution` reproduces prior
  `oop_contribution` / `ip_contribution` payoffs exactly.
- **Specialization dispatch:** `evaluate_showdown<2>` is selected for 2-player
  arrays; the primary template `static_assert` fires for any other `N`.
- **Fold parity:** `<2>` fold evaluation matches current fold values.
- **Determinism:** unchanged tie-breaking by combo index.

---

## Benchmark requirements

- Confirm **no regression** on the heads-up showdown and fold benchmarks after
  the refactor (array/context indirection must not cost measurable time).
- **Code-generation check:** templates can silently duplicate code. Verify the
  assembly of `evaluate_showdown<2>` is essentially identical to today —
  `reach[0]`/`reach[1]` in registers, full inlining, no extra branches — and
  watch binary size for template bloat.
- Reuse the existing benchmark structure and standards
  (`evaluator_benchmark.cpp` conventions; see also `terminal_benchmark.cpp`).
- Compare pre/post refactor for: cache construction, reach index construction,
  dense showdown, dense fold, sparse scaling (50/100/300/1081).
- Do not add memory-layout no-op micro-benchmarks.

---

## Summary

This plan future-proofs the terminal evaluator with minimal, surgical changes,
inside a commercial-grade multi-kernel architecture:

- Public interface, infrastructure, caches, and reach indices become (or remain)
  player-neutral over a shared **cache/range layer** (Layers 1–2).
- The **heads-up algorithm stays a distinct, hand-tuned compile-time
  specialization** (`evaluate_showdown<2>`) — as fast as today, the fastest
  special case.
- A `terminal_engine` dispatches by regime: **heads-up exact** (2, current kernel),
  **sparse 3-way exact** (opportunistic, narrow ranges only), and **sampled**
  (the general 3–6-player route) — chosen on player count, range sizes, accuracy,
  and CFR mode.
- Multiplayer support is **additive**: each new kernel plugs into the same cache,
  range, API, and memory layers rather than forcing a redesign.
- The real competitive edge comes from systems engineering, sampling quality,
  memory architecture, and parallelism — not a cleverer showdown kernel alone.
