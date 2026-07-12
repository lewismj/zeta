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
    const terminal_context<Players>&      context,   // lean: gross_pot, rake, contribution
    const pot_structure<Players>&         pots,      // side pots + active-set (payoff kernel)
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

### Layer 5b — Payoff kernel: active-set & `pot_structure` (design)

Multiway pots make payoff computation a **distinct concern** from ranking hands.
Heads-up has a single pot and a single opponent, so ranking *is* payoff. Multiway
does not: unequal all-in stacks create **side pots**, and folded players leave
**dead money** they can never win. The pipeline must be layered so the hot ranking
kernels never learn about pots:

```
showdown ranking            // per-seat hand strength (rank_key) or mass decomposition
        |
        v
active-set + side-pot build // who is eligible for which pot layer
        |
        v
payoff calculator           // distribute each layer, net vs contribution, rake
```

#### Concepts

- **Contribution** `c_i` — total chips seat `i` committed to the pot this hand.
- **Active-set (contested seats)** — the seats that reached showdown *without
  folding*. A folded seat's chips stay in the pots (dead money) but it is never
  eligible to win any layer. This is the "who folded" information that was
  deliberately kept **out** of the lean showdown `terminal_context<N>` — it belongs
  here, in the payoff input, not on the ranking hot path.
- **Side-pot layer** — a slice of the pot defined by an all-in level. Layer `k`
  spans contribution band `(level_{k-1}, level_k]` and is contestable only by seats
  that committed at least `level_k`.

#### Input type — `pot_structure<N>`

`pot_structure<N>` is the payoff-kernel input (separate from the showdown context),
templated on the compile-time seat count:

```cpp
// Compact seat set: N <= 8 fits a byte; keep it a value type, no allocation.
template <std::size_t N>
using seat_set = std::bitset<N>;            // or a uint8_t mask for N <= 8

template <std::size_t N>
struct pot_structure {
    utility               rake         = 0.0;   // raked off the gross pot
    std::array<utility,N> contribution{};        // c_i per seat (folded seats included)
    seat_set<N>           contested{};           // active-set: seats still eligible to win
};
```

`sum(contribution)` is the gross pot; folded seats appear in `contribution` (their
dead money) but are cleared in `contested`.

#### Derived type — side-pot layers

Layers are *derived* from `pot_structure`, not stored on it (keep the input
minimal). At most `N` distinct all-in levels ⇒ at most `N` layers:

```cpp
struct side_pot {
    utility     amount   = 0.0;   // chips in this layer (gross, pre-rake-scale)
    seat_set<N> eligible{};        // contributed >= level AND contested
};

template <std::size_t N>
struct side_pot_layers {
    std::array<side_pot, N> layer{};
    std::size_t             count = 0;
};
```

**Build algorithm** (`build_side_pots`, `O(N^2)` — trivial for N <= 6):

1. Collect the distinct positive contribution levels, ascending: `L_1 < ... < L_m`.
2. For each level `L_k`, let `prev = L_{k-1}` (0 for `k = 1`). The band width is
   `w_k = L_k - prev`. Let `S_k` = seats with `c_i >= L_k`.
   - `amount_k = w_k * |S_k|` (every seat still in at this level pays the band).
   - `eligible_k = S_k ∩ contested` (folded seats fund the layer but cannot win).
3. Emit only layers with `amount_k > 0`.

A layer whose `eligible` set is a single seat (everyone else at that depth folded)
is simply **returned** to that seat — a degenerate "uncontested overflow" award,
which is how a deep stack gets change back when opponents were all-in shorter.

#### Payoff kernel — `distribute_pots`

The ranking kernels supply concrete per-seat hand strengths (for exact/sampled
N-way you draw concrete combos and read `cache.hand_rank`); the payoff kernel is
pure chip arithmetic and knows nothing about cards:

```cpp
template <std::size_t N>
struct showdown_ranks {
    std::array<rank_key, N> rank{};   // higher = stronger; only contested seats read
};

template <std::size_t N>
[[nodiscard]] std::array<utility, N> distribute_pots(
    const pot_structure<N>&    pots,
    const showdown_ranks<N>&   ranks) noexcept;
```

Algorithm:

1. `layers = build_side_pots(pots)`.
2. `net_i = -contribution_i` for all seats (everyone starts down their commitment).
3. Rake scale: `f = (gross - rake) / gross` (the same net factor as the heads-up
   `distributed_pot`); apply `f` to each layer amount so the only chip leak is the
   rake. (Alternative conventions — rake only the main pot, capped rake — are a
   config knob; the zero-sum-minus-rake invariant must hold whichever is chosen.)
4. For each layer: among `eligible`, find the max `rank`; the winners (ties share)
   split `f * amount` equally; add to their `net`.
5. Return `net`. Invariant: `sum(net) == -rake` (exact zero-sum when `rake == 0`).

#### Heads-up reduction (correctness anchor)

For `N == 2`, `pot_structure<2>` with `contribution = {oop_c, ip_c}` and both seats
contested reproduces today's model exactly:

- Equal contributions → one layer, winner takes `f * (oop_c + ip_c)` = `distributed_pot`; tie splits it — identical to `payoff_for_oop_win` / `payoff_for_tie`.
- A fold → the folded seat is cleared from `contested`; the single eligible seat
  wins every layer — identical to `payoff_for_fold`.
- Unequal contributions (one all-in short) → the overflow band has a single
  eligible seat and is returned to it — a case the current single-pot heads-up
  helpers do **not** model, and the reason even heads-up eventually wants the
  layered engine once all-in-for-less is supported.

The existing `evaluate_showdown_heads_up` mass-decomposition kernel stays as the
fast path for the common equal-contribution case; `distribute_pots` is only invoked
by the enumeration/sampled kernels (and, later, by a heads-up all-in-for-less path)
that already work with concrete ranks rather than mass streams.

#### Why this stays off the hot path

`pot_structure<N>` and `distribute_pots` are touched **once per evaluated terminal
combo tuple**, not per mass-bucket. Heads-up equal-pot traversal never constructs
them. This preserves the sacred two-stream kernel while giving multiway a correct,
allocation-free, compile-time-sized side-pot engine.

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

Beyond the near-term refactor, the tiered `terminal_engine` grows in this order.
The following reordering (from review feedback) reflects commercial priorities:

**Phase 1 (current): API migration & heads-up isolation** ✅

Steps 1–6 above (complete).

**Phase 2: Multiplayer infrastructure & workspace**

7. **`terminal_workspace<N>`** — the `river_reach_index` is ~129 KB per player; do
   not retain per CFR node or copy around. Wrap reach indices in a reusable scratch
   object (thread-local):
   
   ```cpp
   template <size_t N>
   struct terminal_workspace {
       std::array<river_reach_index, N> reach;
       // future: scratch buffers
   };
   ```
   
   Pass by reference to `evaluate_terminal(workspace&, cache, context)`. This
   validates the architecture's memory model: persistent cache + ephemeral
   workspace.

8. **`evaluate_fold_values<N>` generic kernel** — the fold evaluator is almost
   free to generalize (compatible mass × constant payoff) and validates the N-way
   architecture with minimal risk. Implement generic loop over active players; add
   unit tests. No sampling, no special casing — just the first "non-heads-up"
   kernel.

9. **`pot_structure<N>` & side-pot infrastructure** — separate showdown ranking
   from payoff computation. Implement `pot_structure<N>` with:
   - Rake policy abstraction (Step 16 hook): `rake_policy` struct with `compute_rake()` 
     method. Extensible for rake caps, no-flop rules, time collection, etc.
   - Range data policy (Step 15 hook): `range_data_policy` marker struct for future 
     bucketed/sampled range support without API churn.
   - Memory layout policy (Step 17 hook): `memory_layout_policy` for NUMA affinity, 
     alignment hints, and shared/thread-local lifetime tracking.
   - Side-pot structure: `side_pot<N>` for eligible-seat tracking and amount accumulation.
   - Heads-up specialization: `pot_structure<2>` with direct boolean fields (no bitsets).
   
   **✅ Done** — `pot_structure<N>` implemented with all three policy hooks. No payoff 
   distribution yet (that comes in Phase 3); this validates the structure independently.

**Phase 3: Sampling & generic N-way dispatch**

10. **`terminal_engine<N>` dispatch layer** — route on player count:
    - `N == 2`: heads-up exact (two-stream rank sweep)
    - `N > 2`: generic N-way (sampled evaluator; exact enumeration deferred indefinitely)
    
    The dispatch routes the generic `evaluate_terminal` call to the right kernel.
    No 3-player special case; all N > 2 use sampling.

11. **Sampled N-way kernel** — Monte Carlo evaluation with variance reduction:
    - sample opponent combos according to reach distribution
    - evaluate showdown
    - accumulate EV
    
    Stratified sampling + importance weighting are future refinements.

12. **`range_data` split views** (optional refinement for Step 15) — introduce 
    `sampling_view` (`alias_table` / CDF / strata) as a *separate* structure 
    alongside the existing lean `exact_view` (`river_reach_index`). Do not overload 
    the hot path. Deferred indefinitely (sampling works without it).

**Phase 4 (future): Polish & commercial readiness**

15. **Abstraction support** — bucketed ranges feeding the sampled kernel. 
    **Hooked in pot_structure<N>::range_policy** (Step 9). Bucketing is an 
    abstraction concern, not an evaluator concern. ✅ Ready.

16. **`rake_policy` abstraction** — generalized rake models. 
    **Hooked in pot_structure<N>::rake** (Step 9) with `compute_rake()` method. 
    Supports rake caps, no-flop rules, time collection, etc. ✅ Ready.

17. **Parallelism & memory** — board-first parallelism, NUMA-aware scheduling, 
    compact regret/infoset storage. 
    **Hooked in pot_structure<N>::memory_policy** (Step 9) with alignment, NUMA 
    node, shared/thread-local markers. ✅ Ready.

**Architectural principles (review feedback synthesis):**

This architecture is **commercial-grade**. The critical decisions that *must* remain:

✅ **Compile-time player count** — no dynamic allocation, size known to compiler, no 
   runtime player-count checks. Prefer `std::array<T, N>` over dynamic sizing.

✅ **Heads-up specialization sacred** — the two-stream rank sweep is a fast specialization 
   that must remain isolated and hand-tuned. Do *not* add the HU path as a special case 
   inside a generic N-way loop; the compiler cannot magically optimize it back.

✅ **SoA terminal values** — never nest vectors. Keep `std::array<value_array, N>` flat.
   Even 6 players × 1081 combos ≈ 26 KB, trivial.

✅ **Player-neutral cache & reach indexing** — `river_terminal_cache` and `river_reach_index` 
   know nothing about players. This separation is correct.

✅ **Separate payoff kernel** — ranking (showdown) and payoff (pots, rake, side-pot 
   distribution) are different problems. Do not mix them.

✅ **Sampled N-way strategy** — for N > 2, sampling with variance reduction is the primary 
   route (exact enumeration explodes combinatorially). Heads-up exact 
   (two-stream rank sweep), all other player counts sampled.

✅ **Single-threaded evaluator** — do *not* parallelize rank-bucket sweeps internally 
   (synchronization and cache thrashing). Parallelize the *outer* dimension instead: 
   CFR traversal / board batches call the evaluator with a read-only cache and a 
   thread-local workspace.

- **Reach indices are scratch, not persistent.** `river_terminal_cache` is the
  persistent shared object; a `river_reach_index` is ~129 KB per player and must
  not be retained per CFR node. Wrap them in a thread-local
  `terminal_workspace<N> { std::array<river_reach_index, N> reach; scratch; }` that
  is reused across evaluations (Phase 2, step 7).

- **Do not rename `terminal_context<N>` now.** It is currently `{ pot info }`. Later, 
  when multiway is real, introduce `pot_context<N>` (just this struct) and 
  `terminal_context<N> { pot_context<N> pot; seat_mask<N> active; }` to add the 
  active-set / folded-player information. For now, keep it clean; no churn.

- **Benchmark baseline is excellent.** Dense showdown: 138M compatible_matchups/s. 
  The fold-to-showdown ratio is sensible (fold is just compatible mass × payoff; 
  showdown has bucket traversal + accounting). Preserve this performance in later 
  kernels.
- **One evaluator per street — do not unify.** Keep this a *river-only* evaluator.
  Turn (`C(44,1)` unknown rivers) and flop (`C(45,2)` unknown turn+river) are
  different problems; a single all-streets evaluator destroys the specialization.
  The engine gains sibling `turn_evaluator` / `flop_evaluator`, not a merged one.
- **Sampling is the primary N-way route.** All N > 2 use Monte Carlo; no exact 
  enumeration except the heads-up two-stream rank sweep.

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

## Status

### Completed (Steps 1–8)

✅ **API migration & heads-up preservation complete (Steps 1–6).**

- `terminal_context<N>` templated on player count with `std::array<utility, N> 
  contribution`; heads-up context helpers for compatibility.
- `terminal_values<N>` and `terminal_result<N>` templated as compact SoA 
  (`std::array<value_array, N>`) without nesting.
- Generic `evaluate_showdown<N>` and `evaluate_fold_values<N>` entry points with 
  primary-template `static_assert(N == 2)` guard (non-heads-up calls are compile errors).
- Heads-up algorithm isolated as explicit specialization (`evaluate_showdown<2>`, 
  `evaluate_fold_values_heads_up`); current kernels renamed but algorithm unchanged.
- Existing call sites + 74+ test cases passing; benchmarks confirm no regression.
- Documentation (`terminal_evaluator.md`) updated: objective reworded to "active players"; 
  specialization noted; sections retitled; generic API shape documented; no performance 
  change.

✅ **Generic fold kernel complete (Phase 2, Step 8).**

- Implemented `evaluate_fold_values<N>(cache, reach[], context, folded_mask<N>)` generic entry 
  point replacing heads-up-only `heads_up_player folded` parameter.
- Added `folded_mask<N>` struct template: generic wraps `std::bitset<N>`; specialized `folded_mask<2>` 
  uses direct `bool oop_folded, ip_folded` fields (no bitset overhead in fast path).
- Implemented `evaluate_fold_values_generic<N>()` kernel: for each active player, accumulates 
  compatible mass from all other active opponents.
- Added validation test confirming generic heads-up fold matches specialized fold exactly 
  (bit-identical results).
- All 69 tests passing; no regression.

✅ **Terminal workspace complete (Phase 2, Step 7 reordered).**

- Implemented `terminal_workspace<N>` struct holding `std::array<river_reach_index, N> reach`.
- Added `materialize(const std::array<reach_vector, N>&)` method to build reach indices from ranges.
- New overloads: `evaluate_showdown<N>(workspace&, cache, ranges[], context)` and 
  `evaluate_fold_values<N>(workspace&, cache, ranges[], context, folded_mask<N>)`.
- Architecture: Caller owns ranges, workspace owns reach indices, cache is read-only.
- Data flow: ranges → materialize → workspace.reach → kernel → result.
- Thread-local workspace pattern enables CFR to reuse across nodes without per-node allocation.
- Backward compatibility: Original reach-index APIs still work for existing code.
- All 69 tests passing; no regression.

✅ **Pot structure with policy hooks complete (Phase 2, Step 9).**

- Implemented `pot_structure<N>` with side-pot infrastructure: `side_pot<N>` tracks eligible seats 
  and accumulation per pot.
- Added `rake_policy` abstraction (Step 16 hook): `compute_rake()` method extensible for rake caps, 
  no-flop rules, time collection.
- Added `range_data_policy` abstraction (Step 15 hook): marker struct for future bucketed/sampled 
  ranges without API churn.
- Added `memory_layout_policy` abstraction (Step 17 hook): alignment, NUMA affinity, shared/thread-local 
  markers for parallelism.
- Heads-up specialization `pot_structure<2>`: direct boolean fields `oop_active`, `ip_active` (no bitsets).
- All 69 tests passing; no regression.
- Payoff distribution (`distribute_pots`) deferred to Phase 3; structure itself is production-ready.

**Next immediate step: Phase 3, Step 10 (Implement `terminal_engine<N>` dispatch layer)**

### Deferred (do not implement yet)

❌ **Do not implement generic N-way showdown yet.** The heads-up specialization is 
the only exact showdown kernel; multiplayer will be sampled (not exact).

❌ **Do not add `card_to_combos` to the cache.** It is 10 KB of overhead; keep cache at 22 KB 
until a concrete kernel needs it (Phase 3+).

❌ **Do not rename `terminal_context<N>` to `pot_context<N>` yet.** Rename when the 
active-set / folded-player information is needed in the evaluator API. For now, keep it clean.

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
