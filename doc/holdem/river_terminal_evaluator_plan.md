# River Terminal Evaluator Plan

## Purpose

The river terminal evaluator is the first solver-facing postflop primitive. It
evaluates terminal river outcomes for two heads-up Hold'em ranges on a fixed
five-card board and returns counterfactual value data suitable for the planned
CFR+ river-first implementation.

This layer should be deliberately narrower than a full action-tree solver. It
does not build betting trees, sample future streets, parse solver configs, or
measure exploitability. Its job is to provide a fast, deterministic, and
well-tested payoff engine for:

- showdown terminals
- fold terminals
- per-combo counterfactual values
- aggregate expected value checks
- reusable precomputed river-board state

The implementation should reuse the existing core and Hold'em primitives:

- `zeta::card_mask` as the bit-set representation for cards
- `zeta::ops::popcount`, `suit_ranks`, `lsb`, and related bit operations
- `zeta::holdem::board`
- `zeta::holdem::combination_index`
- `zeta::holdem::combination_count`
- `zeta::holdem::combination_masks`
- `zeta::holdem::combination_mask`
- `zeta::holdem::is_live_combo`
- `zeta::holdem::hand_range`
- `zeta::holdem::evaluate(card_mask)`
- `zeta::holdem::hand_rank`

Zeta currently uses compact integer bitboards rather than `std::bitset`.
Throughout this plan, "bitset" means the existing 64-bit `card_mask` bit-set
model unless explicitly stated otherwise.

## Non-goals

Do not implement these in the river terminal evaluator:

- betting-tree construction
- regret matching
- CFR+ iteration loops
- average-strategy accumulation
- turn/flop chance rollout
- multiway pots
- rake
- all-in side pots
- no-limit bet-size legality
- board generation
- range parsing beyond the existing `parse_range`

Those layers should depend on the terminal evaluator, not be folded into it.

## Source layout

Add a new header-only Hold'em component first:

```text
zeta/holdem/src/terminal.h
```

Then include it in:

```text
zeta/holdem/CMakeLists.txt
```

Register tests in:

```text
zeta/test/src/test_holdem.cpp
zeta/test/CMakeLists.txt
```

A header-only first pass matches the existing `board.h`, `range.h`, and
`evaluator.h` style. If the implementation grows large or needs separately
compiled lookup caches later, split into:

```text
zeta/holdem/src/terminal.h
zeta/holdem/src/terminal.cpp
```

## Terminology

### Player

Use a small enum for solver-facing APIs:

```cpp
enum class player : uint8_t {
    oop,
    ip
};
```

Names should be position-neutral for river subgames:

- `oop`: out of position player
- `ip`: in position player

Avoid `hero` and `villain` in reusable engine types because CFR alternates the
updated player.

### Terminal kind

Use an enum to separate fold and showdown semantics:

```cpp
enum class terminal_kind : uint8_t {
    showdown,
    fold
};
```

For fold terminals, record who folded:

```cpp
struct fold_terminal {
    player folded;
};
```

The player who did not fold receives the terminal transfer according to the pot
model described below.

### Chips and utility units

Do not use `float` as the general solver utility type. CFR+ repeatedly
accumulates values over many iterations, and large regret/value sums experience
cancellation. Use narrow storage where memory matters, but use double-precision
accumulators in terminal arithmetic and CFR accumulation paths.

```cpp
using terminal_value = float;
using accumulator = double;
using utility = double;
```

Use:

- `terminal_value` for stored per-combo terminal output arrays.
- `combo_weight` for range/reach weights, matching `hand_range`.
- `accumulator` for mass sums, bucket totals, EV summaries, and all intermediate
  showdown/fold arithmetic.
- `utility` for scalar pot and payoff inputs.

This keeps the terminal output compact (`2 * 1326 * sizeof(float)`) while
avoiding unnecessary precision loss in the calculations that produce it.

## Pot and payoff model

The terminal evaluator should use normal chip-unit accounting relative to an
explicit solver accounting frame. Do not use a "winner gets half the pot,
loser loses half the pot" transfer convention. That convention is only correct
if all prior contributions have already been removed elsewhere, and it is too
easy to misuse once betting-tree and subgame code are added.

The terminal evaluator therefore needs the gross resolved pot, rake, and the
contributions already invested by each player within the current accounting
frame. The caller chooses the frame:

1. Zero-sum node frame:
   `gross_pot - rake == oop_contribution + ip_contribution`, so every scalar
   terminal payoff satisfies `oop + ip == 0`.
2. Subgame frame: `gross_pot - rake` includes dead money from before the current node, so
   scalar terminal payoff satisfies
   `oop + ip == gross_pot - rake - oop_contribution - ip_contribution`.

Use:

```cpp
struct terminal_pot {
    utility gross_pot = 0.0f;
    utility rake = 0.0f;
    utility oop_contribution = 0.0f;
    utility ip_contribution = 0.0f;
};
```

Interpretation:

- `gross_pot` is the total pot before rake at terminal resolution.
- `rake` is the amount removed from the pot before awarding the winner(s).
- `distributed_pot = gross_pot - rake` is the amount awarded at terminal
  resolution.
- `oop_contribution` is the amount OOP has invested inside this accounting
  frame.
- `ip_contribution` is the amount IP has invested inside this accounting frame.
- If OOP wins: `oop = distributed_pot - oop_contribution`, `ip = -ip_contribution`.
- If IP wins: `oop = -oop_contribution`, `ip = distributed_pot - ip_contribution`.
- If tied: each receives `distributed_pot * 0.5` before subtracting own contribution.
- If IP folds: OOP receives `distributed_pot - oop_contribution`, IP receives
  `-ip_contribution`.
- If OOP folds: IP receives `distributed_pot - ip_contribution`, OOP receives
  `-oop_contribution`.

Example from a river-node frame:

```text
starting pot: 100
IP bets:      100
OOP calls:    100
resolved pot: 300

terminal_pot{
    .gross_pot = 300,
    .rake = 0,
    .oop_contribution = 100,
    .ip_contribution = 100
}

OOP wins: +200 / -100
IP wins:  -100 / +200
```

From a whole-hand frame where both players have invested 150 by showdown, the
same resolved pot produces `+150 / -150`. The evaluator should not infer the
frame; the caller must pass consistent pot and contribution values.

Document this invariant beside every aggregate EV check:

```text
oop_ev + ip_ev == matchup_weight * (gross_pot - rake - oop_contribution - ip_contribution)
```

For zero-sum node-frame inputs, the right-hand side is zero. For subgame-frame
inputs, the right-hand side is the weighted dead money represented by the frame.
This avoids later CFR debugging where regrets appear non-zero-sum simply because
the terminal accounting frame includes pre-existing pot.

Add assertions:

```cpp
assert(pot.gross_pot >= 0.0f);
assert(pot.rake >= 0.0f);
assert(pot.oop_contribution >= 0.0f);
assert(pot.ip_contribution >= 0.0f);
assert(pot.gross_pot >= pot.rake);
assert((pot.gross_pot - pot.rake) >= pot.oop_contribution + pot.ip_contribution);
```

### Terminal context

Wrap terminal accounting inputs in a context object from day one so the evaluator
API does not grow a long parameter list as solver instrumentation expands:

```cpp
struct terminal_context {
    terminal_pot pot{};
};
```

Rake calculation remains a non-goal for the first implementation; callers should
pass `rake = 0.0` unless they have already computed a valid fixed rake amount.
Keeping rake inside `terminal_pot` prevents future ambiguity about whether the
pot field is before-rake or after-rake. The context is still useful now because
future fields such as exploitability instrumentation flags, all-in metadata,
side-pot descriptors, or suit-isomorphic board IDs can be added without changing
every terminal call site.

## Result types

### Scalar terminal payoff

Provide a small scalar payoff type:

```cpp
struct terminal_payoff {
    utility oop = 0.0f;
    utility ip = 0.0f;
};
```

Helper functions:

```cpp
[[nodiscard]] constexpr terminal_payoff payoff_for_oop_win(terminal_pot pot) noexcept;
[[nodiscard]] constexpr terminal_payoff payoff_for_ip_win(terminal_pot pot) noexcept;
[[nodiscard]] constexpr terminal_payoff payoff_for_tie(terminal_pot pot) noexcept;
[[nodiscard]] constexpr terminal_payoff payoff_for_fold(terminal_pot pot, player folded) noexcept;
```

These should be isolated and heavily tested because all terminal evaluators rely
on them.

### Per-combo counterfactual values

CFR needs action values for each private combo at an information set. The river
terminal evaluator should return per-combo values for both players:

```cpp
[[nodiscard]] constexpr std::size_t player_index(player p) noexcept {
    return p == player::oop ? 0u : 1u;
}

struct terminal_values {
    std::array<std::array<terminal_value, combination_count>, 2> values{};

    [[nodiscard]] constexpr const std::array<terminal_value, combination_count>& operator[](player p) const noexcept {
        return values[player_index(p)];
    }

    [[nodiscard]] constexpr std::array<terminal_value, combination_count>& operator[](player p) noexcept {
        return values[player_index(p)];
    }
};
```

Expose player-value accessors rather than encouraging callers to depend on the
internal nested-array layout. The backing storage can remain an array because it
is compact and fast, but public use should go through `operator[]` or an
equivalent `values_for(player)` span-like accessor. This leaves room for future
storage changes such as double-valued debugging output, compressed storage, or
GPU buffers without touching solver code.

The value at `[player][i]` is the counterfactual value for holding combo `i`,
before multiplying by that player's own reach. Opponent range weights are
included. Use player-indexed storage from day one because CFR alternates the
traverser and hot loops naturally write `values[traverser][i]` and
`values[opponent][j]`. Named `oop` / `ip` arrays force branches or duplicated
code once regret updates are wired in.

For example, `values[player::oop][i]` should be:

```text
sum over live opponent combos j:
    ip_reach[j] * payoff(oop combo i versus ip combo j).oop
```

It should not multiply by `oop_reach[i]`. The caller can compute aggregate EV by
dotting these values against `oop_reach`.

### Aggregate summary

Expose a separate summary type for tests, diagnostics, and future UI:

```cpp
struct terminal_summary {
    accumulator oop_ev = 0.0;
    accumulator ip_ev = 0.0;
    accumulator matchup_weight = 0.0;
    accumulator ties = 0.0;
    accumulator oop_wins = 0.0;
    accumulator ip_wins = 0.0;
};
```

`matchup_weight` should be the sum of valid pair weights:

```text
sum_i sum_j oop_reach[i] * ip_reach[j]
```

for non-overlapping, board-live combo pairs only.

`oop_ev` and `ip_ev` should be weighted sums, not normalized means. Add helper
normalization only if needed:

```cpp
[[nodiscard]] terminal_payoff normalized_ev(const terminal_summary& summary) noexcept;
```

This avoids hiding whether the caller is operating in weighted or normalized
range space.

### Combined terminal result

The optimized showdown sweep naturally computes per-combo values and aggregate
diagnostics at the same time. Make the combined result the internal primitive:

```cpp
struct terminal_result {
    terminal_values values{};
    terminal_summary summary{};
};
```

Public convenience wrappers may return only values or only summaries:

```cpp
[[nodiscard]] terminal_values evaluate_showdown_values(...);
[[nodiscard]] terminal_summary summarize_showdown(...);
```

Those wrappers should call `evaluate_showdown(...)` and extract the requested
field. Do not maintain separate traversal implementations for values and
summary; that would double work and invite subtle drift between result paths.

## River board state cache

Repeatedly evaluating `board.mask | combo_mask` inside CFR can be expensive. The
terminal evaluator should introduce a board-specialized cache. Use a
structure-of-arrays layout from day one because the hot rank-sweep path touches
ranks much more often than masks:

```cpp
using combo_bitset = std::array<uint64_t, (combination_count + 63) / 64>;
using rank_key = uint16_t;

struct combo_cards {
    uint8_t first = 0;
    uint8_t second = 0;
};

struct river_terminal_cache {
    uint64_t board_hash = 0;
    board river_board{};
    std::array<card_mask, combination_count> masks{};
    std::array<rank_key, combination_count> rank_keys{};
    std::array<hand_rank, combination_count> unique_ranks{};
    std::uint16_t unique_rank_count = 0;
    std::array<combo_cards, combination_count> cards{};
    combo_bitset live{};
    std::array<combination_index, combination_count> rank_order{};
    std::size_t rank_order_count = 0;
};
```

The combination index is the universal key. Do not store decoded combo objects
or duplicate masks/ranks in every hot structure. The intended access pattern is:

```text
combination_index i
  -> cache.masks[i]
  -> cache.rank_keys[i]
  -> cache.cards[i]
  -> range.weights[i]
  -> regrets[i]
  -> strategy[i]
```

This keeps the river cache compact and cache-friendly:

- `rank_keys`: `1326 * 2`, about 2.6 KiB
- `unique_ranks`: board-local rank-key-to-`hand_rank` table
- `cards`: `1326 * 2`, about 2.6 KiB
- `masks`: `1326 * 8`, about 10 KiB
- `live`: 21 x 64-bit words
- `rank_order`: `1326 * 2`, about 2.6 KiB
- total target: below 32 KiB including alignment

`board_hash` should be a stable identity for the exact river board. Since Zeta's
Hold'em board is already a canonical card mask, `static_cast<uint64_t>(river.mask)`
is sufficient unless a later board type carries extra state. Every
`river_reach_index` built from this cache stores the same hash, and indexed
terminal-evaluation overloads assert that all hashes match. This prevents silent
solver corruption from mixing reach indexes built for a different river board.

Build it once for a fixed river board:

```cpp
[[nodiscard]] river_terminal_cache make_river_terminal_cache(board river) noexcept;
```

Validation:

```cpp
assert(river.board_street() == street::river);
assert(ops::popcount(river.mask) == 5);
```

For each combo index:

```cpp
const card_mask combo = combination_mask(i);
const bool live = (combo & river.mask) == 0;
cache.masks[i] = combo;
cache.cards[i] = extract_combo_cards(combo);
if (live) {
    const auto rank = evaluated_seven_card_rank;
    set_combo_live(cache.live, i);
    cache.rank_order[cache.rank_order_count++] = i;
}
```

Keep both pieces of legality information:

- `cache.live` is the explicit board-legality bitset used by loops that only need
  to filter combos.
- `cache.rank_keys[i] == 0` is reserved for board-blocked combos.
- Live combos receive dense board-local rank keys in showdown order after
  `rank_order` is sorted. Keep the corresponding `hand_rank` in
  `unique_ranks[key]` for debugging and tests.

Do not add a per-combo `bool` field. It does not materially change an AoS
structure size due to padding, and the SoA layout is the more important
architectural choice.

Build the seven-card input through `hand_masks` and the board's suit
decomposition, not by repeatedly calling the generic `evaluate(card_mask)` path:

```cpp
const auto board_masks = suit_rank_masks(river.mask);

const auto combo = combination_mask(i);
if ((combo & river.mask) == 0) {
    const auto combo_masks = suit_rank_masks(combo);
    const hand_masks masks{
        .spades = static_cast<uint16_t>(board_masks.spades | combo_masks.spades),
        .hearts = static_cast<uint16_t>(board_masks.hearts | combo_masks.hearts),
        .diamonds = static_cast<uint16_t>(board_masks.diamonds | combo_masks.diamonds),
        .clubs = static_cast<uint16_t>(board_masks.clubs | combo_masks.clubs)
    };
    const auto rank = evaluate(masks);
}
```

Follow the exact `hand_masks` construction convention used by `evaluator.h`; the
important requirement is to reuse suit-rank bitset operations during cache
construction instead of evaluating opaque 64-bit card masks in the hot rollout
path.

After ranks are evaluated, sort only the live prefix of `rank_order` by
`hand_rank`, then by `combination_index` for deterministic tie order. This sort
is paid once per river board. Then assign dense monotonic `rank_key` values and
populate `unique_ranks`. Do not sort every range/reach index; CFR will build
reach indexes repeatedly, so reach-index construction must be a linear scan over
board-owned rank order.

Precompute `cards[i]` for every combo with bit operations during cache
construction. Terminal evaluation should not repeatedly call `pop_lsb` just to
recover the same two private-card indexes for blocker correction.

Do not cache pairwise matchup results. A full 1326 x 1326 table is large and
mostly invalid because of board blockers and private-card overlap. The
commercial-grade path is a range-specialized index plus rank-sweep evaluation,
not pairwise result caching.

### Cache lifetime and ownership

`river_terminal_cache` is a value object with immutable semantics after
construction:

- one cache represents exactly one river board identity
- copying and sharing a cache is safe
- mutating `river_board`, rank arrays, live bits, or rank order after
  construction is unsafe
- reach indexes must not outlive the board identity they were built from unless
  the matching `board_hash` is preserved and checked

Future CFR code may store these in a board cache such as
`unordered_map<board_id, river_terminal_cache>`. The terminal evaluator should
therefore treat the cache as read-only input and validate `board_hash` on indexed
calls.

## Range-specialized reach index

`hand_range` is a static Hold'em range representation. CFR terminal evaluation
actually consumes reach distributions: static range weights after card removal,
strategy-weighted action reaches, chance-weighted reaches, or opponent reaches
inside a subgame. Keep those concepts separate so the solver does not care where
the weights came from:

```cpp
struct reach_vector {
    std::array<combo_weight, combination_count> weights{};

    [[nodiscard]] combo_weight operator[](combination_index idx) const noexcept;
};
```

Add conversion helpers, not implicit semantic conflation:

```cpp
[[nodiscard]] reach_vector make_reach_vector(const hand_range& range) noexcept;
```

Both efficient fold values and efficient showdown values need opponent reach
mass by card. Build a reach-specialized index from day one instead of adding it
after CFR code has already been written around a weaker cache type.

```cpp
struct river_rank_bucket {
    rank_key rank = 0;
    combo_weight total_mass = 0.0f;
    std::uint16_t begin = 0;
    std::uint16_t end = 0;
    std::uint16_t card_mass_begin = 0;
    std::uint16_t card_mass_end = 0;
};

struct river_bucket_card_mass {
    uint8_t card = 0;
    combo_weight mass = 0.0f;
};

struct river_reach_index {
    uint64_t board_hash = 0;
    std::array<combo_weight, combination_count> weights{};
    std::array<combination_index, combination_count> active_indices{};
    std::array<combo_weight, 52> mass_by_card{};
    combo_weight total_live_mass = 0.0f;
    std::uint16_t active_count = 0;
    std::array<river_rank_bucket, combination_count> rank_buckets{};
    std::uint16_t unique_rank_count = 0;
    std::array<river_bucket_card_mass, combination_count * 2> bucket_card_masses{};
    std::uint16_t bucket_card_mass_count = 0;
};

[[nodiscard]] river_reach_index make_river_reach_index(
    const river_terminal_cache& cache,
    const reach_vector& reach
) noexcept;
```

Index construction:

1. Copy `cache.board_hash` into `index.board_hash`.
2. Iterate `cache.rank_order[0..rank_order_count)`, not raw combo order.
   `rank_order` is already sorted once per river board.
3. For each combo index, read `reach[i]` and skip weights `<= 0.0f`.
4. Store the positive live weight in `weights[i]`.
5. Append `i` to `active_indices`.
6. Add to `total_live_mass`.
7. Populate `mass_by_card` by sweeping set bits in `cache.masks[i]`.
8. Build `rank_buckets` while scanning `active_indices`. Consecutive active
   indices with the same `cache.rank_keys[i]` form one bucket.
9. For each bucket, store:
   - `rank`
   - `begin` / `end` offsets into `active_indices`
   - `total_mass`
   - `card_mass_begin` / `card_mass_end` offsets into sparse
     `bucket_card_masses`

Do not sort in `make_river_reach_index`. Sorting ranges every node, every
iteration, or every traversal would dominate terminal evaluation. The expensive
ordering work belongs to `make_river_terminal_cache`, paid once per river board.

Per-combo card-mass construction:

```cpp
const auto weight = reach[i];
index.weights[i] = weight;
index.active_indices[index.active_count++] = i;
index.total_live_mass += weight;

card_mask m = cache.masks[i];
while (m != 0) {
    const auto bit = ops::pop_lsb(m);
    const auto card = ops::lsb_index(bit);
    index.mass_by_card[card] += weight;
}
```

Bucket construction should use `accumulator` locals for sums, then store compact
`combo_weight` masses in the index. Terminal evaluation casts stored masses back
to `accumulator` before arithmetic. This follows the storage-vs-accumulation
rule: compact hot storage, double-precision calculations.

Do not store `std::array<accumulator, 52>` inside every rank bucket. That would
make the reach index hundreds of KiB:

```text
1326 buckets * 52 doubles ~= 550 KiB
```

Instead, each bucket stores only non-zero blocker masses in a sparse SoA side
array. During bucket construction, use a stack-local `std::array<accumulator, 52>`
for the current bucket, then append the non-zero card entries to
`bucket_card_masses`. The total number of entries is bounded by `2 *
active_count`, because each active combo contributes exactly two private cards to
exactly one rank bucket.

Old pitfalls to avoid:

1. Do not store `{index, mask, rank, weight}` objects in the reach index. Masks
   and ranks are already in `river_terminal_cache`.
2. Skip weights `<= 0.0f`.
3. Do not decode cards repeatedly. Use `cache.masks[i]` and bit operations.
4. Do not allocate. All arrays are fixed-size and live in the index object.

This is the central bitset-heavy data structure for terminal evaluation. It is
small, allocation-free, reusable across fold and showdown terminals at the same
node, and avoids the throwaway O(N^2) fold implementation. It intentionally
stores indices and weights only; masks and ranks are read from
`river_terminal_cache` by index when needed. The key performance rule is: do all
expensive work once per river board; after that, terminal evaluation should be
array scans, integer indexing, and fixed-size bucket lookups.

`rank_buckets` is sized to `combination_count` to stay allocation-free, but only
`rank_buckets[0..unique_rank_count)` is valid. The name
`unique_rank_count` should be used instead of `rank_bucket_count` because the
semantic value is the number of distinct hand ranks present in this reach index.
The heavy per-card bucket data is not stored inline in `rank_buckets`; it lives
in the sparse `bucket_card_masses[0..bucket_card_mass_count)` prefix.

## Live-combo filtering

There are two separate live checks:

1. Combo must not overlap the river board.
2. OOP and IP combos must not overlap each other.

Use the existing bitboard operations:

```cpp
const auto oop_mask = cache.masks[oop_idx];
const auto ip_mask = cache.masks[ip_idx];

if (!combo_live(cache.live, oop_idx)) continue;
if (!combo_live(cache.live, ip_idx)) continue;
if ((oop_mask & ip_mask) != 0) continue;
```

Reach weights are another filter:

```cpp
const auto oop_w = oop_reach[oop_idx];
if (oop_w <= 0.0f) continue;
```

Use `<= 0.0f` rather than `== 0.0f` in terminal loops so invalid negative
weights do not create inverted reach. Parser and range-building code should
prevent negative weights, but terminal code should be robust in release builds.
Tests should still cover zero weights explicitly.

## Showdown algorithm

Main API:

```cpp
[[nodiscard]] terminal_result evaluate_showdown(
    const river_terminal_cache& cache,
    const reach_vector& oop_reach,
    const reach_vector& ip_reach,
    terminal_context context
) noexcept;

[[nodiscard]] terminal_result evaluate_showdown(
    const river_terminal_cache& cache,
    const river_reach_index& oop_index,
    const river_reach_index& ip_index,
    terminal_context context
) noexcept;
```

Convenience APIs:

```cpp
[[nodiscard]] terminal_values evaluate_showdown_values(...);
[[nodiscard]] terminal_summary summarize_showdown(...);
```

The convenience APIs extract from `terminal_result`; they must not run separate
terminal traversals.

### Rank-sweep implementation

Implement showdown with a sorted rank sweep from day one. The public
`evaluate_showdown(cache, oop_reach, ip_reach, context) -> terminal_result`
signature is stable either way, but a direct `O(N^2)` pair loop is throwaway code
for a full CFR+ postflop solver. River terminals are called at very high volume;
the rank-sweep path avoids comparing every compatible pair and uses the
range-specialized bitset index described above.

Provide value/summary convenience wrappers around the two `evaluate_showdown`
overloads:

```cpp
[[nodiscard]] terminal_values evaluate_showdown_values(
    const river_terminal_cache& cache,
    const reach_vector& oop_reach,
    const reach_vector& ip_reach,
    terminal_context context
) noexcept;

[[nodiscard]] terminal_values evaluate_showdown_values(
    const river_terminal_cache& cache,
    const river_reach_index& oop_index,
    const river_reach_index& ip_index,
    terminal_context context
) noexcept;
```

The `reach_vector` overload builds the two reach indexes internally for
convenience. CFR code should build and reuse `river_reach_index` values at each
node and call the indexed overload directly.

Compute each player's values by walking the hero reach index in rank-bucket
order and looking up the opponent's precomputed rank buckets. For a hero combo,
opponent mass is partitioned into lower-rank, equal-rank, and higher-rank
buckets. Each bucket must be blocker-corrected with the hero combo's two private
cards:

```text
compatible(bucket, hero_combo) =
    bucket.total
  - bucket_card_mass(bucket, first_card)
  - bucket_card_mass(bucket, second_card)
  + bucket.weight_of_exact_same_combo
```

The exact-same-combo correction is only non-zero for the equal-rank bucket. A
two-card opponent combo containing both hero private cards is exactly the same
combo index; subtracting both card buckets removes it twice, so add it back once.
This produces exact private-card blocker semantics without enumerating all
opponent pairs.

One-sided pseudo-code (`hero_player` values against `opp_index`) is useful for
explaining the blocker math:

```cpp
void accumulate_showdown_side(
    terminal_values& out,
    const river_terminal_cache& cache,
    player hero_player,
    const river_reach_index& hero_index,
    const river_reach_index& opp_index,
    terminal_payoff hero_win,
    terminal_payoff opp_win,
    terminal_payoff tie
) noexcept {
    assert(cache.board_hash == hero_index.board_hash);
    assert(cache.board_hash == opp_index.board_hash);

    std::size_t opp_bucket = 0;
    accumulator lower_total = 0.0;
    std::array<accumulator, 52> lower_by_card{};

    for each hero_bucket in hero_index.rank_buckets[0..hero_index.unique_rank_count) {
        while (
            opp_bucket < opp_index.unique_rank_count
            && opp_index.rank_buckets[opp_bucket].rank < hero_bucket.rank
        ) {
            lower_total += opp_index.rank_buckets[opp_bucket].total_mass;
            add_sparse_bucket_cards(
                lower_by_card,
                opp_index,
                opp_index.rank_buckets[opp_bucket]
            );
            ++opp_bucket;
        }

        const auto* equal_bucket =
            opp_bucket < opp_index.unique_rank_count
            && opp_index.rank_buckets[opp_bucket].rank == hero_bucket.rank
                ? &opp_index.rank_buckets[opp_bucket]
                : nullptr;
        const auto equal_total = equal_bucket ? equal_bucket->total_mass : 0.0;

        const auto higher_total =
            opp_index.total_live_mass - lower_total - equal_total;

        for offset in [hero_bucket.begin, hero_bucket.end) {
            const auto hero_combo = hero_index.active_indices[offset];
            const auto [c1, c2] = cache.cards[hero_combo];
            const auto equal_c1 = equal_bucket ? bucket_card_mass(opp_index, *equal_bucket, c1) : 0.0;
            const auto equal_c2 = equal_bucket ? bucket_card_mass(opp_index, *equal_bucket, c2) : 0.0;

            const auto lower = compatible_without_exact(
                lower_total, lower_by_card[c1], lower_by_card[c2]
            );

            const auto equal = compatible_with_exact(
                equal_total,
                equal_c1,
                equal_c2,
                opp_index.weights[hero_combo],
            );

            const auto higher_c1 =
                static_cast<accumulator>(opp_index.mass_by_card[c1]) - lower_by_card[c1] - equal_c1;
            const auto higher_c2 =
                static_cast<accumulator>(opp_index.mass_by_card[c2]) - lower_by_card[c2] - equal_c2;
            const auto higher = compatible_without_exact(
                higher_total, higher_c1, higher_c2
            );

            const accumulator value =
                lower  * hero_win_component
              + equal  * tie_component
              + higher * loss_component;

            out[hero_player][hero_combo] = static_cast<terminal_value>(value);
        }
    }
}
```

Because both hero and opponent buckets are rank-sorted, use a merge-style sweep
instead of binary-searching each hero bucket. The important point is that
equal-rank `mass_by_card` is precomputed in the reach index and never rebuilt
inside the per-combo loop.

The production implementation should prefer a fused bidirectional bucket sweep
when it remains clear. A naive implementation can call the one-sided helper once
for OOP and once for IP, but that repeats bucket traversal and lower/equal/higher
mass derivation. A fused implementation walks the union of OOP/IP rank buckets
once and accumulates both players' terminal values:

```text
oop bucket rank r
ip bucket rank r

for OOP combos at rank r:
    lower/equal/higher IP mass -> values[oop][combo]

for IP combos at rank r:
    lower/equal/higher OOP mass -> values[ip][combo]

advance the side(s) with rank r
```

Maintain separate running lower-mass totals for each side:

```cpp
accumulator oop_lower_total = 0.0;
std::array<accumulator, 52> oop_lower_by_card{};

accumulator ip_lower_total = 0.0;
std::array<accumulator, 52> ip_lower_by_card{};
```

At each rank key, the equal bucket for the other player is either the bucket at
that rank or zero. Higher mass is still derived as:

```text
opponent.total_live_mass - opponent_lower_total - opponent_equal_total
```

This preserves the standard counterfactual weighting:

- OOP combo values are weighted by IP reach only.
- IP combo values are weighted by OOP reach only.

The two-pass one-sided helper may remain as a test/debug implementation or a
fallback if it proves equally fast, but the plan should not require duplicated
rank traversal in the production path.

### Aggregate EV from values

Aggregate weighted EV can be derived from per-combo values:

```cpp
terminal_payoff aggregate{};

for (combination_index i = 0; i < combination_count; ++i) {
    aggregate.oop += oop_reach[i] * values[player::oop][i];
    aggregate.ip += ip_reach[i] * values[player::ip][i];
}
```

For accounting consistency, `aggregate.oop + aggregate.ip` should be close to
zero when the accounting frame includes the full distributed pot investment
(`gross_pot - rake == oop_contribution + ip_contribution`). If the frame starts
at a later node with dead money already in the pot, the aggregate sum should
equal:

```text
matchup_weight * (gross_pot - rake - oop_contribution - ip_contribution)
```

## Fold algorithm

Fold terminal values do not depend on hand strength or opponent overlap, except
that impossible private-card pairings must still be excluded when computing
counterfactual values against an opponent range.

API:

```cpp
[[nodiscard]] terminal_values evaluate_fold_values(
    const river_terminal_cache& cache,
    const reach_vector& oop_reach,
    const reach_vector& ip_reach,
    terminal_context context,
    player folded
) noexcept;

[[nodiscard]] terminal_values evaluate_fold_values(
    const river_terminal_cache& cache,
    const river_reach_index& oop_index,
    const river_reach_index& ip_index,
    terminal_context context,
    player folded
) noexcept;
```

Do not implement fold with a direct `O(N^2)` matchup loop. It would be correct,
but it is exactly the kind of implementation the CFR+ solver would delete.
Fold values should use the closed-form compatible-mass formula immediately:

```text
compatible_mass(combo i against opponent) =
    opponent.total_live_mass
  - opponent.mass_by_card[first_card(i)]
  - opponent.mass_by_card[second_card(i)]
  + opponent.weights[i]
```

The correction is exact: a two-card opponent combo containing both private cards
of combo `i` is the same combo index `i`. It appears in both per-card masses and
is therefore subtracted twice; add it back once. If the opponent does not carry
positive weight for that same combo, `opponent.weights[i]` is zero.

Pseudo-code:

```cpp
terminal_values values{};
const auto payoff = payoff_for_fold(context.pot, folded);

assert(cache.board_hash == oop_index.board_hash);
assert(cache.board_hash == ip_index.board_hash);

for offset in [0, oop_index.active_count) {
    const auto oi = oop_index.active_indices[offset];
    const auto compatible_ip = compatible_mass(cache, ip_index, oi);
    values[player::oop][oi] = static_cast<terminal_value>(compatible_ip * payoff.oop);
}

for offset in [0, ip_index.active_count) {
    const auto ii = ip_index.active_indices[offset];
    const auto compatible_oop = compatible_mass(cache, oop_index, ii);
    values[player::ip][ii] = static_cast<terminal_value>(compatible_oop * payoff.ip);
}
```

Even though every valid matchup has the same payoff, card removal still matters.
For example, if OOP holds `AsKs`, IP cannot simultaneously hold `AsQs`; the IP
range mass available against that OOP combo changes by blockers. The closed-form
index preserves that semantics in `O(N)` time.

## Board-specialized rank-sweep helpers

The grouped-rank path is the first implementation, not a future optimization.
Keep the helpers small and independently testable:

```cpp
[[nodiscard]] accumulator compatible_mass(
    const river_terminal_cache& cache,
    const river_reach_index& opponent,
    combination_index hero_combo
) noexcept;

[[nodiscard]] accumulator compatible_mass_from_bucket(
    accumulator total,
    accumulator first_card_mass,
    accumulator second_card_mass,
    accumulator exact_same_combo_weight
) noexcept;

[[nodiscard]] accumulator bucket_card_mass(
    const river_reach_index& index,
    const river_rank_bucket& bucket,
    uint8_t card
) noexcept;
```

Combo card extraction should be precomputed in `river_terminal_cache::cards`.
`compatible_mass_from_bucket` should assert or otherwise guard against tiny
negative values from floating-point roundoff before returning zero for values
close to zero; it must not hide materially negative values caused by bad
range/index construction.

The showdown sweep should use `river_reach_index::rank_buckets` rather than
rebuilding equal-rank card masses on the stack. Bucket construction is paid once
when the reach index is built; terminal evaluation then performs fixed-size
bucket lookups and per-active-combo blocker correction. Per-bucket card mass is
looked up from sparse `bucket_card_masses`, not from a 52-wide array stored in
every bucket.

## Compatible range mass helper

Fold evaluation and showdown blocker correction both depend on compatible range
mass, so add this helper as part of the first implementation:

```cpp
[[nodiscard]] accumulator compatible_reach_mass(
    const river_terminal_cache& cache,
    const river_reach_index& opponent,
    combination_index hero_combo
) noexcept;
```

For combo `i`, return:

```text
opponent.total_live_mass
- opponent.mass_by_card[first_card(i)]
- opponent.mass_by_card[second_card(i)]
+ opponent.weights[i]
```

This helper should not include the acting player's own range weight. It only
answers "how much opponent reach is compatible with this private combo?" The
caller multiplies by the scalar payoff and, for aggregate EV, by the acting
player's own reach.

## API shape

Recommended first public API in `terminal.h`:

```cpp
#pragma once

#include <array>
#include <cassert>
#include <cstdint>

#include "board.h"
#include "evaluator.h"
#include "range.h"

namespace zeta::holdem {

    enum class player : uint8_t {
        oop,
        ip
    };

    using terminal_value = float;
    using accumulator = double;
    using utility = double;

    struct terminal_pot {
        utility gross_pot = 0.0;
        utility rake = 0.0;
        utility oop_contribution = 0.0;
        utility ip_contribution = 0.0;
    };

    struct terminal_context {
        terminal_pot pot{};
    };

    struct terminal_payoff {
        utility oop = 0.0;
        utility ip = 0.0;
    };

    [[nodiscard]] constexpr std::size_t player_index(player p) noexcept;

    struct terminal_values {
        std::array<std::array<terminal_value, combination_count>, 2> values{};

        [[nodiscard]] constexpr const std::array<terminal_value, combination_count>& operator[](player p) const noexcept;
        [[nodiscard]] constexpr std::array<terminal_value, combination_count>& operator[](player p) noexcept;
    };

    struct terminal_summary {
        accumulator oop_ev = 0.0;
        accumulator ip_ev = 0.0;
        accumulator matchup_weight = 0.0;
        accumulator ties = 0.0;
        accumulator oop_wins = 0.0;
        accumulator ip_wins = 0.0;
    };

    struct terminal_result {
        terminal_values values{};
        terminal_summary summary{};
    };

    struct reach_vector {
        std::array<combo_weight, combination_count> weights{};

        [[nodiscard]] constexpr combo_weight operator[](combination_index idx) const noexcept;
        [[nodiscard]] constexpr combo_weight& operator[](combination_index idx) noexcept;
    };

    [[nodiscard]] reach_vector make_reach_vector(const hand_range& range) noexcept;

    using combo_bitset = std::array<uint64_t, (combination_count + 63) / 64>;
    using rank_key = uint16_t;

    struct combo_cards {
        uint8_t first = 0;
        uint8_t second = 0;
    };

    [[nodiscard]] constexpr bool combo_live(const combo_bitset& bits, combination_index idx) noexcept;
    constexpr void set_combo_live(combo_bitset& bits, combination_index idx) noexcept;

    struct river_terminal_cache {
        uint64_t board_hash = 0;
        board river_board{};
        std::array<card_mask, combination_count> masks{};
        std::array<rank_key, combination_count> rank_keys{};
        std::array<hand_rank, combination_count> unique_ranks{};
        std::uint16_t unique_rank_count = 0;
        std::array<combo_cards, combination_count> cards{};
        combo_bitset live{};
        std::array<combination_index, combination_count> rank_order{};
        std::size_t rank_order_count = 0;
    };

    struct river_rank_bucket {
        rank_key rank = 0;
        combo_weight total_mass = 0.0f;
        std::uint16_t begin = 0;
        std::uint16_t end = 0;
        std::uint16_t card_mass_begin = 0;
        std::uint16_t card_mass_end = 0;
    };

    struct river_bucket_card_mass {
        uint8_t card = 0;
        combo_weight mass = 0.0f;
    };

    struct river_reach_index {
        uint64_t board_hash = 0;
        std::array<combo_weight, combination_count> weights{};
        std::array<combination_index, combination_count> active_indices{};
        std::array<combo_weight, 52> mass_by_card{};
        combo_weight total_live_mass = 0.0f;
        std::uint16_t active_count = 0;
        std::array<river_rank_bucket, combination_count> rank_buckets{};
        std::uint16_t unique_rank_count = 0;
        std::array<river_bucket_card_mass, combination_count * 2> bucket_card_masses{};
        std::uint16_t bucket_card_mass_count = 0;
    };

    [[nodiscard]] constexpr terminal_payoff payoff_for_oop_win(terminal_pot pot) noexcept;
    [[nodiscard]] constexpr terminal_payoff payoff_for_ip_win(terminal_pot pot) noexcept;
    [[nodiscard]] constexpr terminal_payoff payoff_for_tie(terminal_pot pot) noexcept;
    [[nodiscard]] constexpr terminal_payoff payoff_for_fold(terminal_pot pot, player folded) noexcept;

    [[nodiscard]] river_terminal_cache make_river_terminal_cache(board river) noexcept;

    [[nodiscard]] river_reach_index make_river_reach_index(
        const river_terminal_cache& cache,
        const reach_vector& reach
    ) noexcept;

    [[nodiscard]] terminal_result evaluate_showdown(
        const river_terminal_cache& cache,
        const reach_vector& oop_reach,
        const reach_vector& ip_reach,
        terminal_context context
    ) noexcept;

    [[nodiscard]] terminal_result evaluate_showdown(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        terminal_context context
    ) noexcept;

    [[nodiscard]] terminal_values evaluate_showdown_values(
        const river_terminal_cache& cache,
        const reach_vector& oop_reach,
        const reach_vector& ip_reach,
        terminal_context context
    ) noexcept;

    [[nodiscard]] terminal_values evaluate_showdown_values(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        terminal_context context
    ) noexcept;

    [[nodiscard]] terminal_values evaluate_fold_values(
        const river_terminal_cache& cache,
        const reach_vector& oop_reach,
        const reach_vector& ip_reach,
        terminal_context context,
        player folded
    ) noexcept;

    [[nodiscard]] terminal_values evaluate_fold_values(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        terminal_context context,
        player folded
    ) noexcept;

    [[nodiscard]] terminal_summary summarize_showdown(
        const river_terminal_cache& cache,
        const reach_vector& oop_reach,
        const reach_vector& ip_reach,
        terminal_context context
    ) noexcept;

    [[nodiscard]] terminal_summary summarize_showdown(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        terminal_context context
    ) noexcept;

}
```

Avoid exceptions, heap allocation, polymorphism, and virtual dispatch. Existing
Hold'em components are direct, value-based, and mostly `noexcept`; this should
follow the same pattern.

## Error handling and validation

Use `assert` for programmer errors, matching existing code:

- river board must have exactly five cards
- board street must be `street::river`
- gross pot, rake, and contributions must be non-negative
- gross pot must be at least rake
- distributed pot (`gross_pot - rake`) must be at least the sum of both
  contributions in the selected accounting frame
- reach indexes passed to indexed overloads must have `board_hash` equal to
  `cache.board_hash`

Do not silently return zero values for invalid boards. A zero-shaped successful
result would make solver bugs hard to diagnose.

Range weights should be treated defensively in loops:

- `<= 0.0f`: skip
- positive: include as reach mass

Do not normalize ranges inside terminal evaluation. CFR reach probabilities are
not necessarily normalized probability distributions.

## Tests

Add focused tests to `test_holdem.cpp` and register them in
`zeta/test/CMakeLists.txt`.

### Payoff unit tests

Use a pot where net values are obvious:

```cpp
terminal_pot pot{.gross_pot = 100.0, .rake = 0.0, .oop_contribution = 50.0, .ip_contribution = 50.0};
```

Expected:

- OOP win: `+50`, `-50`
- IP win: `-50`, `+50`
- tie: `0`, `0`
- IP fold: OOP `+50`, IP `-50`
- OOP fold: OOP `-50`, IP `+50`

Also test a river-node frame where there is dead money in the pot before the
terminal betting action:

```cpp
terminal_pot pot{.gross_pot = 300.0, .rake = 0.0, .oop_contribution = 100.0, .ip_contribution = 100.0};
```

Expected:

- OOP win: `+200`, `-100`
- IP win: `-100`, `+200`
- tie: `+50`, `+50`

Also test explicit rake handling:

```cpp
terminal_pot pot{.gross_pot = 300.0, .rake = 15.0, .oop_contribution = 100.0, .ip_contribution = 100.0};
```

Expected:

- OOP win: `+185`, `-100`
- IP win: `-100`, `+185`
- tie: `+42.5`, `+42.5`

### River cache tests

Create a deterministic five-card board and verify:

- board-live combo count is `C(47, 2) = 1081`
- any combo containing a board card has `combo_live(cache.live, i) == false`
  and sentinel `rank_key{0}`
- any combo disjoint from the board has `combo_live(cache.live, i) == true`
  and non-zero rank key
- `unique_ranks[rank_keys[i]]` for a live combo equals
  `evaluate(board.mask | combination_mask(i))`
- `board_hash` equals the canonical river-board identity
- `rank_order[0..rank_order_count)` contains exactly the board-live combos
- `rank_order` is sorted by `cache.rank_keys[index]`, then by index
- for every adjacent pair in `rank_order`:
  - `unique_ranks[rank_keys[a]] <= unique_ranks[rank_keys[b]]`
  - if the ranks are equal, `a < b`
- `cards[i]` contains the two card indexes from `masks[i]` for every combo

### Reach index tests

Create dense and sparse reaches and verify:

- zero and negative weights are excluded
- board-blocked combos are excluded
- `total_live_mass` equals the sum of positive board-live weights
- `mass_by_card[c]` equals the sum of positive board-live combo weights
  containing card `c`
- `board_hash` matches the source `river_terminal_cache`
- `active_indices` follows `cache.rank_order` and is not independently sorted
- `rank_buckets` cover exactly `active_indices[0..active_count)`
- every `rank_bucket` has the expected total mass
- every `rank_bucket`'s sparse `bucket_card_masses` slice has the expected
  non-zero per-card blocker masses
- `bucket_card_mass_count <= active_count * 2`
- `unique_rank_count` is the number of populated rank buckets
- `compatible_reach_mass` matches a slow reference loop for every live combo on
  at least one board

### Showdown single-combo tests

Create reaches with exactly one combo each:

- OOP has a winning combo
- IP has a winning combo
- both tie using board-only best hand or equivalent same-rank holdings

Expected per-combo values:

- the selected OOP combo receives the scalar OOP payoff
- the selected IP combo receives the scalar IP payoff
- all other combo values remain zero

### Blocker tests

Use reaches where one side has multiple combos and one opponent combo overlaps a
private card. Verify overlapping matchups are excluded.

Example:

- river board does not contain ace or king spades
- OOP reach contains `AsKs`
- IP reach contains `AsQs` and `2h3d`

Only `2h3d` should contribute to `values[player::oop][AsKs]`; `AsQs` is
impossible.

### Board blocker tests

Use a range combo containing a river board card. Verify it contributes nothing,
even if its range weight is positive.

### Aggregate consistency tests

For small synthetic reaches:

1. Compute `terminal_values`.
2. Dot OOP values by OOP reach.
3. Dot IP values by IP reach.
4. Compare against `terminal_summary`.
5. Check the accounting identity:
   `oop_ev + ip_ev ~= matchup_weight * (gross_pot - rake - oop_contribution - ip_contribution)`.

### Fold tests

Use the same single-combo and blocker setups as showdown. Verify:

- folded player always receives their fold payoff
- non-folded player always receives their win payoff
- impossible overlapping combos still do not contribute
- board-blocked combos still do not contribute

### Reference-oracle tests

Add a deliberately slow reference implementation for tests and debugging only.
It must not live in `zeta/holdem/src`, must not be included by production
terminal code, and must not become part of the core Hold'em API. Acceptable
locations:

- local helpers inside `zeta/test/src/test_holdem.cpp`
- a test-only header under `zeta/test/src/`
- a dedicated debug/reference application outside the core libraries

The reference implementation should use the obvious pair loop:

```text
for each OOP combo i
  for each IP combo j
    skip board-blocked and overlapping pairs
    compare evaluate(board | i) and evaluate(board | j)
    accumulate values and summary
```

Use it as the oracle for the optimized rank-sweep implementation:

- 100 random river boards
- 100 random reach-vector pairs
- dense, sparse, zero-weight, and blocker-heavy reaches
- every live combo value compared within tight floating tolerance
- summary fields compared against the same reference result

This catches rank-bucket errors, equal-rank bugs, blocker mistakes, board-play
ties, and accumulation issues that synthetic examples miss.

### Suit-isomorphic and pathological board tests

Add fixed regression boards that stress rank ordering and ties:

- broadway straight flush board: `AsKsQsJsTs`
- quads on board plus kicker: e.g. `AsAhAdAc2s`
- wheel/straight board: `2s3h4d5c6s`
- paired monotone board
- double-paired board
- flush-heavy monotone board
- board-only best hand where most private combos tie

These boards should be tested against the reference oracle.

### Determinism tests

Solver runs need reproducible terminal values. Add tests that build the same
cache and reach indexes twice and require identical:

- `board_hash`
- `rank_order`
- `rank_keys`
- `unique_ranks`
- `active_indices`
- `rank_buckets`
- `terminal_result` values and summaries

Keep deterministic tie-breaking by combination index anywhere ranks compare
equal.

## Benchmarking

Do not add a benchmark before correctness tests pass. Once the API stabilizes,
add a small benchmark under the existing Hold'em benchmark area:

```text
zeta/benchmark/holdem/src/
```

Measure:

- `make_river_terminal_cache`
- `make_river_terminal_cache` rank-order sort cost
- `make_river_reach_index` linear scan over `rank_order`
- rank-sweep showdown values for full reaches on one board
- fold values for full reaches on one board
- sparse reaches with 50, 100, and 300 combos

Record:

- total valid matchup count
- nanoseconds per evaluated hero combo
- effective nanoseconds per compatible matchup compared with a reference loop
- cache construction time
- reach-index construction time
- active combo count and rank bucket count
- confirmation that reach-index construction performs no sort
- evaluator validation count:
  - for each live combo, `unique_ranks[rank_keys[i]] == evaluate(board | combo)`
  - for each adjacent pair in `rank_order`, `rank(a) <= rank(b)`
  - if adjacent ranks are equal, `index(a) < index(b)`
- `sizeof(river_terminal_cache)`
- `sizeof(river_reach_index)`
- `sizeof(terminal_values)`

Initial memory-layout targets:

- `terminal_values`: about 10.6 KiB
- `river_terminal_cache`: below 32 KiB if practical
- `river_reach_index`: below 64 KiB if practical; this depends on keeping rank
  bucket card masses sparse and out-of-line, not storing `52 * double` in every
  bucket

The target is correctness, predictable allocation-free behavior, and a benchmark
that reflects the production algorithm the CFR+ solver will actually use.

## Implementation sequence

1. Add `terminal.h` with types and scalar payoff helpers.
2. Add payoff tests.
3. Add `river_terminal_cache` with board hash, SoA masks/ranks, live bitset, and
   board-owned `rank_order`.
4. Add cache tests for live combo count, rank correctness, board hash, and
   rank-order sorting.
5. Add `river_reach_index` with board hash, weights, active indices, card-mass
   construction, rank buckets, prefix masses, and compatible-mass helpers.
6. Add reach-index, rank-bucket, board-hash, no-sort, and compatible-mass tests
   against slow reference loops.
7. Add closed-form `evaluate_fold_values`.
8. Add fold tests.
9. Add rank-sweep `evaluate_showdown_values`.
10. Add test/debug-only reference oracle outside `zeta/holdem/src`.
11. Add single-combo, tie, blocker, board-blocker, random-reference,
    suit-isomorphic, pathological-board, and determinism showdown tests.
12. Add `terminal_result` summary accumulation.
13. Add aggregate consistency tests.
14. Run the Hold'em test target.
15. Add benchmark coverage for cache construction, reach-index construction,
    fold values, rank-sweep showdown values, dense reaches, and sparse reaches.
16. Add memory-layout benchmarks for the hot value/cache/index objects.

## Future extensions

### CFR+ integration

The CFR+ river layer should consume `terminal_values` directly:

- terminal action value for each combo
- no own-reach multiplication inside terminal evaluator
- opponent reach included through the opponent range
- action utility arrays can be accumulated into regret arrays

### Turn and flop integration

Turn and flop chance rollout should call this evaluator after generating river
boards and applying card removal:

```text
turn state
  -> enumerate river card
  -> make or reuse river_terminal_cache
  -> evaluate terminal values
  -> chance-weight back to turn combo values
```

Flop rollout similarly enumerates turn and river chance cards. Cache reuse will
matter there, but the river terminal API should remain board-specific and
stateless.

### Later optimizations

Do not plan on replacing the first implementation wholesale. The first version
already includes blocker-aware opponent mass and rank-sweep showdown. Future work
should be incremental and benchmark-driven:

- cache and reuse river-terminal caches across turn/flop chance rollout
- specialize sparse-range paths if index construction dominates
- use SIMD or tighter structure packing only if profiling shows the current
  fixed-array layout is the bottleneck
- add optional normalized summaries for UI only, keeping solver-facing values
  weighted
