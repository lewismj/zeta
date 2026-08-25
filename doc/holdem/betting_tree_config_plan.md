# Betting-Tree Configuration Plan

## 1. Context and goals

The existing `betting_abstraction_policy` in `cfr/betting/betting.h` provides a useful
skeleton, but several surfaces needed for GTO Wizard-style workflows are missing or
incomplete:

- Size selection ignores actor index; there is no per-position size set.
- Raise sizes can only be expressed as pot fractions; raise-multiple sizing
  (e.g., 2.5× the current bet) is not supported, and has undefined semantics when
  `current_bet == 0`.
- All-in threshold and max-raises-per-street are single global scalars; per-street
  overrides are absent.
- Re-raise legality does not enforce the standard poker rule that a re-raise increment
  must be at least as large as the immediately preceding full raise increment; short
  all-ins that fail to meet this requirement must not update the increment tracker.
- `min_raise` and `last_raise_increment` are conflated; the opening-bet minimum and the
  re-raise minimum are distinct quantities and must be handled separately.
- There are no factory functions for canonical named configurations (single-size,
  multi-size, geometric, overbet, all-in-inclusive).
- The betting tree config hash is computed over the lowered graph but is not bound into
  the `cfr_checkpoint_header`, so checkpoints do not record the abstraction that
  produced them.
- The abstraction identity is not wired into `holdem_infoset_key`, leaving the CFR
  state space under-identified.
- There is no serialization format for the configuration itself.

This plan is pre-release.  No backward compatibility with existing
`betting_abstraction_policy` field names or `cfr_checkpoint_header` versions is
required.  Existing callers in test and benchmark files are migrated as part of this
change.

---

## 2. File layout

All new and changed files live under `zeta/holdem/src/cfr/betting/`.

```
cfr/betting/
  betting.h                    (existing - rewritten)
  betting_config.h             (new - named presets)
  betting_config_serializer.h  (new - Boost.JSON round-trip)
  betting_config_serializer.cpp(new)
```

Tests go into `zeta/test/src/test_betting_config.cpp` (new unit test file).
Benchmarks go into `zeta/benchmark/holdem/src/betting_benchmark.cpp` (new).

---

## 3. Design decisions

### 3.1  Actor-indexed size sets

The current `fractions_for_street(holdem_street)` method returns one shared list for
all actors.  Replace it with a general actor-indexed accessor that exposes the full per-
actor size set, not just the pitfall-prone fraction list:

```cpp
[[nodiscard]] const actor_size_set&
sizes_for_actor(holdem_street street, uint8_t actor) const noexcept;
```

The backing data uses a flat actor-indexed map so the design generalises cleanly to
N-player trees without baking heads-up terminology into the representation:

```cpp
struct actor_size_set {
    std::vector<double> fractions{};       // pot-fraction bets/raises
    std::vector<double> raise_multiples{}; // multiples of current_bet, raise-only (see §3.2)
};

// indexed [street_index][actor]: street_index = static_cast<uint8_t>(holdem_street)
std::array<std::vector<actor_size_set>, 5> street_actor_sizes{};
```

`street_actor_sizes[street_index]` is a `vector<actor_size_set>` of length equal to the
player count at the time the policy is applied.  When the vector is empty, or when
`actor >= street_actor_sizes[street_index].size()`, the implementation falls back to
`fixed_pot_fractions`.

For heads-up convenience, the preset factories (§3.5) provide named wrappers:
`oop_sizes` maps to actor 0 and `ip_sizes` maps to actor 1.

### 3.2  Raise-multiple semantics and fraction sizing

A single public function defines the abstraction for all size formulas:

```cpp
[[nodiscard]] utility target_from_fraction(
    const betting_state<N>& state,
    double fraction,
    const betting_abstraction_policy& policy) noexcept;
```

The chosen semantics are:

- `fraction` is applied to the live pot at the betting node.
- `pot` means the live gross pot represented by `betting_state` immediately before the
  action is applied, including all contributions already committed on the street.
- `target_commitment = current_bet + pot * fraction` for bet and raise targets.
- `raise_multiples` are **raise-only** multipliers applied to `current_bet`, producing
  an absolute target commitment:

```text
target_commitment = current_bet * multiple
```

This rule is intentionally different from a "raise by multiple of the amount to call"
interpretation: `raise_multiple` always means **raise-to multiple of the current street
commitment**, never a multiple of the call amount or the last raise increment.

They are **only legal when `current_bet > 0`**.  When `current_bet == 0`, raise-multiple
entries in the active size set are ignored for that node; only pot-fraction bet sizes apply.
This is runtime action-generation behaviour and is not a configuration error.  The policy
remains valid because those multiples are meaningful on streets or nodes where an actual
bet exists.

Raise multiples must satisfy `multiple > 1.0`.  The validator (§3.6) rejects values
≤ 1.0.

### 3.3  Per-street max raises and all-in threshold

Replace the scalar `max_raises_per_street` and `forced_all_in_threshold` fields with
per-street optionals, plus canonical defaults:

```cpp
uint16_t max_raises = 2;                               // opening bet does not count
double   all_in_threshold = 0.95;                      // fraction of final all-in commitment

std::array<std::optional<uint16_t>, 5> max_raises_by_street{};
std::array<std::optional<double>,   5> all_in_threshold_by_street{};
```

Using `std::optional` avoids the sentinel-value ambiguity: `std::nullopt` means
"use the default"; a present value overrides it.  Zero is not a valid value for
either field; the validator rejects it.

Helper accessors:

```cpp
[[nodiscard]] uint16_t max_raises_for_street(holdem_street s) const noexcept;
[[nodiscard]] double   all_in_threshold_for_street(holdem_street s) const noexcept;
```

### 3.4  Minimum raise semantics and `last_raise_increment`

The key semantic contract is that `last_raise_increment` means:

- `last_raise_increment == 0`  => no full opening bet/raise increment has yet established the minimum.
- `last_raise_increment > 0`  => this is the increment required for the next full raise.

This is the minimum increment the next full raise must meet or exceed, and it is not a
counter of raises.  The invariant is:

`min_bet_increment` applies only while `last_raise_increment == 0`; once an opening bet
establishes `last_raise_increment`, subsequent full raises use that state value.  A short
opening all-in can legally leave `last_raise_increment == 0` even though some aggression
occurred, because it did not establish a valid full raise increment.

```cpp
current_bet = highest commitment on the street
```

A separate internal predicate is also used so the legality logic and transition logic share
one definition of a reopenable raise:

```cpp
[[nodiscard]] bool can_raise(
    const betting_state<N>& state,
    uint8_t actor,
    const betting_abstraction_policy& policy) noexcept;
```

This is intentionally based on the betting-state reopen semantics, not just on the presence
of a configured size.  A player who is not reopened by a short all-in must not be treated
as if they were legally eligible to raise just because a configured size exists.

The implementation must distinguish the table-level change from the actor-level
contribution:

```cpp
const utility raise_increment = new_current_bet - state.current_bet;
const utility player_increment = new_commitment - state.committed[actor];
```

The authoritative full-raise predicate is therefore:

```cpp
[[nodiscard]] utility required_raise_increment(
    const betting_state<N>& state,
    const betting_abstraction_policy& policy) noexcept;

[[nodiscard]] bool is_full_raise(
    const betting_state<N>& state,
    utility new_current_bet,
    const betting_abstraction_policy& policy) noexcept;
```

```cpp
const utility required_increment =
    state.last_raise_increment > 0.0
        ? state.last_raise_increment
        : policy.min_bet_increment;

const utility increment = new_current_bet - state.current_bet;
const bool full_raise = increment >= required_increment;
```

Now the transition semantics are explicit:

```text
Initial state
    current_bet = 0
    last_raise_increment = 0
    raise_count = 0

Opening bet of X
    current_bet = X
    last_raise_increment = X
    raise_count = 0
    reopens betting = Yes

Full raise by X
    current_bet += X
    last_raise_increment = X
    raise_count += 1
    reopens betting = Yes

Short all-in by X
    current_bet += X
    last_raise_increment unchanged
    raise_count unchanged
    reopens betting = No

Call / check
    unchanged
```

This makes the opening-bet semantics explicit and keeps the helper semantics easy to test:
after `check → bet 50`, a subsequent raise to `100` is a valid `50`-chip raise because
`last_raise_increment == 50` and `raise_count == 0`.

Street transitions reset the betting-state bookkeeping explicitly:

```text
on street transition to next street:
    current_bet = 0;
    last_raise_increment = 0;
    raise_count = 0;
    acted_since_aggression = ... ;
```

The state transition uses the same full-raise predicate so action legality and state
application are consistent:

```cpp
const utility increment = next.current_bet - state.current_bet;
const utility required_increment =
    state.last_raise_increment > 0.0
        ? state.last_raise_increment
        : policy.min_bet_increment;

if (increment >= required_increment) {
    next.last_raise_increment = increment;
    ++next.raise_count;
    reset_acted_since_aggression(next);
} else {
    // short all-in: do not update last_raise_increment, do not increment raise_count,
    // and do not reopen betting.
}
```

### 3.5  All-in action precedence algorithm

There are two distinct mechanisms at work:

A. Size abstraction: generate a configured size target from pot fractions or
   raise multiples.
B. Explicit all-in generation: if the actor still has chips and an all-in action is
   legal, emit `ALL_IN` independently of the configured size targets.

The action-generation pipeline is therefore:

```text
generate configured size targets
    ↓
convert to absolute target_commitment values
    ↓
all_in_snap_target = all_in_threshold_for_street(street) * all_in_commitment
    ↓
for each target_commitment:
    if target_commitment >= all_in_snap_target:
        snap to ALL_IN
    else:
        keep size action
    ↓
add explicit ALL_IN if legal and not already present
    ↓
legality / min-raise test
    ↓
deduplicate by (action_kind, target_commitment)
```

The API definition is explicit:

`all_in_threshold` is the fraction of the actor's final all-in commitment at which a
produced absolute target commitment is snapped to `ALL_IN`.

This is intentionally distinct from a rule expressed as `remaining_stack <= threshold ×
stack`.  The implementation variable names should reflect that distinction:

```cpp
const utility all_in_commitment = committed + remaining_stack;
const utility all_in_snap_target = all_in_threshold * all_in_commitment;
```

For example, if the actor has `committed = 50`, `remaining stack = 150`, and the final
all-in commitment is `200`, then a threshold of `0.95` snaps any candidate target at or
above `190` to `ALL_IN`.

A BET is emitted when `current_bet == 0`; a RAISE is emitted otherwise.  This is the
existing behaviour, made explicit.

If the actor has chips remaining and an all-in action is legal, `ALL_IN` is emitted
unless an equivalent all-in action is already present.  A short all-in may be legal as a
betting action, but it does not reopen betting and it is not treated as a full raise for
`raise_count` or `last_raise_increment` purposes.

The deduplication invariant is explicit: there is at most one action per `(action_kind,
target_commitment)` and at most one `ALL_IN` action.  This covers the case where a snapped
non-all-in target, a raise target, and an explicit all-in all collapse to the same final
absolute commitment.

### 3.6  `raise_count` definition

`raise_count` in `betting_state<N>` counts **full raises made after the opening bet on
this street**.

- Opening bet: does not increment.
- Full raise: increments.
- Short all-in: does not increment.

Examples:

```text
check → bet                         raise_count = 0
check → bet → raise                raise_count = 1
check → bet → raise → raise        raise_count = 2
```

`max_raises_for_street` is compared against `raise_count` to decide whether further
raises are legal.

### 3.7  Named preset factory functions

Declared in `betting_config.h`:

```cpp
// Single pot fraction applied to every actor on every street.
std::expected<betting_abstraction_policy, betting_policy_error>
make_single_size_policy(
    double fraction = 0.75,
    uint16_t max_raises = 2);

// Fixed list of pot fractions; same for all actors.
std::expected<betting_abstraction_policy, betting_policy_error>
make_multi_size_policy(
    std::vector<double> fractions = {0.33, 0.67, 1.0},
    uint16_t max_raises = 2);

// Geometric sequence of n pot fractions from min_fraction to 1.0 (pot bet).
// size_i = min_fraction * (1.0 / min_fraction)^(i / (size_count - 1))
// for i in 0..size_count-1
// size_count == 0 -> invalid policy error
// size_count == 1 -> {min_fraction}
// size_count >= 2 -> geometric sequence ending at 1.0
// Produces exactly size_count pot-fraction values; all-in is added separately
// by the threshold mechanism.
std::expected<betting_abstraction_policy, betting_policy_error>
make_geometric_policy(
    double min_fraction = 0.33,
    uint16_t size_count = 3,
    uint16_t max_raises = 2);

// Standard sizes plus explicit overbet fractions (> 1.0 pot).
std::expected<betting_abstraction_policy, betting_policy_error>
make_overbet_policy(
    std::vector<double> base_fractions = {0.5, 1.0},
    std::vector<double> overbet_fractions = {1.5, 2.0},
    uint16_t max_raises = 2);

// All other presets use the threshold mechanism for all-in.
// This preset sets all_in_threshold = 1.0 so threshold snapping is disabled for
// non-all-in size targets; an explicit ALL_IN action is still emitted independently.
std::expected<betting_abstraction_policy, betting_policy_error>
make_all_in_inclusive_policy(
    std::vector<double> fractions = {0.5, 1.0},
    uint16_t max_raises = 2);

// Per-actor, per-street size sets. actor_sizes[street_index] is indexed by actor.
// Street indices outside {flop=2, turn=3, river=4} are ignored.
std::expected<betting_abstraction_policy, betting_policy_error>
make_actor_policy(
    std::array<std::vector<actor_size_set>, 5> actor_sizes,
    uint16_t max_raises = 2);
```

These factories return `std::expected<...>` rather than asserting on invalid inputs, so
bad runtime configuration is surfaced to the caller rather than terminating the
process.

### 3.8  Abstraction policy validation

Replaces ad-hoc checks currently scattered in `lower_betting_tree_to_graph`:

```cpp
enum class betting_policy_error_kind : uint8_t {
    negative_fraction,
    zero_max_raises,
    invalid_all_in_threshold,    // must be in (0, 1]
    invalid_min_bet_increment,   // must be > 0
    raise_multiple_too_small,    // must be > 1.0
    invalid_geometric_size_count // must be > 0
};

struct betting_policy_error {
    betting_policy_error_kind kind{};
    uint8_t street = 0;
    uint8_t actor  = 0;
};

[[nodiscard]] std::expected<void, betting_policy_error>
validate_betting_abstraction_policy(const betting_abstraction_policy& policy) noexcept;
```

`min_bet_increment` is an abstraction parameter for the state machine rather than an
implicit table rule.  In a solver configuration it is the minimum opening-bet increment for
this abstraction; it does not automatically mean "table rule" in a live poker context
unless the caller chooses to encode that convention explicitly in the policy.

### 3.9  Deterministic config hash

Keep the two hash concepts clearly separate:

- **`deterministic_hash`** on `holdem_betting_graph<N>` — content hash of the lowered
  graph topology and terminal states (existing).
- **`config_hash`** on `holdem_betting_graph<N>` — hash of the configuration inputs
  that produced the graph (new).

```cpp
[[nodiscard]] uint64_t hash_betting_abstraction_policy(
    const betting_abstraction_policy& policy) noexcept;

template <std::size_t N>
[[nodiscard]] uint64_t hash_betting_graph_config(
    const holdem_betting_graph_config<N>& config) noexcept;
```

`hash_betting_graph_config` covers: player count, street, initial stacks and
committed, root actor, and `hash_betting_abstraction_policy(config.abstraction)`.

The policy hash must include all semantically relevant policy state, including:

- `min_bet_increment`
- `max_raises`
- `all_in_threshold`
- `max_raises_by_street`
- `all_in_threshold_by_street`
- `street_actor_sizes`
- `fixed_pot_fractions`
- `stack_ratio_buckets`

For optional values, hash the presence bit and the value bits separately so that
`nullopt != optional(0.0)`.  For vectors and arrays, hash length first and then
elements in deterministic index order.  This avoids structurally different
configurations colliding at the hash/serialization semantic level.

Hash-collision tests should be semantic, not absolutist: the contract is that changing a
semantically relevant field changes the hash with overwhelming practical likelihood, not
that every unequal pair of policies must have a different 64-bit value by construction.

Floating-point values are hashed as their canonical IEEE-754 bit representation via
`std::bit_cast<uint64_t>`.  The serializer round-trip invariant (§3.10) ensures that
`hash(policy) == hash(deserialize(serialize(policy)))`.

`holdem_betting_graph<N>` gains the field:

```cpp
uint64_t config_hash = 0;
```

`lower_betting_tree_to_graph` owns both ID generation and reduction: it computes
`abstraction_id = hash_betting_abstraction_policy(config.abstraction)` and
`config_hash = hash_betting_graph_config(config)` itself.

### 3.10  Infoset identity wiring

Changing the `betting_abstraction_policy` changes the action space and therefore the
infoset assignment.  The existing `betting_history_abstraction_id` field in
`holdem_infoset_key` must incorporate the abstraction hash so that distinct
abstractions produce distinct infoset identities.

The lowering code therefore computes:

```cpp
const uint64_t abstraction_id = hash_betting_abstraction_policy(config.abstraction);
const uint64_t config_hash = hash_betting_graph_config(config);
```

and writes the abstraction hash into each emitted infoset key while storing the config
hash in the resulting graph metadata.

This closes the correctness boundary:

```
policy
  ↓
hash_betting_abstraction_policy(policy)
  ├──→ infoset identity (holdem_infoset_key)
  └──→ config_hash / checkpoint identity (§3.11)
```

### 3.11  Checkpoint binding

Rename `graph_config_metadata_hash` in `cfr_checkpoint_header` to
`betting_tree_config_hash` and populate it from `holdem_betting_graph::config_hash`.
The checkpoint load path validates this field alongside the existing `compatibility`
key; a mismatch is a hard rejection.  Checkpoint `version` bumps from `2` to `3`.
Existing version-2 checkpoints are rejected by the version-3 reader; no migration path
is provided.

### 3.12  Serialization via Boost.JSON

The project already depends on `Boost::json` (used in `solve_cli.cpp`,
`document_json.cpp`, and `solution_store.cpp`).  The serializer uses it directly:

```cpp
[[nodiscard]] boost::json::value
to_json(const betting_abstraction_policy& policy);

[[nodiscard]] std::string
serialize_betting_abstraction_policy(const betting_abstraction_policy& policy);

[[nodiscard]] std::expected<betting_abstraction_policy, std::string>
deserialize_betting_abstraction_policy(std::string_view json);
```

The configuration values are `double` in the policy object, while the runtime betting state
uses `utility`/`float` for arithmetic.  This distinction must remain explicit: JSON
round-trips preserve the policy's double values, not the runtime `utility` arithmetic state
itself.

The JSON schema is:

```json
{
  "schema_version": 1,
  "min_bet_increment": 1.0,
  "max_raises": 2,
  "all_in_threshold": 0.95,
  "max_raises_by_street": { "flop": 2, "turn": 2, "river": 2 },
  "all_in_threshold_by_street": { "flop": 0.95, "turn": 0.95, "river": 0.95 },
  "street_actor_sizes": {
    "flop":  [
      { "fractions": [0.5, 1.0], "raise_multiples": [] },
      { "fractions": [0.5, 1.0], "raise_multiples": [] }
    ],
    "turn":  [ ... ],
    "river": [ ... ]
  },
  "fixed_pot_fractions": [0.5, 1.0],
  "stack_ratio_buckets": []
}
```

Per-street optionals are omitted from the JSON when not set.  Omitted fields
deserialise as `std::nullopt`.

The serializer is defined over the semantic policy, not raw JSON bytes: object key order
is not significant, and the canonical representation is the deserialized `policy` object
itself.  In other words:

```text
{"max_raises":2,"min_bet_increment":1}

and

{"min_bet_increment":1,"max_raises":2}

must deserialize to equivalent policies and therefore to the same hash.
```

**Round-trip invariant**: `hash(policy) == hash(deserialize(serialize(policy)))`.
This is tested explicitly (Suite G).

Floating-point policy values must serialize with sufficient precision to round-trip to the
same IEEE-754 `double` bit pattern.  In C++, this means using `std::setprecision(
std::numeric_limits<double>::max_digits10)` (or equivalent) rather than a weak
"6 significant digits" approximation.

---

## 4. Changes to existing code

### `betting_abstraction_policy` struct

| Change | Notes |
|--------|-------|
| Remove `max_raises_per_street` scalar | Replaced by `max_raises` + `max_raises_by_street` |
| Remove `forced_all_in_threshold` scalar | Replaced by `all_in_threshold` + `all_in_threshold_by_street` |
| Remove `min_raise` | Renamed to `min_bet_increment` |
| Replace `street_pot_fractions[5]` with `street_actor_sizes` | Actor-indexed size sets |
| Add `max_raises_by_street[5]` as `optional` | Per-street override |
| Add `all_in_threshold_by_street[5]` as `optional` | Per-street override |
| Remove `fractions_for_street` | Replaced by `sizes_for_actor(street, actor)` |
| Remove `geometric_size_count` scalar | Geometric sequences are expressed as computed `fractions` in the actor size set |

Existing callers in `test_cfr_graph.cpp` and `cfr_benchmark.cpp` are updated to
the new API as part of this change.

### Internal target representation

Add a private helper used during action generation before conversion to action enum:

```cpp
struct betting_target {
    utility commitment{};
    enum class source { fraction, raise_multiple, all_in };
};
```

This keeps the policy-level size generation, threshold snapping, and explicit all-in logic
separate from final action classification.  The pipeline is:

`policy → generated targets → absolute commitments → snap-to-all-in → deduplicate → legality → actions`.

### `betting_state<N>` struct

| Change | Notes |
|--------|-------|
| Add `last_raise_increment = 0.0` | Full-raise minimum tracker |

### `legal_betting_actions`

- Call `sizes_for_actor(state.street, state.actor)` for size lookup.
- Add raise-multiple targets when `current_bet > 0`.
- Apply `last_raise_increment`-based minimum filter for raise/re-raise targets.
- Use `can_raise(state, actor, policy)` to gate whether a generated raise is legal given
  short-all-in reopening semantics.
- Use `max_raises_for_street()` and `all_in_threshold_for_street()` accessors.
- Follow the all-in precedence algorithm (§3.5).

### `apply_betting_action`

- Update `last_raise_increment` only when the new increment constitutes a full raise
  (§3.4).
- Increment `raise_count` only for full raises and full all-ins (§3.6).
- Do not reopen betting after a short all-in.

### `holdem_betting_graph<N>`

- Add `uint64_t config_hash = 0`.

### `holdem_betting_graph_config<N>`

- No caller-populated abstraction ID is required; `lower_betting_tree_to_graph`
  computes the abstraction hash internally.

### `lower_betting_tree_to_graph`

- Compute `abstraction_id = hash_betting_abstraction_policy(config.abstraction)` and
  `config_hash = hash_betting_graph_config(config)` internally.
- Pass the computed `abstraction_id` into `betting_history_abstraction_id` for each
  infoset key.
- Store `config_hash` on the resulting `holdem_betting_graph`.

### `cfr_checkpoint_header`

- Rename `graph_config_metadata_hash` → `betting_tree_config_hash`.
- Bump `version` from `2` to `3`.

---

## 5. Test plan

All tests use Boost.Test in `zeta/test/src/test_betting_config.cpp`.

### Suite A: `betting_abstraction_policy` construction and validation

| Test | What it checks |
|------|---------------|
| `policy_default_is_valid` | Default-constructed policy passes `validate_betting_abstraction_policy`. |
| `policy_negative_fraction_rejected` | Fraction < 0 returns `negative_fraction` error. |
| `policy_zero_max_raises_rejected` | `max_raises = 0` returns `zero_max_raises`. |
| `policy_invalid_threshold_rejected` | `all_in_threshold = 0.0` returns `invalid_all_in_threshold`. |
| `policy_threshold_above_one_rejected` | `all_in_threshold > 1.0` returns `invalid_all_in_threshold`. |
| `policy_raise_multiple_too_small_rejected` | Raise multiple ≤ 1.0 returns `raise_multiple_too_small`. |
| `policy_zero_min_bet_increment_rejected` | `min_bet_increment = 0` returns `invalid_min_bet_increment`. |
| `per_street_override_returns_override` | `max_raises_for_street(flop)` returns the per-street optional when set. |
| `per_street_fallback_to_default` | `std::nullopt` entry returns `max_raises`. |

### Suite B: `sizes_for_actor` dispatch

| Test | What it checks |
|------|---------------|
| `actor_0_receives_actor_0_fractions` | Actor-0 fractions returned when configured. |
| `actor_1_receives_actor_1_fractions` | Actor-1 fractions returned when configured. |
| `out_of_range_actor_falls_back` | Actor index beyond configured set falls back to `fixed_pot_fractions`. |
| `empty_street_falls_back_to_fixed` | Empty per-street actor list falls back to `fixed_pot_fractions`. |
| `raise_multiples_ignored_when_no_bet` | Raise-multiple entries silently ignored when `current_bet == 0`. |

### Suite C: `legal_betting_actions` — basic action set

| Test | What it checks |
|------|---------------|
| `check_legal_when_no_bet` | Check present when `current_bet == 0`. |
| `fold_call_legal_when_facing_bet` | Fold and call present when facing a non-zero bet. |
| `bet_sizes_match_pot_fractions` | Bet targets are exactly `current_bet + pot * fraction`. |
| `raise_sizes_match_pot_fractions` | Raise targets are exactly `current_bet + pot * fraction` when facing a bet. |
| `raise_multiple_targets_correct` | Raise-multiple targets are `current_bet * multiple`. |
| `raise_multiple_not_emitted_when_no_current_bet` | No raise-multiple actions when `current_bet == 0`. |
| `mixed_fractions_and_multiples_deduped` | Duplicate targets after merging are deduplicated. |
| `all_in_present_when_stack_nonzero` | All-in action always present when actor has stack remaining. |
| `all_in_threshold_snaps_to_all_in` | Target ≥ threshold × effective stack emits `all_in` not `bet`. |
| `raise_count_cap_enforced` | No bet/raise actions when `raise_count >= max_raises_for_street`. |
| `per_street_raise_cap_used` | Per-street cap overrides global `max_raises`. |
| `actor_specific_sizes_used` | Different actors get their respective configured sizes. |
| `per_street_sizes_not_leaking_to_other_streets` | Flop sizes absent on turn; turn sizes absent on river. |

### Suite D: `legal_betting_actions` — minimum raise poker rules

| Test | What it checks |
|------|---------------|
| `opening_bet_minimum_is_min_bet_increment` | First bet must meet `min_bet_increment` threshold. |
| `opening_bet_sets_last_raise_increment_to_bet_size` | After `check → bet 50`, `last_raise_increment == 50` while `raise_count == 0`. |
| `re_raise_min_uses_last_increment` | Re-raise target must be at least `current_bet + last_raise_increment`. |
| `targets_below_re_raise_min_filtered` | Targets below the re-raise minimum are not emitted. |
| `full_raise_updates_last_increment` | After a full raise, `last_raise_increment` is updated. |
| `short_all_in_does_not_update_increment` | All-in below raise minimum leaves `last_raise_increment` unchanged. |
| `raise_after_short_all_in_uses_previous_increment` | Re-raise after a short all-in still uses the prior full raise increment. |
| `short_all_in_is_still_legal` | A short all-in below the re-raise minimum is always a legal action. |
| `short_opening_all_in_does_not_establish_full_raise_increment` | With `min_bet_increment = 50`, `A: check`, `B: all-in to 30`, then `current_bet == 30`, `last_raise_increment == 0`, and `raise_count == 0`. |
| `opening_bet_sets_last_raise_increment` | After `check → bet 50`, `current_bet == 50`, `last_raise_increment == 50`, and `raise_count == 0`. |
| `full_raise_increment_is_table_level_not_player_increment` | If `A` has already contributed `20`, `current_bet == 50`, and `A` raises to `100`, then the raise increment is `100 - 50 = 50`, not `100 - 20 = 80`. |
| `street_transition_resets_raise_state` | On a flop→turn transition, `current_bet`, `last_raise_increment`, and `raise_count` are reset to their initial values for the new street. |
| `multiway_short_all_in_does_not_reopen_betting` | In `A bets 50; B raises to 100; C shorts to 130`, `current_bet == 130`, `last_raise_increment == 50`, `raise_count == 1`, `C` is all-in, and `A/B` are not reopened by `C`'s action. |

### Suite E: `apply_betting_action` state transitions

| Test | What it checks |
|------|---------------|
| `fold_sets_folded_mask` | Fold marks seat as folded. |
| `call_reduces_stack_and_commits` | Committed increases; stack decreases by `to_call`. |
| `partial_call_when_short_stacked` | Short-stack call commits remaining stack only. |
| `bet_sets_current_bet_without_incrementing_raise_count` | `current_bet` updates, `raise_count` unchanged. |
| `raise_sets_current_bet_and_increments_raise_count` | Same for a full raise. |
| `full_all_in_sets_all_in_flag_and_raise_count` | `all_in` mask set; `raise_count` incremented for full all-in. |
| `short_all_in_sets_flag_no_raise_count` | `all_in` mask set; `raise_count` unchanged for short all-in. |
| `last_raise_increment_updated_on_full_raise` | `last_raise_increment` reflects new increment. |
| `last_raise_increment_preserved_on_short_all_in` | `last_raise_increment` unchanged after short all-in. |
| `acted_since_aggression_reset_on_full_raise` | All seats' flag reset after a full raise. |
| `acted_since_aggression_not_reset_on_short_all_in` | Short all-in does not reopen the betting round. |
| `terminal_fold_when_one_active` | `terminal_kind = fold` when one player remains. |
| `terminal_showdown_when_round_complete` | `terminal_kind = showdown` when all players acted. |
| `illegal_action_returns_error` | Applying an action not in legal set returns `illegal_action`. |

### Suite E1: legality metamorphic constraints

| Test | What it checks |
|------|---------------|
| `legal_actions_are_accepted_by_apply` | Every action returned by `legal_betting_actions(state, policy)` is accepted by `apply_betting_action`. |
| `accepted_actions_appear_in_legal_actions` | Every non-terminal action accepted by `apply_betting_action` appears in `legal_betting_actions` for the pre-action state. |
| `policy_hash_changes_when_semantically_relevant_field_changes` | Changing a semantically relevant field changes the policy hash and config hash; the test is targeted instead of asserting impossible 64-bit uniqueness. |
| `checkpoint_from_policy_a_cannot_resume_under_policy_b` | A checkpoint produced under policy A fails under policy B. |

### Suite F: `lower_betting_tree_to_graph` graph correctness

| Test | What it checks |
|------|---------------|
| `hu_check_call_fold_tree_shape` | HU single-street check/call/fold tree has correct node and edge count. |
| `hu_bet_raise_fold_tree_shape` | HU tree with one bet size has correct topology. |
| `hu_multi_size_tree_node_count` | Node count scales correctly with multiple bet sizes. |
| `max_history_cap_enforced` | Tree expansion stops at `max_history` depth. |
| `deterministic_hash_stable` | Two calls with identical config produce identical `deterministic_hash`. |
| `config_hash_differs_for_different_policy` | Different abstraction produces different `config_hash`. |
| `config_hash_populated_in_lowered_graph` | `holdem_betting_graph::config_hash` equals `hash_betting_graph_config(config)`. |
| `abstraction_id_in_infoset_keys` | `betting_history_abstraction_id` matches `hash_betting_abstraction_policy`. |
| `terminal_states_correct_pot_accounting` | Terminal state contributions sum correctly to pot. |
| `graph_validation_passes` | `validate_all` on built graph returns success. |
| `solver_graph_view_validation_passes` | `validate_solver_graph_view` on annotations returns success. |

### Suite G: Named preset factories

| Test | What it checks |
|------|---------------|
| `single_size_policy_one_action_per_street` | Each actor and street yields exactly one bet/raise action. |
| `multi_size_policy_produces_distinct_legal_size_actions` | In a sufficiently deep-stack state with no filters or snaps, the configured size set produces the expected number of distinct legal non-all-in actions. |
| `single_size_policy_produces_distinct_legal_size_action` | In a sufficiently deep-stack state, the single-size policy emits the expected legal non-all-in bet/raise action set. |
| `geometric_policy_sizes_correct` | Sizes follow `min_fraction * (1/min_fraction)^(i/(n-1))` for `i` in 0..n-1. |
| `geometric_policy_size_count_one_returns_single_size` | `size_count == 1` yields `{min_fraction}` and does not divide by zero. |
| `geometric_policy_min_fraction_is_first` | First size equals `min_fraction`. |
| `geometric_policy_last_size_near_one` | Last size equals or is near 1.0 pot. |
| `overbet_policy_includes_overbet_sizes` | At least one size > 1.0 pot present. |
| `all_in_inclusive_threshold_is_one` | `all_in_threshold == 1.0` disables threshold snapping for non-all-in size targets; explicit `ALL_IN` remains independently generated. |
| `actor_policy_actors_get_correct_sizes` | Each actor receives its configured fractions. |
| `preset_policies_pass_validation` | All presets pass `validate_betting_abstraction_policy`. |

### Suite H: Serialization round-trip

| Test | What it checks |
|------|---------------|
| `serialize_default_round_trips` | Default policy survives JSON round-trip. |
| `serialize_multi_size_round_trips` | Multi-size preset survives round-trip. |
| `serialize_actor_policy_round_trips` | Per-actor, per-street sizes survive round-trip. |
| `serialize_per_street_optionals_round_trips` | Set per-street overrides survive; unset omitted from JSON. |
| `deserialize_missing_key_returns_error` | Missing required JSON field returns error string. |
| `deserialize_invalid_fraction_returns_error` | Negative fraction in JSON returns error. |
| `schema_version_present` | Serialized JSON contains `schema_version: 1`. |
| `hash_invariant_after_round_trip` | `hash(policy) == hash(deserialize(serialize(policy)))`. |

### Suite I: Checkpoint compatibility

| Test | What it checks |
|------|---------------|
| `checkpoint_version_is_3` | `cfr_checkpoint_header::version == 3`. |
| `checkpoint_header_has_tree_config_hash` | Field `betting_tree_config_hash` is non-default after save. |
| `checkpoint_load_rejects_mismatched_config_hash` | Load with different abstraction policy returns error. |
| `checkpoint_round_trip_same_policy` | Save and load with same policy succeeds; hash matches. |

### Suite J: Hash stability and determinism

| Test | What it checks |
|------|---------------|
| `policy_hash_equal_for_equal_configs` | Equal policies produce identical hash. |
| `policy_hash_differs_on_fraction_change` | Changing a fraction changes the hash. |
| `policy_hash_differs_on_actor_1_change` | Changing actor-1-only sizes changes the hash. |
| `policy_hash_differs_on_max_raises_change` | Changing `max_raises` changes the hash. |
| `policy_hash_differs_on_threshold_change` | Changing `all_in_threshold` changes the hash. |
| `graph_config_hash_covers_stacks` | Different initial stacks change the config hash. |
| `graph_config_hash_covers_street` | Different starting street changes the config hash. |
| `config_hash_same_policy_different_stack_differs` | The same policy with different initial stack states produces a different config hash. |
| `deterministic_hash_covers_terminal_amounts` | Different committed contributions change `deterministic_hash`. |

---

## 6. Benchmark plan

All benchmarks go in `zeta/benchmark/holdem/src/betting_benchmark.cpp` using Google
Benchmark.

### Group 1: Action enumeration throughput

```
BM_LegalActions_SingleSize         // single fraction, HU river
BM_LegalActions_MultiSize_3        // 3 fractions, HU river
BM_LegalActions_MultiSize_5        // 5 fractions, HU river
BM_LegalActions_WithRaiseMultiples // 2 fractions + 2 raise multiples, facing a bet
BM_LegalActions_Geometric3         // geometric preset with 3 sizes
```

Measure: action-set generations per second across 10 000 random betting states per
benchmark.

### Group 2: Tree lowering throughput

```
BM_LowerTree_SingleSize_River
BM_LowerTree_MultiSize_River
BM_LowerTree_SingleSize_Flop        // deeper tree
BM_LowerTree_MultiSize_Flop
BM_LowerTree_Geometric_River
BM_LowerTree_Overbet_River
```

Measure: trees lowered per second (each iteration lowers a fresh tree from config).

### Group 3: Hash computation cost

```
BM_HashPolicy                       // hash_betting_abstraction_policy
BM_HashGraphConfig                  // hash_betting_graph_config
BM_HashBettingGraph_Small           // hash_betting_graph on ~50-node tree
BM_HashBettingGraph_Large           // hash_betting_graph on ~500-node tree
```

Measure: nanoseconds per call.

### Group 4: Serialization throughput

```
BM_SerializePolicy
BM_DeserializePolicy
BM_SerializeDeserializeRoundTrip
```

Measure: microseconds per call.

### Group 5: CFR iteration with different abstraction sizes

Parameterise the existing `BM_CfrIteration`-style harness from `cfr_benchmark.cpp`:

```
BM_CfrIteration_SingleSize          // baseline
BM_CfrIteration_MultiSize_3
BM_CfrIteration_MultiSize_5
BM_CfrIteration_Geometric3
BM_CfrIteration_Overbet
```

Measure: iterations/second and regret-table size at fixed iteration count.

---

## 7. Implementation order

Keep the dependency chain short and explicit:

```text
State semantics
    ↓
Policy model
    ↓
Action generation
    ↓
Tree lowering
    ↓
Graph/config identity
    ↓
Infoset identity
    ↓
Checkpoint binding
    ↓
Presets / serialization
    ↓
Solver / UI surfacing
    ↓
Benchmarks
```

This is the implementation-order contract; the detailed staged milestones remain in §9.

---

## 8. Open questions and constraints

- **Chip granularity**: `utility` is `float`.  Pot-fraction arithmetic at fractional
  chip values accumulates rounding error.  The plan does not change chip type, but the
  serializer writes floating-point values with sufficient precision to round-trip to the
  same IEEE-754 `double` bit pattern (`max_digits10`-style precision), not a weaker
  6-significant-digit approximation.

- **Multiway position semantics**: `actor == 0` is OOP and `actor == 1` is IP in
  heads-up as a naming convention in presets.  The underlying `street_actor_sizes`
  representation is actor-index-generic.  Multiway callers populate actor slots
  explicitly; no HU-specific interpretation is embedded in the data layout.

- **`raise_count` and betting reopen**: When a short all-in does not reopen betting,
  players who have already acted since the last aggression are not required to act
  again.  The `acted_since_aggression` reset must be conditional on whether the all-in
  constitutes a full raise.  This is a subtle correctness requirement that affects
  multiway more than heads-up.  The tests in Suite D and the short-all-in edge cases
  in Suite C explicitly cover this.

---

## 9. Staged implementation plan

Yes — this work should be implemented in staged milestones, each with a small,
compileable patch and a focused test gate.  That keeps the poker-rule semantics from
being buried under a large refactor, and it makes mistakes easier to isolate.

### Stage 1: Betting-state semantics and full-raise rule

Goal:
- lock the betting-state model before changing action generation.

Implementation:
- add `last_raise_increment` to `betting_state<N>`
- add `required_raise_increment(...)`, `is_full_raise(...)`, and `can_raise(...)` helpers
- update `apply_betting_action` to use the same rule as `legal_betting_actions`
- define `raise_count` semantics explicitly
- ensure `acted_since_aggression` resets only on full raises, not short all-ins
- define the street transition reset contract for `current_bet`, `last_raise_increment`,
  `raise_count`, and `acted_since_aggression`

Acceptance tests:
- full raise updates `last_raise_increment`
- short all-in does not update `last_raise_increment`
- short opening all-in leaves `last_raise_increment == 0`
- raise after short all-in still uses the previous full raise increment
- opening bet does not increment `raise_count`
- full raise increments `raise_count`, short all-in does not
- street transition resets branching state correctly

### Stage 2: Policy configuration model and validation

Goal:
- replace the scalar policy with the canonical actor/street-aware model.

Implementation:
- rewrite `betting_abstraction_policy`
- add `std::optional<uint16_t>` and `std::optional<double>` overrides
- add `sizes_for_actor(street, actor)` and generic `street_actor_sizes`
- rename `min_raise` -> `min_bet_increment`
- add `validate_betting_abstraction_policy` and fail with `std::expected`

Acceptance tests:
- default policy validates
- negative fractions rejected
- zero max raises rejected
- zero min bet increment rejected
- invalid threshold rejected
- per-street override uses override value, otherwise falls back to default

Status: implemented in `zeta/holdem/src/cfr/betting/betting.h` and covered by
`zeta/test/src/test_cfr_graph.cpp` under `cfr_betting_config`.

### Stage 3: Legal action generation and all-in precedence

Goal:
- generate legal actions from policy + state with consistent poker semantics.

Implementation:
- rewrite `legal_betting_actions`
- support pot fractions and raise-multiple targets
- ignore raise multiples when `current_bet == 0`
- enforce minimum raise target for re-raises
- implement all-in threshold snapping and deduplication rules
- enforce `max_raises_for_street` and per-street cap logic

Acceptance tests:
- check/call/fold legality at all relevant states
- bet target generation from pot fractions
- raise-multiple target generation from `current_bet * multiple`
- target below required re-raise increment filtered out
- all-in target snapped correctly
- duplicate targets deduplicated
- per-actor and per-street sizes used correctly

Status: implemented in `zeta/holdem/src/cfr/betting/betting.h` and covered by
`zeta/test/src/test_cfr_graph.cpp` under `cfr_betting_generation`.

### Stage 4: Lowering, hash identity, and checkpoint binding

Goal:
- ensure the graph carries a deterministic config identity and checkpoint integrity.

Implementation:
- add `hash_betting_abstraction_policy(...)`
- add `hash_betting_graph_config(...)`
- add `config_hash` to `holdem_betting_graph`
- compute `abstraction_id` inside `lower_betting_tree_to_graph`
- bind `betting_tree_config_hash` into checkpoint header version 3
- validate mismatch on load

Acceptance tests:
- same config gives same config hash
- different policy gives different config hash
- same abstraction hash in infoset identity for same config
- different abstraction hash in infoset identity for different config
- checkpoint produced with one policy rejects a different policy on load

Status: implemented in `zeta/holdem/src/cfr/betting/betting.h` and verified by the
Stage 4 checks in `zeta/test/src/test_cfr_graph.cpp`.

### Stage 5: Named presets, serialization, and release-quality validation

Goal:
- validate canonical workflows and configuration portability.

Implementation:
- add `make_single_size_policy`, `make_multi_size_policy`, `make_geometric_policy`,
  `make_overbet_policy`, `make_all_in_inclusive_policy`, `make_actor_policy`
- add Boost.JSON serialization and deserialisation
- enforce hash-preserving round-trip invariant
- add benchmark coverage for legal-action generation, lowering, hashing, and iteration

Acceptance tests:
- all preset factories pass validation
- all preset policies survive JSON round-trip
- round-trip preserves hash
- benchmark outputs stable relative ordering

Status: implemented in `zeta/holdem/src/cfr/betting/betting.h` and covered by the
Stage 5 JSON/preset round-trip checks in `zeta/test/src/test_cfr_graph.cpp`.

### Stage 6: Solver and UI surfacing

Goal:
- expose the betting configuration through the solver entry points and the holdem UI.

Implementation:
- add betting-config fields to `solve_spot` / document JSON with legacy defaults for older documents
- thread the selected policy through `solver_session` and the solver invocation path so lowering uses the chosen abstraction
- add UI controls for preset selection and policy editing in the holdem workspace, or a dedicated betting-config editor if that keeps the layout cleaner
- surface validation errors in the existing solver/spot validation UI before a solve starts
- persist the selected configuration through save/load so document round-trips preserve the solver setup

Acceptance tests:
- legacy spot JSON loads with the default betting configuration
- saved documents reload with the same betting-config values
- solver session uses the selected policy and produces the expected config hash
- invalid config is rejected before solve start and surfaced in the UI/solver path

### Stage 7: Release gate and regression sweep

Goal:
- ensure the feature is ready for pre-release solver work.

Implementation:
- run the betting-config unit suite
- run graph validation suite
- run benchmark smoke checks for single/multi-size/geometric/overbet policies
- review checkpoint compatibility assumptions and mismatch behavior
- confirm tree hash + config hash + infoset binding are consistent across a representative solve

Acceptance tests:
- all betting-config targets pass
- representative graphs lower successfully for a few named presets
- config hash mismatch is rejected during checkpoint resume
- no solver-state ambiguities remain across different betting abstractions

This staged sequence keeps each patch reviewable and gives you a clear place to stop if
an earlier rule change proves incorrect.  It also matches the real risk profile of the
feature: poker semantics first, configuration representation second, then graph/hash
identity, then serialization and presets, then solver/UI surfacing.
