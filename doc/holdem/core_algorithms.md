# Hold'em Core Algorithms

This document describes the major algorithms used by Hold'em runtime paths: evaluation, terminal value computation, and CFR iteration.

> [!NOTE]
> For the full mathematics and implementation-level walkthrough, see
> [Hold'em Core Algorithms: Maths and Implementation Deep Dive](core_algorithms_deep_dive.md).

## 1. 7-card hand evaluation

The evaluator uses a two-path design:

1. flush/straight-flush path by direct mask lookup
2. non-flush path by rank-multiplicity encoding and dense restricted-quinary index

### Flush path

From a 7-card `card_mask`, the evaluator builds four 13-bit suit-rank masks (`spades`, `hearts`, `diamonds`, `clubs`).

- flush detection: any suit mask with popcount >= 5
- flush index: the suit's 13-bit rank mask
- lookup: `flush_table[flush_index]`

### Non-flush path

The same suit-rank masks are converted to canonical rank multiplicity layers:

- `ones` (count >= 1)
- `twos` (count >= 2)
- `threes` (count >= 3)
- `fours` (count >= 4)

These threshold layers are canonical for non-flush hands because they ignore
suit identity and preserve only rank multiplicities. For a rank with count `3`,
the corresponding bit is set in `ones`, `twos`, and `threes`, but not `fours`.
The exact rank count is reconstructed as:

```cpp
count(rank) =
      bit(ones, rank)
    + bit(twos, rank)
    + bit(threes, rank)
    + bit(fours, rank);
```

The four layers are packed into a canonical key:

```cpp
key = uint64_t(ones)
    | (uint64_t(twos)   << 13)
    | (uint64_t(threes) << 26)
    | (uint64_t(fours)  << 39);
```

The packed key layout is:

```text
bits  0..12  ones
bits 13..25  twos
bits 26..38  threes
bits 39..51  fours
bits 52..63  unused
```

The rank-count vector is naturally quinary because each rank can appear `0..4`
times. For a 7-card hand the 13 digits are restricted by:

```text
0 <= ci <= 4
sum(ci) = 7
```

There are exactly 49,205 valid 13-rank count vectors under those constraints.
The evaluator maps each valid vector to a dense perfect index in `0..49204`,
with no empty buckets, stored keys, collisions, or probe loops. The dense index
is then used directly:

```cpp
rank = non_flush_table[index];
```

Conceptually the index is a combinatorial rank: at each rank, count how many
valid suffixes would have existed for smaller digit choices before consuming the
current rank count. The runtime hot path avoids a 13-rank dependent DP loop by
splitting ranks into `4 + 4 + 5` chunks. Each chunk table entry packs:

```text
bits  0..23  dense-index contribution
bits 24..31  cards used by chunk
```

Within a chunk, small pair-weight tables combine `ones/twos` and
`threes/fours` fields, replacing four separate weight lookups with two pair
lookups plus one addition per chunk.

## 2. Range parsing (PokerStove grammar)

`parse_range(std::string_view)` is single-pass and writes directly into `hand_range::weights`.

Supported forms include:

- classes (`AA`, `AKs`, `AKo`, `AK`)
- exact combos (`AsKh`)
- plus/range expansions (`22+`, `A5s+`, `KTs-KQs`)
- weighted terms (`AA:0.5`)

The parser is exception-free and returns `range_parse_result` with error position on failure.

## 3. River terminal cache materialization

For a fixed river board, `make_river_terminal_cache` precomputes per-combo immutable data:

- board+combo masks
- rank keys / rank ordering
- live-combo bitset
- unpacked card ids for blocker checks

`make_river_reach_index` then materializes a `river_reach_index` for each player's reach at a terminal leaf:

- active combo list
- mass by card
- rank buckets and per-bucket card mass

This shifts expensive preprocessing out of terminal hot loops.

## 4. Showdown evaluation algorithms

### Heads-up exact kernel

`evaluate_showdown_heads_up` is the dedicated fast path:

1. traverse rank buckets in order
2. accumulate win/tie/loss mass with blocker correction
3. convert matchup mass to terminal utilities from `terminal_context`

The kernel is allocation-free in runtime and reuses cached rank structures.

### Multi-player routes

For more than two players, showdown and fold values come from exact kernels
(`evaluate_showdown_values_exact`, `evaluate_fold_values_exact`) that enumerate
every compatible tuple of opponent hands and pay each one from the layered pots in
`terminal_state<N>`. They return counterfactual values on the same scale as the
heads-up kernels (the sum over opponent tuples of their reach product times the
payoff), so heads-up and multiway terminals plug into the same value backup. Cost
grows with the product of the opponents' active range sizes.

## 5. Fold terminal evaluation

`evaluate_fold_values` computes utility transfer when one or more players fold:

- identify eligible winners from `folded_mask` and terminal state
- apply rake-adjusted pot accounting (including layered pots when present)
- distribute terminal values per seat

Heads-up has specialized convenience overloads; generic N-way paths use templated seat arrays.

## 6. Chance enumeration and validation

Chance nodes are backed by `chance_event_table`.

For each chance node:

1. outcomes are aligned to graph child edges (`child_node`, `action_index`)
2. probabilities are validated (finite, non-negative, sum to ~1)
3. outcome cards are validated against board/dead-card constraints

The value backup weights each child by `probability_for_edge(node_id, edge)`,
rescaled by `private_hand_chance_scale` so every outcome a set of hole cards
allows carries its true conditional probability.

## 7. Action-layout construction

Every player node's infoset id indexes a contiguous block of action slots.
`make_action_table_layout(graph)` builds those action offsets so the solver's
combo x action tables share flat storage.

## 8. Regret matching

For a player node, each hand's action probabilities come from its regrets:

```text
p(a) = max(r(a), 0) / sum_b max(r(b), 0), if positive sum > 0
p(a) = 1 / |A| otherwise
```

`normalize_combo_action_table` applies this per hand and per infoset, honouring
the active action set when dynamic pruning is on.

## 9. Combo-vectorised CFR+

Every solve (heads-up or multiway river, heads-up flop and turn) runs the same
CFR+ loop over a public game graph, with a separate strategy for every hand at
every infoset. One iteration updates each player in turn:

1. **Current strategy:** regret matching per hand (section 8).
2. **Reach:** `build_node_reach_vectors` pushes each seat's per-hand reach down
   the graph, multiplying by that seat's action probabilities at its own nodes.
3. **Terminal values:** every terminal leaf is evaluated for the updating player
   against the other seats' reach at that leaf (section 10).
4. **Value backup:** non-terminal nodes are visited in post-order (children
   first). Chance nodes weight children by their conditional probability; the
   updating player's nodes take the strategy-weighted sum over actions; every
   other seat's nodes sum their children, because that seat's strategy is already
   in the reach the leaves used.
5. **Updates:** at the updating player's nodes, for every hand,

```text
regret(a)       = max(0, regret(a) + V(child_a) - V(node))      (CFR+)
strategy_sum(a) += reach(own hand at node) * p(a)
```

Values are counterfactual: they are weighted by the opponents' reach, so EVs in
chips are the counterfactual value divided by the reach mass of opponent hands
compatible with the hand.

## 10. Parallel terminal evaluation

Terminal evaluation dominates the cost of an iteration, and every leaf is
independent: it reads shared, immutable inputs and writes only its own node's
values. Each evaluation pass therefore runs the leaves as one batch on a
`scheduler::worker_pool`, with `worker_threads` threads (the calling thread
included). The pool hands out leaves in chunks from a shared counter, so leaves of
uneven cost balance across threads. The backup and table updates that follow stay
on one thread: they are cheap, and nodes can share an infoset.

Because no two tasks write the same memory and every floating-point operation
happens in the same order, a solve is bit-identical for any worker count.

## 11. Multi-street unified solve, memory budgeting, isomorphism, and dynamic pruning

Flop and turn spots lower to a single exact multi-street public game
(`lower_multi_street_public_game`) that interleaves a per-street betting round
with each chance deal (flop bet round -> turn card -> turn bet round -> river
card -> river bet round -> showdown). A single CFR+ loop
(`solve_multi_street_public_game`) traverses the whole tree: it weights chance
children by outcome probability, dispatches showdown leaves to the per-river
terminal cache keyed by river public-state id, and uses one street-general value
backup so a turn or flop solve exercises the same chance-backup code a river
solve uses. `iterations` therefore drives convergence on every street; there is
no separate runout-average path.

### Pre-build memory budgeting

Before any graph is materialised, `estimate_multi_street_public_game_shape`
returns the exact node / edge / infoset / chance-outcome / terminal counts, and
`estimate_multi_street_cfr_memory` turns that shape into a byte-accurate
`multi_street_memory_estimate` broken down by component (graph, infoset, regret,
strategy_sum, working, reach, value, terminal_cache, chance, worker, extraction)
and records the `dominant` dimension. The worker component is the reach indices
and terminal values each worker thread holds while it evaluates a leaf. The regret / strategy_sum / working tables
are dimensioned `combos x actions` over the full 1326-combo width the solver
actually allocates, so the estimate matches the real allocation. If the estimate
exceeds the resolved memory budget the solve is refused before allocation with an
actionable error that names the dominant dimension.

### Exact suit-isomorphism reduction

When enabled, suit-isomorphic turn/river boards are collapsed under the board's
suit-permutation symmetry group, shrinking the chance layer. The reduction is
lossless only when the ranges are invariant under the collapsed permutations; a
lossless request over asymmetric ranges is refused unless the caller opts into
the lossy approximation.

### Dynamic action pruning (opt-in, approximate)

Dynamic pruning is an explicitly approximate solver policy, default off. When
off, the solver is bit-identical to the exact multi-street solve. When on, each
infoset maintains an active action set:

- An action's score is its reach-weighted positive-regret contribution,
  `score(a) = sum_combo max(0, reach) * max(0, regret(a))`, and its share is
  `score(a) / sum_a score(a)`.
- Actions whose share falls below `prune_threshold` are deactivated: their
  subtree is skipped and their regret is frozen (never reset), subject to always
  keeping at least `minimum_active_actions` active (ranked by score). When the
  total score is non-positive, or the infoset already has at most the minimum,
  all actions stay active.
- Pruning preserves the original action indexing so persisted strategies stay
  aligned, and it changes only which children are traversed - regret and
  average-strategy accumulation for active actions are unchanged.
- Every `reconsider_interval` iterations the active set is reconsidered from the
  parent/infoset reach and the retained regret (including the frozen action's
  own regret), so a pruned action reactivates once its share recovers without
  ever traversing the inactive child to detect it.

The contract is bounded-to-tolerance: a pruned solve stays within a configured
strategy-distance / EV tolerance of the unpruned solve, not identical to it.

## 12. Convergence and exploitability reporting

Every solve emits user-facing confidence signals attached to the artifact's
`solver` metadata: reproducible input hashes, an optional convergence report, and
approximation warnings.

### Reproducible input hashes

Six stable FNV-1a digests identify the exact inputs a solve consumed, so two
solves can be compared for equivalence without replaying them:

- `tree` -- lowered game-graph topology and metadata (via the solver graph view).
- `range` -- every seat's per-combo reach weights.
- `board` -- the public board card mask.
- `betting_policy` -- the betting abstraction plus stack / pot / contribution
  configuration.
- `solver_config` -- algorithm identity, iteration budget, isomorphism, and
  pruning knobs.
- `solve` -- a combined digest folding the five components together.

Hashes are always computed and serialize as `0x`-prefixed hex strings.

### Best-response exploitability (heads-up)

For heads-up solves (river, turn and flop), the solver measures exact best-response
exploitability against the average strategy. It reuses the same vectorized
node-value backup, but at the exploiter's own decision nodes it takes the
per-combo maximum over that seat's actions instead of the strategy-weighted sum.
Because each combo is its own infoset and the counterfactual node reach for the
opponent is independent of the exploiter's strategy, the per-combo maximum is an
exact best response. The per-seat gap is `best_response_value - profile_value`,
each the seat's root range EV: its counterfactual values weighted by its reach and
divided by the reach mass of the hand matchups the ranges allow, so the gap is in
chips. It is non-negative by construction. The aggregate exploitability is the mean of the
two seats' gaps (`NashConv / 2`), reported in EV (chip) units together with its
fraction of the gross pot. The measurement uses no dynamic pruning so the best
response may exploit pruned-away actions.

Measurement is opt-in. When disabled (the default), the solver skips the
best-response passes but still records hashes and warnings, leaving the strategy
and timing unchanged.

### Normalized regret (multiway)

Multiway solves do not measure an exact best response; the report instead carries a normalized average-regret metric: the mean,
over infosets and active combos, of the maximum positive accumulated regret,
divided by the iteration count. It trends toward zero as the average strategy
approaches equilibrium and leaves `exploitability_available` false.

### Convergence curve and quality-threshold stopping

With a positive `measurement_interval`, the primary metric (heads-up
exploitability) is sampled every that-many iterations into a curve of
`(iteration, metric, elapsed_ms)` points, capped at `max_curve_samples`. With a
positive `target_exploitability`, the CFR loop stops early the first time the
measured exploitability drops to or below the threshold; the recorded iteration
count reflects the reduced work and `reached_target` is set.

### Warnings

Each solve lists the ways it departs from an exact no-limit equilibrium:
abstraction-only equilibrium, multiway normalized-regret-only reporting, lossy
card isomorphism, and dynamic pruning.
