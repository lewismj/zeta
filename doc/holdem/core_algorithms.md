# Hold'em Core Algorithms

This document describes the major algorithms used by Hold'em runtime paths: evaluation, terminal value computation, and CFR iteration.

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

These layers are packed into a canonical key and mapped to a dense restricted-quinary index, then:

```cpp
rank = non_flush_table[index];
```

The runtime indexer uses precomputed chunk tables (4+4+5 rank chunks) to avoid a long dependent DP loop.

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

`terminal_workspace` then materializes `river_reach_index` for each player's range:

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

`evaluate_showdown_values` dispatches by player count and mode:

- exact route for compatible contexts
- sampled route (`evaluate_showdown_values_multiplayer_sampled`) for scalable N-way estimation using configured samples per combo

Both routes consume the same cached terminal structures.

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

Traversal uses `probability_for_edge(node_id, edge)` and multiplies chance reach by that probability.

## 7. Infoset lowering and action-layout construction

Player-node metadata is lowered from semantic keys (`holdem_infoset_key`) to dense infoset IDs:

1. validate per-node metadata (actor/street/abstraction ids/legal actions)
2. ensure shared infosets are internally consistent
3. emit dense mapping + compact legal-action side arrays

`make_action_table_layout(graph)` then builds contiguous action offsets so regret and strategy tables can share flat storage.

## 8. Regret matching

For a player node, action probabilities are computed from regret values:

```text
p(a) = max(r(a), 0) / sum_b max(r(b), 0), if positive sum > 0
p(a) = 1 / |A| otherwise
```

This is implemented by `compute_regret_matching_strategy` with policy-specialized positive-regret extraction.

## 9. CFR traversal and update equations

`run_cfr_iteration<N>` executes one full iteration over shared graph/tables with worker-local deltas.

### Traversal

The production kernel is iterative (explicit frame stacks), not recursive:

1. enter node
2. compute/lookup action probabilities
3. propagate reach through edges
4. accumulate child values back to parent
5. exit node and emit local deltas

Chance nodes:

```text
V(node) = sum_a p_chance(a) * V(child_a)
```

Player nodes:

```text
V(node) = sum_a pi(a) * V(child_a)
```

### Strategy accumulation

Per action:

```text
strategy_delta(a) += strategy_weight
                   * chance_reach
                   * own_reach(actor)
                   * pi(a)
```

### Regret update (updating player only, alternating mode)

Per action:

```text
regret_delta(a) += counterfactual_reach(actor)
                 * (V(child_a) - V(node))
```

This is accumulated in each worker's local delta buffer.

## 10. Parallel scheduling and deterministic reduction

Iteration work is partitioned and scheduled as `(board, graph-partition)` tasks.

- scheduler: dynamic atomic queue, configurable worker count/chunk size
- each worker accumulates local table deltas
- reductions merge deltas into global regret/strategy tables using an owner map and deterministic reduction plan

Reduction diagnostics track:

- remote delta routing
- touched values
- per-owner merge time

## 11. Checkpoint save/load

Checkpoint flow:

1. validate context and compatibility surfaces
2. write/read fixed header (version, policies, hashes, counts)
3. stream owner-range chunks of regrets + strategy sums
4. reject load if metadata/layout/policy hashes differ

This preserves resumability while guarding against unsafe state mismatches.

## 12. Reference and release-gate validation algorithms

The solver includes a slower reference traversal over the same surfaces:

- produces full node/action/probability/reach delta vectors
- compares production vs reference within configured tolerances

Convergence/quality gates are computed from:

- exploitability estimate
- regret norm
- average strategy mass

These diagnostics provide a deterministic release-facing acceptance surface for CFR behavior.
