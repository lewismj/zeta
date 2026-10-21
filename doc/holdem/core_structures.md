# Hold'em Core Structures

This document maps the core runtime structures used by the Hold'em evaluator, terminal engine, and CFR solver surfaces.

## Card identity and bit indexing

Hold'em uses the shared `zeta::card_mask` representation from core:

```cpp
using card_mask = uint64_t;
using card = uint8_t;
```

For the default 52-card deck, each card is one bit in a 64-bit mask:

```text
bit = suit * 13 + rank

bits  0..12  spades   (2..A)
bits 13..25  hearts   (2..A)
bits 26..38  diamonds (2..A)
bits 39..51  clubs    (2..A)
```

This layout is the basis for:

1. fast suit extraction via shift/mask
2. dead-card collision checks via bitwise `&`
3. branch-light combo liveness and blocker logic

A suit-rank mask is the compact 13-bit view for one suit. Bit `r` is set when
that suit contains rank `r`; for example, `0b1000000000001` means ace and two
are present in that suit. The 7-card evaluator builds four of these masks per
hand and reuses them for flush detection and non-flush rank-count encoding.

## Board and hole-combination indexing

`board` stores public cards as one `card_mask` and enforces street cardinality through assertions:

- flop: 3 cards
- turn: 4 cards
- river: 5 cards

Two-card private holdings use a dense index:

- `using combination_index = uint16_t`
- `combination_count = 1326`
- `combination_masks[1326]` maps each index to a two-bit `card_mask`

The count is the number of unordered two-card combinations:

```text
C(52, 2) = 1326
```

`combination_masks` are generated in deterministic rank/suit order and reused throughout:

- range storage (`hand_range`)
- terminal cache materialization
- reach-index construction
- CFR combo selection and benchmarking fixtures

## Range and reach vectors

`hand_range` is an owning dense vector of per-combo weights:

```cpp
std::array<float, 1326> weights;
```

Key operations are intentionally in-place and allocation-free:

- `normalize()`
- `remove_dead(dead_mask)`
- `scale(factor)`
- direct `operator[](combination_index)`

Terminal and solver paths reuse this storage shape through `reach_vector` and derived reach indices.

A dead-card mask contains cards that cannot appear in a private hand, such as
board cards or known blockers. A combo is live when it does not overlap that
mask:

```cpp
(combination_masks[i] & dead) == 0
```

Removing dead combos from a range is represented by setting blocked combo
weights to zero while preserving dense `combination_index` addressing.

## Terminal-state model

The terminal layer is player-count templated (`N`) and uses the same structures for heads-up and multiway:

- `terminal_context<N>`: pot/rake/contribution accounting
- `player_mask<N>` and `folded_mask<N>`: seat-state masks
- `pot_layer<N>`: side-pot accounting with eligibility/contributor masks
- `terminal_state<N>`: one auditable terminal record (showdown or fold)
- `terminal_state_table<N>`: owning contiguous terminal-state storage

For heads-up, `folded_mask<2>` is specialized to two booleans (`oop_folded`, `ip_folded`) to avoid generic bitset overhead.

## River terminal cache and reach index

`river_terminal_cache` is an immutable board-specialized cache for one river board:

- `masks[1326]`: board+combo masks
- `rank_keys[1326]`: evaluated rank keys
- `live` combo bitset
- `rank_order`: combos sorted by rank for showdown kernels
- `cards[1326]`: unpacked per-combo card ids

`river_reach_index` is a range-conditioned view derived from cache + reach weights:

- `weights[1326]`
- `active_indices[]` and `active_count`
- `mass_by_card[52]`
- rank-bucket arrays for rank-sweep/blocker-correction logic

These structures are reused by the terminal showdown and fold kernels for every terminal leaf.

## CFR graph topology

The solver graph uses immutable CSR storage (`game_graph`):

- `row_offsets[node_count + 1]`
- `edges[]` with `{child_node, action_index}`
- `node_types[]` (`player_chance`, `player`, `chance`, `terminal`)
- `infoset_id[]`
- `node_depth[]`, `subtree_size[]`

Graph ordering is DFS post-order, so one ascending pass visits children before parents; the value backup relies on this.

## Graph side metadata

`solver_graph_annotations` stores side arrays indexed by node id:

- `actor_by_node`
- `chance_event_id_by_node`
- `terminal_leaf_id_by_node`
- `state_by_node` (`street`, public-state id, betting-state id)

The betting-tree lowering assigns dense infoset ids directly; nodes that share an
infoset share its action count.

## Action-table layout and combo tables

The solver's tables share one contiguous layout:

- `action_table_layout::action_offsets[infoset_count + 1]`
- one row of action slots per infoset, repeated for each of the 1326 hands

`combo_action_table` (in `cli/solve_cli.h`) stores regrets, strategy sums, and the
current and average strategies in this `hand x action-slot` shape.

## Chance-event storage

Public chance enumeration is stored as:

- `chance_event_table::events[]` (node-level slices)
- `chance_event_table::outcomes[]` (flattened outcomes)
- `event_id_by_node[]` lookup

Each `chance_outcome` includes:

- child alignment (`child_node`, `action_index`)
- `probability`
- dealt `cards`
- `dead_cards`
- board partition id and legality flags

## Parallel terminal evaluation

`scheduler::worker_pool` owns `worker_threads - 1` threads; the calling thread is
the remaining worker. `run(task_count, body, chunk_size)` hands task indices out in
chunks from an atomic counter and returns once every task has run, rethrowing the
first exception a task threw. The solver uses one pool per solve and runs each
evaluation pass's terminal leaves as one batch; each task allocates its own reach
indices, so tasks share no mutable state.
