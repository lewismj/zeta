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

## Board and hole-combination indexing

`board` stores public cards as one `card_mask` and enforces street cardinality through assertions:

- flop: 3 cards
- turn: 4 cards
- river: 5 cards

Two-card private holdings use a dense index:

- `using combination_index = uint16_t`
- `combination_count = 1326`
- `combination_masks[1326]` maps each index to a two-bit `card_mask`

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

## Terminal-state model

The terminal layer is player-count templated (`N`) and uses the same structures for heads-up and multiway:

- `terminal_context<N>`: pot/rake/contribution accounting
- `player_mask<N>` and `folded_mask<N>`: seat-state masks
- `pot_layer<N>`: side-pot accounting with eligibility/contributor masks
- `terminal_state<N>`: one auditable terminal record (showdown/fold/etc.)
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

These structures are reused by terminal showdown/fold algorithms and CFR terminal leaf evaluation.

## CFR graph topology

The solver graph uses immutable CSR storage (`game_graph`):

- `row_offsets[node_count + 1]`
- `edges[]` with `{child_node, action_index}`
- `node_types[]` (`player_chance`, `player`, `chance`, `terminal`)
- `infoset_id[]`
- `node_depth[]`, `subtree_size[]`

Graph ordering is DFS post-order, which supports bottom-up style processing and deterministic traversal properties.

## Graph side metadata and infoset lowering

`solver_graph_annotations` stores side arrays indexed by node id:

- `actor_by_node`
- `chance_event_id_by_node`
- `terminal_leaf_id_by_node`
- `state_by_node` (`street`, public-state id, betting-state id)

Hold'em infoset identity starts as `holdem_infoset_key` (actor/street/player count and abstraction ids), then lowers to dense IDs through `holdem_infoset_lowering`:

- `dense_id_by_node`
- `key_by_infoset`
- `owner_by_infoset`
- compact legal-action arrays via offsets

## Action-table layout and value tables

Regret and strategy tables share one contiguous layout:

- `action_table_layout::action_offsets[infoset_count + 1]`
- flat values indexed by `offset(infoset_id, action_index)`

This infoset-major contiguous addressing is used by:

- `regret_table`
- `strategy_sum_table`
- worker-local delta buffers
- checkpoint chunking and owner-range reduction

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

## Worker and solver context surfaces

`cfr_solver_context<N>` is a non-owning bundle of shared state:

- graph + annotations
- action layout
- regrets + strategy sums
- chance events
- terminal provider
- numeric/reduction policy
- infoset owner map

`traversal::worker_context` is per-worker mutable state:

- iterative traversal frame arrays
- reach/value scratch buffers
- edge-probability and child-value buffers
- local `table_delta_buffer`
- per-worker diagnostics

The split is intentional: shared immutable/read-mostly surfaces in solver context, private scratch and local deltas in worker context.

## Scheduler and partition structures

Parallel iteration uses board-partition tasks:

- `graph_partition`: node span + estimated work
- `board_partition_plan`: Cartesian product of boards and partitions
- `board_partition_task`: immutable `(board_index, partition_index, task_index)`

This plan is consumed by a dynamic atomic-queue scheduler and supports deterministic accounting counters (`tasks_executed`, `estimated_work`).

## Checkpoint structures

Checkpointing uses a compatibility-checked binary header:

- `cfr_checkpoint_header`: versioning, solver config, hashes, counts
- per-owner `cfr_checkpoint_table_chunk_header`: value spans for regrets/strategy sums

Compatibility keys hash graph metadata, action layout, numeric/reduction policy, and chance mode to prevent unsafe resume across incompatible solver states.
