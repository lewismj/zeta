# Runout Handling Implementation Plan

## Goal

Implement step 1 from `doc/holdem/next_steps.md` as a first-class solver capability: exact public-card chance expansion from flop and turn spots, blocker-aware runout enumeration, deterministic board-partition scheduling, reusable per-runout terminal caches, and persisted runout-aware solve artifacts. The implementation should follow the current architecture directly rather than layering compatibility shims onto the existing river-only extraction path.

## Current state

Stages 1 through 6 are implemented and validated end to end for the exact heads-up baseline. Stage 4 (parallel scheduler integration) is wired into the CLI solve path: the heads-up runout solve distributes per-runout river-terminal evaluation across worker threads through `run_board_partition_scheduler`, binding each scheduler task to a canonical runout index, and then reduces the per-runout results sequentially in runout order so the solved output is bit-identical across worker counts. The identity model, registries, enumeration, public-game lowering, runout terminal table, worker-local materialization, parallel runout-aware CLI solving, schema-3 persistence, and the runout-aware UI inspection surfaces are all in place, without compatibility shims or river-only fallbacks.

### Implemented lowering shape (important scope note)

`lower_public_game_tree(...)` lowers a non-river spot into a **single combined-runout** public game rather than a per-street betting expansion:

- the root is a player node with one pass-through action into one chance node,
- the chance node deals the complete remaining public cards to the river in a single step (two cards for a flop spot, one for a turn spot), enumerating every blocker-safe outcome,
- each outcome maps to a distinct river public state and a showdown terminal.

This is a deliberate first-class baseline, not a placeholder: the `complete_runout` record carries both `dealt_turn` and `dealt_river`, and the runout registry, chance-event table, public-state registry, and terminal table are all fully populated and validated for this shape. It intentionally does **not** yet model intermediate per-street betting (flop betting -> turn chance -> turn betting -> river chance -> river betting). Adding per-street betting expansion inside the public game is a separate future stage; until then the non-river solve reports the blocker-aware runout-averaged showdown value per hero combo.

### Implemented solve path

`solve_spot(...)` routes non-river heads-up spots through `solve_public_game_runout(...)`, which:

- lowers the public game with `lower_public_game_tree(...)`,
- builds a `runout_terminal_table` from the public-state registry,
- for every runout, materializes the river reach indices and evaluates the exact river showdown, accumulating a blocker-aware average value per hero combo,
- emits a schema-3 artifact carrying the public states, chance events, runouts, and solved-node graph via `append_solved_graph_payload(...)`.

River spots continue to use the existing vectorized root-actor river fast path. Multi-way (N > 2) non-river spots return a clear unsupported-solver error because the exact showdown kernel is defined for heads-up play only.

### Supporting layers reused

- `chance_event_table` and the enumeration helpers in `zeta\holdem\src\cfr\chance\chance.h` model public-card outcomes with aligned child edges, probabilities, dead-card metadata, and deterministic `board_partition_id` values.
- The terminal layer stays river-specialized: `make_river_terminal_cache(...)`, `make_river_reach_index(...)`, `terminal_workspace<N>`, and `terminal_engine<N>` reuse immutable per-river-board caches (`zeta\holdem\src\terminal\reach_index.h`, `zeta\holdem\src\terminal\workspace.h`, `zeta\holdem\src\terminal\engine.h`).
- `make_runout_terminal_table(...)` and the worker-local `get_or_materialize_reach_indices(public_state_id, ...)` provide the per-public-state river cache lookup (`zeta\holdem\src\cfr\chance\chance.h`, `zeta\holdem\src\terminal\workspace.h`).
- The CLI artifact, the UI `solution_store`, and the document envelope are all on schema version 3 and hard-reject older schemas rather than migrating them.

## Design principles

1. **Make chance expansion structural, not post-processing.** Flop and turn boards must be represented in the graph and solver metadata, not approximated by pre-averaged terminal values.
2. **Preserve the current split between immutable shared state and worker-local scratch.** New runout surfaces should fit the existing `cfr_solver_context` plus `worker_context` architecture.
3. **Keep identity namespaces separate.** Public-state identity, chance-event identity, chance-outcome identity, complete-runout identity, and scheduler partition identity must not be conflated.
4. **Reuse existing exact river methodology.** All earlier-street solving should reduce to existing river terminal evaluation on derived river boards, not a parallel custom evaluation path.
5. **Prove chance math before parallelism concerns.** Exact probability propagation and normalization are correctness requirements, not just implementation details.
6. **No backward-compatible shims in the new design.** The primary solve path should become runout-native. Existing fallback-only artifact behavior can be removed or version-bumped where necessary instead of being preserved indefinitely.

## Identity model

The implementation must keep the following IDs as separate namespaces with separate semantics.

| Identity | Represents | Cardinality / scope |
| --- | --- | --- |
| `public_state_id` | One exact public board state | One per canonical board in the lowered game |
| `chance_event_id` | One public-card dealing decision | One per chance node |
| `chance_outcome_id` | One child outcome of one chance event | Unique within one chance-event registry |
| `runout_id` | One complete root-to-river public-card path | Global within one solved spot |
| `board_partition_id` | One scheduler-oriented partition label | Execution metadata, not game-state identity |

Examples:

- on a flop solve, a turn-card reveal is a `chance_outcome_id`, not a complete `runout_id`
- on a flop solve, `runout_id` refers to a complete turn+river completion
- on a turn solve, a river reveal may be both the only `chance_outcome_id` step and the complete `runout_id`
- `board_partition_id` may initially map 1:1 to exact complete runouts, but that is an implementation choice, not the meaning of the ID

## Core data model

The cleanest design is to introduce one new shared layer that sits beside the graph and chance table rather than hiding runout semantics inside ad hoc side arrays.

### Public-state registry

Add an owning registry for exact public board states:

```cpp
struct public_board_state {
    public_state_id id = invalid_public_state_id;
    holdem_street street = holdem_street::invalid;
    card_mask board_cards = 0;
    public_state_id parent_state_id = invalid_public_state_id;
    chance_event_id chance_event_id_from_parent = invalid_chance_event_id;
    chance_outcome_id chance_outcome_id_from_parent = invalid_chance_outcome_id;
    bool is_root_state = false;
    bool is_terminal_river_state = false;
};

struct public_state_registry {
    std::vector<public_board_state> states;
    std::vector<public_state_id> state_id_by_node;
};
```

This registry is the canonical owner of:

- exact board cards
- public-state ancestry
- chance ancestry

It should be validated independently from the graph so bugs in lowering are easier to isolate.

### Chance registry

Keep `chance_event_table`, but enrich the conceptual model around it:

```cpp
struct chance_outcome {
    uint32_t child_node = game_graph::INVALID_NODE;
    uint16_t action_index = 0;
    float probability = 0.0f;
    board_partition_id partition = invalid_board_partition_id;
    chance_outcome_id outcome = invalid_chance_outcome_id;
    card_mask cards = 0;
    card_mask dead_cards = 0;
    bool legal = true;
};
```

Important distinction:

- `chance_outcome_id` is game-state identity
- `board_partition_id` is execution metadata

The first implementation may set them numerically equal in some cases, but the design must not rely on that.

### Runout registry

For flop and turn solves, add a canonical complete-runout registry:

```cpp
struct complete_runout {
    runout_id id = invalid_runout_id;
    public_state_id root_public_state_id = invalid_public_state_id;
    public_state_id river_public_state_id = invalid_public_state_id;
    card_mask dealt_turn = 0;
    card_mask dealt_river = 0;
};

struct runout_registry {
    std::vector<complete_runout> runouts;
    std::vector<public_state_id> river_public_state_by_runout;
};
```

This is the canonical bridge between:

- public-card expansion
- sequential traversal verification
- scheduler board-major execution
- artifact serialization

The intended mental model is:

- `public_state_id` = node in the public-card tree
- `runout_id` = complete root-to-river path through that tree

## Runtime architecture

The runtime should remain a composition of immutable shared state plus worker-local scratch.

### Shared immutable solve context

The runout-aware solve context should conceptually contain:

```cpp
template <std::size_t N>
struct runout_solver_context {
    game_graph* graph = nullptr;
    solver_graph_annotations* graph_annotations = nullptr;
    public_state_registry* public_states = nullptr;
    chance_event_table* chance_events = nullptr;
    runout_registry* runouts = nullptr;
    runout_terminal_table* terminals = nullptr;
    action_table_layout* layout = nullptr;
    regret_table* regrets = nullptr;
    strategy_sum_table* strategy_sums = nullptr;
    const infoset_owner_map* owner_map = nullptr;
    numeric_policy numeric{};
    reduction_policy reduction{};
    chance_mode chance = chance_mode::enumerate;
};
```

This should replace the assumption that one solve binds one river cache globally.

### Runout terminal table

Add a shared registry of river-only terminal caches:

```cpp
struct runout_terminal_entry {
    public_state_id public_state = invalid_public_state_id;
    board river_board{};
    river_terminal_cache cache{};
};

struct runout_terminal_table {
    std::vector<runout_terminal_entry> entries;
    std::vector<uint32_t> entry_id_by_public_state;
};
```

Invariants:

- every river public state maps to exactly one terminal entry
- non-river public states map to no terminal entry
- terminal lookup uses `public_state_id`, never `board_partition_id`

### Worker-local scratch

Retain the current scratch split but make terminal binding lookup-based:

```cpp
template <std::size_t N>
struct runout_terminal_worker_cache {
    public_state_id current_public_state = invalid_public_state_id;
    terminal_workspace<N> workspace{};
    std::array<river_reach_index, N>* current_reach_indices = nullptr;
};
```

The important API is:

- `get_or_materialize_reach_indices(public_state_id, ranges)`

That leaves room for:

- current-public-state cache only
- tiny LRU per worker
- precomputed shared reach indices later, if ever justified

without forcing the first implementation to commit to one caching strategy permanently.

## End-to-end data flow

The intended data flow should be explicit.

### Build phase

1. Parse spot and normalize board/range inputs.
2. Build root public state from the input board.
3. Enumerate legal public-card transitions.
4. Lower a full multi-street public game graph.
5. Emit:
   - `game_graph`
   - `solver_graph_annotations`
   - `public_state_registry`
   - `chance_event_table`
   - `runout_registry`
   - terminal leaf table
6. Build `runout_terminal_table` for all river public states.
7. Validate cross-links between all registries.
8. Validate that runout identity is path-based rather than stored on intermediate public states.

### Solve phase

1. Bind shared solve context.
2. Traverse chance nodes by multiplying chance reach by the stored edge probability.
3. When a terminal leaf is reached:
   - resolve `public_state_id`
   - resolve the river terminal cache from `runout_terminal_table`
   - resolve or materialize worker-local reach indices for that public state
   - evaluate via existing `terminal_engine<N>`
4. Accumulate regrets and average strategy on the unified graph.

### Persist phase

1. Serialize canonical solved structure:
   - spot summary
   - public states
   - chance events
   - runouts
   - solved nodes
   - strategy/EV surfaces
2. Exclude runtime-only structures:
   - `river_terminal_cache`
   - materialized reach indices
   - worker scratch
   - scheduler task queues

## Detailed design decisions

### Exact enumeration policy

Canonical child ordering should be deck-order deterministic and shared across:

- chance outcome construction
- public-state creation
- runout ID assignment
- artifact serialization
- scheduler mapping

For single-card transitions, use increasing card ID order. For multi-card transitions, use lexicographic order on sorted card IDs. The current `enumerate_public_card_outcomes(...)` shape already points in this direction and should become the sole canonical source.

### Graph lowering boundary

Do not overload `lower_betting_tree_to_graph(...)` with a radically different conceptual contract. A new public-game lowerer is cleaner, for example:

```cpp
template <std::size_t N>
std::expected<holdem_public_game_graph<N>, betting_validation_error>
lower_public_game_tree(const holdem_public_game_config<N>& config);
```

Internally it can reuse:

- existing betting-state transition logic
- existing terminal-state construction
- existing graph builder

But the outward contract should clearly be:

- input: spot + public-card methodology
- output: full public game with chance and river terminals

### Infoset baseline

The first exact implementation should deliberately avoid abstraction:

- `public_board_abstraction_id = exact public_state_id`
- no public-board merging
- no board bucketing
- no runout bucketing

That gives a correctness baseline before later abstraction work. Only after this path is proven should optional board abstraction be introduced.

### Probability semantics

Probability handling must be treated as a formal solver invariant:

- `chance_event_table` owns conditional probabilities
- traversal consumes them directly
- complete-runout probability is the path product of conditional probabilities
- scheduler normalization must be mathematically identical to sequential traversal

This should be proven with golden tests before parallel scheduling is introduced.

### Scheduler binding

The scheduler should remain an execution layer, not a semantic owner:

- scheduler task -> `runout_id`
- `runout_id` -> river `public_state_id`
- `public_state_id` -> terminal cache

That preserves future freedom to:

- group multiple runouts into one task
- rebalance partitions
- add abstraction-driven partitions
- add sampling
- distribute execution

without redefining game-state identity.

## Staged implementation plan

## Stage 1 - Identity, registries, and exact enumeration

Build the canonical semantic layer first.

### Deliverables

- add identifier types / aliases and invalid sentinels for:
  - `public_state_id`
  - `chance_event_id`
  - `chance_outcome_id`
  - `runout_id`
  - `board_partition_id`
- add:
  - `public_state_registry`
  - `runout_registry`
  - validation helpers for both
- make public-card enumeration emit:
  - canonical outcome ordering
  - conditional probabilities
  - exact chance-outcome identity
- use the resulting public-state tree to construct canonical complete root-to-river runout identities
- add exact cardinality tests:
  - legal outcome count matches remaining live cards
  - no duplicate boards
  - every legal board appears exactly once
  - conditional probability sums are 1

### Files most directly affected

- `zeta\holdem\src\cfr\chance\chance.h`
- `zeta\holdem\src\cfr\solver\metadata.h`
- new public-state / runout registry files under `zeta\holdem\src\cfr\...`
- `zeta\test\src\test_cfr_graph.cpp`

## Stage 2 - Multi-street public-game lowering

Move chance into the actual game graph.

### Deliverables

- introduce a new public-game lowerer
- expand:
  - flop betting -> turn chance -> turn betting
  - turn betting -> river chance -> river betting
- emit in one lowering pass:
  - `game_graph`
  - annotations
  - public-state registry
  - chance-event table
  - runout registry
  - terminal leaf table
- populate exact per-node street and public-state metadata
- validate graph / registry / chance cross-links together

### Files most directly affected

- `zeta\holdem\src\cfr\betting\betting.h`
- `zeta\holdem\src\cfr\graph\builder.h`
- `zeta\holdem\src\cfr\graph\builder.cpp`
- `zeta\holdem\src\cfr\chance\chance.h`
- `zeta\test\src\test_cfr_graph.cpp`

## Stage 3 - Sequential runout-aware solving and terminal binding

Prove the semantics before parallelism.

### Deliverables

- add a runout-aware solve context
- add `runout_terminal_table`
- replace single-river terminal binding with `public_state_id` lookup
- add worker-local `get_or_materialize_reach_indices(public_state_id, ...)`
- add a sequential traversal / iteration path over the unified graph
- prove:
  - `sum(turn outcome probabilities) == 1`
  - `sum(river outcome probabilities | turn) == 1`
  - `sum(complete runout probabilities from root) == 1`
- add golden EV tests comparing:
  - graph traversal
  - explicit independent enumeration

### Files most directly affected

- `zeta\holdem\src\cfr\solver\context.h`
- `zeta\holdem\src\cfr\solver\river_context.h` or replacement file
- `zeta\holdem\src\cfr\solver\iteration.h`
- `zeta\holdem\src\cfr\traversal\traversal.h`
- `zeta\holdem\src\terminal\workspace.h`
- `zeta\test\src\test_cfr_graph.cpp`

## Stage 4 - Parallel scheduler integration and deterministic CFR

Integrate the existing scheduler after sequential correctness is established. This
stage is implemented: `solve_public_game_runout` builds a board-partition plan over
runouts and drives `run_board_partition_scheduler` to evaluate each runout's river
terminal on a worker thread, writing results to disjoint per-runout slots and then
reducing them sequentially in runout order.

### Deliverables

- [x] bind scheduler tasks to canonical `runout_id` (the task `board_index` maps directly to the runout index)
- [x] keep `board_partition_id` separate from game-state identity (it remains execution-only scheduling metadata)
- [x] ensure parallel normalization matches sequential semantics exactly (ordered reduction over per-runout results)
- [x] ensure deterministic solved output across worker counts (covered by `holdem_cli_runout_solve_is_deterministic_across_worker_counts`, which asserts bit-identical EVs for 1 vs 8 workers)
- benchmark board-major scheduling on the new graph shape

### Files most directly affected

- `zeta\holdem\src\cli\solve_cli.h`
- `zeta\holdem\src\cfr\scheduler\scheduler.h`
- `zeta\holdem\src\cfr\chance\chance.h`
- `zeta\benchmark\holdem\src\cfr_benchmark.cpp`
- `zeta\test\src\test_holdem_cli.cpp`
- `zeta\test\src\test_cfr_graph.cpp`

## Stage 5 - Replace CLI solving with the native runout-aware path

Delete the non-river workaround and make the CLI drive the real solver graph.

### Deliverables

- remove fixed-terminal averaging for flop/turn spots
- remove the dependency on choosing one compatible combo set for the solve
- lower the full public game from the input spot
- solve flop, turn, and river through one unified path
- update extraction to read solved results from the unified graph rather than from a root-only artifact shape

The CLI now lowers the full public game from each non-river spot and solves it through the dedicated runout-aware path (`solve_public_game_runout(...)`) instead of the single-street betting-tree CFR loop. River spots still use the vectorized river fast path.

### Files most directly affected

- `zeta\holdem\src\cli\solve_cli.h`
- `zeta\holdem\src\cli\solve_cli.cpp`
- `zeta\tools\holdem\src\solve_cli.cpp`
- `zeta\test\src\test_holdem_cli.cpp`

## Stage 6 - Runout-aware persistence and inspection surfaces

Persist the solved structure directly, then expose it in the UI.

### Deliverables

- set the CLI artifact schema to version 3 (uniform across all artifacts)
- serialize:
  - public states
  - chance events
  - runouts
  - solved node graph
  - strategy / EV surfaces
- set the UI solution store and document schema to version 3
- stop reconstructing a fake same-street tree from `spot`
- render chance nodes and exact board/runout state in the explorer
- hard-reject incompatible old schemas instead of adding migration shims

### Files most directly affected

- `zeta\holdem\src\cli\solve_cli.h`
- `zeta\holdem\src\cli\solve_cli.cpp`
- `zeta\ui\holdem\src\solver\solution_store.h`
- `zeta\ui\holdem\src\solver\solution_store.cpp`
- `zeta\ui\holdem\src\document\document_json.h`
- `zeta\ui\holdem\src\document\document_json.cpp`
- `zeta\ui\holdem\src\widgets\strategy_explorer.cpp`
- `zeta\ui\holdem\src\viewmodels\strategy_view_model.cpp`
- `zeta\ui\holdem\src\main_window.cpp`
- `zeta\ui\holdem\src\study\study_workflow.cpp`
- `zeta\test\src\test_holdem_cli.cpp`
- `zeta\test\src\test_holdem_ui.cpp`

## Validation plan

## Core graph and registry tests

Add targeted tests covering:

1. exact legal-outcome cardinality from remaining live cards
2. no duplicate resulting public boards
3. every legal resulting public board appears exactly once
4. exact `public_state_id` uniqueness per canonical board
5. exact `runout_id` uniqueness per complete root-to-river public-state path
6. correct street metadata after each public-card transition
7. correct parent/child ancestry across public states
8. every runout terminates at exactly one river public state
9. `runout_id` maps bijectively to one complete root-to-river public-state path

## Solver correctness tests

Add tests covering:

1. [x] chance outcome probabilities sum to 1 for every chance event (`public_game_lowering_runout_probabilities_form_valid_distribution`)
2. [x] complete runout probabilities sum to 1 from the root (`public_game_lowering_runout_probabilities_form_valid_distribution`)
3. [x] chance traversal EV equals independently enumerated probability-weighted EV on golden games (`holdem_cli_runout_solve_matches_hand_computed_ev`)
4. river spots still solve through the new unified path without structural regressions
5. turn solves use explicit chance nodes rather than fixed pre-averaged terminals
6. flop solves use both turn and river public-card expansion
7. no terminal evaluation is attempted on non-river public states
8. [x] solved output remains deterministic across worker counts (`holdem_cli_runout_solve_is_deterministic_across_worker_counts`)

## Serialization tests

Add tests covering:

1. artifact schema v2 round-trip with public states, chance events, and runouts
2. solution store schema bump and round-trip
3. UI document envelope round-trip with embedded runout-aware solution payload
4. hard rejection of old incompatible schema versions

## Benchmark and performance checks

Use existing benchmarks to add focused measurements for:

- public-state registry construction
- chance outcome table construction
- runout terminal cache construction
- traversal cost with board-major scheduling
- memory growth from added public-state and runout registries

## Key risks and mitigations

| Risk | Why it matters | Mitigation |
| --- | --- | --- |
| Infoset collapse across distinct public boards | Produces structurally invalid equilibrium even if traversal runs | Make `public_state_id` exact and validate shared infosets aggressively |
| Chance probability normalization drift | Produces plausible-looking but wrong EVs and regrets | Prove probability-sum and golden-EV invariants before parallel scheduling |
| Scheduler identity leaking into game identity | Prevents later regrouping, sampling, and execution changes | Keep `board_partition_id` separate from `public_state_id` and `runout_id` |
| Recomputing reach indices too often | Can erase the benefit of exact river cache reuse | Keep reach-index lookup behind an API so caching policy can evolve after profiling |
| Chance outcome ordering drift | Breaks determinism, artifacts, and scheduler/task reproducibility | Define one canonical outcome ordering and reuse it everywhere |
| Artifact/UI regeneration from `spot` instead of solved graph | Loses exact chance structure and runout identity | Persist solved graph references directly and read them back verbatim |
| Trying to preserve the old non-river pre-averaging shortcut | Leaves two competing solver methodologies in the code while the river-specialized terminal evaluator should remain the exact terminal optimization | Delete only the non-river pre-averaging shortcut once the native chance-expanded path is in place |

## Recommended implementation order

1. Identity, registries, and exact enumeration
2. Multi-street public-game lowering
3. Sequential runout-aware solving and terminal binding
4. Parallel scheduler integration and deterministic CFR
5. Replace CLI solving with the native runout-aware path
6. Runout-aware persistence and inspection surfaces

Six stages is a better fit here. It is enough separation to isolate correctness risks, but compact enough that the plan still reads like one coherent feature implementation rather than a long backlog.
