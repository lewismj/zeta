# Runout Handling Fix Rollout

## Goal

Finish the runout-aware Hold'em implementation without shims by rebuilding the
public-game lowering and then wiring solve, persistence, and UI layers onto the
resulting canonical graph.

This rollout is corrective. It assumes the current implementation is only
partially complete and that some status claims in
`doc\holdem\runout_handling_plan.md` are ahead of the code.

## Status

Stages A, B (via direct runout evaluation), C, D, E, and F are complete and
validated: `lower_public_game_tree(...)` is rebuilt, the non-river CLI solve runs
through the dedicated runout-aware path, persistence and UI consume the solved
runout payload, and the full `zeta_tests` suite passes. Stage C (parallel
scheduler / multi-worker evaluation over runout tasks) is wired in: per-runout
river-terminal evaluation is distributed through `run_board_partition_scheduler`
and reduced in canonical runout order, so solved EVs are bit-identical across
worker counts (asserted by `holdem_cli_runout_solve_is_deterministic_across_worker_counts`).

The implemented lowering models each non-river spot as a **single combined
runout**: the root player node has one pass-through action into one chance node
that deals the complete remaining board to the river in a single step, and each
enumerated outcome becomes one river public state and one showdown terminal. This
matches the canonical registry contract (`chance_events.events.size() == 1` per
solve, `complete_runout` carrying both `dealt_turn` and `dealt_river`) and is a
deliberate baseline that does not yet model intermediate per-street betting.

## Current blockers

The gaps found in the current code fall into four buckets:

1. `lower_public_game_tree(...)` does not yet produce a valid canonical
   flop->turn->river public-state graph.
2. CLI solving is still structurally river-oriented and cannot consume a true
   runout-aware graph end to end.
3. Persistence and UI layers still reconstruct or inspect root-oriented data
   instead of a solved runout-aware graph payload.
4. Test coverage is missing the exact integration proofs needed to prevent
   placeholder logic from reappearing.

## Non-goals

- Do not preserve removed heads-up alias formats or other compatibility shims.
- Do not add abstraction, bucketing, or sampling shortcuts as part of the fix.
- Do not ship partial stages that leave two competing solve paths in place.

## Rollout order

1. Rebuild public-game lowering.
2. Rebind sequential solve and terminal lookup to the lowered graph.
3. Integrate scheduler/CFR over runout-aware tasks.
4. Replace CLI solve/extract path.
5. Replace persistence and UI inspection surfaces.
6. Tighten tests, documentation, and final validation.

## Stage A - Rebuild public-game lowering

### Objective

Make `lower_public_game_tree(...)` the canonical source of the exact public-card
 game graph for flop and turn inputs.

### Required implementation steps

1. Build lowering around public-state identity first, not graph node identity.
2. Represent the public-card tree explicitly:
   - flop or turn root state
   - one combined-runout chance event that deals the complete remaining board
     (turn + river for a flop root, river for a turn root) in a single step
   - one river public state per enumerated legal complete board, each a terminal
     public state
3. Lower graph nodes from that tree only after the public-state structure is
   known.
4. Ensure node metadata is assigned after graph-builder reordering, using stable
   mappings from canonical public states and canonical chance events.
5. Emit and validate:
   - `public_state_registry`
   - `chance_event_table`
   - `runout_registry`
   - `terminal_leaves`
   - `terminal_states`
6. Ensure every river public state becomes exactly one terminal graph leaf and
   one runout terminal entry.

### Concrete code work

- Replace the current ad hoc one-step expansion in
  `zeta\holdem\src\cfr\betting\betting.h`.
- Introduce a local lowering structure that stores:
  - canonical public-state records
  - canonical chance-event records
  - parent/child state edges
  - root-to-river runout records
  - pre-reorder graph node handles
- After `graph_builder.build()`, translate all pre-reorder handles to final
  graph node ids before filling annotations and chance tables.
- Construct terminal states from actual river public states only.

### Exit criteria

- `lower_public_game_tree(...)` succeeds for both flop and turn roots.
- Metadata validation passes:
  - `validate_public_state_registry(...)`
  - `validate_chance_event_table(...)`
  - `validate_runout_registry(...)`
  - `validate_solver_graph_view(...)`
- No root/chance/terminal node relies on guessed post-reorder ids.

## Stage B - Bind the solver to public-state terminal lookup

### Objective

Make sequential traversal consume the runout-aware graph directly, resolving
terminal caches by `public_state_id`.

### Required implementation steps

1. Promote `runout_solver_context` from a side type to the actual basis for
   non-river solving.
2. Replace single-cache global terminal assumptions with:
   - `runout_terminal_table`
   - worker-local reach-index materialization by `public_state_id`
3. Ensure terminal evaluation is attempted only on river public states.
4. Preserve exact chance probabilities from `chance_event_table` during
   traversal.

### Concrete code work

- Extend/replace `cfr_solver_context` and terminal-provider plumbing in
  `zeta\holdem\src\cfr\solver\iteration.h`.
- Use `zeta\holdem\src\terminal\workspace.h` as the worker-local terminal
  materialization boundary.
- Add a terminal provider that resolves:
  `node_id -> public_state_id -> runout_terminal_entry -> reach indices`.
- Keep the existing fixed-terminal provider only for isolated unit tests and
  benchmarks, not as a production solve path.

### Exit criteria

- Flop and turn solves resolve terminal caches by `public_state_id` through the
  `runout_terminal_table`, using the lowered public-state metadata.
- No production solve path averages fixed terminal values outside the runout
  enumeration.
- Terminal evaluation only ever runs on river public states.

## Stage C - Integrate CFR and scheduler over runouts

### Objective

Run deterministic multi-worker evaluation over the unified graph while preserving
exact runout semantics.

### Status

Implemented. `solve_public_game_runout` builds a `board_partition_plan` whose board
count equals the runout count and drives `run_board_partition_scheduler`; each task's
`board_index` is the canonical runout index. Workers evaluate river terminals into
disjoint per-runout slots, and a single-threaded reduction folds those slots into the
per-combo EV accumulators in runout order, keeping the solved output bit-identical
across worker counts.

### Required implementation steps

1. [x] Bind scheduler work to canonical runout/public-state outputs, not ad hoc
   river-board vectors.
2. [x] Keep `board_partition_id` as execution metadata only.
3. [x] Ensure multi-worker normalization matches the single-worker sequential path.
4. [x] Materialize terminal caches once per river public state and reuse them across
   tasks.

### Concrete code work

- `zeta\holdem\src\cli\solve_cli.h` derives the scheduler board count from the
  canonical runouts and reuses the prebuilt `runout_terminal_table` from every task.
- Evaluation uses the same chance table and terminal lookup regardless of worker
  count; the ordered reduction removes any cross-worker floating-point drift.
- Non-river solve correctness no longer depends on manually chosen disjoint combo
  fixtures.

### Exit criteria

- [x] Worker-count determinism holds on runout-aware solves.
- [x] Sequential and scheduled traversal agree on probabilities and EVs.

## Stage D - Replace CLI solve and extraction

### Objective

Make `solve_spot(...)` drive one native path for river, turn, and flop.

### Required implementation steps

1. River solve may still use the current vectorized fast path if it is exact,
   but turn/flop must use the native runout-aware graph.
2. Remove all non-river workaround logic:
   - choosing one compatible combo set
   - precomputing averaged fixed terminal utilities
   - external public-runout board averaging for EV extraction
3. Extract results from the solved graph/context, not from a separate fallback
   methodology.

### Concrete code work

- Rework `zeta\holdem\src\cli\solve_cli.h`.
- Keep `solve_vectorized_root_actor_river(...)` only for exact river optimization.
- Introduce native solve helpers for:
  - flop
  - turn
  - shared artifact extraction from solved tables

### Exit criteria

- `solve_spot(...)` has no production non-river shim path.
- Flop and turn spots solve through graph chance expansion.
- EV extraction for non-river spots comes from the solved runout-aware path.

## Stage E - Replace persistence and inspection surfaces

### Objective

Persist solved runout-aware structure directly and inspect it without rebuilding
fake trees from the original spot.

### Required implementation steps

1. Define the canonical persisted payload for:
   - public states
   - chance events
   - runouts
   - solved nodes
   - strategy/EV surfaces
2. Reject incompatible old schemas rather than preserving migration shims.
3. Make the UI explorer render chance/public-state/runout structure directly.

### Concrete code work

- Update:
  - `zeta\holdem\src\cli\solve_cli.h`
  - `zeta\holdem\src\cli\solve_cli.cpp`
  - `zeta\ui\holdem\src\solver\solution_store.h`
  - `zeta\ui\holdem\src\solver\solution_store.cpp`
  - `zeta\ui\holdem\src\document\document_json.h`
  - `zeta\ui\holdem\src\document\document_json.cpp`
  - `zeta\ui\holdem\src\widgets\strategy_explorer.cpp`
- Remove root-only reconstruction fallback from `solution_store`.

### Exit criteria

- Solution payloads preserve runout identity directly.
- UI/document parsing rejects incompatible old payloads cleanly.
- Explorer can inspect chance/public-state structure without reconstructing it
  from the spot.

## Stage F - Tests and validation

### Objective

Prove the implementation end to end and prevent regressions.

### Required test additions

#### Lowering and registry tests

- flop root lowers to:
  - one combined-runout chance event
  - one river public state per legal turn+river board completion
- turn root lowers to:
  - one combined-runout chance event
  - one river public state per legal river board
- canonical ancestry is preserved across all public states
- all legal boards appear exactly once
- all chance outcome probabilities sum to 1 per event (`public_game_lowering_runout_probabilities_form_valid_distribution`)
- full runout probabilities sum to 1 from the root (`public_game_lowering_runout_probabilities_form_valid_distribution`)

#### Solver tests

- non-river solve lowers to a graph containing the combined-runout chance node
- non-river solve never uses fixed-terminal averaging outside runout enumeration
- non-river solve never evaluates terminal values on non-river public states
- non-river EV matches explicit independent runout enumeration on small spots (`holdem_cli_runout_solve_matches_hand_computed_ev`)
- multi-way (N > 2) non-river solves return a clear unsupported-solver error

#### CLI tests

- flop solve succeeds through native runout-aware path
- turn solve succeeds through native runout-aware path
- removed alias fields stay rejected

#### Persistence/UI tests

- solution store round-trip includes runout-aware payload
- document round-trip includes runout-aware solution payload
- incompatible old schema versions are rejected
- explorer can navigate chance/public-state structure

### Final validation commands

Use the existing build targets and test binaries only:

#### Windows

```powershell
"C:\Program Files\JetBrains\CLion 2026.1\bin\cmake\win\x64\bin\cmake.exe" --build C:\Users\lewis\develop\zeta\cmake-build-visual-studio-release --target zeta-ui-holdem -j 14
```

#### Linux

```bash
/usr/local/bin/cmake --build /mnt/c/Users/lewis/develop/zeta/cmake-build-release-wsl-clang --target zeta-ui-holdem -j 14
```

Run the existing `zeta_tests` binary with focused selectors during iteration,
then broader holdem coverage once the lowering and native solve path are stable.

## Implementation notes

- Keep edits staged around the rollout order above; do not mix persistence
  redesign into lowering work.
- Prefer deleting incomplete branches over preserving them behind compatibility
  conditionals.
- Treat any root-only fallback, pre-averaged non-river terminal utility path, or
  schema reconstruction shim as a bug unless it is restricted to tests.

## Definition of done

The fix is complete only when all of the following are true:

1. `lower_public_game_tree(...)` produces a valid canonical combined-runout
   public-state graph for flop and turn roots.
2. Production flop/turn solves run through the native runout-aware path
   (chance-expanded lowering plus direct runout evaluation).
3. Persistence and UI consume the solved runout-aware structure directly.
4. Documentation describes the real architecture and contains no stale claims.
5. Targeted holdem tests pass, followed by the relevant broader build/test pass.
