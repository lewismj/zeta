# CFR Production Solver Plan

## Objective

Deliver the first production-quality Zeta Hold'em CFR solver. This plan is end-state only: no disposable mini-solvers, no alternate graph models, no placeholder terminal paths, and no APIs that are expected to be replaced later.

The production solver must use the existing Zeta surfaces:

- Immutable `game_graph` topology plus solver annotations.
- Infoset-major action table layout.
- Regret and strategy-sum tables.
- Worker-local delta buffers and deterministic reduction.
- Board/partition scheduler.
- Terminal-state records and terminal engine APIs.
- Checkpoint compatibility metadata.
- Zeta Hold'em validation and benchmark style.

Every task below should land production-shaped code. If a dependency is missing, implement the dependency in its final intended shape rather than adding a temporary adapter that hides the gap.

---

## Task 1: Replace placeholder traversal entry points with production HU/N-way CFR kernels

Helpers exist, but `cfr_engine<2>::traverse` and `cfr_engine<N>::traverse` still delegate to old/default traversal paths; `run_cfr_iteration` uses generic templated recursive logic, not a proper HU fast path plus N-way kernel split.

End state:

1. `run_cfr_iteration` dispatches through `cfr_engine<N>` rather than owning traversal math directly.
2. `cfr_engine<2>` is a real heads-up kernel with scalar OOP/IP reach state, direct opponent-reach formulas, and no avoidable generic N-way loops in hot CFR math.
3. `cfr_engine<N>` for `N >= 3` is a real N-way kernel with compile-time `N` reach state and explicit counterfactual reach products.
4. Both kernels use compact iterative DFS traversal frames with node ID, edge cursor, reach slot, value slot, and phase.
5. Reach and value state live in reusable side scratch, not inside every traversal frame.
6. Action values, node values, edge probabilities, regret deltas, and strategy deltas are preallocated before traversal.
7. No recursive CFR traversal remains on the production iteration path.
8. Kernel APIs consume production graph annotations, chance event tables, terminal providers, table views, iteration config, and worker-local scratch explicitly.
9. Regret writes occur only at updating-player infosets in alternating mode.
10. Average-strategy writes use `strategy_weight * chance_reach * own_reach(actor) * sigma[action]`.
11. CFR+ clipping remains outside traversal and happens only after deterministic reduction.
12. Diagnostics count real nodes, edges, action values, terminal evaluations, regret updates, strategy updates, chance outcomes, zero-reach skips, stack depth, and timing.

Acceptance:

- Existing CFR correctness fixtures pass through `cfr_engine<N>`.
- A heads-up fixture proves the HU kernel does not use generic N-way reach loops for opponent reach.
- A three-player fixture proves N-way counterfactual reach is correct.
- Stack and scratch capacity tests prove traversal does not allocate in hot loops.
- Old/default traversal may remain only as a legacy/reference helper, not as the production iteration path.

---

## Task 2: Replace scalar terminal placeholders with terminal-state evaluation in CFR

The production CFR leaf path must not be `terminal_utility_by_node` as the primary terminal interface.

End state:

1. Terminal graph leaves reference terminal-leaf metadata.
2. Terminal leaves resolve to `terminal_state<N>` records.
3. `terminal_engine<N>` evaluates each terminal state and returns `terminal_values<N>`.
4. CFR selects `utility[updating_player]` inside traversal.
5. Heads-up terminal evaluation keeps exact HU showdown/fold fast paths.
6. N-way terminal evaluation uses the N-way terminal API and carries folded/all-in/eligible/contribution/rake data.
7. Terminal providers are production interfaces, not placeholder scalar maps.
8. Tests may use a fixed terminal provider only as an explicit reference fixture, never as the production default.

Acceptance:

- Traversal terminal leaf values match direct terminal-engine calls.
- HU showdown and fold fixtures use real river terminal cache and reach indices.
- N-way fold/showdown fixtures validate player-indexed utility vectors.
- Invalid terminal-state references fail with typed errors.

---

## Task 3: Implement production betting-state generation and graph lowering

The solver needs real Hold'em graph generation, not hand-authored graph fixtures as the production source.

End state:

1. `betting_state<N>` models street, actor, stacks, committed amounts, folded flags, all-in flags, current bet, raise count, action history, pot layers, and terminal state.
2. State transitions for fold, check, call, bet, raise, and all-in are pure and deterministic.
3. Legal action generation is derived from the current betting state, stack sizes, action history, street, all-in state, and abstraction policy.
4. Action abstraction policies support fixed pot fractions, geometric sizes, street-specific sizing sets, stack-ratio buckets, and forced all-in thresholds.
5. Generated rich states lower to immutable `game_graph` plus solver side arrays.
6. Terminal betting states lower to `terminal_state<N>` records.
7. Lowering assigns actor, infoset ID, chance event ID, terminal leaf ID, street/state metadata, and action indices.
8. Graph generation validates shape, action ordering, terminal references, pot-layer invariants, infoset compatibility, and deterministic ordering.

Acceptance:

- Tiny generated river graphs replace hand-authored graphs for production-path CFR tests.
- Invalid betting states and illegal actions return typed validation errors.
- Generated graph hashes are deterministic for identical configs.

---

## Task 4: Implement production infoset identity and table planning

Infoset identity must be stable before large graph or chance expansion.

End state:

1. Hold'em infoset keys include acting player, street, private abstraction, public board abstraction, chance/runout abstraction, betting history abstraction, stack/pot abstraction, legal action set, player count, and subgame/root context.
2. Abstraction policies explicitly produce board and chance/runout abstraction IDs.
3. Infoset keys lower to dense IDs before table allocation.
4. Shared infosets validate identical actor, street, legal action IDs/order, abstraction IDs, player count, and owner.
5. Memory planning estimates nodes, edges, infosets, action values, regret bytes, strategy-sum bytes, owner-map bytes, worker scratch, delta buffers, terminal states, chance events, chance outcomes, river caches, and checkpoint bytes.
6. Planning fails before materialization when configured limits are exceeded.

Acceptance:

- Large graph construction cannot begin until memory limits pass.
- Infoset/action layout hashes are stable and checkpoint-compatible.
- Shared-infoset collisions fail with actionable diagnostics.

---

## Task 5: Implement enumerated chance expansion and scheduler integration

Chance must be explicit, deterministic, and compatible with checkpoints and scheduling.

End state:

1. Chance events represent flop, turn, and river public-card events.
2. Enumerated chance outcomes include cards, probability, child node, board partition ID, and legality metadata.
3. Chance traversal uses outcome probabilities, never uniform child placeholders.
4. Chance validation checks outcome count, graph child alignment, probability sums, board-card uniqueness, and dead-card collisions.
5. Sampling remains disabled until an explicit checkpoint-compatible RNG stream policy exists.
6. Chance outcomes map deterministically to board partitions.
7. The board/partition scheduler consumes generated chance partition metadata rather than using separate benchmark-only task wiring.

Acceptance:

- Flop/turn/river outcome counts are correct under blockers.
- Chance probability mass is one for each legal chance node.
- Board partition scheduling is deterministic across worker counts.

---

## Task 6: Implement production multi-worker scheduled iteration

The production iteration must use scheduler partitions and all supplied workers, not just `workers.front()`.

End state:

1. Iteration builds a deterministic work plan from graph partitions and board partitions.
2. Workers traverse assigned partitions using worker-local scratch and delta buffers.
3. No worker writes directly to global regret or strategy tables during traversal.
4. Reduction merges all worker-local raw deltas deterministically.
5. Owner-routed reduction routes sparse deltas by infoset owner range.
6. CFR+ clipping runs once after all raw regret deltas have merged.
7. Worker-count determinism is preserved under the selected numeric/reduction policy.
8. Diagnostics report per-worker traversal time, reduction time, remote delta volume, owner hit distribution, and scheduler task counts.

Acceptance:

- Equivalent iterations with 1, 2, and N workers produce identical tables under deterministic policy.
- Owner-routed reduction reports remote deltas and owner hit distributions.
- Scheduler failures return typed task/partition context.

---

## Task 7: Complete terminal and side-pot production semantics

Terminal APIs already carry production-shaped fields, but betting, lowering, and evaluation must use them end to end.

End state:

1. Pot layers represent amount, eligible mask, and contributors mask.
2. Betting-state transitions maintain pot layers through all-ins and folds.
3. Terminal states carry folded seats, all-in eligibility, active eligibility, contributions, rake, and terminal kind.
4. HU keeps exact optimized terminal paths.
5. N-way terminal evaluation handles fold and showdown with side-pot-aware accounting.
6. Rake-adjusted and variant-specific terminal kinds have explicit typed payload boundaries.

Acceptance:

- Hand-audited all-in/side-pot fixtures balance total contributions and utility sums.
- Folded players cannot win ineligible pots.
- HU fast path remains covered by exact terminal tests.

---

## Task 8: Implement production checkpoint and resume

Checkpointing must resume real solving safely, not only round-trip table vectors in memory.

End state:

1. Checkpoints include format version, endianness, player count, solver config hash, graph/config metadata hash, infoset/action/table layout hash, numeric policy, reduction policy, chance mode, owner ranges, terminal-state layout hash, iteration number, and CFR variant state.
2. Regret and strategy-sum tables save with their storage encoding.
3. Checkpoint chunks align with infoset owner ranges.
4. Resume rejects incompatible graph shape, graph metadata, infoset count, action offsets, player count, terminal-state layout, CFR variant, precision/layout/reduction policy, chance mode, owner ranges, or RNG policy.
5. Resume restores the solver to a state that produces the same next-iteration result as uninterrupted solving.

Acceptance:

- Save/load/resume works over generated Hold'em graphs.
- Incompatible metadata cases fail with typed errors.
- Resume equivalence is tested against uninterrupted solving.

---

## Task 9: Add production reference validation and convergence gates

Correctness must be proven against Zeta production paths, not standalone games.

End state:

1. A slow reference traversal uses the same `game_graph`, annotations, terminal-state table, terminal engine, action layout, regret table, strategy table, and chance table.
2. Reference traversal compares action values, node values, regret deltas, strategy deltas, terminal values, chance probabilities, and reach products.
3. Tiny generated Hold'em subgames have known or hand-audited expected behavior.
4. Known-equilibrium convergence tests exist for tiny generated games where exact validation is feasible.
5. Exploitability/best-response APIs consume the same terminal-state and utility-vector interface as CFR.

Acceptance:

- Production CFR iteration matches reference traversal on tiny generated graphs.
- Per-infoset diagnostics identify the action range responsible for max regret or largest strategy movement.
- Convergence gates fail if strategy quality regresses.

---

## Task 10: Add production observability and benchmarks

Benchmarks must measure real solver work on production structures.

End state:

1. Iteration diagnostics report nodes visited, edges scanned, player nodes, chance nodes, terminal nodes, terminal evaluations, regret updates, strategy updates, chance outcomes, reduction entries, reduction values, clipped values, scheduler tasks, stack depth, max action count, zero-reach skips, and phase timings.
2. Memory diagnostics report nodes, edges, infosets, action values, chance events, chance outcomes, terminal leaves, terminal states, table bytes, owner-map bytes, worker scratch, delta buffers, river cache/workspace, and checkpoint estimates.
3. Quality diagnostics report regret norm, max regret, max-regret infoset/action, mean regret, positive regret count, strategy mass by player, strategy movement, entropy changes, convergence metrics, and exploitability when available.
4. Benchmark tiers cover hand-audited graph fixtures, tiny generated river graphs, small generated river abstractions, board batches through the scheduler, large table memory stress, and turn/flop chance expansion.
5. Benchmark output distinguishes traversal, terminal evaluation, chance, scheduler, reduction, checkpointing, memory, and quality.

Acceptance:

- Benchmarks exercise generated Zeta Hold'em structures.
- Benchmark counters prove real regret updates, strategy updates, terminal evaluations, reductions, and memory use.
- No benchmark tier relies on unrelated standalone games.

---

## Production release gate

The first production-quality solver is releasable only when:

1. The production iteration path uses real HU/N-way kernels.
2. Terminal leaves evaluate through terminal-state records and terminal engines.
3. Generated Hold'em graphs are the primary solver input.
4. Enumerated chance is deterministic and scheduler-integrated.
5. Multi-worker scheduled traversal and deterministic reduction are wired end to end.
6. Checkpoint resume is compatible and equivalent to uninterrupted solving.
7. Reference traversal validates CFR math over production graph/table/terminal/chance surfaces.
8. Known tiny generated games pass convergence gates.
9. Observability exposes correctness, quality, memory, and scaling risks.
10. No production path depends on placeholder scalar terminal utilities, recursive traversal, benchmark-only graph generation, or standalone-game scaffolding.
