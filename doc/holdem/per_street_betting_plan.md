# Per-Street Betting Expansion for Flop/Turn Solves - Implementation Plan

This plan implements roadmap item 1 of `next_steps.md`: replace the interim
runout-average rollout for non-river spots with a real, game-theoretic flop/turn
solve that interleaves a betting round on every street. It also delivers the
scalability machinery the roadmap defers (up-front memory estimation, card
isomorphism reduction, and optional dynamic action pruning) so that full postflop
multi-street solving is tractable and never silently exhausts memory.

The work is split into four reviewable steps. Correctness of the fundamental game
tree is established first, in isolation from the scalability optimisations, so
that memory reduction and pruning can never obscure a tree/CFR bug. Each step
builds on the previous one and leaves the tree in a buildable, test-passing state.

## Background: what exists today

- `lower_public_game_tree` (`cfr/betting/betting.h`) lowers a flop/turn spot to a
  single scaffold: `root player -> one chance node -> one river terminal per
  complete runout`. There is no per-street betting; the chance node deals the
  whole board to the river in one step.
- `solve_public_game_runout` (`cli/solve_cli.h`) evaluates that scaffold by
  averaging blocker-aware showdown EV across runouts. It runs no CFR, so
  `iterations` is inert and the artifact algorithm is `"runout-enumeration"`.
- `lower_betting_tree_to_graph` already lowers a *single-street* (river) betting
  round into a full CFR graph with infosets, terminals, and action layout.
- The CFR traversal kernel (`cfr/solver/iteration.h`) and the vectorized
  combo-CFR kernel (`cli/solve_cli.h`) both already traverse chance nodes
  generically via `chance_event_table::probability_for_edge`.
- `chance/chance.h` provides blocker-aware public-card enumeration, public-state
  / chance-event / runout registries, and per-river terminal cache
  materialization (`make_runout_terminal_table`).
- Memory estimation exists (`estimate_cfr_memory`, `cfr_memory_plan_limits`,
  `cfr_memory_shape`, `cfr_memory_estimate`) but is only wired into the
  single-street river lowering, and only *after* a graph is built.

The strategic content today lives entirely in the terminal showdown. The goal is
to move it into a real multi-street betting tree while keeping the vectorized
per-river terminal evaluation as a leaf optimization, not a separate solve path.

## Guiding architectural principle

The vectorized/templated river terminal evaluation is a *leaf optimization inside*
the unified solve, not a separate methodology. The multi-street solver is:

```
CFR
 |-- flop betting
 |-- chance (deal turn)
 |-- turn betting
 |-- chance (deal river)
 \-- river betting
      \-- vectorised river terminal evaluation (existing, unchanged)
```

It is never a generic terminal evaluator that rebuilds river equity from general
graph state. The expensive hand evaluation stays outside the CFR hot loop.

## Step 1: Exact multi-street public-game lowering (correctness first)

**Status: Done.** The exact multi-street lowering (`lower_multi_street_public_game`)
interleaves per-street betting with chance deals, enforces the street-boundary
invariants, rejects multi-way non-river lowering, and ships the inactive
card-isomorphism API plus the cheap `estimate_multi_street_public_game_shape`
pre-build path. Covered by the structural tests in `test_cfr_graph.cpp`.

Goal: produce one lowered public game that interleaves betting and chance, using
*exact* (non-collapsed) card enumeration only. No isomorphism reduction and no
pruning are active in this step.

Structural invariants (made explicit and tested):

```
non-river public state:
    -> betting subtree for the current street
        -> fold terminal                                 (hand ends)
        -> continue-to-next-street chance node
            -> next public state (turn, then river)

river public state:
    -> betting subtree
        -> fold terminal
        -> showdown terminal
```

- A chance node never occurs in the middle of a betting round.
- A betting node never crosses a street boundary.
- Each legal board completion produces exactly one river public state / runout.

Deliverables:

- Replace `lower_public_game_tree` with a recursive lowering that, at each public
  state, expands the street's betting round via the existing
  `legal_betting_actions` / `apply_betting_action` state machine, then attaches a
  chance node at each non-terminal end-of-round state that advances the board
  (flop->turn, turn->river). River end-of-round showdown states become showdown
  terminals; fold can occur on any street and ends the hand immediately.
- Preserve exact public-state / chance-event / runout identity by reusing the
  existing `public_state_registry`, `chance_event_table`, and `runout_registry`
  types and their validators. Every reachable river public state is registered
  once; each chance node's outcomes align to its graph child edges; each legal
  board completion is recorded once.
- Build infoset lowering, actor annotations, terminal leaves, and per-node rich
  state metadata for the whole multi-street graph. Street is taken from each
  node's public state, not from a single spot street.
- Reject multi-way (N > 2) non-river lowering with a clear
  `betting_validation_error` that the CLI maps to an explicit
  unsupported-solver message. Contract: river = HU + multiway; turn/flop = HU only.
- **Card-isomorphism API only (inactive).** Add the canonicalisation type and
  entry point next to `enumerate_public_card_outcomes`, but keep exact enumeration
  as the only active lowering path. The API is defined so Step 3 can enable it
  without reshaping the lowering. The isomorphism representation is explicit and
  captures the full mapping, not just a weighted edge:

  ```
  canonical_public_outcome {
      representative public-card set
      probability weight (sum of collapsed members)
      public-card permutation (board suits -> canonical suits)
      private-combo permutation (induced relabeling of hole-card combos)
  }
  ```

  so that a private combo such as `AsKs` in a collapsed outcome maps to the
  correct transformed combo in the representative outcome. This step ships the
  type and an explicit identity implementation (one representative per outcome, no
  collapse); Step 3 supplies the real reduction.
- **Cheap shape-estimation path (pre-build).** Add a function that estimates the
  multi-street graph shape (node / edge / infoset / chance-outcome / terminal /
  runout counts) from the spot + betting policy + ranges *without materialising
  the full graph*. This is the prerequisite the budget check in Step 3 consumes,
  and it is introduced here so large flop trees are never built merely to discover
  they are too large.

Tests (unit): structural assertions that a small turn and a small flop lowering
produce exactly the legal set of streets, betting nodes, chance nodes, terminals,
and runouts (each legal board once), and that the street-boundary invariants hold;
multi-way non-river lowering is rejected; the shape estimator matches the actual
built graph counts for small spots.

## Step 2: Unified heads-up CFR+ solve over the exact multi-street game

**Status: Done.** `solve_multi_street_public_game` replaces the deleted
`solve_public_game_runout` / `"runout-enumeration"` path (no shim): a single CFR+
loop (regret-matching+, uniform averaging, alternating updating player,
`algorithm = "cfr+"`) traverses flop/turn/river, weighting chance children by
outcome probability and dispatching showdown leaves to the per-river terminal
cache keyed by river public-state id. The value backup is street-general, so the
turn solve exercises the identical chance-backup code a flop solve uses. Covered
by the multi-street CLI tests in `test_holdem_cli.cpp`
(`holdem_cli_multi_street_*`), including a hand-computed golden nuts EV
(`= 50.0`), an `iterations`-sensitivity convergence test, worker-count
determinism, schema-3 payload persistence, and a restricted-deck flop solve that
drives both chance layers end to end. Note: a *full-deck* flop solve remains
intractable until the Step 3 memory/isomorphism controls land; the restricted
deck keeps the flop test tree tiny while still validating the two-chance-layer
backup.

Goal: one CFR solve path for river, turn, and flop, run over the exact tree from
Step 1.

Algorithm contract (explicit, not just a metadata label):

- `algorithm = "cfr+"`, `cfr_variant::cfr_plus`.
- Regret update: regret-matching+ (regrets floored at 0 each update), identical to
  the existing river vectorized solver (`std::max(0.0f, regret + (child - node))`).
- Averaging: uniform strategy weight (`strategy_weight = 1.0`), same as the river
  path; no linear/weighted averaging is introduced here.
- Alternation: alternating updating player per iteration (`updating_player` loop),
  unchanged from the river path.
- Reuse the existing CFR+ update/averaging semantics unchanged; the multi-street
  path must not silently diverge from the river solver's algorithm identity, which
  the persisted artifact and convergence comparisons depend on.

Terminal-cache contract (explicit key):

```
terminal leaf
    -> river public_state_id (from node metadata)
    -> per-river terminal cache (make_runout_terminal_table)
    -> combo x value lookup
```

Terminal evaluation looks up the cache by the node's river public state id; it
never infers the board from general graph state.

Deliverables:

- Extend the vectorized combo-CFR kernel (`build_node_reach_vectors`,
  `evaluate_combo_profile`, `update_combo_cfr_tables`,
  `normalize_combo_action_table`) to operate on the multi-street public graph.
  Chance nodes weight child values by outcome probability (already supported by
  reach/value propagation); terminal nodes dispatch to the per-river cache keyed as
  above.
- Regret and average-strategy accumulation across every street so `iterations`
  drives convergence for flop/turn spots. Root and per-node strategies are
  extracted from the average strategy exactly as the river path does today.
- Delete `solve_public_game_runout` and the `"runout-enumeration"` algorithm.
  Route all non-river heads-up spots through the unified solve; non-river
  artifacts report `"cfr+"`. No backward-compatible shim is kept.
- Keep exact per-runout terminal cache reuse and deterministic multi-worker board
  partitioning for terminal evaluation.

Tests (unit), in three explicit levels:

1. Structural correctness: exact street sequence, node counts, chance outcomes,
   terminal counts, and runout identity for a small turn and small flop spot
   (extends Step 1's structural tests to the solved graph).
2. Numerical correctness: converge a deliberately tiny abstraction (one bet size,
   low `max_raises`, small ranges) whose equilibrium is independently verifiable,
   and assert betting frequency and per-hand EV within tolerance. The independent
   reference is a tiny hand-solved game, not "looks like an equilibrium".
3. Regression correctness / `iterations` sensitivity: assert non-river output
   changes with `iterations` (retiring the "iterations is inert" characterization)
   and that strategy/EV/regret are stable across worker counts and across runs
   within tolerance.

## Step 3: Pre-build memory budgeting and exact card isomorphism

Goal: make full postflop multi-street solving safe up front, and activate the
exact suit-isomorphism reduction. The pipeline becomes:

```
spot + betting policy + ranges
      -> estimate graph shape (Step 1 cheap path)
      -> estimate CFR memory (with breakdown)
      -> budget check
      -> allocate / build   (only if within budget)
```

Deliverables:

- Wire the Step 1 shape estimator into a full memory estimate that runs *before*
  any large allocation or graph materialisation. Extend `estimate_cfr_memory`
  usage to the multi-street footprint.
- Return a **breakdown**, not just a total, so the CLI error is actionable. The
  estimate accounts for every component and records which dimension dominates:

  ```
  graph_bytes            (nodes, edges, node/edge metadata)  ~ nodes
  infoset_bytes          (infoset metadata, action layout)   ~ infosets
  regret_bytes           ~ infosets x combos x actions x sizeof(regret)
  strategy_sum_bytes     ~ infosets x combos x actions x sizeof(strategy)
  working_bytes          ~ infosets x combos x actions x sizeof(strategy)
  reach_bytes            per-node reach vectors              ~ nodes x combos
  value_bytes            per-node value vectors              ~ nodes x combos
  terminal_cache_bytes   per-river caches                    ~ river public states
  chance_bytes           chance events + outcomes            ~ chance outcomes
  worker_bytes           worker-local scratch / deltas       ~ workers x ...
  extraction_bytes       artifact assembly buffers
  total_bytes
  ```

  The regret / strategy-sum / working tables are dimensioned by the number of
  *actions* at each infoset (not a generic value/payoff count): each is
  `infosets x combos x actions` entries of the respective element size.

- A configurable memory budget on `solve_runtime_options`, defaulting to a value
  derived from detected available system memory. If the estimate exceeds the
  budget, return a clear `cli_error` that names the dominating dimension(s) and
  suggests concrete changes: fewer bet sizes, lower `max_raises` per street,
  enable card isomorphism, restrict to turn, or reduce ranges. Nothing past the
  budget is allocated.
- **Activate exact card isomorphism** (the Step 1 API): collapse suit-isomorphic
  turn/river outcomes into their canonical representative with summed probability
  weight and the recorded public-card / private-combo permutations, applied in the
  lowering and consumed correctly by reach propagation and terminal evaluation.
  For suit-symmetric ranges this is lossless; the reduction is proven exact
  against the Step 2 exact solve for a symmetric spot. When ranges are not
  suit-symmetric, isomorphism is an explicit, documented, opt-in lossy option
  (default off).

  **Operational definition of suit symmetry.** The lossless guarantee requires
  that combo membership and weight be invariant specifically under the suit
  permutations the canonicalizer actually applies to collapse outcomes - not the
  stronger property of invariance under arbitrary suit permutations. The validator
  checks this weaker, permutation-scoped property: for each collapse the
  canonicalizer performs, every private combo's weight is preserved under the
  induced private-combo permutation. A range that is symmetric only under the used
  permutations still qualifies for the lossless path; a `range_is_suit_symmetric()`
  style check against arbitrary permutations would be both too strict and the wrong
  property.

Tests (unit): estimator matches actual allocation within tolerance for a known
small tree, and the breakdown attributes the dominant term correctly; an
over-budget spot returns the actionable error and allocates nothing;
isomorphism-enabled solve matches the exact solve within tolerance for a
suit-symmetric spot.

## Step 4: Optional dynamic pruning, surfacing, docs, tests, benchmarks

Goal: add the least mathematically transparent optimisation last, behind an
explicit approximate contract, then finish surfacing, documentation, tests, and
benchmarks.

Dynamic action pruning is defined as an **explicitly approximate solver policy**,
not a transparent optimisation, with a precise contract:

- `active action set` per infoset, `prune_threshold`, `minimum_active_actions`,
  and a `reactivation` policy.
- Pruning is threshold-based on reach-weighted regret contribution; at least
  `minimum_active_actions` (>= 1) always remain active; pruning preserves the
  original action indexing so persisted strategies stay aligned.
- Reactivation semantics are explicit: a pruned action's regret state is retained
  (never reset) while inactive, and its child is not traversed while inactive.
  Pruned actions are periodically reconsidered from the parent/infoset state
  (positive regret of the still-active actions and the infoset's reach), and an
  action is reactivated - resuming traversal of its child - when that
  reconsideration shows it would re-cross the threshold. Reactivation therefore
  never requires traversing the inactive child to detect it.
- Pruning affects which children are traversed, but regret and average-strategy
  accumulation semantics for active actions are unchanged.
- Default off. When off, the solver is exactly the Step 2/3 solver.

Deliverables:

- Implement dynamic pruning as an opt-in policy exposed in the betting/solve
  policy, CLI spot JSON, and UI solver controls, alongside the isomorphism and
  memory-budget knobs.
- Persist the multi-street solved graph via the existing schema-3 payload
  (`append_solved_graph_payload`): public states, chance events, runouts, and
  per-node strategies. **Compatibility check:** confirm schema 3 already carries
  street, chance node, public state, chance event, runout, betting history, and
  per-node strategy semantics. If it does, reuse it unchanged; if a field is
  missing, extend schema 3 explicitly rather than silently redefining its meaning.
- UI rollout handling: remove any special-casing that presented non-river solves
  as a rollout / `"runout-enumeration"` methodology. The strategy explorer and
  study workflow now show `"cfr+"` metadata and real per-node strategies for
  turn/flop; a completed flop/turn solve reads as a solve, not an equity rollout.
- Documentation (no mojibake): update `core_algorithms.md` (multi-street lowering
  and unified solve), `betting_tree_config_plan.md` (per-street rounds, chance
  interleave, isomorphism, memory budget, pruning), `cli_usage.md` (new policy /
  runtime knobs and the over-budget error), `ui/user_guide.md` (flop/turn now
  produce real solves and how memory guidance appears), and `next_steps.md` (mark
  item 1 delivered and remove the interim-rollout description).
- Benchmarks (new deliverable, `benchmark/holdem`): for flop/small abstraction,
  turn/small abstraction, and flop/realistic abstraction, record graph nodes,
  edges, infosets, chance outcomes, terminal leaves, estimated memory, actual
  memory, build time, iterations/s, and terminal evaluations/s.
- Tests: UI tests asserting the completed non-river solve surfaces real solve
  metadata (rollout handling retired); for a defined benchmark suite and threshold
  range, the pruned solve stays within the configured strategy-distance / EV
  tolerance of the unpruned solve (the claim is bounded-to-tolerance, not
  "identical converged strategy"). Run the full `zeta-ui-holdem` build and the
  entire test suite.

## Build and validation

Windows:

```
"C:\Program Files\JetBrains\CLion 2026.1\bin\cmake\win\x64\bin\cmake.exe" --build C:\Users\lewis\develop\zeta\cmake-build-visual-studio-release --target zeta-ui-holdem -j 14
```

Linux:

```
/usr/local/bin/cmake --build /mnt/c/Users/lewis/develop/zeta/cmake-build-release-wsl-clang --target zeta-ui-holdem -j 14
```

Each step ends green: the target builds and the full test suite passes before the
next step begins.

## Constraints

- No backward-compatible shims; there is no released version to preserve.
- No placeholder logic; each step is fully implemented before it is considered
  done. (The Step 1 isomorphism API ships an explicit identity implementation, not
  a hidden TODO; Step 3 replaces it with the real reduction.)
- Code and docs contain no mojibake.
- Coding style follows current conventions in the touched files.

## Summary of review adjustments incorporated

- Active card isomorphism moved out of Step 1 (API + identity impl only); the real
  reduction is enabled in Step 3, after exact-tree correctness is established.
- A cheap pre-build graph-shape estimator is introduced in Step 1 so the memory
  budget check in Step 3 runs before the tree is materialised, not after.
- Dynamic pruning is deferred to Step 4 and defined as an explicitly approximate
  policy with active-set, threshold, minimum-active, and reactivation semantics;
  its test claim is bounded-to-tolerance, not "identical converged strategy".
- The CFR+ algorithm contract and the terminal-cache key are stated explicitly.
- The memory estimate returns a per-component breakdown for actionable errors.
- Convergence testing is specified at three levels (structural, numerical vs an
  independent tiny reference, regression) instead of "looks like an equilibrium".
- A benchmark deliverable and a schema-3 compatibility check are added.
