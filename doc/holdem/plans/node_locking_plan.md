# Node Locking and Strategy Constraints Plan

This plan defines a complete, implementation-level delivery for node locking in
the Zeta hold'em solver. Node locking lets a user constrain the strategy (action
probabilities) of a seat at selected decision points -- across a whole range or
for specific combos -- and re-solve so the free (unconstrained) part of the tree
best-responds. This is the machinery behind exploitative questions such as "what
if BTN never raises this node?" or "solve OOP's response to this population
tendency".

Everything here is specified for the current release. There is no deferred
"future mode": the constraint primitive is **partial action constraints**
(constrain some actions, leave the rest free), which subsumes both
full-distribution locks and single-action pins. Soft/penalty constraints are the
only explicit non-goal, and they are a separate feature, not a half-built hook in
this one.

The plan is written against the real solver internals; each phase names the
concrete types, files, and functions it touches:

- `zeta::holdem::hand_range` (`range.h`): `std::array<combo_weight, 1326>`; combos
  are addressed by `combination_index` (`uint16_t`, `combination_count == 1326`,
  `board.h`).
- `strategy_context_id` and `combo_local_index` (`cfr/extraction/contract.h`): a
  strategy context is `public_state + betting_history + acting_seat`; a node's
  combos are a dense local slice `0..combo_count-1`, each mapping to a canonical
  `combination_index`.
- `cfr::game_graph` (`cfr/graph/graph.h`): immutable CSR tree; player nodes carry
  `infoset_id`; actions are contiguous `action_index` `0..degree-1`.
- Two solve regimes exist and this plan hooks both (see "Solver architecture and
  regret-state granularity" below):
  - **Per-combo vectorized heads-up** -- `combo_action_table` (`cli/solve_cli.h`),
    a `[combo][infoset][action]` flat store, driven by `normalize_combo_action_table`
    (regret-matching -> current strategy) and `update_combo_tables`
    (per-combo regret/strategy-sum update). Used by `solve_multi_street_public_game`
    (flop/turn/river HU) and `solve_vectorized_root_actor_river` (river root-actor
    HU). Strategy, regret, and extraction are **already per-combo** here.
  - **Scalar shared-strategy fallback** -- `cfr::regret_table` /
    `cfr::strategy_sum_table` (`cfr/tables/*.h`, infoset-major flat), driven by
    `compute_regret_matching_strategy<StrategyPolicy>` and the CFR value/regret
    backup (`child_action_value`, delta buffer) in `cfr/solver/iteration.h` /
    `cfr/traversal/traversal.h`, run via `run_cfr_iteration`. One strategy per
    infoset, **shared across all combos**. Used for multiway (`N > 2`) and any
    abstraction that does not reach the vectorized HU paths.
- Extraction: the vectorized HU paths emit genuinely per-combo surfaces
  (`average_strategy.value(combo, infoset_id, action)` written into per-combo
  `action_strategy`). The scalar path uses `normalized_average_strategy`
  (`cfr/extraction/contract.h`) + `write_strategy_surface`
  (`cfr/extraction/strategy_surface.h`), which replicate one infoset vector to
  every combo in `result_store`.
- `solve_hashes`, `solver_metadata`, `solved_node`, `solve_runtime_options`,
  `convergence_options`, `current_artifact_schema_version` (currently `4`) in
  `cli/solve_cli.h`; spot/runtime JSON parsing (boost::json) in `cli/solve_cli.cpp`;
  FNV-1a `compatibility_hasher` in `cfr/solver/metadata.h`.
- `solver_graph_annotations` / `validate_solver_graph_view` (`cfr/solver/metadata.h`)
  and graph `validation.cpp/.h` for the structural-validation precedent.

## Goals

- constrain one or more legal actions at selected nodes to fixed probabilities,
  leaving the remaining actions free to be solved
- support range-scoped constraints (applied to all live combos of the acting seat
  at the context) and combo-scoped constraints (per `combination_index`)
- validate every constraint against the lowered tree and the seat's live combo
  domain before any iteration runs
- enforce constraints as hard constraints inside a mathematically correct CFR+
  update so regret minimization operates only over the free sub-simplex, and the
  rest of the tree best-responds
- keep the no-lock path's **solver state and numeric output** (CFR state, strategy,
  EV, hashes) byte-identical to the current solver and within its performance
  budget; the schema-5 artifact is **semantically identical** to the schema-4
  artifact for a no-lock solve (the serialized structure may differ only by
  additive schema/metadata fields, never by any solve-affecting value)
- record lock provenance in artifacts (new `locks_hash`, folded into `solve_hash`)
  and annotate constrained nodes in `solved_node`
- expose a stable constraint schema through the spot JSON consumed by `solve_cli`

## Non-goals

- soft / penalty / "prefer" constraints (a distinct future feature with its own
  schema and math; not scaffolded here)
- constraining chance (board) frequencies or terminal payoffs
- combo-scoped constraints under card isomorphism when they cannot be represented
  exactly (such locks are rejected -- see Phase 2/4 -- rather than silently
  broadened)

UI integration for the Qt hold'em app **is** in scope and specified in Phase 7:
the desktop app is a first-class solver driver (`ui/holdem/src/solver/solver_session.cpp`
calls `cli::solve_spot` directly and persists `solve_spot`/`solve_artifact`), so
node locking is not complete until the app can create, run, inspect, and persist
locks.

## Identity model (authoritative)

The result-surface architecture deliberately separates three distinct identities --
strategic identity, concrete node evaluation, and combo identity -- and node
locking touches exactly those boundaries. They are named explicitly here and used
consistently for the rest of this document:

- **`strategy_context_id`** = `public_state + betting_history + acting_seat`. It
  identifies the *public* decision context only. The private holding is **not**
  part of `strategy_context_id`.
- **strategy state** = `strategy_context_id x private combo x action`. This is the
  object a lock ultimately constrains.
- **concrete node** = `node_id + strategy_context_id + combo domain +
  reach/Q/V/equity/...`. A single `strategy_context_id` may materialize as several
  concrete nodes.

> **Strategic identity invariant.** `strategy_context_id` identifies the public
> decision context. Private holding is an explicit strategy dimension, not part of
> `strategy_context_id`. A vectorized/refined state therefore owns strategy at
> `(strategy_context_id, combo, action)`. A scalar state intentionally *collapses*
> the combo dimension into `(strategy_context_id, action)` until selective
> refinement is requested.

The scalar regime's single per-infoset vector is therefore a **range-level/shared
strategy abstraction**, not a per-hand information set: the underlying poker
information sets are private-hand specific, and the scalar solver applies a shared
strategy abstraction over them. Where this document says an infoset vector is
"shared across all combos", it means this abstraction -- not that the combos are
genuinely one information set.

## Core semantics (authoritative definitions)

Terminology: *node locking* is the user-facing feature name. Internally a "lock"
is a set of **hard action-probability constraints**; the mathematical primitive is
a per-action fixed probability with the remaining actions free. Public/artifact
identifiers (`lock_set`, `lock_entry`, `resolved_lock`, `lock_state`, `locks_hash`)
keep the user-facing name; the internal constraint primitive is named
`action_constraint*`. Both refer to the same thing.

Let a constrained player node have legal actions partitioned into locked set `L`
and free set `F`:

- each locked action `a in L` has a fixed probability `p_a >= 0`
- `S = sum_{a in L} p_a`, with the validation invariant `0 <= S <= 1`
- residual free mass `r = 1 - S`
- validity requires `F` non-empty OR `S == 1` (i.e. no residual mass left
  unassignable); `r == 0` with `F` non-empty is permitted and forces every free
  action to probability 0 (and, per the edge cases below, performs no free regret
  update because the feasible set has collapsed to a point)

Behavioral strategy actually played during traversal:

```
sigma_a = p_a            for a in L          (fixed)
sigma_a = r * rm_a       for a in F          (rm_a = regret matching over F only)
```

where `rm_a` is regret matching **restricted to the free sub-simplex**: it uses
only the free actions' regrets and sums to 1 over `F` (uniform over `F` when the
free positive-regret mass is 0). The residual scalar `r` is applied only when
forming `sigma`, never inside `rm`.

This `sigma` is the single behavioral strategy used for everything: reach
propagation to children, node-value backup to the parent, and strategy-sum
accumulation. Consequences:

- The value propagated upward is the full behavioral value
  `V_node = sum_{a in L} p_a * V_a + sum_{a in F} (r * rm_a) * V_a`, so the
  opponent best-responds to the constrained behavior.
- Because `sigma` is accumulated every iteration, the extracted average strategy
  naturally converges to `p_a` on locked actions with **no special extraction
  path**.

Regret update (the mathematically critical part): for `r > 0`, regrets are updated
**only for free actions**, with the comparator restricted to the free sub-simplex
and scaled by the residual `r` so the stored values are the literal counterfactual
regrets of the restricted game:

```
V_free = sum_{a in F} rm_a * V_a            (rm_a normalized over F, sums to 1)
R_a   += cf_reach * r * (V_a - V_free)      for a in F, when r > 0
# locked actions' regrets are never updated
# CFR+ clipping R_a = max(R_a, 0) applies to free actions only
```

The residual factor `r` is included deliberately: deviating the free
sub-strategy from `rm` to a pure free action changes the *complete* behavioral
strategy by only the residual mass `r`, so the restricted-game deviation advantage
is `r * (V_a - V_free)`. Zeta **stores the literal restricted-game regret, so the
residual factor is mandatory.** Omitting it would produce a scaled representation
that is strategy-equivalent under ordinary regret matching + CFR+ clipping, but is
not the defined Zeta regret state and is undesirable for inspectability, reference
testing, and future regret-state consumers.

The free-simplex comparator `V_free` (not the full node value `V_node`) is what
prevents the accumulator from "learning" to move probability off a locked action
-- a move it is mathematically forbidden to make. `cf_reach` is the usual
counterfactual reach (opponent reach product x chance), identical to the unlocked
update.

Edge cases, all defined:

- `F` empty and `S == 1`: `sigma` fully determined; no regret update; still
  accumulate into strategy sums.
- `r == 0` (whether or not `F` is non-empty): the strategy is fully determined by
  the constraints and the feasible set has collapsed to a point, so there is
  nothing to optimize. Every free action has behavioral probability 0, and **no
  free regret update is performed**. Strategy sums are still accumulated.
  Concretely at `r == 0`: strategy = the lock; free regrets are **frozen**; locked
  regrets are **frozen**; strategy sums receive `sigma`. Existing free regrets are
  **not zeroed** when the lock becomes active -- the lock is a solve-time
  constraint, and runtime state starts from whatever initialization the solve
  uses.
- `0 < r < 1`, free positive-regret mass 0: `rm_a` is uniform over `F` (matching
  the existing `compute_regret_matching_strategy` uniform fallback, confined to
  `F`).

## Solver architecture and regret-state granularity (authoritative)

Per-hand (combo-scoped) locking requires that the acting seat can hold a
**different strategy per private combo** at a constrained infoset. Whether that is
free or expensive depends entirely on how CFR state is stored, which differs
between Zeta's two solve regimes. This section is the authoritative statement of
that granularity; Phase 2 enforces it as a validation invariant and Phase 3
implements against it.

### Verified granularity of the current solver

- **Per-combo vectorized heads-up regime (primary target).** The heads-up
  multi-street solve (`solve_multi_street_public_game`) and the river root-actor
  solve (`solve_vectorized_root_actor_river`) store regrets and strategy sums in
  `combo_action_table`, indexed `value(combo, infoset_id, action_index)` over a
  `combination_count`-wide combo stride (`cli/solve_cli.h`). The strategy is
  materialized per combo in `normalize_combo_action_table`; regrets and
  strategy-sums are updated per combo in `update_combo_tables`
  (`regrets.value(combo, infoset_id, a) += ...`); and extraction writes a distinct
  `action_strategy` vector per combo (`average_strategy.value(combo, infoset_id,
  a)`). **Two different hands at the same betting infoset already carry
  independent strategies.** No new per-combo machinery is needed for per-hand
  locks here.
- **Scalar shared-strategy regime (fallback).** `run_cfr_iteration`
  (`cfr/solver/iteration.h`, `cfr/traversal/traversal.h`) reads one
  `infoset_regrets(infoset_id)` span per infoset and materializes **one** strategy
  shared by every combo; `write_strategy_surface` then replicates that single
  vector to all combos. The betting graph confirms this: `set_infoset_id` keys an
  infoset on `(public_state, action_history, actor)` only
  (`cfr/betting/betting.h`), with no private-hand dimension. Used for multiway and
  any spot that does not reach a vectorized HU path.

### Regret-state granularity invariant

> A combo-scoped lock may only constrain regret/strategy state whose strategy
> dimensions are **private to that combo**. If several combos share one
> regret/strategy vector, a combo-scoped constraint must not selectively modify
> that shared vector. Either the constrained CFR state is refined to combo
> granularity before the solve, or the lock is rejected. Range-scoped locks have
> no such requirement: they constrain the whole shared vector and apply to both
> regimes unchanged.

Phase 2 evaluates this per target: it classifies each resolved target's solve
regime and, for combo-scoped locks, requires per-combo state (native in the
vectorized regime; produced by refinement in the scalar regime -- below).

### Decision: full per-hand locks (option B), no half measures

Per-hand (combo) locks are a **first-class capability in every supported
heads-up regime**, because those regimes are already per-combo. The scalar
fallback regime is brought to parity by **per-combo refinement of locked
infosets** rather than by downgrading the feature:

- For any infoset carrying at least one combo-scoped lock on the scalar path,
  replace its shared `[action]` regret and strategy-sum vectors with a per-combo
  `[combo][action]` block drawn from the acting seat's live combo domain at that
  infoset (reusing the existing `combo_action_table` primitive rather than
  inventing a new container). Unlocked infosets keep the shared scalar vector, so
  memory and the no-lock fast path are untouched.
- **Semantic consequence (must be stated to users):** refining an infoset to
  per-combo granularity also lets the *free* combos at that infoset hold
  independent strategies, so they stop sharing one vector. This **removes the
  scalar shared-strategy abstraction at that context**: it is more expressive
  (per-hand strategy), not automatically "more correct" in the sense of preserving
  the same game -- the unconstrained combos at a refined infoset can now learn
  different strategies. It is intentional and necessary for a combo lock, a visible
  behavior change for the free combos at refined infosets relative to a scalar
  solve of the same spot, and confined to refined infosets; all other infosets are
  byte-identical. (In the vectorized HU regime there is no change at all: every
  combo was always independent.)
- Determinism and extraction follow the existing `combo_action_table` code path:
  the refined infosets emit genuine per-combo surfaces (no replication), which the
  result store already represents.

Because the dominant production target (heads-up single- and multi-street solves)
runs the vectorized per-combo regime, **the common case needs no refinement at
all**; refinement exists only to honor option B on the multiway/scalar fallback.

## Lock representation layers (authoritative)

The implementation keeps explicit representations so semantic correctness and
hot-loop representation never bleed into each other:

```
lock_set            (authored syntax, user/canonical; Phase 1)
   -> resolved_lock_set   (validated semantic constraints, authoritative; Phase 2)
        -> effective_lock_set   (canonical effective constraint function after
                                 range/combo precedence + merging; hashed in Phase 4)
           -> runtime_lock_view   (compact, per-regime hot loop; Phase 3)
```

- **`lock_set`** -- exactly what the user authored / what round-trips through spot
  JSON: canonical action labels, targets by selector, scope, combos, quantized
  probabilities. Never used in the hot loop.
- **`resolved_lock_set`** -- fully validated: each `strategy_context_id` resolved
  to its concrete infosets; canonical labels resolved to each infoset's local
  `action_index`; combo domain resolved to `combo_local_index`; authoritative
  `acting_seat`; regime classification and (scalar regime) the set of infosets to
  refine. This is the authoritative object that diagnostics (Phase 5) read.
- **`effective_lock_set`** -- the canonical *effective constraint function* keyed
  by **canonical target identity**, produced by applying range/combo action-wise
  overlay and merging and then canonicalizing, so any two input sets that denote
  the same effective per-combo mapping over the same concrete coverage produce
  **identical bytes**. This is exactly the representation `locks_hash` hashes
  (Phase 4). Structure:
  ```
  effective_lock_target {
      target_scope      // context | infoset | node
      target_identity   // canonical SEMANTIC descriptor (see below), not raw CSR id
  }
  effective_lock_function {
      effective_lock_target
      default action constraints   // range-level, within the target's coverage
      sorted combo exceptions (each: combo -> action constraints)
  }
  ```
  canonicalized so same effective mapping over the same coverage => same canonical
  bytes => same `locks_hash`.

  > **Target-coverage invariant.** `strategy_context_id` alone is **insufficient**
  > to key the effective function: a `node_id` or `infoset_id` target must not be
  > widened into a context-wide constraint. The canonical key therefore carries the
  > resolved `target_scope` and a `target_identity` so that two lock sets hash
  > identically **iff** they constrain exactly the same concrete strategy states
  > with exactly the same action-probability function. A context target expands to
  > all covered states; an infoset target to the selected infoset; a node target to
  > that one concrete node only.
  >
  > **Semantic target identity (concrete contract).** `target_identity` is a
  > *semantic* descriptor; the runtime integers are **allocation-ordered and must
  > never be hashed** -- `strategy_context_id` is assigned as insertion order
  > (`strategy_surfaces.size()`, keyed off `infoset_id`, in `ev_surface.h`),
  > `infoset_id` is `next_infoset_id++` in build order (`betting.h`), and `node_id`
  > is `nodes.size()` (`ev_surface.h`). The stable semantic key is the same tuple
  > the betting graph itself keys infosets on in `set_infoset_id`:
  > ```
  > semantic_target_identity =
  >     context_descriptor          // (public_state, canonical betting action_history, acting_seat)
  >     + target_kind               // context | infoset | node
  >     + stable_target_discriminator
  > ```
  > where:
  > - **`context_descriptor`** = `public_state_id` (the node's
  >   `annotations.state_by_node[...].public_state_id`) + the betting
  >   `action_history` (the `vector<betting_action_record{actor, action}>`)
  >   canonicalized to **canonical action labels** (not local `action_index`) +
  >   `acting_seat`. This is exactly `strategy_context_id`'s *meaning*, independent
  >   of its allocation-order integer.
  > - **`context` / `infoset` targets** carry an empty
  >   `stable_target_discriminator` (in the scalar regime an infoset *is* one
  >   strategy context).
  > - **`node` targets** set `stable_target_discriminator` to the concrete
  >   public-state realization that distinguishes two concrete nodes sharing one
  >   `strategy_context_id` (hidden-state aliasing): the canonical descriptor of
  >   that node's public/board runout, **not** its `node_id`. If the current graph
  >   exposes no such stable descriptor, defining one is part of this feature (do
  >   **not** fall back to `node_id`, allocation order, traversal order,
  >   worker-dependent numbering, or an ephemeral `infoset_id`).
  >
  > The descriptor source is fixed in exactly one helper so resolution, hashing,
  > and diagnostics agree. This prevents "same poker lock + semantically identical
  > rebuilt graph => different `locks_hash`".

  **Overlay canonicalization.** Combo exceptions are action-wise overlays on the
  default (see Phase 2). After overlay, any combo exception whose effective
  constraint is **identical to the effective default is eliminated**, yielding a
  genuinely minimal canonical function (so `range: raise=0.7` + `combo AA:
  raise=0.7` canonicalizes to the bare range constraint with no AA exception).
- **`runtime_lock_view`** -- the minimal traversal structure: a dense per-infoset
  `has_lock` bit, the per-infoset range lock (if any), and a per-infoset sorted
  flat `(combo_local_index -> locked mask/probabilities/residual)` index for combo
  locks. Built once from `effective_lock_set`/`resolved_lock_set`; immutable during
  the solve.

The mapping chain made explicit:

```
strategy_context_id
  -> concrete infoset_ids (covered player nodes)
     -> combo strategy state (per-combo in vectorized regime; refined in scalar)
        -> regret/strategy vector actually constrained
```

A combo lock is permitted to modify regret state only at the final arrow, and only
where that state is private to the combo.

## Lock target semantics (authoritative)

A lock targeted by `strategy_context_id` constrains **every** solver player-node
representation belonging to that context, with identical action-label semantics --
not a single graph node. A strategy context is `public_state + betting_history +
acting_seat`, i.e. a semantic strategy identity, so a constraint on it must apply
everywhere that context materializes (consistent with the result-surface
architecture, which keys strategy by `strategy_context_id`).

Lock targets form a union with precise, distinct semantics:

| Target | Meaning |
|---|---|
| `strategy_context_id` | lock **every** represented strategy state in that public context (all covered infosets/nodes), subject to the context-coherence invariant below |
| `infoset_id` | lock one strategic information state (one infoset) |
| `node_id` | lock the one concrete represented decision node |

A `node_id` lock constrains only the strategy state of the combo domain
represented by that concrete node; it does **not** expand to sibling concrete
nodes that share the same `strategy_context_id`. (This matters once hidden-state
aliasing lets several concrete nodes share strategic state: `strategy_context_id`
is the aliasing-wide target, `node_id` is the single-node target.)

Action identity is by **canonical action label**, not by graph-local
`action_index`. Graph-local `action_index` is not assumed globally canonical: two
representations of the same context could legitimately order `fold/call/raise`
differently. Constraints are authored and hashed against canonical action labels
and resolved to each infoset's local `action_index` at resolution time.
Precisely: `action_label` is the **semantic betting-action identity** (e.g.
`raise_to_75`), while `action_index` is a **local edge position**. The validator
compares canonical action *semantics*, not merely the strings emitted by
`solved_node_action.action`; those strings may serve as the canonical identity
only where they are guaranteed to be the canonical semantic IDs (otherwise a
presentation-label change could masquerade as a semantic incompatibility). The
canonical-label source is fixed in one place.

Invariant to enforce in Phase 2/3:

> A `strategy_context_id` lock constrains all player nodes mapped to that context.
> All covered infosets must expose the same legal action labels with a consistent
> label -> action semantic mapping; runtime resolution maps each canonical action
> constraint to that infoset's local `action_index`. If a single context maps to
> infosets whose action-label sets or semantics differ (not merely their index
> order), the lock is rejected (the target is not a coherent strategy context) and
> the validator requires the caller to target by concrete `infoset_id`/node
> identity.

The internal resolved target carries the authoritative acting seat:

```
resolved_lock_target {
    uint32_t strategy_context_id;
    uint8_t  acting_seat;        // resolved from the graph/context, not the user
    // concrete infoset ids covered by this context (for the runtime map)
}
```

A user-supplied `"seat"` is an **optional assertion** validated against
`acting_seat` for good diagnostics; it is never an independent source of truth.

## Phase 1: Constraint schema, parsing, and canonical model

**Outcome:** a versioned constraint representation that round-trips spot JSON ->
internal `lock_set` -> canonical bytes with no semantic drift, supports partial
constraints natively, and leaves no-lock solves byte-identical.

### Deliverables

- `cfr/locks/lock_model.h` (new header-only `cfr/locks/` directory) defining:
  ```
  enum class lock_scope : uint8_t { range, combo };

  struct action_constraint_entry {   // one "actions" map, pre-resolution
      // action label -> fixed probability; only constrained actions appear
      small_vector<pair<string, float>> by_label;
  };

  struct lock_entry {
      uint32_t   lock_id;                 // document-local provenance id, input order; non-semantic
      // user-facing target selector resolved in Phase 2:
      node_selector target;               // strategy_context_id or infoset/node id
      optional<uint8_t> asserted_seat;    // optional user assertion only
      lock_scope scope;
      action_constraint_entry constraints;
      sorted_vector<combination_index> combos;  // required iff scope == combo
  };

  struct lock_set { vector<lock_entry> entries; /* canonical order */ };
  ```
- Spot JSON surface: a new optional top-level `"locks"` array parsed in
  `cli/solve_cli.cpp` using the existing boost::json helpers (`parse_object`,
  `find_value`, number/bool/string extractors) and the hand-token parsing already
  used by `range_parser.h` for `"combos"`:
  ```json
  {
    "locks": [
      { "node": { "strategy_context_id": 12 }, "seat": 0, "scope": "range",
        "actions": { "raise": 0.0 } },
      { "node": { "strategy_context_id": 44 }, "scope": "combo",
        "combos": ["AhKh", "AsKs"],
        "actions": { "fold": 0.0, "call": 0.7, "raise": 0.3 } }
    ]
  }
  ```
  The `"actions"` map lists **only constrained actions**: `{ "raise": 0.0 }` means
  "raise is pinned to 0, everything else is free", not "the whole strategy is
  {raise:0}". A full map pins all actions.
- Action labels are the same tokens already emitted in `solved_node_action.action`;
  they resolve to `action_index` in Phase 2 (legal label set is known only after
  the tree is built).
- Canonical normalization + `to_canonical_bytes()` for hashing (Phase 4): stable
  ordering by `(resolved target, scope, sorted combos, sorted action_index)`,
  carrying the quantized probabilities.
- Threading: carry the parsed `lock_set` through the solve request alongside
  `convergence_options` and `pruning` in `solve_runtime_options`.

### Quantization invariant (exact order)

```
parse decimal
-> validate finite and >= 0
-> quantize to fixed grid (round to nearest 1e-6, ties-to-even)
-> validate mass on the QUANTIZED values (S = sum locked, 0 <= S <= 1)
-> store quantized value
-> normalize representation (full lock -> all-locked/residual 0; see below)
-> enforce quantized value at runtime
-> hash the quantized representation
```

**Rounding rule (deterministic):** quantization snaps each probability to the
nearest multiple of `1e-6` using **round-half-to-even** (banker's rounding) on the
scaled integer, computed identically on every platform (e.g.
`std::nearbyint(p * 1e6)` under the default `FE_TONEAREST`, or an explicit
ties-to-even integer round; the chosen helper is fixed in one place and unit
tested). Because locked probabilities are non-negative this only matters at exact
half-ulp ties, but the rule is specified so canonicalization and `locks_hash` are
bit-stable across builds.

**Full-lock normalization:** a constraint map that pins *every* legal action is
normalized at resolution into the single runtime model "all actions locked,
residual `r = 0`", identical in representation to a partial lock whose locked mass
happens to sum to 1. The runtime therefore has exactly one model (locked subset
`L`, free subset `F`, residual `r`) and never distinguishes "full input" from
"partial input".

All mass checks, the residual `r = 1 - S`, runtime enforcement, and hashing use
the stored quantized values, so JSON text formatting can never change a solve's
identity or produce a validate/enforce mismatch.

### Implementation notes

- `lock_id` is assigned in input order and preserved through canonical sorting so
  validation/diagnostics can cite the exact user entry. `lock_id` is **not**
  hashed (it is presentation, not solver semantics). It is a **document-local
  provenance identifier** assigned from input order and deliberately non-semantic;
  it is not stable across edits, and the UI may regenerate or retain IDs when
  entries are duplicated/reordered. It is never solver identity.
- An absent `"locks"` key yields an empty `lock_set`; every downstream path treats
  empty as "no behavior change" behind an explicit guard so the no-lock numeric
  path is untouched.

### Exit criteria

- a valid `"locks"` payload parses, normalizes, and re-serializes to identical
  canonical bytes across runs; partial and full constraint maps both round-trip
- malformed entries fail with field-level `cli_error` messages citing `lock_id`
- a spot with no `"locks"` key produces an empty `lock_set` and an unchanged solve

## Phase 2: Resolution and pre-solve validation

**Outcome:** every accepted constraint is resolved to authoritative internal
coordinates and proven legal against the lowered `game_graph`, its
`solver_graph_annotations`, and the seat's live combo domain; all detectable
violations are reported together before any iteration.

### Deliverables

- `cfr/locks/lock_validation.{h,cpp}` running after graph build and
  `validate_solver_graph_view`, returning `std::expected<resolved_lock_set,
  lock_validation_report>`. Errors are classified by an explicit kind so Phase 5
  and the UI can react differently to an input mistake versus a context-coherence
  problem:
  ```
  enum class lock_validation_error_kind : uint8_t {
      malformed,              // schema/syntax (bad JSON shape, non-number prob)
      target_not_found,       // selector resolves to no player node/context
      seat_mismatch,          // asserted seat != authoritative acting seat
      incoherent_context,     // one context covers infosets with different labels
      illegal_action,         // constrained label not legal at the node
      invalid_probability,    // non-finite or < 0
      invalid_mass,           // S > 1, or fully-locked != 1, or residual unassignable
      invalid_combo,          // out-of-domain / blocked / duplicate token in entry
      conflicting_constraint, // contradictory entries on same (ctx, action, combo)
      isomorphism_inexact,    // combo lock not exactly representable under iso
      granularity_unsupported // combo lock on state that cannot be made per-combo
  };
  ```
  The report accumulates all errors (does not fail on first), mirroring the
  structural validators, and each carries the offending `lock_id`.
- Target resolution and coverage (produces the exact concrete strategy-state
  coverage of the target, not merely its context):
  - resolve the selector to its **canonical target identity** and `target_scope`
    (`context` | `infoset` | `node`) and to the exact set of concrete strategy
    states it covers: a context target expands to **all** covered infosets/nodes;
    an infoset target to the selected infoset; a node target to that **one**
    concrete node only. This resolved coverage -- not `strategy_context_id` alone
    -- is what Phase 4 hashes (see the target-coverage invariant above).
  - for context targets, resolve the `strategy_context_id` and the full set of
    player nodes/infosets belonging to that context
  - enforce the lock-target invariant above (reject contexts whose covered
    infosets expose different legal action-label sets or an inconsistent
    label -> action mapping; mere differences in local `action_index` ordering are
    fine and are reconciled by per-infoset label resolution). Require
    infoset/node targeting instead when the context is incoherent.
  - resolve `acting_seat` from the graph; if `asserted_seat` is present and
    differs, reject with a clear message
- Regime and granularity classification (combo scope):
  - classify each resolved target by solve regime (per-combo vectorized HU vs
    scalar shared-strategy) from the same dispatch logic the solver uses
  - enforce the **regret-state granularity invariant**: a combo-scoped lock is
    accepted only if its constrained state is per-combo. In the vectorized regime
    this is always true. In the scalar regime, mark the covered infosets for
    per-combo refinement (Phase 3); if an infoset cannot be refined (e.g. it is
    not combo-addressable in that abstraction), reject with
    `granularity_unsupported` rather than silently constraining a shared vector.
  - range-scoped locks skip this check entirely (they constrain the shared vector
    legitimately in both regimes)
- Action legality (against the node's real edges + labels):
  - every constrained label resolves to a real `action_index` at the node
  - no duplicate action labels within an entry
  - each constrained probability is finite and `>= 0`
- Effective constraint assembly (**action-wise overlay**, precedence **before**
  mass validation):
  - resolve the range-level constraint function for the target's coverage
  - overlay combo constraints on top of it **per action dimension**: for a combo,
    a combo-specified action **replaces** the range constraint for *that action
    only*; actions the combo entry omits **inherit** the range constraint.
    "Override" is action-wise overlay, not whole-entry replacement. Examples:
    `range: raise=0` + `combo AA: raise=0.5` -> AA `raise=0.5`;
    `range: raise=0.7` + `combo AA: call=0.4` -> AA `raise=0.7, call=0.4`.
  - merge action constraints **within each effective combo**
  - canonicalize: a combo exception whose effective constraint equals the effective
    default is **eliminated** (e.g. `range: raise=0.7` + `combo AA: raise=0.7`
    leaves no AA exception), so the canonical function is minimal
  - only then validate mass on the resulting effective per-combo constraint
  - range and combo entries are **not** mass-validated independently: e.g.
    `range: raise = 0.7` + `combo AA: call = 0.4` yields the effective AA
    constraint `raise = 0.7, call = 0.4` -> residual `-0.1` -> **reject**
    (`invalid_mass`), even though each entry is individually legal
- Mass policy (partial-aware, on quantized values, applied to each **effective
  per-combo** constraint from the assembly step above):
  - `S = sum of locked probabilities`; require `0 <= S <= 1`
  - require `F` non-empty OR `S == 1` (otherwise residual mass is unassignable ->
    reject)
  - reject a fully-locked distribution whose mass is not exactly 1
  - residual `r = 1 - S` is implicit and belongs to the free actions; there is no
    user-visible renormalization of free actions
- Combo-domain validation (combo scope):
  - each `combination_index` is in the acting seat's live range at the context
    (`hand_range` weight > 0, not blocked by board/dead cards; cross-check against
    the node's materialized combo domain / `combo_indices`)
  - duplicate combo **tokens within a single entry** are a validation error
    (`invalid_combo`, citing the repeated token) -- there is no useful meaning to
    `["AhKh", "AhKh"]`, so it is caught as a user mistake rather than silently
    merged; **identical constraints across separate entries** are merged (see
    conflict handling). Out-of-domain combos are rejected citing the offending
    `combination_index`.
  - if card isomorphism is enabled, a combo-scoped lock is accepted only when the
    solver's active representation can map that **exact physical combination** to a
    unique constrained strategy state **without simultaneously constraining any
    other physical combination not covered by the lock**. If two physical combos
    share a strategically equivalent state under the active isomorphism, a lock on
    one would implicitly constrain the other -- **reject** with
    `isomorphism_inexact` (exact locking stays exact). This test must also check
    **range-weight symmetry**: the induced private-card permutation must leave the
    range weights invariant, since Zeta's isomorphism semantics assume that
    invariance.
- Conflict handling (explicit, deterministic schema semantics):
  - identical duplicate constraints -> merge
  - merge identity: two entries are "identical" iff they agree on
    `(target semantic context, scope, combo set, canonical action label,
    quantized probability)`. Matching entries are a **semantic merge** into one
    effective constraint, while all contributing `lock_id`s are retained as
    **provenance**. `asserted_seat` and diagnostics are presentation/validation
    metadata and must **not** affect the effective constraint or the merge
    decision (entries that agree on the semantic key but differ only in asserted
    seat still merge semantically; both `lock_id`s are kept).
  - range + combo overlap -> permitted; combo constraints **action-wise overlay**
    range constraints for the overlapping combinations (a combo-specified action
    replaces the range constraint for that action only; omitted actions inherit the
    range constraint), and the overlay is recorded in diagnostics (Phase 5)
  - contradictory constraints on the same `(context, action_index, combo)` ->
    error (never silently resolved)

### Implementation notes

- Build a `combination_index -> combo_local_index` reverse map once per target
  context and reuse it in Phase 3 (combo domains are already materialized for
  extraction).
- Accumulate all errors into one `lock_validation_report` rather than failing on
  the first, mirroring the structural validators.
- Validation is read-only over the graph and runs before table allocation.

### Exit criteria

- invalid constraint sets are rejected pre-solve with deterministic, field-level
  errors citing `lock_id`
- valid sets resolve to `resolved_lock` coordinates (below) with authoritative
  seat and infoset coverage, and no mutation of unlocked behavior

## Phase 3: Constrained CFR+ integration

**Outcome:** the constrained behavioral strategy and the free-sub-simplex regret
update from "Core semantics" are implemented exactly; locked probabilities are
enforced every iteration; the no-lock path is byte-identical.

### Runtime representation (`runtime_lock_view`)

Built once from `resolved_lock_set`/`effective_lock_set` after validation, keyed
for O(1) lookup in traversal, with a compact flat layout (no hash maps in the hot
path, per Zeta's performance architecture). This is the compact hot-loop layer of
the representation model: the semantic `resolved_lock_set` stores only locked
probabilities per canonical action; the residual is **derived** (`r = 1 - sum
locked`) and cached here purely as a hot-loop convenience, never as independent
state.

Design principle: the runtime lock lookup uses the **same offset/index design
philosophy as the game graph** -- a CSR-like layout (`lock_row_offsets` /
`lock_entries`, i.e. per-infoset `offset/count` slices), never per-node dynamic
containers or hash maps in the hot path. (The exact field layout is an
implementation detail; the principle is mandatory.)

```
struct resolved_lock {          // one record inside runtime_lock_view
    uint32_t strategy_context_id;
    uint32_t infoset_id;
    uint16_t combo_local_index;      // INVALID => range-scoped (all combos)
    uint8_t  action_count;
    uint8_t  locked_mask_bits;       // bit a set => action a is locked
    float    locked_probability[MAX_ACTIONS];  // valid where mask bit set
    float    residual;               // r = 1 - sum locked; derived/cached only
};
```

`locked_probability` is indexed by the infoset's **local** `action_index`
(resolved from canonical action labels in Phase 2). A full lock is already
normalized (Phase 1) to "all bits set, `residual == 0`", so the runtime sees one
model only.

- range locks: one `resolved_lock` per constrained infoset, `combo_local_index ==
  INVALID`
- combo locks: a per-infoset **sorted flat index** of
  `(combo_local_index -> resolved_lock)` (prescribed; not a general hash map),
  looked up by the already-local combo index during traversal
- a per-infoset `has_lock` flag (dense bitset) so unlocked infosets pay nothing:
  the hot path checks the flag first and otherwise runs the existing code
  unchanged

### Integration points per regime

The constrained strategy (hook 1) and constrained regret update (hook 2) are the
same math in both regimes; only the state they touch differs.

- **Per-combo vectorized heads-up regime (primary):** hook 1 extends
  `normalize_combo_action_table` (the per-`(combo, infoset)` regret-matching ->
  `current_strategy` step); hook 2 extends `update_combo_tables` (the per-`(combo,
  infoset)` regret / strategy-sum update). Because this regime is already
  `[combo][infoset][action]`, a combo lock simply selects the matching
  `resolved_lock` for that `(combo, infoset)` and a range lock applies to every
  combo at the infoset. No new storage, no refinement, no extraction change.
- **Scalar shared-strategy regime (fallback):** hook 1 extends
  `compute_regret_matching_strategy<StrategyPolicy>`; hook 2 extends the CFR
  value/regret backup (`child_action_value` / delta buffer) in
  `cfr/solver/iteration.h`. Range locks apply directly to the shared vector.
  **Combo locks require per-combo refinement** (below) so the constrained combos
  own private regret state.

### Per-combo refinement (scalar regime only)

For any scalar-regime infoset marked in Phase 2 as carrying a combo-scoped lock,
replace its shared `[action]` regret and strategy-sum vectors with a per-combo
`[combo][action]` block over the acting seat's live combo domain at that infoset,
reusing the existing `combo_action_table` primitive. Then both hooks operate on
the per-combo block exactly as in the vectorized regime. Properties:

- unlocked infosets are untouched (still one shared vector) -- memory and the
  no-lock fast path do not regress; only refined infosets grow to combo width
- the free combos at a refined infoset also gain independent regret state, so they
  may diverge from the shared-strategy solution at that infoset. This is the
  intended removal of the scalar shared-strategy abstraction at that context (more
  expressive, not merely "more correct") and is confined to refined infosets;
  it is surfaced in diagnostics (Phase 5) and covered by a dedicated test
  (Phase 6)
- extraction for refined infosets writes genuine per-combo surfaces (the
  `combo_action_table` extraction already does this); `write_strategy_surface`'s
  replicate-one-vector behavior is used only for still-shared infosets
- determinism is preserved: refinement is a pure function of the resolved lock set
  and the graph, independent of worker count or scheduling
- **initialization (explicit):** refinement happens **before the first CFR
  iteration** (solve construction time), not by dynamically changing granularity
  mid-solve. Refined per-combo regret and strategy-sum state is initialized using
  the **same initial-state policy as native vectorized combo state** (e.g. zeroed
  for a fresh solve); **no scalar regret/strategy-sum state is copied, scaled, or
  redistributed** into the refined vectors. If a resume/checkpoint occurs after
  refinement, the refined per-combo state must be persisted in the checkpoint
  representation; checkpoint/resume of refined state is otherwise out of scope for
  this feature and no scalar->combo conversion is ever synthesized.

### Strategy materialization (hook 1)

Selected when `has_lock` is set for the current infoset (and, for combo scope, the
current combo). The locked/free partition and residual come from the matching
`resolved_lock`:

```
if (residual == 0) {
    // feasible set collapsed to a point: strategy fully determined
    for a in L: edge_prob[a] = locked_probability[a]
    for a in F: edge_prob[a] = 0
} else {
    rm = regret_matching(positive_regrets restricted to F)   // sums to 1 over F
    for a in L: edge_prob[a] = locked_probability[a]
    for a in F: edge_prob[a] = residual * rm[a]
}
```

For the no-lock case the function body is literally unchanged (fast path).

### Regret update (hook 2)

At the regret-accumulation point (where per-action child values are read from
`child_action_value` / folded through the delta buffer), for constrained infosets
with `residual > 0` compute the **free-simplex comparator** and update only free
actions, scaling by the residual so the stored value is the literal restricted
-game regret:

```
if (residual == 0) {
    // no feasible movement: skip the free regret update entirely
} else {
    V_free = sum_{a in F} rm[a] * child_value[a]            // rm normalized over F
    for a in F: R_a += cf_reach * residual * (child_value[a] - V_free)
}
// locked actions: no regret update
// CFR+ positive clipping applies to free actions only
```

The node value propagated to the parent remains the full behavioral value
`V_node = sum_a sigma_a * child_value[a]` (locked + free), so opponents
best-respond to the constrained behavior. These two quantities (`V_free` for
regret, `V_node` for backup) are computed from the same `child_value[]` and
`sigma`/`rm` already in scope.

### Average strategy (no special path)

The strategy-sum state accumulates the **actual behavioral strategy `sigma`** used
for traversal, exactly as today -- per `(combo, infoset)` in the vectorized /
refined regime (`strategy_sums.value(combo, infoset_id, a)`), per shared infoset
elsewhere. Because `sigma_a == p_a` every iteration for locked actions, the
extraction pipeline (`combo_action_table` surfaces in the vectorized/refined
regime; `normalized_average_strategy` / `write_strategy_surface` for still-shared
infosets) is unchanged and the extracted average is genuinely the average of what
the solver played. A final assertion checks `average(locked action) == p_a` within
quantization tolerance.

### Determinism

The runtime lock view is immutable during the solve and only changes per-infoset
(or per-combo, at refined/vectorized infosets) strategy vectors, not reduction
order, so the existing deterministic reduction plan is unaffected. Scalar-regime
per-combo refinement is a pure function of the resolved lock set and the graph.
Verify identical output across worker counts for both range and combo locks.

### Exit criteria

- locked action probabilities equal `p_a` within tolerance at every targeted
  infoset/combo, every iteration and in the final average
- free actions carry exactly residual mass `r`, distributed by free-simplex regret
  matching
- combo locks constrain only the targeted combos; non-targeted combos at the same
  infoset are solver-derived (and, in the scalar regime, their post-refinement
  divergence is confined to refined infosets and matches an independent
  hybrid-abstraction reference with identical per-combo refinement boundaries)
- when `r == 0` (fully constrained mass), free actions carry probability 0 and no
  free regret is accumulated
- the opponent's downstream strategy shifts toward the best response against the
  constraint (validated in Phase 6)
- empty `lock_set` => byte-identical strategy, EV, hashes, and within-noise timing
  vs the current solver

## Phase 4: Re-solve workflow, hashing, and artifact provenance

**Outcome:** constrained solves are first-class, reproducible artifacts whose
identity and metadata capture the enforced constraint semantics.

### Deliverables

- Hashing in `cli/solve_cli.h`/`.cpp`:
  - add `uint64_t locks_hash` to `solve_hashes` (beside `betting_policy_hash`,
    `solver_config_hash`)
  - compute it from the **canonical `effective_lock_set`** (per canonical target
    identity, preserving context/infoset/node scope) via FNV-1a
    `compatibility_hasher`
  - **Empty-lock hash compatibility (mandatory invariant):** an empty effective
    lock set contributes **no bytes** to the solve hash. Concretely:
    ```
    if effective_lock_set.empty():
        locks_hash = 0
        solve_hash = <legacy schema-4 solve_hash computation, unchanged>
    else:
        locks_hash = hash(effective_lock_set)
        solve_hash = <legacy solve_hash stream> + combined.add_u64(locks_hash)
    ```
    Do **not** unconditionally `combined.add_u64(locks_hash)`; that would change the
    stream even for `locks_hash == 0` and break identity. Therefore a no-lock
    schema-5 solve has **exactly** the same `solve_hash` as the existing schema-4
    solve. (Keep three identities distinct: solver numeric state/output identity,
    artifact *schema* identity -- schema-5 adds fields -- and hash identity; only
    the artifact schema changes for a no-lock solve.)
  - serialize/deserialize `hashes["locks"]` in the artifact JSON
- Hash-identity invariant:
  > `locks_hash` hashes the **canonical `effective_lock_set`** -- the effective
  > constraint function after range/combo precedence, merging, and
  > canonicalization -- not the raw input entries and not the resolved-but-
  > uncanonicalized set. The canonical effective representation guarantees that two
  > input sets denoting the same effective per-combo mapping produce **identical
  > bytes** and therefore identical hashes (e.g. `range: raise=0` + `combo AA:
  > raise=0.5` versus any other encoding of the same mapping). The hashed content
  > is, **per canonical target identity** (preserving whether the constraint is
  > context-, infoset-, or node-scoped -- `strategy_context_id` alone is
  > insufficient), the **canonical action identity (action label), not graph-local
  > `action_index`**, the quantized default (range-level) constraints, and the
  > sorted combo exceptions with their quantized constraints -- and nothing else.
  > The target identity is the **semantic descriptor defined under "Semantic
  > target identity (concrete contract)"** -- `context_descriptor` (public_state +
  > canonical betting `action_history` + acting_seat) plus `target_kind` and, for
  > node targets, the stable public-state discriminator -- **never** the
  > allocation-ordered `strategy_context_id` / `infoset_id` / `node_id` integers,
  > so a semantically identical graph rebuild does not change `locks_hash`. Hashing canonical action labels (not
  > indices) keeps the identity stable if a graph-building change renumbers
  > semantically equivalent actions. It explicitly **excludes input ordering of
  > constraints**, `lock_id`, warnings, derived combo-local indices, resolved local
  > `action_index`, and diagnostic ordering. Canonicalization is applied before
  > hashing so entry order cannot affect identity (asserted by a dedicated reorder
  > test in Phase 6).
- Schema version bump `current_artifact_schema_version` `4 -> 5`, with readers
  backward-compatible: a schema-4 (no-lock) artifact loads with an empty set and
  absent/zero `locks_hash`, no migration tooling required.
- `solver_metadata` provenance: a `locks` section recording the canonical
  normalized constraint payload (or compact summary + `locks_hash`), total count,
  and per-scope counts. Use the existing `solver_metadata.warnings` channel for
  non-semantic notes only (e.g. a combo override was applied); anything that
  changes solve semantics must be in the hashed payload or rejected (never a
  warning).
- `solver_metadata` refinement provenance: record `refined_contexts` (and a
  `refinement_hash`) listing which strategic abstractions were expanded to
  per-combo state as a **consequence** of the effective lock semantics and solve
  regime. This is provenance only -- it is **not** part of `locks_hash` and not
  required to equal it. Keep the three identities distinct: `locks_hash` = what
  the user constrained; `solve_hash` = complete solve identity;
  `solver_metadata.refinement` = what strategic abstraction was expanded as a
  consequence.
- `solved_node` annotation: a `lock_state` field (`none`, `range`, `combo`,
  `mixed`) plus, for constrained player nodes, the enforced per-action
  probabilities so inspection distinguishes solver-derived from locked frequencies
  without recomputation.
- CLI acceptance: the solve entry point threads the `resolved_lock_set` end to end
  into the emitted artifact.

### Exit criteria

- two solves differing only in constraints produce different `solve_hash` and
  `locks_hash`; identical constraint semantics (regardless of `lock_id` order or
  JSON formatting) produce identical hashes
- a schema-5 constrained artifact round-trips through artifact JSON; schema-4
  artifacts still load
- inspection can reconstruct exactly which constraints were enforced and on which
  combos
- **Semantic-identity Definition of Done:** a lock is complete only when its
  canonical effective constraint function identifies **exactly** the concrete
  strategy states it constrains -- including the distinction between context-,
  infoset-, and node-scoped targets -- and this function is the **sole** semantic
  input to `locks_hash`. Equivalently: two lock sets share a `locks_hash` iff they
  constrain exactly the same concrete strategy states with exactly the same
  action-probability function.

## Phase 5: Diagnostics and CLI feedback

**Outcome:** users see what was accepted, overridden, and rejected, and each
constraint's footprint, without reading raw artifacts.

### Deliverables

- Structured solve-response diagnostics:
  - accepted constraints (count, scope breakdown, affected contexts/combos)
  - rejected constraints with `lock_id` + reason (from Phase 2 error kinds)
  - recorded range-vs-combo overrides (which combos overrode which range
    constraint)
- Constraint-impact summary:
  - number of constrained infosets and combos (and, in the scalar regime, how many
    infosets were refined to per-combo state)
  - constrained action mass per seat/street
  - locked-vs-free frequency deltas at affected nodes. This is a **diagnostic
    footprint only**: it introduces no compare-domain model and no compare
    persistence. A future locked-vs-unlocked compare mode (item 7 in
    `next_steps.md`) may consume these numbers, but that engine is explicitly not
    built or scaffolded here.
- Human-readable CLI formatting for validation errors and the impact summary,
  matching the existing `cli_error` presentation.

### Exit criteria

- common mistakes (illegal action, out-of-domain combo, mass > 1, residual with
  no free action, contradictory constraints, isomorphism-inexact combo) are
  diagnosable from a single validate/solve response
- constraint footprint and any overrides are visible in CLI output

## Phase 6: Test matrix, performance guardrails, and rollout

**Outcome:** node locking ships with correctness (including an exact constrained
-CFR reference), determinism, and performance confidence.

### Deliverables

- Unit tests (extend the holdem suite, e.g. near `test/src/test_holdem_cli.cpp`
  and the cfr table/metadata tests):
  - schema parse/normalize/canonical round-trip incl. partial maps and
    quantization ordering
  - validation coverage for every error kind in `lock_validation_error_kind`:
    target/seat, incoherent context, action legality, mass policy (`S>1`,
    residual-with-no-free, fully-locked != 1), combo-domain, duplicate combo
    **token within an entry = error**, cross-entry identical merge, range/combo
    override, contradiction, isomorphism-inexact rejection, and
    `granularity_unsupported` for a combo lock on non-combo-addressable scalar
    state
- **Exact constrained-CFR reference test (contract test, written first):** per the
  engineering sequence this test is authored against "Core semantics" **before**
  the Phase 3 runtime exists, then the runtime is implemented to pass it.
  Construct a tiny synthetic river node using existing hold'em infrastructure --
  3 actions, 1-2 combos, known terminal utilities, one action pinned. Choose
  values so that `V_node != V_free`:

  ```
  locked action value   = 10,  p_locked = 0.25
  free action values    = {2, 6},  r = 0.75
  free regret matching starts uniform: rm = {0.5, 0.5}
  V_free = 0.5*2 + 0.5*6 = 4
  V_node = 0.25*10 + 0.75*4 = 5.5          # != V_free, != any single value
  first-iteration free regret deltas:
      dR_(val2) = cf_reach * 0.75 * (2 - 4) = cf_reach * (-1.5)
      dR_(val6) = cf_reach * 0.75 * (6 - 4) = cf_reach * (+1.5)
  locked action regret delta = 0 (never updated)
  ```

  Assert Zeta reproduces these numbers **iteration-by-iteration** (strategy,
  regrets, strategy sums), and that the value backed up to the parent is `V_node =
  5.5` while the free regret baseline is `V_free = 4`. Implement the reference as a
  **genuinely independent scalar oracle** (`reference_sigma`,
  `reference_free_value`, `reference_regret_delta`, `reference_strategy_sum`) with
  explicit arithmetic -- **not** by calling the same helpers production CFR uses --
  so a shared mistake in implementation and test cannot hide. This catches:
  - wrong constrained baseline (`V_node` vs `V_free`)
  - wrong residual scaling of free actions (missing/incorrect `r` factor)
  - wrong counterfactual-reach weighting
  - accidental updating of locked regrets
  - incorrect CFR+ clipping on locked dims
  - average-strategy drift
  Also assert these **mathematical invariants on every constrained iteration**
  (independent of the golden numbers, so they hold for any valid lock):
  - `sum_a sigma[a] == 1`; `sigma[a] == p[a]` for locked `a`;
    `sum_{a in F} sigma[a] == r`; `sigma[a] >= 0`
  - `locked regret delta == 0`
  - `sum_{a in F} free regret delta == 0` (follows from
    `sum rm[a](V[a] - V_free) == 0`) -- a strong implementation tripwire
  - `V_node == sum_a sigma[a] V[a]` and `V_free == sum_{a in F} rm[a] V[a]`
- Named edge-case semantic tests (each asserted explicitly, not folded into
  "special values"):
  - **zero-probability lock (`p_locked = 0`, the dominant "never raise" case):**
    raise `sigma == 0` every iteration and in the average; raise regret is never
    updated; the free actions renormalize to sum 1; the opponent responds to the
    no-raise behavior (its strategy differs from the unlocked solve).
  - **`S == 1` with a non-empty free set (e.g. `fold = 1`, call/raise left
    syntactically free):** `fold == 1`, `call == 0`, `raise == 0`, and **no free
    regret update occurs** -- specifically guards against regret matching still
    being invoked just because `F` is non-empty.
  - **opponent-response test (asymmetric):** Hero locked `raise = 0`; Villain has
    two actions with sharply different values. Assert Villain's converged strategy
    **changes relative to the unlocked game** -- the user-facing meaning of node
    locking (the lock changes the game the other players face).
- Convergence-semantics tests:
  - `r == 0` constrained solve: the converged strategy is exactly the lock, and no
    free regret is accumulated (free regrets stay at their initial value)
  - `0 < r < 1` constrained solve: the free sub-strategy converges normally while
    locked dimensions never enter the free regret baseline
- Integration / correctness tests:
  - range lock on a small heads-up spot: locked frequencies exact; opponent EV /
    frequencies shift vs a hand-computed best response
  - per-combo lock (vectorized HU regime): only targeted combos constrained;
    others solver-derived with no change vs the unlocked solve at the same infoset
  - per-combo lock (scalar regime, refinement path): targeted combos constrained;
    free combos at a **refined** infoset may diverge from the shared-strategy
    solution, but every non-refined infoset is byte-identical to the unlocked
    solve, and the refined-infoset result matches an independent
    **hybrid-abstraction** reference with identical per-combo refinement boundaries
    (per-combo state at the refined infosets, scalar shared state everywhere else)
    -- **not** an unrestricted fully-per-combo solve, which is a different game
  - range/combo override: `range raise = 0` with `combo AA raise = 0.5` yields
    `AA -> 0.5` (rest per the free solve) and every other combo `raise -> 0`; the
    override appears in diagnostics and does not change `locks_hash` beyond the
    resulting canonical semantics
  - "raise pinned to 0" partial lock: residual mass splits correctly across free
    actions and the opponent exploits it
  - no-lock equivalence: byte-identical strategy/EV and identical hashes vs a
    baseline solve with the feature compiled in but unused
  - artifact round-trip: `locks_hash`/`solve_hash` change with constraints;
    schema-5 parse/serialize; schema-4 back-compat load; hash invariance under
    `lock_id` reorder and JSON reformatting
- Determinism tests across worker counts/seeds for a constrained solve.
- Performance guardrails:
  - no-lock overhead within existing timing noise (the `has_lock` fast path must
    not regress the hot loop)
  - constrained-solve scaling measured vs number of constrained infosets/combos
  - **refined-scalar memory amplification reported**, not just time: baseline
    scalar table bytes `+ refined_infosets x combo_count x action_count x
    state_bytes`, since a broad range of combo locks on the scalar path can
    materially grow memory
- Rollout: gated by presence of the `"locks"` key (absent => current behavior);
  default-on vs documented flag decided per the repo convention used for the
  `pruning` / `convergence` opt-ins.

### Exit criteria

- full regression coverage of constraint types, every validation failure mode,
  and the exact iteration-by-iteration constrained-CFR reference
- no-lock path proven byte-identical and within performance budget
- feature enabled per the documented rollout decision

## Phase 7: UI integration (Qt hold'em app)

**Outcome:** the desktop app ships a **complete lock-management editor**. Users
author, edit, organize, validate, run, inspect, and persist arbitrary sets of
range- and combo-scoped locks across any node in the spot, with locked frequencies
visually distinguished from solver-derived ones and full save/reload fidelity.
This is the real product surface, not a reduced subset.

The app is tightly coupled to the solver structs (not JSON-only):
`ui/holdem/src/solver/solver_session.cpp` calls `cli::solve_spot(spot_snapshot,
iterations, runtime)`, and `ui/holdem/src/document/document_json.cpp` persists both
the spot and artifact via `cli::serialize_spot_json` / `serialize_artifact_json`
(and `parse_spot_json` / `parse_artifact_json`). `spot_document`
(`ui/holdem/src/spot_document.{h,cpp}`) owns the editable `spot`, optional
`artifact`, optional `solution_store`, metadata, and dirty state;
`document_workspace_widget` hosts spot editing and an inspector; `range_editor`
(`widgets/range_editor.{h,cpp}`) is the established 13x13-matrix + combo-table
authoring pattern that writes back to the spot via an `on_spot_changed(spot)`
callback. The lock editor reuses these patterns and writes into the **same
canonical `lock_set`** (`spot.locks`) the CLI consumes -- the UI is one more author
of that model, never a parallel representation.

The work is organized into four sub-phases so each lands independently and
testably; together they constitute the full editor.

### Phase 7.1: Lock transport, persistence, and solve plumbing

**Outcome:** a lock set authored anywhere in the app is carried into the solve and
survives save/reload with byte-level fidelity, before any editing UI exists on top.

- Persistence round-trip:
  - `solve_spot.locks` and the schema-5 artifact (incl. `locks_hash`,
    `solver_metadata.locks`, per-node `lock_state`) round-trip through
    `spot_document` / `document_json` save+load with no loss, using the existing
    cli serializers
  - the UI document gate is strict (`document_schema_version == 3`, "Only version
    3 is accepted"); bump `document_schema_version` and
    `current_solution_schema_version` (`solver/solution_store.h`, currently `3`)
    to carry lock payloads, with a loader that still accepts prior documents
    (empty lock set) so existing studies open unchanged
  - add document regression tests that save and reload documents containing
    range locks, combo locks, and mixed sets
- Solve request plumbing: locks travel inside the spot, so `cli::solve_spot`'s
  signature is unchanged, but `solver_session_request` snapshotting
  (`main_window_solver.cpp`, `solver/solver_session.*`) must include the lock set
  in the spot snapshot so the solved artifact matches the edited spot, and a
  re-solve after a lock edit produces an artifact whose `locks_hash` matches the
  edited set.

### Phase 7.2: Lock authoring at a node

**Outcome:** a user can create a lock directly from a decision point -- choosing
the action(s), probabilities, and scope -- without hand-writing JSON.

- Node targeting independent of a prior solve: locks target nodes by
  `strategy_context_id`, which is a function of the spot's betting structure, so
  the editor must be able to enumerate targetable nodes from the lowered betting
  tree even on an unsolved spot (reuse the graph lowering already available to the
  solver), and also bind to the post-solve node tree in `strategy_explorer`
  (`node_tree_`, `node_action_table_`) when an artifact exists.
- Authoring controls in `widgets/strategy_explorer.{h,cpp}` (and reachable from the
  spot inspector for unsolved spots):
  - per legal action at the selected node: toggle locked/free and set a probability
    (spin/slider), with a live residual-mass readout and inline enforcement of the
    mass policy (`S <= 1`, residual assignment) mirroring Phase 2
  - scope selector: range (all live combos at the context) vs combo; combo scope
    reuses the `range_editor` 13x13 matrix + combo-table selection so the user
    paints the exact `combination_index` set
  - "apply" writes a `lock_entry` into `spot.locks`, marks the document dirty via
    `on_spot_changed`, and refreshes the manager (7.3)

### Phase 7.3: Lock management panel (full editor)

**Outcome:** a dedicated surface to see and manage **all** locks in the document at
once, decoupled from whichever node is currently selected.

- New `widgets/lock_editor.{h,cpp}` panel, surfaced as an inspector tab in
  `document_workspace_widget` (alongside the actions/hands panels) so locks are
  editable with or without a current solve:
  - a list/table of every `lock_entry`: target breadcrumb (resolved context /
    node), scope, action→probability summary, combo count, enabled state, and a
    per-entry validation badge
  - operations: add (hands off to 7.2 authoring or an inline row editor), edit,
    duplicate, delete, enable/disable, and clear-all; editing any entry writes back
    to `spot.locks` and marks dirty
  - an enabled/disabled flag per entry is a UI convenience that simply includes or
    excludes the entry from the emitted `spot.locks` (disabled entries are retained
    in the document for quick toggling but never reach the solver)
- Inline validation: run the Phase 2 resolution/validation against the current
  lowered graph as edits happen, map each `lock_validation_error_kind` to a
  human-readable per-entry message, and **block the solve action** while any
  enabled lock is invalid (consistent with how the app already gates solves on
  invalid spots)
- Conflict/override surfacing: show where a combo lock overrides a range lock
  (Phase 2/5 diagnostics) directly in the list so the effective constraint is
  obvious

### Phase 7.4: Locked-state display, derived model, exports, metadata

**Outcome:** results make locked frequencies unmistakable and locks flow through
every downstream view and export.

- Extend `viewmodels/strategy_view_model.{h,cpp}` structs
  (`strategy_action_frequency`, `strategy_hand_row`, `strategy_matrix_cell`,
  `strategy_action_card`) with a per-action `locked` flag and enforced value
  sourced from `solved_node.lock_state` and the enforced frequencies; populate them
  in `make_strategy_view_model`
- Render locked entries distinctly in `strategy_explorer` (badge/color) across the
  node action table, matrix cells, hand table, and detail inspector, so a user can
  always tell locked from solved frequencies
- Carry `lock_state` from `solver/solution_store.{h,cpp}` into
  `solution_table_state` / the node model so the CSV export and the compare helpers
  in `study/study_workflow.cpp` reflect which frequencies were locked. (The
  locked-vs-unlocked *compare mode* itself is roadmap item 7 and is not built here;
  this phase only ensures the data it will need is present and labeled.)
- The UI must **never infer lock state from displayed strategy frequencies**: it
  always sources locked/free state from `spot.locks` / the artifact `lock_state`,
  so a solver-derived `0.0` and a user-locked `0.0` stay distinguishable (the exact
  distinction node locking exists to expose)
- Surface lock presence and a summary (counts per scope, affected
  contexts/combos, `locks_hash`) in `spot_summary_helpers.cpp` and the solve
  metadata panels, consistent with how convergence metadata is already surfaced

### Exit criteria

- a user can build, edit, reorder-independently, duplicate, enable/disable, and
  delete any number of range- and combo-scoped locks from the lock-management
  panel and from node authoring, and see live validation per lock
- authored locks drive the solve: locked frequencies are enforced in the result
  and visually distinguished from solved ones in every strategy view
- invalid locks are rejected in-app with the Phase 2 error-kind diagnostics and
  block the solve before it starts
- saving and reloading a document preserves the full lock set (including disabled
  entries) and the artifact lock provenance exactly, across range, combo, and mixed
  sets

## Cross-phase risks and mitigations

| Risk | Impact | Mitigation |
|---|---|---|
| Constrained regret uses full node value as baseline | Solver illegally moves mass off locked actions | Free-simplex comparator `V_free`; update free regrets only (Phase 3) |
| Partial locks treated as full distributions | Core use case ("never raise") impossible | `action_constraints` are partial by design; residual to free actions (Phase 1/2) |
| Average strategy drifts from constraint | Misleading output | Accumulate actual `sigma`; no special extraction; final assertion (Phase 3) |
| Ambiguous `strategy_context_id` target | Lock applied inconsistently | Context invariant; fall back to infoset/node targeting (Phase 2) |
| Combo lock on shared-strategy (scalar) regret state | Lock corrupts other combos sharing the vector | Regret-granularity invariant (Phase 2); per-combo refinement of locked infosets; reject if not combo-addressable (Phase 3) |
| Per-combo refinement changes free combos at refined infosets | Surprising behavior vs scalar solve | Stated as intended per-hand semantics; confined to refined infosets; diagnostics + dedicated test (Phase 3/5/6) |
| Combo locks blow up memory/time | Slow/large solves | Native per-combo tables in HU regime; refine only locked infosets elsewhere; sorted flat per-infoset index + `has_lock` gate, no hash maps (Phase 3) |
| Isomorphism changes a combo lock's meaning | Silent semantic change | Reject inexact combo locks under isomorphism (Phase 2/4) |
| No-lock path regresses | Breaks existing solves | Empty-set guard + fast path + byte-identical test (Phase 3/6) |
| Hash includes presentation fields or raw input order | Spurious identity changes | Hash effective post-resolution semantics only; exclude `lock_id`/warnings/input ordering (Phase 4) |
| Effective function keyed by context only collapses node/infoset locks into context-wide | Wrong semantics + wrong `locks_hash` | Key `effective_lock_set` by canonical target identity + `target_scope`; target-coverage invariant (representation layers / Phase 2/4) |
| Adding `locks_hash` to the hash stream drifts no-lock `solve_hash` | Breaks no-lock identity promise | Empty effective set contributes no bytes; legacy `solve_hash` preserved when empty (Phase 4) |
| `node_id`/`infoset_id` in hash not stable across graph rebuilds | Same poker lock hashes differently | Hash a semantic target descriptor, not raw CSR ids (representation layers / Phase 4) |
| Quantize-after-validate mismatch | Accept then enforce different mass | Fixed quantize-then-validate-then-hash order (Phase 1) |
| UI silently drops locks on save | Lost constraints, mismatched re-solves | Round-trip full `solve_spot.locks`/schema-5 through `document_json`; save/reload regression over range/combo/mixed/disabled sets (Phase 7.1) |
| Lock editor diverges from canonical model | UI and CLI disagree on semantics | Editor writes the same `spot.locks` the CLI consumes; inline Phase 2 validation; solve gated on invalid locks (Phase 7.2/7.3) |

## Suggested engineering sequence

1. `cfr/locks/lock_model.h` + `"locks"` JSON parsing + quantization (ties-to-even)
   + full-lock normalization + canonical serialization (Phase 1)
2. `cfr/locks/lock_validation.{h,cpp}` resolution + partial-aware validation +
   context invariant + regime/granularity classification + conflict rules
   (Phase 2)
3. **write the exact constrained-CFR contract test first** (from "Core
   semantics", with the `V_free=4` / `V_node=5.5` golden numbers) so the runtime
   is implemented against a failing reference (Phase 6 test, authored here)
4. `runtime_lock_view` + constrained strategy materialization (hook 1) +
   free-simplex regret update (hook 2) in the per-combo HU hooks
   (`normalize_combo_action_table` / `update_combo_tables`) and the scalar hooks
   (`compute_regret_matching_strategy` / regret backup) + scalar-regime per-combo
   refinement; make the contract test pass (Phase 3)
5. `locks_hash` (effective post-resolution semantics) + `solve_hash` folding +
   schema-5 artifact + `lock_state` annotation (Phase 4)
6. diagnostics + CLI formatting (diagnostic footprint only, no compare engine)
   (Phase 5)
7. remaining unit/integration/determinism/performance matrix + named edge-case
   tests + rollout gate (Phase 6)
8. Qt app full lock-management editor (Phase 7): transport/persistence/solve
   plumbing (7.1), node authoring (7.2), lock-management panel with per-entry
   validation/enable/duplicate/delete (7.3), locked-state rendering + derived
   model/exports/metadata (7.4)

## Definition of done

Node locking is done when a user can submit a `"locks"` payload containing range-
or combo-scoped **action constraints** (partial or full), receive deterministic
pre-solve validation, and run a constrained CFR+ solve in which locked action
probabilities are enforced exactly and regret minimization operates over the
remaining feasible (free) strategy dimensions. **Combo (per-hand) locks are fully
supported, not a half measure**: they are native in the per-combo vectorized
heads-up regime and brought to parity in the scalar regime by per-combo refinement
of locked infosets, governed by the regret-state granularity invariant. The actual
behavioral strategy used during traversal is accumulated into the normal
average-strategy tables, so no special extraction path is required. The resulting schema-5 artifact records the
canonical lock semantics, `locks_hash`, `solve_hash`, and per-node `lock_state`,
while an empty lock set produces byte-identical solver output and remains within
the established no-lock performance budget. The Qt hold'em app ships a **full
lock-management editor**: a user can author locks at any decision point, manage the
complete set of range- and combo-scoped locks in a dedicated panel (add, edit,
duplicate, enable/disable, delete) with live per-lock validation, solve, see locked
frequencies enforced and visually distinguished from solved ones across every
strategy view, and save and reload the document with the full lock set and artifact
lock provenance intact.
