# Hold'em Solver Next Steps

This roadmap focuses on the highest-impact solver functionality needed to move
Zeta closer to a GTO Wizard-style postflop analysis tool. The priority is not to
add every UI feature first; it is to build the solver capabilities that unlock
useful, accurate analysis.


## 1. Per-street betting expansion for flop/turn solves (delivered)

**Status: Delivered.** Flop and turn spots now lower to an exact multi-street
public game that interleaves a betting round on every street with the chance
deals (flop bet round -> turn card -> turn bet round -> river card -> river bet
round -> showdown), and a single unified CFR+ solve path traverses river, turn,
and flop. The vectorized/templated river terminal evaluation is now only a
low-level leaf optimisation, not a distinct solve path, so `iterations`
meaningfully drives convergence for flop/turn spots. Multi-way (N > 2) non-river
spots are rejected with a clear unsupported-solver error. Pre-build memory
budgeting and exact suit-isomorphism reduction bound the tree size, and an
opt-in dynamic action-pruning policy (default off, bit-identical to the exact
solver when disabled) trades a bounded strategy/EV tolerance for speed on large
trees.

The runout / chance-expansion infrastructure this built on -- blocker-aware
public-card enumeration, the public-state / chance-event / runout identity model
and registries, per-runout terminal cache reuse, deterministic board
partitioning with multi-worker determinism, and schema-3 runout-aware
persistence and inspection -- remains in place and is exercised by the unified
solve.

Delivered tests:

- convergence / correctness tests for small flop and turn spots that assert
  betting frequencies and per-hand EV (including a hand-computed golden nuts EV)
- a test asserting `iterations` changes non-river solve output once betting exists
- a multi-way (N > 2) non-river test asserting the supported / rejected contract
- an end-to-end test asserting the lowered flop/turn graph produces exactly the
  legal set of streets, betting nodes, and runouts (each legal board once)
- pruning-tolerance tests bounding the pruned solve to the exact solve, and a UI
  test asserting completed flop/turn solves surface real `"cfr+"` solve metadata

Board abstraction/bucketing for large multiway solves remains deferred until a
multiway solve path is in place.

The full implementation record, including the four-step breakdown and per-step
status, is in [`per_street_betting_plan.md`](per_street_betting_plan.md); every
step is marked done.

Why it matters: serious postflop analysis needs real flop and turn solves with
betting, not single-river terminal states or check-down equity. This is also the
foundation for aggregated reports by turn/river class.

## 2. Robust convergence and exploitability reporting (delivered)

**Status: Delivered.** Every solve now carries user-facing confidence signals
alongside the strategy. For supported heads-up abstractions (vectorized river
root-actor and multi-street flop/turn solves) the solver computes an exact
best-response exploitability -- half of NashConv, expressed in EV (chip) units --
by backing up a per-combo best response against each seat's average strategy and
reporting the per-seat best-response gaps, the aggregate exploitability, and its
fraction of the gross pot. Multiway spots, where an exact best response is
expensive, instead report a normalized average-regret metric (mean max positive
regret per infoset divided by iterations) and flag that exact exploitability is
unavailable.

Exploitability measurement is opt-in (`runtime.convergence.measure_exploitability`,
default off) so existing solve timings are unchanged. When enabled, a positive
`measurement_interval` records a convergence curve (iteration, metric, elapsed
time) capped at `max_curve_samples`, and a positive `target_exploitability` stops
the solve early once the measured exploitability drops to or below the threshold,
recording the reduced iteration count and a `reached_target` flag.

Every solve -- regardless of the measurement toggle -- records reproducible input
hashes (tree, range, board, betting-policy, solver-config, and a combined solve
hash; stable FNV-1a digests serialized as hex) so two solves can be compared for
equivalence without replaying them, plus warnings whenever a solve is
abstraction-limited, multiway/normalized-regret-only, Monte-Carlo sampled, or
approximate due to lossy card isomorphism or dynamic pruning. All fields
round-trip through the schema-3 artifact JSON.

Delivered tests:

- heads-up exploitability is reported, non-negative, equals NashConv/2, and is
  near-zero for a hand-computed nuts spot; it decreases as iterations increase
- the convergence curve is populated and ordered for a positive interval, and the
  retained sample count honours `max_curve_samples`
- a quality target stops the solve early and records the reduced iteration count
- multiway solves report a normalized-regret metric with exploitability
  unavailable and the corresponding warning
- input hashes are stable across identical solves and change when the board,
  range, betting policy, or iteration budget changes
- the reporting runtime knobs parse from the spot JSON (defaulting off, rejecting a
  negative target), and hashes/convergence/warnings round-trip through artifact JSON

Why it matters: users need to know whether a strategy is stable enough to trust.
This is more valuable than simply running more iterations blindly.

## 3. Strategy and EV result surfaces (delivered)

**Status: Delivered.** Solved-node outputs now expose the full strategy/EV
inspection surfaces needed for postflop analysis workflows: per-node action
frequencies, per-hand strategy/EV/equity, range-level EV by player, action EV
and regret summaries, hand-category aggregation, and versioned solved-node JSON
payloads.

The solver should emit enough structured data for detailed inspection, not just
a flat hand/action table.

Core deliverables:

- per-node action frequencies
- per-hand strategy, EV, and equity
- range-level EV by player
- action EV and regret summaries
- hand-category aggregation: pair, two pair, draw, blocker, showdown class
- JSON schema versioning for solved-node payloads

Why it matters: GTO-style analysis is driven by comparing frequencies and EVs at
each node. The UI can only become powerful if the solver artifact carries these
surfaces cleanly.

## 4. Node locking and strategy constraints

Node locking is one of the most valuable practical solver features.

Core deliverables:

- fixed action frequencies at selected nodes
- per-hand locks and range-level locks
- validation that locks match legal actions and hand domains
- re-solve from locked strategy constraints
- artifact metadata that records all locks
- UI/CLI schema for lock input

Why it matters: users often want to answer exploitative questions: "What if
villain over-folds?", "What if BTN never raises?", or "How should OOP respond to
this population strategy?"

Detailed phased implementation plan: [`node_locking_plan.md`](node_locking_plan.md).

## 5. Range editing beyond preflop syntax

The current PokerStove parser is a good base, but solver workflows need richer
postflop range tools.

Core deliverables:

- postflop category filters, such as top pair, flush draw, open-ender, blocker
- suit-aware filters
- weighted range algebra: add, subtract, intersect, scale, normalize
- exact-combo exclusions
- import/export of solved-node ranges
- range-diff view between two nodes or strategies

Why it matters: users need to construct and inspect ranges by hand properties,
not only preflop class notation.

## 6. Saved spot library and solve cache

A GTO Wizard-like tool becomes useful when spots are reusable and comparable.

Core deliverables:

- canonical spot hash from board, ranges, stacks, rake, tree, and solver settings
- local solved-spot cache keyed by that hash
- searchable spot/study library
- tags, pinned studies, and recent solves
- cache compatibility checks when solver versions or tree schemas change
- fast open of previous results without re-solving

Why it matters: users should build a library of solved spots instead of treating
each solve as disposable.

## 7. Compare mode and reports

After solving, the highest-value analysis is comparison.

Core deliverables:

- compare two strategies at the same node
- compare locked vs unlocked solves
- aggregate frequency deltas
- EV-loss reports for alternative actions
- best-action and mixed-action summaries
- exportable CSV/JSON reports

Why it matters: practical study is often about differences: one sizing tree vs
another, one range assumption vs another, or equilibrium vs locked population
behavior.

## 8. Trainer and drill mode

Training should come after the solver result surfaces are strong.

### Architectural Assessment: Dedicated Engine/Module

Item 8 should have its own dedicated engine/module (`zeta::trainer` / `libzeta_trainer`), designed as a lightweight **Training & Simulation Engine** that consumes solved game artifacts rather than another numerical CFR solver.

To keep Zeta clean, performant, and maintainable, Item 8 should not duplicate CFR/solver logic, nor should it be tightly coupled to the UI layer.

#### 1. Why a Separate Engine/Module is Necessary

A trainer mode has entirely different responsibilities, runtime lifecycles, and state management compared to the CFR solver engine:

| Characteristic | Core CFR Solver Engine (`zeta::solver`) | Trainer / Drill Engine (`zeta::trainer`) |
| :--- | :--- | :--- |
| **Primary Goal** | Compute equilibrium strategies, EVs, and counterfactual regrets via CFR+ iterations. | Sample game states, validate user choices, track drill sessions, and evaluate EV loss. |
| **Workload Type** | Compute-heavy, multi-threaded numerical batch processing. | Interactive, low-latency, stateful turn-by-turn game simulation and scoring. |
| **Inputs** | Raw game parameters (ranges, board, tree configuration, stopping criteria). | Solved game trees / artifacts (from Item 3 & Item 6), drill configuration/filters. |
| **Outputs** | Strategy tensors, reach probabilities, exploitability metrics, EV tables. | Feedback metrics (EV loss, blunders, frequency divergence), user session stats, drill queues. |

Placing drill logic directly into the solver engine would pollute the numerical CFR core, while placing it directly inside Qt UI widgets would make drills impossible to test headless, script via CLI, or reuse across web/desktop frontends.

#### 2. Core Responsibilities of the Trainer Engine

The trainer engine should be structured around three primary functional subsystems:

- **A. Spot & Node Sampling Engine:**
  - **Tree Traversal & Node Filtering:** Filter nodes across solved spots by criteria (e.g., Street: Turn, Pot Type: Single Raised Pot, Position: OOP Check-Raise, Texture: Monotone boards, Hand Class: Nut Flush Draw).
  - **Probability-Weighted State Sampling:** Sample deal runouts and villain actions according to equilibrium reach probabilities (or custom villain profile distributions).
- **B. Decision Evaluator & Scoring Engine:**
  - **EV Loss Calculation:** Compute $\Delta\text{EV} = \text{EV}(\text{optimal}) - \text{EV}(\text{action chosen})$.
  - **Mixed Frequency Scoring:** Score user decisions based on strategy distribution overlap (e.g., comparing user's chosen frequency vs. solver's equilibrium mixing frequency).
  - **Blunder & Inaccuracy Classification:** Classify decisions using standard thresholds (e.g., Best Action, Acceptable Alternative, Inaccuracy (< 0.25 bb), Mistake (0.25 - 1.0 bb), Blunder (> 1.0 bb)).
- **C. Session & Practice Progression (SRS) Engine:**
  - **Spaced Repetition System (SRS):** Track error history per spot category/pattern and requeue failed hands for future practice sessions.
  - **Session State & History:** Record history of drills, user streaks, aggregate EV lost, and accuracy percentages.

#### 3. Recommended Architectural Integration in Zeta

```text
┌────────────────────────────────────────────────────────┐
│                   User Interfaces                      │
│        (Qt GUI Drill Widget / CLI Interactive Tool)    │
└───────────────────────────┬────────────────────────────┘
                            │ (Events & User Actions)
                            ▼
┌────────────────────────────────────────────────────────┐
│             Trainer Engine (`zeta_trainer`)            │
│  - Spot & Hand Sampler (Board/Runout/Action sampling)  │
│  - Session Manager & SRS / Mistake Queue               │
│  - Evaluation & Scoring (EV Loss, Blunder flags)       │
└──────────────┬──────────────────────────┬──────────────┘
               │                          │
               ▼                          ▼
┌──────────────────────────────┐ ┌──────────────────────────────┐
│  Spot Library & Cache        │ │  Result Surfaces & Data      │
│  (`zeta_library` / Item 6)   │ │  (`zeta_artifacts` / Item 3) │
│  - Pre-solved trees          │ │  - Per-node strategy / EV    │
│  - Canonical spot queries    │ │  - Hand categories / equity  │
└──────────────────────────────┘ └──────────────────────────────┘
```

### Core Deliverables

- Separate standalone engine/library (`libzeta_trainer` / `zeta::trainer`) decoupled from CFR solving and UI widgets
- Spot & node sampling across solved studies with probability weighting (deal runouts, villain actions)
- Evaluation and scoring by EV loss, strategy mixing fidelity, and blunder/mistake classification
- Filter drills by street, position, pot type, board texture, or hand category
- Session tracking and Spaced Repetition System (SRS) over missed spots and blunder queues

Why it matters: this turns solver output into an interactive study and practice workflow, while maintaining modularity and headless testability by consuming solved artifacts without solver coupling.

## Recommended implementation order

1. Per-street betting expansion for flop/turn solves. (delivered)
2. Robust convergence and exploitability reporting.
3. Strategy and EV result surfaces. (delivered)
4. Node locking.
5. Postflop range tools.
6. Saved spot library and solve cache.
7. Compare mode and reports.
8. Trainer/drill mode.

The first four items are the core solver foundation; items 1 through 3 are now
delivered, leaving node locking as the final remaining core-solver foundation
step. Items five through eight are what make the solver feel like a complete
analysis product.
