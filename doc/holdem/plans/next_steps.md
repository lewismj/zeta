# Hold'em Solver Next Steps

This roadmap focuses on the highest-impact solver functionality needed to move
Zeta closer to a GTO Wizard-style postflop analysis tool. The priority is not to
add every UI feature first; it is to build the solver capabilities that unlock
useful, accurate analysis.


## Delivered

The core solver foundation is in place:

- **Multi-street solving.** Flop and turn spots solve as one exact game: a betting
  round on every street, chance deals between them, exact suit isomorphism, a memory
  budget checked before the tree is built, and optional dynamic pruning. See
  [core algorithms](../core_algorithms.md).
- **Convergence reporting.** Heads-up solves report best-response exploitability
  (half of NashConv, in chips) with an optional convergence curve and an early stop
  at a target; multiway solves report normalised average regret. Every artifact
  carries input hashes and warnings. See the [CLI guide](../cli_usage.md).
- **Strategy and EV surfaces.** Every solved node carries action frequencies,
  per-hand strategy and EV in chips, range EV by seat, action EVs and hand-category
  summaries, all shown in the Strategy Explorer.
- **Node locking.** Fixed action frequencies at chosen decisions, for the whole range
  or named hands, re-solved so the other player exploits them. See
  [node locking](../node_locking.md).
- **Speed and control.** Solves use every worker thread (with identical results for
  any thread count) and can be cancelled at any stage.

The items below turn this into a complete analysis tool.

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

| Characteristic | CFR solver (`cli/combo_cfr.h`) | Trainer / Drill Engine (`zeta::trainer`) |
| :--- | :--- | :--- |
| **Primary Goal** | Compute equilibrium strategies, EVs, and counterfactual regrets via CFR+ iterations. | Sample game states, validate user choices, track drill sessions, and evaluate EV loss. |
| **Workload Type** | Compute-heavy, multi-threaded numerical batch processing. | Interactive, low-latency, stateful turn-by-turn game simulation and scoring. |
| **Inputs** | Raw game parameters (ranges, board, tree configuration, stopping criteria). | Solve artifacts (from the solver and the spot library, item 6), drill configuration/filters. |
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
│  (item 6)                    │ │  (solve artifacts)           │
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

5. Postflop range tools.
6. Saved spot library and solve cache.
7. Compare mode and reports.
8. Trainer/drill mode.

Range tools come first because locks, compare mode and the trainer all want to name
hands by what they make on the board rather than by preflop class.
