# Hold'em Solver Next Steps

This roadmap focuses on the highest-impact solver functionality needed to move
Zeta closer to a GTO Wizard-style postflop analysis tool. The priority is not to
add every UI feature first; it is to build the solver capabilities that unlock
useful, accurate analysis.

## 1. Real betting-tree configuration

The biggest missing solver surface is a configurable betting abstraction.

Core deliverables:

- street-aware bet-size presets for flop, turn, and river
- separate IP/OOP size sets
- raise-size rules by previous bet size and stack depth
- all-in threshold handling
- check, bet, call, fold, raise legality derived from stack/pot state
- deterministic betting-tree hash included in solve artifacts and checkpoints

Why it matters: without configurable action trees, results are only useful for
toy spots. A GTO Wizard-like workflow needs repeatable trees such as single-size,
multi-size, geometric, overbet, and all-in-inclusive configurations.

## 2. Accurate flop/turn chance and board-runout handling

Zeta already has river terminal evaluation and chance-node machinery. The next
step is making runout enumeration a first-class solver surface.

Core deliverables:

- complete turn and river chance expansion from any flop/turn spot
- blocker-aware public-card enumeration from current board and live ranges
- deterministic board partitioning for parallel CFR
- optional board abstraction/bucketing for large multiway solves
- per-runout terminal cache reuse
- artifacts that preserve board/runout IDs for later inspection

Why it matters: serious postflop analysis needs flop and turn solves, not only
single-river terminal states. This is also the foundation for aggregated reports
by turn/river class.

## 3. Robust convergence and exploitability reporting

The solver needs user-facing confidence signals, not just iteration counts.

Core deliverables:

- exploitability or best-response estimate for supported heads-up abstractions
- normalized regret metrics for multiway where exact exploitability is expensive
- convergence curves over time
- stop conditions based on quality thresholds
- reproducible solve metadata: tree hash, range hash, board hash, policy hashes
- clear warnings when a solve is approximate, sampled, or abstraction-limited

Why it matters: users need to know whether a strategy is stable enough to trust.
This is more valuable than simply running more iterations blindly.

## 4. Strategy and EV result surfaces

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

## 5. Node locking and strategy constraints

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

## 6. Range editing beyond preflop syntax

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

## 7. Saved spot library and solve cache

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

## 8. Compare mode and reports

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

## 9. Trainer and drill mode

Training should come after the solver result surfaces are strong.

Core deliverables:

- sample decision nodes from solved studies
- ask the user for an action/frequency
- score by EV loss and frequency match
- filter drills by street, position, pot type, or hand category
- spaced repetition over missed spots

Why it matters: this turns solver output into study workflow, but it depends on
accurate per-node strategy and EV data first.

## Recommended implementation order

1. Betting-tree configuration.
2. Flop/turn chance expansion and runout cache reuse.
3. Structured strategy/EV artifact surfaces.
4. Convergence and quality reporting.
5. Node locking.
6. Postflop range tools.
7. Saved spot library and solve cache.
8. Compare mode and reports.
9. Trainer/drill mode.

The first four items are the core solver foundation. Items five through nine are
what make the solver feel like a complete analysis product.
