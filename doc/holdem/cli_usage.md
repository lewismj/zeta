# zeta-solve CLI

`zeta-solve` solves Texas Hold'em flop, turn, and river spots with CFR+, validates saved
artifacts, and renders strategy tables.

## Commands

### solve

```sh
zeta-solve solve --spot <spot.json> --iterations <n> --output <solution.json>
```

Runs CFR+ and writes a solution artifact JSON file.

| Flag               | Required | Description                       |
| ------------------ | -------- | --------------------------------- |
| `--spot <path>`    | Yes      | Spot description JSON             |
| `--iterations <n>` | No       | CFR+ iterations (default: `1000`) |
| `--output <path>`  | Yes      | Output artifact JSON path         |

### validate

```sh
zeta-solve validate <solution.json>
```

Checks artifact structure and numeric validity.

### dump

```sh
zeta-solve dump <solution.json>
```

Prints a fixed-width strategy table.

## Spot JSON

`board` is always required. Use the unified array-based format for all player counts.

### Multiway-capable format (2 to 7 players)

```json
{
  "players": ["BTN", "BB", "CO", "HJ", "UTG"],
  "board": ["Ah", "Kd", "Qc", "Jh", "2s"],
  "ranges": [
    "AA,AKs,AQo",
    "AA,KK,QQ,AKo",
    "QQ,JJ,TT,AKs",
    "JJ,TT,99,AQs",
    "TT,99,88,AJs"
  ],
  "gross_pot": 200.0,
  "rake": 0.0,
  "contributions": [40.0, 40.0, 40.0, 40.0, 40.0],
  "stacks": [250.0, 250.0, 250.0, 250.0, 250.0],
  "bet_fraction": 0.75,
  "betting_policy": {
    "fixed_pot_fractions": [0.75],
    "max_raises": 1,
    "min_bet_increment": 1.0,
    "all_in_threshold": 0.95
  },
  "max_history": 8,
  "public_state_id": 0,
  "root_actor": "BB",
  "hero_seat": "BTN"
}
```

The solver accepts either the `bet_fraction` single-bet field or the richer `betting_policy` object. If the policy object is omitted, the default is a single-size policy with `fixed_pot_fractions: [0.75]` and `max_raises: 1`.

### Fields

| Field               | Type     | Default         | Notes                                                                   |
| ------------------- | -------- | --------------- | ----------------------------------------------------------------------- |
| `players`           | string[] | `["BTN","BB"]`  | 2..7 player labels                                                      |
| `street`            | string   | `"river"`       | One of `flop`, `turn`, `river`                                          |
| `board`             | string[] | required        | Exactly 3 cards on flop, 4 on turn, 5 on river                          |
| `ranges`            | string[] | `["AA","AA"]`   | Must match player count                                                 |
| `gross_pot`         | number   | `100.0`         | Must be positive                                                        |
| `rake`              | number   | `0.0`           | Must be in `[0, gross_pot]`                                             |
| `contributions`     | number[] | `[50.0,50.0]`   | Must match player count                                                 |
| `stacks`            | number[] | `[100.0,100.0]` | Must match player count                                                 |
| `bet_fraction`      | number   | `0.75`          | Single-size shorthand; mirrored from the first policy fraction           |
| `betting_policy`    | object   | default single-size policy | `fixed_pot_fractions`, `max_raises`, `min_bet_increment`, `all_in_threshold` |
| `max_history`       | integer  | `8`             | Betting history cap                                                     |
| `public_state_id`   | integer  | `0`             | User-defined public-state discriminator (advanced; leave at `0` for single-board spots) |
| `root_actor`        | string or integer | first player | Player label (e.g. `"BB"`) or index of acting seat at root; the listed player acts first at the root node |
| `hero_seat`         | string or integer | first player | Player label (e.g. `"BTN"`) or index of the seat whose strategy rows appear in the artifact |

River spots solve for any player count; flop and turn spots are heads-up only.
Multiway terminals are evaluated exactly, by enumerating every compatible tuple of
opponent hands, so cost grows quickly with the number and width of the ranges.

When `hero_seat` is not the root actor, the hero's rows carry its EVs at the root
but an empty `strategy`, because the hero has no decision there; the per-node rows
in `solved_nodes` show every seat's decisions.

### Solver runtime knobs (`runtime` object)

Solves accept an optional `runtime` object that tunes the solver without changing
the game being solved. When it is omitted the solver runs single-threaded with an
auto-derived memory budget.

```json
{
  "street": "turn",
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c", "4h"],
  "ranges": ["AhKh", "QdJd"],
  "gross_pot": 100.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "betting_policy": {"fixed_pot_fractions": [0.75], "max_raises": 1},
  "runtime": {
    "worker_threads": 4,
    "memory_budget_bytes": 8589934592,
    "card_isomorphism": true,
    "allow_lossy_card_isomorphism": false,
    "dynamic_pruning": {
      "enabled": true,
      "prune_threshold": 0.01,
      "minimum_active_actions": 1,
      "reconsider_interval": 64
    },
    "convergence": {
      "measure_exploitability": true,
      "measurement_interval": 100,
      "target_exploitability": 0.5,
      "max_curve_samples": 128
    }
  }
}
```

| Field                                    | Type    | Default        | Notes                                                                                     |
| ---------------------------------------- | ------- | -------------- | ----------------------------------------------------------------------------------------- |
| `worker_threads`                         | integer | `1`            | Threads that evaluate terminal leaves in parallel (clamped to `1..64`); results are bit-identical for any value |
| `memory_budget_bytes`                    | integer | `0`            | Pre-build footprint cap; `0` derives `0.8 x` detected available memory (fallback 4 GiB)   |
| `card_isomorphism`                       | boolean | `false`        | Collapse suit-isomorphic turn/river boards (lossless only for suit-symmetric ranges)      |
| `allow_lossy_card_isomorphism`           | boolean | `false`        | Permit isomorphism even when ranges are not suit-symmetric (approximate)                  |
| `dynamic_pruning.enabled`                | boolean | `false`        | Opt-in approximate action pruning; when off the solve is bit-identical to the exact solve |
| `dynamic_pruning.prune_threshold`        | number  | `0.0`          | Reach-weighted positive-regret share below which an action is frozen (`>= 0`)             |
| `dynamic_pruning.minimum_active_actions` | integer | `1`            | Never prune an infoset below this many active actions (`>= 1`)                           |
| `dynamic_pruning.reconsider_interval`    | integer | `64`           | Iterations between prune/reactivate reconsideration passes (`>= 1`)                      |
| `convergence.measure_exploitability`     | boolean | `false`        | Opt-in best-response exploitability (heads-up) / normalized regret (multiway) reporting   |
| `convergence.measurement_interval`       | integer | `0`            | Iterations between convergence-curve samples; `0` measures only at the end                |
| `convergence.target_exploitability`      | number  | `0.0`          | Stop early once heads-up exploitability drops to/below this EV-unit threshold (`>= 0`, `0` disables) |
| `convergence.max_curve_samples`          | integer | `128`          | Cap on retained convergence-curve samples                                                 |

Dynamic pruning is an **explicitly approximate** policy: it trades a bounded
strategy-distance / EV error for speed on large trees. Frozen actions keep their
regret state (never reset) and their subtree is not traversed while inactive;
every `reconsider_interval` iterations the full action set is reconsidered so a
frozen action reactivates once its regret share recovers. Pruning preserves the
original action indexing, so persisted strategies stay aligned.

### Convergence and exploitability reporting

Every solve records reproducible input hashes (`solver.hashes`) and any applicable
approximation warnings (`solver.warnings`) regardless of the `convergence` block.
The reporting knobs above are **opt-in** and default off, so leaving the block out
does not change solve timing.

When `measure_exploitability` is enabled, heads-up solves (river, turn and flop)
compute an exact
best-response exploitability -- half of NashConv, in EV (chip) units -- against the
average strategy. Multiway solves instead report a normalized average-regret metric
and mark `exploitability_available: false`. A positive `measurement_interval`
additionally records a convergence curve; a positive `target_exploitability` stops
the solve once the measured heads-up exploitability reaches the threshold, and the
artifact's `solver.iterations` reflects the reduced count.

### Over-budget error

Before allocating the CFR tables, the solver estimates the multi-street footprint
from the graph shape and compares it to the memory budget. If the estimate
exceeds the budget the solve is refused with a `solver` error that names the
dominant cost dimension and the byte figures, and nothing is allocated. Raise
`memory_budget_bytes`, enable `card_isomorphism` for suit-symmetric spots, or
shrink the tree (`max_history`, `betting_policy`) to bring the footprint down.

## Artifact JSON schema

```json
{
  "schema_version": 6,
  "extraction_version": 1,
  "game": "holdem",
  "street": "turn",
  "players": ["BTN", "BB"],
  "board": ["Ah", "Kd", "Qc", "Jh"],
  "hero_seat": 0,
  "solver": {
    "algorithm": "cfr+",
    "iterations": 5000,
    "timestamp": "2026-08-02T10:00:00Z",
    "git_revision": "abc1234",
    "hashes": {
      "tree": "0x0123456789abcdef",
      "range": "0x0123456789abcdef",
      "board": "0x0123456789abcdef",
      "betting_policy": "0x0123456789abcdef",
      "solver_config": "0x0123456789abcdef",
      "solve": "0x0123456789abcdef"
    },
    "convergence": {
      "exploitability_available": true,
      "exploitability": 0.42,
      "exploitability_pot_fraction": 0.0042,
      "nash_conv": 0.84,
      "best_response_gap": [0.40, 0.44],
      "normalized_regret": 0.0,
      "target_exploitability": 0.0,
      "reached_target": false,
      "curve": [
        {"iteration": 100, "metric": 3.1, "elapsed_ms": 12.0},
        {"iteration": 200, "metric": 1.2, "elapsed_ms": 24.0}
      ]
    },
    "warnings": [
      "Strategy is an equilibrium only within the configured betting abstraction (bet sizings and max raises); it is not the full no-limit game."
    ]
  },
  "strategy": [
    {
      "combination_index": 3,
      "hand": "AsKs",
      "strategy": [
        {"action": "check", "frequency": 0.42},
        {"action": "bet_75", "frequency": 0.58}
      ],
      "ev": 1.234567,
      "counterfactual_value": 24.69134,
      "opponent_reach_mass": 20.0
    }
  ]
}
```

Value fields (schema 6):

- `ev` is in chips: the hand's net result over the whole hand (its contribution
  already in the pot included), averaged over the opponent hands it does not block.
- `counterfactual_value` is the unnormalised value the solver works with: the sum,
  over compatible opponent hands, of their reach times the payoff.
- `opponent_reach_mass` is the reach of those compatible opponent hands, so
  `ev * opponent_reach_mass == counterfactual_value`. A hand with no compatible
  opponent hand reports `0` for all three.
- In `solved_nodes[*].seat_values`, `matchup_reach_mass` is the sum of each hand's
  path reach times its opponent reach mass, and `conditional_range_ev` is the
  seat's range EV in chips: `reach_weighted_value / matchup_reach_mass`. The
  category summaries' `average_ev` is normalised the same way.
- Heads-up these are exact. Multiway, the opponent mass is the product of each
  opponent's compatible mass, which ignores card collisions between opponents.

Schema 5 artifacts reported unnormalised values in `ev` and are rejected.

Validation checks include:

1. Schema/game/street fields
2. Street-consistent unique board (3/4/5 cards)
3. 2..7 players and valid `hero_seat`
4. Unique hand rows with exactly one combo per row
5. Action frequencies in `[0,1]` summing to `1` (tolerance `1e-3`)
6. Finite EV values, with `ev * opponent_reach_mass` equal to `counterfactual_value`

The `solver.hashes` (FNV-1a digests as hex strings), `solver.convergence`
(exploitability / normalized-regret report and curve), and `solver.warnings`
(approximation notes) objects are always emitted. Parsing tolerates their absence,
so older artifacts without these fields still load. `exploitability`,
`exploitability_pot_fraction`, `nash_conv`, and `best_response_gap` are populated
only when `exploitability_available` is `true`; otherwise `normalized_regret`
carries the multiway quality signal.

When the solved graph payload is emitted, the top-level `root_node_id` field
identifies which `solved_nodes[*].node_id` is the decision root. Graph nodes are
numbered in DFS post-order, so the root is **not** node `0` (node `0` is the first
terminal leaf); consumers must use `root_node_id` rather than assuming `0`. It is
validated to reference an existing solved node whose `parent_node_id` is absent
(the parentless root).

## Examples

### Solve heads-up

```sh
zeta-solve solve --spot hu_spot.json --iterations 2000 --output hu_solution.json
```

### Solve 4-way river

```sh
zeta-solve solve --spot spot_4way_river.json --iterations 2000 --output solution_4way_river.json
```

`spot_4way_river.json`:

```json
{
  "street": "river",
  "players": ["BTN", "BB", "CO", "HJ"],
  "board": ["Ah", "Kd", "Qc", "Jh", "2s"],
  "ranges": ["AA,AKs,AQo", "AA,KK,QQ,AKo", "QQ,JJ,TT,AKs", "JJ,TT,99,AQs"],
  "gross_pot": 220.0,
  "rake": 0.0,
  "contributions": [55.0, 55.0, 55.0, 55.0],
  "stacks": [300.0, 300.0, 300.0, 300.0],
  "bet_fraction": 0.75,
  "max_history": 8,
  "public_state_id": 12,
  "root_actor": "BTN",
  "hero_seat": "BTN"
}
```

### Solve 3-way river

```sh
zeta-solve solve --spot spot_3way_river.json --iterations 2000 --output solution_3way_river.json
```

`spot_3way_river.json`:

```json
{
  "street": "river",
  "players": ["BTN", "BB", "CO"],
  "board": ["Ah", "Kd", "Qc", "7s", "2h"],
  "ranges": ["AA,AKs,AQo", "AA,KK,QQ,AKo", "QQ,JJ,TT,AKs"],
  "gross_pot": 180.0,
  "rake": 0.0,
  "contributions": [60.0, 60.0, 60.0],
  "stacks": [260.0, 260.0, 260.0],
  "bet_fraction": 0.75,
  "max_history": 8,
  "public_state_id": 9,
  "root_actor": "BTN",
  "hero_seat": "BTN"
}
```

### Solve 5-way

```sh
zeta-solve solve --spot spot_5way.json --iterations 5000 --output solution_5way.json
```

### Solve 4-way with exact combo ranges

```sh
zeta-solve solve --spot spot_4way_exact_combos.json --iterations 2000 --output solution_4way_exact_combos.json
```

`spot_4way_exact_combos.json`:

```json
{
  "street": "river",
  "players": ["BTN", "BB", "CO", "HJ"],
  "board": ["Ah", "Kd", "Qc", "Jh", "2s"],
  "ranges": ["AsKh", "QdJd", "Tc9c", "8s8h"],
  "gross_pot": 160.0,
  "rake": 0.0,
  "contributions": [40.0, 40.0, 40.0, 40.0],
  "stacks": [220.0, 220.0, 220.0, 220.0],
  "bet_fraction": 0.75,
  "max_history": 8,
  "public_state_id": 21,
  "root_actor": "BTN",
  "hero_seat": "BTN"
}
```

### Validate and inspect

```sh
zeta-solve validate solution_5way.json
zeta-solve dump solution_5way.json
```

## Card and range syntax

- Cards: rank + suit, e.g. `As`, `Td`, `7c`
- Ranges: PokerStove-style text (see [range_parser.md](range_parser.md))

## Environment variable

| Variable            | Purpose                                                |
| ------------------- | ------------------------------------------------------ |
| `ZETA_GIT_REVISION` | Stored in `solver.git_revision` in the output artifact |

## Building from source

```sh
cmake --build cmake-build-release-wsl-clang
```
