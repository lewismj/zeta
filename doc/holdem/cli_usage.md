# zeta-solve CLI

`zeta-solve` is a command-line solver for heads-up Texas Hold'em river spots.
It reads a spot description from a JSON file, runs CFR+, and writes a solution
artifact to a second JSON file.  The artifact can then be validated for
structural correctness or rendered as a human-readable strategy table.

## Build

```sh
cmake --build cmake-build-release-wsl-clang
```

The binary is written to `cmake-build-release-wsl-clang/zeta/tools/holdem/zeta-solve`.

## Subcommands

### solve

Run CFR+ on a spot and write a solution artifact.

```
zeta-solve solve --spot <spot.json> --iterations <n> --output <solution.json>
```

| Flag | Required | Description |
|---|---|---|
| `--spot <path>` | Yes | Path to the spot description JSON file |
| `--iterations <n>` | No | Number of CFR+ iterations (default: 1000) |
| `--output <path>` | Yes | Path to write the solution artifact JSON |

On success the tool prints a timing summary to stdout and exits 0.

```
solve:
  graph_build        0.123ms
  cfr_iterations     47.2ms
  extraction         0.018ms
  serialization      0.003ms

total                47.344ms
```

On failure an error message is printed to stderr and the exit code is 2.

### validate

Parse and structurally validate a solution artifact.

```
zeta-solve validate <solution.json>
```

Checks performed:
- `schema_version` equals 1
- `game` is `"holdem"`, `street` is `"river"`
- `board` contains exactly 5 distinct, parseable cards
- Every strategy row references exactly one combo
- Action frequencies in each row are in `[0, 1]` and sum to 1 (tolerance 1e-3)
- All EV values are finite
- No duplicate hands

On success prints a timing summary and exits 0.  On failure prints the first
error found to stderr and exits 2.

### dump

Print the strategy table from a solution artifact.

```
zeta-solve dump <solution.json>
```

Parses and validates the artifact (same checks as `validate`), then renders the
OOP strategy as a fixed-width table:

```
Hand    check     bet 75%   EV
-----------------------------
AhKh    55%       45%       1.23
AhQh    72%       28%       0.98
...
```

Columns are: `Hand`, one column per action labelled by action type and pot
fraction, and `EV` (the expected value for OOP).

## Spot JSON format

The spot file describes the game state and solver parameters.  `board` and
`gross_pot` are required; all other fields are optional and fall back to the
defaults shown below.

```json
{
  "board":            ["Ah", "Kd", "Qc", "Jh", "2s"],
  "players":          ["BTN", "BB"],
  "oop_range":        "AA",
  "ip_range":         "AA",
  "gross_pot":        100.0,
  "rake":             0.0,
  "oop_contribution": 50.0,
  "ip_contribution":  50.0,
  "oop_stack":        100.0,
  "ip_stack":         100.0,
  "bet_fraction":     0.75,
  "max_history":      8,
  "public_state_id":  0
}
```

| Field | Type | Default | Description |
|---|---|---|---|
| `board` | string[5] | — | Five community cards; see [card syntax](#card-syntax) |
| `players` | string[2] | `["BTN","BB"]` | Labels for OOP and IP players |
| `oop_range` | string | `"AA"` | OOP range in PokerStove notation |
| `ip_range` | string | `"AA"` | IP range in PokerStove notation |
| `gross_pot` | number | — | Total pot before any river action (must be positive) |
| `rake` | number | `0.0` | Rake taken from the pot; must be in `[0, gross_pot]` |
| `oop_contribution` | number | `50.0` | Amount OOP has put into the pot |
| `ip_contribution` | number | `50.0` | Amount IP has put into the pot |
| `oop_stack` | number | `100.0` | OOP effective stack (non-negative) |
| `ip_stack` | number | `100.0` | IP effective stack (non-negative) |
| `bet_fraction` | number | `0.75` | Single bet size as a fraction of the gross pot (positive) |
| `max_history` | integer | `8` | Maximum betting-action history length |
| `public_state_id` | integer | `0` | Opaque public state identifier stored in the artifact |

## Card syntax

Cards use rank followed by suit, both case-insensitive for the rank, lowercase
for the suit:

```
2s  3h  4d  5c  6s  7h  8d  9c  Ts  Jh  Qd  Kc  As
```

Ranks: `2 3 4 5 6 7 8 9 T J Q K A`  
Suits: `s` (spades)  `h` (hearts)  `d` (diamonds)  `c` (clubs)

All five board cards must be distinct.

## Range syntax

Ranges follow PokerStove preflop notation.  See [range_parser.md](range_parser.md)
for the full grammar.  Common forms:

| Example | Meaning |
|---|---|
| `AA` | Pocket aces (all 6 combos) |
| `AKs` | Ace-king suited (4 combos) |
| `AKo` | Ace-king offsuit (12 combos) |
| `AK` | Ace-king any suitedness (16 combos) |
| `TT+` | Tens through aces |
| `A5s+` | Suited aces from A5s to AKs |
| `AA:0.5` | Pocket aces at 50% weight |
| `AsKh` | Exact combo |

## Environment variables

| Variable | Description |
|---|---|
| `ZETA_GIT_REVISION` | Git revision string recorded in the `solver.git_revision` field of the artifact |

## Solution artifact JSON schema (version 1)

```json
{
  "schema_version": 1,
  "game": "holdem",
  "street": "river",
  "players": ["BTN", "BB"],
  "board": ["Ah", "Kd", "Qc", "Jh", "2s"],
  "solver": {
    "algorithm": "cfr+",
    "iterations": 1000,
    "timestamp": "2024-01-15T10:30:00Z",
    "git_revision": "abc1234"
  },
  "strategy": [
    {
      "hand": "AsKs",
      "strategy": [
        {"action": "check",   "frequency": 0.55},
        {"action": "bet 75%", "frequency": 0.45}
      ],
      "ev": 1.234567
    }
  ]
}
```

| Field | Description |
|---|---|
| `schema_version` | Always `1` |
| `game` | Always `"holdem"` |
| `street` | Always `"river"` |
| `players` | Two player labels, OOP first |
| `board` | Five community cards |
| `solver.algorithm` | Always `"cfr+"` |
| `solver.iterations` | Number of CFR+ iterations run |
| `solver.timestamp` | ISO 8601 UTC solve timestamp |
| `solver.git_revision` | Value of `ZETA_GIT_REVISION` at solve time |
| `strategy` | One row per live OOP combo |
| `strategy[].hand` | Exact two-card combo label, e.g. `"AsKs"` |
| `strategy[].strategy` | Action frequencies summing to 1 |
| `strategy[].ev` | OOP expected value for this combo |

## Examples

### Solve a river spot

`spot.json`:

```json
{
  "board": ["Ah", "Kd", "Qc", "Jh", "2s"],
  "oop_range": "AA,KK,QQ,JJ,AKs,AKo",
  "ip_range":  "AA,KK,QQ,JJ,AKs,AKo",
  "gross_pot": 100.0,
  "oop_contribution": 50.0,
  "ip_contribution":  50.0,
  "oop_stack": 200.0,
  "ip_stack":  200.0,
  "bet_fraction": 0.75
}
```

```sh
zeta-solve solve --spot spot.json --iterations 2000 --output solution.json
```

### Validate an existing solution

```sh
zeta-solve validate solution.json
```

Expected output on a valid file:

```
validate:
  structural         0.42ms

total                0.42ms
```

### Inspect the strategy table

```sh
zeta-solve dump solution.json
```

```
Hand    check     bet 75%   EV
-----------------------------
AsAh    45%       55%       12.34
AsAd    45%       55%       12.34
...
```

### Record the git revision in the artifact

```sh
ZETA_GIT_REVISION=$(git rev-parse --short HEAD) \
  zeta-solve solve --spot spot.json --iterations 5000 --output solution.json
```
