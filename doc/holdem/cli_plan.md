# Hold'em CLI MVP Plan

## Goal

Create a deterministic CLI workflow that produces a versioned solver artifact for downstream UI consumption, with no UI and no CFR internals exposed.

Artifact boundary:

`spot -> artifact -> visualization`

---

## Command model: subcommands from day one

Use a command family instead of a single flag-only executable.

### `solve`

```bash
zeta-solve solve \
    --spot river_bvb_sr.json \
    --iterations 100000 \
    --output solution.json
```

Responsibilities:

1. Load problem definition
2. Build game graph
3. Run CFR
4. Extract strategy
5. Serialize artifact

### `validate`

```bash
zeta-solve validate solution.json
```

Validate artifact independently of a running solver.

### `dump`

```bash
zeta-solve dump solution.json
```

Human-readable strategy inspection.

This structure naturally supports future commands:

`benchmark`, `profile`, `checkpoint`, `replay`

---

## Artifact schema (define first)

### Principles

1. Versioned contract for forward compatibility
2. Solver metadata for reproducibility/debugging
3. Action-aware strategy format (not single-frequency only)

### MVP shape

```json
{
  "schema_version": 1,
  "game": "holdem",
  "street": "river",
  "players": [
    "BTN",
    "BB"
  ],
  "board": [
    "As",
    "Kd",
    "7c",
    "4h",
    "2s"
  ],
  "solver": {
    "algorithm": "cfr+",
    "iterations": 100000,
    "timestamp": "2026-08-01T19:47:11Z",
    "git_revision": "abc1234"
  },
  "strategy": [
    {
      "hand": "AhKh",
      "strategy": [
        {
          "action": "bet_75",
          "frequency": 0.73
        },
        {
          "action": "check",
          "frequency": 0.27
        }
      ],
      "ev": 1.24
    },
    {
      "hand": "QcJc",
      "strategy": [
        {
          "action": "bet_75",
          "frequency": 0.12
        },
        {
          "action": "check",
          "frequency": 0.88
        }
      ],
      "ev": -0.15
    }
  ]
}
```

---

## `dump` output format

Prefer grouped range-style table output:

```text
Hand       Bet       Check      EV
------------------------------------
AA         100%       0%       +2.31
AKs         74%      26%       +1.44
QJs         32%      68%       +0.20
76s          0%     100%       -0.41
```

If more than two actions exist, add columns per action label.

---

## `validate` checks

Split validation into two tiers.

### Structural (always, solver-independent)

1. Schema version is recognized
2. Legal cards only
3. No duplicate cards
4. Per-hand action probabilities sum correctly
5. Global frequencies/probabilities are in valid ranges

### Solver consistency (optional)

1. Infoset consistency
2. Strategy table dimensions
3. Regret table dimensions
4. Checkpoint compatibility

---

## Timing output standard

Standardize timed phases (especially for `solve`):

```text
solve:
  graph_build        12.4ms
  terminal_cache      4.1ms
  cfr_iterations      8.72s
  extraction         13.0ms
  serialization       2.0ms

total                8.75s
```

Apply the same timing style to `validate` and `dump` where meaningful.

---

## Delivery order

1. Finalize schema definition
2. Implement `solve` producing schema-compliant artifacts
3. Implement `validate` (structural first, solver consistency optional)
4. Implement `dump` range-style inspection

---

## Non-goals (MVP)

1. UI/Qt integration
2. Range editing
3. Tree navigation
4. Real-time solving
