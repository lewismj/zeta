# Node Locking

A node lock fixes some of a player's action frequencies at a decision and leaves the
rest free, so a re-solve shows how the other player exploits that play: "what if
villain over-folds to this bet?", "what if BTN never raises here?", "how should OOP
play against this population line?".

Locks are part of the spot (`locks` in the spot JSON), so they are saved with
documents, appear in the Spot JSON editor, and mark a solved document stale when
they change. The [CLI guide](cli_usage.md#node-locks-locks-array) has the field
reference; the [UI guide](ui/user_guide.md#node-locks) covers the editor.

## Format

```json
"locks": [
  { "path": [], "actions": { "check": 1.0 } },
  { "path": ["check", "bet_75"], "actions": { "raise_150": 0.0 } },
  { "path": ["bet_75", "*", "check"], "hands": "AA,KK,AKs",
    "actions": { "bet_75": 0.6, "check": 0.4 } }
]
```

- **`path`**: action labels from the root to the locked decision; `[]` is the root.
  A step after a street ends names the dealt card (`"Kh"`), or `"*"` for every card,
  which locks the matching decision on every runout. Under card isomorphism a card
  names its whole suit class, and only the class's representative card is accepted.
- **`hands`**: optional range text. Absent, the lock covers the whole range. Present,
  it overrides a whole-range lock at the same node for those hands (the hand's
  constraint is replaced, not merged). Hands outside the seat's range are ignored.
- **`actions`**: only the actions being fixed. `{"raise_150": 0.0}` pins the raise to
  zero and leaves the other actions free. Frequencies must be in [0, 1] and add up
  to at most 1; if every action is listed they must add up to 1. Frequencies are
  rounded to 1e-6, so the JSON text cannot change a solve's identity.

### Action labels

Every action in a solved artifact carries its label: `fold`, `check`, `call`,
`all_in`, `bet_<pct>` and `raise_<pct>`. `<pct>` is the chips the action adds as a
percentage of the gross pot before it (the spot's `gross_pot` plus every chip
committed since the root). If two actions at one node would share a label, the later
ones get `.2`, `.3`, ... so labels are unique. Chance actions are labelled `deal`,
with the cards in `dealt_cards`; `cli::node_lock_path` turns any solved node into the
path that reaches it.

### Validation

A lock that does not fit the tree is rejected before any iteration runs. The error
names the lock by its 1-based position:

- an unknown action label, or a chance step that is not a dealt card or `"*"`
- a path that ends where a card is dealt, or at the end of the hand
- frequencies that are out of range or add up to more than 1, or that do not add up
  to 1 when every action is listed
- `hands` that do not parse, or that match nothing in the acting seat's range
- a second whole-range lock at the same node, or hand locks that overlap
- a lock on specific hands with card isomorphism on (it breaks suit symmetry)

## How the solver applies a lock

At a locked node, the acting seat's actions split into a locked set `L`, each with a
fixed probability `p_a`, and a free set `F`. With `S = sum p_a` and residual
`r = 1 - S`:

```
sigma_a = p_a              a in L
sigma_a = r * rm_a         a in F, rm = regret matching over F only
                           (uniform over F when no free action has positive regret)
```

`sigma` is the only behavioural strategy: it drives reach, the value backup and the
strategy-sum accumulation, so opponents best-respond to the constrained play.

Free actions update their regrets against the free-only value, scaled by `r`:

```
R_a = max(0, R_a + r * (V_a - V_free))      a in F, V_free = sum_{b in F} rm_b * V_b
```

Locked actions never accumulate regret, and with `r = 0` nothing does. Using the
free-only value as the comparator keeps regret from "learning" to move probability
off a locked action; the factor `r` makes the stored value the regret of the
restricted game.

The average strategy writes locked actions as exactly `p_a` and free actions as `r`
times their normalised strategy sums, so the lock holds even at nodes the seat never
reaches. Dynamic pruning never deactivates actions at a locked node.

**Exploitability** is measured in the locked game: at its own locked node a seat's
best response keeps the locked frequencies and puts the residual on its best free
action (`V = sum_{a in L} p_a * V_a + r * max_{a in F} V_a`). The artifact warns
that this is so.

With no locks the solver's arithmetic is unchanged: every lock hook sits behind a
null lock view, as pruning does.

## In the artifact

- `solver.locks`: the locks the solve honoured, in canonical form (actions sorted,
  frequencies rounded).
- `solver.hashes.locks`: an FNV-1a hash of the canonical locks (hands by the combos
  they name, so `AK` and `AKs,AKo` hash alike). It is `0` without locks and only
  then left out of `solver.hashes.solve`, so a solve without locks keeps its hash.
- `solved_nodes[*].lock_indices`: indices into `solver.locks` of the locks applied at
  that node (present only on locked nodes).
- `hand_strategy.locked`: true when the hand's play at that node is fixed.

## Not supported

- Soft or penalty constraints.
- Locking chance frequencies or payoffs.
- Hand locks under card isomorphism.
