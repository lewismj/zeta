# Node Locking Plan

Node locking fixes some of a seat's action frequencies at chosen decision points and
re-solves, so the rest of the tree best-responds to that behaviour. It answers
exploitative questions such as "what if villain over-folds to this bet?", "what if
BTN never raises here?" or "how should OOP play against this population line?".

This plan replaces the earlier draft, which targeted the graph engine and the
`cfr/extraction/*` surfaces. Those were deleted when the engines were merged
(`aa8a777`). Every solve now runs the combo-vectorised CFR+ in `cli/solve_cli.h`,
which already keeps a separate strategy per hand at every decision point, so locks
need exactly one implementation.

## Solver facts this plan relies on

- Every player node has its own infoset (`set_infoset_id(node, next_infoset_id++)`
  in both lowerings), so "a decision point" and "an infoset" are the same thing.
- Regrets, strategy sums and strategies live in `combo_action_table`, indexed
  `(combo, infoset, action)`. Two hands at the same node already have independent
  strategies, so per-hand locks need no new storage.
- The current strategy is formed by `normalize_combo_action_table` (regret
  matching), the average strategy by the same function over strategy sums, and
  regrets and sums are updated in `update_combo_cfr_tables`. The best response used
  for exploitability is the `best_response` branch of `backup_combo_node`.
- Edge order at a player node is the order of `legal_betting_actions` for that
  node's betting state, so action labels can be recovered by replaying the betting
  state from the root.

## Semantics

At a locked node, the acting seat's actions split into a locked set `L`, each with
a fixed probability `p_a`, and a free set `F`. With `S = sum p_a` over `L` and
residual `r = 1 - S`:

```
sigma_a = p_a              a in L
sigma_a = r * rm_a         a in F, rm = regret matching over F only
                           (uniform over F when no free action has positive regret)
```

`sigma` is the only behavioural strategy: it drives reach, value backup and the
strategy-sum accumulation, so opponents best-respond to the constrained play and
the average strategy converges to `p_a` on locked actions.

Regret update for free actions when `r > 0`:

```
V_free = sum_{b in F} rm_b * V_b
R_a    = max(0, R_a + r * (V_a - V_free))      a in F
       = max(0, R_a + r * V_a - sum_{b in F} sigma_b * V_b)
```

Locked actions' regrets are never updated. When `r == 0`, nothing is updated: the
strategy is fully determined. The comparator is the free-only value, not the node
value, so regret never "learns" to move probability off a locked action; the factor
`r` makes the stored value the literal regret of the restricted game.

When the average strategy is extracted, locked actions are written as exactly
`p_a`, and free actions as `r` times their normalised strategy sums. This holds even
at nodes the seat never reaches, where the unlocked solver would report a uniform
strategy.

Best response (exploitability) at the seat's own locked node:
`V = sum_{a in L} p_a * V_a + r * max_{a in F} V_a`. Exploitability is therefore
measured *in the locked game*: how far each seat is from its best response given
that the locked play is fixed. A warning says so.

Dynamic pruning never deactivates actions at a locked node.

## Authoring format

Locks are part of the spot, so they persist with documents, appear in the spot JSON
editor and mark a solved document stale when they change. A new optional top-level
`"locks"` array:

```json
"locks": [
  { "path": [], "actions": { "check": 1.0 } },
  { "path": ["check", "bet_75"], "actions": { "raise_150": 0.0 } },
  { "path": ["bet_75", "*", "check"], "hands": "AA,KK,AKs",
    "actions": { "bet_75": 0.6, "check": 0.4 } }
]
```

- **`path`**: action labels from the root to the locked node. Player steps use the
  node's action labels (`fold`, `check`, `call`, `all_in`, `bet_<pct>`,
  `raise_<pct>`). Chance steps name the dealt card (`"Kh"`), or `"*"` for every
  card. A path through `"*"` locks the matching node on every runout. Under card
  isomorphism a card names its whole suit class, and only the class
  representative's card is accepted.
- **`hands`**: optional range text, using the range parser's syntax. Absent means
  the whole range. Hands not in the seat's range are ignored. A lock whose hands
  match nothing in the range is an error.
- **`actions`**: only the constrained actions. `{"raise_150": 0.0}` pins raise to 0
  and leaves the other actions free. Each frequency must be finite and in [0, 1],
  and they must add up to at most 1. If every legal action is listed, they must add
  up to 1, within 1e-6, and are rescaled to add up to 1 exactly. Frequencies are
  rounded to 1e-6 when parsed, so the text format cannot change a solve's identity.

### Action labels

Every action in the artifact gets a real label, not just at the root (today, deeper
nodes are labelled `"0"`, `"1"`, ...). A bet or raise is labelled by the chips
added as a percentage of the gross pot before the action, where gross pot is the
spot's `gross_pot` plus every chip committed since the root. That is the root's
existing convention, extended down the tree. If two actions at one node round to the
same label, the second gets a `.2` suffix, the third `.3`, and so on, so labels
are unique. Chance actions stay `"deal"`, with the cards in `dealt_cards`.

### Precedence and conflicts

At one node, a lock with `hands` overrides the whole-range lock for those hands
(the hand's constraint is replaced, not merged). A second whole-range lock on the
same node, or two hand locks whose hands overlap, is an error. Two `"*"` paths
that reach the same node count as the same node.

### Validation errors (before any iteration runs)

Every error names the lock by its 1-based position:
- unknown action label, or a chance step that is not a card or `"*"`, at step *k*
- a path that ends at a chance or terminal node
- frequencies that are negative, non-finite, above 1, or add up to more than 1
- a lock that lists every action but does not add up to 1
- `hands` that fail to parse, or match nothing in the acting seat's range
- duplicate or overlapping locks at one node (see above)
- a `hands` lock with card isomorphism enabled: a hand lock breaks suit symmetry,
  so it cannot be represented exactly on collapsed runouts

## Artifact and provenance

- Schema 7. Additions: `solver.locks` (the canonical lock list),
  `solver.hashes.locks`, `solved_node.lock_indices` (indices into `solver.locks`
  that apply at that node) and `hand_strategy.locked` (true when the hand's play at
  that node is constrained). Schema 6 artifacts still load, with no locks.
- `locks_hash` is an FNV-1a hash of the canonical locks: path, sorted hand list,
  actions sorted by label, and frequencies as rounded integers. It is folded into
  `solve_hash` only when locks exist, so a no-lock solve's hashes are unchanged.
- Warning when locks exist: the strategy is a best response within the locked game,
  and exploitability is measured in that game.
- With no locks, the solver's arithmetic is unchanged. Every lock hook sits behind
  a null runtime-lock pointer, as pruning does.

## UI

- **Strategy Explorer:** a lock button for the selected player node opens a lock
  editor. The editor has a lock checkbox and a frequency for each action, plus an
  optional hands field, and pre-fills the node's solved frequencies. Applying it
  adds or replaces the lock in the document's spot, which marks the solve stale; the
  user re-solves as usual. Locked nodes are marked in the action tree, and the
  action table shows which actions are locked.
- **Locks panel:** lists the document's locks (path, hands, actions) with remove and
  clear-all. It sits beside the spot builder so locks can be reviewed before
  solving.
- **Validation:** lock errors from the solver surface through the existing
  solve-failure path, with the lock number and the reason.

## Delivery steps

1. **Labels:** replay betting states to label every action. The artifact's
   `solved_nodes[*].actions` and `range_action_frequencies` carry real labels.
   Add a helper that gives a node's lock path.
2. **Model and parsing:** `node_lock` in `solve_spot`; parse, serialize and round
   in `solve_cli.cpp`; parse-time field checks.
3. **Resolution:** walk each lock's path over the lowered graph, map labels to
   action indices, resolve hands to combos, check conflicts, and build the runtime
   view (per-infoset, per-combo constraint index).
4. **Solver hooks:** current/average strategy, regret update, best response, and
   pruning exemption, in both `solve_river` and `solve_multi_street_public_game`.
5. **Provenance:** locks hash, artifact fields, warning, schema 7 with schema-6
   reads, and the CLI docs.
6. **Tests:**
   - parse round-trip and every validation error
   - a no-lock solve is unchanged (hashes and strategies)
   - a full lock reproduces its frequencies exactly in the artifact
   - a partial lock pins its actions while the free actions still solve
   - the opponent exploits a lock: a river spot where locking a bluff-catcher to
     always fold makes the bettor bluff more and raises its EV
   - a hand lock overrides the range lock only for its hands
   - `"*"` paths cover every runout
   - exploitability is reported in the locked game
   - determinism across worker counts
   - hashes change when locks change
7. **UI:** lock editor, tree and action-table markers, locks panel, and UI tests
   for applying, replacing and removing locks and the solved display.

## Non-goals

- Soft or penalty constraints.
- Locking chance frequencies or payoffs.
- Hand locks under card isomorphism (rejected, see above).
