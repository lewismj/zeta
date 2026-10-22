# Hold'em Core Algorithms: Maths and Implementation Deep Dive

This document is the implementation-level companion to
[`core_algorithms.md`](core_algorithms.md). It expands the runtime algorithms
used by the Hold'em engine with explicit formulas, data layouts, and why each
path is structured the way it is.

The content here aligns with the runtime sources under `zeta\holdem\src\...`.

## 1. 7-card evaluator internals

### 1.1 Bit layout and suit-rank decomposition

For a 52-card deck, cards are represented in a `uint64_t card_mask` with:

```text
bit = suit * 13 + rank
```

The evaluator starts by projecting a 7-card mask into four 13-bit suit-rank
masks (`spades`, `hearts`, `diamonds`, `clubs`) using
`suit_rank_masks(...)` in `eval\evaluator.h`.

That decomposition is the shared input for both evaluation paths:

1. flush/straight-flush (`flush_table`)
2. non-flush (`non_flush_table` via restricted-quinary index)

### 1.2 Flush path

`evaluate(...)` checks each suit's rank mask in turn for at least 5 bits set:

```text
popcount(suit_mask) >= 5
```

If true, the 13-bit rank mask of that suit becomes a direct table index:

```text
rank = flush_table[suit_rank_mask]
```

Because the index is already canonical for all 5+/7 flush supersets, no
runtime combinatorics are needed in this path.

### 1.3 Non-flush path: canonical multiplicity layers

For non-flush hands, suit identity is irrelevant; rank multiplicity is enough.
The runtime derives four threshold layers (where each layer is a threshold mask,
not an exact multiplicity mask):

- `ones`: rank appears at least once
- `twos`: rank appears at least twice
- `threes`: rank appears at least three times
- `fours`: rank appears four times

The exact rank count is reconstructed per rank bit:

```text
count(rank) = bit(ones) + bit(twos) + bit(threes) + bit(fours)
```

The packed canonical key is:

```text
key = ones | (twos << 13) | (threes << 26) | (fours << 39)
```

This key is canonical for non-flush 7-card rank multisets.

### 1.4 Restricted quinary dense indexing

Each of the 13 ranks maps to a quinary digit `ci in [0,4]` with:

```text
sum(ci) = 7
```

The number of legal 13-digit vectors under those constraints is:

```text
49,205
```

The non-flush table therefore has exact size 49,205 (`non_flush_quinary_table_size`).

`tables.h` computes a perfect dense rank via combinatorial DP:

```text
index += quinary_dp[count][remaining_ranks][remaining_cards]
remaining_cards -= count
```

At runtime, the evaluator performs $O(1)$ table lookups with small fixed arithmetic/bitwise work.
The hot path avoids a full 13-step loop by splitting into `4 + 4 + 5` rank chunks and using generated
chunk tables:

- `quinary_chunk0` over ranks `0..3`
- `quinary_chunk1` over ranks `4..7`
- `quinary_chunk2` over ranks `8..12`

Each packed chunk entry stores:

```text
low 24 bits: index contribution
high 8 bits: cards consumed by this chunk
```

The final dense index is the sum of the three chunk contributions, with
remaining-card state threaded chunk-to-chunk.

> [!NOTE]
> This is a dense perfect index over legal non-flush rank multisets: there are
> no collisions, key probes, or sparse holes.

### 1.5 Why pair-weight tables exist

`quinary_pair_weights4` and `quinary_pair_weights5` combine layer nibbles into
base-5 chunk codes with two lookups instead of four independent weighted sums.
This keeps the runtime path branch-light and arithmetic-light while preserving
exactness.

## 2. Range parser internals (PokerStove grammar)

The parser (`range_parser.h`) is single-pass, exception-free, and writes
directly into `hand_range::weights[1326]`.

### 2.1 Grammar fragments and expansion semantics

Accepted term categories:

- pair class: `AA`
- non-pair class: `AK`, `AKs`, `AKo`
- exact combo: `AsKh`
- plus expansion: `22+`, `A5s+`
- bounded range: `KTs-KQs`
- weighted term: `AA:0.5`

Each term expands to explicit combo indices and assigns the provided weight.
Repeated terms overwrite the same combo slots with the latest assignment.

### 2.2 Combo indexing and determinism

`combo_index_from_cards(...)` canonicalizes rank/suit ordering before indexing.
This guarantees deterministic mapping for all equivalent card-order spellings.

### 2.3 Failure model

Errors report both code and byte position (`range_parse_error`), with specific
codes such as:

- `expected_rank`
- `expected_suit`
- `invalid_exact_combo`
- `invalid_plus`
- `invalid_range`
- `invalid_weight`

No exceptions are thrown; parse failure is returned as data.

## 3. River terminal cache and reach indexing

### 3.1 Board-specialized immutable cache

`make_river_terminal_cache(board)` materializes immutable per-combo data for a
single river board:

1. board-blocked combos are filtered out
2. each live combo is evaluated once (`evaluate(...)`)
3. live combos are rank-sorted (`rank_order`)
4. monotone `rank_key` buckets are assigned

This shifts expensive board+combo evaluation out of hot terminal loops.

### 3.2 Reach index as a range-conditioned projection

`make_river_reach_index(cache, reach_vector)` builds a compact active view:

- active combo ids
- per-combo weights
- total live mass
- mass by card (`mass_by_card[52]`)
- rank buckets with per-bucket card mass slices

The key blocker-correction identity is:

```text
compatible_mass = total_mass - mass(card1) - mass(card2) + mass(exact_same_combo)
```

where `mass(card)` is the total reach mass of opponent combos containing that card,
and `mass(exact_same_combo)` is the mass of the unique two-card combo containing both cards.
The final addend corrects double subtraction of the exact same two-card combo.

### 3.3 Numerical safety

`clamp_compatible_mass(...)` clamps tiny negative values near zero (floating
point noise), while retaining assertions against materially invalid negatives.

## 4. Showdown value algorithms

### 4.1 Heads-up exact kernel (rank-bucket sweep)

`evaluate_showdown_heads_up(...)` merges OOP/IP bucket streams by ascending
`rank_key`. With `rank_order` / `rank_key` ascending from weakest to strongest hand rank,
for each hero combo:

1. buckets strictly below the hero bucket (lower compatible opponent mass) are wins
2. equal buckets (equal compatible opponent mass) are ties
3. buckets strictly above the hero bucket (higher compatible opponent mass) are losses

Value is:

```text
V = lower * win_component + equal * tie_component + higher * loss_component
```

Where payoff components come from rake-adjusted pot/contribution accounting in
`terminal_context<2>`.

The implementation also accumulates summary matchup statistics (`oop_wins`,
`ip_wins`, `ties`, EV totals) in the same pass.

### 4.2 N-way exact path

For `N > 2`, `evaluate_showdown_values_exact(...)` recursively enumerates
compatible opponent combo assignments per hero combo, then applies layered pot
awards by best rank among eligible, non-folded seats.

This is exact and state-faithful, but combinatorially expensive for large
active domains.

## 5. Fold terminal evaluation

Fold utilities are computed from layered pots and seat eligibility:

1. initialize each seat at `-contribution[seat]`
2. for each pot layer, split rake-adjusted amount among eligible non-folded
   winners
3. accumulate seat payoff

Heads-up has a dedicated fast path (`evaluate_fold_values_heads_up`), while
multiway routes evaluate exact layered payoffs via `terminal_state<N>`.

## 6. Chance event tables and validation

Chance nodes are represented by `chance_event_table`:

- event slice per graph node (`first_outcome`, `outcome_count`)
- flat `chance_outcome` array
- `event_id_by_node` side lookup

The value backup asks:

```text
p = probability_for_edge(node_id, edge) * private_hand_chance_scale(event, N)
```

The public probability is uniform over unseen cards; the scale turns it into the
probability conditioned on the hole cards of a compatible hand profile.

Validation (`validate_chance_event_table`) enforces:

1. side-array dimensions match graph
2. every chance node has a valid aligned event
3. outcome count matches edge count
4. outcome probabilities are finite, non-negative, and sum to ~1
5. no outcome collides with board/dead cards
6. canonical (isomorphism-collapsed) events never exceed full enumeration count

This guarantees the solver sees a coherent stochastic process.

## 7. Action-table layout

`make_action_table_layout(graph)` builds a contiguous infoset-major offset array:

```text
action_offsets[infoset] .. action_offsets[infoset + 1)
```

All nodes that share an infoset share its action count. The solver's
`combo x action` tables (regrets, strategy sums, current and average strategy)
index through this flat layout, one row of action slots per hand.

## 8. Regret matching and strategy accumulation

`normalize_combo_action_table(...)` computes each hand's policy from its regrets:

```text
r+(a) = max(regret(a), 0)
p(a)  = r+(a) / sum_b r+(b), if sum_b r+(b) > 0
p(a)  = 1 / |A|              otherwise
```

Only active actions participate when dynamic pruning is on.

Average strategy accumulation is realization-weighted. For hand `h` of the
acting seat `i`, the per-action strategy-sum increment is:

```text
strategy_sum(a) += pi_i(n | h) * p(a)
```

where `pi_i(n | h)` is the seat's own reach to the node, including its root range
weight. CFR regret updates instead use counterfactual values, which carry the
opponents' and chance's reach (section 9).

## 9. CFR value backup and regret updates

Every solve uses the same combo-vectorised CFR+ loop (`solve_cli.h`). Graph
nodes are in post-order, so a single ascending pass visits children before their
parent.

### 9.1 Node value equations

Values are per hand of the updating player `i`. Terminal leaves are evaluated
against the other seats' reach at the leaf (section 10). Then:

```text
chance node:              V(n, h) = sum_a p_chance(a) * V(child_a, h)
seat i's own node:        V(n, h) = sum_a pi_i(a | n, h) * V(child_a, h)
another seat's node:      V(n, h) = sum_a V(child_a, h)
```

The last case sums because that seat's action probabilities are already in the
reach the leaves were evaluated with.

### 9.2 Reach propagation

Each seat carries a per-hand reach vector, starting from its range weights:

```text
reach_actor[h] *= pi_actor(a | n, h)   at the actor's nodes
```

Chance probabilities are not folded into reach; the backup applies them at chance
nodes.

### 9.3 Regret update

Because terminal values already carry the opponents' reach, `V` is a
counterfactual value and the CFR+ update needs no extra weight:

```text
regret(a) = max(0, regret(a) + V(child_a, h) - V(n, h))
```

Players update in turn (alternating updates); each update recomputes the current
strategy, reach and values first.

## 10. Parallel terminal evaluation

Terminal evaluation dominates each pass, and every leaf reads only shared,
immutable inputs and writes only its own node's values. The evaluators run all
leaves as one batch on a `scheduler::worker_pool` with `worker_threads` threads
(the calling thread included); the pool hands out leaves in chunks from an atomic
counter so uneven leaves balance. The post-order backup and the table updates run
on one thread afterwards.

No two tasks write the same memory and every floating-point operation keeps its
order, so a solve is bit-identical for any worker count.

## 11. Terminal engine dispatch contracts

`terminal_engine<N>` is compile-time dispatch:

- `N == 2`: heads-up exact kernels
- `N > 2`: exact multiplayer kernels

`evaluate_terminal_values(...)` dispatches by `terminal_state_kind`:

- showdown -> showdown evaluator
- fold -> fold evaluator

Unsupported terminal kinds are treated as an internal invariant violation and currently fail via assertion; user-facing validation rejects unsupported terminal configurations before solving.

> [!NOTE]
> The dispatch boundary means the solver can stay generic over `N` while still
> using optimized heads-up kernels where available.

## 12. Complexity and performance notes

| Surface | Dominant runtime behavior |
|---|---|
| 7-card evaluator | O(1) table lookups with small fixed arithmetic/bitwise work |
| HU showdown | Near-linear in active rank buckets and active combos with blocker-corrected mass arithmetic |
| N-way exact showdown | Exponential in active opponent branching (exact combinatorial enumeration) |
| CFR iteration | Terminal evaluation (parallel) plus O(edges x active hands) backup and updates per player |

## 13. Formal strategy/EV mathematics

This section states the quantities the solver reports, in solver-ready form.

> [!IMPORTANT]
> `Q`, `V`, and `A` below are **profile-evaluation** quantities under the
> frozen exported average strategy. They are not cumulative CFR regrets.

### 13.1 Domains and random variables

Let:

- `i` be a seat index
- `n` be a concrete game node in the solved game graph
- `h` be a private combo for seat `i`
- `a` be an action in the represented legal action set at `(n, h)` after betting/action abstraction
- `z` be a terminal history

Define domains:

- `H_i(n)`: legal private combos for seat `i` at node `n`
- `A(n, h)`: legal actions for `(n, h)` (same action set across the lowered infoset strategy context)
- `Z(n, h)`: terminal histories reachable from `(n, h)`

For every represented combo:

```text
h in H_i(n)  <=>  (h ∩ B(n) = ∅)
```

where `B(n)` is the public board mask at node `n`.

### 13.2 Reach decomposition and mass quantities

Use the decomposition from the contract:

```text
w0_i(h)        : initial range mass for seat i, combo h
pi_i(n | h)    : seat i path reach to node n given h
Pi_-i(n | h)   : product of all opponents' path reaches to n, conditioned on hero holding h (including card-removal effects)
pi_c(n)        : chance reach to n
```

Derived masses:

```text
range_reach_weight_i(n, h) = w0_i(h) * pi_i(n | h)
joint_reach_mass_i(n, h)   = w0_i(h) * pi_i(n | h) * Pi_-i(n | h) * pi_c(n)
```

Seat-level aggregates:

```text
W_i(n) = range_reach_mass_i(n)
       = Σ_{h in H_i(n)} range_reach_weight_i(n, h)
       = Σ_{h in H_i(n)} w0_i(h) * pi_i(n | h)

J_i(n) = Σ_{h in H_i(n)} joint_reach_mass_i(n, h)
```

`W_i(n)` is hero-range mass at the node (not joint node probability).

Opponent and matchup masses, which turn counterfactual values into EVs:

```text
M_-i(n, h) = Σ_{h_-i compatible with h and B(n)} Π_{j != i} w0_j(h_j) * pi_j(n | h_j)
MM_i(n)    = Σ_{h in H_i(n)} range_reach_weight_i(n, h) * M_-i(n, h)
```

Heads-up, `M_-i` is exact (blocker inclusion-exclusion). Multiway, the solver
uses the product of each opponent's compatible mass, which ignores card
collisions between opponents.

### 13.3 Local intervention operator and profile action values

Let `σ̄` be the exported average strategy profile. Define a local intervention
policy `σ̄^{n,h→a}` that is identical to `σ̄` everywhere except at the current
decision `(n, h)`, where it plays action `a` with probability 1.

Then:

```text
Q_profile_i(n, h, a)
= E_{z ~ P(· | n, h, σ̄^{n,h→a})}[ u_i(z) ]
```

This is a one-step deviation value under the expectation over chance transitions,
opponent strategy $\bar{\sigma}_{-i}$, future own strategy $\bar{\sigma}_i$, and terminal payoff/rake
conventions, while only the current decision at `(n, h)` is intervened to action `a`.

### 13.4 Profile combo value and advantage

Define:

```text
V_profile_i(n, h)
= Σ_{a in A(n,h)} σ̄(a | n, h) * Q_profile_i(n, h, a)
```

and:

```text
A_profile_i(n, h, a)
= Q_profile_i(n, h, a) - V_profile_i(n, h)
```

So the action decomposition is:

```text
Q_profile_i(n,h,a) = V_profile_i(n,h) + A_profile_i(n,h,a)
```

### 13.5 Strategy-weighted advantage identity (proof sketch)

From the definition of `V_profile`:

```text
Σ_a σ̄(a|n,h) A_profile_i(n,h,a)
= Σ_a σ̄(a|n,h) (Q_profile_i(n,h,a) - V_profile_i(n,h))
= Σ_a σ̄(a|n,h) Q_profile_i(n,h,a) - V_profile_i(n,h) Σ_a σ̄(a|n,h)
= V_profile_i(n,h) - V_profile_i(n,h)
= 0
```

Hence, for every legal `(n, h)`:

```text
Σ_a σ̄(a|n,h) A_profile_i(n,h,a) = 0
```

### 13.6 Node-level seat values and conditioning

The extracted seat-value surfaces are:

```text
reach_weighted_value_i(n)
= Σ_{h in H_i(n)} range_reach_weight_i(n,h) * CFV_i(n,h)

conditional_range_ev_i(n)
= reach_weighted_value_i(n) / MM_i(n),  when MM_i(n) > 0
```

where `CFV_i(n,h)` is the hand's counterfactual value (its `V_profile` weighted by
the opponents' reach). Per hand, `EV_i(n,h) = CFV_i(n,h) / M_-i(n,h)` is in chips.

Counterfactual value:

```text
CFV_i(n)
= Σ_{h in H_i(n)} w0_i(h) * Pi_-i(n|h) * pi_c(n) * V_profile_i(n,h)
```

`Pi_-i(n|h)` includes the card-removal-conditioned opponent reach distribution induced by fixing hero combo `h`.

The difference versus `reach_weighted_value` is the hero weighting:

- `reach_weighted_value` weights by the path reach `w0_i * pi_i`
- `CFV` weights by the root range `w0_i` only

### 13.7 Heads-up and N-way counterfactual form

Heads-up (`i` vs `j`):

```text
CFV_i(n) = Σ_h w0_i(h) * pi_j(n|h) * pi_c(n) * V_profile_i(n,h)
```

N-way:

```text
CFV_i(n) = Σ_h w0_i(h) * ( Π_{k != i} pi_k(n|h) ) * pi_c(n) * V_profile_i(n,h)
```

### 13.8 Showdown equity vs strategic EV

For seat `i`, combo `h`, node `n`, pot-share showdown equity is evaluated over all
mutually compatible joint opponent combo assignments $h_{-i} = (h_j)_{j \neq i}$:

```text
μ_-i(n, h_-i)
= P(initial opponent combo assignment h_-i) * P(reach n | h, h_-i)
```

Then:

```text
Equity_i(n, h)
= [ Σ_{h_-i compatible with h} μ_-i(n, h_-i) * share_i(h, h_-i) ]
  / [ Σ_{h_-i compatible with h} μ_-i(n, h_-i) ]
```

For heads-up ($N = 2$), this reduces to the single-opponent marginal:

```text
μ_-i(n, h') = w0_-i(h') * pi_-i(n | h')
```

where `share_i(h, h')` is 1 for win, 1/2 for tie, and 0 for loss. In $N$-way ($N > 2$), `share_i(h, h_-i)`
is the exact fractional pot share partitioned across eligible winners.

This is not betting EV; it is no-further-betting pot share under the reaching
distribution.

### 13.9 Conservation identities

For a fixed public node $n$ and compatible joint private-hand distribution, zero-sum conservation
holds at the joint-state / jointly weighted level:

```text
Σ_{h_0, h_1 compatible} P(h_0, h_1 | n) [ V_0(n, h_0, h_1) + V_1(n, h_0, h_1) ] = 0
```

Equivalently, integrating over the joint reach distribution:

```text
U_0(n) + U_1(n) = 0       (without rake)
U_0(n) + U_1(n) = -rake(n) (with rake)
```

where:

```text
U_i(n) = Σ_{h in H_i(n)} P(h | n) * V_profile_i(n, h)
```

Pointwise unweighted combo EVs $V\_profile\_0(n, h_0) + V\_profile\_1(n, h_1)$ do not sum to zero
because they are conditioned on differing private hands and opponent distributions.

For normalized showdown pot-share equity with fractional ties:

```text
HU:    Equity_0 + Equity_1 = 1
N-way: Σ_i Equity_i = 1
```

### 13.10 Regret-matching and extracted profile relationship

Each iteration computes strategy probabilities from regrets:

```text
r⁺(a) = max(R(a), 0)
σ(a)  = r⁺(a) / Σ_b r⁺(b),   if Σ_b r⁺(b) > 0
σ(a)  = 1 / |A|              otherwise
```

This `σ` is the per-iteration policy driver. The exported strategy `σ̄` is the
weighted average over iterations:

```text
σ̄(a|I,h) = [ Σ_t w_t * σ_t(a|I,h) ] / [ Σ_t w_t ]
```

where $w_t(n, h)$ is the realization-weight assigned to the strategy context:

```text
w_t(n, h) = w0_i(h) * pi_i^t(n | h)
```

The reported values (`V_profile`, `CFV`, EVs) are evaluated on that frozen
average profile `σ̄`.

### 13.11 Exploitability and NashConv linkage

For two-player zero-sum reporting:

```text
NashConv(σ̄) = BR_0(σ̄_1) - u_0(σ̄) + BR_1(σ̄_0) - u_1(σ̄)
Exploitability = NashConv / 2
```

where `BR_i` is best-response value for seat `i` and `u_i(σ̄)` is profile value.
This definition assumes the same utility normalization and player-perspective convention
are used for both `BR_i` and `u_i`.

Multiway solves report normalized regret instead of exact exploitability.

### 13.12 Artifact-to-equation mapping

| Artifact field | Mathematical quantity |
|---|---|
| `strategy_rows[].range_weight` | `w0_i(h)` |
| `strategy_rows[].reach_probability` | `pi_i(n\|h)` |
| `strategy_rows[].counterfactual_value` | `CFV_i(n,h)` |
| `strategy_rows[].opponent_reach_mass` | `M_-i(n,h)` |
| `strategy_rows[].ev` | `EV_i(n,h)` |
| `seat_values[].range_reach_mass` | `W_i(n)` |
| `seat_values[].matchup_reach_mass` | `MM_i(n)` |
| `seat_values[].reach_weighted_value` | `reach_weighted_value_i(n)` |
| `seat_values[].conditional_range_ev` | `conditional_range_ev_i(n)` |
| `seat_values[].counterfactual_value` | `Σ_h w0_i(h) * CFV_i(n,h)` |

`Q_profile` and `A_profile` are not stored; they are defined here because action
EVs in the UI are the acting seat's `conditional_range_ev` at each child node.
