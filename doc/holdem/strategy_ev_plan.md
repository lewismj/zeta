# Strategy and EV Result Surfaces Implementation Plan

This plan details the implementation of **Section 3: Strategy and EV result surfaces** from [`next_steps.md`](next_steps.md). It outlines the architecture, data structures, algorithms, schema evolution, UI/CLI integrations, and test verification strategies required to deliver rich per-node strategy, per-hand EV, equity surfaces, action-level regrets/EVs, and postflop hand-category aggregations across the Zeta Hold'em Solver.

---

## 1. Executive Summary & Objectives

### 1.1 Goal
Upgrade Zeta's solver extraction pipeline and artifact representation from root-centric/summary views to a fully structured, multi-street postflop result surface. This enables deep GTO analysis, range vs. range equity breakdowns, action EV and regret inspections, hand-category frequency distributions (e.g., top pair, flush draws, blockers), and seamless UI integration while maintaining rigorous mathematical and architectural boundaries across:
- **Solver CFR State** (infoset-level cumulative regrets and strategy accumulation / averaging state),
- **Extracted Profile Values** (concrete game-node EV, action Q-values, average-profile deviation advantages, showdown pot-share equities),
- **Derived Classifications** (intrinsic hand/board categorization),
- **Derived Query Views & Summaries** (category aggregations, range-weighted metrics),
- **Serialized Artifacts** (compact Schema v4 projections),
- **UI Solution Store** (interactive explorer view-models).

### 1.2 Core Architectural Invariants
$$\mathbf{CFR\ State \ne Extraction\ Result \ne Artifact \ne UI\ Model}$$

- **Extensive-Form Information Set vs. Strategy Decision Context**:
  - **Standard Extensive-Form Poker Infoset**:
    $$\text{infoset} = \text{acting\_seat} + \text{public\_state} + \text{betting\_history} + \text{player's private cards}$$
    Opponent private cards and hidden chance events (e.g. opponent hole card dealing) are strictly excluded.
    - Two concrete game tree nodes $n_A$ and $n_B$ share an extensive-form information set $I$ if and only if the acting player's observation is identical (same public state, same betting sequence, and **same private holding**), differing solely in hidden opponent cards.
    - Conversely, at the same public state and betting sequence, two different private holdings (e.g., $A\spadesuit Q\spadesuit$ vs. $A\heartsuit Q\heartsuit$) constitute **distinct** information sets.
  - **Range Strategy Decision Context (`strategy_context_id`)**:
    In CFR solvers and the Result Store, the public decision context $\mathcal{S} = (\text{public\_state}, \text{betting\_history}, \text{acting\_seat})$ groups the vectorized strategy surface across all legal private holdings $h \in H_{\text{legal}}(\mathcal{S})$.
    - The strategy surface is indexed by:
      $$(\text{strategy\_context\_id}, \text{combo\_local\_index}, \text{action\_index})$$
    - Each $(\text{strategy\_context\_id}, h)$ uniquely identifies an individual extensive-form information set $I(\mathcal{S}, h)$.
    - **Strategy Ownership Invariant**: The strategy surface is owned exactly once by the range decision context (`strategy_surface_record`). Concrete game nodes reference `strategy_context_id` and never duplicate strategy entries.
    - **Evaluated State Invariant**: A concrete game node $n$ references `strategy_context_id` while owning its own distinct evaluated state (reach weights, combo profile values $V_{\text{profile}}$, action $Q_{\text{profile}}$ and $A_{\text{profile}}$, showdown equities, and classifications).
    $$\text{strategy} \implies \text{strategy\_context\_id (shared range decision)}$$
    $$\text{values / reach / public state / equities} \implies \text{node\_id (concrete game node)}$$

### 1.3 Core Deliverables
1. **Extensive-Form Infoset vs. Range Strategy Context**: Decouple concrete game nodes (`node_id`, reach probabilities, concrete public state, node-specific action values, node EVs, showdown equities) from extensive-form information sets and range strategy contexts (`strategy_context_id`, action strategies $\bar{\sigma}$, cumulative regrets $R_T^+$).
2. **Strategy Surface vs. CFR State**: Distinguish raw CFR solver state from extracted average-profile strategy surfaces (`strategy_surface_entry`). Structure strategy records with dense flat layout and $O(1)$ combo/action lookups, ensuring $\sum_a \bar{\sigma}(a \mid I, h) = 1.0$ is strictly normalized. Decouple solver-internal cumulative regret into an optional diagnostic surface (`regret_surface_entry`).
3. **Decoupled Regret vs. Action Value Models**: Separate solver state (`average_strategy`, `cumulative_regret`) from extracted profile values (`q_profile` $Q_{\text{profile}}$, `profile_advantage` / `deviation_value` $A_{\text{profile}}$).
4. **Precisely Conditioned Q $\to$ V $\to$ A Chain & Value Quantities**:
   - $Q_{\text{profile}}(n, h, a)$: Expected terminal payoff for acting player under local one-step action intervention $a$ at concrete node $n$, with all subsequent decisions following the frozen exported average strategy profile $\bar{\sigma}$.
   - $V_{\text{profile}}(n, h) = \sum_a \bar{\sigma}(a \mid I, h) Q_{\text{profile}}(n, h, a)$.
   - $A_{\text{profile}}(n, h, a) = Q_{\text{profile}}(n, h, a) - V_{\text{profile}}(n, h)$, satisfying the **Strategy-Weighted Advantage Identity** $\sum_a \bar{\sigma}(a \mid I, h) A_{\text{profile}}(n, h, a) = 0$.
   - Explicit formal distinctions across conditioning quantities: `combo_profile_value` ($V_i(n, h)$), `range_reach_weight` ($w_0(h)\pi_i(n \mid h)$), `joint_reach_mass`, `range_reach_mass` ($W_i(n)$), `reach_weighted_ev`, `conditional_range_ev`, and unnormalized/chance-conditioned `counterfactual_value` ($CFV_i(n)$).
5. **Chance Node & Terminal Node Extraction Semantics**: Formal extraction contracts distinguishing player nodes (strategy, combo values, action Q/A, reach), chance nodes (outcomes, probabilities, child values), and terminal nodes (terminal utilities).
6. **Explicit Combo Reach & Derived Range Weight**:
   - `range_weight`: Initial preflop range mass $w_0(h)$.
   - `reach_probability`: Conditional path probability $\pi_i(n \mid h)$.
   - `range_reach_weight`: $w_i(n, h) = w_0(h)\pi_i(n \mid h)$ (player's remaining range mass conditional on path, derived in `double`).
   - `joint_reach_mass`: $w_0(h)\pi_i(n \mid h)\Pi_{-i}(n \mid h)\pi_c(n)$ (joint realization probability).
   - Zero-reach legal combos preserved in the Result Store ($reach = 0 \implies reach\_weighted\_contribution = 0$, but strategy, profile values, and category remain valid).
7. **Contiguous Offset-Indexed High-Performance Result Store**: Dense rectangular arrays with flat offsets separating structural index (`node_record`), strategy surfaces (`strategy_surface_record`), combo value surfaces, action value surfaces, equity surfaces, and classification surfaces.
   - Formal **Combo Domain Offset Sharing Invariant**: Reaches, combo values, equities, and classifications share the exact same contiguous combo offset `combo_begin + i`.
   - Pure $O(1)$ combo/action indexing with zero allocations on query paths. Canonical combo and action indexing contracts. String representations (`hand`, `hand_class`) omitted from core store and generated only at serialization time. Direct flat buffer writing during extraction loops.
8. **Deterministic Intrinsic Hand Categorizer & Blocker Engine**: Complete 10-window 5-rank window enumeration straight draw engine (`straight_draw_info`), board-geometry-aware paired-board `pair_position` and `pair_source`, deterministic live-rank-based `kicker_quality` (Zeta intrinsic kicker classification), and isolated algorithmic board-flush `blocker_flags` (Phase 1.6). (Range-relative blockers deferred to Phase 5).
9. **Schema v4 as Explicit Projection**: JSON Schema v4 is an explicit artifact projection of the binary Result Store with three deterministic export modes (`summary`, `standard`, `full`). Includes `schema_version: 4`, `extraction_version: 1`, explicit `seat_values`, deduplicated `public_states`, canonical `"combination_index"`, and versioned derived query projections (`"derived": { "category_summaries": { "derivation_version": 1, "items": [...] } }`).
10. **Multi-Dimensional Quality & Convergence Semantics**: Orthogonal characterization of `solve_mode`, `termination_reason` (`iteration_limit`, `exploitability_target`, `user_interrupted`), `convergence_status` (`target_met`, `iteration_limited`, `not_evaluated`), `evaluation_method` (`exact` vs `sampled`), and `abstraction_mode`.
11. **Comprehensive Golden Verification & Reference Evaluator**: Standalone unoptimized reference evaluator oracle (Phase 0.5) operating strictly on semantic inputs and sharing zero indexing/traversal code with production, plus comprehensive golden test fixtures (Golden 1–9 covering per-combo river equity matrices, mixed strategy Q/V/A identities, HU zero-sum payoff checks, zero-reach preservation, shared-infoset aliasing, private-card separation, strategy storage aliasing, bitwise extraction determinism, and artifact round-trip consistency).

---

## 2. Mathematical Foundations & Invariants

To eliminate ambiguity, all quantities exposed by the extraction pipeline and artifact schema adhere to the following formal definitions.

```
                         SOLVED CFR STATE
                                │
                ┌───────────────┴───────────────┐
                │                               │
        INFORMATION SET                  CONCRETE NODE
      I(S, acting_seat, h)                      n
  (includes private cards h)         (hidden state / reach / pot)
                │                               │
       average strategy σ¯                reach / values
       cumulative regrets R_T+            equity / category
                │                               │
                └───────────────┬───────────────┘
                                │
                           EXTRACTION
                                │
               ┌────────────────┼────────────────┐
               │                │                │
           STRATEGY          Q / V / A        EQUITY
           SURFACE            SURFACE         SURFACE
      (strategy_context_id) (node-local)    (node-local)
               │                │                │
               └────────────────┼────────────────┘
                                │
                          RESULT STORE
                                │
                   ┌────────────┴────────────┐
                   │                         │
            intrinsic category         derived queries
                   │                         │
                   └────────────┬────────────┘
                                │
                           SCHEMA v4
                                │
                          UI SOLUTION STORE
```

### 2.1 Strategy Representation & Combo Universe
- **Extensive-Form Information Set ($I$) vs. Range Strategy Context ($\mathcal{S}$)**:
  - In extensive-form game theory, an information set $I = (\mathcal{S}, h)$ uniquely identifies the acting seat, public state, betting history, and the player's private holding $h$.
  - In CFR solver architecture and the Result Store, the public decision context $\mathcal{S} = (\text{public\_state}, \text{betting\_history}, \text{acting\_seat})$ is indexed as `strategy_context_id`. It owns the vectorized range strategy across all legal private holdings $h \in H_{\text{legal}}(\mathcal{S})$ and legal actions $a \in A(\mathcal{S})$.
  - A concrete game node $n$ references `strategy_context_id` while maintaining its own distinct evaluated state (reaches, profile values, equities, classifications).
- **Strategy Source**: Solved artifacts expose the **Average Strategy** $\bar{\sigma}(a \mid I, h) = \bar{\sigma}(a \mid \mathcal{S}, h)$ converged over CFR iterations:
  $$\bar{\sigma}(a \mid I, h) = \frac{\sum_{t=1}^T w_t \cdot \sigma_t(a \mid I, h)}{\sum_{t=1}^T w_t}$$
  where $w_t$ is the iteration weight (e.g., linear in CFR+, uniform in standard CFR).
- **Strategy Invariant**: Strategy is normalized over all legal actions for every legal combo:
  $$\sum_{a \in A(\mathcal{S})} \bar{\sigma}(a \mid I, h) = 1.0 \quad (\forall h \in H_{\text{legal}}(n))$$
- **Canonical Combo Ordering Contract**:
  For every node, `combo_local_index` $i \in [0, \text{combo\_count})$ refers strictly to the $i$-th legal combination in canonical ascending `combination_index` order ($0 \le \text{idx} < 1326$) after filtering out combinations conflicting with the public board ($h \cap B \ne \emptyset$). The Result Store provides $O(1)$ mapping via `combination_index combo(uint32_t local_index)`.
- **Canonical Action Ordering Contract**:
  For an infoset/strategy context, `action_index` $a \in [0, \text{action\_count})$ refers strictly to the position in the canonical legal-action table produced by the game graph for that public decision context. The same `action_index` identifies the identical strategic action across every concrete node belonging to that strategy context.
- **Combo Universe Distinctions**:
  - **Legal combo**: Any combinatorially valid 2-card holding that does not conflict with the public board ($h \cap B = \emptyset$).
  - **Reachable combo**: A legal combo with non-zero conditional reach probability at the current node ($\pi_i(n \mid h) > 0$).
  - **Active combo**: A legal combo with non-zero range weight in the initial preflop/starting range ($w_0(h) > 0$).
  - **Serialized combo**: A combo emitted in the external JSON artifact projection.
  - **Critical Invariant**: legal vs reachable vs serialized are distinct domains. A legal combo may be unreachable, and a reachable combo may be omitted from compact JSON serialization without changing its validity in the canonical Result Store.
- **Zero-Reach Policy Invariant**:
  $$reach(n, h) = 0 \implies reach\_weighted\_contribution(n, h) = 0$$
  Having zero reach ($\pi_i(n \mid h) = 0$) or zero initial range weight ($w_0(h) = 0$) does **not** imply:
  $$strategy(h) = 0 \quad \text{or} \quad category(h) = \text{none} \quad \text{or} \quad V_{\text{profile}}(n, h) = 0$$
  Profile values ($V_{\text{profile}}$, $Q_{\text{profile}}$, $A_{\text{profile}}$), showdown equities, strategies $\bar{\sigma}(a \mid I, h)$, and intrinsic classifications are defined for every legal represented combo ($h \cap B = \emptyset$), regardless of reach. Reach-weighted aggregations naturally ignore zero-reach combos ($\text{range\_reach\_weight} = 0$). Compact serializers may filter zero-reach combos for artifact size reduction, but the core Result Store preserves them.

### 2.2 Canonical Reach & Conditioning Notation
We introduce explicit, unambiguous notation for reach, mass, and evaluation conditioning:

- $w_0(h)$: Initial preflop probability mass assigned to combo $h$ in player $i$'s starting range.
- $\pi_i(n \mid h)$: Conditional probability that player $i$ reaches node $n$ given they hold combo $h$ (product of player $i$'s action probabilities along the tree path leading to $n$).
- $\pi_{-i}(n \mid h')$: Conditional probability that opponents reach node $n$ given holdings $h'$ (product of opponents' action probabilities along the path).
- $\pi_c(n)$: Cumulative chance reach probability along the tree path from root leading to $n$ (product of public card transition probabilities).
- $\text{range\_reach\_weight}(n, h) = \text{static\_cast<double>}(w_0(h)) \times \text{static\_cast<double>}(\pi_i(n \mid h))$: **Derived** remaining range mass of combo $h$ for player $i$ conditional on player $i$'s own path to $n$.
- $\text{joint\_reach\_mass}(n, h) = w_0(h) \cdot \pi_i(n \mid h) \cdot \Pi_{-i}(n \mid h) \cdot \pi_c(n)$: Joint probability mass of combo $h$ reaching node $n$, incorporating hero, opponent, and chance path probabilities.
- $\text{range\_reach\_mass}(n) = W_i(n) = \sum_{h \in H_i} \text{range\_reach\_weight}(n, h) = \sum_{h \in H_i} w_0(h) \pi_i(n \mid h)$: Total remaining active range mass of player $i$ reaching node $n$ (note: this represents hero-range mass, not joint node probability).
- $\text{joint\_reach\_mass}(n) = \sum_{h \in H_i} \text{joint\_reach\_mass}(n, h)$: Total joint realization probability of reaching node $n$.
- **Precision Rule**: `range_reach_weight` is derived on demand by promoting `range_weight` and `reach_probability` to `double` prior to multiplication, and all aggregations accumulate in `double`.

### 2.3 Value Conditioning & Mathematical Definitions
To eliminate conflation between solver CFR counterfactual state and post-solve profile evaluations, we explicitly define all value quantities with mechanical formulas:

| Quantity | Mathematical Definition | Scope / Perspective | Primary Identity |
| :--- | :--- | :--- | :--- |
| **`combo_profile_value` / $V_{\text{profile}}(n, h)$** | $\sum_a \bar{\sigma}(a \mid I, h) Q_{\text{profile}}(n, h, a)$ | Unweighted combo payoff; acting player perspective | Node + Combo |
| **$Q_{\text{profile}}(n, h, a)$** | Expected payoff after local action intervention $a$ at $n$, following $\bar{\sigma}$ thereafter | Unweighted action payoff; acting player perspective | Node + Combo + Action |
| **`profile_advantage` / $A_{\text{profile}}(n, h, a)$** | $Q_{\text{profile}}(n, h, a) - V_{\text{profile}}(n, h)$ | Profile deviation advantage (never termed regret) | Node + Combo + Action |
| **`range_reach_mass` / $W_i(n)$** | $\sum_{h \in H_i} w_0(h) \pi_i(n \mid h)$ | Total reach weight mass of seat $i$ at node $n$ | Node + Seat |
| **`reach_weighted_ev` / $\text{SeatValue}_i(n)$** | $\sum_{h \in H_i} \text{range\_reach\_weight}(n, h) V_i(n, h)$ | Unnormalized EV mass carried by reaching range | Node + Seat |
| **`conditional_range_ev`** | $\frac{\sum_{h \in H_i} \text{range\_reach\_weight}(n, h) V_i(n, h)}{W_i(n)} = \frac{\text{reach\_weighted\_ev}}{\text{range\_reach\_mass}}$ | Normalized strategic EV conditional on reaching node $n$ | Node + Seat |
| **`counterfactual_value` / $CFV_i(n)$** | $\sum_{h \in H_i} w_0(h) \cdot \Pi_{-i}(n \mid h) \cdot \pi_c(n) \cdot V_i(n, h)$ | CFR-style counterfactual value weighted by opponent & chance reach | Node + Seat |
| **`showdown_equity`** | Expected pot share for $h$ against opponent joint reach distribution $\pi_{-i}$ conditional on reaching $n$ | Reach-weighted opponent distribution; combo holder | Node + Combo |

#### 2.3.1 Counterfactual Value Conditioning (HU & Multiway)
For an extensive-form information set $I$ with private cards $h$:
$$CFV_i(I, h) = \sum_{z \in Z(I, h)} \pi_{-i}(z \mid I, h) \cdot \pi_c(z) \cdot u_i(z)$$
For a concrete node $n$ and seat $i$:
- **Heads-Up (2-player)**:
  $$CFV_i(n) = \sum_{h \in H_i} w_{0,i}(h) \cdot \pi_{-i}(n \mid h) \cdot \pi_c(n) \cdot V_i(n, h)$$
- **Multiway ($N$-player)**:
  $$\Pi_{-i}(n \mid h) = \prod_{j \ne i} \pi_j(n \mid h, \text{relevant hidden state})$$
  $$CFV_i(n) = \sum_{h \in H_i} w_{0,i}(h) \cdot \Pi_{-i}(n \mid h) \cdot \pi_c(n) \cdot V_i(n, h)$$
**Conditioning Contract**:
1. $h \in H_i$ is summed over all legal private holdings of player $i$.
2. $w_{0,i}(h)$ is player $i$'s initial range realization weight.
3. Player $i$'s own historical strategy reach $\pi_i(n \mid h)$ is deliberately excluded (defining counterfactual reach).
4. Opponent reach $\Pi_{-i}(n \mid h)$ is calculated under opponents' strategies conditioned on the card removal state represented by $h$.
5. $V_i(n, h)$ is the downstream profile value under the frozen average profile.
6. $\pi_c(n)$ is the cumulative chance reach probability from root to node $n$.

### 2.4 Values, Strategic EV, and the Canonical Q $\to$ V $\to$ A Chain
The extraction engine strictly evaluates the strategic chain under the exported average profile $\bar{\sigma}$:

1. **Profile Action Value ($Q_{\text{profile}}(n, h, a)$)**:
   $Q_{\text{profile}}(n, h, a)$ is obtained by replacing the acting player's strategy at the current decision node $n$ only with a unit probability on action $a$. Every subsequent player decision uses the frozen exported average strategy associated with its information set:
   - **Local Node Intervention**: Hero fixes action $a$ **only** at this immediate decision node $n$.
   - All subsequent decisions (by hero and opponents) use the frozen exported average strategy profile $\bar{\sigma}$.
   - Chance events follow the exact game probability distribution.
   - Payoff is measured in chips from the acting player's perspective.
2. **Profile Combo Value ($V_{\text{profile}}(n, h)$)**:
   $$V_{\text{profile}}(n, h) = \sum_{a \in A(\mathcal{S})} \bar{\sigma}(a \mid I, h) \cdot Q_{\text{profile}}(n, h, a)$$
   *(Note: $V_{\text{profile}}$ is a node/combo profile evaluation under $\bar{\sigma}$, not raw CFR solver state).*
3. **Profile Deviation Advantage ($A_{\text{profile}}(n, h, a)$)**:
   $$A_{\text{profile}}(n, h, a) = Q_{\text{profile}}(n, h, a) - V_{\text{profile}}(n, h)$$

**Fundamental Mathematical Invariants**:
- **Strategy-Weighted Advantage Identity**:
  For every legal combo $h$ at every player node $n$:
  $$V_{\text{profile}}(n, h) = \sum_{a \in A(\mathcal{S})} \bar{\sigma}(a \mid I, h) Q_{\text{profile}}(n, h, a) \implies \sum_{a \in A(\mathcal{S})} \bar{\sigma}(a \mid I, h) A_{\text{profile}}(n, h, a) = 0$$
- **Heads-Up Zero-Sum Payoff Conservation**:
  For HU zero-sum games under standard payoff conventions:
  $$V_1(n) + V_2(n) = 0 \quad (\text{subject to rake and pot conservation})$$

*Vocabulary Guardrails*:
- **EV**: Refers exclusively to strategic expected game payoff under future betting ($Q, V, A$), where $\text{EV} \in \mathbb{R}$.
- **Equity**: Refers exclusively to showdown pot share assuming no further betting, where $\text{Equity} \in [0, 1]$.
- **Rule**: Never use "equity" as a synonym for "EV" anywhere in the code, comments, schema, or UI.
- **Regret vs. Advantage**: `cumulative_regret` ($R_T^+$) is a solver-state quantity accumulated over iterations; `profile_advantage` ($A_{\text{profile}}$) is a post-solve evaluation quantity under the frozen average profile. $A_{\text{profile}}$ is never termed "regret".

### 2.5 Canonical Phase 0 Contract Box
```
For node n, player i, combo h, action a:

w0(h)                  Initial preflop range mass
pi_i(n | h)            Acting player's conditional reach probability
pi_-i(n | h)           Opponents' conditional reach probability
pi_c(n)                Cumulative chance reach probability along path from root
range_reach_weight     = static_cast<double>(w0(h)) * static_cast<double>(pi_i(n | h))
joint_reach_mass       = w0(h) * pi_i(n | h) * Pi_-i(n | h) * pi_c(n)

V(n, h)                Profile value from player i's perspective
Q(n, h, a)             Profile value after local action intervention a at node n
A(n, h, a)             = Q(n, h, a) - V(n, h)  [Profile advantage]

Identities:
  Sum_a sigma(n, h, a) * Q(n, h, a) = V(n, h)
  Sum_a sigma(n, h, a) * A(n, h, a) = 0  [Strategy-Weighted Advantage Identity]

Aggregations (calculated in double):
  range_reach_mass(n)    = Sum_h w0(h) * pi_i(n | h)  [Hero range mass, not joint prob]
  reach_weighted_ev(n)   = Sum_h range_reach_weight(n, h) * V(n, h)
  conditional_range_ev(n)= reach_weighted_ev(n) / range_reach_mass(n)
  counterfactual_val(n)  = Sum_h w0(h) * Pi_-i(n | h) * pi_c(n) * V(n, h)

Invariants:
  1. Q, V, A are profile-evaluation quantities; CFR cumulative regret R_T+ is solver state.
  2. A is never called regret.
  3. Showdown equity in [0, 1] is never called strategic EV.
  4. Result Store combo surfaces contain only combos legal for the node's public state (h \cap B = empty).
  5. Profile values and intrinsic classifications are defined for every legal represented combo, regardless of reach.
```

### 2.6 CFR State vs. Average-Profile Extraction State
We enforce strict separation between solver state and post-solve extraction state:

| Layer | Quantities Owned | Description |
| :--- | :--- | :--- |
| **CFR Solver State** | $R_T(I, h, a)$, $R_T^+(I, h, a)$, strategy accumulator $\sum w_t \sigma_t$ | Internal solver state for regret minimization and strategy averaging across information sets $I(\mathcal{S}, h)$. |
| **Extraction Result** | $\bar{\sigma}(a \mid I, h)$, $Q_{\text{profile}}(n, h, a)$, $V_{\text{profile}}(n, h)$, $A_{\text{profile}}(n, h, a)$ | Evaluation of the exported strategy profile via backward induction / matrix evaluation. |
| **Diagnostic Regret Surface** | $R_T^+(I, h, a)$ (optional) | Decoupled diagnostic snapshot of solver regrets, excluded from canonical strategy surface. |

*Terminology Guardrail*: `deviation_value` / `profile_advantage` ($A_{\text{profile}}$) is never termed "regret". Solver state persists $R_T^+$ in CFR tables; extraction computes $Q, V, A$ from the frozen profile $\bar{\sigma}$ and presents stored $R_T^+$ separately.

### 2.7 Node Kind Payload Contracts
The Result Store enforces clear payload contracts across tree node kinds:
- **Player Node**:
  - Strategy surface: $\bar{\sigma}(a \mid I, h)$ (keyed by `strategy_context_id`).
  - Value surface: $Q_{\text{profile}}(n, h, a)$, $V_{\text{profile}}(n, h)$, $A_{\text{profile}}(n, h, a)$, and reaches (keyed by `node_id`).
  - Equity surface: Showdown equities over reaching opponent distribution.
  - Classification surface: Intrinsic hand/board categories for each legal combo.
- **Chance Node**:
  - Outcome transition probabilities $P(\text{outcome} \mid \text{chance\_state})$.
  - Child node pointers for each dealt card outcome.
  - Expected incoming combo values: $V_{\text{chance}}(n, h) = \sum_c P(c) V(\text{child}(c), h)$.
- **Terminal Node**:
  - Showdown terminal: exact pot-share payoff based on hand ranks and accumulated pot.
  - Fold terminal: pot awarded to surviving player(s).
  - Terminal values $V_{\text{terminal}}(n, h)$ evaluate directly to terminal utility without action branching.

### 2.8 Exactness, Convergence, and Abstraction
- **Solve Mode (`solve_mode`)**: `normal`, `preview`.
- **Termination Reason (`termination_reason`)**:
  - `iteration_limit`: Configured iteration budget exhausted (including normal scheduled solve completion).
  - `exploitability_target`: Target exploitability reached.
  - `user_interrupted`: Early cancellation requested by user / signal.
- **Convergence Status (`convergence_status`)**:
  - `target_met`: Convergence criterion verified ($\text{exploitability} \le \text{target}$).
  - `iteration_limited`: Reached iteration ceiling without meeting target exploitability.
  - `not_evaluated`: Exploitability was not evaluated.
  *(Note: `termination_reason` and `convergence_status` are independent).*
- **Evaluation Method (`evaluation_method`)**:
  - `exact`: Exact mathematical evaluation of the represented game graph and strategy profile without stochastic rollout sampling. (Contract qualification: exact evaluation of an abstracted game is an exact evaluation of the *abstracted model*, not an exact evaluation of the underlying unrestricted Hold'em game).
  - `sampled`: Derived via Monte Carlo rollouts or lossy sampling.
- **Abstraction Mode (`abstraction_mode`)**: `exact`, `suit_isomorphic`, `range_abstracted`, `mixed`.
- **Version Tracking**: Solved artifacts track both `schema_version` (JSON format version) and `extraction_version` (mathematical extraction contract version).

### 2.9 Showdown Equity vs. Strategic EV
- **Pot-Share Showdown Equity**: Evaluated against the reaching opponent joint distribution conditioned on reaching $n$ under the exported profile $\bar{\sigma}$, assuming uniform remaining card runouts and zero additional betting:
  $$\text{ShowdownEquity}_i(h, n) = \frac{\sum_{h' \cap h = \emptyset} w_{0,-i}(h') \pi_{-i}(n \mid h') \cdot \left( \mathbb{I}(h > h') + \frac{1}{2} \mathbb{I}(h = h') \right)}{\sum_{h' \cap h = \emptyset} w_{0,-i}(h') \pi_{-i}(n \mid h')}$$
- **Equity Conservation**:
  - Heads-Up: $E_1(n) + E_2(n) = 1.0$ at identical joint distribution $\pi_{\text{joint}}$.
  - Multiway ($N$-way): $\sum_{i=1}^N E_i(n) = 1.0$ with fractional tie sharing across all tied players.
  - Open Multiway Implementation: The contract fixes $\sum_i E_i = 1.0$ with fractional ties over the reaching joint distribution; the underlying computation is encapsulated behind `equity_surface` without restricting the solver to naive Cartesian enumeration.

### 2.10 Derived Category Summary Reductions
Category summaries are **strictly derived query reductions**, never canonical primary state:
- **Category Reach Frequency**:
  $$F(C) = \frac{\sum_{h \in C} w_0(h)\pi_i(n \mid h)}{\sum_{h \in H_i} w_0(h)\pi_i(n \mid h)} = \frac{\sum_{h \in C} \text{range\_reach\_weight}(n, h)}{\text{range\_reach\_mass}(n)}$$
- **Category-Conditional Action Frequency**:
  $$F(a \mid C) = \frac{\sum_{h \in C} w_0(h)\pi_i(n \mid h) \cdot \bar{\sigma}(a \mid I, h)}{\sum_{h \in C} w_0(h)\pi_i(n \mid h)}$$
- **Category Average EV & Showdown Equity**:
  $$\overline{\text{EV}}(C) = \frac{\sum_{h \in C} \text{range\_reach\_weight}(n, h) V(n, h)}{\sum_{h \in C} \text{range\_reach\_weight}(n, h)}, \quad \overline{\text{Equity}}(C) = \frac{\sum_{h \in C} \text{range\_reach\_weight}(n, h) \text{Equity}_i(n, h)}{\sum_{h \in C} \text{range\_reach\_weight}(n, h)}$$

---

## 3. Detailed Technical Architecture

### 3.1 Hand Categorization Engine (`eval/categorizer.h`)

The categorizer separates made hand ranking, pair geometry, kicker quality, and draws into intrinsic properties (derivable purely from 2 hole cards + board). Opponent-range-dependent interactions (e.g., blocking an opponent's value/calling range) require strategic range context and are isolated from intrinsic classification.

```
┌────────────────────────────────────────────────────────┐
│             POSTFLOP INTRINSIC CLASSIFICATION          │
├────────────────────────────────────────────────────────┤
│ 1. Made Hand Tier (Strict hierarchy, 1-of-N)          │
│    high_card | pair | two_pair | trips | straight |   │
│    flush | full_house | quads | straight_flush         │
├────────────────────────────────────────────────────────┤
│ 2. Pair Geometry (Pair Source & Position; tier == pair)│
│    source: hole_pair | hole_board_pair |               │
│            board_only_pair | none                      │
│    position: overpair | top_pair | middle_pair |       │
│              bottom_pair | underpair |                 │
│              pocket_pair_below_board | none            │
├────────────────────────────────────────────────────────┤
│ 3. Kicker Quality (Deterministic live-rank position)   │
│    top | strong | medium | weak | none                 │
├────────────────────────────────────────────────────────┤
│ 4. Draw Flags (10-Window enumeration completion mask)  │
│    nut_flush_draw | flush_draw | backdoor_flush_draw   │
│    open_ended_straight | gutshot | double_gutter       │
│    backdoor_straight                                   │
├────────────────────────────────────────────────────────┤
│ 5. Intrinsic Card Removal / Blocker Flags              │
│    nut_flush_blocker | second_nut_blocker              │
└────────────────────────────────────────────────────────┘
```

#### 3.1.1 Dimensional Classification Types & Structs
```cpp
namespace zeta::holdem {

    /// Mutually exclusive made hand rank (strictly ordered hierarchy)
    enum class made_hand_tier : uint8_t {
        high_card = 0,
        pair,
        two_pair,
        trips,
        straight,
        flush,
        full_house,
        quads,
        straight_flush
    };

    /// Source of pair grouping in 5-card evaluated hand
    enum class pair_source : uint8_t {
        none = 0,
        hole_pair,           // Pocket pair held in hole cards
        hole_board_pair,     // One hole card matches one board card
        board_only_pair      // Board contains a pair; hole cards match no board card
    };

    /// Board-relative pair position.
    /// Invariant: pair_position is classified ONLY after made_hand_tier is established.
    /// pair_position is meaningful strictly when made_hand_tier == pair and pair_source != board_only_pair.
    enum class pair_position : uint8_t {
        none = 0,
        overpair,                 // Pocket pair > highest board rank
        top_pair,                 // Hole card matches highest distinct board rank
        middle_pair,              // Hole card matches second-highest distinct board rank
        bottom_pair,              // Hole card matches lowest distinct board rank
        underpair,                // Pocket pair between highest and lowest board ranks
        pocket_pair_below_board   // Pocket pair < lowest board rank
    };

    /// Deterministic kicker quality based on live rank position in 5-card hand (Zeta Intrinsic Kicker Classification).
    enum class kicker_quality : uint8_t {
        none = 0,
        weak,
        medium,
        strong,
        top
    };

    /// Straight draw completion type
    enum class straight_draw_type : uint8_t {
        none = 0,
        backdoor_straight,
        gutshot,
        double_gutter,
        open_ended_straight
    };

    /// Straight draw completion details based on one-card immediate completions
    struct straight_draw_info {
        uint16_t immediate_completion_ranks = 0;   // Bitmask of single ranks [2..A] completing straight
        uint8_t immediate_completion_count = 0;     // Number of distinct immediate completing ranks
        bool has_backdoor = false;                 // 2-card backdoor straight possibility on flop
        straight_draw_type type = straight_draw_type::none;
    };

    /// Orthogonal draw flags
    enum class draw_flags : uint16_t {
        none                = 0,
        backdoor_flush_draw = 1 << 0,
        flush_draw          = 1 << 1,
        nut_flush_draw      = 1 << 2,
        backdoor_straight   = 1 << 3,
        gutshot             = 1 << 4,
        open_ended_straight = 1 << 5,
        double_gutter       = 1 << 6
    };

    /// Intrinsic blocker / removal flags (derivable from hand + board alone)
    enum class blocker_flags : uint16_t {
        none                = 0,
        nut_flush_blocker   = 1 << 0,
        second_nut_blocker  = 1 << 1
    };

    /// Intrinsic hand classification (8 bytes total)
    struct hand_category_classification {
        made_hand_tier made_tier = made_hand_tier::high_card; // 1 byte
        pair_source source = pair_source::none;                // 1 byte
        pair_position pair_pos = pair_position::none;          // 1 byte
        kicker_quality kicker = kicker_quality::none;          // 1 byte
        draw_flags draws = draw_flags::none;                   // 2 bytes
        blocker_flags blockers = blocker_flags::none;          // 2 bytes
    };
    static_assert(sizeof(hand_category_classification) == 8, "hand_category_classification must be exactly 8 bytes");

    /// Intrinsic hand classifier (pure hand + board)
    [[nodiscard]] hand_category_classification classify_postflop_hand(
        combination_index combo,
        card_mask board_mask,
        street current_street) noexcept;

    /// Straight draw evaluator based on complete 10-window enumeration
    [[nodiscard]] straight_draw_info evaluate_straight_draw(
        combination_index combo,
        card_mask board_mask,
        street current_street) noexcept;
}
```

#### 3.1.2 Authoritative Algorithmic Rules & Truth Tables

This is a Phase 0.5 freeze point: the categorizer truth table must be fixed before any extraction code is written. Every dimension (`made_hand_tier`, `pair_source`, `pair_position`, `kicker_quality`, `draw_flags`, `blocker_flags`) is defined by an explicit algorithmic contract with enumerated precedence, not by examples or ad hoc fixtures.

1. **Paired-Board Geometry, Pair Source, and Pair Position**:
   - **Order of Execution**: Pair source and pair position are evaluated **only after** the 5-card made hand evaluation fixes `made_hand_tier == pair`. (If hero makes two pair, trips, or a full house on a paired board, `pair_position` and `pair_source` evaluate to `none`).
   - Let $R_0 > R_1 > \dots > R_{m-1}$ be the sorted distinct board ranks.
   - For a holding with hole cards $H_0, H_1$:
     - If hero holds a pocket pair ($H_0 == H_1$):
       - `pair_source` = `hole_pair`.
       - If $H_0 > R_0$: `pair_pos` = `overpair`.
       - If $R_{m-1} < H_0 < R_0$ and does not match board: `pair_pos` = `underpair`.
       - If $H_0 < R_{m-1}$: `pair_pos` = `pocket_pair_below_board`.
     - If hero holds unpaired cards matching distinct board ranks:
       - `pair_source` = `hole_board_pair`.
       - If matching $R_0$: `pair_pos` = `top_pair`.
       - If matching $R_1$: `pair_pos` = `middle_pair`.
       - If matching $R_k$ ($k \ge 2$): `pair_pos` = `bottom_pair`.
     - If the board contains a pair but hero's hole cards match no board card and are unpaired:
       - `pair_source` = `board_only_pair`.
       - `pair_pos` = `none`.

   **Paired-Board & Geometry Truth Table**:
   | Board | Hero Combo | Best 5-Card Hand | `made_hand_tier` | `pair_source` | `pair_pos` | Notes |
   | :--- | :--- | :--- | :--- | :--- | :--- | :--- |
   | `Ks Kd 8s` | `Qh 8c` | `K K 8 8 Q` $\implies$ Two Pair (K and 8) | `two_pair` | `none` | `none` | Hero paired the 8 on a paired K board |
   | `Ks Kd 8s` | `Qh Jc` | `K K Q J 8` $\implies$ Pair of Kings | `pair` | `board_only_pair` | `none` | Board has pair; hole cards match no board card |
   | `Ks Kd 8s` | `8h 8c` | `K K 8 8 8` $\implies$ Full House (8s full of Ks) | `full_house` | `none` | `none` | Hand rank exceeds pair |
   | `Ks Kd 8s` | `Kh Qh` | `K K K Q 8` $\implies$ Three of a Kind (Ks) | `trips` | `none` | `none` | Hand rank exceeds pair |
   | `Ks Kd 8s` | `Ah Ac` | `A A K K 8` $\implies$ Two Pair (Aces & Kings) | `two_pair` | `none` | `none` | Pocket pair above board pair forms two pair |
   | `Ks Jd 8s` | `Ah As` | `A A K J 8` $\implies$ Pair of Aces | `pair` | `hole_pair` | `overpair` | Pocket pair above highest board rank ($R_0=K$) |
   | `Ks Jd 8s` | `Qh 8c` | `8 8 K J Q` $\implies$ Pair of 8s | `pair` | `hole_board_pair` | `bottom_pair` | 8 is lowest distinct board rank ($R_2$) |
   | `Ks Jd 8s` | `9h 9c` | `9 9 K J 8` $\implies$ Pair of 9s | `pair` | `hole_pair` | `underpair` | Pocket pair between J and 8 |
   | `Ks Jd 8s` | `2h 2c` | `2 2 K J 8` $\implies$ Pair of 2s | `pair` | `hole_pair` | `pocket_pair_below_board` | Pocket pair below lowest board rank ($R_2=8$) |

2. **Deterministic Live-Rank Kicker Quality (Zeta Intrinsic Kicker Classification)**:
   Kicker quality is deterministically partitioned based on the live rank ordering of the highest kicker card contributing to hero's evaluated 5-card hand. This is defined explicitly as Zeta's intrinsic kicker classification convention:
   - **Definition**: `kicker_rank_1` ($K$) is the rank of the highest non-paired/non-group card in hero's best 5-card hand (e.g. for made hand tier `pair`, `two_pair`, `trips`, `high_card`).
   - Let $L = (L_0 > L_1 > L_2 > \dots > L_{p-1})$ be the ordered descending list of available live ranks in $\{2 \dots A\}$ that do not appear in the made group(s) of the 5-card hand (live uncommitted kicker candidate ranks).
   - The kicker rank $K$ maps deterministically to live-rank ordinal $j$ where $K = L_j$:
     - $j = 0$ ($K == L_0$): `top` (e.g., Ace kicker for Pair of Kings on `K-8-2` board).
     - $j = 1$ ($K == L_1$): `strong` (e.g., Queen kicker for Pair of Kings on `K-8-2` board with Ace unblocked).
     - $j \in \{2, 3\}$ ($K \in \{L_2, L_3\}$): `medium` (e.g., Jack or Ten kicker).
     - $j \ge 4$ ($K \le L_4$): `weak` (e.g., 9 or lower).
     - If the 5-card hand contains no kickers (e.g., full house, quads, straight, flush, straight flush): `none`.

   **Exhaustive Kicker Classification Examples**:
   - **High Card** (`Kd 8s 5c` + `Ah Qc`): Best hand `A-K-Q-8-5`. Group: High card Ace. $K = K$ ($L_0 = K$) $\implies$ `top`.
   - **Pair** (`Ks 8d 2c` + `Kh Qc`): Best hand `K-K-Q-8-2`. Made pair: Kings. Live ranks: `A, Q, J, T, 9, ...`. $K = Q$. Since $A$ is live ($L_0 = A$), $Q = L_1 \implies j = 1 \implies$ `strong`.
   - **Two Pair** (`Ks 8d 2c` + `Kh 8c`): Best hand `K-K-8-8-2` with hole kicker `2c` vs `Kh 8s` holding `As 2c` yielding `K-K-8-8-A`. For `A` kicker, $K = A = L_0 \implies$ `top`. For `2` kicker, $K = 2 \le L_4 \implies$ `weak`.
   - **Trips** (`Ks Kd 2c` + `Kh Qh`): Best hand `K-K-K-Q-2`. Made trips: Kings. Live ranks: `A, Q, J, ...`. $K = Q = L_1 \implies$ `strong`.

3. **Complete 10-Window Straight-Draw Algorithm**:
   Straight draw evaluation is performed by algorithmic window enumeration across all 10 possible 5-rank straight spans:
   $$\begin{aligned}
   W_0 &= [A, 2, 3, 4, 5] \quad (\text{Wheel}) \\
   W_1 &= [2, 3, 4, 5, 6] \\
   W_2 &= [3, 4, 5, 6, 7] \\
   W_3 &= [4, 5, 6, 7, 8] \\
   W_4 &= [5, 6, 7, 8, 9] \\
   W_5 &= [6, 7, 8, 9, 10] \\
   W_6 &= [7, 8, 9, 10, J] \\
   W_7 &= [8, 9, 10, J, Q] \\
   W_8 &= [9, 10, J, Q, K] \\
   W_9 &= [10, J, Q, K, A] \quad (\text{Broadway})
   \end{aligned}$$
   - **Step 1: Enumerate 5-Rank Windows**: For each straight window $W_k$ ($k=0 \dots 9$), compute the set of missing ranks $M_k = W_k \setminus \text{ranks}(\text{hand} \cup \text{board})$.
   - **Step 2: Missing Rank Analysis**:
     - $|M_k| = 0 \implies$ Made straight (`made_hand_tier >= straight`, draw evaluation returns `none`).
     - $|M_k| = 1 \implies$ The missing rank $r \in M_k$ is added to `immediate_completion_ranks` bitmask.
     - $|M_k| = 2$ (Flop only) $\implies$ If both missing ranks $r_1, r_2 \in M_k$ are distinct, legally unblocked in the deck, and neither card duplicates existing made ranks, record a candidate 2-card backdoor straight pattern.
   - **Step 3: Classify from Completion Set & Window Structure**:
     - `open_ended_straight`: Hand has 2 immediate completion ranks that form the open boundaries of an unbroken 4-card sequence (e.g., $[8, 9, 10, J]$ completed by $7$ or $Q$).
     - `double_gutter`: Hand has 2 immediate completion ranks filling internal gaps across 2 distinct gapped windows (e.g., $[8, 10, Q, K]$ completed by $9$ or $J$; or $A-J-9-7-5$ completed by $K$ or $8$).
     - `gutshot`: Hand has exactly 1 immediate completion rank (e.g., $9-T-Q-K$ needing $J$; Broadway $T-J-Q-K$ needing $A$; Wheel $A-2-3-4$ needing $5$).
     - `backdoor_straight`: (Flop only) At least one valid 2-card backdoor straight sequence is available across turn and river.
     - **Orthogonality Invariant**: `backdoor_straight` is **not mutually exclusive** with an immediate straight draw. For example, a holding can simultaneously possess an immediate gutshot in one straight window and a backdoor straight pattern in another.
   - **Oracle Verification**: Phase 0.5 oracle exhaustively validates all $\binom{52}{2} \times \binom{50}{3} = 1326 \times 19600 \approx 2.6 \times 10^7$ flop rank configurations against this window evaluator.

   **Flop AsQs on Ks-Jh-8s Example**:
   - Board: `Ks Jh 8s`, Hole cards: `As Qs`. Spades: `Ks 8s As Qs` (4 spades) $\implies$ `flush_draw`.
   - Ranks present: $A, K, Q, J, 8$.
   - Window $10-J-Q-K-A$: Ranks present are $A, K, Q, J$. Missing rank is $10$ ($|M| = 1$) $\implies$ Immediate completion rank $\{10\} \implies$ `gutshot`.
   - Window $8-9-10-J-Q$: Ranks present are $8, J, Q$. Missing ranks are $9, 10$ ($|M| = 2$) $\implies$ 2-card future backdoor straight pattern needing 9 and 10 on turn and river $\implies$ `backdoor_straight`.
   - Final classification: `high_card`, `draws: flush_draw | gutshot | backdoor_straight`, `blockers: nut_flush_blocker`.

4. **Algorithmic Board-Flush Blocker Semantics (Phase 1.6)**:
   Intrinsic card removal is determined strictly from hole cards, board texture, and current street (isolated in Phase 1.6 to keep the core made/draw categorizer pure):
   - For each board suit $S$ with multiplicity $\ge 2$ (flop) or $\ge 3$ (turn/river):
     - Determine the maximum possible flush rank achievable by any legal opponent hand:
       $$R_{\text{nut\_flush}}(S) = \max(\{r \in \{2 \dots A\} \mid r \notin \text{board\_ranks}(S)\})$$
     - Determine the second-highest non-board rank $R_{2\text{nd\_flush}}(S)$.
     - **Rules**:
       - If hero holds a card of suit $S$ with rank $R_{\text{nut\_flush}}(S)$ and has not already made a flush $\implies$ `nut_flush_blocker`.
       - If hero holds $R_{2\text{nd\_flush}}(S)$ (and does not hold $R_{\text{nut\_flush}}(S)$) and has not made a flush $\implies$ `second_nut_blocker`.
   - *Range-Relative Blockers Deferred*: Range-dependent blockers (blocking an opponent's value region or calling range) depend on strategy profiles and opponent range distributions; they are deferred to Phase 5 under `range_interaction_classification`.

---

### 3.2 High-Performance In-Memory Contiguous Offset-Indexed Result Store (`cfr/extraction/result_store.h`)

The Result Store uses a **contiguous offset-indexed / CSR-style** memory architecture. It provides dense rectangular surfaces stored in flat, contiguous memory buffers with indexed offsets, eliminating fragmented heap allocations while ensuring $O(1)$ zero-allocation lookups. The in-memory Result Store is the **canonical extracted result model**, while JSON serialization is an export projection.

```
┌────────────────────────────────────────────────────────────────────────┐
│               CONTIGUOUS OFFSET-INDEXED RESULT STORE MODEL             │
├────────────────────────────────────────────────────────────────────────┤
│ nodes: span<const node_record> (purely structural topology index)      │
│   ├── [node_id]: strategy_context_id, public_state_id, kind, offsets...│
│                                                                        │
│ strategy_surfaces: span<const strategy_surface_record>                 │
│   ├── [strategy_context_id]: strategy_begin, combo_count, action_count│
│                                                                        │
│ strategy_entries: span<const strategy_surface_entry> (dense flat array)│
│   ├── offset = strategy_begin + combo_local_idx * action_count + a    │
│   └── payload: average_strategy                                        │
│                                                                        │
│ node_seat_values: span<const seat_value>                               │
│   ├── [seat_val_begin + seat_idx]: range_reach_mass, reach_weighted_ev,│
│   │                                conditional_range_ev, cfv           │
│                                                                        │
│ combo_reaches: span<const combo_reach_entry> (flat dense array)        │
│   └── payload: range_weight, reach_probability                         │
│                                                                        │
│ combo_values: span<const combo_value_entry> (flat dense array)         │
│   └── payload: combo_profile_value (V_profile)                         │
│                                                                        │
│ action_values: span<const action_value_entry> (flat dense matrix)      │
│   ├── offset = action_val_begin + combo_local_idx * action_count + a  │
│   └── payload: q_profile, profile_advantage (A_profile)                │
│                                                                        │
│ equity_surface: span<const float> (flat dense array)                   │
│   └── payload: showdown_equity [0, 1]                                  │
│                                                                        │
│ classification_surface: span<const hand_category_classification>       │
│   └── payload: made_tier, pair_source, pair_pos, kicker, draws, blocker│
└────────────────────────────────────────────────────────────────────────┘
```

#### 3.2.1 Canonical Data Structures & Offset Layout Contracts
```cpp
namespace zeta::holdem::cfr {

    /// Extracted strategy surface entry (Projection of solved CFR state: average strategy only)
    /// Dense rectangular layout: combo and action indices are structurally encoded by offset.
    struct strategy_surface_entry {
        float average_strategy = 0.0f;
    };

    /// Optional diagnostic solver state (Cumulative regrets snapshot)
    struct regret_surface_entry {
        double cumulative_regret = 0.0;
    };

    /// Reach surface entry per legal combo
    struct combo_reach_entry {
        float range_weight = 0.0f;          // Initial preflop weight w_0(h)
        float reach_probability = 0.0f;     // Conditional reach probability pi_i(n|h)
    };

    /// Evaluated combo profile value
    struct combo_value_entry {
        double combo_profile_value = 0.0;   // V_profile(n, h)
    };

    /// Evaluated action profile value
    /// Dense rectangular layout: combo and action indices are structurally encoded by offset.
    struct action_value_entry {
        double q_profile = 0.0;             // Q_profile(n, h, a)
        double profile_advantage = 0.0;     // A_profile(n, h, a) = Q - V
    };

    /// Evaluated scalar metrics for a player/seat at a game node
    struct seat_value {
        double range_reach_mass = 0.0;       // Sum_h w_0(h) pi_i(n|h) [Hero active range mass]
        double reach_weighted_value = 0.0;   // Sum_h range_reach_weight * V_i(n, h)
        double conditional_range_ev = 0.0;   // reach_weighted_value / range_reach_mass
        double counterfactual_value = 0.0;   // Sum_h w_0(h) Pi_-i(n|h) pi_c(n) V_i(n, h)
    };

    /// Structural record defining the strategy surface dimensions for a range decision context
    struct strategy_surface_record {
        uint32_t strategy_begin = 0;
        uint32_t combo_count = 0;
        uint16_t action_count = 0;
    };

    /// Purely structural record for a Concrete Game Node
    struct node_record {
        uint32_t node_id = 0;
        uint32_t strategy_context_id = 0;
        uint32_t public_state_id = 0;
        node_kind kind = node_kind::player;
        uint8_t acting_seat = 0;
        
        uint32_t combo_begin = 0;
        uint32_t combo_count = 0;
        
        uint32_t action_val_begin = 0;
        uint16_t action_count = 0;

        uint32_t seat_value_begin = 0;
        uint16_t seat_value_count = 0;
    };

    /// Non-owning Query Views with O(1) Lookups

    class strategy_view {
    public:
        strategy_view(
            std::span<const strategy_surface_entry> entries,
            uint32_t combo_count,
            uint16_t action_count) noexcept
            : entries_(entries), combo_count_(combo_count), action_count_(action_count) {}
        
        [[nodiscard]] std::span<const strategy_surface_entry> entries() const noexcept { return entries_; }
        
        /// O(1) indexed lookup: offset = combo_local_index * action_count + action_index
        [[nodiscard]] float frequency(uint32_t combo_local_index, action_index action) const noexcept {
            return entries_[combo_local_index * action_count_ + action].average_strategy;
        }

    private:
        std::span<const strategy_surface_entry> entries_;
        uint32_t combo_count_ = 0;
        uint16_t action_count_ = 0;
    };

    class value_view {
    public:
        value_view(
            std::span<const combo_reach_entry> reaches,
            std::span<const combo_value_entry> combo_vals,
            std::span<const action_value_entry> action_vals,
            const seat_value& seat_val,
            uint16_t action_count) noexcept
            : reaches_(reaches), combo_vals_(combo_vals), action_vals_(action_vals),
              seat_val_(seat_val), action_count_(action_count) {}

        [[nodiscard]] double range_reach_mass() const noexcept { return seat_val_.range_reach_mass; }
        [[nodiscard]] double reach_weighted_ev() const noexcept { return seat_val_.reach_weighted_value; }
        [[nodiscard]] double conditional_range_ev() const noexcept { return seat_val_.conditional_range_ev; }
        [[nodiscard]] double counterfactual_value() const noexcept { return seat_val_.counterfactual_value; }
        
        /// O(1) direct offset lookups
        [[nodiscard]] double combo_ev(uint32_t combo_local_index) const noexcept {
            return combo_vals_[combo_local_index].combo_profile_value;
        }
        [[nodiscard]] double q_value(uint32_t combo_local_index, action_index action) const noexcept {
            return action_vals_[combo_local_index * action_count_ + action].q_profile;
        }
        [[nodiscard]] double profile_advantage(uint32_t combo_local_index, action_index action) const noexcept {
            return action_vals_[combo_local_index * action_count_ + action].profile_advantage;
        }
        [[nodiscard]] float reach_probability(uint32_t combo_local_index) const noexcept {
            return reaches_[combo_local_index].reach_probability;
        }
        [[nodiscard]] float range_weight(uint32_t combo_local_index) const noexcept {
            return reaches_[combo_local_index].range_weight;
        }
        [[nodiscard]] double range_reach_weight(uint32_t combo_local_index) const noexcept {
            return static_cast<double>(reaches_[combo_local_index].range_weight) *
                   static_cast<double>(reaches_[combo_local_index].reach_probability);
        }

    private:
        std::span<const combo_reach_entry> reaches_;
        std::span<const combo_value_entry> combo_vals_;
        std::span<const action_value_entry> action_vals_;
        seat_value seat_val_{};
        uint16_t action_count_ = 0;
    };

    class equity_view {
    public:
        equity_view(std::span<const float> equities) noexcept : equities_(equities) {}
        [[nodiscard]] double showdown_equity(uint32_t combo_local_index) const noexcept {
            return static_cast<double>(equities_[combo_local_index]);
        }

    private:
        std::span<const float> equities_;
    };

    class category_view {
    public:
        category_view(
            std::span<const hand_category_classification> categories,
            std::span<const combo_reach_entry> reaches,
            std::span<const combo_value_entry> values,
            std::span<const strategy_surface_entry> strategies,
            uint16_t action_count) noexcept
            : categories_(categories), reaches_(reaches), values_(values),
              strategies_(strategies), action_count_(action_count) {}

        [[nodiscard]] hand_category_classification classification(uint32_t combo_local_index) const noexcept {
            return categories_[combo_local_index];
        }

        /// Derived on-demand aggregation (returns allocated vector since this is a query reduction)
        [[nodiscard]] std::vector<category_summary> compute_summaries() const;

    private:
        std::span<const hand_category_classification> categories_;
        std::span<const combo_reach_entry> reaches_;
        std::span<const combo_value_entry> values_;
        std::span<const strategy_surface_entry> strategies_;
        uint16_t action_count_ = 0;
    };

    /// Unified Node View facilitating natural traversal:
    /// node.strategy(), node.values(), node.equity(), node.categories()
    class node_view {
    public:
        node_view(
            const node_record& record,
            strategy_view strat,
            value_view val,
            equity_view eq,
            category_view cat) noexcept
            : record_(record), strat_(strat), val_(val), eq_(eq), cat_(cat) {}

        [[nodiscard]] uint32_t node_id() const noexcept { return record_.node_id; }
        [[nodiscard]] uint32_t strategy_context_id() const noexcept { return record_.strategy_context_id; }
        [[nodiscard]] uint32_t public_state_id() const noexcept { return record_.public_state_id; }
        [[nodiscard]] uint8_t acting_seat() const noexcept { return record_.acting_seat; }

        [[nodiscard]] strategy_view strategy() const noexcept { return strat_; }
        [[nodiscard]] value_view values() const noexcept { return val_; }
        [[nodiscard]] equity_view equity() const noexcept { return eq_; }
        [[nodiscard]] category_view categories() const noexcept { return cat_; }

    private:
        node_record record_;
        strategy_view strat_;
        value_view val_;
        equity_view eq_;
        category_view cat_;
    };

    /// High-Performance Result Store
    class result_store {
    public:
        // Unified node traversal
        [[nodiscard]] node_view node(uint32_t node_id) const;

        // Strategy-context-keyed range strategy query
        [[nodiscard]] strategy_view context_strategy(uint32_t strategy_context_id) const;
        
        // Node-keyed independent surface queries
        [[nodiscard]] value_view node_values(uint32_t node_id) const;
        [[nodiscard]] equity_view node_equity(uint32_t node_id) const;
        [[nodiscard]] category_view node_categories(uint32_t node_id) const;

        [[nodiscard]] uint32_t node_strategy_context_id(uint32_t node_id) const;

    private:
        std::vector<node_record> nodes_;
        std::vector<strategy_surface_record> strategy_surfaces_;
        std::vector<seat_value> node_seat_values_;
        
        // Contiguous flat storage surfaces
        std::vector<strategy_surface_entry> strategy_entries_;
        std::vector<regret_surface_entry> regret_entries_; // Diagnostic solver state (optional)
        std::vector<combo_reach_entry> combo_reaches_;
        std::vector<combo_value_entry> combo_values_;
        std::vector<action_value_entry> action_values_;
        std::vector<float> equity_surface_;
        std::vector<hand_category_classification> classification_surface_;
    };
}
```

#### 3.2.2 Lookup Invariants & Flat Indexing Contract
- **Strategy Storage Aliasing Invariant**:
  Strategy storage in `strategy_entries_` is owned exactly once per `strategy_context_id`. Multiple concrete game nodes that share the same public decision context reference the identical `strategy_surface_record` and never duplicate strategy entries.
- **Combo Domain Offset Sharing Invariant**:
  For any node $n$ with structural fields `combo_begin` and `combo_count`, the following surfaces share the exact same contiguous combo indexing domain and offset `combo_begin + i` ($i \in [0, \text{combo\_count})$):
  $$\text{combo\_reaches}[\text{combo\_begin} + i]$$
  $$\text{combo\_values}[\text{combo\_begin} + i]$$
  $$\text{equity\_surface}[\text{combo\_begin} + i]$$
  $$\text{classification\_surface}[\text{combo\_begin} + i]$$
- **Dense Zero-Based ID Invariant**:
  `node_id` and `strategy_context_id` are dense, contiguous zero-based integers ($0 \dots N-1$). The internal storage vectors `nodes_[node_id]` and `strategy_surfaces_[strategy_context_id]` are indexed directly in $O(1)$ without hash maps or binary searches.
- **Seat Value Indexing Contract**:
  - `node_record` specifies `seat_value_begin` and `seat_value_count`. For active seat $s$ at node $n$, `node_seat_values_[seat_value_begin + s]` contains that seat's evaluated metrics.
- **Flat Indexing Contract**: Data is stored in dense combo-major rectangular matrices. For any query at node $n$ with local combo index $c \in [0, \text{combo\_count})$ and legal action $a \in [0, \text{action\_count})$:
  $$\text{action\_value\_offset} = \text{action\_val\_begin} + c \cdot \text{action\_count} + a$$
  $$\text{strategy\_offset} = \text{strategy\_begin} + c \cdot \text{action\_count} + a$$
  The index is represented structurally by the flat offset, so each entry contains only its payload and requires no redundant combo/action identifiers.
- **Canonical Combo & Action Ordering**:
  - `combo_local_index` $c$ maps deterministically to the $c$-th non-board-conflicting `combination_index` ($0 \dots 1325$) in ascending order.
  - `action_index` $a$ maps deterministically to the game graph's canonical legal-action table position for that strategy context.
- **Combo Universe Invariant**: Result Store combo surfaces represent **only combos legal for the node's public state** ($h \cap B = \emptyset$).
  - Zero range weight ($w_0(h) = 0$): Valid represented entry.
  - Zero reach ($\pi_i(n \mid h) = 0$): Valid represented entry.
  - Board-conflicting illegal combo ($h \cap B \ne \emptyset$): Omitted from the node's legal combo domain.
  - Illegal action ($a \notin A(\mathcal{S})$): Omitted from the node's legal action domain.
- **String Separation**: String representations (`"AhKd"`, `"AKo"`) are omitted entirely from the core Result Store. Conversions from combo indices to strings occur only during JSON serialization or UI display.
- **Hot Path Direct Buffer Extraction**: The Result Store is a canonical output model and query layer. During the solver's hot extraction recursion, workers write directly into pre-allocated contiguous flat spans rather than repeatedly allocating or instantiating `node_view` wrappers.

#### 3.2.3 Memory Footprint Model & Budgets
To guarantee high performance across trees with thousands of nodes, we establish an explicit memory footprint budget:

| Surface Component | Data Type & Size | Bytes / Combo ($A$ actions) | Example ($A = 2$) | Example ($A = 8$) |
| :--- | :--- | :--- | :--- | :--- |
| **Strategy Surface** | `float average_strategy` (4B) | $4 \times A$ bytes | 8 B | 32 B |
| **Action Value Surface** | `double q_profile` (8B) + `double profile_advantage` (8B) | $16 \times A$ bytes | 32 B | 128 B |
| **Combo Reach Surface** | `float range_weight` (4B) + `float reach_probability` (4B) | 8 bytes | 8 B | 8 B |
| **Combo Value Surface** | `double combo_profile_value` (8B) | 8 bytes | 8 B | 8 B |
| **Equity Surface** | `float showdown_equity` (4B) | 4 bytes | 4 B | 4 B |
| **Classification Surface**| `hand_category_classification` (8B struct) | 8 bytes | 8 B | 8 B |
| **Total per Legal Combo**| — | **$28 + 20A$ bytes** | **68 B** | **188 B** |

- **Per-Node Memory Cost** (for $\approx 1100$ legal postflop combos):
  - $A = 2$ actions (check/bet): $1100 \times 68 \text{ B}$ plus fixed record headers $\approx \mathbf{74.88\text{ KB}}$ per node.
  - $A = 8$ actions: $1100 \times 188 \text{ B} \approx \mathbf{206.8\text{ KB}}$ per node.
- **Full Tree Budgets**:
  - **River Solve** (~50–200 nodes): $5 \text{ MB} - 40 \text{ MB}$ total memory footprint.
  - **Turn Solve** (~500–2,000 nodes): $50 \text{ MB} - 250 \text{ MB}$.
  - **Flop Solve** (thousands of nodes): Tiered export modes (`summary`, `standard`, `full`) ensure solves remain well within standard workstation budgets (< 500 MB).

---

### 3.3 Artifact Representation & Schema v4 (`cli/solve_cli.h`)

#### 3.3.1 Boundary Structs & Projection Modes
Schema v4 represents an **explicit artifact projection** of the binary Result Store. To balance artifact compactness and full auditability, the exporter defines three deterministic export modes:

| Surface Component | `summary` | `standard` (default) | `full` |
| :--- | :---: | :---: | :---: |
| **Metadata & Solve Status** | ✓ | ✓ | ✓ |
| **Public States & Topology** | ✓ | ✓ | ✓ |
| **Action Tables & Seat Values** | ✓ | ✓ | ✓ |
| **Range Action Frequencies** | ✓ | ✓ | ✓ |
| **Category Summaries (Derived)** | ✓ | ✓ | ✓ |
| **Strategy per combo ($\bar{\sigma}$)** | — | ✓ (Player decision nodes) | ✓ (All nodes) |
| **EV per combo ($V_{\text{profile}}$)** | — | ✓ (Player decision nodes) | ✓ (All nodes) |
| **Action Q / A ($Q_{\text{profile}}, A_{\text{profile}}$)** | — | ✓ (Player decision nodes) | ✓ (All nodes) |
| **Showdown Equity** | — | ✓ (Player decision nodes) | ✓ (All nodes) |
| **Classification per combo** | — | ✓ (Player decision nodes) | ✓ (All nodes) |

Public board state is indexed via `public_state_id` to prevent duplicating string boards across every game node.

```cpp
namespace zeta::holdem {

    enum class artifact_export_mode : uint8_t {
        summary,
        standard,
        full
    };

    enum class solve_status : uint8_t {
        iteration_limited,
        converged,
        not_evaluated
    };

    enum class solve_mode : uint8_t {
        normal,
        preview
    };

    enum class termination_reason : uint8_t {
        iteration_limit,        // Configured iteration budget exhausted (normal schedule completion)
        exploitability_target,  // Converged to target exploitability
        user_interrupted        // Early cancellation requested
    };

    enum class convergence_status : uint8_t {
        target_met,
        iteration_limited,
        not_evaluated
    };

    enum class evaluation_method : uint8_t {
        exact,
        sampled
    };

    enum class abstraction_mode : uint8_t {
        exact,
        suit_isomorphic,
        range_abstracted,
        mixed
    };

    struct action_table_entry {
        uint16_t action_index = 0;
        std::string action;                  // e.g., "bet 75%"
        uint32_t child_node_id = cfr::game_graph::INVALID_NODE;
    };

    struct action_frequency_detail {
        uint16_t action_index = 0;
        float frequency = 0.0f;
    };

    struct strategy_action_detail {
        uint16_t action_index = 0;
        float frequency = 0.0f;
        std::optional<double> cumulative_regret; // Optional diagnostic solver state
    };

    struct action_value_detail {
        uint16_t action_index = 0;
        double q_profile = 0.0;             // Q_profile(n, h, a)
        double profile_advantage = 0.0;     // A_profile(n, h, a) = Q - V
    };

    struct combo_detail {
        uint16_t combination_index = 0;      // Canonical global combo index [0..1325]
        std::string hand;                    // Serialized string e.g. "AhKd"
        std::string hand_class;              // Serialized string e.g. "AKo"
        float range_weight = 1.0f;           // Initial range weight w_0(h)
        float reach_probability = 1.0f;      // Conditional reach pi_i(n|h)
        double ev = 0.0;                     // V_profile(n, h)
        double showdown_equity = 0.0;        // Exact showdown equity [0, 1]
        hand_category_classification category{};
        std::vector<strategy_action_detail> strategy;
        std::vector<action_value_detail> action_values;
    };

    struct category_summary_item {
        std::string category_name;
        double frequency = 0.0;              // F(C)
        double range_weight = 0.0;           // Reach mass sum
        double average_ev = 0.0;             // Reach-weighted strategic EV
        double average_showdown_equity = 0.0;// Reach-weighted showdown equity
        std::vector<action_frequency_detail> action_frequencies; // F(a|C)
    };

    struct category_summaries_projection {
        uint32_t derivation_version = 1;
        std::vector<category_summary_item> items;
    };

    struct seat_value_entry {
        uint8_t seat = 0;
        double range_reach_mass = 0.0;       // Sum_h w_0(h) pi_i(n|h)
        double reach_weighted_value = 0.0;   // Sum_h range_reach_weight * V_i(n, h)
        double conditional_range_ev = 0.0;   // reach_weighted_value / range_reach_mass
        double counterfactual_value = 0.0;   // Sum_h w_0(h) Pi_-i(n|h) pi_c(n) V_i(n, h)
    };

    struct solve_metadata {
        uint64_t iterations = 0;
        solve_status status = solve_status::iteration_limited;
        solve_mode mode = solve_mode::normal;
        termination_reason termination = termination_reason::exploitability_target;
        convergence_status convergence = convergence_status::target_met;
        std::optional<double> exploitability_mbb;
        std::optional<double> target_exploitability_mbb;
        std::string averaging_mode = "linear";
    };

    struct artifact_metadata {
        uint32_t schema_version = 4;
        uint32_t extraction_version = 1;
        artifact_export_mode export_mode = artifact_export_mode::standard;
        evaluation_method evaluation = evaluation_method::exact;
        abstraction_mode abstraction = abstraction_mode::exact;
        std::string split_pot_policy = "fractional_tie";
    };

    struct public_state_entry {
        uint32_t public_state_id = 0;
        std::vector<std::string> board;      // Board cards e.g. ["Ah", "Kd", "2s"]
        std::string street;                  // "flop", "turn", "river"
    };

    struct solved_node {
        uint32_t node_id = cfr::game_graph::INVALID_NODE;
        uint32_t strategy_context_id = 0;
        std::string kind;                    // "player", "chance", "terminal"
        uint32_t public_state_id = 0;
        uint32_t parent_node_id = cfr::game_graph::INVALID_NODE;
        uint8_t acting_seat = 0;
        bool terminal = false;
        
        std::vector<action_table_entry> actions;
        std::vector<action_frequency_detail> range_action_frequencies; // Derived range-aggregate action distribution
        std::vector<seat_value_entry> seat_values;
        double pot = 0.0;

        // Canonical Combo Details (Serialized based on export_mode)
        std::vector<combo_detail> combo_details;

        // Derived Presentations (Optional Export Projection)
        std::optional<category_summaries_projection> derived_category_summaries;
    };

    struct solved_artifact {
        artifact_metadata metadata{};
        solve_metadata solve{};
        std::vector<public_state_entry> public_states;
        std::vector<solved_node> nodes;
    };
}
```

#### 3.3.2 Schema v4 JSON Format
```json
{
  "metadata": {
    "schema_version": 4,
    "extraction_version": 1,
    "export_mode": "standard",
    "evaluation": "exact",
    "abstraction": "exact",
    "split_pot_policy": "fractional_tie"
  },
  "solve": {
    "iterations": 25000,
    "mode": "normal",
    "termination": "exploitability_target",
    "convergence": "target_met",
    "exploitability_mbb": 0.09,
    "target_exploitability_mbb": 0.10,
    "averaging_mode": "linear"
  },
  "public_states": [
    {
      "public_state_id": 1,
      "street": "flop",
      "board": ["Ah", "Kd", "2s"]
    }
  ],
  "nodes": [
    {
      "node_id": 4,
      "strategy_context_id": 12,
      "kind": "player",
      "public_state_id": 1,
      "parent_node_id": 0,
      "acting_seat": 0,
      "terminal": false,
      "actions": [
        {"action_index": 0, "action": "check", "child_node_id": 5},
        {"action_index": 1, "action": "bet 75%", "child_node_id": 6}
      ],
      "range_action_frequencies": [
        {"action_index": 0, "frequency": 0.425},
        {"action_index": 1, "frequency": 0.575}
      ],
      "seat_values": [
        {
          "seat": 0,
          "range_reach_mass": 0.850,
          "reach_weighted_value": 46.11,
          "conditional_range_ev": 54.25,
          "counterfactual_value": 128.30
        },
        {
          "seat": 1,
          "range_reach_mass": 1.000,
          "reach_weighted_value": 45.75,
          "conditional_range_ev": 45.75,
          "counterfactual_value": 96.70
        }
      ],
      "combo_details": [
        {
          "combination_index": 45,
          "hand": "AsQh",
          "hand_class": "AQo",
          "range_weight": 1.0,
          "reach_probability": 1.0,
          "ev": 68.45,
          "showdown_equity": 0.812,
          "category": {
            "made_tier": "pair",
            "pair_source": "hole_board_pair",
            "pair_pos": "top_pair",
            "kicker": "strong",
            "draws": ["backdoor_straight"],
            "blockers": ["nut_flush_blocker"]
          },
          "strategy": [
            {"action_index": 0, "frequency": 0.10, "cumulative_regret": -6.45},
            {"action_index": 1, "frequency": 0.90, "cumulative_regret": 0.71}
          ],
          "action_values": [
            {"action_index": 0, "q_profile": 62.0, "profile_advantage": -6.45},
            {"action_index": 1, "q_profile": 69.16, "profile_advantage": 0.71}
          ]
        }
      ],
      "derived": {
        "category_summaries": {
          "derivation_version": 1,
          "items": [
            {
              "category_name": "top_pair",
              "frequency": 0.284,
              "range_weight": 14.2,
              "average_ev": 72.10,
              "average_showdown_equity": 0.785,
              "action_frequencies": [
                {"action_index": 0, "frequency": 0.150},
                {"action_index": 1, "frequency": 0.850}
              ]
            }
          ]
        }
      }
    }
  ]
}
```

---

## 4. Implementation Roadmap & Staged Ordering

To streamline implementation and establish clear architectural boundaries, the mathematical contract, in-memory Result Store skeleton, memory budgets, and DTO projection contracts are frozen before extraction integration. The standalone reference evaluator (Phase 0.5) operates strictly on semantic inputs and serves as a permanent correctness oracle sharing zero indexing or traversal code with production. Legacy Schema v3 migration tooling is postponed until v4 semantics and producers are stable.

```
┌─────────────────────────────────────────────────────────────────┐
│ Phase 0: Freeze Mathematical Contract & Conditioning            │
│ ├── 0.1 Infoset & Range Strategy Context Semantics              │
│ ├── 0.2 Reach & Conditioning Precision Semantics (Double)       │
│ └── 0.3 Local Intervention Q / V / A & CFV Semantics            │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│ Phase 0.5: Standalone Reference Evaluator Oracle                │
│ - Pure semantic input evaluation (board, pot, ranges, profile)  │
│ - Deliberately simple, unoptimized correctness oracle:          │
│   recursive tree traversal, plain maps, combinatorial enum      │
│ - Completely isolated: shares zero indexing/CSR logic with prod │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│ Phase 1: Canonical Result Store Model & Memory Footprint Budget │
│ - Contiguous flat buffer layout & struct definitions            │
│ - node_record, strategy_surface_record, seat_value, views       │
│ - Strategy storage aliasing & combo offset sharing invariants   │
│ - O(1) direct indexing & per-node memory budget verification    │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│ Phase 1.1: Serialization DTO & Schema v4 Projection Contract    │
│ - Define deterministic export modes: summary, standard, full    │
│ - Freeze JSON Schema v4 DTOs, hoisted public_states, and derived│
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│ Phase 1.5: Core Intrinsic Categorizer Freeze & Implementation   │
│ - 10-window straight engine with backdoor sequence validation   │
│ - Paired-board pair_source & pair_position truth table          │
│ - Deterministic Zeta intrinsic kicker classification            │
│ - Enforce sizeof(hand_category_classification) == 8 bytes       │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│ Phase 1.6: Intrinsic Card Removal & Blocker Engine              │
│ - Isolated board-flush card removal rules (nut/2nd nut blocker) │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│ Phase 2: River HU Exact Extraction                              │
│ - Exact river showdown pot-share equity calculation             │
│ - Backward induction for Q_profile, V_profile, A_profile        │
│ - Extraction pass populating canonical Result Store surfaces    │
│ - Hot path direct buffer writes without allocating node_views   │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│ Phase 2.5: Differential Validation & Benchmarks                 │
│ - Differential testing: Production Result Store vs Oracle       │
│ - O(1) query lookups without linear scans or allocations        │
│ - Comprehensive Golden 1–9 verification suite                   │
│ - Bitwise determinism for fixed build/platform/FP configuration │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│ Phase 3: Full Serializer Implementation & Round-Trip Tests      │
│ - Implement serializer/deserializer for summary, standard, full │
│ - Hoisted public_states lookup and versioned derived projections│
│ - Artifact round-trip verification fixture (Golden 7)           │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│ Phase 4: Turn / Flop Exact HU Multi-Street Extraction           │
│ - Deterministic board-runout equity summation                   │
│ - Chance node and multi-street average profile value extraction │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│ Phase 5: Multiway Equity & Category Aggregation                 │
│ - N-way exact equity evaluator with fractional tie handling     │
│ - On-demand range-weighted category summaries in Result Store   │
│ - Range-interaction classification (range-relative blockers)   │
└────────────────┬────────────────────────────────────────────────┘
                 │
                 ▼
┌─────────────────────────────────────────────────────────────────┐
│ Phase 6: UI Solution Store & Strategy Explorer Integration      │
│ - solution_store / strategy_view_model / strategy_explorer      │
│ - Category filter matrix, EV heatmaps, abstraction degradation  │
└─────────────────────────────────────────────────────────────────┘
```

### Detailed Phase Work Breakdown

#### Phase 0: Freeze Mathematical Contract
- **Deliverables**:
  1. Define formal mathematical definitions for $Q_{\text{profile}}(n, h, a)$, $V_{\text{profile}}(n, h)$, $A_{\text{profile}}(n, h, a)$, $R_T^+(I, h, a)$, $\pi_i(n \mid h)$, $\Pi_{-i}(n \mid h)$, $\pi_c(n)$, $w_0(h)$, derived $\text{range\_reach\_weight}$, $\text{joint\_reach\_mass}$, $\text{range\_reach\_mass}$ ($W_i(n)$), $\text{reach\_weighted\_ev}$, $\text{conditional\_range\_ev}$, and $CFV_i(n)$ (`counterfactual_value`).
  2. Define multi-dimensional quality enums: `solve_mode`, `termination_reason`, `convergence_status` (`target_met`, `iteration_limited`, `not_evaluated`), `evaluation_method`, and `abstraction_mode`.
  3. Formalize the Strategy-Weighted Advantage Identity ($\sum_a \bar{\sigma} A = 0$) and tolerance bounds (`abs_error <= 1e-5`).
  4. **Phase 0.1**: Formally resolve extensive-form information sets $I(\mathcal{S}, h)$ vs range decision contexts $\mathcal{S}$ (`strategy_context_id`).
  5. **Phase 0.2**: Formalize reach conditioning precision (promote to `double` prior to multiplication).
  6. **Phase 0.3**: Formalize local one-step action interventions for $Q_{\text{profile}}$ and exhaustive conditioning for $CFV$.

#### Phase 0.5: Standalone Reference Evaluator Oracle
- **Deliverables**:
  1. Build a lightweight, deliberately simple reference evaluator oracle in test harness (`test_reference_evaluator.cpp`).
  2. Oracle operates strictly on semantic inputs (`board`, `pot`, `contributions`, `ranges`, `betting_tree`, `strategy_profile`), avoiding production intermediate graph structures.
  3. Implement unoptimized recursive tree traversals using plain `std::vector`/`std::map` and double precision for $Q, V, A$, reach propagation, river equity, and classification.
  4. Require that the oracle shares zero code, zero CSR offset calculations, and zero traversal optimizations with production.
  5. Use the oracle to establish ground truth assertions against the high-performance production extraction engine (differential testing).

#### Phase 1: Canonical Result Store Model & Memory Footprint Budget
- **Files**: `zeta/holdem/src/cfr/extraction/result_store.h`.
- **Deliverables**:
  1. Define flat memory layout structs: `node_record`, `strategy_surface_record`, `seat_value`, `combo_reach_entry`, `combo_value_entry`, `action_value_entry`.
  2. Implement non-owning views (`strategy_view`, `value_view`, `equity_view`, `category_view`, `node_view`) with guaranteed $O(1)$ direct indexing.
  3. Enforce the Combo Domain Offset Sharing Invariant, Dense Zero-Based ID Invariant, and Strategy Storage Aliasing Invariant.
  4. Verify memory consumption bounds against the defined footprint budget ($74.88 \text{ KB}$ per 2-action node, including fixed record headers).

#### Phase 1.1: Serialization DTO & Schema v4 Projection Contract
- **Files**: `zeta/holdem/src/cli/solve_cli.h`.
- **Deliverables**:
  1. Freeze Schema v4 DTOs, deterministic export modes (`summary`, `standard`, `full`), `schema_version: 4`, `extraction_version: 1`.
  2. Hoist `public_states` to deduplicate board cards across nodes.
  3. Decouple top-level node `range_action_frequencies` (derived aggregate) from combo-level canonical strategy projections (`combination_index`).

#### Phase 1.5: Core Intrinsic Categorizer Freeze & Implementation
- **Files**: `zeta/holdem/src/eval/categorizer.h`, `zeta/holdem/src/eval/categorizer.cpp`, `zeta/test/src/test_hand_categorizer.cpp`.
- **Deliverables**:
  1. Implement complete 10-window enumeration straight engine (`straight_draw_info`) across $W_0 \dots W_9$ with 2-card backdoor straight validation (orthogonal to immediate draws).
  2. Implement `pair_source` and board-geometry-aware `pair_position` handling paired/multi-paired boards with explicit truth table fixtures.
  3. Implement deterministic live-rank-based `kicker_quality` (Zeta intrinsic kicker classification).
  4. Enforce `static_assert(sizeof(hand_category_classification) == 8)`.

#### Phase 1.6: Intrinsic Card Removal & Blocker Engine
- **Deliverables**:
  1. Implement algorithmic board-flush blocker rules (`nut_flush_blocker`, `second_nut_blocker`) conditioned on board texture and street.
  2. Keep blocker evaluation cleanly isolated from core made-hand and straight-draw categorization.

#### Phase 2: River HU Exact Extraction
- **Files**: `zeta/holdem/src/cfr/extraction/strategy_surface.h`, `zeta/holdem/src/cfr/extraction/equity_surface.h`, `zeta/holdem/src/cfr/extraction/ev_surface.h`.
- **Deliverables**:
  1. Average strategy extraction pass over converged CFR tables exposing explicit action dimensions.
  2. River pot-share showdown equity calculation.
  3. Profile action EV ($Q_{\text{profile}}$), combo value ($V_{\text{profile}}$), and profile advantage ($A_{\text{profile}} = Q - V$) extraction.
  4. Populate canonical Result Store surfaces, with extraction writing directly into pre-allocated contiguous flat spans.
- **Implementation notes**:
  - `strategy_surface.h` normalizes non-negative average-strategy sums per infoset and expands them into the canonical `(strategy_context_id, combo_local_index, action_index)` flat surface. Empty/zero strategy sums resolve to the mathematically neutral uniform strategy for that infoset.
  - `equity_surface.h` computes exact river heads-up pot-share equity against the opponent's reaching range, removing overlapping private-card matchups and assigning fractional tie share.
  - `ev_surface.h` backs up the frozen average profile through river heads-up graphs, computes local one-step intervention action values, derives combo values and profile advantages, preserves zero-reach legal river combos, aliases shared-infoset strategy surfaces, and writes Result Store vectors directly after pre-sizing their contiguous spans.

#### Phase 2.5: Differential Validation & Benchmarks
- **Deliverables**:
  1. Run differential validation suite comparing production Result Store output with Phase 0.5 reference evaluator across comprehensive hand/action combinations.
  2. Validate query ergonomics (`node -> combo -> action`, `node -> category`, `context -> strategy`) with guaranteed $O(1)$ lookup time and zero heap allocations.
  3. Verify zero-reach combo preservation (Golden 4), shared-infoset aliasing (Golden 5), private-card separation (Golden 8), strategy storage aliasing (Golden 9), and bitwise extraction determinism (Golden 6).
  4. Benchmark extraction throughput and assert memory footprint under tight bounds.
- **Implementation status**: Completed in `zeta/test/src/test_extraction_contract.cpp` with differential oracle parity checks, deterministic byte-level replay checks, zero-reach preservation assertions, shared-strategy aliasing assertions, and extraction throughput/memory-budget benchmark coverage.

#### Phase 3: Full Serializer Implementation & Round-Trip Tests
- **Files**: `zeta/holdem/src/cli/solve_cli.h`, `zeta/holdem/src/cli/solve_cli.cpp`, `zeta/test/src/test_holdem_cli.cpp`.
- **Deliverables**:
  1. Implement three deterministic export modes: `summary`, `standard`, and `full`.
  2. Serialize canonical action tables, normalized `seat_values`, and deduplicated `public_states`.
  3. Format derived category summaries under `"derived": { "category_summaries": { "derivation_version": 1, "items": [...] } }`.
  4. Build artifact round-trip test fixture (Golden 7) verifying that deserialized artifacts perfectly reproduce Result Store surfaces.

#### Phase 4: Turn / Flop Exact HU Multi-Street Extraction
- **Deliverables**:
  1. Deterministic runout summation ($\binom{47}{2} = 1081$ for flop, $46$ for turn) for exact HU showdown pot-share equity.
  2. Chance-node extraction contracts and multi-street average profile backward induction for $Q_{\text{profile}}(n, h, a)$, $V_{\text{profile}}(n, h)$, and $A_{\text{profile}}(n, h, a)$.

#### Phase 5: Multiway Equity & Category Aggregation
- **Files**: `zeta/holdem/src/cfr/extraction/category_surface.h / .cpp`.
- **Deliverables**:
  1. Multiway equity evaluation over joint reaching opponent distributions with fractional pot-share tie handling ($\sum_i E_i = 1$).
  2. On-demand range-weighted category aggregations computed in Result Store.
  3. Implement `range_interaction_classification` for opponent-range-dependent blockers.

#### Phase 6: UI Integration
- **Files**: `zeta/ui/holdem/src/solver/solution_store.h / .cpp`, `zeta/ui/holdem/src/viewmodels/strategy_view_model.h / .cpp`, `zeta/ui/holdem/src/widgets/strategy_explorer.h / .cpp`.
- **Deliverables**:
  1. Update `solution_store` to consume Result Store non-owning views directly.
  2. Category-filtered matrix views and action EV inspectors.
  3. Graceful UI degradation for non-exact or abstracted solves.

---

## 5. Testing & Verification Strategy

### 5.1 Golden Test Fixtures

#### Golden 1: River Polarized HU Spot (Per-Combo & Matrix Equity Verification)
- **Board**: `Ah Kd Qc Jh 2s`
- **OOP Range**: `AA, AKs, AQo`
- **IP Range**: `AA, KK, QQ, AKo`
- **Verification Targets**:
  - **Per-combo equity matrix**: Verify exact combo $\times$ opponent combo showdown pot share matrix under fractional tie rules.
  - **Weighted aggregate equity**: Verify reach-weighted aggregate equity matches analytical reference.
  - **Hand classification**: `AA` $\implies$ `trips`, `AKs` $\implies$ `two_pair`, `AQo` $\implies$ `pair (top_pair)`.

#### Golden 2: Flop Comprehensive Feature Spot
- **Board**: `Ks Jh 8s`
- **Test Hand Matrix** (asserted against window-enumeration algorithm and frozen Phase 1.5 truth table):
  - `As Qs`: Spades `Ks 8s As Qs` (4 spades) + ranks `8, J, Q, K, A`. Window `10-J-Q-K-A` has 1 immediate completion rank ($10 \implies$ gutshot). Window `8-9-10-J-Q` requires 2 future unblocked cards ($9, 10 \implies$ backdoor straight).
    $\implies$ `high_card`, `draws: flush_draw | gutshot | backdoor_straight`, `blockers: nut_flush_blocker`.
  - `Qs Ts`: Ranks `8, 10, J, Q, K`. Completing ranks: $9$ (making $8-9-10-J-Q$) and $A$ (making $10-J-Q-K-A$). 2 boundary completion ranks $\implies$ `open_ended_straight`.
    $\implies$ `high_card`, `draws: flush_draw | open_ended_straight`.
  - `9s 8d`: Bottom pair + 1 completion rank ($10$).
    $\implies$ `pair (bottom_pair)`, `draws: gutshot`.
  - `8h 8c`:
    $\implies$ `trips`, `pair_source: none`, `pair_pos: none`.
  - `As Ah`:
    $\implies$ `pair (overpair)`, `pair_source: hole_pair`, `blockers: nut_flush_blocker`.
  - `7s 6s`:
    $\implies$ `high_card`, `draws: flush_draw | gutshot`.
  - `2d 2c`:
    $\implies$ `pair (pocket_pair_below_board)`, `pair_source: hole_pair`.

#### Golden 3: Mixed Strategy Q / V / A & Payoff Identity Fixture
- **Tree**: River decision tree testing non-trivial **mixed strategy** distributions (e.g. $\sigma(\text{check}) = 0.4$, $\sigma(\text{bet}) = 0.6$).
- **Assertions**:
  - **Profile Combo Value Equation**: $V(h) = 0.4 \cdot Q(h, \text{check}) + 0.6 \cdot Q(h, \text{bet})$.
  - **Profile Deviation Advantages**: $A(h, \text{check}) = Q(h, \text{check}) - V(h)$ and $A(h, \text{bet}) = Q(h, \text{bet}) - V(h)$.
  - **Strategy-Weighted Advantage Identity**: $0.4 \cdot A(h, \text{check}) + 0.6 \cdot A(h, \text{bet}) = 0.0$.
  - **HU Zero-Sum Payoff Check**: $V_1(n) + V_2(n) = 0.0$ (under standard chip-conservation conventions).

#### Golden 4: Zero-Reach Combo Preservation Fixture
- **Setup**: Solve a node where a legal combo has positive initial range weight $w_0(h) > 0$ but reaches with zero probability ($\pi_i(n \mid h) = 0$).
- **Assertions**:
  - `classification(combo)` exists and is valid.
  - `range_reach_weight(combo) == 0.0`.
  - Combo contributes exactly $0.0$ to `range_reach_mass`, `reach_weighted_ev`, and category summary frequencies $F(C)$.
  - `result_store::context_strategy(strategy_context_id)` returns valid strategy frequencies $\sum_a \bar{\sigma} = 1.0$.
  - `result_store::node(node_id)` provides valid $Q(h, a)$, $V(h)$, $A(h, a)$, and showdown equity.

#### Golden 5: Shared-Infoset / Hidden State Aliasing Fixture
- **Setup**: Construct two distinct concrete game tree nodes $n_A$ and $n_B$ that share the same public board, betting sequence, and acting seat, with identical hero private cards but distinct hidden opponent cards / reaching paths:
  ```
  node A: hero cards = As Qs, villain cards = Kh Kd (path reach A, opponent reach A) ──┐
                                                                                       ├── infoset X (shared observation, identical strategy)
  node B: hero cards = As Qs, villain cards = Kc Ks (path reach B, opponent reach B) ──┘
  ```
- **Assertions**:
  - **Infoset Observation & Strategy Equality**:
    $$\text{infoset}(n_A) == \text{infoset}(n_B)$$
    $$\text{strategy}(n_A, \text{AsQs}) == \text{strategy}(n_B, \text{AsQs})$$
  - **Concrete Node Evaluation Independence**:
    $$\pi_i(n_A) \ne \pi_i(n_B)$$
    $$\text{node}(n_A).\text{values}().\text{combo\_ev} \ne \text{node}(n_B).\text{values}().\text{combo\_ev} \quad [\text{when opponent distributions differ}]$$
    $$\text{node}(n_A).\text{equity}() \ne \text{node}(n_B).\text{equity}() \quad [\text{when opponent reach profiles differ}]$$
    $$\text{node}(n_A).\text{values}().\text{conditional\_range\_ev}() \ne \text{node}(n_B).\text{values}().\text{conditional\_range\_ev}()$$
  *(Invariant: Same infoset does NOT imply same node evaluation).*

#### Golden 6: Extraction Determinism Fixture (Bitwise Reproducibility)
- **Setup**: Execute extraction passes across varying thread worker counts (1, 2, 4, 8) and different tree traversal partitions on identical converged CFR states for a fixed build/platform/FPU configuration.
- **Assertions**:
  - Bitwise identical byte layout and exact floating-point equality across all generated Result Store surfaces.

#### Golden 7: Artifact Round-Trip Preservation Fixture
- **Setup**: Extract Result Store $\to$ Serialize to Schema v4 JSON $\to$ Deserialize back to solved artifact.
- **Assertions**:
  - Strategies, action values ($Q, A$), combo values ($V$), reach parameters, showdown equities, and category classifications match the source Result Store within float precision bounds.

#### Golden 8: Private-Card Infoset Separation Fixture
- **Setup**: Construct concrete game nodes with identical public state and betting sequence but different private holdings:
  - Node A: hero cards = `As Qs`, villain cards = `Kh Kd`
  - Node B: hero cards = `Ah Qh`, villain cards = `Kh Kd`
  - Node C: hero cards = `As Qs`, villain cards = `8d 8c`
- **Assertions**:
  - **Different Hero Holdings $\implies$ Distinct Infosets**:
    $$\text{infoset\_id}(n_A) \ne \text{infoset\_id}(n_B)$$
  - **Same Hero Holding + Different Villain Cards $\implies$ Identical Infoset**:
    $$\text{infoset\_id}(n_A) == \text{infoset\_id}(n_C)$$

#### Golden 9: Strategy Storage Aliasing Fixture
- **Setup**: Two concrete game tree nodes $n_A$ and $n_B$ sharing the same range decision context (`strategy_context_id` 17):
  ```
  node A ─┐
          ├── strategy_context 17 ── shared strategy surface entries
  node B ─┘
  ```
- **Assertions**:
  - **Shared Strategy Storage (Zero Duplication)**:
    $$\text{node}(n_A).\text{strategy}().\text{entries}().\text{data}() == \text{node}(n_B).\text{strategy}().\text{entries}().\text{data}()$$
  - **Independent Evaluated Node Storage**:
    $$\text{node}(n_A).\text{values}() \text{ memory buffer} \ne \text{node}(n_B).\text{values}() \text{ memory buffer}$$

### 5.2 Mathematical Invariant Checks
Across every extracted node, test suites assert:
1. **Strategy Normalization**: $\sum_{a \in A(\mathcal{S})} \bar{\sigma}(a \mid I, h) = 1.0 \quad (\forall h \in H_{\text{legal}})$.
2. **Strategy Value Equation**: $V(n, h) = \sum_{a \in A(\mathcal{S})} \bar{\sigma}(a \mid I, h) Q(n, h, a)$.
3. **Deviation Advantage Equation**: $A(n, h, a) = Q(n, h, a) - V(n, h)$.
4. **Strategy-Weighted Advantage Identity**: $\sum_{a \in A(\mathcal{S})} \bar{\sigma}(a \mid I, h) A(n, h, a) = 0.0$.
5. **Conditional Range EV**: $\text{conditional\_range\_ev}(n) = \frac{\sum_{h \in H_i} \text{range\_reach\_weight}(n, h) V(n, h)}{\text{range\_reach\_mass}(n)} = \frac{\text{reach\_weighted\_ev}}{\text{range\_reach\_mass}}$.
6. **Pot-Share Equity Conservation**: $\sum_{i=1}^N E_i = 1.0$ (in HU: $E_1 + E_2 = 1.0$ at identical joint distribution $\pi_{\text{joint}}$).
7. **Reach Conservation**: $\sum_{a \in A(\mathcal{S})} \pi_i(\text{child}(a) \mid h) = \pi_i(n \mid h) \cdot \bar{\sigma}(a \mid I, h)$.
8. **Category Conservation**: $\sum_{C \in \text{Partition}} F(C) = 1.0$, and $\sum_{C \in \text{Partition}} F(C) \cdot \overline{\text{EV}}(C) = \text{conditional\_range\_ev}$.
9. **Tolerances**: Float accumulations checked with `abs_error <= 1e-5`. Reject `NaN`, `+inf`, `-inf`. Allow valid negative strategic EV.

---

## 6. Risk Assessment & Mitigations

| Risk | Impact | Mitigation Strategy |
| :--- | :--- | :--- |
| **Artifact Size Explosion**: 1326 combos $\times$ thousands of nodes yields 100s of MBs of JSON. | High | 1. **Canonical In-Memory Result Store**: Primary binary contiguous model; JSON as export projection.<br>2. **Dense Flat Matrices**: Omit combo/action indices from entry structs.<br>3. **Deterministic Export Modes**: Three explicit modes (`summary`, `standard`, `full`).<br>4. **Deduplicate Board Strings**: Lookup boards via `public_state_id`.<br>5. **Omit Combo Strings in Store**: `hand` and `hand_class` generated only during JSON serialization. |
| **Flop/Turn Exact Equity Computation Cost**: Complete runout evaluation is CPU-intensive. | Medium | 1. Dedicated post-solve extraction pass decoupled from CFR loops.<br>2. Parallel deterministic evaluation reusing solver worker infrastructure.<br>3. Vectorized rank evaluations and shared runout lookup tables. |
| **Abstraction / Isomorphism Semantic Drift**: Abstracted infosets mapping to ambiguous concrete combos. | High | 1. Multi-dimensional status metadata: `solve_mode`, `termination_reason`, `convergence_status`, `evaluation_method`, and `abstraction_mode`.<br>2. UI explicitly flags non-exact surfaces.<br>3. Contract clarifies: exact extraction on abstracted game evaluates the abstracted model exactly, not the unrestricted game. |
| **Multiway Exact Equity Combinatorics**: Joint enumeration over $N \ge 3$ active players explodes. | High | 1. **Contract-First Formulation**: Exact N-way showdown equity over joint reaching distribution with fractional tie handling ($\sum_i E_i = 1$).<br>2. Implementation encapsulates evaluation behind `equity_surface` interface without prematurely committing to naive Cartesian enumeration. |
| **Multi-Threaded Extraction Non-Determinism**: Asynchronous reductions producing float jitter across worker counts. | Medium | 1. Implement worker-local accumulation buffers.<br>2. Enforce deterministic tree reduction order across threads to ensure bitwise reproducibility on fixed build/platform targets (Golden 6). |

---

## 7. Delivery Checklist

- [x] Mathematical contract formalization (Phase 0: Q, V, A canonical chain, $\sum \bar{\sigma} A = 0$, conditioning table, seat values, counterfactual values, tolerances).
- [x] Phase 0.1 extensive-form information set $I(\mathcal{S}, h)$ and range strategy context $\mathcal{S}$ (`strategy_context_id`) formalization.
- [x] Phase 0.2 reach and conditioning precision contracts (derived `range_reach_weight` in `double`).
- [x] Phase 0.3 local one-step intervention $Q_{\text{profile}}$ and counterfactual value $CFV$ conditioning contracts.
- [ ] Phase 0.5 standalone reference evaluator oracle (`test_reference_evaluator.cpp`) using recursive traversal and plain enumeration on semantic inputs without sharing production logic.
- [x] Phase 1 Result Store skeleton with contiguous offset-indexed memory layout, `node_record`, `strategy_surface_record`, `seat_value`, `node_view` traversal, strategy storage aliasing, and memory footprint budgets ($74.88\text{ KB}$ per 2-action node including fixed record headers).
- [x] Phase 1.1 Schema v4 DTO definitions and deterministic projection contract (`summary`, `standard`, `full` modes, canonical `"combination_index"`).
- [x] Phase 1.5 `eval/categorizer.h` with 10-window straight engine ($W_0 \dots W_9$, `straight_draw_info`), paired-board `pair_source` & `pair_position` truth table, deterministic live-rank `kicker_quality` (Zeta intrinsic kicker classification), and `static_assert(sizeof(hand_category_classification) == 8)`.
- [x] Phase 1.6 isolated algorithmic board-flush blocker rules (`nut_flush_blocker`, `second_nut_blocker`).
- [x] Phase 2 exact river HU extraction pass with average-profile value extraction ($Q_{\text{profile}}, V_{\text{profile}}, A_{\text{profile}}$) and direct pre-sized flat buffer writing.
- [x] Phase 2.5 differential validation (Production Result Store vs Oracle), bitwise determinism (Golden 6), and performance benchmarks.
- [ ] Phase 3 full Schema v4 serializer/deserializer with export modes, explicit `seat_values`, deduplicated `public_states`, and versioned derived category summaries.
- [ ] Phase 4 flop/turn exact HU runout evaluators and chance-node extraction contracts.
- [ ] Phase 5 multiway exact equity evaluator with joint pot-share distribution conservation ($\sum_i E_i = 1$) and range interaction classification.
- [ ] Phase 6 UI `solution_store` and `strategy_explorer` integration with category matrices and action EV inspection.
- [ ] Comprehensive Golden test suite covering Golden 1–9 fixtures (per-combo equity matrices, mixed strategy identities, zero-reach preservation, shared-infoset aliasing, private-card separation, strategy storage aliasing, bitwise determinism, and artifact round-trip).
