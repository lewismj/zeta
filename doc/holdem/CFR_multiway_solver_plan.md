# CFR Multiway Solver Plan

## Objective

Build the multiway CFR solver by extending the existing Zeta Hold'em CFR infrastructure, not by adding unrelated standalone-game scaffolding or alternate mini-solvers.

The plan is deliberately Hold'em-first. Do not implement unrelated standalone games as intermediate products. They would introduce a second model of graph generation, information sets, terminal semantics, validation, and convergence that does not exercise the actual production code paths. Validation should use small generated Zeta Hold'em subgames and hand-audited synthetic graphs that are built with the same `game_graph`, infoset, table, scheduler, terminal, and reduction APIs used by the real solver.

The solver should preserve these existing surfaces:

- `game_graph` / graph builder / graph validation.
- Infoset-major `action_table_layout`.
- Contiguous `regret_table` and `strategy_sum_table`.
- Worker-local `table_delta_buffer`.
- Deterministic reduction framework.
- DFS partitioner and board/partition scheduler.
- River terminal cache, reach index, workspace, and terminal engine APIs.
- Checkpoint/table layout direction.
- Validation and benchmark style.

The specialization should happen in the CFR engine and terminal kernels:

```text
                 CFR engine
                     |
          +----------+----------+
          |                     |
      HU kernel             N-way kernel
       N=2                 N>=3
```

The graph, infoset storage, regret tables, strategy tables, checkpoint format, scheduler, reduction framework, and validation framework should remain shared.

---

## Current codebase facts that constrain the plan

### Graph and metadata

`zeta/holdem/src/cfr/graph/graph.h` already provides an immutable DFS post-order CSR tree:

- `row_offsets[node]..row_offsets[node + 1]` address outgoing edges.
- `edges` stores `child_node` and `action_index`.
- `node_types` distinguishes `player_chance`, `player`, `chance`, and `terminal`.
- `infoset_id[node]` maps player-like nodes to dense infosets.
- `node_depth`, `subtree_size`, `node_count`, `terminal_count`, `infoset_count`, and `max_depth` are already available.

Important implications:

- Keep hot traversal offset-based.
- Do not add hash-map lookups to traversal.
- Add new metadata as dense side arrays parallel to `node_count`, not as per-node heap objects.
- Preserve DFS post-order and immutable graph reads.

Missing graph metadata for real CFR:

- Acting player per player node.
- Chance-event ID per chance node.
- Terminal leaf ID or terminal metadata ID per terminal node.
- Optional street/state class per node for validation and diagnostics.

Recommended extension shape:

```cpp
struct solver_graph_annotations {
    std::vector<uint8_t> actor_by_node;          // INVALID for non-player nodes.
    std::vector<uint32_t> chance_event_by_node;  // INVALID for non-chance nodes.
    std::vector<uint32_t> terminal_leaf_by_node; // INVALID for non-terminal nodes.
    std::vector<uint8_t> street_by_node;         // validation/diagnostics, not strategy lookup.
};

template <std::size_t N>
struct solver_graph_view {
    const game_graph& graph;
    std::span<const uint8_t> actor_by_node;
    std::span<const uint32_t> chance_event_by_node;
    std::span<const uint32_t> terminal_leaf_by_node;
    std::span<const uint8_t> street_by_node;
};
```

Do not replace `game_graph`, and do not make CFR annotations required by graph debugging, graph generation, visualization, or alternative solvers. Keep topology owned by `game_graph`; make CFR metadata side arrays/views layered on top.

### Table layout

`zeta/holdem/src/cfr/tables/table_layout.h` already has the correct table shape:

```cpp
action_offsets[infoset]
action_offsets[infoset + 1]
```

Both `regret_table` and `strategy_sum_table` currently use contiguous flat `std::vector<float>` storage with infoset-major offsets. This is the right logical foundation for large CFR tables, but the storage precision must remain a layout policy, not an incidental implementation detail.

Reserve a precision policy in table construction and checkpoints:

```cpp
enum class regret_precision : uint8_t {
    float32,
    float16,
    scaled_int16
};

enum class accumulation_precision : uint8_t {
    float32,
    float64
};

enum class reduction_order_policy : uint8_t {
    deterministic_worker_order,
    owner_sharded_deterministic
};
```

`float32` table storage is the first implementation target. The default reproducibility policy is `float32` storage, `float32` worker accumulation, and deterministic worker-order reduction. A higher-reproducibility policy may use `float32` storage, `float64` worker accumulation, ordered merge, and cast-on-write. The table layout and checkpoint hashes must include storage precision, accumulation precision, and reduction order so a checkpoint cannot be resumed into an incompatible numeric policy.

The memory monster is not graph nodes. It is:

```text
infosets x actions x tables
```

Example:

```text
10,000,000 infosets
8 actions average
80,000,000 regret entries
80,000,000 strategy entries

float regrets:       320 MB
float strategy sums: 320 MB
total:               640 MB
```

That is before checkpoints, worker deltas, scratch, graph metadata, chance outcome tables, terminal leaves, river caches, and build-time state.

Therefore memory estimation and table layout compatibility must be mandatory during context construction before chance generation or large graph generation.

### Worker-local deltas and reduction

`table_delta_buffer` already stores sparse worker-local regret and strategy deltas:

- `entries_` records touched infosets.
- `entry_by_infoset_` maps infoset to sparse entry.
- `regret_deltas_` and `strategy_deltas_` store only touched values.
- `apply_worker_reductions` merges workers deterministically in `worker_order`.

This is the right reduction foundation, but multi-thread scaling needs explicit infoset ownership:

```text
node -> infoset -> owner worker/table shard
```

Target model:

```text
worker 0 owns infosets [0, 100000)
worker 1 owns infosets [100000, 200000)
worker 2 owns infosets [200000, 300000)
...
```

Traversal may be scheduled by board and graph partition, but table ownership should be by infoset range. A worker can accumulate local deltas for any infoset, then reduction routes those deltas to the owner shard. This avoids false sharing and gives a path to NUMA-local table slices.

### Traversal

`zeta/holdem/src/cfr/traversal/traversal.h` currently has a heads-up-shaped frame:

```cpp
struct traversal_frame {
    uint32_t node_id;
    uint32_t next_edge_offset;
    float reach_oop;
    float reach_ip;
    float chance_weight;
    float accumulated_utility;
    traversal_phase phase;
};
```

Current limitations:

- Reach is hard-coded as OOP/IP.
- Player nodes multiply both reaches by the same action probability.
- Acting player is not represented.
- Average-strategy updates use `chance_weight * (reach_oop + reach_ip)`, which is a placeholder.
- Regret deltas are not computed from child action values.
- Chance nodes use uniform child probability.
- Utility is scalar, not per-player or per-updating-player with stored child values.

The existing allocation-free iterative traversal is valuable and should be retained. The frame shape and scratch layout need to change.

### Terminal APIs

The terminal layer already points in the desired direction:

- `terminal_context<N>` is templated.
- `terminal_values<N>` is templated.
- `terminal_workspace<N>` owns `std::array<river_reach_index, N>`.
- `terminal_engine<2>` has heads-up convenience overloads.
- `terminal_engine<N>` dispatches N-way fold and sampled N-way showdown values.
- Heads-up showdown and fold have specialized exact fast paths.

This means the solver should not flatten everything into a generic slow path. Keep the existing terminal split:

- `N == 2`: exact heads-up kernels.
- `N >= 3`: N-way kernels.

The CFR solver should mirror this split while sharing graph/table/scheduler/reduction storage.

The terminal layer should always return player-indexed values and should not know the CFR update mode:

```text
terminal_engine<N>
        |
        v
terminal_values<N>
        |
        v
utility vector [u0, u1, ..., uN-1]
```

The CFR engine chooses the perspective after terminal evaluation:

```text
leaf_value = terminal_values.utility[updating_player]
```

This keeps alternating CFR, future simultaneous updates, exploitability calculation, best response, and EV reporting on the same terminal API.

### Pot accounting

N-way Hold'em terminal semantics require side-pot structure from the graph-generation stage onward, not as a terminal afterthought:

```cpp
struct pot_layer {
    float amount;
    uint64_t eligible_mask;
    uint64_t contributors_mask;
};
```

The first implementation can emit exactly one main-pot layer, but terminal leaves and betting state should use the layered shape immediately so all-in and side-pot support does not require an API break. Keep `eligible_mask` and `contributors_mask` explicit: who can win a pot and who created/funded it are different auditing questions, especially in all-in cases.

---

## Non-negotiable design constraints

1. **No standalone-game churn**: validation fixtures must go through Zeta's actual graph, infoset, terminal, table, scheduler, and reduction APIs.
2. **Infosets before chance**: chance determines nodes, but infosets determine solver memory and table layout.
3. **No per-frame N-way reach arrays**: reach storage should be split from traversal control frames.
4. **No independent worker CFR+ clipping**: workers accumulate raw regret deltas; clipping happens once after merge.
5. **Heads-up must stay fast**: do not regress existing two-player terminal optimizations or force HU through avoidable N-way loops.
6. **No hot-loop allocation**: all traversal stack, reach stack, node values, edge values, edge probabilities, and deltas are preallocated.
7. **Typed errors over silent fallback**: continue using `std::expected`-style validation and explicit error kinds.
8. **Deterministic reduction remains default**: table updates must be replayable independent of worker count where practical.
9. **Concrete floating-point policy**: storage precision, accumulation precision, and reduction order are part of solver configuration and checkpoint compatibility.
10. **Quality instrumentation required**: speed and determinism are not enough; the solver must expose convergence and regret diagnostics early.

---

## Target solver architecture

### Shared context

Keep one shared high-level context shape:

```cpp
template <std::size_t N>
struct cfr_solver_context {
    const game_graph& graph;
    solver_graph_annotations graph_annotations;
    infoset_directory infosets;
    infoset_owner_map infoset_owners;
    action_table_layout layout;
    regret_table regrets;
    strategy_sum_table strategy_sums;
    terminal_leaf_table<N> terminals;
    chance_event_table chance_events;
    solver_parameters parameters;
};
```

The existing `cfr_context` can evolve into this. It should not become a separate solver for every experimental validation game. CFR code should construct `solver_graph_view<N>` from `game_graph` plus annotations when it enters traversal; other graph consumers should not need to carry CFR metadata.

### Engine specialization

Use a shared interface with specialized implementation:

```cpp
template <std::size_t N>
struct cfr_engine;

template <>
struct cfr_engine<2> {
    using reach_state = hu_reach_state;
    static constexpr bool heads_up = true;
};

template <std::size_t N>
requires (N >= 3)
struct cfr_engine<N> {
    using reach_state = nway_reach_state<N>;
    static constexpr bool heads_up = false;
};
```

The specialization belongs inside reach propagation, counterfactual reach calculation, terminal policy dispatch, and maybe traversal scratch. It should not duplicate graph construction, table layout, reduction, scheduler, validation, or checkpoint format.

---

## Phase 1: Information-set storage and ownership

Move information-set design before chance implementation.

### 1.1 Define infoset identity for Hold'em

An infoset key must be a build-time object that lowers to a dense integer ID. It should include enough Hold'em state to guarantee identical strategy choices:

- Acting player.
- Street.
- Private hand abstraction or exact combo class.
- Public board abstraction ID or exact board key.
- Chance history / runout class ID.
- Betting history abstraction.
- Stack/pot abstraction.
- Legal action set ID.
- Player count.
- Subgame/root context ID if solving a street-specific subgame.

The board and chance/runout fields must be explicit abstraction outputs, not assumptions hidden inside the key generator. For example, `As Kd 7c 2h` and `As Kd 7c 2s` may share rank texture while differing in blocker effects. The abstraction policy decides whether they merge by producing the same or different IDs.

Hot traversal should only see:

```cpp
uint32_t infoset_id = graph.topology.infoset_id[node_id];
```

Do not hash infoset keys in traversal.

### 1.2 Validate infoset/action compatibility

Existing `make_action_table_layout(const game_graph&)` already rejects inconsistent action counts for the same infoset. Extend validation so shared infosets also have:

- Same acting player.
- Same legal action IDs and order.
- Same street.
- Same abstraction class.
- Same table owner.

Validation error context should include:

- Node ID.
- Infoset ID.
- Related node ID if another node conflicts.
- Actor.
- Action count.

### 1.3 Keep infoset-major tables

Retain:

```cpp
action_offsets[infoset + 1]
regrets[action_offsets[infoset] + action]
strategy_sums[action_offsets[infoset] + action]
```

Add memory accounting at context construction:

```text
infoset_count
value_count
regret bytes
strategy_sum bytes
action_offsets bytes
owner map bytes
worker delta reserved bytes
checkpoint estimate
```

The plan should treat table memory as the primary scaling driver.

Memory accounting belongs before large graph generation commits. The generator should be able to produce or estimate:

```text
estimated_nodes
estimated_infosets
estimated_actions
estimated_regret_bytes
estimated_strategy_sum_bytes
estimated_delta_bytes
estimated_checkpoint_bytes
```

If the estimate exceeds configured memory limits, fail during planning/build lowering rather than after materializing a huge chance-expanded graph.

### 1.4 Add infoset ownership

Introduce:

```cpp
struct infoset_owner_range {
    uint32_t owner_worker;
    uint32_t begin_infoset;
    uint32_t end_infoset;
};

struct infoset_owner_map {
    std::vector<uint32_t> owner_by_infoset;
    std::vector<infoset_owner_range> ranges;
};
```

For an initial implementation, contiguous ranges are enough. Later NUMA work can align ranges to pages or huge pages.

Reduction model:

```text
traversal worker accumulates raw local deltas
    |
    v
deterministic reduction groups deltas by owner range
    |
    v
owner shard applies regret and strategy updates
```

Do not let multiple workers write directly into shared global regret/strategy arrays during traversal.

Traversal scheduling and table ownership are separate dimensions. A board task may run on worker 7 while most touched infosets belong to owner 0. Add diagnostics so pathological abstraction or scheduling choices are visible:

- `remote_delta_count`.
- `remote_delta_bytes`.
- `owner_hit_distribution`.
- `owner_remote_hit_distribution`.
- Per-owner reduction time and touched value count.

### 1.5 Table shard compatibility

The existing global `regret_table` and `strategy_sum_table` can remain the logical storage. Add optional views:

```cpp
struct table_shard_view {
    uint32_t owner_worker;
    uint32_t begin_infoset;
    uint32_t end_infoset;
    uint32_t begin_value;
    uint32_t end_value;
};
```

This allows:

- Owner-local reductions.
- NUMA-aware allocation later.
- Checkpoint chunking by shard.
- Avoiding false sharing between adjacent worker-owned ranges.

---

## Phase 2: CFR math and reduction correctness

This phase makes the current traversal a real CFR iteration.

### 2.1 Add solver and iteration parameters

Add an explicit parameter/config object:

```cpp
enum class cfr_variant : uint8_t {
    vanilla,
    cfr_plus,
    linear_cfr,
    discounted_cfr
};

enum class cfr_update_mode : uint8_t {
    alternating,
    simultaneous
};

struct iteration_config {
    cfr_variant variant;
    cfr_update_mode update_mode;
    uint64_t iteration;
    uint8_t updating_player; // valid for alternating mode.
    float strategy_weight;
};
```

No hidden global iteration counters.

The first production path is `cfr_update_mode::alternating`: one `updating_player` traversal at a time, scalar utility for that updating player's perspective, and regret writes only at that player's infosets. Simultaneous all-player utility-vector traversal is a separate future path and should not be mixed into the alternating formulas.

### 2.2 Add explicit strategy policy

Do not bake regret matching into traversal. Add a policy layer between regret storage and action probabilities:

```cpp
template <class Policy>
struct strategy_policy;

struct regret_matching_plus {
    static void compute_strategy(
        std::span<const float> regrets,
        std::span<float> strategy) noexcept;
};
```

Traversal asks the policy for `sigma[action]`; the policy owns details such as vanilla regret matching, CFR+, linear CFR weighting, DCFR discounting, and future regret pruning. Required behavior:

- All non-positive regrets produce a valid uniform strategy.
- All zero regrets produce a valid distribution.
- Positive regrets produce proportional probabilities over positive regret mass.

### 2.3 Add acting player metadata

Every `node_kind::player` and `node_kind::player_chance` node needs an actor:

```cpp
std::vector<uint8_t> actor_by_node;
```

Validation:

- Player-like nodes have `actor < N`.
- Non-player nodes use invalid actor.
- All nodes in the same infoset have the same actor.
- Actor is consistent with legal betting state.

### 2.4 Store action child values

Regret update requires:

```text
regret_delta[action] = cf_reach * (action_value - node_value)
```

Current traversal only accumulates scalar utility upward. Add scratch:

```cpp
std::vector<float> value_stack;       // depth-local value for current updating player
std::vector<float> edge_child_value;  // value returned by child edge
std::vector<float> edge_probability;  // already exists
```

When a child exits:

```text
edge_child_value[parent_edge_offset] = child_value
```

When the parent player node exits:

```text
value_stack[value_slot] = sum(strategy[action] * edge_child_value[action])
```

Terminal leaves still produce utility vectors, but this alternating traversal stores only the value for `updating_player` in depth-local value slots and `edge_child_value`.

### 2.5 Compute regret deltas in traversal

At a player node in alternating update mode:

```text
actor = actor_by_node[node]
infoset = infoset_id[node]
```

If `actor == updating_player`, compute regret deltas:

```text
cf_reach = chance_reach * product(player_reach[j] for j != updating_player)
delta[action] += cf_reach * (action_value[action] - value_stack[value_slot])
```

If `actor != updating_player`, only propagate through the node using that actor's strategy. Do not write regret deltas for that node in alternating update mode.

At a terminal node:

```text
terminal_values = terminal_engine<N>::evaluate(...)
value_stack[value_slot] = terminal_values.utility[updating_player]
```

The terminal layer returns the vector; the CFR engine selects the updating-player perspective.

### 2.6 Correct average-strategy updates

Average strategy is weighted by the acting player's own reach. In the first alternating path, execute this write only when `actor == updating_player`:

```text
strategy_delta[action] += strategy_weight * chance_reach * player_reach[actor] * sigma[action]
```

This replaces the current placeholder:

```text
chance_weight * (reach_oop + reach_ip)
```

Zero-reach infosets should not produce NaN or invalid probabilities. Regret matching still returns a valid strategy, but strategy-sum deltas should be zero when own reach is zero.

### 2.7 CFR+ clipping rule

Mandate this exact order:

```text
worker:
    accumulate raw regret deltas

merge:
    regret += delta

CFR+:
    regret = max(regret, 0)
```

Do not clip in each worker before merge.

Why:

```text
worker A delta = +10
worker B delta = -20

wrong independent clipping:
    clip(+10) + clip(-20) = 10 + 0 = 10

correct:
    clip(+10 - 20) = clip(-10) = 0
```

This affects convergence and must be covered by reduction tests.

### 2.8 Extend deterministic reduction

Current `apply_worker_reductions` applies each worker's `table_delta_buffer` in worker order. Extend with a policy:

```cpp
enum class reduction_mode : uint8_t {
    deterministic_worker_order,
    owner_sharded_deterministic
};
```

For owner-sharded reduction:

1. Iterate workers in deterministic order.
2. For each sparse entry, route by `owner_by_infoset[entry.infoset_id]`.
3. Apply raw regret deltas to that owner shard.
4. Apply strategy deltas.
5. After all raw regrets are merged, run CFR+ clipping over touched regret values or owner ranges.

Reduction diagnostics must record remote delta volume and owner hit distribution so the scheduler can be tuned independently from table shard ownership.

---

## Phase 3: Reach representation and traversal scratch

The planned multiway reach shape is correct:

```cpp
std::array<float, N> reach;
```

But do not store it inside every traversal frame.

### 3.1 Keep traversal frames small

Avoid:

```cpp
template <std::size_t N>
struct traversal_frame_nway {
    uint32_t node_id;
    uint32_t next_edge_offset;
    std::array<float, N> reach;
    float chance_weight;
    traversal_phase phase;
};
```

For `N = 6`, that frame is roughly:

```text
node_id             4 bytes
edge cursor         4 bytes
reach              24 bytes
chance/phase/pad    several bytes
------------------------------
~40 bytes per frame
```

Depth may be small now, but millions of traversals make cache behavior matter.

Use a compact frame:

```cpp
struct traversal_frame {
    uint32_t node_id;
    uint32_t edge_cursor;
    uint32_t reach_slot;
    uint32_t value_slot;
    traversal_phase phase;
};
```

Keep reach and value state in separate scratch:

```cpp
template <std::size_t N>
struct nway_reach_state {
    std::array<float, N> player;
    float chance;
};

std::vector<nway_reach_state<N>> reach_stack; // size max_depth + margin
std::vector<float> value_stack;               // size max_depth + margin
```

The frame points to depth-local reach and value slots. Traversal is DFS, so prefer reusable depth-stack slots over node-indexed vectors wherever possible. Maximum scratch should scale with `O(max_depth)` for reach/value state, not `O(nodes)`, unless a specific algorithm requires node-indexed storage.

### 3.2 Heads-up reach specialization

For `N == 2`, do not force generic loops where direct scalar code is trivial.

Possible shape:

```cpp
struct hu_reach {
    float oop;
    float ip;
    float chance;
};
```

or:

```cpp
struct hu_traversal_state {
    float player_reach[2];
    float chance_reach;
};
```

Counterfactual opponent reach becomes:

```cpp
float opponent_reach =
    updating_player == 0
        ? reach.ip
        : reach.oop;
```

No loop, no product, and no branch inside an action loop if the updating player is hoisted.

Average strategy own reach:

```cpp
float own_reach = actor == 0 ? reach.oop : reach.ip;
```

Child propagation:

```cpp
child = parent;
if (actor == 0) {
    child.oop *= probability;
} else {
    child.ip *= probability;
}
```

### 3.3 N-way reach helper

For `N >= 3`, centralize the operations:

```cpp
template <std::size_t N>
[[nodiscard]] float opponent_reach_product(
    const std::array<float, N>& reach,
    uint8_t updating_player) noexcept
{
    float product = 1.0f;
    for (std::size_t seat = 0; seat < N; ++seat) {
        if (seat != updating_player) {
            product *= reach[seat];
        }
    }
    return product;
}
```

Later optimize with prefix/suffix products only if benchmarks show the product loop matters.

### 3.4 Worker context evolution

Current `worker_context` has:

```cpp
std::vector<traversal_frame> stack;
std::vector<float> node_utility;
std::vector<float> edge_probability;
table_delta_buffer delta_buffer;
```

Evolve to:

```cpp
template <std::size_t N>
struct worker_context {
    worker_input_views<N> inputs;
    std::vector<traversal_frame> stack;
    std::vector<reach_state_for<N>> reach_stack;
    std::vector<float> value_stack;
    std::vector<float> edge_probability;
    std::vector<float> edge_child_value;
    table_delta_buffer delta_buffer;
    traversal_diagnostics diagnostics;
};
```

For `N == 2`, `reach_state_for<2>` resolves to `hu_reach`. For `N >= 3`, it resolves to `nway_reach_state<N>`.

---

## Phase 4: Heads-up and N-way solver kernels

### 4.1 Shared public API

Expose a single iteration entry point:

```cpp
template <std::size_t N>
std::expected<iteration_result, iteration_error> run_cfr_iteration(
    cfr_solver_context<N>& context,
    iteration_config config,
    std::span<worker_context<N>> workers);
```

Responsibilities:

- Validate graph/table/context compatibility.
- Select HU or N-way traversal kernel at compile time.
- Schedule board/partition tasks.
- Traverse for each updating player or configured player subset.
- Accumulate worker-local raw deltas.
- Reduce deterministically.
- Apply CFR+ clipping once after merge.
- Return diagnostics.

### 4.2 Heads-up kernel

The HU kernel should preserve existing optimization direction:

- Use direct OOP/IP reach scalars.
- Use `terminal_engine<2>`.
- Use existing exact heads-up showdown/fold kernels.
- Avoid N-way folded masks where `folded_mask<2>` is already specialized.
- Avoid `for seat in N` products for opponent reach.

HU CFR formulas:

```text
opponent_reach = updating_player == oop ? reach.ip : reach.oop
cf_reach = reach.chance * opponent_reach
own_reach = actor == oop ? reach.oop : reach.ip
```

### 4.3 N-way kernel

The N-way kernel should use:

- `std::array<float, N>` for player reach in separate reach scratch.
- `terminal_engine<N>` for terminal values.
- `folded_mask<N>` and `terminal_context<N>`.
- Product of all non-updating-player reaches for regret updates.

N-way CFR formulas:

```text
cf_reach = chance_reach * product(reach[j] for j != updating_player)
avg_weight = strategy_weight * chance_reach * reach[actor]
```

### 4.4 Terminal policy integration

Current `river_terminal_leaf_policy` is heads-up:

- `terminal_context<2>`
- `heads_up_player perspective`
- `combination_index combo`
- `reach_oop/reach_ip`

Generalize without deleting the HU fast path:

```cpp
template <std::size_t N>
struct river_terminal_leaf_nway {
    terminal_leaf_kind kind;
    terminal_context<N> context;
    folded_mask<N> folded;
};
```

HU specialization can keep:

- `heads_up_player folded_player`
- direct `evaluate_showdown_values(cache, oop_index, ip_index, context)`
- direct `evaluate_fold_values(cache, oop_index, ip_index, context, folded_player)`

N-way uses `terminal_engine<N>`.

---

## Phase 5: Exact Hold'em validation without standalone-game churn

Validation must exercise real Zeta solver components.

### 5.1 Replace standalone-game milestones

Do not add:

- Unrelated standalone game models.
- Separate recursive mini-solvers with their own graph, infoset, table, or terminal model.

Instead add:

- Hand-authored `game_graph` fixtures using `graph_builder`.
- Generated tiny Hold'em river subgames.
- Generated tiny betting states lowered through the same future graph-generation path.
- Direct terminal comparisons using `river_terminal_cache`, `river_reach_index`, and `terminal_engine<N>`.

### 5.2 Reference checks should compare Zeta code paths

Acceptable reference implementations:

- A slow recursive traversal over the same `game_graph`.
- A slow action-value computation using the same `regret_table` and `strategy_sum_table` layout.
- A direct terminal evaluator call compared against traversal terminal leaves.
- A single-thread reduction compared against multi-worker deterministic reduction.

Not acceptable:

- A separate standalone-game abstraction.
- A different infoset model.
- A separate table layout.
- A separate terminal payoff convention.

### 5.3 Required validation fixtures

Regret matching fixture:

- Negative regrets produce a uniform strategy.
- All zero regrets produce a valid distribution.
- Positive regrets produce proportional probabilities over positive regret mass.

Small graph fixture:

- 1 root player node.
- 2 actions.
- Terminal leaves with fixed utility.
- One infoset.
- Confirms strategy, child value, node value, regret delta, and strategy delta.

Two-level graph fixture:

- Alternating actors.
- Confirms only updating-player regret is written.
- Confirms non-updating actor reach affects counterfactual reach.

Infoset collision fixture:

- Two nodes with the same infoset ID but different actors must fail validation.
- Two nodes with the same infoset ID but different legal action IDs/order must fail validation.

Reach correctness fixture:

- Three-player node with reaches `p0 = 0.5`, `p1 = 0.25`, `p2 = 0.8`.
- For `updating_player = 0`, counterfactual reach is `0.25 * 0.8 = 0.2`, multiplied by chance reach.

Chance fixture:

- One chance node with explicit outcome probabilities.
- Confirms chance reach multiplies terminal values and regret deltas.

River terminal fixture:

- Uses `make_river_terminal_cache`.
- Uses `make_river_reach_index`.
- Traversal terminal leaf result matches direct `evaluate_showdown_values` or `evaluate_fold_values`.
- Terminal utility vector `[-2, +2, +0]` with `updating_player = 1` produces traversal leaf value `+2`.

Reduction fixture:

- Two workers produce `+10` and `-20` regret deltas for the same infoset/action.
- Merged CFR+ result is `0`, not `10`.

Ownership fixture:

- Infosets are assigned to two owner ranges.
- Worker deltas for remote infosets route to owner shards deterministically.

Determinism fixture:

- Run equivalent iterations with `workers=1`, `workers=2`, and `workers=8`.
- Expect identical tables within the selected exact floating-point/reduction policy.

---

## Phase 6: Chance implementation after infosets

Chance comes after infoset/table ownership because chance expansion can explode node counts and must lower into an already-defined infoset/table model.

### 6.1 Chance event table

Add dense chance event metadata:

```cpp
enum class chance_event_kind : uint8_t {
    deal_flop,
    deal_turn,
    deal_river
};

struct chance_event {
    chance_event_kind kind;
    card_mask dead_cards;
    uint32_t first_outcome;
    uint32_t outcome_count;
};

struct chance_outcome {
    card_mask cards;
    float probability;
    uint32_t child_node;
    uint32_t board_partition_id;
};
```

`solver_graph_view<N>::chance_event_by_node[node]` points into `chance_event_table`.

`dead_cards` is the parent legality context for the event: folded/active hole cards known to the subgame, the current board, and any other removed cards. If equivalent state is carried by the parent node/state instead, the chance table must expose it through a validation view. The validation path must be able to check legality without reconstructing the full betting context repeatedly:

```text
((parent_board | outcome.cards) & hole_cards) == 0
(chance_event.dead_cards & outcome.cards) == 0
```

### 6.2 Traversal probability

Replace current uniform chance behavior:

```cpp
child.chance_weight *= 1.0f / action_count;
```

with:

```cpp
child_reach.chance *= chance_outcome.probability;
```

Validation:

- Outcome count matches graph child count.
- Each outcome child matches the graph edge child.
- Probabilities sum to one for the legal blocked state.
- Board masks do not duplicate cards.
- Outcome cards do not collide with the event's dead-card mask.

### 6.3 Board partition integration

The existing scheduler has:

- `graph_partition`
- `board_partition_plan`
- board-major task indexing
- dynamic and static board schedulers

Chance implementation should feed that scheduler rather than replacing it.

Target:

```text
chance outcome -> board_partition_id -> board/partition task
```

For river subgames, board ownership remains simple: each river board/cache is a board task. For turn/flop subgames, downstream boards should map deterministically to board partitions.

---

## Phase 7: Real betting graph generation

Do not build a parallel validation game generator. Build the actual Hold'em betting graph generator as the source of both tests and production graphs.

### 7.1 Betting state

```cpp
template <std::size_t N>
struct betting_state {
    street current_street;
    uint8_t actor;
    std::array<float, N> stack;
    std::array<float, N> committed;
    std::array<bool, N> folded;
    std::array<bool, N> all_in;
    float pot;
    small_vector<pot_layer, N> pot_layers;
    float current_bet;
    uint16_t raise_count;
    action_history_id history;
};
```

State transitions must be pure and deterministic.

`pot_layers` should be maintained as part of betting-state evolution. Even before full side-pot support, generated terminals should carry a single `pot_layer{pot, active_eligible_mask, active_contributors_mask}` rather than a naked pot scalar.

### 7.2 Legal actions

Actions:

- Fold.
- Check.
- Call.
- Bet.
- Raise.
- All-in.

Action abstraction policies:

- Fixed pot fractions.
- Geometric sizes.
- Street-specific sizing sets.
- Stack-ratio buckets.
- Forced all-in threshold.

Every generated action must be legal for the current state, stack, and action history.

### 7.3 Lowering to existing graph

Use two stages:

1. Mutable rich nodes for generation and validation.
2. Lower to immutable CSR `game_graph` plus solver side arrays.

The hot solver graph should not retain rich betting state unless needed by traversal.

Lowering must assign:

- Node kind.
- Actor.
- Infoset ID.
- Chance event ID.
- Terminal leaf ID.
- Action indices.
- Street metadata.

---

## Phase 8: Terminal and side-pot semantics

### 8.1 Preserve HU fast path

Existing HU terminal code is not disposable. Keep:

- `evaluate_showdown_heads_up`.
- `evaluate_fold_values_heads_up`.
- `terminal_engine<2>`.
- `folded_mask<2>` direct booleans.
- OOP/IP ergonomic overloads.

### 8.2 N-way terminal policy

For `N >= 3`, terminal leaves must carry enough information for:

- Folded seats.
- Eligible all-in seats.
- Side-pot layers.
- Contributions.
- Rake.
- Showdown/fold kind.

The terminal API should consume:

```cpp
std::span<const pot_layer> side_pots;
```

The current generic fold kernel is not full side-pot accounting. Treat it as a stepping stone, not final production semantics, and keep the terminal API shaped around pot layers from the start.

### 8.3 Terminal workspace

Keep `terminal_workspace<N>` as the owner of materialized `river_reach_index` arrays. Build once per river-board context and reuse across terminal leaves.

Do not rebuild reach indices inside terminal-node loops.

---

## Phase 9: Checkpointing and table compatibility

Implement checkpointing after the table layout, infoset ownership model, precision policy, and reduction policy have stabilized. The format still needs to be designed early enough that layout choices are checkpoint-compatible, but persistence should not lead the implementation sequence.

Checkpoint format must reflect the shared architecture:

- Format version.
- Endianness.
- Player count `N`.
- Solver config hash.
- Graph/config metadata hash.
- Infoset/action/table layout hash, including storage precision.
- Infoset owner ranges.
- Iteration number.
- CFR variant.
- Regret table.
- Strategy-sum table.
- Precision policy, accumulation precision, and storage encoding.
- Reduction order policy.
- Scheduler/chance seed if sampling is enabled.

Resume must reject incompatible:

- Graph shape.
- Infoset count.
- Action offsets.
- Player count.
- CFR variant state.
- Precision/layout/reduction policy.

Checkpoint chunks should align naturally with infoset owner ranges and table shard views.

---

## Phase 10: Benchmarks and observability

Benchmarks should report real solver work, not just framework throughput.

### 10.1 Required counters

Iteration diagnostics should include:

- Nodes visited.
- Edges scanned.
- Player nodes.
- Chance nodes.
- Terminal nodes.
- Terminal evaluations.
- Regret updates.
- Strategy updates.
- Chance outcomes.
- Reduction entries.
- Reduction values.
- CFR+ clipped values.
- Scheduler tasks.
- Max traversal stack depth.
- Max action count.
- Zero-reach prunes.
- Time in traversal, terminal evaluation, reduction, chance, and checkpointing.

### 10.2 Solver quality counters

Every iteration should expose enough math diagnostics to catch a fast but wrong solver:

- `exploitability_estimate` when an exact or sampled estimator is available.
- `average_strategy_mass`.
- `regret_norm`.
- `max_regret`.
- `mean_regret`.
- `positive_regret_count`.
- Strategy-sum mass by player.

Before full best-response support exists, tiny graph fixtures should still verify convergence toward a known equilibrium rather than only checking that tables changed. The API should leave room for exact best response and exploitability calculations to consume the same terminal utility-vector interface used by CFR.

### 10.3 Memory counters

Context construction should report:

- Nodes.
- Edges.
- Infosets.
- Action values.
- Chance events.
- Chance outcomes.
- Terminal leaves.
- Regret bytes.
- Strategy-sum bytes.
- Action-offset bytes.
- Owner-map bytes.
- Worker scratch bytes.
- Delta-buffer reserved bytes.
- River cache/workspace bytes.

### 10.4 Performance tiers

Use tiers based on real Zeta structures:

- Tiny hand-authored `game_graph` correctness fixture.
- Tiny generated Hold'em river graph.
- Small generated river abstraction.
- Board batch with existing scheduler.
- Large table memory stress.
- Turn/flop chance expansion once implemented.

Do not add benchmark tiers for unrelated standalone games.

---

## Solver capability matrix

Track capabilities explicitly so HU and N-way kernels do not accidentally promise identical production readiness while their terminal and betting semantics differ.

| Feature | HU | N-way |
| --- | --- | --- |
| Exact showdown | yes | limited initially |
| Fold terminal values | yes | yes, side-pot-aware API required |
| Side pots | no initially | yes, via `pot_layer` |
| CFR+ | yes | yes |
| Regret matching policy abstraction | yes | yes |
| Checkpoint/resume | yes | yes |
| Deterministic reduction | yes | yes |
| Chance sampling | future | future |
| Exact chance expansion | yes for supported subgames | yes for supported subgames |
| Convergence diagnostics | yes | yes |
| Exploitability/best response | future | future |

---

## Recommended implementation sequence

### Milestone A: Graph metadata, infoset identity, and construction accounting

1. Add solver side arrays/views around `game_graph` without moving topology ownership out of `game_graph`.
2. Add actor, chance-event, terminal-leaf, and street metadata.
3. Define infoset identity lowering for Hold'em-generated states.
4. Include explicit board abstraction ID and chance/runout class ID in the key.
5. Add dense infoset ID assignment before large graph generation.
6. Add mandatory memory accounting for estimated infosets, actions, table layout, precision policy, owner maps, scratch, and checkpoint estimates.
7. Include storage precision, accumulation precision, and reduction order in compatibility hashing.
8. Add terminal utility-vector plumbing so terminal leaves return `terminal_values<N>`.
9. Add initial `pot_layer` terminal/betting metadata with one main-pot layer, including eligible and contributor masks.

### Milestone B: Real CFR math on tiny graphs

1. Add explicit iteration config with `cfr_update_mode::alternating`.
2. Add explicit strategy/regret-matching policy abstraction.
3. Add `edge_child_value`.
4. Compute node value from strategy-weighted child values.
5. Select terminal utility by `terminal_values.utility[updating_player]`.
6. Compute raw regret deltas only for `actor == updating_player`.
7. Propagate through non-updating-player nodes without regret writes.
8. Correct average strategy weighting with acting-player own reach for the selected averaging policy.
9. Apply CFR+ clipping once after merge.
10. Add early solver-quality diagnostics for regret norms and strategy mass.
11. Make `regret_updates/s` nonzero in the real CFR iteration benchmark.

### Milestone C: Heads-up kernel

1. Add `cfr_engine<2>` specialization.
2. Preserve HU terminal exact paths.
3. Use scalar OOP/IP reach and direct opponent-reach formulas.
4. Add tests that prove `N=2` does not use generic N-way opponent product loops where avoidable.

### Milestone D: Ownership and deterministic reduction

1. Add infoset owner ranges.
2. Add owner-aware deterministic reduction design behind current reduction API.
3. Add infoset/action/actor collision validation.
4. Add infoset owner routing tests.
5. Add remote delta volume and owner hit distribution diagnostics.
6. Preserve the invariant that CFR+ clipping happens only after deterministic raw-delta merge.

### Milestone E: N-way reach and terminal integration

1. Split traversal frame from reach stack.
2. Add `cfr_engine<N>` for `N >= 3`.
3. Add N-way reach state in separate scratch.
4. Add N-way counterfactual reach product helper.
5. Route N-way terminal leaves through `terminal_engine<N>`.
6. Add three-player reach correctness tests.

### Milestone F: Chance events

1. Add chance event/outcome tables.
2. Include dead-card or parent-legality metadata for chance validation.
3. Replace uniform chance with outcome probabilities.
4. Add blocker-safe flop/turn/river generation.
5. Connect chance outcomes to board partition IDs.
6. Validate probability sums and board legality.

### Milestone G: Real betting graph

1. Implement deterministic betting state transitions.
2. Maintain `pot_layer` side-pot structure during state transitions.
3. Implement legal action generation.
4. Implement action abstraction policies.
5. Lower generated states to `game_graph` plus solver side arrays.
6. Add terminal leaf generation with pot/contribution context.

### Milestone H: Hold'em validation fixtures

1. Add hand-authored graph fixtures through `graph_builder`.
2. Add slow reference traversal over the same `game_graph`.
3. Add direct terminal-vs-traversal comparisons using river caches.
4. Add CFR+ reduction-order tests.
5. Add infoset owner routing tests.
6. Add regret-matching, terminal-perspective, reach-correctness, infoset-collision, and worker-count determinism tests.
7. Add tiny known-equilibrium convergence tests.
8. Add initial exploitability or best-response estimator hooks where exact tiny-graph evaluation is available.

### Milestone I: Scaling and persistence

1. Add checkpoint save/load/resume.
2. Add shard-aligned checkpoint chunks.
3. Add large table memory stress benchmarks.
4. Add NUMA-aware owner range metadata.
5. Add long-run diagnostics and convergence reporting on generated Hold'em abstractions.

---

## Immediate next tasks

1. **Add graph metadata and memory accounting**: actor, chance-event, terminal-leaf, street, numeric policy, and construction-time memory estimates.
2. **Define infoset identity before chance expansion**: include board abstraction ID, chance/runout class, dense ID assignment, and estimated table bytes before large graph generation.
3. **Separate terminal vectors from CFR perspective**: terminal engines return `terminal_values<N>`; alternating CFR reads `utility[updating_player]`.
4. **Add regret matching policy**: traversal consumes strategy probabilities from an explicit strategy policy instead of embedding regret matching.
5. **Add child action values**: regret deltas require stored child values and node/depth-local values.
6. **Mandate alternating update semantics first**: update regrets only when `actor == updating_player`; non-updating actors only propagate.
7. **Fix average strategy weighting**: use `chance_reach * reach[actor] * sigma[action]` under the selected averaging policy.
8. **Mandate raw-delta reduction**: apply CFR+ clipping only after worker deltas are merged.
9. **Keep HU optimized**: implement a `cfr_engine<2>` path that uses scalar OOP/IP reach and the existing exact HU terminal kernels.
10. **Add ownership diagnostics**: track remote delta volume and owner hit distribution.
11. **Shape side-pot APIs now**: carry `pot_layer` with eligible and contributor masks even if the first graph emits one main-pot layer.
12. **Add solver-quality hooks**: expose regret norms, strategy mass, and known-equilibrium convergence checks before relying on speed benchmarks.
13. **Use Hold'em validation fixtures**: small generated or hand-authored Zeta graphs only, using the same graph/table/terminal/reduction code paths as production.

---

## Definition of done for the first serious solver

The first serious multiway-capable solver is complete when:

- A full CFR/CFR+ iteration computes action values, node values, regret deltas, and strategy deltas.
- Terminal evaluation returns utility vectors and CFR selects the updating-player perspective.
- Regret matching is behind an explicit strategy policy.
- Worker deltas are raw and CFR+ clipping happens once after merge.
- Infoset ownership is explicit and reductions can route by owner range.
- Reduction diagnostics report remote delta volume and owner hit distribution.
- Infoset keys explicitly control board abstraction and chance/runout abstraction.
- Memory estimates exist for infosets, actions, table bytes, worker deltas, and checkpoints before large graph generation.
- HU uses a specialized fast path and keeps exact terminal kernels.
- N-way traversal uses compile-time `N` reach state without bloating every traversal frame.
- Chance events use explicit outcome probabilities, not uniform child placeholders.
- Chance validation has dead-card or equivalent parent-legality metadata.
- Generated Hold'em graphs lower into the same `game_graph` and table layout used by traversal.
- Terminal and betting APIs carry `pot_layer` side-pot structure with eligible and contributor masks.
- Checkpoints validate graph, infoset, action-offset, player-count, table-layout, storage precision, accumulation precision, and reduction-order compatibility.
- Solver-quality diagnostics report regret norms, strategy mass, and convergence on at least one tiny known-equilibrium graph.
- Benchmarks report real regret updates, strategy updates, terminal evaluations, reductions, and memory use.
- Validation fixtures exercise Zeta Hold'em code paths rather than unrelated standalone games.
