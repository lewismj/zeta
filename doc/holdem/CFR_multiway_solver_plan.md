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

This sparse routing model is correct for the first implementation, but it may become the bottleneck at large scale when millions of infosets, hundreds of workers, and billions of updates make reduction dominate traversal. Keep a future optimization path open:

```text
scheduler partition
    |
    v
expected infoset locality
    |
    v
worker preferred owner
```

Do not require locality-aware scheduling initially; require instrumentation that can prove whether remote sparse routing is becoming the limiting factor.

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
terminal_state<N>
        |
        v
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

Do not make terminal leaves directly encode evaluator calls. Terminal leaves should point to or carry a lowered `terminal_state<N>` that describes the terminal condition, then the terminal engine dispatches the appropriate evaluator. This leaves room for showdown, fold, timeout, rake, insurance, jackpot/rakeback variants, bomb pots, and other terminal semantics without leaking CFR traversal details into terminal representation.

This keeps alternating CFR, future simultaneous updates, exploitability calculation, best response, and EV reporting on the same terminal-state and utility-vector API.

### Pot accounting

N-way Hold'em terminal semantics require side-pot structure from the graph-generation stage onward, not as a terminal afterthought:

```cpp
using player_mask = uint64_t;

struct pot_layer {
    float amount;
    player_mask eligible_mask;
    player_mask contributors_mask;
};
```

The first implementation can emit exactly one main-pot layer, but terminal leaves and betting state should use the layered shape immediately so all-in and side-pot support does not require an API break. Keep `eligible_mask` and `contributors_mask` explicit: who can win a pot and who created/funded it are different auditing questions, especially in all-in cases. Use the `player_mask` alias at API boundaries so the terminal API names the concept instead of directly leaking a raw `N <= 64` representation assumption.

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
    terminal_state_table<N> terminal_states;
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

If `remote_delta_count`, `remote_delta_bytes`, or per-owner reduction time dominate, later scheduler policies should prefer assigning partitions to workers that own or are near the expected touched infoset ranges, while preserving deterministic reduction semantics.

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
terminal_state = terminal_state_table[terminal_leaf.terminal_state_id]
terminal_values = terminal_engine<N>::evaluate(terminal_state, ...)
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
    uint32_t terminal_state_id;
    terminal_context<N> context;
    folded_mask<N> folded;
};
```

HU specialization can keep:

- `heads_up_player folded_player`
- direct `evaluate_showdown_values(cache, oop_index, ip_index, context)`
- direct `evaluate_fold_values(cache, oop_index, ip_index, context, folded_player)`

N-way uses `terminal_engine<N>`.

The terminal leaf is an index/handle into terminal-state data, not a direct instruction to call a specific evaluator. The engine owns dispatch from `terminal_state<N>` to showdown, fold, timeout, rake-adjusted, or future variant-specific evaluation.

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
enum class chance_mode : uint8_t {
    enumerate,
    sample
};

enum class chance_event_kind : uint8_t {
    deal_flop,
    deal_turn,
    deal_river
};

struct chance_event {
    chance_event_kind kind;
    chance_mode mode;
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

Only `chance_mode::enumerate` needs to be implemented first, but the mode belongs in the early API. Enumerated and sampled chance have different traversal semantics, determinism requirements, checkpoint metadata, convergence diagnostics, and scheduler behavior:

```text
enumerate:
    chance node
        |
        +-- outcome child
        +-- outcome child
        +-- outcome child

sample:
    chance node
        |
        sample outcome using configured RNG stream
        |
        selected child
```

Sampling must use an explicit, checkpointed seed/stream policy and must report sampling diagnostics separately from exact chance-outcome enumeration.

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
- `chance_mode::enumerate` nodes expose all legal outcomes as graph children.
- `chance_mode::sample` nodes expose a reproducible sampling policy and checkpoint-compatible RNG stream metadata before they are enabled.

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
- Chance mode and sampling/RNG stream policy.

Resume must reject incompatible:

- Graph shape.
- Infoset count.
- Action offsets.
- Player count.
- CFR variant state.
- Chance mode.
- Sampling/RNG stream policy if sampling is enabled.
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
- `max_regret_infoset_id`.
- `mean_regret`.
- `positive_regret_count`.
- `largest_strategy_entropy_drop`.
- `largest_strategy_change`.
- Strategy-sum mass by player.

Global regret metrics are not enough: mean regret can improve while one broken infoset dominates the solution. Diagnostics should retain enough per-infoset context to identify the specific infoset/action range responsible for the largest regret or strategy movement. Before full best-response support exists, tiny graph fixtures should still verify convergence toward a known equilibrium rather than only checking that tables changed. The API should leave room for exact best response and exploitability calculations to consume the same terminal-state and utility-vector interface used by CFR.

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

## Implementation plan

Each task should land the relevant surface in its intended production shape. Do not add temporary mini-solvers, standalone game models, placeholder terminal semantics, or graph/table APIs that are expected to be replaced by a later task.

### Task 1: Establish solver metadata and compatibility policies

Status: implemented.

Add the shared metadata and compatibility layer that every later task will build on:

1. [x] Keep topology owned by immutable `game_graph`.
2. [x] Add `solver_graph_annotations` and `solver_graph_view<N>` side arrays for actor, chance-event ID, terminal-leaf ID, and street/state metadata.
3. [x] Add numeric policy types for table storage precision, accumulation precision, and reduction order.
4. [x] Add `chance_mode` to chance metadata with `enumerate` and `sample`, while supporting only `enumerate` initially.
5. [x] Add `player_mask` and use it in terminal/betting APIs instead of raw mask types.
6. [x] Add compatibility hashing inputs for graph metadata, infoset/action layout, numeric policy, reduction policy, chance mode, and player count.
7. [x] Add validation that side arrays match `node_count`, node kinds, actor ranges, chance-node IDs, terminal-node IDs, and street metadata constraints.

Implemented in `zeta/holdem/src/cfr/solver/metadata.h`, `zeta/holdem/src/terminal/terminal_types.h`, and the `cfr_graph_metadata` test suite.

This task creates stable names and compatibility contracts first so later graph generation, traversal, checkpointing, and diagnostics do not need type or format refactors.

### Task 2: Define Hold'em infoset identity and memory planning

Implement infoset identity before any large chance or betting expansion:

1. Define the Hold'em infoset key with acting player, street, private abstraction/exact combo class, public board abstraction ID, chance/runout class ID, betting history abstraction, stack/pot abstraction, legal action set ID, player count, and subgame/root context ID.
2. Add abstraction-policy hooks that explicitly produce board abstraction IDs and chance/runout class IDs.
3. Lower infoset keys to dense IDs before table construction.
4. Validate that shared infoset IDs have identical actor, street, legal action IDs/order, abstraction class, player count, and owner.
5. Estimate nodes, infosets, action values, regret bytes, strategy-sum bytes, owner-map bytes, worker-delta bytes, terminal-state bytes, chance-event bytes, scratch bytes, and checkpoint bytes before materializing large graphs.
6. Fail graph construction/planning early when estimates exceed configured limits.

This task makes memory and table layout the controlling design constraint before chance expansion can explode the tree.

### Task 3: Build terminal-state and pot-layer semantics

Separate terminal representation from evaluator dispatch in its final API shape:

1. Add `terminal_state<N>` and `terminal_state_table<N>`.
2. Make terminal leaves reference terminal-state records instead of directly encoding evaluator calls.
3. Represent showdown, fold, timeout, rake-adjusted, and future variant-specific cases through terminal-state kinds/data.
4. Keep `terminal_engine<2>` on exact heads-up showdown/fold kernels.
5. Keep `terminal_engine<N>` for N-way terminal dispatch.
6. Ensure all terminal evaluation returns `terminal_values<N>` utility vectors and never takes a CFR updating-player perspective.
7. Add `pot_layer { amount, player_mask eligible_mask, player_mask contributors_mask }` and carry pot layers in terminal states, even when the first implementation emits one main-pot layer.
8. Include contribution, folded/all-in eligibility, and rake fields needed to audit terminal values.

This task avoids terminal API churn by making evaluator selection a terminal-engine responsibility from the start.

### Task 4: Implement deterministic betting-state generation and graph lowering

Build the real Hold'em graph-generation path instead of a disposable validation generator:

1. Implement deterministic `betting_state<N>` transitions for fold, check, call, bet, raise, and all-in.
2. Maintain stacks, committed amounts, folded/all-in flags, current bet, raise count, action history, and `pot_layer` side-pot state during every transition.
3. Implement legal action generation from current betting state.
4. Implement action abstraction policies for fixed pot fractions, geometric sizes, street-specific sizing sets, stack-ratio buckets, and forced all-in thresholds.
5. Lower generated states to immutable `game_graph` plus solver annotations.
6. Lower terminal betting states to terminal-state records.
7. Assign action indices, infoset IDs, chance-event IDs, terminal-leaf IDs, and street metadata during lowering.
8. Validate graph shape, node kinds, action ordering, terminal-state references, side-pot invariants, and infoset/action compatibility.

This task produces the production graph path used by both tests and real solving, so no later task needs to replace validation scaffolding.

### Task 5: Implement enumerated chance events and scheduler integration

Add chance using the final chance abstraction boundary, with enumeration as the first supported mode:

1. Add `chance_event_table` and `chance_outcome` storage.
2. Require `chance_mode::enumerate` for enabled chance traversal.
3. Reject or explicitly disable `chance_mode::sample` until a checkpoint-compatible RNG stream policy is implemented.
4. Include dead-card or parent-legality metadata for chance validation.
5. Replace uniform chance-node traversal with outcome probabilities.
6. Generate blocker-safe flop, turn, and river outcomes.
7. Validate outcome counts, graph child alignment, probability sums, board legality, and dead-card collisions.
8. Connect chance outcomes to board partition IDs and the existing board/partition scheduler.
9. Report exact chance-outcome diagnostics separately from future sampled-chance diagnostics.

This task makes chance expansion compatible with determinism, scheduling, convergence reporting, and future sampling without changing traversal APIs later.

### Task 6: Implement CFR iteration math and strategy policy

Turn traversal into a real alternating CFR/CFR+ iteration over the shared graph/table/terminal surfaces:

1. Add explicit solver and iteration config with CFR variant, update mode, iteration number, updating player, strategy weight, numeric policy, reduction policy, and chance mode.
2. Add strategy-policy abstraction for vanilla regret matching, CFR+, and future linear/DCFR policies.
3. Ensure regret matching returns valid distributions for all-negative, all-zero, and positive-regret inputs.
4. Add child action value storage and depth-local value storage.
5. Compute terminal leaf values by evaluating terminal state to `terminal_values<N>` and selecting `utility[updating_player]` only inside CFR traversal.
6. Compute node values from strategy-weighted child values.
7. Compute raw regret deltas only when `actor == updating_player`.
8. Propagate through non-updating-player nodes without regret writes.
9. Weight average-strategy deltas by `strategy_weight * chance_reach * reach[actor] * sigma[action]`.
10. Keep worker deltas raw and apply CFR+ clipping only after deterministic merge.
11. Add real iteration diagnostics for regret updates, strategy updates, terminal evaluations, reduction values, and timing.

This task establishes mathematically correct CFR behavior before optimizing HU or N-way kernels.

### Task 7: Implement HU and N-way traversal kernels without duplicating shared systems

Specialize only hot CFR math/traversal and terminal dispatch, while keeping graph, tables, scheduler, reduction, checkpoints, and validation shared:

1. Add `cfr_engine<2>` with scalar OOP/IP reach, direct opponent-reach formulas, and exact HU terminal paths.
2. Add `cfr_engine<N>` for `N >= 3` with `std::array<float, N>` reach state stored outside traversal frames.
3. Use compact traversal frames with node ID, edge cursor, reach slot, value slot, and phase.
4. Store reach/value scratch by reusable DFS depth slots where possible, not by node ID unless required.
5. Add N-way counterfactual reach product helper.
6. Ensure HU avoids generic N-way loops in opponent-reach and terminal hot paths.
7. Share action-value, regret-delta, strategy-delta, scheduler, terminal-state, and reduction code wherever it is not in the hot specialized kernel.
8. Add HU and three-player reach/value correctness fixtures.

This task preserves heads-up performance while enabling N-way reach semantics without contaminating shared infrastructure.

### Task 8: Implement infoset ownership and deterministic reduction

Make multi-worker updates deterministic and ready for NUMA-aware table ownership:

1. Add `infoset_owner_map` with contiguous owner ranges.
2. Add optional `table_shard_view` over global regret and strategy-sum storage.
3. Route sparse worker-local deltas to owner ranges during reduction.
4. Apply raw regret deltas and strategy deltas in deterministic order.
5. Apply CFR+ clipping only after all raw regret deltas are merged.
6. Track `remote_delta_count`, `remote_delta_bytes`, `owner_hit_distribution`, `owner_remote_hit_distribution`, per-owner touched values, and per-owner reduction time.
7. Report enough reduction diagnostics to decide whether sparse remote routing is dominating traversal.
8. Preserve worker-count determinism under the selected floating-point/reduction policy.
9. Keep a future scheduler-locality path open where partitions can prefer workers near expected touched infoset owners without changing the reduction API.
10. Add owner-routing, CFR+ clipping-order, and worker-count determinism tests.

This task separates traversal scheduling from table ownership without allowing concurrent traversal writes into global tables. Locality-aware scheduling is a later optimization, not a prerequisite for the first deterministic owner-sharded reduction.

### Task 9: Add validation, reference checks, and convergence instrumentation

Build correctness coverage over the actual Zeta solver components:

1. Add hand-authored `game_graph` fixtures through `graph_builder`.
2. Add generated tiny Hold'em river and betting-state fixtures through the production graph generator.
3. Add slow reference traversal over the same `game_graph`, table layout, terminal-state table, and terminal engine.
4. Compare traversal terminal leaves with direct terminal-engine calls using river caches and reach indices.
5. Test regret matching, terminal perspective selection, child values, node values, alternating-update semantics, average-strategy weighting, reach correctness, chance probabilities, infoset collisions, owner routing, CFR+ reduction order, and worker-count determinism.
6. Add tiny known-equilibrium convergence tests.
7. Add quality diagnostics: `exploitability_estimate` where available, `average_strategy_mass`, `regret_norm`, `max_regret`, `max_regret_infoset_id`, `mean_regret`, `positive_regret_count`, `largest_strategy_entropy_drop`, `largest_strategy_change`, and strategy-sum mass by player.
8. Add per-infoset diagnostic tests that identify the infoset/action range responsible for max regret or largest strategy movement.

This task proves the solver is mathematically plausible, not merely deterministic and fast.

### Task 10: Add checkpointing and resume compatibility

Implement persistence after graph, table, owner, numeric, terminal-state, and chance-mode layouts are stable:

1. Add checkpoint format versioning, endianness, player count, solver config hash, graph/config metadata hash, infoset/action/table layout hash, numeric policy, reduction policy, chance mode, owner ranges, iteration number, and CFR variant state.
2. Save and load regret and strategy-sum tables with their storage encoding.
3. Include accumulation precision and reduction-order policy in compatibility checks.
4. Include terminal-state/table compatibility and chance-mode compatibility.
5. Include sampling/RNG stream policy if sampled chance is enabled in the future.
6. Reject resume on incompatible graph shape, infoset count, action offsets, player count, CFR variant, precision/layout/reduction policy, terminal-state layout, chance mode, or RNG stream policy.
7. Align checkpoint chunks with infoset owner ranges and table shard views.
8. Add save/load/resume tests over tiny generated Hold'em graphs and worker-owned table shards.

This task persists only stable production layout decisions and avoids checkpoint-format churn.

### Task 11: Add benchmarks, observability, and scaling checks

Measure real solver work and expose scaling risks:

1. Add iteration counters for nodes visited, edges scanned, player nodes, chance nodes, terminal nodes, terminal evaluations, regret updates, strategy updates, chance outcomes, reduction entries, reduction values, CFR+ clipped values, scheduler tasks, max traversal stack depth, max action count, zero-reach prunes, and phase timings.
2. Add memory counters for nodes, edges, infosets, action values, chance events, chance outcomes, terminal leaves, terminal states, regret bytes, strategy-sum bytes, action-offset bytes, owner-map bytes, worker scratch bytes, delta-buffer reserved bytes, river cache/workspace bytes, and checkpoint estimates.
3. Add benchmark tiers for hand-authored graph fixtures, tiny generated Hold'em river graphs, small generated river abstractions, board batches through the existing scheduler, large table memory stress, and turn/flop chance expansion.
4. Add long-run convergence reporting on generated Hold'em abstractions.
5. Add NUMA-ready owner range metadata without changing logical table storage.
6. Ensure benchmarks report real regret updates, strategy updates, terminal evaluations, reductions, memory, and quality diagnostics.

This task makes performance and quality visible on production structures rather than synthetic framework throughput.

---

## Definition of done for the first serious solver

The first serious multiway-capable solver is complete when:

- A full CFR/CFR+ iteration computes action values, node values, regret deltas, and strategy deltas.
- Terminal leaves reference terminal-state records; terminal evaluation returns utility vectors and CFR selects the updating-player perspective.
- Regret matching is behind an explicit strategy policy.
- Worker deltas are raw and CFR+ clipping happens once after merge.
- Infoset ownership is explicit and reductions can route by owner range.
- Reduction diagnostics report remote delta volume and owner hit distribution.
- Infoset keys explicitly control board abstraction and chance/runout abstraction.
- Memory estimates exist for infosets, actions, table bytes, worker deltas, and checkpoints before large graph generation.
- HU uses a specialized fast path and keeps exact terminal kernels.
- N-way traversal uses compile-time `N` reach state without bloating every traversal frame.
- Chance events use explicit outcome probabilities, not uniform child placeholders.
- Chance metadata includes an explicit `chance_mode`; enumeration is the first implemented mode and sampling requires checkpoint-compatible RNG policy.
- Chance validation has dead-card or equivalent parent-legality metadata.
- Generated Hold'em graphs lower into the same `game_graph` and table layout used by traversal.
- Terminal and betting APIs carry `pot_layer` side-pot structure with `player_mask` eligible and contributor masks.
- Checkpoints validate graph, infoset, action-offset, player-count, table-layout, storage precision, accumulation precision, and reduction-order compatibility.
- Checkpoints validate chance mode and sampling/RNG policy if sampling is enabled.
- Solver-quality diagnostics report regret norms, max-regret infoset ID, strategy movement/entropy changes, strategy mass, and convergence on at least one tiny known-equilibrium graph.
- Benchmarks report real regret updates, strategy updates, terminal evaluations, reductions, and memory use.
- Validation fixtures exercise Zeta Hold'em code paths rather than unrelated standalone games.
