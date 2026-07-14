# CFR+ Outline Plan (Hold'em)

## Objective

Define a cache-efficient, multithread-friendly CFR+ architecture that consumes the existing river terminal evaluator (`river_terminal_cache`, `river_reach_index`, terminal value APIs).

This document focuses on:
1. Core data structures
2. Threading model
3. Update/reduction model
4. Implementation sequence

---

## Project Organization

**Recommended folder structure for CFR+ implementation:**

Organize holdem/src with core domain models at top level and subsystems in folders:

```
holdem/
└── src/
    ├── board.h             (core poker domain model)
    ├── range.h             (core poker domain model)
    ├── range_parser.h      (core poker domain model)
    ├── tables.h            (precomputed lookup tables)
    ├── tables.cpp
    ├── tables.generated.cpp
    │
    ├── eval/               (hand evaluation subsystem) ✓ IMPLEMENTED
    │   ├── eval.h
    │   └── evaluator.h
    │
    ├── terminal/           (river terminal evaluation subsystem)
    │   └── terminal.h
    │
    └── cfr/                (CFR+ solver subsystem)
        ├── graph.h         (game_graph, CSR topology + partitions)
        ├── regret_table.h   (regret storage, contiguous layout)
        ├── strategy_table.h (strategy storage, contiguous layout)
        ├── traversal.h     (traversal stack, worker context)
        ├── scheduler.h     (work scheduling, board-level partitioning)
        ├── reduction.h     (reduction logic, batch updates)
        └── solver.h        (top-level solver orchestration)
```

**Benefits of dedicated folder:**

1. **Clear separation of concerns** – CFR+ is a distinct subsystem from existing terminal eval
2. **Isolation** – reduces coupling with board.h, eval.h, etc.
3. **Scalability** – easy to add more CFR+ modules (infoset indexing, value approximation, etc.)
4. **Testability** – unit tests can focus on CFR+ without polluting holdem/ test suite
5. **Dependency clarity** – CFR+ consumes terminal evaluator (unidirectional), not vice versa

**Include patterns:**

- Internal CFR+ files use: `#include <holdem/cfr/graph.h>`
- Solver client code uses: `#include <holdem/cfr/solver.h>`
- No other holdem/ files should `#include <holdem/cfr/*>` except the solver entry point

---

## Design principles

1. **Immutable shared state** for board-specialized evaluator caches.
2. **Thread-local mutable state** for traversal-time data and updates.
3. **Contiguous storage (SoA/CSR-like)** over pointer-heavy object graphs.
4. **Allocation-free hot loops** (no per-node heap work).
5. **Batch reduction** of thread-local updates into global tables.

---

## Core data structures

## 1) Immutable board caches (shared)

Use existing:

```cpp
struct river_terminal_cache { ... };
```

One per river board (or reused board set).  
Shared by all workers as read-only.

---

## 2) Thread-local reach indices

Use existing:

```cpp
river_reach_index make_river_reach_index(const river_terminal_cache&, const reach_vector&);
```

Per traversal/thread:
- OOP reach index
- IP reach index

No sharing, no locking.

---

## 3) Regret tables (global)

Contiguous layout, no nested vectors:

```cpp
struct regret_table {
    // infoset-major: [infoset0_a0..aN][infoset1_a0..aN]...
    std::vector<float> regrets;
    std::vector<uint32_t> action_offset; // size infoset_count + 1
};
```

Access:
- `begin = action_offset[infoset]`
- `end = action_offset[infoset + 1]`

---

## 4) Strategy sums (global)

Same layout as regrets:

```cpp
struct strategy_sum_table {
    std::vector<float> sums;
    std::vector<uint32_t> action_offset; // mirrored with regret table
};
```

---

## 5) Thread-local update buffers

**Critical architectural concern:**

Duplicating entire regret/strategy space per thread does not scale.

For example:
- 300 MB global regrets + 300 MB global strategy sums = 600 MB
- 16 threads duplicating this = 9.6 GB just in update buffers

This stops scaling surprisingly quickly. Commercial solvers instead use:

1. **Sparse update buffers** – store only visited infosets
2. **Block-local accumulation** – partition the problem space, each thread owns a block
3. **Board-local accumulation** – accumulate deltas per board batch, merge once
4. **Chunk reduction** – divide traversals into chunks with separate buffers
5. **Ownership partitioning** – each thread owns slices of the regret table (no contention)

**Recommended approach for this solver:**

Use board-local accumulation combined with cache-aligned worker contexts:

```cpp
struct alignas(64) worker_context {
    // Immutable references
    const game_graph* graph;
    const river_terminal_cache* board_cache;
    
    // Thread-local (small, fixed size)
    std::array<traversal_frame, MAX_DEPTH> traversal_stack;
    river_reach_index reach_oop;
    river_reach_index reach_ip;
    
    // Per-board accumulators (reset each board)
    std::vector<float> board_regret_deltas;
    std::vector<float> board_strategy_deltas;
    
    // Scratch arrays
    std::vector<float> scratch;
};
```

Workers accumulate updates for one board at a time, then flush to global tables before moving to the next board. This:
- Keeps working set small
- Eliminates false sharing
- Makes reduction deterministic (can replay or validate)
- Scales linearly with board count

Workers write only local buffers during traversal.  
Global merge occurs once per board (or iteration), not per node visit.

---

## 6) Traversal stack (thread-local, fixed)

No recursion, no heap allocation:

```cpp
struct traversal_frame {
    uint32_t node_id;
    float reach_oop;
    float reach_ip;
    float chance;
    uint16_t action_index;
};

std::array<traversal_frame, MAX_DEPTH> stack;
```

---

## 7) Infoset graph storage (CSR-like + partition metadata)

`game_graph` is built once, then read extremely often during solving. That makes
CSR the right base layout: contiguous, compact, cache-friendly, and immutable.
Do not bolt scheduling on later — include partition metadata at construction.

```cpp
struct edge {
    uint32_t child_node;
    uint16_t action_index;
};

struct graph_partition {
    uint32_t begin_node;       // inclusive
    uint32_t end_node;         // exclusive
    uint32_t subtree_size;
    uint32_t terminal_count;
    uint32_t action_count;
    uint16_t min_depth;
    uint16_t max_depth;
    uint64_t estimated_work;
};

struct game_graph {
    // Immutable topology (CSR)
    std::vector<uint32_t> row_offsets;   // size node_count + 1
    std::vector<edge> edges;             // flattened adjacency
    std::vector<uint8_t> node_kind;      // player/chance/terminal
    std::vector<uint32_t> infoset_id;    // mapping for player nodes

    // Immutable scheduling metadata
    std::vector<graph_partition> partitions;
};
```

Traversal:

```cpp
for (uint32_t e = row_offsets[node]; e < row_offsets[node + 1]; ++e) {
    auto child = edges[e].child_node;
}
```

Partitioning objective: equalize estimated traversal cost, not raw node count.
Node-ID ranges alone are usually imbalanced in poker trees.

`estimated_work` should be a construction-time heuristic (exact formula can evolve),
for example:

```
estimated_work ~= descendants * action_count * terminal_probability
```

or a simpler approximation when probability data is unavailable.

---

## 8) Terminal evaluator integration

At river terminal nodes:
1. Select board cache
2. Build thread-local reach indices
3. Call terminal API (`evaluate_showdown_values` / `evaluate_fold_values`)
4. Propagate utilities upward

No own-reach multiplication inside terminal evaluator; apply CFR math at traversal level.

---

## 9) Work scheduling

**Recommended primary approach: Board-level partitioning**

Poker has embarrassingly parallel work at the board level.

```
Iteration N
├─ Board 1 → Thread 0 (complete CFR traversal)
├─ Board 2 → Thread 1 (complete CFR traversal)
├─ Board 3 → Thread 2 (complete CFR traversal)
├─ ...
└─ Reduction (global tables updated)
```

Characteristics:
- Each worker gets independent board batches
- No intra-board synchronization
- Near-linear scaling
- Each board is completely self-contained

Later extensions:
- Dynamic work queue for load balancing (some boards take longer than others)
- NUMA-aware board partitioning if latency matters
- Only parallelize within a board if board count < thread count

Keep this separation strict:
- `game_graph`: immutable data + valid work partitions
- `scheduler`: mutable execution policy/state

---

## 10) Reduction

Per iteration:
1. Workers finish local traversals
2. Reduce `local_updates[*]` into global `regret_table` / `strategy_sum_table`
3. Apply CFR+ regret floor and strategy update
4. Begin next iteration

Use chunked contiguous reductions for cache efficiency.

---

## Memory and ownership model

Organize state around who owns and when it's accessed:

```
SHARED IMMUTABLE (read-only, shared across all threads)
────────────────────────────────────
├─ game_graph (infoset tree structure)
├─ river_terminal_cache (one per river board)
├─ Precomputed lookup tables (rank buckets, combos, etc.)
└─ CFR+ parameters (learning rates, iteration count)

THREAD-LOCAL (per worker, no synchronization needed)
──────────────────────────────────
├─ traversal_stack (fixed size array)
├─ river_reach_index (two per board: OOP, IP)
├─ scratch_arrays (temp space for calculations)
└─ board_regret_deltas (per-board accumulator)
│  + board_strategy_deltas

GLOBAL MUTABLE (shared, updated once per board/iteration)
──────────────────────────────────
├─ regret_table[infoset][action]
└─ strategy_sum_table[infoset][action]

WORKER REDUCTIONS (batch accumulate before global merge)
──────────────────────────────────
├─ Per-thread local accumulators (flushed to global once per board)
└─ Deterministic reduction order (for replay/validation)
```

Benefits of this separation:
- **Immutable shared** → zero synchronization cost
- **Thread-local** → cache-friendly, no false sharing
- **Global mutable** → minimal contention (batch updates only)
- **Reduction** → deterministic, reproducible, testable

---

## What to avoid

1. Pointer-based node trees with scattered memory.
2. `std::vector<std::vector<float>>` for regrets/strategies.
3. Atomic/global writes in the inner traversal loop.
4. Heap allocation during traversal.
5. False sharing across thread-local buffers (pad/align worker state).
6. **Duplicating entire regret/strategy space per thread.** This scales as O(threads × memory), not O(memory). Use board-local or sparse accumulation instead.
7. Mixing synchronization granularities (some updates per-node, some per-iteration).

**Critical for multithread performance:**

Always use `alignas(64)` on worker context structs to ensure cache-line isolation:

```cpp
struct alignas(64) worker_context {
    // This guarantees no adjacent threads share a cache line
};
```

This is one of the highest-impact changes; false sharing alone can reduce throughput by 50%.

---

## Critical insights from architecture review

**Terminal evaluator is already well-designed for CFR+**

The current `river_terminal_cache` and `river_reach_index` components are close to ideal:
- Immutable, reusable board caches (shared, no locking)
- Small, cheap-to-rebuild reach indices (thread-local)
- Contiguous array storage (excellent memory layout)
- Allocation-free evaluation (inline stack)
- Showdown cost: ~284 µs per board
- Fold cost: ~193 µs per board

**The bottleneck will not be terminal evaluation**

Performance scaling will depend on:
1. Information-set storage (CSR-like is strongly preferred)
2. Regret/strategy table locality (SoA, not AoS or nested vectors)
3. Thread-local accumulation strategy (board-local is safer than full-space duplication)
4. Reduction bandwidth (batch + deterministic)

The reach index size (~156 KiB per board) is acceptable but worth profiling once the full solver exists. At that point, profile for:
- L1/L2 miss rates
- Instructions retired per cycle
- Branch mispredictions

Current expectation: memory bandwidth, not CPU, will limit scaling at 16 threads.

---

## Suggested implementation sequence

1. Add immutable `game_graph` CSR storage **with partition metadata and estimated traversal cost**, plus validation tests.
2. Add contiguous regret/strategy table types with indexed accessors and thread-local delta-buffer interfaces.
3. Add iterative traversal stack prototype on the multithread-capable execution path (run with `threads=1` for parity and `threads>1` for scaling).
4. Integrate terminal evaluator at river leaves.
5. Add thread-local update buffers + deterministic reduction.
6. Add multithread board-partition scheduler (consumes `game_graph.partitions` directly).
7. Add solver correctness tests (small toy trees vs reference).
8. Add performance benchmarks (single vs multi-thread scaling, reduction cost).

---

## Next steps (trackable)

Status legend: `[ ]` not started, `[~]` in progress, `[x]` complete.

### Step 1 — `game_graph` foundation (next)

- [ ] **S1.1 Define immutable graph types**: `edge`, `graph_partition`, `game_graph` (CSR topology + partition metadata).
- [ ] **S1.2 Build graph constructor**: produce `row_offsets`, `edges`, `node_kind`, `infoset_id` from source tree.
- [ ] **S1.3 Build partition constructor**: compute `subtree_size`, `terminal_count`, `action_count`, depth bounds, and `estimated_work`.
- [ ] **S1.4 Add graph validation tests**: CSR offset monotonicity, edge bounds, partition coverage/non-overlap, deterministic build.
- [ ] **S1.5 Add scheduling sanity checks**: verify partition cost balance quality (cost-balanced, not node-ID balanced).
- [ ] **S1.6 Add graph benchmark hooks**: graph build time, partition build time, and read-only traversal scan throughput.

### Immediate follow-on (after Step 1)

- [ ] **S2.1 Regret/strategy table types** with contiguous storage and indexed accessors.
- [ ] **S2.2 Thread-local delta buffers** + deterministic chunk reduction API.

---

## Planned graph/scheduler refinements

### Refinement 1 — Cost metadata (first production refinement)

```cpp
struct partition {
    uint32_t begin_node;
    uint32_t end_node;
    uint64_t estimated_work;
};
```

Use work-balanced scheduling (equalize `estimated_work`), not node-count balancing.

### Refinement 2 — Depth-aware partitions

```cpp
struct partition {
    uint32_t begin_node;
    uint32_t end_node;
    uint16_t min_depth;
    uint16_t max_depth;
    uint64_t estimated_work;
};
```

This supports depth-limited and street-specific traversal policies.

### Refinement 3 — Traversal order separate from storage order

Keep CSR storage order immutable, but allow execution order overlays:

```cpp
std::span<const uint32_t> traversal_order;
```

or partition-local node order views.

### Refinement 4 — Information-set grouping

Move from node-centric traversal metadata to explicit infoset indexing:

```cpp
struct game_graph {
    // CSR topology...
    information_set_index infosets;
};
```

This is required once regret/strategy storage dominates memory.

### Refinement 5 — Persistent solver layout

Offset-based solver addressing (no hash lookups in hot path):

```cpp
struct solver_graph {
    game_graph topology;
    std::vector<uint32_t> regret_offsets;
    std::vector<uint32_t> strategy_offsets;
    std::vector<uint32_t> terminal_offsets;
};
```

### Refinement 6 — NUMA-aware partitioning (large machines)

Add NUMA placement metadata when scaling beyond a single shared-memory domain:

```cpp
struct partition {
    uint32_t begin_node;
    uint32_t end_node;
    uint16_t numa_domain;
};
```

### Refinement 7 — Dynamic scheduling overlay (not in `game_graph`)

Do not make graph mutable. Keep dynamic scheduling in a separate runtime object:

```cpp
struct work_queue {
    std::atomic<uint32_t> next_partition;
};
```

`game_graph` defines valid work units; scheduler decides execution order.

### Recommended roadmap

1. **Initial**: CSR + static partitions
2. **First production refinement**: cost-weighted partitions + depth metadata
3. **Solver integration refinement**: infoset indexing + regret/strategy offsets
4. **Large-scale refinement**: NUMA placement + dynamic scheduling overlay

---

## Test requirements

1. Regret/strategy table indexing correctness.
2. Traversal determinism (same seed/work partition => same outputs).
3. Thread-local vs single-thread parity after reduction.
4. Terminal leaf parity vs direct terminal evaluator calls.
5. Multi-iteration convergence smoke tests on tiny trees.

---

## Benchmark requirements

Track at minimum:
1. Traversals/sec (single thread and N threads)
2. Reduction time per iteration
3. Terminal evaluator share of total iteration time
4. Cache/index build time share
5. Memory footprint of graph/tables/thread-local buffers
