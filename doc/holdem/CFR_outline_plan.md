# CFR+ Outline Plan (Hold'em)

## Objective

Define a cache-efficient, multithread-friendly CFR+ architecture that consumes the existing river terminal evaluator (`river_terminal_cache`, `river_reach_index`, terminal value APIs).

This document focuses on:
1. Core data structures
2. Threading model
3. Update/reduction model
4. Implementation sequence

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

## 7) Infoset graph storage (CSR-like)

Avoid pointer trees. Use compact arrays:

```cpp
struct game_graph {
    std::vector<uint32_t> child_index;   // flattened child node ids
    std::vector<uint32_t> child_offset;  // size node_count + 1
    std::vector<uint8_t>  node_kind;     // player/chance/terminal
    std::vector<uint32_t> infoset_id;    // mapping for player nodes
};
```

Traversal:

```cpp
for (i = child_offset[node]; i < child_offset[node + 1]; ++i) {
    auto child = child_index[i];
}
```

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

1. Add `game_graph` CSR-like storage and validation tests.
2. Add contiguous regret/strategy table types with indexed accessors.
3. Add single-thread iterative traversal stack prototype (no threading).
4. Integrate terminal evaluator at river leaves.
5. Add thread-local update buffers + deterministic reduction.
6. Add multithread board-partition scheduler.
7. Add solver correctness tests (small toy trees vs reference).
8. Add performance benchmarks (single vs multi-thread scaling, reduction cost).

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

