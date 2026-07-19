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
    └── cfr/                         (CFR+ solver subsystem)
        ├── graph/
        │   ├── graph.h              (implemented)
        │   ├── graph.cpp            (implemented)
        │   ├── validation.h         (implemented)
        │   ├── validation.cpp       (implemented)
        │   ├── builder.h            (implemented)
        │   └── builder.cpp          (implemented)
        ├── scheduler/
        │   ├── dfs_partitioner.h    (implemented)
        │   ├── dfs_partitioner.cpp  (implemented)
        │   └── scheduler.*          (planned)
        ├── tables/
        │   ├── regret_table.h       (implemented)
        │   ├── strategy_table.h     (implemented)
        │   ├── table_layout.h       (implemented)
        │   └── delta_buffer.h       (implemented)
        ├── traversal/
        │   ├── traversal.h          (planned)
        │   ├── external_sampling.h  (planned)
        │   └── chance_sampling.h    (planned)
        └── solver/
            ├── cfr_plus.cpp         (planned)
            └── iteration.cpp        (planned)
```

**Later subsystem expansion:**

As the solver grows, split `cfr/` by subsystem rather than by file type:

```text
cfr/
    graph/
        graph.h
        graph.cpp
        builder.h
        builder.cpp
        validation.h
        validation.cpp

    scheduler/
        dfs_partitioner.h
        dfs_partitioner.cpp
        scheduler.h
        work_stealing.h
        numa_scheduler.h
        gpu_scheduler.h

    tables/
        table_layout.h
        regret_table.h
        strategy_table.h
        delta_buffer.h

    traversal/
        traversal.h
        external_sampling.h
        chance_sampling.h

    solver/
        cfr_plus.cpp
        iteration.cpp
```

**Benefits of dedicated folder:**

1. **Clear separation of concerns** – CFR+ is a distinct subsystem from existing terminal eval
2. **Isolation** – reduces coupling with board.h, eval.h, etc.
3. **Scalability** – easy to add more CFR+ modules (infoset indexing, value approximation, etc.)
4. **Testability** – unit tests can focus on CFR+ without polluting holdem/ test suite
5. **Dependency clarity** – CFR+ consumes terminal evaluator (unidirectional), not vice versa

**Include patterns:**

- Internal CFR+ files use subsystem paths such as `#include <holdem/cfr/graph/graph.h>`
- Solver client code uses: `#include <holdem/cfr/solver/cfr_plus.h>` once the solver entry point exists.
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

**S3.1 traversal stack prototype scope**

The prototype is the first executable traversal path over `game_graph`; it should
prove the memory layout and control flow before terminal evaluation and full CFR+
math are attached. The goal is not a feature-complete solver yet. The goal is an
allocation-free, deterministic, multithread-safe traversal kernel that can scan
player/chance/terminal nodes with explicit stack frames and write only to
thread-local scratch/update buffers.

**Performance priorities:**

1. **No heap allocation in traversal hot loops.**
   - Allocate stack, value buffers, reach scratch, child utility scratch, and
     delta buffers in the worker context before traversal begins.
   - Traversal functions should accept spans/references to preallocated storage.
   - Unit tests should include allocator instrumentation or capacity checks once
     the worker context exists.

2. **Frame size must stay cache-friendly.**
   - Keep `traversal_frame` plain-old-data, trivially copyable, and compact.
   - Prefer integer IDs and flat offsets over pointers.
   - Avoid storing spans, vectors, or owning objects in each frame.
   - Keep fields ordered to minimize padding; validate `sizeof(traversal_frame)`
     with a unit test.
   - Target 32 bytes or less initially; revisit if extra fields are justified by
     measured branch or memory savings.

3. **Worker state must be cache-line isolated.**
   - The future `worker_context` that owns the stack should be `alignas(64)`.
   - Frequently mutated per-thread counters, stack depth, local buffers, and
     deltas must not share cache lines with other workers.
   - Global immutable graph/table/cache memory is shared read-only; mutable
     traversal state is strictly thread-local.

4. **CSR access should be sequential and predictable.**
   - Iterate children using `row_offsets[node]..row_offsets[node + 1]`.
   - Avoid per-node dynamic lookup structures in the hot path.
   - Use local references/pointers to `row_offsets`, `edges`, `node_types`, and
     `infoset_id` arrays before entering the loop.
   - Consider prefetching child rows only after profiling indicates graph reads
     are a bottleneck; do not add speculative prefetch complexity blindly.

5. **Branching should be simple and stable.**
   - Dispatch on `node_kind` with a small switch or equivalent branch structure.
   - Keep terminal/player/chance handling in small internal functions only if
     inlining keeps the hot path clear.
   - Measure branch misses before introducing more complex dispatch tables.

6. **Traversal order must support bottom-up values.**
   - The current graph uses DFS post-order node IDs, so children have lower IDs
     than parents. The prototype can exploit this for bottom-up value buffers.
   - The explicit stack is still needed for execution policy, reach propagation,
     chance/player action iteration, and later sampling variants.
   - The prototype should clearly separate storage order from execution order so
     later external-sampling/chance-sampling traversals can reuse the same graph.

7. **No global writes during node visits.**
   - Regret and strategy updates go to `table_delta_buffer` or scratch buffers.
   - Reduction into global `regret_table` / `strategy_sum_table` stays outside
     the traversal loop.
   - This avoids atomics, locks, and false sharing in the main traversal path.

8. **Determinism is a correctness feature.**
   - Same graph, same initial reach values, same partition/board, same seed
     must produce identical local deltas and utility outputs.
   - Iteration order over edges must remain action-index order.
   - Floating-point reduction order should be stable inside the single-worker
     prototype.

9. **Multithread readiness must be designed into the API.**
   - The traversal entry point should take immutable shared inputs plus one
     mutable worker-local context.
   - It should not use global mutable state, hidden singletons, static scratch,
     shared RNG state, or thread-local globals that hide ownership.
   - Future parallel callers should be able to run the same traversal function
     concurrently on different contexts without synchronization.

10. **Measure before optimizing beyond layout.**
    - Initial benchmarks should capture traversal scans/sec, nodes/sec,
      edges/sec, stack max depth, bytes touched per node, branch miss rate if
      available, and allocator count.
    - Keep hooks cheap enough to compile out or disable in release hot loops.

11. **Avoid type-erased hot-path callbacks.**
    - Do not put `std::function`, virtual dispatch, or heap-owned callback state
      in the inner traversal loop.
    - Prefer template policy objects, small concrete callback structs, or
      function pointers only where profiling shows the call overhead is
      irrelevant.
    - Terminal/player/chance hooks should be replaceable without forcing dynamic
      allocation or unpredictable indirect branches per node.

12. **Keep partition and worker ownership explicit.**
    - The traversal prototype should accept an optional node/partition range or
      traversal root descriptor, even if S3.1 starts with full-root traversal.
    - Worker-local buffers belong to exactly one worker for the entire traversal.
    - Cross-worker sharing is limited to immutable graph/table/cache memory and
      deterministic post-traversal reduction.

---

## 7) Infoset graph storage (CSR-like + partition metadata)

`game_graph` is built once, then read extremely often during solving. That makes
CSR the right base layout: contiguous, compact, cache-friendly, and immutable.
Do not bolt scheduling on later; build immutable topology first, then let scheduler
modules derive explicit partition plans from it.

```cpp
struct edge {
    uint32_t child_node;
    uint16_t action_index;
};

struct graph_partition {
    uint32_t begin_node;       // inclusive
    uint32_t end_node;         // exclusive
    uint32_t node_count;
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

1. **IMPLEMENTED**: Add immutable `game_graph` CSR storage, plus validation tests.
2. **IMPLEMENTED**: Add DFS-order partition metadata and estimated traversal cost in `cfr/scheduler/dfs_partitioner.h`.
3. **IMPLEMENTED**: Add contiguous regret/strategy table types with indexed accessors.
4. **IMPLEMENTED**: Add sparse thread-local delta-buffer interfaces and deterministic reduction helpers.
5. Add iterative traversal stack prototype on the multithread-capable execution path (run with `threads=1` for parity and `threads>1` for scaling).
6. Integrate terminal evaluator at river leaves.
7. Add solver-level deterministic reduction orchestration across workers.
8. Add multithread board-partition scheduler runtime (consumes scheduler partition plans).
9. Add solver correctness tests (small toy trees vs reference).
10. Add performance benchmarks (single vs multi-thread scaling, reduction cost).

---

## Next steps (trackable)

Status legend: `[ ]` not started, `[~]` in progress, `[x]` complete.

### Implemented status snapshot

- [x] `cfr/graph/graph.h` / `cfr/graph/graph.cpp`: immutable CSR `game_graph` topology and metadata.
- [x] `cfr/graph/builder.h` / `cfr/graph/builder.cpp`: DFS post-order graph construction from mutable source-tree input.
- [x] `cfr/graph/validation.h` / `cfr/graph/validation.cpp`: graph structure, metadata, and infoset validation helpers.
- [x] `cfr/scheduler/dfs_partitioner.h` / `cfr/scheduler/dfs_partitioner.cpp`: DFS-order greedy partition metadata, estimated-work heuristic, partition validation, balance metric, unit tests, and benchmark hooks.
- [x] `cfr/tables/table_layout.h`: contiguous infoset-major action-offset layout, graph-derived layout construction, raw offset validation, and indexed offset helpers.
- [x] `cfr/tables/regret_table.h`: contiguous global regret storage with infoset/action accessors.
- [x] `cfr/tables/strategy_table.h`: contiguous global strategy-sum storage with infoset/action accessors.
- [x] `cfr/tables/delta_buffer.h`: cache-line-aligned sparse thread-local regret/strategy delta buffer and deterministic reduction helpers into global tables.
- [x] `test_cfr_graph.cpp`: focused unit tests for graph, partitions, contiguous tables, sparse delta buffers, clear/reset behavior, and reduction.

### Step 1 — `game_graph` foundation

- [x] **S1.1 Define immutable graph types**: `edge`, `game_graph` (CSR topology) and scheduler `graph_partition`.
- [x] **S1.2 Build graph constructor**: produce `row_offsets`, `edges`, `node_kind`, `infoset_id` from source tree.
- [x] **S1.3 Build DFS partitioner**: compute `node_count`, `terminal_count`, `action_count`, depth bounds, and `estimated_work`.
- [x] **S1.4 Add graph validation tests**: CSR offset monotonicity, edge bounds, partition coverage/non-overlap, deterministic build.
- [x] **S1.5 Add scheduling sanity checks**: verify partition coverage, metadata, and balance metric behavior.
- [x] **S1.6 Add graph benchmark hooks**: graph build, partition build, validation, and traversal-scan benchmark coverage.

### Step 2 — table storage and local update buffers

- [x] **S2.1 Regret/strategy table types** with contiguous storage and indexed accessors.
- [x] **S2.2 Thread-local delta buffers** + deterministic chunk reduction API.

### Step 3 — traversal prototype and worker-local execution

#### S3.1 Traversal stack prototype with allocation-free iterative traversal

- [ ] **S3.1.1 Define traversal module boundary**: add `cfr/traversal/traversal.h` for public traversal types and, if needed, `traversal.cpp` for non-template implementation. Keep the prototype independent of solver orchestration.
- [ ] **S3.1.2 Define compact `traversal_frame`**: include only fields required to resume a node visit: `node_id`, next edge/action cursor, reach values, chance weight, and a phase/state byte. Keep it trivially copyable and test `sizeof`/alignment.
- [ ] **S3.1.3 Decide frame phase model**: use explicit phases such as enter node, visit next child, reduce children, exit node. The phase model must avoid recursion and avoid re-scanning child lists unnecessarily.
- [ ] **S3.1.4 Establish maximum depth policy**: derive stack capacity from graph metadata plus a safety margin, reject graphs deeper than supported capacity, and test overflow handling without undefined behavior.
- [ ] **S3.1.5 Preallocate stack storage**: use caller-owned `std::span<traversal_frame>` or a fixed worker-context array. Do not allocate inside traversal. Do not resize vectors in the hot loop.
- [ ] **S3.1.6 Preallocate node/value scratch**: provide flat scratch buffers for per-node utility, per-action child utility, and reach propagation. Size them once from `graph.node_count`, max action count, and infoset layout.
- [ ] **S3.1.7 Define traversal input/output contract**: inputs are immutable graph/table/cache views plus worker-local mutable buffers; outputs are local utility roots, diagnostic counters, and local regret/strategy deltas only.
- [ ] **S3.1.8 Implement deterministic full-tree DFS scan**: start with traversal that visits every reachable node from `root_node`, respects action-index order, and records node/edge/terminal/player/chance counts.
- [ ] **S3.1.9 Add bottom-up value skeleton**: compute placeholder child-to-parent utility flow using preallocated buffers so later CFR math can plug in without changing stack mechanics.
- [ ] **S3.1.10 Add player-node strategy hook**: read infoset/action offsets and current regrets/strategy values through contiguous table accessors; write placeholder deltas only to `table_delta_buffer`.
- [ ] **S3.1.11 Add chance-node hook**: represent chance weighting in the frame and propagation path, but keep probability source abstract so board/card chance logic can be attached later.
- [ ] **S3.1.12 Add terminal-node hook**: record terminal visits and invoke a placeholder terminal callback interface. Do not integrate river evaluator in S3.1; that belongs to S4.1.
- [ ] **S3.1.13 Avoid global writes in traversal**: assert or review that traversal mutates only worker-local buffers and its output object. No atomics, locks, global counters, or writes to global tables inside node visits.
- [ ] **S3.1.14 Keep hot data local**: cache pointers/spans to `row_offsets`, `edges`, `node_types`, `infoset_id`, and table offsets before the loop. Avoid repeated vector member lookups where they obscure the hot path.
- [ ] **S3.1.15 Control branch shape**: use a small `node_kind` dispatch and keep per-kind handling short. Add comments only where the phase machine is non-obvious.
- [ ] **S3.1.16 Add traversal diagnostics**: collect optional counters for nodes visited, edges scanned, stack high-water mark, max action count observed, terminal count, and local delta entries touched.
- [ ] **S3.1.17 Add single-thread correctness tests**: verify visit order, node counts, stack high-water mark, action order, no stack overflow on normal graphs, overflow rejection on too-deep synthetic graphs, and deterministic outputs over repeated runs.
- [ ] **S3.1.18 Add allocation tests**: verify traversal does not allocate after context setup. If allocator instrumentation is impractical initially, test buffer capacities before/after and keep a follow-up benchmark allocator counter.
- [ ] **S3.1.19 Add multithread-readiness tests**: run the same immutable graph concurrently with multiple worker-local stacks/buffers and verify independent deterministic outputs and no shared mutable state.
- [ ] **S3.1.20 Add microbenchmarks**: measure traversal-only nodes/sec and edges/sec on small, medium, and deep synthetic graphs; include stack high-water mark and local buffer sizes in benchmark counters.
- [ ] **S3.1.21 Add perf guardrails**: document expected frame size, worker-context cache-line alignment, zero hot-loop allocations, and no global writes as non-negotiable review requirements.
- [ ] **S3.1.22 Keep sampling variants out of the prototype**: design the stack state so external-sampling and chance-sampling can reuse it, but do not implement those algorithms until the deterministic full-tree traversal is correct and measured.
- [ ] **S3.1.23 Avoid type-erased hot callbacks**: keep terminal/player/chance hooks concrete or template-driven for the prototype; no `std::function` allocation or virtual dispatch in the node loop.
- [ ] **S3.1.24 Prepare partition-aware entry points**: allow traversal inputs to name root/range/partition context so later board-level and partition-level schedulers can reuse the same kernel.
- [ ] **S3.1.25 Define done criteria**: S3.1 is complete when a deterministic full-tree traversal can run on toy graphs with preallocated stack/scratch, produce stable local diagnostics/deltas, pass single-thread and concurrent read-only tests, and report traversal benchmark numbers.

#### S3.2 Worker context

- [ ] **S3.2 Worker context** tying graph views, terminal caches, reach indices, scratch arrays, traversal stack, diagnostics, and delta buffers together.
- [ ] **S4.1 Terminal evaluator integration** at river terminal leaves.
- [ ] **S5.1 Solver-level reduction orchestration** across worker-local buffers and board batches.
- [ ] **S6.1 Runtime scheduler** for multithread board/partition execution.
- [ ] **S6.2 Scheduler strategy split** for work-stealing, NUMA-aware, and GPU scheduler modules when needed.

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
