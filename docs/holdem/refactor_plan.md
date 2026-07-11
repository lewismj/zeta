# Hold'em Direct-Index Refactor Plan (Ambitious End State)

## Target End State

Move the evaluator to:

```cpp
7 cards -> canonical encoding -> direct index -> rank
```

Runtime goal: no hash, no probe loop, no key search.

## Why This Refactor

The old non-flush runtime path did:

- key construction
- hash
- robin-hood probe/search

The canonical state space is already known to be small (`49,205` non-flush classes), so the evaluator should become a direct state-to-rank function.

## Core Design Principles

1. **Canonical encoding over physical cards**
   - Ignore card identity and order.
   - Ignore suit names for non-flush states.
   - Preserve flush structure for flush states.

2. **Index, not key**
   - A key describes a state and requires lookup.
   - An index is already the lookup address.
   - Target: encoding should land directly in `[0..N-1]` (or near-direct with one tiny translation step during transition).

3. **Two canonical domains**
   - **Non-flush domain:** rank-multiplicity structure.
   - **Flush domain:** suited 13-bit rank mask structure.

## Proposed Runtime Architecture

Phase target API:

```cpp
hand_rank evaluate(card_mask hand) {
    auto sig = canonical_signature(hand);      // cheap extraction/classification
    uint32_t idx = direct_index(sig);          // no hash/probe
    return rank_table[idx];
}
```

Near-term split form (acceptable transitional):

```cpp
if (is_flush(sig)) return flush_rank_table[flush_index(sig)];
return non_flush_rank_table[non_flush_index(sig)];
```

## Encoding Strategy (Best Fit for Current Bitboard Style)

### Flush

- Keep current suited 13-bit mask model.
- Flush index is naturally direct (`0..8191`).
- Keep `NO_FLUSH` sentinel for branch selection.

### Non-flush

Build canonical non-flush encoding from rank multiplicities, derived from suit bitboards:

- `ones`, `twos`, `threes`, `fours` bitsets (already present conceptually).
- Convert into canonical rank-multiplicity signature.
- Replace runtime key->hash lookup with direct dense index.

Preferred progression:

1. **Transitional dense-id map (generator only):**
   - offline `key -> dense_id` for all `49,205` known non-flush keys.
   - emit `non_flush_rank[dense_id]`.
2. **Direct encoded index (runtime):**
   - runtime computes `dense_id` directly from multiplicity encoding.
   - eliminate key materialization as a semantic object.

## Implementation Phases

## Phase 0 - Baseline and Instrumentation

Add/keep component benchmarks with separate timing for:

1. suit extraction / mask build
2. flush classification
3. non-flush encoding (`index(hand)` candidate)
4. final table load
5. full evaluate(random hands)
6. full evaluate(random canonical states)

Gate all decisions on measured `cost(index(hand))`.

## Phase 1 - Stage-1 Migration Path (Completed)

Maintain current sentinel-based branch flow:

- flush via direct table index
- non-flush via key/hash/probe

Purpose: stable migration scaffold and correctness anchor.

## Phase 2 - Replace Hash Search with Dense ID (Completed)

Generator changes:

1. enumerate all non-flush canonical keys (`49,205`)
2. assign deterministic dense id `[0..N-1]`
3. emit:
   - `non_flush_rank_by_id[N]`
   - transitional `key -> id` artifact for validation/debug only

Runtime changes:

- add `non_flush_id_from_key(key)` path initially (can be binary search or minimal perfect table if needed).
- return `non_flush_rank_by_id[id]`.

Outcome: removed the robin-hood probe loop from the hot path.

## Phase 3 - Direct Non-Flush Index Encoding (Completed)

Eliminate runtime key semantics entirely:

1. define canonical multiplicity encoding contract that maps directly to dense id.
2. implement `non_flush_index(hand_masks)` returning `[0..N-1]`.
3. validate exhaustive equivalence vs Phase-2 ranks.

Outcome: non-flush path is now restricted-quinary index + dense array load.

## Phase 4 - Unified Final Rank Table (Optional Endgame)

If total canonical cardinality and memory budget permit:

- unify flush + non-flush state indices into one global dense space.
- emit single `rank_by_state[]`.
- runtime becomes:

```cpp
return rank_by_state[state_index(hand)];
```

This is the fastest conceptual endpoint.

## Generator Refactor Details

1. **Canonical contract module**
   - single shared encoder spec used by generator and runtime.
   - version stamp generated artifacts.

2. **Deterministic ordering**
   - stable dense-id assignment across builds/toolchains.
   - avoid nondeterministic container iteration for emitted ids.

3. **Artifact outputs**
   - `flush_rank_table` (existing style)
   - `non_flush_rank_by_id`
   - optional debug tables:
     - `id -> canonical descriptor`
     - `key -> id` (transitional only)

4. **Validation mode**
   - generation-time cross-checks for collisions/holes.
   - strict fail on non-bijective mappings.

## Correctness Requirements

Must preserve exact rank ordering and tie-break semantics:

- old evaluator vs each new phase bit-exact rank value equality
- edge suites:
  - all straight flush boundaries (wheel + broadway)
  - board-paired flush/full-house interactions
  - quads/full-house/trips overlap cases
  - near-flush non-flush hands
  - heavy duplicate-rank distributions

## Performance Targets

Primary metric:

`new_cost = cost(index(hand)) + state load + branch + final lookup`

Track:

- ns/eval (median/min/stddev)
- evals/sec
- component timings above

Target trend:

1. remove probe/search cost first
2. then reduce `index(hand)` until it becomes the only meaningful CPU work

## Memory Budget and Layout

For each phase, report:

1. table cardinality
2. `sizeof(entry)` and total bytes
3. alignment/padding impact
4. cache-locality expectations

Do not accept designs that trade small speed gains for impractical memory growth.

## Risk Register

1. **Index-contract drift** between generator/runtime
   - Mitigation: shared encoding codepath + version checks.
2. **Dense-id instability across builds**
   - Mitigation: deterministic sort/order guarantees.
3. **Indexing cost dominates after hash removal**
   - Mitigation: benchmark-gated phase progression; redesign encoding if needed.
4. **State-space growth beyond budget**
   - Mitigation: explicit cardinality gates in CI/tooling.

## Milestones

1. Phase 0 metrics established and recorded.
2. Phase 2 landed: no robin-hood probe loop in runtime evaluator.
3. Phase 3 landed: direct non-flush index encoding.
4. Optional Phase 4 landed: single state->rank table.
5. Legacy key/hash code removed after sustained equivalence/perf validation.

## Acceptance Criteria

1. Runtime evaluator has no hash-probe lookup in production path.
2. Rank outputs are bit-identical to baseline evaluator.
3. Benchmarks show clear ns/eval reduction with component-level attribution.
4. Table memory remains within agreed deployment budget.

## Combination Identity Layer (Adopted)

To keep canonical identity simple and stable, represent a starting hand as:

```cpp
struct combination {
    combination_index index; // 0..1325
};
```

Guidelines:

1. Use `index` as the only source of truth for hole-card combinations.
2. Keep `combination_masks[1326]` and add a tiny `combination_meta[1326]` table (`card0`, `card1`).
3. Prefer metadata lookup over repeated bit extraction from masks.
4. Provide reverse lookup `index_of(mask)` and guarantee canonical unordered mapping.
5. Expose derived accessors (`mask_of`, `is_pair`, `is_suited`, `higher_rank`, `lower_rank`) instead of leaking tables.

API guarantee:

- Caller card order is ignored.
- `Ah Ad` and `Ad Ah` map to the same canonical index.

Invariant checklist:

- `sizeof(combination) == sizeof(combination_index)`
- `index in [0,1325]`
- `popcount(mask_of(c)) == 2`
- `index_of(mask_of(combination{i})) == i`
- `mask_of(make_combination(mask)) == mask` for valid combination masks
- equality/order semantics are index-based.
