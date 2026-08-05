# Hold'em River Terminal Evaluator

This document describes the terminal evaluator that is implemented in the
Hold'em module. It covers river showdown and fold values for ranges, the
board-specialized cache used by the kernels, and the terminal-state path used
by CFR traversal.

## Scope

The terminal evaluator operates on a fixed river board. Earlier streets are
handled by public-card chance enumeration before terminal evaluation reaches a
river state.

Implemented surfaces:

- immutable river cache construction for one board
- reach-index materialization from per-combo ranges
- dedicated heads-up evaluator with exact showdown values and summary statistics
- dedicated heads-up fold values
- exact N-way terminal-state showdown and fold values
- sampled N-way showdown value estimation
- side-pot and rake-aware payoff distribution through `terminal_state<N>`
- reusable `terminal_workspace<N>` storage for repeated solver calls

## Main Types

`river_terminal_cache` is the immutable board cache. For each of the 1326 hole
card combinations it stores:

- combo card masks
- unpacked combo cards
- evaluated rank keys for live river combinations
- a live-combo bitset
- live combinations sorted by rank

`river_reach_index` is a range-conditioned view of that cache. It stores:

- active combo indices
- per-combo weights
- total live mass
- mass by individual card for blocker correction
- rank buckets with per-bucket card mass

`terminal_context<N>` stores gross pot, rake, and per-seat contributions.
`terminal_state<N>` wraps that context with terminal kind, folded seats,
active/eligible masks, and `pot_layer<N>` entries for main-pot and side-pot
distribution.

`terminal_values<N>` stores one `value_array` per seat. Each value array is
indexed by `combination_index`.

## River Cache

`make_river_terminal_cache(board)` requires a five-card river board. It evaluates
every live hole-card combination against the board, assigns rank keys, and sorts
live combinations by rank.

Board-blocked combinations remain absent from the live rank order and receive no
active reach when an index is materialized.

## Reach Indexing

`make_river_reach_index(cache, reach)` filters a dense `reach_vector` through the
river cache and builds the active combo list. It also accumulates card-level mass
so kernels can subtract blocked opponent combos without enumerating every
opponent hand for each hero combo.

For a hero combo, compatible opponent mass is computed as:

```text
compatible = total_mass
           - first_card_mass
           - second_card_mass
           + exact_same_combo_weight
```

The final term corrects the double subtraction of the exact same two-card combo.

## Dedicated Heads-Up Evaluator

`evaluate_showdown_heads_up` is a specific heads-up evaluator, not a thin call
into the generic N-way path. It consumes two `river_reach_index` values and a
`terminal_context<2>`.

The showdown kernel sweeps rank buckets and computes, for each combo:

- lower-ranked compatible opponent mass
- equal-ranked compatible opponent mass
- higher-ranked compatible opponent mass

Those masses are multiplied by the win, tie, and loss payoffs derived from
`gross_pot`, `rake`, and the two player contributions. The result is returned as
`terminal_result<2>`, which contains both per-combo values and heads-up summary
statistics.

Heads-up fold values use the matching dedicated fold evaluator,
`evaluate_fold_values_heads_up`. The winner receives the rake-adjusted
distributed pot less their own contribution; the folded player loses their
contribution. Per-combo values are scaled by compatible opponent mass.

## Generic Fold Values

Generic N-way fold values use `terminal_state<N>` and `pot_layer<N>`. Each pot
layer is awarded only to non-folded eligible seats, and rake is distributed
proportionally across layers.

## N-Way Terminal States

`terminal_engine<N>::evaluate_terminal_values` dispatches terminal states by
kind:

- `showdown`: exact N-way terminal-state enumeration for `N > 2`
- `fold`: exact N-way fold enumeration for `N > 2`
- heads-up states use the specialized heads-up kernels

The exact N-way terminal-state showdown path recursively enumerates compatible
opponent combos for each hero combo, ranks each sampled seat, and distributes
each pot layer to the best eligible hand or tied best hands.

## Sampled N-Way Showdown Values

`evaluate_showdown_values_multiplayer_sampled` provides a scalable N-way
showdown value estimator. It samples compatible opponent combos from rank-order
strata and applies importance weighting. The caller controls sampling with
`samples_per_combo`.

This path returns `terminal_values<N>` rather than a heads-up summary. It is used
when a caller wants approximate multi-player showdown values without exhaustive
N-way enumeration.

## Workspace API

`terminal_workspace<N>` owns the materialized reach indices for repeated
evaluations. Solver code can keep one workspace per worker, materialize incoming
ranges into it, and evaluate terminals without per-call heap allocation for the
large reach-index arrays.

## CFR Integration

CFR traversal consumes terminal values through `cfr_terminal_provider<N>`.
Terminal leaves reference entries in a terminal-state table, and the provider
binds those leaves to:

- the river cache
- per-seat reach indices
- terminal states
- selected combo indices for the current traversal

At terminal leaves, the solver evaluates the referenced terminal state and reads
the current updating player's combo value from `terminal_values<N>`.

## Limits

The implemented terminal evaluator is river-only. Turn and flop solving reaches
this evaluator through chance enumeration that produces river boards.

`terminal_result<N>` summary statistics are specialized for heads-up only.
N-way callers receive per-seat value arrays through `terminal_values<N>`.
