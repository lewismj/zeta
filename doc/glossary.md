# Glossary and Math Notes

This glossary defines the terms, data layouts, and small mathematical ideas used
so far in Zeta's card and Hold'em evaluator code.

## Card and bit-mask representation

### Card

A `card` is a small integer index into a deck. For the current standard 52-card
deck, valid card indices are `0..51`.

### Card mask

A `card_mask` is a 64-bit integer used as a bit set of cards. Bit `i` is set
when card `i` is present.

For the standard deck:

```text
bit = suit * 13 + rank

bits  0..12  spades   (2s..As)
bits 13..25  hearts   (2h..Ah)
bits 26..38  diamonds (2d..Ad)
bits 39..51  clubs    (2c..Ac)
```

This makes many set operations single machine instructions:

| Operation | Meaning |
|---|---|
| `a | b` | Union of two card sets. |
| `a & b` | Intersection of two card sets. |
| `a & ~b` | Cards in `a` excluding cards in `b`. |
| `popcount(mask)` | Number of cards in the set. |

### Suit-rank mask

A suit-rank mask is a 13-bit value for one suit. Bit `r` is set if that suit
contains rank `r`.

Example:

```text
spades = 0b1000000000001
```

means ace of spades and two of spades are present.

The evaluator builds four such masks per 7-card hand:

```cpp
hand_masks {
    uint16_t spades;
    uint16_t hearts;
    uint16_t diamonds;
    uint16_t clubs;
};
```

## Hold'em combinations and ranges

### Combination

A Hold'em private hand, or combo, is an unordered pair of two distinct cards.
There are:

```text
C(52, 2) = 52! / (2! * 50!) = 1326
```

possible two-card combinations.

Zeta stores these in `combination_masks`, where each entry is a `card_mask` with
exactly two bits set. A `combination_index` is a `uint16_t` index into this table.

### Range

A range is a weight for each of the 1326 private-hand combinations:

```cpp
std::array<float, 1326> weights;
```

Weight `0.0f` means the combo is absent. Positive weights represent relative
frequency or probability mass. The current `hand_range` is intentionally simple:
contiguous storage, no dynamic allocation, and direct indexed access.

### Dead cards and live combos

A dead-card mask contains cards that cannot be used by a private hand, for
example board cards or known blockers.

A combo is live if it does not overlap dead cards:

```cpp
(combination_masks[i] & dead) == 0
```

Removing dead combos from a range means setting blocked combo weights to zero.

## 7-card evaluator terms

### 21-subset enumeration

A naive 7-card evaluator can evaluate all 5-card subsets of a 7-card hand:

```text
C(7, 5) = 21
```

Zeta avoids this runtime search. It uses lookup tables for both flush and
non-flush hands.

### Flush path

A flush exists if any suit-rank mask has at least five bits set:

```cpp
popcount(suit_ranks) >= 5
```

For flush and straight-flush hands, the 13-bit suited rank mask directly indexes
`flush_table`.

### Non-flush path

Non-flush hands are evaluated by rank multiplicities only. Suits no longer
matter once a flush has been ruled out.

For each rank, the evaluator needs to know whether it appears once, twice, three
times, or four times across the four suits.

## Rank-count layers

### Threshold layers

The evaluator represents rank multiplicities with four 13-bit layers:

| Layer | Bit is set when rank count is... |
|---|---|
| `ones` | `>= 1` |
| `twos` | `>= 2` |
| `threes` | `>= 3` |
| `fours` | `>= 4` |

For a rank with count `3`, the corresponding bit is set in `ones`, `twos`, and
`threes`, but not `fours`.

The exact count can be reconstructed as:

```cpp
count(rank) =
      bit(ones, rank)
    + bit(twos, rank)
    + bit(threes, rank)
    + bit(fours, rank);
```

### Carry-save-style layer builder

The runtime evaluator derives the four layers from four suit-rank masks using a
small threshold network:

```cpp
pair01_single = s0 ^ s1;
pair23_single = s2 ^ s3;
pair01_double = s0 & s1;
pair23_double = s2 & s3;

split_pairs = pair01_single & pair23_single;
fours       = pair01_double & pair23_double;
twos        = pair01_double | pair23_double | split_pairs;
threes      = (pair01_double & pair23_single)
            | (pair23_double & pair01_single)
            | fours;
ones        = pair01_single | pair23_single | twos;
```

This is "carry-save-style" in the sense that it combines two pairs of one-bit
inputs without first building all pairwise and triple intersections. It computes
the same threshold layers as the direct formulas:

```text
twos   = any pair of suits has the rank
threes = any three suits have the rank
fours  = all four suits have the rank
```

but with fewer wide Boolean terms in the hot path.

## Canonical non-flush key

The four rank-count layers can be packed into a 64-bit key:

```cpp
key = uint64_t(ones)
    | (uint64_t(twos)   << 13)
    | (uint64_t(threes) << 26)
    | (uint64_t(fours)  << 39);
```

Layout:

```text
bits  0..12  ones
bits 13..25  twos
bits 26..38  threes
bits 39..51  fours
bits 52..63  unused
```

This key is canonical for non-flush hands because it ignores suit identity and
keeps only rank multiplicities.

## Quinary rank-count encoding

### Why base 5?

Each rank can appear `0..4` times in a 52-card deck. That is five possible
values, so a rank-count vector is naturally represented as base-5, or quinary,
digits.

A 7-card hand has 13 rank counts:

```text
[c0, c1, c2, ..., c12]
```

where:

```text
0 <= ci <= 4
sum(ci) = 7
```

The `sum(ci) = 7` condition is the restricted part: most base-5 13-digit
patterns are impossible for a 7-card hand.

### Restricted quinary perfect index

The evaluator maps every valid 7-card rank-count vector to a dense table index:

```text
0..49204
```

There are exactly 49,205 valid 13-rank count vectors whose digits are `0..4` and
whose sum is `7`.

The dense index is perfect: every valid non-flush rank-count class maps to one
slot, and every slot is used. There are no empty buckets, stored keys, hash
collisions, or probe loops.

### Dynamic-programming index idea

The rank-count index can be viewed as a combinatorial ranking problem: "How many
valid count vectors come before this one?"

At each rank:

```cpp
index += quinary_dp[count][remaining_ranks][remaining_cards];
remaining_cards -= count;
```

The DP table stores how many suffixes are possible for smaller choices at the
current rank.

## Chunked quinary indexing

The runtime hot path avoids a 13-rank dependent loop by splitting ranks into
chunks:

```text
4 ranks + 4 ranks + 5 ranks
```

Each chunk produces:

1. the dense-index contribution for that chunk
2. the number of cards consumed by that chunk

Those are packed into one 32-bit value:

```text
bits  0..23  dense-index contribution
bits 24..31  cards used by chunk
```

The low 24 bits are selected with:

```cpp
packed & 0x00ff'ffff
```

The high byte is read with:

```cpp
packed >> 24
```

### Pair-weight tables

Within each chunk, the evaluator converts threshold-layer bit fields into
quinary chunk codes. It combines `ones/twos` and `threes/fours` with small
pair-weight tables.

For a 4-rank chunk:

```text
2^8 = 256 entries
```

For a 5-rank chunk:

```text
2^10 = 1024 entries
```

This replaces four separate weight lookups with two pair lookups plus one
addition per chunk.

## Table sizes

| Table | Meaning | Size |
|---|---|---:|
| `flush_table` | 13-bit flush rank-mask lookup | 8192 entries |
| `non_flush_table` | Dense restricted-quinary rank lookup | 49205 entries |
| `quinary_chunk0` | First 4-rank chunk index table | `8 * 625` entries |
| `quinary_chunk1` | Second 4-rank chunk index table | `8 * 625` entries |
| `quinary_chunk2` | Final 5-rank chunk index table | `8 * 3125` entries |

The current evaluator table data is about 337 KiB.

## Benchmark terms

### ns/eval

Nanoseconds per 7-card evaluation. Lower is better.

### items_per_second

Google Benchmark throughput counter. For evaluator benchmarks, items are usually
hands evaluated. For dense table scans, items are table slots or lookups.

### Exhaustive 52 choose 7

The benchmark named `BM_EvaluateAllSevenCards` evaluates every 7-card hand:

```text
C(52, 7) = 133,784,560
```

This is useful because it removes sampling bias from random hand generation and
exercises all evaluator paths in their real frequencies.

## CFR and solver terms planned next

These terms are not fully implemented yet, but they explain the roadmap language
used in the README.

### Action tree

A tree of legal betting decisions from a game state. Nodes are player actions,
chance events, or terminal outcomes.

### CFR

Counterfactual Regret Minimization. CFR is an iterative method for approximating
equilibrium strategies in imperfect-information games.

### CFR+

A CFR variant that clips cumulative regrets at zero and often converges faster
in practice.

### Regret

The amount by which a player would have preferred one action over the actions
actually taken by the current strategy, measured counterfactually.

### Average strategy

The time-weighted average of strategies visited during CFR iterations. This is
usually the strategy exported or evaluated for exploitability.

### Exploitability

A measure of how much a perfect opponent could gain against a strategy compared
with an equilibrium strategy. Lower exploitability means closer to equilibrium.
