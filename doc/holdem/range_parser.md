# PokerStove Range Parser

Zeta implements the PokerStove preflop range format for Hold'em ranges. The
parser is intentionally direct: it reads a `std::string_view` and writes
immediately into `hand_range::weights` without building an AST, using regular
expressions, or allocating parser objects.

## Entry point

```cpp
zeta::holdem::range_parse_result result = zeta::holdem::parse_range("22+,A5s+,KQo");

if (result.ok()) {
    const zeta::holdem::hand_range& range = result.range;
}
```

On failure, `range_parse_result::error` contains an error code and input
position. Parsing is exception-free.

## PokerStove style grammar

Terms are comma-separated. Whitespace is allowed around terms and separators.

| Syntax    | Meaning                                           | Combos |
| --------- | ------------------------------------------------- | ------:|
| `AA`      | Pair hand class                                   | 6      |
| `AKs`     | Suited non-pair hand class                        | 4      |
| `AKo`     | Offsuit non-pair hand class                       | 12     |
| `AK`      | Both suited and offsuit non-pair hand class       | 16     |
| `AsKh`    | Exact two-card combo                              | 1      |
| `22+`     | Pair plus: `22..AA`                               | 78     |
| `A5s+`    | Suited plus with fixed high rank: `A5s..AKs`      | 36     |
| `AJo+`    | Offsuit plus with fixed high rank: `AJo..AKo`     | 36     |
| `55-99`   | Pair range                                        | 30     |
| `A5s-A9s` | Non-pair range with same high rank and suitedness | 20     |
| `KTs-KQs` | Non-pair range with same high rank and suitedness | 12     |

If a combo appears more than once, the later term overwrites the previous
weight.

## Zeta weight extension

PokerStove's original notation is binary: a combo is either in the range or out.
Zeta adds the solver-style suffix:

```text
term:weight
```

Examples:

```text
AA:0.50
AKs:0.25
QJo+:0.75
AsKh:0.10
```

| Syntax      | Meaning                               |
| ----------- | ------------------------------------- |
| `AA:0.5`    | Pair hand class at weight `0.5`       |
| `QJo+:0.75` | Plus expression at weight `0.75`      |
| `AsKh:0.10` | Exact two-card combo at weight `0.10` |

Weights are parsed as non-negative decimal values. Percent syntax such as `50%`
is not currently implemented.

## Plus semantics

For pairs:

```text
TT+ = TT, JJ, QQ, KK, AA
```

For non-pairs, the high rank remains fixed and the low rank increases up to one
below the high rank:

```text
A5s+ = A5s, A6s, A7s, A8s, A9s, ATs, AJs, AQs, AKs
KTs+ = KTs, KJs, KQs
AJo+ = AJo, AQo, AKo
```

## Range semantics

Pair ranges expand inclusively:

```text
55-99 = 55, 66, 77, 88, 99
```

Non-pair ranges require the same high rank and the same suitedness marker:

```text
A5s-A9s = A5s, A6s, A7s, A8s, A9s
KTs-KQs = KTs, KJs, KQs
```

Exact-combo ranges, mixed suitedness ranges such as `A5s-A9o`, and ranges with
different high ranks are rejected.

## Not implemented

The parser implements the PokerStove preflop range grammar above. These
non-PokerStove extensions and broader range-language features are not currently
supported:

| Syntax                                     | Status          |
| ------------------------------------------ | --------------- |
| Parentheses / grouping                     | Not implemented |
| Boolean set operations such as `!`, `&`, ` | `, subtraction  |
| Named ranges or variables                  | Not implemented |
| Percent weights such as `AA:50%`           | Not implemented |
| Equality weights such as `AA=50%`          | Not implemented |
| Suit wildcards such as `AsKx`              | Not implemented |
| Exact combo exclusions such as `!AsKh`     | Not implemented |
| Postflop hand categories                   | Not implemented |

These can be added later without changing the core `hand_range` storage.

## Error handling

The parser returns `range_parse_result`, not exceptions:

```cpp
struct range_parse_result {
    hand_range range;
    range_parse_error error;

    bool ok() const noexcept;
};
```

`range_parse_error::position` is the byte offset in the input where parsing
failed. The parser may have written partial weights before discovering an error;
callers should only use `range` when `ok()` is true.
