# Core Structures

The core library provides the generic card and bit-mask primitives used by game-specific modules such as Texas Hold'em.

## Card representation

Cards are represented as bit positions inside a `card_mask`:

```cpp
using card_mask = uint64_t;
using card = uint8_t;
```

For the default 52-card deck, cards are laid out by contiguous suit blocks:

| Bits | Suit | Ranks |
|---:|---|---|
| 0-12 | Spades | 2 through Ace |
| 13-25 | Hearts | 2 through Ace |
| 26-38 | Diamonds | 2 through Ace |
| 39-51 | Clubs | 2 through Ace |

This layout makes suit extraction cheap: for the default deck, a suit's 13-bit rank mask is a shift and mask.

## Deck traits

Deck variants are described through `deck_traits<Variant>`:

```cpp
struct default_deck {
    static constexpr int num_ranks = 13;
    static constexpr int num_suits = 4;
};
```

The core code is written against the variant trait interface rather than hard-coding the default deck everywhere. Variants must fit in a 64-bit `card_mask`, and suit-rank extraction currently assumes at most 16 ranks per suit.

## Suits and ranks

The core library defines the standard suits and ranks:

```cpp
enum class suit : uint8_t {
    spades,
    hearts,
    diamonds,
    clubs
};

enum class rank : uint8_t {
    two, three, four, five, six, seven, eight,
    nine, ten, jack, queen, king, ace
};
```

The Swiss aliases map Jass suit names onto the same underlying suit enum.

## Bit operations

`zeta::ops` contains the low-level operations used by game engines:

| Operation | Purpose |
|---|---|
| `popcount(mask)` | Count cards in a mask |
| `lsb(mask)` / `pop_lsb(mask)` | Iterate set cards |
| `lsb_index(mask)` / `msb_index(mask)` | Find card indices |
| `cards_in_suit<Variant>(mask, suit)` | Filter cards by suit |
| `suit_ranks<Variant>(mask, suit)` | Convert suited cards to a compact rank mask |
| `ranks_to_cards<Variant>(ranks, suit)` | Scatter a rank mask back into card bits |
| `nth_set_bit(mask, n)` | Select the nth card in a mask |

`suit_ranks` and `ranks_to_cards` can use BMI2 `PEXT`/`PDEP` when enabled, but the default 52-card contiguous suit layout is already efficient with shift-and-mask operations.

## Design intent

The core layer is intentionally small:

- represent cards and sets of cards
- support deck variants through traits
- provide fast, reusable bit operations
- leave game rules, hand evaluation, betting, and solving logic to higher-level modules

This keeps game-specific code, such as the Hold'em evaluator and future postflop solver, focused on poker semantics while sharing the same card identity and mask operations.
