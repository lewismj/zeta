#pragma once

#include <algorithm>
#include <array>
#include <bit>
#include <cassert>
#include <cstdint>
#include <ostream>
#include <limits>
#include <numeric>

#include "arch.h"


namespace zeta {

    /**
     *  BitBoard-style bitmask for sets of cards.
     *
     *  52 standard cards mapped to bits 0-51:
     *
     *  0-12 Spades (bit 0 = 2♠, bit 12 = A♠)
     *  13-25 Hearts (bit 13 = 2♥, bit 25 = A♥)
     *  26-38 Diamonds (bit 26 = 2♦, bit 38 = A♦)
     *  39-51 Clubs (bit 39 = 2♣, bit 51 = A♣)
     */
    using card_mask = uint64_t;

    /**
     * Type for the card index. 0-51 for standard cards, or special values for jokers/wildcards if needed.
     */
    using card = uint8_t;

    /**
     * Enum. For the four standard suits in a deck of cards.
     * Spades, Hearts, Diamonds, Clubs.
     */
    enum class suit: uint8_t {
        spades,
        hearts,
        diamonds,
        clubs
    };

    namespace swiss {
        /**
         *  Jass:
         *      Schellen (Bells)    = Diamonds
         *      Schilten (Shields)  = Spades
         *      Rosen (Roses)       = Hearts
         *      Eicheln (Acorns)    = Clubs
         */
        constexpr auto schilten = suit::spades;
        constexpr auto rosen = suit::hearts;
        constexpr auto schellen = suit::diamonds;
        constexpr auto eicheln = suit::clubs;
    }


    std::ostream& operator<<(std::ostream& os, const suit s);

    /**
     * Enum. Represents a maximum (and default) thirteen standard ranks in a deck of cards.
     */
    enum class rank: uint8_t {
        two,
        three,
        four,
        five,
        six,
        seven,
        eight,
        nine,
        ten,
        jack,
        queen,
        king,
        ace
    };

    /** Allow for future variants, e.g. Jass. */
    template<typename Variant> struct deck_traits;

    /** Game types, e.g. if implementing Jass. */
    struct default_deck {
        static constexpr int num_ranks = 13;
        static constexpr int num_suits = 4;
    };

    template<>
    struct deck_traits<default_deck> {
        static constexpr int num_ranks = default_deck::num_ranks;
        static constexpr int num_suits = default_deck::num_suits;
    };

    /** Number of cards in a deck, some games may include Jokers, Jass has 36 cards. */
    template<typename Variant>
    inline constexpr int num_cards = deck_traits<Variant>::num_ranks * deck_traits<Variant>::num_suits;

    /** Alias template for an array of cards for a specific variant. */
    template <typename Variant>
    using card_array = std::array<card, num_cards<Variant>>;

    template<typename Variant>
    constexpr void validate_variant() {
        static_assert(deck_traits<Variant>::num_ranks > 0, "num_ranks must be > 0");
        static_assert(deck_traits<Variant>::num_suits > 0, "num_suits must be > 0");
        static_assert(num_cards<Variant> <= 64);
        static_assert(deck_traits<Variant>::num_ranks <= 16,
            "num_ranks must be <= 16; suit_ranks/ranks_to_cards use uint16_t");
    }

    template<typename Variant>
    [[nodiscard]] constexpr card_mask rank_mask() {
        validate_variant<Variant>();

        if constexpr (deck_traits<Variant>::num_ranks == std::numeric_limits<card_mask>::digits) {
            return ~card_mask{0};
        } else {
            return (card_mask{1} << deck_traits<Variant>::num_ranks) - 1;
        }
    }

    template<typename Variant>
    [[nodiscard]] constexpr card_mask make_suit_mask(const int suit_index) {
        validate_variant<Variant>();
        assert(suit_index >= 0 && suit_index < deck_traits<Variant>::num_suits);
        const card_mask ranks = rank_mask<Variant>();

        if constexpr (deck_traits<Variant>::num_ranks == std::numeric_limits<card_mask>::digits) {
            return suit_index == 0 ? ranks : card_mask{0};
        } else {
            return ranks << (deck_traits<Variant>::num_ranks * suit_index);
        }
    }

    template<typename Variant>
    [[nodiscard]] constexpr card_mask make_suit_mask(const suit s) {
        return make_suit_mask<Variant>(static_cast<int>(s));
    }

    template<typename Variant>
    [[nodiscard]] constexpr std::array<card_mask, deck_traits<Variant>::num_suits> make_suit_masks() {
        std::array<card_mask, deck_traits<Variant>::num_suits> masks{};
        for (int i = 0; i < deck_traits<Variant>::num_suits; ++i) {
            masks[i] = make_suit_mask<Variant>(i);
        }
        return masks;
    }

  namespace ops {

        /** Generic masks for each suit, adjusted for the variant's deck size. */
        template<typename Variant>
        inline constexpr auto suit_masks = make_suit_masks<Variant>();

        /** Least significant bit in mask. */
        [[nodiscard]] constexpr card_mask lsb(const card_mask m) {
            assert(m != 0);
            return m & (~m + 1);
        }

        /** Remove and return the lowest-set bit. */
        [[nodiscard]] constexpr card_mask pop_lsb(card_mask& m) {
            card_mask b = lsb(m);
            m &= m - 1;
            return b;
        }

        /** Index of the lowest set bit (0-based). Undefined if m == 0. */
        [[nodiscard]] constexpr int lsb_index(const card_mask m) {
            assert(m);
            return std::countr_zero(m);
        }

        /** Index of the highest set bit (0-based). Undefined if m == 0. */
        [[nodiscard]] constexpr int msb_index(const card_mask m) {
            assert(m);
            return 63 - std::countl_zero(m);
        }

        /** Return the number of cards in the mask. */
        [[nodiscard]] constexpr inline_always int popcount(const card_mask m) { return std::popcount(m); }

        /** True if no cards in the mask, false otherwise. */
        [[nodiscard]] constexpr bool is_empty(const card_mask m) { return m == 0; }

        /**
         * Returns a mask containing only the cards of the specified suit present in the input mask.
         */
        template<typename Variant>
        [[nodiscard]] constexpr card_mask cards_in_suit(const card_mask m, const suit s) {
            assert(static_cast<int>(s) < deck_traits<Variant>::num_suits);
            return m & suit_masks<Variant>[static_cast<int>(s)];
        }

        /**
         * Checks if the input mask contains any cards of the specified suit.
         */
        template<typename Variant>
        [[nodiscard]] constexpr bool has_suit(const card_mask m, const suit s) {
            return !is_empty(cards_in_suit<Variant>(m, s));
        }

        /**
         * Extract the rank pattern for a given suit from a hand.
         * With PEXT this is a single instruction; otherwise shift+mask.
         * Result: bit 0 = has the lowest rank, ...
         */
        template<typename Variant>
        [[nodiscard]] inline_always uint16_t suit_ranks(const card_mask hand, const suit s) {
#ifdef USE_PEXT
            return static_cast<uint16_t>(_pext_u64(hand, suit_masks<Variant>[static_cast<int>(s)]));
#else
            return static_cast<uint16_t>((hand >> (static_cast<int>(s) * deck_traits<Variant>::num_ranks)) & rank_mask<Variant>());
#endif
        }

        /**
         * Scatter a rank pattern back into a card_mask for suit s.
         * Inverse of suit_ranks():  ranks_to_cards(suit_ranks(h,s), s) == cards_in_suit(h,s)
         * With PDEP this is a single instruction; otherwise a shift.
         */
        template<typename Variant>
        [[nodiscard]] inline_always card_mask ranks_to_cards(const uint16_t ranks, const suit s) {
#ifdef USE_PEXT
            return _pdep_u64(ranks, suit_masks<Variant>[static_cast<int>(s)]);
#else
            return (static_cast<card_mask>(ranks) & rank_mask<Variant>()) << (static_cast<int>(s) * deck_traits<Variant>::num_ranks);
#endif
        }

        /** Highest card in the given suit within mask, or 0 if none. */
        template<typename Variant>
        [[nodiscard]] constexpr card_mask highest_in_suit(const card_mask m, const suit s) {
            const card_mask suited = cards_in_suit<Variant>(m, s);
            if (suited == 0) return 0;
            return 1ull << msb_index(suited);
        }

        /** Lowest card in the given suit within mask, or 0 if none. */
        template<typename Variant>
        [[nodiscard]] constexpr card_mask lowest_in_suit(const card_mask m, const suit s) {
            const card_mask suited = cards_in_suit<Variant>(m, s);
            if (suited == 0) return 0;
            return lsb(suited);
        }

        /**
         * Return a mask containing only the n-th (0-based) set bit of m.
         * With PDEP this is a single instruction; otherwise shift+pop_lsb loop.
         */
        [[nodiscard]] inline_always card_mask nth_set_bit(card_mask m, int n) {
            assert(n >= 0 && n < popcount(m));
#ifdef USE_PEXT
            return _pdep_u64(card_mask{1} << n, m);
#else
            for (int i = 0; i < n; ++i) auto _ = pop_lsb(m);
            return lsb(m);
#endif
        }

      /** Build and shuffle a fresh card array for a given variant. */
      template <typename Variant, std::uniform_random_bit_generator T>
      [[nodiscard]] card_array<Variant> shuffled(T& rng) {
            card_array<Variant> cards;

            std::iota(cards.begin(), cards.end(), card{0});
            std::shuffle(cards.begin(), cards.end(), rng);

            return cards;
        }

    }



}
