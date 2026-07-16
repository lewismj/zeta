#include <boost/test/unit_test.hpp>

#include <algorithm>
#include <random>

#include "repr.h"

BOOST_AUTO_TEST_CASE(default_deck_suit_masks_have_13_cards_each) {
    constexpr auto masks = zeta::make_suit_masks<zeta::default_deck>();

    for (const auto mask : masks) {
        BOOST_TEST(zeta::ops::popcount(mask) == 13);
    }

    zeta::card_mask overlap = 0;
    for (std::size_t i = 0; i < masks.size(); ++i) {
        for (std::size_t j = i + 1; j < masks.size(); ++j) {
            overlap |= masks[i] & masks[j];
        }
    }
    BOOST_TEST(overlap == 0u);
}

BOOST_AUTO_TEST_CASE(enum_suit_mask_api_matches_index_api) {
    constexpr std::array<zeta::suit, 4> suits = {
        zeta::suit::spades, zeta::suit::hearts, zeta::suit::diamonds, zeta::suit::clubs
    };

    for (int i = 0; i < static_cast<int>(suits.size()); ++i) {
        BOOST_TEST(
            zeta::make_suit_mask<zeta::default_deck>(suits[i]) ==
            zeta::make_suit_mask<zeta::default_deck>(i)
        );
    }
}

BOOST_AUTO_TEST_CASE(num_cards_default_deck_is_52) {
    BOOST_TEST(zeta::num_cards<zeta::default_deck> == 52);
}

BOOST_AUTO_TEST_CASE(rank_mask_default_deck_is_13_low_bits) {
    constexpr auto mask = zeta::rank_mask<zeta::default_deck>();
    BOOST_TEST(mask == 0x1FFFu);
    BOOST_TEST(zeta::ops::popcount(mask) == 13);
}

BOOST_AUTO_TEST_CASE(suit_masks_cover_correct_bit_ranges) {
    /** Spades bits 0-12, Hearts 13-25, Diamonds 26-38, Clubs 39-51. */
    BOOST_TEST(zeta::make_suit_mask<zeta::default_deck>(zeta::suit::spades)   == 0x1FFFull);
    BOOST_TEST(zeta::make_suit_mask<zeta::default_deck>(zeta::suit::hearts)   == (0x1FFFull << 13));
    BOOST_TEST(zeta::make_suit_mask<zeta::default_deck>(zeta::suit::diamonds) == (0x1FFFull << 26));
    BOOST_TEST(zeta::make_suit_mask<zeta::default_deck>(zeta::suit::clubs)    == (0x1FFFull << 39));
}

BOOST_AUTO_TEST_CASE(ops_suit_masks_constexpr_matches_make_suit_masks) {
    constexpr auto from_ops  = zeta::ops::suit_masks<zeta::default_deck>;
    constexpr auto from_make = zeta::make_suit_masks<zeta::default_deck>();
    for (std::size_t i = 0; i < 4; ++i) {
        BOOST_TEST(from_ops[i] == from_make[i]);
    }
}

/** Ops primitives. */

BOOST_AUTO_TEST_CASE(lsb_returns_lowest_set_bit) {
    BOOST_TEST(zeta::ops::lsb(0b1010u) == 0b0010u);
    BOOST_TEST(zeta::ops::lsb(1u)      == 1u);
    BOOST_TEST(zeta::ops::lsb(0b1100u) == 0b0100u);
}

BOOST_AUTO_TEST_CASE(pop_lsb_removes_lowest_set_bit_and_returns_it) {
    zeta::card_mask m = 0b1010u;
    const auto bit = zeta::ops::pop_lsb(m);
    BOOST_TEST(bit == 0b0010u);
    BOOST_TEST(m   == 0b1000u);
}

BOOST_AUTO_TEST_CASE(lsb_index_returns_position_of_lowest_set_bit) {
    BOOST_TEST(zeta::ops::lsb_index(0b0001u) == 0);
    BOOST_TEST(zeta::ops::lsb_index(0b0010u) == 1);
    BOOST_TEST(zeta::ops::lsb_index(0b1010u) == 1);
    BOOST_TEST(zeta::ops::lsb_index(0b1000u) == 3);
}

BOOST_AUTO_TEST_CASE(msb_index_returns_position_of_highest_set_bit) {
    BOOST_TEST(zeta::ops::msb_index(0b0001u) == 0);
    BOOST_TEST(zeta::ops::msb_index(0b0010u) == 1);
    BOOST_TEST(zeta::ops::msb_index(0b1010u) == 3);
    BOOST_TEST(zeta::ops::msb_index(0b1000u) == 3);
}

BOOST_AUTO_TEST_CASE(is_empty_true_only_for_zero_mask) {
    BOOST_TEST( zeta::ops::is_empty(0u));
    BOOST_TEST(!zeta::ops::is_empty(1u));
    BOOST_TEST(!zeta::ops::is_empty(~zeta::card_mask{0}));
}

BOOST_AUTO_TEST_CASE(nth_set_bit_selects_correct_bit) {
    /** 0b10110 has bits 1, 2, 4 set. */
    constexpr zeta::card_mask m = 0b10110u;
    BOOST_TEST(zeta::ops::nth_set_bit(m, 0) == (zeta::card_mask{1} << 1));
    BOOST_TEST(zeta::ops::nth_set_bit(m, 1) == (zeta::card_mask{1} << 2));
    BOOST_TEST(zeta::ops::nth_set_bit(m, 2) == (zeta::card_mask{1} << 4));
}

/** Suit-aware ops. */

BOOST_AUTO_TEST_CASE(cards_in_suit_isolates_one_suit) {
    /** 2♠ = bit 0, 2♥ = bit 13. */
    constexpr zeta::card_mask hand = (zeta::card_mask{1} << 0) | (zeta::card_mask{1} << 13);

    BOOST_TEST(zeta::ops::cards_in_suit<zeta::default_deck>(hand, zeta::suit::spades)
               == (zeta::card_mask{1} << 0));
    BOOST_TEST(zeta::ops::cards_in_suit<zeta::default_deck>(hand, zeta::suit::hearts)
               == (zeta::card_mask{1} << 13));
    BOOST_TEST(zeta::ops::cards_in_suit<zeta::default_deck>(hand, zeta::suit::diamonds) == 0u);
    BOOST_TEST(zeta::ops::cards_in_suit<zeta::default_deck>(hand, zeta::suit::clubs)    == 0u);
}

BOOST_AUTO_TEST_CASE(has_suit_detects_presence_of_suit) {
    constexpr zeta::card_mask hand = zeta::card_mask{1} << 0; /**< 2♠ only. */
    BOOST_TEST( zeta::ops::has_suit<zeta::default_deck>(hand, zeta::suit::spades));
    BOOST_TEST(!zeta::ops::has_suit<zeta::default_deck>(hand, zeta::suit::hearts));
    BOOST_TEST(!zeta::ops::has_suit<zeta::default_deck>(hand, zeta::suit::diamonds));
    BOOST_TEST(!zeta::ops::has_suit<zeta::default_deck>(hand, zeta::suit::clubs));
}

BOOST_AUTO_TEST_CASE(suit_ranks_and_ranks_to_cards_roundtrip) {
    /** 2♠ (bit 0), 3♠ (bit 1), A♥ (rank 12 in hearts = bit 13+12=25). */
    constexpr zeta::card_mask hand =
        (zeta::card_mask{1} << 0)  |
        (zeta::card_mask{1} << 1)  |
        (zeta::card_mask{1} << 25);

    for (const auto s : {zeta::suit::spades, zeta::suit::hearts, zeta::suit::diamonds, zeta::suit::clubs}) {
        const auto ranks = zeta::ops::suit_ranks<zeta::default_deck>(hand, s);
        const auto back  = zeta::ops::ranks_to_cards<zeta::default_deck>(ranks, s);
        BOOST_TEST(back == zeta::ops::cards_in_suit<zeta::default_deck>(hand, s));
    }
}

BOOST_AUTO_TEST_CASE(highest_in_suit_returns_top_card_or_zero) {
    /** 2♠ (bit 0) and 3♠ (bit 1) in hand. */
    constexpr zeta::card_mask hand = (zeta::card_mask{1} << 0) | (zeta::card_mask{1} << 1);
    BOOST_TEST(zeta::ops::highest_in_suit<zeta::default_deck>(hand, zeta::suit::spades)
               == (zeta::card_mask{1} << 1));
    BOOST_TEST(zeta::ops::highest_in_suit<zeta::default_deck>(hand, zeta::suit::hearts) == 0u);
}

BOOST_AUTO_TEST_CASE(lowest_in_suit_returns_bottom_card_or_zero) {
    /** 2♠ (bit 0) and 3♠ (bit 1) in hand. */
    constexpr zeta::card_mask hand = (zeta::card_mask{1} << 0) | (zeta::card_mask{1} << 1);
    BOOST_TEST(zeta::ops::lowest_in_suit<zeta::default_deck>(hand, zeta::suit::spades)
               == (zeta::card_mask{1} << 0));
    BOOST_TEST(zeta::ops::lowest_in_suit<zeta::default_deck>(hand, zeta::suit::hearts) == 0u);
}

/** make_suit_masks size. */

BOOST_AUTO_TEST_CASE(make_suit_masks_size_matches_num_suits) {
    constexpr auto masks = zeta::make_suit_masks<zeta::default_deck>();
    static_assert(masks.size() == zeta::deck_traits<zeta::default_deck>::num_suits);
    BOOST_TEST(masks.size() == static_cast<std::size_t>(zeta::deck_traits<zeta::default_deck>::num_suits));
}

/** Jass variant: 9 ranks, 4 suits = 36 cards. */

namespace {
    struct jass_deck {
        static constexpr int num_ranks = 9;
        static constexpr int num_suits = 4;
    };
}

template<>
struct zeta::deck_traits<jass_deck> {
    static constexpr int num_ranks = jass_deck::num_ranks;
    static constexpr int num_suits = jass_deck::num_suits;
};

BOOST_AUTO_TEST_CASE(jass_deck_num_cards_is_36) {
    BOOST_TEST(zeta::num_cards<jass_deck> == 36);
}

BOOST_AUTO_TEST_CASE(jass_rank_mask_is_9_low_bits) {
    constexpr auto mask = zeta::rank_mask<jass_deck>();
    BOOST_TEST(mask == 0x1FFu);
    BOOST_TEST(zeta::ops::popcount(mask) == 9);
}

BOOST_AUTO_TEST_CASE(jass_suit_masks_have_9_cards_each_no_overlap) {
    constexpr auto masks = zeta::make_suit_masks<jass_deck>();
    static_assert(masks.size() == zeta::deck_traits<jass_deck>::num_suits);

    for (const auto mask : masks) {
        BOOST_TEST(zeta::ops::popcount(mask) == 9);
    }

    zeta::card_mask overlap = 0;
    for (std::size_t i = 0; i < masks.size(); ++i) {
        for (std::size_t j = i + 1; j < masks.size(); ++j) {
            overlap |= masks[i] & masks[j];
        }
    }
    BOOST_TEST(overlap == 0u);
}

BOOST_AUTO_TEST_CASE(jass_suit_masks_cover_correct_bit_ranges) {
    BOOST_TEST(zeta::make_suit_mask<jass_deck>(zeta::suit::spades)   == 0x1FFull);
    BOOST_TEST(zeta::make_suit_mask<jass_deck>(zeta::suit::hearts)   == (0x1FFull << 9));
    BOOST_TEST(zeta::make_suit_mask<jass_deck>(zeta::suit::diamonds) == (0x1FFull << 18));
    BOOST_TEST(zeta::make_suit_mask<jass_deck>(zeta::suit::clubs)    == (0x1FFull << 27));
}

BOOST_AUTO_TEST_CASE(jass_suit_ranks_and_ranks_to_cards_roundtrip) {
    /** 6♠ (bit 0), 7♠ (bit 1), A♥ (rank 8 in hearts = bit 9+8=17). */
    constexpr zeta::card_mask hand =
        (zeta::card_mask{1} << 0)  |
        (zeta::card_mask{1} << 1)  |
        (zeta::card_mask{1} << 17);

    for (const auto s : {zeta::suit::spades, zeta::suit::hearts, zeta::suit::diamonds, zeta::suit::clubs}) {
        const auto ranks = zeta::ops::suit_ranks<jass_deck>(hand, s);
        const auto back  = zeta::ops::ranks_to_cards<jass_deck>(ranks, s);
        BOOST_TEST(back == zeta::ops::cards_in_suit<jass_deck>(hand, s));
    }
}

/** Deck utilities. */

BOOST_AUTO_TEST_CASE(shuffled_contains_each_card_exactly_once) {
    std::mt19937 rng{42};
    const auto cards = zeta::ops::shuffled<zeta::default_deck>(rng);

    BOOST_TEST(cards.size() == 52u);

    std::array<bool, 52> seen{};
    for (const auto c : cards) {
        BOOST_TEST(c < 52u);
        BOOST_TEST(!seen[c]);
        seen[c] = true;
    }
    BOOST_TEST(std::all_of(seen.begin(), seen.end(), [](bool b) { return b; }));
}

/** Swiss aliases. */

BOOST_AUTO_TEST_CASE(swiss_aliases_map_to_correct_suits) {
    BOOST_TEST(static_cast<int>(zeta::swiss::schilten) == static_cast<int>(zeta::suit::spades));
    BOOST_TEST(static_cast<int>(zeta::swiss::rosen)    == static_cast<int>(zeta::suit::hearts));
    BOOST_TEST(static_cast<int>(zeta::swiss::schellen) == static_cast<int>(zeta::suit::diamonds));
    BOOST_TEST(static_cast<int>(zeta::swiss::eicheln)  == static_cast<int>(zeta::suit::clubs));
}
