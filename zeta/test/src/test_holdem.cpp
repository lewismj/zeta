#include <boost/test/unit_test.hpp>

#include <initializer_list>
#include <unordered_set>
#include <utility>

#include "board.h"
#include "evaluator.h"

namespace {

    constexpr zeta::card_mask card(const int suit, const int rank) {
        return zeta::card_mask{1} << (suit * 13 + rank);
    }

    constexpr zeta::card_mask hand7(std::initializer_list<std::pair<int, int>> cards) {
        zeta::card_mask m = 0;
        for (const auto [s, r] : cards) {
            m |= card(s, r);
        }
        return m;
    }

    constexpr zeta::holdem::hand_category category_of(const zeta::holdem::hand_rank r) {
        return static_cast<zeta::holdem::hand_category>(r.value >> 24);
    }
}

BOOST_AUTO_TEST_CASE(holdem_detects_straight_flush) {
    const auto seven = hand7({
        {0, 8}, {0, 9}, {0, 10}, {0, 11}, {0, 12}, // T? J? Q? K? A?
        {1, 2}, {2, 4}
    });

    const auto rank = zeta::holdem::evaluate(seven);
    BOOST_TEST(category_of(rank) == zeta::holdem::hand_category::straight_flush);
}

BOOST_AUTO_TEST_CASE(holdem_detects_quads) {
    const auto seven = hand7({
        {0, 12}, {1, 12}, {2, 12}, {3, 12}, // quad aces
        {0, 11}, {1, 5}, {2, 3}
    });

    const auto rank = zeta::holdem::evaluate(seven);
    BOOST_TEST(category_of(rank) == zeta::holdem::hand_category::quads);
}

BOOST_AUTO_TEST_CASE(holdem_precomputed_masks_match_card_mask_path) {
    const auto seven = hand7({
        {0, 12}, {1, 12}, {2, 12}, {3, 12},
        {0, 11}, {1, 5}, {2, 3}
    });

    const auto from_card_mask = zeta::holdem::evaluate(seven);
    const auto from_masks = zeta::holdem::evaluate(zeta::holdem::suit_rank_masks(seven));
    BOOST_TEST(from_card_mask.value == from_masks.value);
}

BOOST_AUTO_TEST_CASE(holdem_ranking_ordering_is_monotonic) {
    const auto straight_flush = zeta::holdem::evaluate(hand7({
        {0, 8}, {0, 9}, {0, 10}, {0, 11}, {0, 12}, {1, 2}, {2, 4}
    }));

    const auto quads = zeta::holdem::evaluate(hand7({
        {0, 12}, {1, 12}, {2, 12}, {3, 12}, {0, 11}, {1, 5}, {2, 3}
    }));

    const auto full_house = zeta::holdem::evaluate(hand7({
        {0, 10}, {1, 10}, {2, 10}, // trip queens
        {0, 7}, {1, 7},            // pair nines
        {2, 3}, {3, 2}
    }));

    BOOST_TEST(straight_flush > quads);
    BOOST_TEST(quads > full_house);
}

BOOST_AUTO_TEST_CASE(non_flush_quinary_table_integrity) {
    const auto& dense = zeta::holdem::lookup::non_flush_table;
    BOOST_CHECK_EQUAL(dense.size(), zeta::holdem::lookup::non_flush_quinary_table_size);

    std::size_t zero_rank_count = 0;
    for (const auto rank : dense) {
        if (rank.value == 0) {
            ++zero_rank_count;
        }
    }
    BOOST_CHECK_EQUAL(zero_rank_count, 0);

    const auto seven = hand7({
        {0, 12}, {1, 12}, {2, 12}, {3, 12},
        {0, 11}, {1, 5}, {2, 3}
    });
    const auto masks = zeta::holdem::suit_rank_masks(seven);
    const auto index = zeta::holdem::non_flush_quinary_index(masks);
    BOOST_CHECK(index < dense.size());
    BOOST_CHECK_EQUAL(dense[index].value, zeta::holdem::evaluate(seven).value);
}

BOOST_AUTO_TEST_CASE(holdem_combination_masks_are_complete_and_ordered) {
    constexpr zeta::card_mask as_ah = card(0, 12) | card(1, 12);
    constexpr zeta::card_mask as_ad = card(0, 12) | card(2, 12);
    constexpr zeta::card_mask two_d_two_c = card(2, 0) | card(3, 0);

    BOOST_CHECK_EQUAL(zeta::holdem::combination_masks.size(), 1326u);
    BOOST_CHECK_EQUAL(zeta::holdem::combination_masks.front(), as_ah);
    BOOST_CHECK_EQUAL(zeta::holdem::combination_masks[1], as_ad);
    BOOST_CHECK_EQUAL(zeta::holdem::combination_masks.back(), two_d_two_c);

    std::unordered_set<zeta::card_mask> expected;
    expected.reserve(1326);
    for (int i = 0; i < 52; ++i) {
        for (int j = i + 1; j < 52; ++j) {
            expected.insert((zeta::card_mask{1} << i) | (zeta::card_mask{1} << j));
        }
    }

    std::unordered_set<zeta::card_mask> actual;
    actual.reserve(zeta::holdem::combination_masks.size());
    for (const auto mask : zeta::holdem::combination_masks) {
        BOOST_CHECK_EQUAL(zeta::ops::popcount(mask), 2);
        BOOST_CHECK(expected.contains(mask));
        actual.insert(mask);
    }

    BOOST_CHECK_EQUAL(expected.size(), 1326u);
    BOOST_CHECK_EQUAL(actual.size(), expected.size());
}
