#include <boost/test/unit_test.hpp>

#include <algorithm>
#include <array>
#include <chrono>
#include <cstdint>
#include <initializer_list>
#include <unordered_set>
#include <utility>
#include <vector>

#include "board.h"
#include "evaluator.h"
#include "range.h"
#include "range_parser.h"

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

    std::size_t non_zero_combo_count(const zeta::holdem::hand_range& range) {
        return static_cast<std::size_t>(std::count_if(range.begin(), range.end(), [](const auto weight) {
            return weight != 0.0f;
        }));
    }

    std::array<uint8_t, 13> rank_counts_from_key(const uint64_t key) {
        std::array<uint8_t, 13> counts{};
        const auto ones = static_cast<uint16_t>(key);
        const auto twos = static_cast<uint16_t>(key >> 13);
        const auto threes = static_cast<uint16_t>(key >> 26);
        const auto fours = static_cast<uint16_t>(key >> 39);
        for (std::size_t rank = 0; rank < counts.size(); ++rank) {
            const auto bit = static_cast<uint16_t>(1u << rank);
            counts[rank] = static_cast<uint8_t>(((ones & bit) != 0)
                + ((twos & bit) != 0)
                + ((threes & bit) != 0)
                + ((fours & bit) != 0));
        }
        return counts;
    }

    zeta::card_mask non_flush_mask_from_key(const uint64_t key) {
        const auto counts = rank_counts_from_key(key);
        std::array<int, 4> suit_load{};
        zeta::card_mask mask = 0;

        for (int rank = 0; rank < static_cast<int>(counts.size()); ++rank) {
            std::array<int, 4> suits{0, 1, 2, 3};
            std::sort(suits.begin(), suits.end(), [&](const int lhs, const int rhs) {
                if (suit_load[lhs] != suit_load[rhs]) {
                    return suit_load[lhs] < suit_load[rhs];
                }
                return lhs < rhs;
            });

            for (int i = 0; i < counts[rank]; ++i) {
                mask |= card(suits[i], rank);
                ++suit_load[suits[i]];
            }
        }

        return mask;
    }


    uint64_t key_from_rank_counts(const std::array<uint8_t, 13>& counts) {
        uint16_t ones = 0;
        uint16_t twos = 0;
        uint16_t threes = 0;
        uint16_t fours = 0;
        for (int rank = 0; rank < 13; ++rank) {
            const auto bit = static_cast<uint16_t>(1u << rank);
            const auto count = counts[rank];
            if (count >= 1) ones = static_cast<uint16_t>(ones | bit);
            if (count >= 2) twos = static_cast<uint16_t>(twos | bit);
            if (count >= 3) threes = static_cast<uint16_t>(threes | bit);
            if (count >= 4) fours = static_cast<uint16_t>(fours | bit);
        }
        return static_cast<uint64_t>(ones)
            | (static_cast<uint64_t>(twos) << 13)
            | (static_cast<uint64_t>(threes) << 26)
            | (static_cast<uint64_t>(fours) << 39);
    }

    template<typename Fn>
    void for_each_rank_count_class(const int rank, const int remaining, std::array<uint8_t, 13>& counts, Fn&& fn) {
        if (rank == 13) {
            if (remaining == 0) {
                fn(counts);
            }
            return;
        }

        const int max_count = std::min(4, remaining);
        for (int count = 0; count <= max_count; ++count) {
            counts[rank] = static_cast<uint8_t>(count);
            for_each_rank_count_class(rank + 1, remaining - count, counts, fn);
        }
        counts[rank] = 0;
    }

    zeta::card_mask flush_mask_from_ranks(const uint16_t ranks) {
        zeta::card_mask mask = 0;
        int cards = 0;
        for (int rank = 0; rank < 13; ++rank) {
            if ((ranks & (uint16_t{1} << rank)) != 0) {
                mask |= card(0, rank);
                ++cards;
            }
        }
        for (int rank = 0; cards < 7 && rank < 13; ++rank) {
            if ((ranks & (uint16_t{1} << rank)) == 0) {
                mask |= card(1, rank);
                ++cards;
            }
        }
        return mask;
    }

    zeta::card_mask deterministic_hand(uint64_t state) {
        zeta::card_mask mask = 0;
        while (zeta::ops::popcount(mask) < 7) {
            state ^= state << 13;
            state ^= state >> 7;
            state ^= state << 17;
            mask |= zeta::card_mask{1} << (state % 52);
        }
        return mask;
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


BOOST_AUTO_TEST_CASE(holdem_board_tracks_streets_and_mutations) {
    zeta::holdem::board b{};
    BOOST_CHECK(b.empty());
    BOOST_CHECK_EQUAL(b.size(), 0);
    BOOST_CHECK_EQUAL(static_cast<int>(b.board_street()), static_cast<int>(zeta::holdem::street::preflop));

    const auto flop = card(0, 12) | card(1, 11) | card(2, 10);
    b.add(flop);
    BOOST_CHECK(!b.empty());
    BOOST_CHECK_EQUAL(b.size(), 3);
    BOOST_CHECK_EQUAL(static_cast<int>(b.board_street()), static_cast<int>(zeta::holdem::street::flop));
    BOOST_CHECK(b.contains(12));
    BOOST_CHECK(b.contains(24));
    BOOST_CHECK(b.contains(36));

    const auto turn = card(3, 9);
    b.add(turn);
    BOOST_CHECK_EQUAL(b.size(), 4);
    BOOST_CHECK_EQUAL(static_cast<int>(b.board_street()), static_cast<int>(zeta::holdem::street::turn));

    const auto river = card(0, 0);
    b.add(river);
    BOOST_CHECK_EQUAL(b.size(), 5);
    BOOST_CHECK_EQUAL(static_cast<int>(b.board_street()), static_cast<int>(zeta::holdem::street::river));

    b.remove(turn);
    BOOST_CHECK_EQUAL(b.size(), 4);
    BOOST_CHECK_EQUAL(static_cast<int>(b.board_street()), static_cast<int>(zeta::holdem::street::turn));
    BOOST_CHECK(!b.contains(48));
}

BOOST_AUTO_TEST_CASE(holdem_combination_masks_do_not_overlap_sample_boards) {
    const zeta::holdem::board flop{card(0, 12) | card(1, 11) | card(2, 10)};
    std::size_t live_combos = 0;
    for (const auto combo : zeta::holdem::combination_masks) {
        if ((combo & flop.mask) == 0) {
            ++live_combos;
        }
    }

    BOOST_CHECK_EQUAL(live_combos, 1176u);
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

BOOST_AUTO_TEST_CASE(holdem_full_range_initializes_all_combo_weights) {
    const auto r = zeta::holdem::full_range(0.5f);

    BOOST_CHECK_EQUAL(r.weights.size(), zeta::holdem::combination_count);
    BOOST_CHECK(!r.empty());
    BOOST_CHECK_EQUAL(r[0], 0.5f);
    BOOST_CHECK_EQUAL(r[zeta::holdem::combination_count - 1], 0.5f);
    BOOST_CHECK_CLOSE(r.total_weight(), 663.0f, 0.001);
}

BOOST_AUTO_TEST_CASE(holdem_range_fill_and_accessors_cover_storage) {
    auto r = zeta::holdem::hand_range{};
    r.fill(0.25f);

    BOOST_CHECK_EQUAL(r.data(), r.weights.data());
    BOOST_CHECK_EQUAL(std::distance(r.begin(), r.end()), static_cast<std::ptrdiff_t>(zeta::holdem::combination_count));
    BOOST_CHECK_EQUAL(*r.begin(), 0.25f);
    BOOST_CHECK_EQUAL(*(r.end() - 1), 0.25f);
    BOOST_CHECK_CLOSE(r.total_weight(), 331.5f, 0.001);

    const auto& const_range = r;
    BOOST_CHECK_EQUAL(const_range.data(), r.weights.data());
    BOOST_CHECK_EQUAL(std::distance(const_range.begin(), const_range.end()), static_cast<std::ptrdiff_t>(zeta::holdem::combination_count));
}

BOOST_AUTO_TEST_CASE(holdem_range_normalize_and_scale) {
    auto r = zeta::holdem::hand_range{};
    BOOST_CHECK(r.empty());

    r[0] = 2.0f;
    r[1] = 1.0f;

    r.normalize();
    BOOST_CHECK_CLOSE(r.total_weight(), 1.0f, 0.001);
    BOOST_CHECK_CLOSE(r[0], 2.0f / 3.0f, 0.001);
    BOOST_CHECK_CLOSE(r[1], 1.0f / 3.0f, 0.001);

    r.scale(3.0f);
    BOOST_CHECK_CLOSE(r.total_weight(), 3.0f, 0.001);

    r.clear();
    BOOST_CHECK(r.empty());
    BOOST_CHECK_EQUAL(r.total_weight(), 0.0f);
}

BOOST_AUTO_TEST_CASE(holdem_range_normalize_empty_and_non_positive_is_noop) {
    auto empty = zeta::holdem::hand_range{};
    empty.normalize();
    BOOST_CHECK(empty.empty());
    BOOST_CHECK_EQUAL(empty.total_weight(), 0.0f);

    auto negative = zeta::holdem::hand_range{};
    negative[0] = -2.0f;
    negative[1] = 1.0f;
    negative.normalize();
    BOOST_CHECK_EQUAL(negative[0], -2.0f);
    BOOST_CHECK_EQUAL(negative[1], 1.0f);
    BOOST_CHECK_EQUAL(negative.total_weight(), -1.0f);
}

BOOST_AUTO_TEST_CASE(holdem_range_remove_dead_filters_blocked_combos) {
    auto r = zeta::holdem::full_range();
    const auto flop = card(0, 12) | card(1, 11) | card(2, 10);

    r.remove_dead(flop);

    std::size_t live_count = 0;
    for (zeta::holdem::combination_index i = 0; i < zeta::holdem::combination_count; ++i) {
        const bool live = zeta::holdem::is_live_combo(i, flop);
        if (live) {
            ++live_count;
            BOOST_CHECK_EQUAL(r[i], 1.0f);
        } else {
            BOOST_CHECK_EQUAL(r[i], 0.0f);
        }
    }

    BOOST_CHECK_EQUAL(live_count, 1176u);
    BOOST_CHECK_EQUAL(r.total_weight(), 1176.0f);
}

BOOST_AUTO_TEST_CASE(holdem_range_remove_dead_zero_preserves_weights) {
    auto r = zeta::holdem::full_range(0.25f);

    r.remove_dead(0);

    BOOST_CHECK(!r.empty());
    BOOST_CHECK_EQUAL(r[0], 0.25f);
    BOOST_CHECK_EQUAL(r[zeta::holdem::combination_count - 1], 0.25f);
    BOOST_CHECK_CLOSE(r.total_weight(), 331.5f, 0.001);
}

BOOST_AUTO_TEST_CASE(holdem_combination_mask_helper_matches_table) {
    BOOST_CHECK_EQUAL(zeta::holdem::combination_mask(0), zeta::holdem::combination_masks.front());
    BOOST_CHECK_EQUAL(
        zeta::holdem::combination_mask(zeta::holdem::combination_count - 1),
        zeta::holdem::combination_masks.back()
    );
    BOOST_CHECK_EQUAL(zeta::ops::popcount(zeta::holdem::combination_mask(17)), 2);
}

BOOST_AUTO_TEST_CASE(holdem_range_parser_direct_combo_index_matches_combination_table) {
    for (zeta::holdem::combination_index i = 0; i < zeta::holdem::combination_count; ++i) {
        const auto mask = zeta::holdem::combination_mask(i);
        std::array<uint8_t, 2> ranks{};
        std::array<uint8_t, 2> suits{};
        std::size_t card_count = 0;

        for (uint8_t card_index = 0; card_index < 52; ++card_index) {
            if ((mask & (zeta::card_mask{1} << card_index)) != 0) {
                BOOST_REQUIRE_LT(card_count, ranks.size());
                ranks[card_count] = static_cast<uint8_t>(card_index % 13);
                suits[card_count] = static_cast<uint8_t>(card_index / 13);
                ++card_count;
            }
        }

        BOOST_REQUIRE_EQUAL(card_count, 2u);
        BOOST_CHECK_EQUAL(zeta::holdem::detail::combo_index_from_cards(ranks[0], suits[0], ranks[1], suits[1]), i);
        BOOST_CHECK_EQUAL(zeta::holdem::detail::combo_index_from_cards(ranks[1], suits[1], ranks[0], suits[0]), i);
    }
}

BOOST_AUTO_TEST_CASE(holdem_range_parser_parses_basic_hand_classes) {
    const auto aa = zeta::holdem::parse_range("AA");
    BOOST_REQUIRE(aa.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(aa.range), 6u);
    BOOST_CHECK_EQUAL(aa.range.total_weight(), 6.0f);

    const auto aks = zeta::holdem::parse_range("AKs");
    BOOST_REQUIRE(aks.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(aks.range), 4u);
    BOOST_CHECK_EQUAL(aks.range.total_weight(), 4.0f);

    const auto ako = zeta::holdem::parse_range("AKo");
    BOOST_REQUIRE(ako.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(ako.range), 12u);
    BOOST_CHECK_EQUAL(ako.range.total_weight(), 12.0f);

    const auto ak = zeta::holdem::parse_range("AK");
    BOOST_REQUIRE(ak.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(ak.range), 16u);
    BOOST_CHECK_EQUAL(ak.range.total_weight(), 16.0f);
}

BOOST_AUTO_TEST_CASE(holdem_range_parser_parses_plus_notation) {
    const auto pairs = zeta::holdem::parse_range("22+");
    BOOST_REQUIRE(pairs.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(pairs.range), 78u);
    BOOST_CHECK_EQUAL(pairs.range.total_weight(), 78.0f);

    const auto suited_aces = zeta::holdem::parse_range("A5s+");
    BOOST_REQUIRE(suited_aces.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(suited_aces.range), 36u);
    BOOST_CHECK_EQUAL(suited_aces.range.total_weight(), 36.0f);

    const auto offsuit_aces = zeta::holdem::parse_range("AJo+");
    BOOST_REQUIRE(offsuit_aces.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(offsuit_aces.range), 36u);
    BOOST_CHECK_EQUAL(offsuit_aces.range.total_weight(), 36.0f);
}

BOOST_AUTO_TEST_CASE(holdem_range_parser_parses_dash_ranges) {
    const auto pairs = zeta::holdem::parse_range("55-99");
    BOOST_REQUIRE(pairs.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(pairs.range), 30u);
    BOOST_CHECK_EQUAL(pairs.range.total_weight(), 30.0f);

    const auto suited_aces = zeta::holdem::parse_range("A5s-A9s");
    BOOST_REQUIRE(suited_aces.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(suited_aces.range), 20u);
    BOOST_CHECK_EQUAL(suited_aces.range.total_weight(), 20.0f);

    const auto suited_kings = zeta::holdem::parse_range("KTs-KQs");
    BOOST_REQUIRE(suited_kings.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(suited_kings.range), 12u);
    BOOST_CHECK_EQUAL(suited_kings.range.total_weight(), 12.0f);
}

BOOST_AUTO_TEST_CASE(holdem_range_parser_parses_exact_combos) {
    const auto exact = zeta::holdem::parse_range("AsKh");
    BOOST_REQUIRE(exact.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(exact.range), 1u);
    BOOST_CHECK_EQUAL(exact.range.total_weight(), 1.0f);

    const auto exact_mask = card(0, 12) | card(1, 11);
    for (zeta::holdem::combination_index i = 0; i < zeta::holdem::combination_count; ++i) {
        BOOST_CHECK_EQUAL(exact.range[i], zeta::holdem::combination_mask(i) == exact_mask ? 1.0f : 0.0f);
    }
}

BOOST_AUTO_TEST_CASE(holdem_range_parser_parses_unions_whitespace_and_weights) {
    const auto parsed = zeta::holdem::parse_range(" AA, AKs:0.5, 55-66 ");
    BOOST_REQUIRE(parsed.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(parsed.range), 22u);
    BOOST_CHECK_EQUAL(parsed.range.total_weight(), 20.0f);
}

BOOST_AUTO_TEST_CASE(holdem_range_parser_overwrites_duplicate_weights) {
    const auto parsed = zeta::holdem::parse_range("AKs:0.25,AKs:0.75");
    BOOST_REQUIRE(parsed.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(parsed.range), 4u);
    BOOST_CHECK_EQUAL(parsed.range.total_weight(), 3.0f);
}

BOOST_AUTO_TEST_CASE(holdem_range_parser_reports_invalid_syntax) {
    BOOST_CHECK(!zeta::holdem::parse_range("").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("AsAs").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("AsKh+").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("A5s-A9o").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("AA:").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("AA:0.5x").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("AA,,KK").ok());
}

BOOST_AUTO_TEST_CASE(non_flush_rank_classes_exhaustively_match_dense_table_and_evaluator) {
    const auto& dense = zeta::holdem::lookup::non_flush_table;
    std::vector<uint8_t> seen(dense.size(), 0);
    std::array<uint8_t, 13> counts{};
    std::size_t classes = 0;

    for_each_rank_count_class(0, 7, counts, [&](const std::array<uint8_t, 13>& rank_counts) {
        const auto key = key_from_rank_counts(rank_counts);
        const auto reference_index = zeta::holdem::lookup::quinary_index_from_counts(rank_counts);
        const auto shared_index = zeta::holdem::lookup::quinary_index_from_key(key);
        BOOST_REQUIRE_LT(shared_index, dense.size());
        BOOST_CHECK_EQUAL(shared_index, reference_index);
        seen[shared_index] = 1;

        const auto mask = non_flush_mask_from_key(key);
        const auto masks = zeta::holdem::suit_rank_masks(mask);
        BOOST_REQUIRE_EQUAL(zeta::ops::popcount(mask), 7);
        BOOST_REQUIRE_LT(zeta::ops::popcount(masks.spades), 5);
        BOOST_REQUIRE_LT(zeta::ops::popcount(masks.hearts), 5);
        BOOST_REQUIRE_LT(zeta::ops::popcount(masks.diamonds), 5);
        BOOST_REQUIRE_LT(zeta::ops::popcount(masks.clubs), 5);
        BOOST_CHECK_EQUAL(zeta::holdem::non_flush_quinary_index(masks), shared_index);
        BOOST_CHECK_EQUAL(zeta::holdem::evaluate(mask).value, dense[shared_index].value);
        ++classes;
    });

    BOOST_CHECK_EQUAL(classes, zeta::holdem::lookup::non_flush_quinary_table_size);
    BOOST_CHECK(std::all_of(seen.begin(), seen.end(), [](const uint8_t value) { return value != 0; }));
}

BOOST_AUTO_TEST_CASE(flush_rank_patterns_match_flush_table_and_evaluator) {
    std::size_t flush_patterns = 0;
    for (uint16_t ranks = 0; ranks < zeta::holdem::lookup::flush_table.size(); ++ranks) {
        const auto suited_count = zeta::ops::popcount(ranks);
        if (suited_count < 5 || suited_count > 7) {
            continue;
        }

        const auto mask = flush_mask_from_ranks(ranks);
        const auto masks = zeta::holdem::suit_rank_masks(mask);
        BOOST_REQUIRE_EQUAL(zeta::ops::popcount(mask), 7);
        BOOST_REQUIRE_GE(zeta::ops::popcount(masks.spades), 5);
        BOOST_CHECK_EQUAL(zeta::holdem::evaluate(mask).value, zeta::holdem::lookup::flush_table[ranks].value);
        ++flush_patterns;
    }

    BOOST_CHECK_EQUAL(flush_patterns, 4719u);
}

BOOST_AUTO_TEST_CASE(holdem_evaluator_release_perf_smoke) {
    std::vector<zeta::card_mask> corpus;
    corpus.reserve(4096);
    for (uint64_t i = 1; i <= 4096; ++i) {
        corpus.push_back(deterministic_hand(i * 0x9e3779b97f4a7c15ull));
    }

    uint64_t sink = 0;
    for (const auto hand : corpus) {
        sink += zeta::holdem::evaluate(hand).value;
    }

    const auto start = std::chrono::steady_clock::now();
    for (int iteration = 0; iteration < 128; ++iteration) {
        for (const auto hand : corpus) {
            sink += zeta::holdem::evaluate(hand).value;
        }
    }
    const auto stop = std::chrono::steady_clock::now();

    const auto evaluations = static_cast<double>(corpus.size()) * 128.0;
    const auto elapsed = std::chrono::duration<double, std::nano>(stop - start).count();
    const auto ns_per_eval = elapsed / evaluations;
    BOOST_TEST_MESSAGE("holdem evaluator perf smoke: " << ns_per_eval << " ns/eval, sink " << sink);
    BOOST_CHECK_NE(sink, 0u);
#ifdef NDEBUG
    BOOST_CHECK_LT(ns_per_eval, 150.0);
#endif
}
