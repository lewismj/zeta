#include <boost/test/unit_test.hpp>

#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <functional>
#include <initializer_list>
#include <memory>
#include <string>
#include <unordered_set>
#include <utility>
#include <vector>

#include "board.h"
#include "evaluator.h"
#include "range.h"
#include "range_parser.h"
#include "terminal.h"

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

    constexpr char rank_char(const uint8_t rank) {
        constexpr std::array<char, 13> chars{'2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'};
        return chars[rank];
    }

    constexpr char suit_char(const uint8_t suit) {
        constexpr std::array<char, 4> chars{'s', 'h', 'd', 'c'};
        return chars[suit];
    }

    std::string hand_class_text(const uint8_t high_rank, const uint8_t low_rank, const char mode = '\0') {
        std::string text;
        text.push_back(rank_char(high_rank));
        text.push_back(rank_char(low_rank));
        if (mode != '\0') {
            text.push_back(mode);
        }
        return text;
    }

    std::string exact_combo_text(
        const uint8_t first_rank,
        const uint8_t first_suit,
        const uint8_t second_rank,
        const uint8_t second_suit
    ) {
        std::string text;
        text.push_back(rank_char(first_rank));
        text.push_back(suit_char(first_suit));
        text.push_back(rank_char(second_rank));
        text.push_back(suit_char(second_suit));
        return text;
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

    zeta::holdem::board deterministic_river_board() {
        return zeta::holdem::board{
            card(0, 12) | card(1, 11) | card(2, 10) | card(3, 9) | card(0, 0)
        };
    }

    zeta::holdem::accumulator slow_compatible_mass(
        const zeta::holdem::river_terminal_cache& cache,
        const zeta::holdem::river_reach_index& opponent,
        const zeta::holdem::combination_index hero_combo
    ) {
        zeta::holdem::accumulator total = 0.0;
        const auto hero_mask = cache.masks[hero_combo];
        for (std::uint16_t offset = 0; offset < opponent.active_count; ++offset) {
            const auto opponent_combo = opponent.active_indices[offset];
            if ((hero_mask & cache.masks[opponent_combo]) == 0) {
                total += opponent.weights[opponent_combo];
            }
        }
        return total;
    }

    zeta::holdem::combination_index combo_index_from_mask(const zeta::card_mask mask) {
        for (zeta::holdem::combination_index i = 0; i < zeta::holdem::combination_count; ++i) {
            if (zeta::holdem::combination_mask(i) == mask) {
                return i;
            }
        }
        BOOST_FAIL("combo mask not found");
        return 0;
    }

    zeta::holdem::terminal_result<2> reference_showdown(
        const zeta::holdem::river_terminal_cache& cache,
        const zeta::holdem::reach_vector& oop_reach,
        const zeta::holdem::reach_vector& ip_reach,
        const zeta::holdem::terminal_context<2>& context
    ) {
        zeta::holdem::terminal_result<2> result{};
        const auto pot = zeta::holdem::heads_up_pot(context);
        const auto oop_win = zeta::holdem::payoff_for_oop_win(pot);
        const auto ip_win = zeta::holdem::payoff_for_ip_win(pot);
        const auto tie = zeta::holdem::payoff_for_tie(pot);

        for (zeta::holdem::combination_index oi = 0; oi < zeta::holdem::combination_count; ++oi) {
            const auto oop_weight = oop_reach[oi];
            if (oop_weight <= 0.0f || !zeta::holdem::combo_live(cache.live, oi)) {
                continue;
            }

            for (zeta::holdem::combination_index ii = 0; ii < zeta::holdem::combination_count; ++ii) {
                const auto ip_weight = ip_reach[ii];
                if (ip_weight <= 0.0f || !zeta::holdem::combo_live(cache.live, ii)) {
                    continue;
                }
                if ((cache.masks[oi] & cache.masks[ii]) != 0) {
                    continue;
                }

                zeta::holdem::terminal_payoff payoff{};
                if (cache.rank_keys[oi] > cache.rank_keys[ii]) {
                    payoff = oop_win;
                    result.summary.oop_wins += static_cast<double>(oop_weight) * ip_weight;
                } else if (cache.rank_keys[ii] > cache.rank_keys[oi]) {
                    payoff = ip_win;
                    result.summary.ip_wins += static_cast<double>(oop_weight) * ip_weight;
                } else {
                    payoff = tie;
                    result.summary.ties += static_cast<double>(oop_weight) * ip_weight;
                }

                result.values[zeta::holdem::player::oop][oi] += static_cast<zeta::holdem::terminal_value>(ip_weight * payoff.oop);
                result.values[zeta::holdem::player::ip][ii] += static_cast<zeta::holdem::terminal_value>(oop_weight * payoff.ip);
                result.summary.matchup_weight += static_cast<double>(oop_weight) * ip_weight;
                result.summary.oop_ev += static_cast<double>(oop_weight) * ip_weight * payoff.oop;
                result.summary.ip_ev += static_cast<double>(oop_weight) * ip_weight * payoff.ip;
            }
        }

        return result;
    }

    void check_close_abs(const double actual, const double expected, const double tolerance = 1.0e-4) {
        BOOST_CHECK_SMALL(actual - expected, tolerance);
    }

    uint64_t xorshift64(uint64_t& state) {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        return state;
    }

    zeta::holdem::board random_river_board(uint64_t& state) {
        zeta::card_mask mask = 0;
        while (zeta::ops::popcount(mask) < 5) {
            mask |= zeta::card_mask{1} << (xorshift64(state) % 52);
        }
        return zeta::holdem::board{mask};
    }

    zeta::holdem::reach_vector random_sparse_reach(
        const zeta::holdem::river_terminal_cache& cache,
        uint64_t& state
    ) {
        zeta::holdem::reach_vector reach{};
        for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
            const auto combo = cache.rank_order[order];
            if ((xorshift64(state) % 100) < 18) {
                reach[combo] = static_cast<float>((xorshift64(state) % 9) + 1) * 0.125f;
            }
        }
        if (std::none_of(reach.weights.begin(), reach.weights.end(), [](const float w) { return w > 0.0f; })) {
            reach[cache.rank_order[xorshift64(state) % cache.rank_order_count]] = 1.0f;
        }
        return reach;
    }

    std::pair<zeta::holdem::combination_index, zeta::holdem::combination_index> find_disjoint_pair(
        const zeta::holdem::river_terminal_cache& cache,
        const std::function<bool(zeta::holdem::rank_key, zeta::holdem::rank_key)>& predicate
    ) {
        for (std::size_t oo = 0; oo < cache.rank_order_count; ++oo) {
            const auto oop_combo = cache.rank_order[oo];
            for (std::size_t ii = 0; ii < cache.rank_order_count; ++ii) {
                const auto ip_combo = cache.rank_order[ii];
                if ((cache.masks[oop_combo] & cache.masks[ip_combo]) != 0) {
                    continue;
                }
                if (predicate(cache.rank_keys[oop_combo], cache.rank_keys[ip_combo])) {
                    return {oop_combo, ip_combo};
                }
            }
        }
        BOOST_FAIL("failed to find disjoint combo pair for showdown test");
        return {0, 0};
    }

    void check_result_matches_reference(
        const zeta::holdem::terminal_result<2>& actual,
        const zeta::holdem::terminal_result<2>& expected,
        const double value_tolerance = 0.01,
        const double summary_tolerance = 0.05
    ) {
        for (zeta::holdem::combination_index i = 0; i < zeta::holdem::combination_count; ++i) {
            check_close_abs(actual.values[zeta::holdem::player::oop][i], expected.values[zeta::holdem::player::oop][i], value_tolerance);
            check_close_abs(actual.values[zeta::holdem::player::ip][i], expected.values[zeta::holdem::player::ip][i], value_tolerance);
        }
        check_close_abs(actual.summary.oop_ev, expected.summary.oop_ev, summary_tolerance);
        check_close_abs(actual.summary.ip_ev, expected.summary.ip_ev, summary_tolerance);
        check_close_abs(actual.summary.matchup_weight, expected.summary.matchup_weight, summary_tolerance);
        check_close_abs(actual.summary.oop_wins, expected.summary.oop_wins, summary_tolerance);
        check_close_abs(actual.summary.ip_wins, expected.summary.ip_wins, summary_tolerance);
        check_close_abs(actual.summary.ties, expected.summary.ties, summary_tolerance);
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

BOOST_AUTO_TEST_CASE(holdem_terminal_payoffs_use_gross_pot_less_rake) {
    const zeta::holdem::terminal_pot zero_sum{
        .gross_pot = 100.0,
        .rake = 0.0,
        .oop_contribution = 50.0,
        .ip_contribution = 50.0
    };

    auto payoff = zeta::holdem::payoff_for_oop_win(zero_sum);
    BOOST_CHECK_EQUAL(payoff.oop, 50.0);
    BOOST_CHECK_EQUAL(payoff.ip, -50.0);

    payoff = zeta::holdem::payoff_for_ip_win(zero_sum);
    BOOST_CHECK_EQUAL(payoff.oop, -50.0);
    BOOST_CHECK_EQUAL(payoff.ip, 50.0);

    payoff = zeta::holdem::payoff_for_tie(zero_sum);
    BOOST_CHECK_EQUAL(payoff.oop, 0.0);
    BOOST_CHECK_EQUAL(payoff.ip, 0.0);

    payoff = zeta::holdem::payoff_for_fold(zero_sum, zeta::holdem::player::ip);
    BOOST_CHECK_EQUAL(payoff.oop, 50.0);
    BOOST_CHECK_EQUAL(payoff.ip, -50.0);

    payoff = zeta::holdem::payoff_for_fold(zero_sum, zeta::holdem::player::oop);
    BOOST_CHECK_EQUAL(payoff.oop, -50.0);
    BOOST_CHECK_EQUAL(payoff.ip, 50.0);

    const zeta::holdem::terminal_pot subgame{
        .gross_pot = 300.0,
        .rake = 15.0,
        .oop_contribution = 100.0,
        .ip_contribution = 100.0
    };

    payoff = zeta::holdem::payoff_for_oop_win(subgame);
    BOOST_CHECK_EQUAL(payoff.oop, 185.0);
    BOOST_CHECK_EQUAL(payoff.ip, -100.0);

    payoff = zeta::holdem::payoff_for_ip_win(subgame);
    BOOST_CHECK_EQUAL(payoff.oop, -100.0);
    BOOST_CHECK_EQUAL(payoff.ip, 185.0);

    payoff = zeta::holdem::payoff_for_tie(subgame);
    BOOST_CHECK_EQUAL(payoff.oop, 42.5);
    BOOST_CHECK_EQUAL(payoff.ip, 42.5);
}

BOOST_AUTO_TEST_CASE(holdem_terminal_reach_vector_copies_hand_range_weights) {
    auto range = zeta::holdem::hand_range{};
    range[0] = 0.25f;
    range[7] = 0.75f;

    auto reach = zeta::holdem::make_reach_vector(range);
    BOOST_CHECK_EQUAL(reach[0], 0.25f);
    BOOST_CHECK_EQUAL(reach[7], 0.75f);

    reach[0] = 1.0f;
    BOOST_CHECK_EQUAL(range[0], 0.25f);
    BOOST_CHECK_EQUAL(reach[zeta::holdem::combination_count - 1], 0.0f);
}

BOOST_AUTO_TEST_CASE(holdem_river_terminal_cache_builds_live_rank_order) {
    const auto river = deterministic_river_board();
    const auto cache = zeta::holdem::make_river_terminal_cache(river);

    BOOST_CHECK_EQUAL(cache.board_hash, static_cast<uint64_t>(river.mask));
    BOOST_CHECK_EQUAL(cache.river_board.mask, river.mask);
    BOOST_CHECK_EQUAL(cache.rank_order_count, 1081u);

    std::size_t live_count = 0;
    std::array<bool, zeta::holdem::combination_count> seen{};
    for (zeta::holdem::combination_index i = 0; i < zeta::holdem::combination_count; ++i) {
        const auto combo = zeta::holdem::combination_mask(i);
        const bool live = (combo & river.mask) == 0;
        BOOST_CHECK_EQUAL(cache.masks[i], combo);
        BOOST_CHECK_EQUAL(zeta::holdem::combo_live(cache.live, i), live);
        BOOST_CHECK_EQUAL(
            (zeta::card_mask{1} << cache.cards[i].first) | (zeta::card_mask{1} << cache.cards[i].second),
            combo
        );

        if (!live) {
            BOOST_CHECK_EQUAL(cache.rank_keys[i], 0u);
            continue;
        }

        ++live_count;
        BOOST_REQUIRE(cache.rank_keys[i] > 0);
        BOOST_REQUIRE(cache.rank_keys[i] <= cache.unique_rank_count);
        BOOST_CHECK_EQUAL(cache.unique_ranks[cache.rank_keys[i]].value, zeta::holdem::evaluate(river.mask | combo).value);
    }

    BOOST_CHECK_EQUAL(live_count, 1081u);

    for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
        const auto combo_index = cache.rank_order[order];
        BOOST_REQUIRE(combo_index < zeta::holdem::combination_count);
        BOOST_CHECK(!seen[combo_index]);
        seen[combo_index] = true;
        BOOST_CHECK(zeta::holdem::combo_live(cache.live, combo_index));

        if (order == 0) {
            continue;
        }

        const auto previous = cache.rank_order[order - 1];
        const auto previous_rank = cache.unique_ranks[cache.rank_keys[previous]];
        const auto current_rank = cache.unique_ranks[cache.rank_keys[combo_index]];
        BOOST_CHECK(previous_rank <= current_rank);
        if (previous_rank == current_rank) {
            BOOST_CHECK(previous < combo_index);
        }
    }
}

BOOST_AUTO_TEST_CASE(holdem_river_reach_index_builds_sparse_rank_buckets) {
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    auto reach = zeta::holdem::reach_vector{};

    for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
        const auto combo_index = cache.rank_order[order];
        if (combo_index % 5 == 0) {
            reach[combo_index] = -1.0f;
        } else if (combo_index % 7 != 0) {
            reach[combo_index] = static_cast<zeta::holdem::combo_weight>((combo_index % 11) + 1) * 0.125f;
        }
    }

    for (zeta::holdem::combination_index i = 0; i < zeta::holdem::combination_count; ++i) {
        if (!zeta::holdem::combo_live(cache.live, i)) {
            reach[i] = 10.0f;
            break;
        }
    }

    const auto index = zeta::holdem::make_river_reach_index(cache, reach);
    BOOST_CHECK_EQUAL(index.board_hash, cache.board_hash);
    BOOST_CHECK(index.bucket_card_mass_count <= index.active_count * 2u);

    zeta::holdem::accumulator expected_total = 0.0;
    std::array<zeta::holdem::accumulator, 52> expected_mass_by_card{};
    std::vector<zeta::holdem::combination_index> expected_active;

    for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
        const auto combo_index = cache.rank_order[order];
        const auto weight = reach[combo_index];
        if (weight <= 0.0f) {
            continue;
        }

        expected_active.push_back(combo_index);
        expected_total += weight;
        const auto [first, second] = cache.cards[combo_index];
        expected_mass_by_card[first] += weight;
        expected_mass_by_card[second] += weight;
    }

    BOOST_CHECK_EQUAL(index.active_count, expected_active.size());
    BOOST_CHECK_CLOSE(index.total_live_mass, static_cast<float>(expected_total), 0.001);
    for (std::size_t offset = 0; offset < expected_active.size(); ++offset) {
        const auto combo_index = expected_active[offset];
        BOOST_CHECK_EQUAL(index.active_indices[offset], combo_index);
        BOOST_CHECK_EQUAL(index.weights[combo_index], reach[combo_index]);
    }

    for (std::size_t card_index = 0; card_index < expected_mass_by_card.size(); ++card_index) {
        BOOST_CHECK_CLOSE(index.mass_by_card[card_index], static_cast<float>(expected_mass_by_card[card_index]), 0.001);
    }

    std::size_t covered = 0;
    for (std::uint16_t bucket_index = 0; bucket_index < index.unique_rank_count; ++bucket_index) {
        const auto& bucket = index.rank_buckets[bucket_index];
        BOOST_REQUIRE(bucket.begin < bucket.end);
        BOOST_CHECK_EQUAL(bucket.begin, covered);
        covered = bucket.end;

        zeta::holdem::accumulator bucket_total = 0.0;
        std::array<zeta::holdem::accumulator, 52> bucket_cards{};
        for (std::uint16_t offset = bucket.begin; offset < bucket.end; ++offset) {
            const auto combo_index = index.active_indices[offset];
            BOOST_CHECK_EQUAL(cache.rank_keys[combo_index], bucket.rank);
            bucket_total += index.weights[combo_index];
            const auto [first, second] = cache.cards[combo_index];
            bucket_cards[first] += index.weights[combo_index];
            bucket_cards[second] += index.weights[combo_index];
        }

        BOOST_CHECK_CLOSE(bucket.total_mass, static_cast<float>(bucket_total), 0.001);
        std::array<zeta::holdem::accumulator, 52> sparse_cards{};
        for (std::uint16_t offset = bucket.card_mass_begin; offset < bucket.card_mass_end; ++offset) {
            const auto entry = index.bucket_card_masses[offset];
            sparse_cards[entry.card] += entry.mass;
        }
        for (std::size_t card_index = 0; card_index < bucket_cards.size(); ++card_index) {
            BOOST_CHECK_CLOSE(static_cast<float>(sparse_cards[card_index]), static_cast<float>(bucket_cards[card_index]), 0.001);
            BOOST_CHECK_CLOSE(
                static_cast<float>(zeta::holdem::bucket_card_mass(index, bucket, static_cast<uint8_t>(card_index))),
                static_cast<float>(bucket_cards[card_index]),
                0.001
            );
        }
    }
    BOOST_CHECK_EQUAL(covered, index.active_count);

    for (zeta::holdem::combination_index i = 0; i < zeta::holdem::combination_count; ++i) {
        if (!zeta::holdem::combo_live(cache.live, i)) {
            continue;
        }
        BOOST_CHECK_CLOSE(
            zeta::holdem::compatible_reach_mass(cache, index, i),
            slow_compatible_mass(cache, index, i),
            0.001
        );
    }
}

BOOST_AUTO_TEST_CASE(holdem_river_reach_index_preserves_cache_rank_order) {
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    auto reach = zeta::holdem::reach_vector{};

    for (zeta::holdem::combination_index i = 0; i < zeta::holdem::combination_count; ++i) {
        if (!zeta::holdem::combo_live(cache.live, i)) {
            continue;
        }
        reach[i] = (i % 2 == 0) ? 0.0f : static_cast<zeta::holdem::combo_weight>((i % 13) + 1) * 0.1f;
    }

    const auto index = zeta::holdem::make_river_reach_index(cache, reach);
    BOOST_CHECK_EQUAL(index.board_hash, cache.board_hash);

    std::vector<zeta::holdem::combination_index> expected_active;
    expected_active.reserve(cache.rank_order_count);
    for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
        const auto combo_index = cache.rank_order[order];
        if (reach[combo_index] > 0.0f) {
            expected_active.push_back(combo_index);
        }
    }

    BOOST_REQUIRE_EQUAL(index.active_count, expected_active.size());
    for (std::size_t offset = 0; offset < expected_active.size(); ++offset) {
        BOOST_CHECK_EQUAL(index.active_indices[offset], expected_active[offset]);
    }
}

BOOST_AUTO_TEST_CASE(holdem_terminal_fold_values_respect_card_removal) {
    const zeta::holdem::board river{
        card(1, 0) | card(2, 1) | card(3, 2) | card(1, 3) | card(2, 7)
    };
    const auto cache = zeta::holdem::make_river_terminal_cache(river);

    const auto as_ks = combo_index_from_mask(card(0, 12) | card(0, 11));
    const auto as_qs = combo_index_from_mask(card(0, 12) | card(0, 10));
    const auto six_h_seven_d = combo_index_from_mask(card(1, 4) | card(2, 5));

    auto oop = zeta::holdem::reach_vector{};
    auto ip = zeta::holdem::reach_vector{};
    oop[as_ks] = 1.0f;
    ip[as_qs] = 1.0f;
    ip[six_h_seven_d] = 1.0f;

    const auto context = zeta::holdem::make_heads_up_context(100.0, 0.0, 50.0, 50.0);

    const auto values = zeta::holdem::evaluate_fold_values(
        cache,
        oop,
        ip,
        context,
        zeta::holdem::player::ip
    );

    BOOST_CHECK_EQUAL(values[zeta::holdem::player::oop][as_ks], 50.0f);
    BOOST_CHECK_EQUAL(values[zeta::holdem::player::ip][as_qs], 0.0f);
    BOOST_CHECK_EQUAL(values[zeta::holdem::player::ip][six_h_seven_d], -50.0f);
}

BOOST_AUTO_TEST_CASE(holdem_terminal_fold_values_handle_board_blockers_and_both_folded_players) {
    const zeta::holdem::board river{
        card(1, 0) | card(2, 1) | card(3, 2) | card(1, 3) | card(2, 7)
    };
    const auto cache = zeta::holdem::make_river_terminal_cache(river);

    const auto as_ks = combo_index_from_mask(card(0, 12) | card(0, 11));
    const auto board_blocked_oop = combo_index_from_mask(card(1, 0) | card(0, 4));
    const auto as_qs = combo_index_from_mask(card(0, 12) | card(0, 10));
    const auto six_h_seven_d = combo_index_from_mask(card(1, 4) | card(2, 5));
    const auto board_blocked_ip = combo_index_from_mask(card(1, 0) | card(3, 8));

    auto oop = zeta::holdem::reach_vector{};
    auto ip = zeta::holdem::reach_vector{};
    oop[as_ks] = 1.0f;
    oop[board_blocked_oop] = 1.0f;
    ip[as_qs] = 1.0f;
    ip[six_h_seven_d] = 1.0f;
    ip[board_blocked_ip] = 1.0f;

    const auto context = zeta::holdem::make_heads_up_context(100.0, 0.0, 50.0, 50.0);

    const auto oop_index = zeta::holdem::make_river_reach_index(cache, oop);
    const auto ip_index = zeta::holdem::make_river_reach_index(cache, ip);
    const auto ip_folds = zeta::holdem::evaluate_fold_values(
        cache,
        oop_index,
        ip_index,
        context,
        zeta::holdem::player::ip
    );
    const auto oop_folds = zeta::holdem::evaluate_fold_values(
        cache,
        oop_index,
        ip_index,
        context,
        zeta::holdem::player::oop
    );

    BOOST_CHECK_EQUAL(
        ip_folds[zeta::holdem::player::oop][as_ks],
        static_cast<zeta::holdem::terminal_value>(
            slow_compatible_mass(cache, ip_index, as_ks) * zeta::holdem::payoff_for_fold(zeta::holdem::heads_up_pot(context), zeta::holdem::player::ip).oop
        )
    );
    BOOST_CHECK_EQUAL(ip_folds[zeta::holdem::player::oop][board_blocked_oop], 0.0f);
    BOOST_CHECK_EQUAL(ip_folds[zeta::holdem::player::ip][board_blocked_ip], 0.0f);
    BOOST_CHECK_EQUAL(ip_folds[zeta::holdem::player::ip][as_qs], 0.0f);
    BOOST_CHECK_EQUAL(ip_folds[zeta::holdem::player::ip][six_h_seven_d], -50.0f);

    BOOST_CHECK_EQUAL(oop_folds[zeta::holdem::player::oop][as_ks], -50.0f);
    BOOST_CHECK_EQUAL(oop_folds[zeta::holdem::player::ip][six_h_seven_d], 50.0f);
}

BOOST_AUTO_TEST_CASE(holdem_terminal_showdown_matches_reference_oracle) {
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    auto oop = zeta::holdem::reach_vector{};
    auto ip = zeta::holdem::reach_vector{};

    for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
        const auto combo_index = cache.rank_order[order];
        if (combo_index % 3 != 0) {
            oop[combo_index] = static_cast<float>((combo_index % 5) + 1) * 0.2f;
        }
        if (combo_index % 4 != 0) {
            ip[combo_index] = static_cast<float>((combo_index % 7) + 1) * 0.15f;
        }
    }

    const auto context = zeta::holdem::make_heads_up_context(300.0, 15.0, 100.0, 100.0);

    const auto actual = zeta::holdem::evaluate_showdown(cache, oop, ip, context);
    const auto expected = reference_showdown(cache, oop, ip, context);
    check_result_matches_reference(actual, expected, 0.01, 0.1);
    check_close_abs(
        actual.summary.oop_ev + actual.summary.ip_ev,
        actual.summary.matchup_weight * (
            context.gross_pot
            - context.rake
            - context.contribution[0]
            - context.contribution[1]
        ),
        0.1
    );
}

BOOST_AUTO_TEST_CASE(holdem_terminal_showdown_single_combo_win_loss_tie) {
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    const auto context = zeta::holdem::make_heads_up_context(100.0, 0.0, 50.0, 50.0);
    const auto oop_win = zeta::holdem::payoff_for_oop_win(zeta::holdem::heads_up_pot(context));
    const auto ip_win = zeta::holdem::payoff_for_ip_win(zeta::holdem::heads_up_pot(context));
    const auto tie = zeta::holdem::payoff_for_tie(zeta::holdem::heads_up_pot(context));

    const auto [oop_wins_combo, ip_loses_combo] = find_disjoint_pair(
        cache,
        [](const auto oop_rank, const auto ip_rank) { return oop_rank > ip_rank; }
    );
    const auto [oop_loses_combo, ip_wins_combo] = find_disjoint_pair(
        cache,
        [](const auto oop_rank, const auto ip_rank) { return oop_rank < ip_rank; }
    );
    const auto [oop_tie_combo, ip_tie_combo] = find_disjoint_pair(
        cache,
        [](const auto oop_rank, const auto ip_rank) { return oop_rank == ip_rank; }
    );

    auto oop = zeta::holdem::reach_vector{};
    auto ip = zeta::holdem::reach_vector{};
    oop[oop_wins_combo] = 1.0f;
    ip[ip_loses_combo] = 1.0f;
    auto win_case = zeta::holdem::evaluate_showdown(cache, oop, ip, context);
    BOOST_CHECK_EQUAL(win_case.values[zeta::holdem::player::oop][oop_wins_combo], static_cast<float>(oop_win.oop));
    BOOST_CHECK_EQUAL(win_case.values[zeta::holdem::player::ip][ip_loses_combo], static_cast<float>(oop_win.ip));
    BOOST_CHECK_EQUAL(win_case.summary.oop_wins, 1.0);
    BOOST_CHECK_EQUAL(win_case.summary.ip_wins, 0.0);
    BOOST_CHECK_EQUAL(win_case.summary.ties, 0.0);

    oop = {};
    ip = {};
    oop[oop_loses_combo] = 1.0f;
    ip[ip_wins_combo] = 1.0f;
    auto loss_case = zeta::holdem::evaluate_showdown(cache, oop, ip, context);
    BOOST_CHECK_EQUAL(loss_case.values[zeta::holdem::player::oop][oop_loses_combo], static_cast<float>(ip_win.oop));
    BOOST_CHECK_EQUAL(loss_case.values[zeta::holdem::player::ip][ip_wins_combo], static_cast<float>(ip_win.ip));
    BOOST_CHECK_EQUAL(loss_case.summary.oop_wins, 0.0);
    BOOST_CHECK_EQUAL(loss_case.summary.ip_wins, 1.0);
    BOOST_CHECK_EQUAL(loss_case.summary.ties, 0.0);

    oop = {};
    ip = {};
    oop[oop_tie_combo] = 1.0f;
    ip[ip_tie_combo] = 1.0f;
    auto tie_case = zeta::holdem::evaluate_showdown(cache, oop, ip, context);
    BOOST_CHECK_EQUAL(tie_case.values[zeta::holdem::player::oop][oop_tie_combo], static_cast<float>(tie.oop));
    BOOST_CHECK_EQUAL(tie_case.values[zeta::holdem::player::ip][ip_tie_combo], static_cast<float>(tie.ip));
    BOOST_CHECK_EQUAL(tie_case.summary.oop_wins, 0.0);
    BOOST_CHECK_EQUAL(tie_case.summary.ip_wins, 0.0);
    BOOST_CHECK_EQUAL(tie_case.summary.ties, 1.0);
}

BOOST_AUTO_TEST_CASE(holdem_terminal_showdown_blocker_excludes_overlapping_matchups) {
    const zeta::holdem::board river{
        card(1, 0) | card(2, 1) | card(3, 2) | card(1, 3) | card(2, 7)
    };
    const auto cache = zeta::holdem::make_river_terminal_cache(river);
    const auto as_ks = combo_index_from_mask(card(0, 12) | card(0, 11));
    const auto as_qs = combo_index_from_mask(card(0, 12) | card(0, 10));
    const auto six_h_seven_d = combo_index_from_mask(card(1, 4) | card(2, 5));

    auto oop = zeta::holdem::reach_vector{};
    auto ip = zeta::holdem::reach_vector{};
    oop[as_ks] = 1.0f;
    ip[as_qs] = 1.0f;
    ip[six_h_seven_d] = 1.0f;

    const auto context = zeta::holdem::make_heads_up_context(120.0, 0.0, 60.0, 60.0);

    const auto actual = zeta::holdem::evaluate_showdown(cache, oop, ip, context);
    const auto expected = reference_showdown(cache, oop, ip, context);
    check_result_matches_reference(actual, expected, 0.001, 0.001);
    BOOST_CHECK_EQUAL(actual.values[zeta::holdem::player::ip][as_qs], 0.0f);
}

BOOST_AUTO_TEST_CASE(holdem_terminal_showdown_board_blocked_combos_contribute_nothing) {
    const zeta::holdem::board river{
        card(1, 0) | card(2, 1) | card(3, 2) | card(1, 3) | card(2, 7)
    };
    const auto cache = zeta::holdem::make_river_terminal_cache(river);

    const auto board_blocked_oop = combo_index_from_mask(card(1, 0) | card(0, 4));
    const auto board_blocked_ip = combo_index_from_mask(card(2, 1) | card(3, 8));
    const auto oop_live = combo_index_from_mask(card(0, 12) | card(0, 11));
    const auto ip_live = combo_index_from_mask(card(1, 4) | card(2, 5));

    auto oop = zeta::holdem::reach_vector{};
    auto ip = zeta::holdem::reach_vector{};
    oop[board_blocked_oop] = 10.0f;
    oop[oop_live] = 1.0f;
    ip[board_blocked_ip] = 11.0f;
    ip[ip_live] = 1.0f;

    const auto context = zeta::holdem::make_heads_up_context(100.0, 0.0, 50.0, 50.0);

    const auto actual = zeta::holdem::evaluate_showdown(cache, oop, ip, context);
    const auto expected = reference_showdown(cache, oop, ip, context);
    check_result_matches_reference(actual, expected, 0.001, 0.001);
    BOOST_CHECK_EQUAL(actual.values[zeta::holdem::player::oop][board_blocked_oop], 0.0f);
    BOOST_CHECK_EQUAL(actual.values[zeta::holdem::player::ip][board_blocked_ip], 0.0f);
}

BOOST_AUTO_TEST_CASE(holdem_terminal_showdown_random_reference_regression) {
    uint64_t state = 0x53a9d2b4b8c671efULL;
    for (int board_iter = 0; board_iter < 20; ++board_iter) {
        const auto board = random_river_board(state);
        const auto cache = zeta::holdem::make_river_terminal_cache(board);

        for (int reach_iter = 0; reach_iter < 20; ++reach_iter) {
            const auto oop = random_sparse_reach(cache, state);
            const auto ip = random_sparse_reach(cache, state);
            const auto context = zeta::holdem::make_heads_up_context(150.0 + static_cast<double>(xorshift64(state) % 250), static_cast<double>(xorshift64(state) % 30), static_cast<double>(50 + (xorshift64(state) % 60)), static_cast<double>(50 + (xorshift64(state) % 60)));
            if ((context.gross_pot - context.rake) < (context.contribution[0] + context.contribution[1])) {
                continue;
            }

            const auto actual = zeta::holdem::evaluate_showdown(cache, oop, ip, context);
            const auto expected = reference_showdown(cache, oop, ip, context);
            check_result_matches_reference(actual, expected, 0.01, 0.1);
        }
    }
}

BOOST_AUTO_TEST_CASE(holdem_terminal_showdown_pathological_boards_match_reference_oracle) {
    const std::array boards{
        zeta::holdem::board{card(0, 12) | card(0, 11) | card(0, 10) | card(0, 9) | card(0, 8)},
        zeta::holdem::board{card(0, 12) | card(1, 12) | card(2, 12) | card(3, 12) | card(0, 0)},
        zeta::holdem::board{card(0, 0) | card(1, 1) | card(2, 2) | card(3, 3) | card(0, 4)},
        zeta::holdem::board{card(0, 12) | card(0, 11) | card(0, 10) | card(1, 9) | card(2, 9)},
        zeta::holdem::board{card(0, 12) | card(1, 12) | card(0, 11) | card(1, 11) | card(2, 0)},
        zeta::holdem::board{card(0, 7) | card(0, 6) | card(0, 5) | card(0, 0) | card(1, 12)},
        zeta::holdem::board{card(0, 12) | card(1, 11) | card(2, 10) | card(3, 9) | card(0, 8)},
        zeta::holdem::board{card(1, 12) | card(1, 11) | card(1, 10) | card(1, 9) | card(1, 8)}
    };

    uint64_t state = 0x92d6b42ca25d57f1ULL;
    for (const auto& board : boards) {
        const auto cache = zeta::holdem::make_river_terminal_cache(board);
        const auto oop = random_sparse_reach(cache, state);
        const auto ip = random_sparse_reach(cache, state);
        const auto context = zeta::holdem::make_heads_up_context(250.0, 10.0, 100.0, 100.0);

        const auto actual = zeta::holdem::evaluate_showdown(cache, oop, ip, context);
        const auto expected = reference_showdown(cache, oop, ip, context);
        check_result_matches_reference(actual, expected, 0.01, 0.1);
    }
}

BOOST_AUTO_TEST_CASE(holdem_terminal_showdown_is_deterministic_for_cache_index_and_result) {
    const auto board = deterministic_river_board();
    auto cache_a = std::make_unique<zeta::holdem::river_terminal_cache>(zeta::holdem::make_river_terminal_cache(board));
    auto cache_b = std::make_unique<zeta::holdem::river_terminal_cache>(zeta::holdem::make_river_terminal_cache(board));
    BOOST_CHECK_EQUAL(cache_a->board_hash, cache_b->board_hash);
    BOOST_CHECK_EQUAL(cache_a->rank_order_count, cache_b->rank_order_count);
    BOOST_CHECK_EQUAL(cache_a->unique_rank_count, cache_b->unique_rank_count);
    for (std::size_t i = 0; i < cache_a->rank_order_count; ++i) {
        BOOST_CHECK_EQUAL(cache_a->rank_order[i], cache_b->rank_order[i]);
    }

    auto oop = zeta::holdem::reach_vector{};
    auto ip = zeta::holdem::reach_vector{};
    std::size_t assigned = 0;
    for (std::size_t i = 0; i < cache_a->rank_order_count && assigned < 8; ++i) {
        const auto combo = cache_a->rank_order[i];
        oop[combo] = static_cast<float>((i % 3) + 1) * 0.5f;
        ip[combo] = static_cast<float>((i % 4) + 1) * 0.25f;
        ++assigned;
    }
    BOOST_REQUIRE(assigned > 0);

    const auto context = zeta::holdem::make_heads_up_context(300.0, 15.0, 100.0, 100.0);
    const auto result_a = zeta::holdem::evaluate_showdown(*cache_a, oop, ip, context);
    const auto result_b = zeta::holdem::evaluate_showdown(*cache_b, oop, ip, context);
    check_result_matches_reference(result_a, result_b, 0.0, 0.0);
}

BOOST_AUTO_TEST_CASE(holdem_terminal_showdown_summary_and_values_are_consistent) {
    auto cache = std::make_unique<zeta::holdem::river_terminal_cache>(
        zeta::holdem::make_river_terminal_cache(deterministic_river_board())
    );
    auto oop = zeta::holdem::reach_vector{};
    auto ip = zeta::holdem::reach_vector{};

    for (std::size_t order = 0; order < cache->rank_order_count; ++order) {
        const auto combo = cache->rank_order[order];
        if (combo % 5 == 1) {
            oop[combo] = static_cast<float>((combo % 4) + 1) * 0.25f;
        }
        if (combo % 6 == 2) {
            ip[combo] = static_cast<float>((combo % 3) + 1) * 0.4f;
        }
    }

    const auto context = zeta::holdem::make_heads_up_context(300.0, 15.0, 100.0, 100.0);

    const auto result = zeta::holdem::evaluate_showdown(*cache, oop, ip, context);
    const auto values = zeta::holdem::evaluate_showdown_values(*cache, oop, ip, context);
    const auto summary = zeta::holdem::summarize_showdown(*cache, oop, ip, context);

    for (zeta::holdem::combination_index i = 0; i < zeta::holdem::combination_count; ++i) {
        BOOST_CHECK_EQUAL(values[zeta::holdem::player::oop][i], result.values[zeta::holdem::player::oop][i]);
        BOOST_CHECK_EQUAL(values[zeta::holdem::player::ip][i], result.values[zeta::holdem::player::ip][i]);
    }

    check_close_abs(summary.oop_ev, result.summary.oop_ev, 1.0e-9);
    check_close_abs(summary.ip_ev, result.summary.ip_ev, 1.0e-9);
    check_close_abs(summary.matchup_weight, result.summary.matchup_weight, 1.0e-9);
    check_close_abs(summary.oop_wins, result.summary.oop_wins, 1.0e-9);
    check_close_abs(summary.ip_wins, result.summary.ip_wins, 1.0e-9);
    check_close_abs(summary.ties, result.summary.ties, 1.0e-9);

    zeta::holdem::accumulator oop_dot = 0.0;
    zeta::holdem::accumulator ip_dot = 0.0;
    for (zeta::holdem::combination_index i = 0; i < zeta::holdem::combination_count; ++i) {
        oop_dot += static_cast<double>(oop[i]) * result.values[zeta::holdem::player::oop][i];
        ip_dot += static_cast<double>(ip[i]) * result.values[zeta::holdem::player::ip][i];
    }

    check_close_abs(oop_dot, result.summary.oop_ev, 0.1);
    check_close_abs(ip_dot, result.summary.ip_ev, 0.1);
    check_close_abs(
        result.summary.oop_ev + result.summary.ip_ev,
        result.summary.matchup_weight * (
            context.gross_pot
            - context.rake
            - context.contribution[0]
            - context.contribution[1]
        ),
        0.1
    );
}

BOOST_AUTO_TEST_CASE(holdem_terminal_showdown_array_api_matches_index_api) {
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    auto oop = zeta::holdem::reach_vector{};
    auto ip = zeta::holdem::reach_vector{};
    std::size_t assigned = 0;
    for (std::size_t order = 0; order < cache.rank_order_count && assigned < 24; ++order) {
        const auto combo = cache.rank_order[order];
        oop[combo] = static_cast<float>((order % 3) + 1) * 0.5f;
        ip[combo] = static_cast<float>((order % 4) + 1) * 0.25f;
        ++assigned;
    }
    BOOST_REQUIRE(assigned > 0);

    const auto context = zeta::holdem::make_heads_up_context(300.0, 15.0, 100.0, 100.0);
    const auto oop_index = zeta::holdem::make_river_reach_index(cache, oop);
    const auto ip_index = zeta::holdem::make_river_reach_index(cache, ip);

    const auto expected = zeta::holdem::evaluate_showdown(cache, oop_index, ip_index, context);

    const std::array<zeta::holdem::river_reach_index, 2> reach{oop_index, ip_index};
    const auto actual = zeta::holdem::evaluate_showdown<2>(cache, reach, context);

    for (zeta::holdem::combination_index i = 0; i < zeta::holdem::combination_count; ++i) {
        BOOST_CHECK_EQUAL(actual.values[zeta::holdem::player::oop][i], expected.values[zeta::holdem::player::oop][i]);
        BOOST_CHECK_EQUAL(actual.values[zeta::holdem::player::ip][i], expected.values[zeta::holdem::player::ip][i]);
    }
    BOOST_CHECK_EQUAL(actual.summary.oop_ev, expected.summary.oop_ev);
    BOOST_CHECK_EQUAL(actual.summary.ip_ev, expected.summary.ip_ev);
    BOOST_CHECK_EQUAL(actual.summary.matchup_weight, expected.summary.matchup_weight);

    // Seat-indexed access on terminal_values<N> must agree with player-enum access.
    for (zeta::holdem::combination_index i = 0; i < zeta::holdem::combination_count; ++i) {
        BOOST_CHECK_EQUAL(actual.values[std::size_t{0}][i], actual.values[zeta::holdem::player::oop][i]);
        BOOST_CHECK_EQUAL(actual.values[std::size_t{1}][i], actual.values[zeta::holdem::player::ip][i]);
    }
}

BOOST_AUTO_TEST_CASE(holdem_terminal_fold_array_api_matches_index_api) {
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    auto oop = zeta::holdem::reach_vector{};
    auto ip = zeta::holdem::reach_vector{};
    std::size_t assigned = 0;
    for (std::size_t order = 0; order < cache.rank_order_count && assigned < 24; ++order) {
        const auto combo = cache.rank_order[order];
        oop[combo] = static_cast<float>((order % 3) + 1) * 0.5f;
        ip[combo] = static_cast<float>((order % 4) + 1) * 0.25f;
        ++assigned;
    }
    BOOST_REQUIRE(assigned > 0);

    const auto context = zeta::holdem::make_heads_up_context(100.0, 0.0, 50.0, 50.0);
    const auto oop_index = zeta::holdem::make_river_reach_index(cache, oop);
    const auto ip_index = zeta::holdem::make_river_reach_index(cache, ip);
    const std::array<zeta::holdem::river_reach_index, 2> reach{oop_index, ip_index};

    for (const auto folded : {zeta::holdem::player::oop, zeta::holdem::player::ip}) {
        const auto expected = zeta::holdem::evaluate_fold_values(cache, oop_index, ip_index, context, folded);
        const auto actual = zeta::holdem::evaluate_fold_values<2>(cache, reach, context, folded);
        for (zeta::holdem::combination_index i = 0; i < zeta::holdem::combination_count; ++i) {
            BOOST_CHECK_EQUAL(actual[zeta::holdem::player::oop][i], expected[zeta::holdem::player::oop][i]);
            BOOST_CHECK_EQUAL(actual[zeta::holdem::player::ip][i], expected[zeta::holdem::player::ip][i]);
        }
    }
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

BOOST_AUTO_TEST_CASE(holdem_range_parser_all_hand_class_forms_have_expected_counts) {
    for (uint8_t rank = 0; rank < 13; ++rank) {
        const auto parsed = zeta::holdem::parse_range(hand_class_text(rank, rank));
        BOOST_REQUIRE_MESSAGE(parsed.ok(), hand_class_text(rank, rank));
        BOOST_CHECK_EQUAL(non_zero_combo_count(parsed.range), 6u);
        BOOST_CHECK_EQUAL(parsed.range.total_weight(), 6.0f);
    }

    for (uint8_t high = 1; high < 13; ++high) {
        for (uint8_t low = 0; low < high; ++low) {
            const auto both = zeta::holdem::parse_range(hand_class_text(high, low));
            BOOST_REQUIRE_MESSAGE(both.ok(), hand_class_text(high, low));
            BOOST_CHECK_EQUAL(non_zero_combo_count(both.range), 16u);
            BOOST_CHECK_EQUAL(both.range.total_weight(), 16.0f);

            const auto suited = zeta::holdem::parse_range(hand_class_text(high, low, 's'));
            BOOST_REQUIRE_MESSAGE(suited.ok(), hand_class_text(high, low, 's'));
            BOOST_CHECK_EQUAL(non_zero_combo_count(suited.range), 4u);
            BOOST_CHECK_EQUAL(suited.range.total_weight(), 4.0f);

            const auto offsuit = zeta::holdem::parse_range(hand_class_text(high, low, 'o'));
            BOOST_REQUIRE_MESSAGE(offsuit.ok(), hand_class_text(high, low, 'o'));
            BOOST_CHECK_EQUAL(non_zero_combo_count(offsuit.range), 12u);
            BOOST_CHECK_EQUAL(offsuit.range.total_weight(), 12.0f);
        }
    }
}

BOOST_AUTO_TEST_CASE(holdem_range_parser_all_exact_combos_map_to_one_combo) {
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

        const auto forward = zeta::holdem::parse_range(exact_combo_text(ranks[0], suits[0], ranks[1], suits[1]));
        BOOST_REQUIRE(forward.ok());
        BOOST_CHECK_EQUAL(non_zero_combo_count(forward.range), 1u);
        BOOST_CHECK_EQUAL(forward.range[i], 1.0f);

        const auto reverse = zeta::holdem::parse_range(exact_combo_text(ranks[1], suits[1], ranks[0], suits[0]));
        BOOST_REQUIRE(reverse.ok());
        BOOST_CHECK_EQUAL(non_zero_combo_count(reverse.range), 1u);
        BOOST_CHECK_EQUAL(reverse.range[i], 1.0f);
    }
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

BOOST_AUTO_TEST_CASE(holdem_range_parser_parses_plus_boundaries) {
    const auto aces = zeta::holdem::parse_range("AA+");
    BOOST_REQUIRE(aces.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(aces.range), 6u);
    BOOST_CHECK_EQUAL(aces.range.total_weight(), 6.0f);

    const auto ace_king_suited = zeta::holdem::parse_range("AKs+");
    BOOST_REQUIRE(ace_king_suited.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(ace_king_suited.range), 4u);
    BOOST_CHECK_EQUAL(ace_king_suited.range.total_weight(), 4.0f);

    const auto ace_king_offsuit = zeta::holdem::parse_range("AKo+");
    BOOST_REQUIRE(ace_king_offsuit.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(ace_king_offsuit.range), 12u);
    BOOST_CHECK_EQUAL(ace_king_offsuit.range.total_weight(), 12.0f);

    const auto ace_king_both = zeta::holdem::parse_range("AK+");
    BOOST_REQUIRE(ace_king_both.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(ace_king_both.range), 16u);
    BOOST_CHECK_EQUAL(ace_king_both.range.total_weight(), 16.0f);
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

BOOST_AUTO_TEST_CASE(holdem_range_parser_parses_descending_and_both_suitedness_ranges) {
    const auto descending_pairs = zeta::holdem::parse_range("99-55");
    BOOST_REQUIRE(descending_pairs.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(descending_pairs.range), 30u);
    BOOST_CHECK_EQUAL(descending_pairs.range.total_weight(), 30.0f);

    const auto descending_suited = zeta::holdem::parse_range("A9s-A5s");
    BOOST_REQUIRE(descending_suited.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(descending_suited.range), 20u);
    BOOST_CHECK_EQUAL(descending_suited.range.total_weight(), 20.0f);

    const auto both = zeta::holdem::parse_range("A5-A9");
    BOOST_REQUIRE(both.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(both.range), 80u);
    BOOST_CHECK_EQUAL(both.range.total_weight(), 80.0f);

    const auto single_pair = zeta::holdem::parse_range("QQ-QQ");
    BOOST_REQUIRE(single_pair.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(single_pair.range), 6u);
    BOOST_CHECK_EQUAL(single_pair.range.total_weight(), 6.0f);
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

BOOST_AUTO_TEST_CASE(holdem_range_parser_accepts_case_insensitive_input_and_decimal_weight_forms) {
    const auto parsed = zeta::holdem::parse_range(" aa, aKs:.5, Kk:1. ");
    BOOST_REQUIRE(parsed.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(parsed.range), 16u);
    BOOST_CHECK_CLOSE(parsed.range.total_weight(), 14.0f, 0.001);
}

BOOST_AUTO_TEST_CASE(holdem_range_parser_overwrites_duplicate_weights) {
    const auto parsed = zeta::holdem::parse_range("AKs:0.25,AKs:0.75");
    BOOST_REQUIRE(parsed.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(parsed.range), 4u);
    BOOST_CHECK_EQUAL(parsed.range.total_weight(), 3.0f);
}

BOOST_AUTO_TEST_CASE(holdem_range_parser_overwrites_overlapping_classes) {
    const auto suited_later = zeta::holdem::parse_range("AK,AKs:0.25");
    BOOST_REQUIRE(suited_later.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(suited_later.range), 16u);
    BOOST_CHECK_CLOSE(suited_later.range.total_weight(), 13.0f, 0.001);

    const auto both_later = zeta::holdem::parse_range("AKs:0.25,AK");
    BOOST_REQUIRE(both_later.ok());
    BOOST_CHECK_EQUAL(non_zero_combo_count(both_later.range), 16u);
    BOOST_CHECK_EQUAL(both_later.range.total_weight(), 16.0f);

    const auto zero_weight = zeta::holdem::parse_range("AA:0");
    BOOST_REQUIRE(zero_weight.ok());
    BOOST_CHECK(zero_weight.range.empty());
    BOOST_CHECK_EQUAL(zero_weight.range.total_weight(), 0.0f);
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

BOOST_AUTO_TEST_CASE(holdem_range_parser_rejects_unsupported_extensions) {
    BOOST_CHECK(!zeta::holdem::parse_range("(AA,KK)").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("AA&KK").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("AA|KK").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("AA!KK").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("!AsKh").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("AA=0.5").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("AA:50%").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("AsKx").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("BTN").ok());
    BOOST_CHECK(!zeta::holdem::parse_range("top pair").ok());
}

BOOST_AUTO_TEST_CASE(holdem_range_parser_reports_expected_error_codes) {
    using enum zeta::holdem::range_parse_error_code;

    BOOST_CHECK(zeta::holdem::parse_range("").error.code == expected_term);
    BOOST_CHECK(zeta::holdem::parse_range("Z").error.code == expected_rank);
    BOOST_CHECK(zeta::holdem::parse_range("AsA").error.code == expected_suit);
    BOOST_CHECK(zeta::holdem::parse_range("AA,").error.code == expected_term);
    BOOST_CHECK(zeta::holdem::parse_range("AA KK").error.code == expected_comma);
    BOOST_CHECK(zeta::holdem::parse_range("AsAs").error.code == invalid_exact_combo);
    BOOST_CHECK(zeta::holdem::parse_range("AsKh+").error.code == invalid_plus);
    BOOST_CHECK(zeta::holdem::parse_range("A5s-A9o").error.code == invalid_range);
    BOOST_CHECK(zeta::holdem::parse_range("AA:").error.code == invalid_weight);
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
