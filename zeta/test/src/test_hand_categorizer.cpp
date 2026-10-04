#include <boost/test/unit_test.hpp>

#include "cfr/extraction/result_store.h"
#include "eval/categorizer.h"

namespace {

    constexpr zeta::card_mask card(const int suit, const int rank)
    {
        return zeta::card_mask{1} << (suit * 13 + rank);
    }

}

BOOST_AUTO_TEST_SUITE(hand_categorizer_suite)

BOOST_AUTO_TEST_CASE(test_paired_board_geometry_contract)
{
    const zeta::holdem::board board{card(0, 11) | card(1, 9) | card(2, 6)};
    const auto overpair = zeta::holdem::categorize_hand(card(0, 12) | card(1, 12), board);
    BOOST_CHECK_EQUAL(static_cast<int>(overpair.made_hand_tier), static_cast<int>(zeta::holdem::hand_category::pair));
    BOOST_CHECK_EQUAL(static_cast<int>(overpair.source), static_cast<int>(zeta::holdem::pair_source::hole_pair));
    BOOST_CHECK_EQUAL(static_cast<int>(overpair.pair_pos), static_cast<int>(zeta::holdem::pair_position::overpair));

    const auto bottom_pair = zeta::holdem::categorize_hand(card(0, 10) | card(1, 6), board);
    BOOST_CHECK_EQUAL(static_cast<int>(bottom_pair.source), static_cast<int>(zeta::holdem::pair_source::hole_board_pair));
    BOOST_CHECK_EQUAL(static_cast<int>(bottom_pair.pair_pos), static_cast<int>(zeta::holdem::pair_position::bottom_pair));

    const auto board_only = zeta::holdem::categorize_hand(card(0, 10) | card(1, 9), zeta::holdem::board{card(0, 11) | card(1, 11) | card(2, 6)});
    BOOST_CHECK_EQUAL(static_cast<int>(board_only.source), static_cast<int>(zeta::holdem::pair_source::board_only_pair));

    const auto trips = zeta::holdem::categorize_hand(card(0, 11) | card(1, 10), zeta::holdem::board{card(2, 11) | card(3, 11) | card(2, 6)});
    BOOST_CHECK_EQUAL(static_cast<int>(trips.made_hand_tier), static_cast<int>(zeta::holdem::hand_category::trips));
    BOOST_CHECK_EQUAL(static_cast<int>(trips.source), static_cast<int>(zeta::holdem::pair_source::none));
    BOOST_CHECK_EQUAL(static_cast<int>(trips.pair_pos), static_cast<int>(zeta::holdem::pair_position::none));
}

BOOST_AUTO_TEST_CASE(test_straight_draw_window_enumeration)
{
    const auto info = zeta::holdem::evaluate_straight_draw(
        card(0, 8) | card(1, 9) | card(2, 10) | card(3, 11),
        zeta::holdem::street::flop
    );
    BOOST_CHECK(info.draws != zeta::holdem::draw_flags::none);
    BOOST_CHECK(info.backdoor_straight);
}

BOOST_AUTO_TEST_CASE(test_result_store_views_are_offset_based)
{
    using namespace zeta::holdem::cfr::extraction;
    result_store store{
        {node_record{.node_id = 0, .strategy_context_id = 0, .public_state_id = 7, .combo_begin = 0, .combo_count = 2, .action_val_begin = 0, .action_count = 2, .seat_value_begin = 0, .seat_value_count = 1}},
        {strategy_surface_record{.strategy_begin = 0, .combo_count = 2, .action_count = 2}},
        {strategy_surface_entry{0.25f}, strategy_surface_entry{0.75f}, strategy_surface_entry{0.6f}, strategy_surface_entry{0.4f}},
        {combo_reach_entry{0.5f, 0.25f}, combo_reach_entry{1.0f, 0.0f}},
        {combo_value_entry{10.0}, combo_value_entry{-5.0}},
        {action_value_entry{8.0, -2.0}, action_value_entry{12.0, 2.0}, action_value_entry{-6.0, -1.0}, action_value_entry{-4.0, 1.0}},
        {seat_value{0.125, 1.25, 10.0, 3.5}},
        {0.7f, 0.2f},
        {zeta::holdem::hand_category_classification{.made_hand_tier = zeta::holdem::hand_category::pair}, zeta::holdem::hand_category_classification{.made_hand_tier = zeta::holdem::hand_category::high_card}}
    };

    const auto node = store.node(0);
    BOOST_CHECK_EQUAL(node.node_id(), 0u);
    BOOST_CHECK_EQUAL(node.public_state_id(), 7u);
    BOOST_CHECK_CLOSE(node.strategy().frequency(1, 0), 0.6f, 0.001f);
    BOOST_CHECK_CLOSE(node.values().range_reach_weight(0), 0.125, 0.001);
    BOOST_CHECK_CLOSE(node.values().q_value(1, 1), -4.0, 0.001);
    BOOST_CHECK_CLOSE(node.equity().showdown_equity(0), 0.7, 0.001);
    BOOST_CHECK_EQUAL(static_cast<int>(node.categories().classification(0).made_hand_tier), static_cast<int>(zeta::holdem::hand_category::pair));
}

BOOST_AUTO_TEST_SUITE_END()
