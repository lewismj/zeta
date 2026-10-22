#include <boost/test/unit_test.hpp>

#include "cfr/graph/builder.h"
#include "cfr/graph/graph.h"
#include "cfr/graph/validation.h"
#include "cfr/betting/betting.h"
#include "cfr/scheduler/scheduler.h"
#include "cfr/solver/metadata.h"
#include "cfr/tables/table_layout.h"
#include "terminal/terminal.h"

#include <algorithm>
#include <array>
#include <atomic>
#include <cmath>
#include <limits>
#include <set>
#include <sstream>
#include <stdexcept>
#include <type_traits>
#include <vector>

using namespace zeta::holdem::cfr;
using namespace zeta::holdem::cfr::solver;

namespace {

    constexpr zeta::card_mask card(const int suit, const int rank)
    {
        return zeta::card_mask{1} << (suit * 13 + rank);
    }

    zeta::holdem::board deterministic_river_board()
    {
        return zeta::holdem::board{
            card(0, 12) | card(1, 11) | card(2, 10) | card(3, 9) | card(0, 0)
        };
    }
}

game_graph require_graph(std::expected<game_graph, graph_build_error> result)
{
    if (!result) {
        BOOST_ERROR("graph build failed: " << result.error().kind);
        return {};
    }
    return std::move(*result);
}

action_table_layout require_layout(std::expected<action_table_layout, table_layout_error> result)
{
    if (!result) {
        BOOST_ERROR("layout build failed: " << result.error().kind);
        return {};
    }
    return std::move(*result);
}

solver_graph_annotations make_default_annotations(const game_graph& graph)
{
    solver_graph_annotations annotations;
    annotations.actor_by_node.assign(graph.node_count, INVALID_PLAYER);
    annotations.chance_event_id_by_node.assign(graph.node_count, INVALID_METADATA_ID);
    annotations.terminal_leaf_id_by_node.assign(graph.node_count, INVALID_METADATA_ID);
    annotations.state_by_node.assign(
        graph.node_count,
        solver_node_state_metadata{
            .street = holdem_street::river,
            .public_state_id = 7,
            .betting_state_id = 11
        });

    uint32_t chance_event_id = 0;
    uint32_t terminal_leaf_id = 0;
    for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
        if (graph.is_player_node(node_id)) {
            annotations.actor_by_node[node_id] = 0;
        }
        if (graph.node_types[node_id] == node_kind::chance
            || graph.node_types[node_id] == node_kind::player_chance) {
            annotations.chance_event_id_by_node[node_id] = chance_event_id++;
        }
        if (graph.is_terminal(node_id)) {
            annotations.terminal_leaf_id_by_node[node_id] = terminal_leaf_id++;
        }
    }

    return annotations;
}

game_graph create_shared_infoset_tree()
{
    graph_builder builder;

    auto root = builder.add_node(node_kind::chance);
    auto lhs_player = builder.add_node(node_kind::player);
    auto rhs_player = builder.add_node(node_kind::player);
    auto lhs_terminal = builder.add_node(node_kind::terminal);
    auto rhs_terminal = builder.add_node(node_kind::terminal);

    builder.add_edge(root, lhs_player, 0);
    builder.add_edge(root, rhs_player, 1);
    builder.add_edge(lhs_player, lhs_terminal, 0);
    builder.add_edge(rhs_player, rhs_terminal, 0);
    builder.set_infoset_id(lhs_player, 0);
    builder.set_infoset_id(rhs_player, 0);

    return require_graph(builder.build());
}

/**
 * Helper to create a simple 3-node tree for testing:
 *   0 (player) -> 1 (terminal) or 2 (terminal)
 */
game_graph create_simple_tree()
{
    graph_builder builder;
    
    /** Add nodes. */
    auto root = builder.add_node(node_kind::player);
    auto term1 = builder.add_node(node_kind::terminal);
    auto term2 = builder.add_node(node_kind::terminal);
    
    /** Add edges. */
    builder.add_edge(root, term1, 0);
    builder.add_edge(root, term2, 1);
    
    /** Set infoset for player node. */
    builder.set_infoset_id(root, 0);
    
    return require_graph(builder.build());
}

/**
 * Helper to create a tree with chance nodes:
 *   0 (player) -> 1 (chance) -> {2, 3} (terminal)
 *   0 (player) -> 4 (chance) -> {5, 6} (terminal)
 */
game_graph create_chance_tree()
{
    graph_builder builder;
    
    auto root = builder.add_node(node_kind::player);
    auto chance1 = builder.add_node(node_kind::chance);
    auto term1_1 = builder.add_node(node_kind::terminal);
    auto term1_2 = builder.add_node(node_kind::terminal);
    auto chance2 = builder.add_node(node_kind::chance);
    auto term2_1 = builder.add_node(node_kind::terminal);
    auto term2_2 = builder.add_node(node_kind::terminal);
    
    builder.add_edge(root, chance1, 0);
    builder.add_edge(root, chance2, 1);
    
    builder.add_edge(chance1, term1_1, 0);
    builder.add_edge(chance1, term1_2, 1);
    
    builder.add_edge(chance2, term2_1, 0);
    builder.add_edge(chance2, term2_2, 1);
    
    builder.set_infoset_id(root, 0);
    
    return require_graph(builder.build());
}

/** Uniform outcomes for every chance node, aligned with the graph edges. */
chance_event_table make_uniform_chance_event_table(const game_graph& graph)
{
    chance_event_table table;
    table.event_id_by_node.assign(graph.node_count, INVALID_CHANCE_EVENT);
    for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
        if (graph.node_types[node_id] != node_kind::chance) {
            continue;
        }
        const auto edges = graph.out_edges(node_id);
        const auto first_outcome = static_cast<uint32_t>(table.outcomes.size());
        table.event_id_by_node[node_id] = static_cast<uint32_t>(table.events.size());
        table.events.push_back(chance_event{
            .node_id = node_id,
            .first_outcome = first_outcome,
            .outcome_count = static_cast<uint32_t>(edges.size())
        });
        const auto probability = edges.empty() ? 0.0f : 1.0f / static_cast<float>(edges.size());
        for (const auto& child_edge : edges) {
            table.outcomes.push_back(chance_outcome{
                .child_node = child_edge.child_node,
                .action_index = child_edge.action_index,
                .probability = probability,
                .board_partition_id = child_edge.action_index,
                .outcome_id = static_cast<chance_outcome_id>(table.outcomes.size())
            });
        }
    }
    return table;
}

BOOST_AUTO_TEST_SUITE(cfr_graph_types)

/** Check that graph types are properly defined. */
BOOST_AUTO_TEST_CASE(edge_structure) {
    edge e{42, 3};
    BOOST_CHECK_EQUAL(e.child_node, 42u);
    BOOST_CHECK_EQUAL(e.action_index, 3u);
}

BOOST_AUTO_TEST_CASE(node_kind_enum) {
    BOOST_CHECK(node_kind::player == node_kind::player);
    BOOST_CHECK(node_kind::player != node_kind::terminal);
    BOOST_CHECK(node_kind::chance != node_kind::terminal);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(cfr_graph_construction)

/** Test graph construction and CSR topology. */
BOOST_AUTO_TEST_CASE(simple_tree_construction) {
    auto graph = create_simple_tree();
    
    BOOST_CHECK_EQUAL(graph.node_count, 3u);
    BOOST_CHECK_EQUAL(graph.terminal_count, 2u);
    BOOST_CHECK_EQUAL(graph.infoset_count, 1u);
}

BOOST_AUTO_TEST_CASE(csr_offset_monotonicity) {
    auto graph = create_simple_tree();
    
    BOOST_CHECK_EQUAL(graph.row_offsets[0], 0u);
    for (size_t i = 1; i < graph.row_offsets.size(); ++i) {
        BOOST_CHECK_LE(graph.row_offsets[i - 1], graph.row_offsets[i]);
    }
    BOOST_CHECK_EQUAL(graph.row_offsets.back(), static_cast<uint32_t>(graph.edges.size()));
}

BOOST_AUTO_TEST_CASE(edge_bounds_validation) {
    auto graph = create_simple_tree();
    
    for (const auto& e : graph.edges) {
        BOOST_CHECK_LT(e.child_node, graph.node_count);
    }
}

BOOST_AUTO_TEST_CASE(out_edges_accessor) {
    auto graph = create_simple_tree();
    
    auto root = graph.root_node;
    auto edges = graph.out_edges(root);
    BOOST_CHECK_EQUAL(edges.size(), 2u);
    BOOST_CHECK_EQUAL(edges[0].child_node, 0u);
    BOOST_CHECK_EQUAL(edges[1].child_node, 1u);
    
    /** Terminal nodes should have 0 edges. */
    auto edges1 = graph.out_edges(0);
    BOOST_CHECK_EQUAL(edges1.size(), 0u);
}

BOOST_AUTO_TEST_CASE(action_count_accessor) {
    auto graph = create_simple_tree();
    
    BOOST_CHECK_EQUAL(graph.action_count(0), 0u);
    BOOST_CHECK_EQUAL(graph.action_count(1), 0u);
    BOOST_CHECK_EQUAL(graph.action_count(2), 2u);
}

BOOST_AUTO_TEST_CASE(node_kind_queries) {
    auto graph = create_simple_tree();
    
    BOOST_CHECK(!graph.is_player_node(0));
    BOOST_CHECK(!graph.is_player_node(1));
    BOOST_CHECK(graph.is_player_node(2));
    BOOST_CHECK(graph.node_types[2] != node_kind::chance);
    BOOST_CHECK(graph.is_terminal(0));
    BOOST_CHECK(graph.is_terminal(1));
    BOOST_CHECK(!graph.is_terminal(2));
}

BOOST_AUTO_TEST_CASE(infoset_id_mapping) {
    auto graph = create_simple_tree();
    
    BOOST_CHECK_EQUAL(graph.infoset_id[0], ~0u);  /**< Non-player node. */
    BOOST_CHECK_EQUAL(graph.infoset_id[1], ~0u);
    BOOST_CHECK_EQUAL(graph.infoset_id[2], 0u);
}

BOOST_AUTO_TEST_CASE(builder_canonicalizes_edges_by_action_index) {
    graph_builder builder;

    auto root = builder.add_node(node_kind::player);
    auto action_one_terminal = builder.add_node(node_kind::terminal);
    auto action_zero_terminal = builder.add_node(node_kind::terminal);

    builder.add_edge(root, action_one_terminal, 1);
    builder.add_edge(root, action_zero_terminal, 0);
    builder.set_infoset_id(root, 0);

    auto graph = require_graph(builder.build());
    auto edges = graph.out_edges(graph.root_node);

    BOOST_REQUIRE_EQUAL(edges.size(), 2u);
    BOOST_CHECK_EQUAL(edges[0].action_index, 0u);
    BOOST_CHECK_EQUAL(edges[1].action_index, 1u);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(cfr_graph_metadata)

BOOST_AUTO_TEST_CASE(solver_graph_view_validates_side_arrays_and_node_kinds) {
    auto graph = create_chance_tree();
    auto annotations = make_default_annotations(graph);

    auto result = validate_solver_graph_view(make_solver_graph_view<2>(graph, annotations));

    BOOST_REQUIRE(result.has_value());
}

BOOST_AUTO_TEST_CASE(solver_graph_view_rejects_non_player_actor_metadata) {
    auto graph = create_simple_tree();
    auto annotations = make_default_annotations(graph);
    annotations.actor_by_node[0] = 0;

    auto result = validate_solver_graph_view(make_solver_graph_view<2>(graph, annotations));

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == solver_graph_metadata_error_kind::invalid_actor);
    BOOST_CHECK_EQUAL(result.error().node_id, 0u);
}

BOOST_AUTO_TEST_CASE(solver_graph_view_rejects_conflicting_shared_infoset_actor) {
    auto graph = create_shared_infoset_tree();
    auto annotations = make_default_annotations(graph);
    uint32_t first_player = game_graph::INVALID_NODE;
    uint32_t second_player = game_graph::INVALID_NODE;
    for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
        if (graph.is_player_node(node_id)) {
            if (first_player == game_graph::INVALID_NODE) {
                first_player = node_id;
            } else {
                second_player = node_id;
            }
        }
    }
    BOOST_REQUIRE_NE(first_player, game_graph::INVALID_NODE);
    BOOST_REQUIRE_NE(second_player, game_graph::INVALID_NODE);
    annotations.actor_by_node[first_player] = 0;
    annotations.actor_by_node[second_player] = 1;

    auto result = validate_solver_graph_view(make_solver_graph_view<2>(graph, annotations));

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == solver_graph_metadata_error_kind::incompatible_infoset_actor);
    BOOST_CHECK_EQUAL(result.error().infoset_id, 0u);
}

BOOST_AUTO_TEST_CASE(player_mask_backs_generic_terminal_masks) {
    zeta::holdem::player_mask<4> active;
    active.set(1);
    active.set(3);

    zeta::holdem::folded_mask<4> folded;
    folded.set_folded(2, true);

    BOOST_CHECK(active[1]);
    BOOST_CHECK(active[3]);
    BOOST_CHECK(!active[0]);
    BOOST_CHECK_EQUAL(active.count(), 2u);
    BOOST_CHECK(folded[2]);
    BOOST_CHECK(!folded[1]);
}

BOOST_AUTO_TEST_CASE(public_state_registry_validates_unique_paths_and_root_links) {
    public_state_registry registry;
    registry.states = {
        public_board_state{
            .id = 0,
            .street = holdem_street::flop,
            .board_cards = card(0, 0) | card(1, 1),
            .is_root_state = true
        },
        public_board_state{
            .id = 1,
            .street = holdem_street::turn,
            .board_cards = card(0, 0) | card(1, 1) | card(2, 2),
            .parent_state_id = 0,
            .chance_event_id_from_parent = 0,
            .chance_outcome_id_from_parent = 0
        },
        public_board_state{
            .id = 2,
            .street = holdem_street::river,
            .board_cards = card(0, 0) | card(1, 1) | card(2, 2) | card(3, 3),
            .parent_state_id = 1,
            .chance_event_id_from_parent = 1,
            .chance_outcome_id_from_parent = 1
        }
    };
    registry.state_id_by_node = {0, 1, 2};
    registry.root_public_state_id = 0;

    auto valid = validate_public_state_registry(registry);
    BOOST_REQUIRE(valid.has_value());

    // Ordered per-street dealing legitimately reaches the same board via distinct
    // events, so a shared board is accepted while a repeated (event, outcome) path
    // is rejected.
    registry.states[2].board_cards = registry.states[0].board_cards;
    BOOST_CHECK(validate_public_state_registry(registry).has_value());

    registry.states[2].chance_event_id_from_parent = registry.states[1].chance_event_id_from_parent;
    registry.states[2].chance_outcome_id_from_parent = registry.states[1].chance_outcome_id_from_parent;
    auto duplicate = validate_public_state_registry(registry);
    BOOST_REQUIRE(!duplicate);
    BOOST_CHECK(duplicate.error().kind == public_state_registry_error_kind::duplicate_public_state_path);
}

BOOST_AUTO_TEST_CASE(runout_registry_validates_complete_root_to_river_paths) {
    runout_registry registry;
    registry.runouts = {
        complete_runout{
            .id = 0,
            .root_public_state_id = 0,
            .river_public_state_id = 2,
            .dealt_turn = card(0, 3),
            .dealt_river = card(1, 4)
        },
        complete_runout{
            .id = 1,
            .root_public_state_id = 0,
            .river_public_state_id = 3,
            .dealt_turn = card(0, 5),
            .dealt_river = card(1, 6)
        }
    };
    registry.river_public_state_by_runout = {2, 3};

    auto valid = validate_runout_registry(registry);
    BOOST_REQUIRE(valid.has_value());

    registry.river_public_state_by_runout[1] = 99;
    auto mismatched = validate_runout_registry(registry);
    BOOST_REQUIRE(!mismatched);
    BOOST_CHECK(mismatched.error().kind == runout_registry_error_kind::invalid_river_path);
}

namespace {
    zeta::card_mask full_deck_mask()
    {
        zeta::card_mask deck = 0;
        for (int suit = 0; suit < 4; ++suit) {
            for (int rank = 0; rank < 13; ++rank) {
                deck |= card(suit, rank);
            }
        }
        return deck;
    }

    uint32_t popcount_mask(const zeta::card_mask mask)
    {
        uint32_t count = 0;
        auto bits = static_cast<uint64_t>(mask);
        while (bits != 0u) {
            bits &= bits - 1u;
            ++count;
        }
        return count;
    }

    holdem_public_game_config<2> tiny_multi_street_config(
        const holdem_street street,
        const zeta::card_mask board,
        const zeta::card_mask live_runout_cards)
    {
        holdem_public_game_config<2> config{};
        config.street = street;
        config.board_cards = board;
        config.dead_cards = full_deck_mask() & ~board & ~live_runout_cards;
        config.initial_stacks = {100.0, 100.0};
        config.initial_committed = {1.0, 1.0};
        config.root_actor = 0;
        config.max_history = 4;
        config.abstraction = make_single_size_policy(1.0, 1).value();
        return config;
    }

    // Verify the graph obeys the Step 1 structural invariants: chance only at a
    // non-river end-of-round, betting never crossing a street, and every edge moving
    // forward in street order.
    void check_multi_street_invariants(const holdem_public_game_graph<2>& lowered)
    {
        const auto& graph = lowered.graph;
        const auto& streets = lowered.annotations.state_by_node;
        for (uint32_t node = 0; node < graph.node_count; ++node) {
            const auto node_street = streets[node].street;
            BOOST_CHECK(node_street != holdem_street::invalid);
            const auto kind = graph.node_types[node];
            for (const auto& edge : graph.out_edges(node)) {
                const auto child_street = streets[edge.child_node].street;
                BOOST_CHECK(static_cast<int>(child_street) >= static_cast<int>(node_street));
                if (kind == node_kind::player) {
                    BOOST_CHECK(child_street == node_street);
                } else if (kind == node_kind::chance) {
                    BOOST_CHECK(node_street != holdem_street::river);
                    BOOST_CHECK(static_cast<int>(child_street) == static_cast<int>(node_street) + 1);
                }
            }
            if (kind == node_kind::terminal) {
                BOOST_CHECK_EQUAL(graph.out_edges(node).size(), 0u);
            }
        }
    }

    void check_shape_matches_graph(const holdem_public_game_graph<2>& lowered,
                                   const multi_street_public_game_shape& shape)
    {
        const auto& graph = lowered.graph;
        uint64_t players = 0;
        uint64_t chances = 0;
        uint64_t terminals = 0;
        for (uint32_t node = 0; node < graph.node_count; ++node) {
            switch (graph.node_types[node]) {
                case node_kind::player: ++players; break;
                case node_kind::chance: ++chances; break;
                case node_kind::terminal: ++terminals; break;
                default: break;
            }
        }

        BOOST_CHECK_EQUAL(shape.node_count, graph.node_count);
        BOOST_CHECK_EQUAL(shape.edge_count, static_cast<uint64_t>(graph.edges.size()));
        BOOST_CHECK_EQUAL(shape.player_node_count, players);
        BOOST_CHECK_EQUAL(shape.chance_node_count, chances);
        BOOST_CHECK_EQUAL(shape.terminal_node_count, terminals);
        BOOST_CHECK_EQUAL(shape.chance_node_count, lowered.chance_events.events.size());
        BOOST_CHECK_EQUAL(shape.chance_outcome_count, lowered.chance_events.outcomes.size());
        BOOST_CHECK_EQUAL(shape.infoset_count, static_cast<uint64_t>(graph.infoset_count));
        BOOST_CHECK_EQUAL(shape.infoset_count, players);
        BOOST_CHECK_EQUAL(shape.public_state_count, lowered.public_states.states.size());
        BOOST_CHECK_EQUAL(shape.runout_count, lowered.runouts.runouts.size());

        uint64_t turn_states = 0;
        uint64_t river_states = 0;
        for (const auto& state : lowered.public_states.states) {
            if (state.street == holdem_street::turn) {
                ++turn_states;
            } else if (state.street == holdem_street::river) {
                ++river_states;
            }
        }
        BOOST_CHECK_EQUAL(shape.turn_public_state_count, turn_states);
        BOOST_CHECK_EQUAL(shape.river_public_state_count, river_states);
    }
}

BOOST_AUTO_TEST_CASE(multi_street_turn_lowering_deals_a_single_street) {
    const auto board = card(0, 0) | card(1, 1) | card(2, 2) | card(3, 3);
    const auto live = card(0, 5) | card(1, 6);
    auto config = tiny_multi_street_config(holdem_street::turn, board, live);

    auto lowered = lower_multi_street_public_game(config);
    if (!lowered) {
        BOOST_TEST_MESSAGE("turn lowering failed kind=" << static_cast<int>(lowered.error().kind)
            << " node_id=" << lowered.error().node_id << " state_id=" << lowered.error().state_id);
    }
    BOOST_REQUIRE(lowered.has_value());

    // A turn root deals only the river. Restricting the live deck to two cards leaves
    // exactly two completable five-card boards; boards are shared across every betting
    // line, so there are exactly two river public states and two runouts.
    BOOST_CHECK_EQUAL(lowered->runouts.runouts.size(), 2u);
    std::set<zeta::card_mask> river_boards;
    for (const auto& runout : lowered->runouts.runouts) {
        const auto& river_state = lowered->public_states.states[runout.river_public_state_id];
        BOOST_CHECK_EQUAL(popcount_mask(river_state.board_cards), 5u);
        river_boards.insert(river_state.board_cards);
        BOOST_CHECK_EQUAL(runout.dealt_turn, zeta::card_mask{0});
        BOOST_CHECK_EQUAL(popcount_mask(runout.dealt_river), 1u);
    }
    BOOST_CHECK_EQUAL(river_boards.size(), 2u);

    // Every chance event on a turn root is a river deal; no turn public state exists.
    for (const auto& event : lowered->chance_events.events) {
        BOOST_CHECK(event.kind == public_chance_event_kind::river);
    }

    check_multi_street_invariants(*lowered);
    BOOST_CHECK(validate_public_state_registry(lowered->public_states, lowered->graph.node_count).has_value());
    BOOST_CHECK(validate_chance_event_table(lowered->graph, lowered->chance_events).has_value());
    BOOST_CHECK(validate_runout_registry(lowered->runouts).has_value());

    auto shape = estimate_multi_street_public_game_shape(config);
    BOOST_REQUIRE(shape.has_value());
    check_shape_matches_graph(*lowered, *shape);
    BOOST_CHECK_EQUAL(shape->turn_public_state_count, 1u);
    BOOST_CHECK_GT(shape->player_node_count, 0u);
}

BOOST_AUTO_TEST_CASE(multi_street_flop_lowering_interleaves_two_streets) {
    const auto board = card(0, 0) | card(1, 1) | card(2, 2);
    const auto live = card(3, 5) | card(3, 6);
    auto config = tiny_multi_street_config(holdem_street::flop, board, live);

    auto lowered = lower_multi_street_public_game(config);
    if (!lowered) {
        BOOST_TEST_MESSAGE("flop lowering failed kind=" << static_cast<int>(lowered.error().kind)
            << " node_id=" << lowered.error().node_id << " state_id=" << lowered.error().state_id);
    }
    BOOST_REQUIRE(lowered.has_value());

    // A flop root interleaves a turn deal and a river deal. Both the turn and river
    // public states must be present, and both chance-event kinds must appear.
    uint32_t turn_states = 0;
    uint32_t river_states = 0;
    for (const auto& state : lowered->public_states.states) {
        if (state.street == holdem_street::turn) {
            ++turn_states;
        } else if (state.street == holdem_street::river) {
            ++river_states;
        }
    }

    bool saw_turn_event = false;
    bool saw_river_event = false;
    for (const auto& event : lowered->chance_events.events) {
        saw_turn_event = saw_turn_event || event.kind == public_chance_event_kind::turn;
        saw_river_event = saw_river_event || event.kind == public_chance_event_kind::river;
    }
    BOOST_CHECK(saw_turn_event);
    BOOST_CHECK(saw_river_event);

    // With only two live cards the sole completable five-card board is reached through
    // both turn/river orderings and every betting line, and board-keyed identity shares
    // it, so there is exactly one river public state and one runout -- the key
    // ordered-deal deduplication case.
    BOOST_CHECK_EQUAL(lowered->runouts.runouts.size(), 1u);
    std::set<zeta::card_mask> river_boards;
    for (const auto& runout : lowered->runouts.runouts) {
        const auto& river_state = lowered->public_states.states[runout.river_public_state_id];
        BOOST_CHECK_EQUAL(popcount_mask(river_state.board_cards), 5u);
        river_boards.insert(river_state.board_cards);
        BOOST_CHECK_EQUAL(popcount_mask(runout.dealt_turn), 1u);
        BOOST_CHECK_EQUAL(popcount_mask(runout.dealt_river), 1u);
    }
    BOOST_CHECK_EQUAL(river_boards.size(), 1u);

    // The two live cards yield two distinct turn boards, each a shared turn public state.
    BOOST_CHECK_EQUAL(turn_states, 2u);
    BOOST_CHECK_EQUAL(river_states, 1u);

    // A real betting round runs on each street before its deal.
    bool saw_player_node = false;
    for (const auto kind : lowered->graph.node_types) {
        saw_player_node = saw_player_node || kind == node_kind::player;
    }
    BOOST_CHECK(saw_player_node);

    check_multi_street_invariants(*lowered);
    BOOST_CHECK(validate_public_state_registry(lowered->public_states, lowered->graph.node_count).has_value());
    BOOST_CHECK(validate_chance_event_table(lowered->graph, lowered->chance_events).has_value());
    BOOST_CHECK(validate_runout_registry(lowered->runouts).has_value());

    auto shape = estimate_multi_street_public_game_shape(config);
    BOOST_REQUIRE(shape.has_value());
    check_shape_matches_graph(*lowered, *shape);
    BOOST_CHECK_GT(shape->turn_public_state_count, 0u);
    BOOST_CHECK_GT(shape->player_node_count, 0u);
}

BOOST_AUTO_TEST_CASE(multi_street_lowering_rejects_multiway) {
    holdem_public_game_config<3> config{};
    config.street = holdem_street::turn;
    config.board_cards = card(0, 0) | card(1, 1) | card(2, 2) | card(3, 3);
    config.initial_stacks = {100.0, 100.0, 100.0};
    config.initial_committed = {1.0, 1.0, 1.0};
    config.abstraction = make_single_size_policy(1.0, 1).value();

    auto lowered = lower_multi_street_public_game(config);
    BOOST_REQUIRE(!lowered);
    BOOST_CHECK(lowered.error().kind == betting_validation_error_kind::unsupported_player_count);

    auto shape = estimate_multi_street_public_game_shape(config);
    BOOST_REQUIRE(!shape);
    BOOST_CHECK(shape.error().kind == betting_validation_error_kind::unsupported_player_count);
}

BOOST_AUTO_TEST_CASE(multi_street_lowering_rejects_non_postflop_root) {
    const auto board = card(0, 0) | card(1, 1) | card(2, 2) | card(3, 3) | card(0, 5);
    auto config = tiny_multi_street_config(holdem_street::river, board, 0);
    config.street = holdem_street::river;

    auto lowered = lower_multi_street_public_game(config);
    BOOST_REQUIRE(!lowered);
}

BOOST_AUTO_TEST_CASE(suit_permutation_relabels_card_lanes) {
    BOOST_CHECK(is_identity_suit_permutation(identity_suit_permutation()));
    BOOST_CHECK_EQUAL(apply_suit_permutation(card(0, 3), identity_suit_permutation()), card(0, 3));

    // Swap spades (0) and hearts (1); ranks are preserved within each lane.
    const suit_permutation swap_spades_hearts{1, 0, 2, 3};
    BOOST_CHECK(!is_identity_suit_permutation(swap_spades_hearts));
    BOOST_CHECK_EQUAL(apply_suit_permutation(card(0, 4), swap_spades_hearts), card(1, 4));
    BOOST_CHECK_EQUAL(apply_suit_permutation(card(1, 7), swap_spades_hearts), card(0, 7));
    BOOST_CHECK_EQUAL(
        apply_suit_permutation(card(0, 4) | card(1, 7) | card(2, 2), swap_spades_hearts),
        card(1, 4) | card(0, 7) | card(2, 2));
}

BOOST_AUTO_TEST_CASE(canonicalize_public_card_outcomes_exact_is_identity) {
    std::vector<chance_outcome> outcomes = {
        chance_outcome{.outcome_id = 0, .cards = card(0, 3)},
        chance_outcome{.outcome_id = 1, .cards = card(2, 9)}
    };
    outcomes[0].probability = 0.5f;
    outcomes[1].probability = 0.5f;

    const auto board = card(0, 0) | card(1, 1) | card(2, 2);
    auto canonical = canonicalize_public_card_outcomes(
        outcomes, board, 0, public_card_isomorphism_mode::exact);
    BOOST_REQUIRE_EQUAL(canonical.size(), outcomes.size());
    for (std::size_t index = 0; index < canonical.size(); ++index) {
        BOOST_CHECK_EQUAL(canonical[index].representative_cards, outcomes[index].cards);
        BOOST_CHECK_CLOSE(canonical[index].probability, outcomes[index].probability, 1e-5);
        BOOST_REQUIRE_EQUAL(canonical[index].members.size(), 1u);
        BOOST_CHECK_EQUAL(canonical[index].members[0].outcome_id, outcomes[index].outcome_id);
        BOOST_CHECK(is_identity_suit_permutation(canonical[index].members[0].permutation));
    }
}

BOOST_AUTO_TEST_CASE(canonicalize_public_card_outcomes_collapses_suit_isomorphic) {
    // Board As Ks 7h: the spade lane (A,K) and the heart lane (7) are pinned, so the
    // only board-fixing suit permutation swaps the two unused suits (diamonds/clubs).
    // Dealing the 2 of either of those suits is isomorphic and must collapse into one
    // representative with the summed probability, while a spade/heart 2 stays distinct.
    const auto board = card(0, 12) | card(0, 11) | card(1, 5);
    const auto two_spades = card(0, 0);
    const auto two_hearts = card(1, 0);
    const auto two_diamonds = card(2, 0);
    const auto two_clubs = card(3, 0);
    std::vector<chance_outcome> outcomes = {
        chance_outcome{.probability = 0.25f, .outcome_id = 0, .cards = two_spades},
        chance_outcome{.probability = 0.25f, .outcome_id = 1, .cards = two_hearts},
        chance_outcome{.probability = 0.25f, .outcome_id = 2, .cards = two_diamonds},
        chance_outcome{.probability = 0.25f, .outcome_id = 3, .cards = two_clubs}
    };

    auto canonical = canonicalize_public_card_outcomes(
        outcomes, board, 0, public_card_isomorphism_mode::suit_isomorphic);

    // Spade, heart, and the collapsed diamond/club class -> three representatives.
    BOOST_REQUIRE_EQUAL(canonical.size(), 3u);
    float probability_total = 0.0f;
    std::size_t collapsed_members = 0;
    for (const auto& outcome : canonical) {
        probability_total += outcome.probability;
        collapsed_members = std::max(collapsed_members, outcome.members.size());
    }
    BOOST_CHECK_CLOSE(probability_total, 1.0f, 1e-4);
    BOOST_CHECK_EQUAL(collapsed_members, 2u);

    // The collapsed class has probability 0.5 and one non-identity member permutation
    // that maps its board onto the representative.
    for (const auto& outcome : canonical) {
        if (outcome.members.size() != 2u) {
            continue;
        }
        BOOST_CHECK_CLOSE(outcome.probability, 0.5f, 1e-4);
        bool saw_non_identity = false;
        for (const auto& member : outcome.members) {
            const auto mapped = apply_suit_permutation(member.cards, member.permutation);
            BOOST_CHECK_EQUAL(mapped, outcome.representative_cards);
            saw_non_identity = saw_non_identity || !is_identity_suit_permutation(member.permutation);
        }
        BOOST_CHECK(saw_non_identity);
    }
}

BOOST_AUTO_TEST_CASE(multi_street_memory_estimate_matches_allocation_dimensions) {
    const auto board = card(0, 0) | card(1, 1) | card(2, 2) | card(3, 3);
    const auto live = card(0, 5) | card(1, 6) | card(2, 7);
    auto config = tiny_multi_street_config(holdem_street::turn, board, live);

    auto lowered = lower_multi_street_public_game(config);
    BOOST_REQUIRE(lowered.has_value());
    auto layout = require_layout(make_action_table_layout(lowered->graph));

    auto shape = estimate_multi_street_public_game_shape(config);
    BOOST_REQUIRE(shape.has_value());
    auto estimate = estimate_multi_street_public_game_memory(config);
    BOOST_REQUIRE(estimate.has_value());

    // The combo dimension is the full table width the solver actually allocates, and
    // the action-slot dimension equals the dense layout's value count.
    BOOST_CHECK_EQUAL(estimate->combo_count, static_cast<uint64_t>(zeta::holdem::combination_count));
    BOOST_CHECK_EQUAL(estimate->action_slot_count, layout.value_count());

    // The four combo x action tables: regrets, strategy_sums, and 2x working strategies.
    const uint64_t combo_table_bytes =
        static_cast<uint64_t>(zeta::holdem::combination_count) * layout.value_count() * sizeof(float);
    BOOST_CHECK_EQUAL(estimate->regret_bytes, combo_table_bytes);
    BOOST_CHECK_EQUAL(estimate->strategy_sum_bytes, combo_table_bytes);
    BOOST_CHECK_EQUAL(estimate->working_bytes, 2u * combo_table_bytes);

    // Per-node reach vectors carry one reach lane per seat; per-node value vectors carry one.
    BOOST_CHECK_EQUAL(estimate->reach_bytes, 2u * estimate->value_bytes);

    // Each worker thread holds one set of reach indices and terminal values while it
    // evaluates a terminal; the default estimate is for a single worker.
    const uint64_t worker_scratch_bytes =
        2u * sizeof(zeta::holdem::river_reach_index) + sizeof(zeta::holdem::terminal_values<2>);
    BOOST_CHECK_EQUAL(estimate->worker_bytes, worker_scratch_bytes);
    auto parallel = estimate_multi_street_public_game_memory(config, 8);
    BOOST_REQUIRE(parallel.has_value());
    BOOST_CHECK_EQUAL(parallel->worker_bytes, 8u * worker_scratch_bytes);
    BOOST_CHECK_EQUAL(parallel->total_bytes - estimate->total_bytes, 7u * worker_scratch_bytes);

    // Total is exactly the sum of the components, and the dominant component is the
    // single largest term.
    const std::array<uint64_t, 11> parts = {
        estimate->graph_bytes, estimate->infoset_bytes, estimate->regret_bytes,
        estimate->strategy_sum_bytes, estimate->working_bytes, estimate->reach_bytes,
        estimate->value_bytes, estimate->terminal_cache_bytes, estimate->chance_bytes,
        estimate->worker_bytes, estimate->extraction_bytes
    };
    uint64_t sum = 0;
    uint64_t largest = 0;
    for (const auto bytes : parts) {
        sum += bytes;
        largest = std::max(largest, bytes);
    }
    BOOST_CHECK_EQUAL(estimate->total_bytes, sum);

    const std::array<std::pair<uint64_t, multi_street_memory_component>, 11> labelled = {{
        {estimate->graph_bytes, multi_street_memory_component::graph},
        {estimate->infoset_bytes, multi_street_memory_component::infoset},
        {estimate->regret_bytes, multi_street_memory_component::regret},
        {estimate->strategy_sum_bytes, multi_street_memory_component::strategy_sum},
        {estimate->working_bytes, multi_street_memory_component::working},
        {estimate->reach_bytes, multi_street_memory_component::reach},
        {estimate->value_bytes, multi_street_memory_component::value},
        {estimate->terminal_cache_bytes, multi_street_memory_component::terminal_cache},
        {estimate->chance_bytes, multi_street_memory_component::chance},
        {estimate->worker_bytes, multi_street_memory_component::worker},
        {estimate->extraction_bytes, multi_street_memory_component::extraction}
    }};
    BOOST_REQUIRE(estimate->dominant == std::ranges::max_element(labelled,
        [](const auto& lhs, const auto& rhs) { return lhs.first < rhs.first; })->second);
    BOOST_CHECK_EQUAL(largest, std::ranges::max_element(labelled,
        [](const auto& lhs, const auto& rhs) { return lhs.first < rhs.first; })->first);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(cfr_betting_generation)

holdem_betting_graph_config<2> tiny_river_betting_config()
{
    holdem_betting_graph_config<2> config{};
    config.street = holdem_street::river;
    config.initial_stacks = {99.0, 98.0};
    config.initial_committed = {1.0, 2.0};
    config.root_actor = 0;
    config.public_state_id = 77;
    config.max_history = 8;
    config.abstraction.fixed_pot_fractions = {0.5, 1.0};
    config.abstraction.max_raises = 1;
    return config;
}

BOOST_AUTO_TEST_CASE(opening_bet_sets_last_raise_increment_and_keeps_raise_count_zero) {
    holdem_betting_graph_config<2> config{};
    config.initial_stacks = {100.0, 100.0};
    config.root_actor = 0;
    config.abstraction.fixed_pot_fractions = {0.5};

    auto state = make_initial_betting_state(config);
    auto actions = legal_betting_actions(state, config.abstraction);
    auto bet = std::ranges::find_if(actions, [](const betting_action& action) {
        return action.kind == betting_action_kind::bet;
    });

    BOOST_REQUIRE(bet != actions.end());

    auto next = apply_betting_action(state, *bet, config.abstraction);
    BOOST_REQUIRE(next.has_value());
    BOOST_CHECK_EQUAL(next->current_bet, 1.0);
    BOOST_CHECK_EQUAL(next->last_raise_increment, 1.0);
    BOOST_CHECK_EQUAL(next->raise_count, 0u);
}

BOOST_AUTO_TEST_CASE(full_raise_updates_last_raise_increment_and_raise_count) {
    holdem_betting_graph_config<3> config{};
    config.initial_stacks = {50.0, 100.0, 100.0};
    config.initial_committed = {50.0, 50.0, 0.0};
    config.root_actor = 1;
    config.abstraction.fixed_pot_fractions = {0.5};

    auto state = make_initial_betting_state(config);
    auto actions = legal_betting_actions(state, config.abstraction);
    auto raise = std::ranges::find_if(actions, [](const betting_action& action) {
        return action.kind == betting_action_kind::raise;
    });

    BOOST_REQUIRE(raise != actions.end());

    auto next = apply_betting_action(state, *raise, config.abstraction);
    BOOST_REQUIRE(next.has_value());
    BOOST_CHECK_EQUAL(next->current_bet, 100.0);
    BOOST_CHECK_EQUAL(next->last_raise_increment, 50.0);
    BOOST_CHECK_EQUAL(next->raise_count, 1u);
}

BOOST_AUTO_TEST_CASE(short_all_in_keeps_previous_increment_and_does_not_reopen) {
    holdem_betting_graph_config<3> config{};
    config.initial_stacks = {30.0, 100.0, 100.0};
    config.initial_committed = {100.0, 100.0, 100.0};
    config.root_actor = 0;

    betting_state<3> state{};
    state.actor = 0;
    state.stacks = {30.0, 100.0, 100.0};
    state.committed = {100.0, 100.0, 100.0};
    state.street_committed = {100.0, 100.0, 100.0};
    state.current_bet = 100.0;
    state.last_raise_increment = 50.0;
    state.raise_count = 1;
    state.acted_since_aggression = {false, true, false};

    auto next = apply_betting_action(
        state,
        betting_action{.kind = betting_action_kind::all_in, .amount = 30.0, .target_bet = 130.0},
        config.abstraction);

    BOOST_REQUIRE(next.has_value());
    BOOST_CHECK_EQUAL(next->current_bet, 130.0);
    BOOST_CHECK_EQUAL(next->last_raise_increment, 50.0);
    BOOST_CHECK_EQUAL(next->raise_count, 1u);
    BOOST_CHECK(next->acted_since_aggression[0]);
    BOOST_CHECK(next->acted_since_aggression[1]);
}

BOOST_AUTO_TEST_CASE(short_all_in_does_not_generate_raise_actions_when_not_reopened) {
    holdem_betting_graph_config<3> config{};
    config.initial_stacks = {100.0, 100.0, 100.0};
    config.initial_committed = {100.0, 100.0, 130.0};
    config.root_actor = 1;
    config.abstraction.fixed_pot_fractions = {1.0};

    betting_state<3> state{};
    state.actor = 1;
    state.stacks = {100.0, 100.0, 100.0};
    state.committed = {100.0, 100.0, 130.0};
    state.street_committed = {100.0, 100.0, 130.0};
    state.current_bet = 130.0;
    state.last_raise_increment = 50.0;
    state.raise_count = 1;
    state.acted_since_aggression = {true, true, false};

    auto actions = legal_betting_actions(state, config.abstraction);

    // Seat 1 acted before the short all-in and was not re-raised, so it may only fold
    // or call. Its own all-in would be a raise and is gated the same way.
    BOOST_REQUIRE_EQUAL(actions.size(), 2u);
    BOOST_CHECK(actions[0].kind == betting_action_kind::fold);
    BOOST_CHECK(actions[1].kind == betting_action_kind::call);
}

BOOST_AUTO_TEST_CASE(targets_below_re_raise_minimum_are_filtered) {
    holdem_betting_graph_config<3> config{};
    config.initial_stacks = {100.0, 100.0, 100.0};
    config.initial_committed = {100.0, 100.0, 130.0};
    config.root_actor = 1;
    config.abstraction.fixed_pot_fractions = {0.1};

    betting_state<3> state{};
    state.actor = 1;
    state.stacks = {100.0, 100.0, 100.0};
    state.committed = {100.0, 100.0, 130.0};
    state.street_committed = {100.0, 100.0, 130.0};
    state.current_bet = 130.0;
    state.last_raise_increment = 50.0;
    state.raise_count = 1;
    state.acted_since_aggression = {true, false, false};

    auto actions = legal_betting_actions(state, config.abstraction);

    BOOST_CHECK(std::ranges::none_of(actions, [](const betting_action& action) {
        return action.kind == betting_action_kind::bet || action.kind == betting_action_kind::raise;
    }));
}

BOOST_AUTO_TEST_CASE(legal_actions_are_derived_from_betting_state_and_abstraction_policy) {
    auto config = tiny_river_betting_config();
    config.abstraction.max_raises = 1;
    auto state = make_initial_betting_state(config);

    auto actions = legal_betting_actions(state, config.abstraction);

    BOOST_REQUIRE_EQUAL(actions.size(), 5u);
    BOOST_CHECK(actions[0].kind == betting_action_kind::fold);
    BOOST_CHECK(actions[1].kind == betting_action_kind::call);
    BOOST_CHECK_EQUAL(actions[1].amount, 1.0);
    const auto raise_25 = std::ranges::find_if(actions, [](const betting_action& action) {
        return action.kind == betting_action_kind::raise && action.target_bet == 2.5;
    });
    const auto raise_40 = std::ranges::find_if(actions, [](const betting_action& action) {
        return action.kind == betting_action_kind::raise && action.target_bet == 4.0;
    });
    BOOST_REQUIRE(raise_25 != actions.end());
    BOOST_REQUIRE(raise_40 != actions.end());
    BOOST_CHECK_EQUAL(raise_25->amount, 2.5);
    BOOST_CHECK_EQUAL(raise_40->amount, 4.0);
    BOOST_CHECK(std::ranges::any_of(actions, [](const betting_action& action) {
        return action.kind == betting_action_kind::all_in && action.target_bet == 99.0;
    }));
}

BOOST_AUTO_TEST_CASE(raise_multiple_targets_use_current_bet_multipliers) {
    betting_state<3> state{};
    state.street = holdem_street::river;
    state.actor = 1;
    // 50 each from earlier streets, then seat 0 bets 50 on the river.
    state.stacks = {150.0, 200.0, 200.0};
    state.committed = {100.0, 50.0, 50.0};
    state.street_committed = {50.0, 0.0, 0.0};
    state.current_bet = 50.0;
    state.last_raise_increment = 50.0;
    state.raise_count = 0;

    betting_abstraction_policy policy{};
    policy.fixed_pot_fractions.clear();
    policy.street_actor_sizes[static_cast<std::size_t>(holdem_street::river)].resize(2);
    policy.street_actor_sizes[static_cast<std::size_t>(holdem_street::river)][1].raise_multiples = {2.5};

    auto actions = legal_betting_actions(state, policy);

    BOOST_CHECK(std::ranges::any_of(actions, [](const betting_action& action) {
        return action.kind == betting_action_kind::raise && action.target_bet == 125.0 && action.amount == 125.0;
    }));
}

BOOST_AUTO_TEST_CASE(snapped_size_targets_become_all_in_and_deduplicate) {
    betting_state<3> state{};
    state.street = holdem_street::river;
    state.actor = 0;
    state.stacks = {100.0, 100.0, 100.0};
    state.committed = {100.0, 0.0, 0.0};
    state.street_committed = {100.0, 0.0, 0.0};
    state.current_bet = 100.0;
    state.last_raise_increment = 50.0;
    state.raise_count = 1;

    betting_abstraction_policy policy{};
    policy.fixed_pot_fractions = {0.5};
    policy.all_in_threshold = 0.75;

    auto actions = legal_betting_actions(state, policy);

    const auto all_in_count = std::ranges::count_if(actions, [](const betting_action& action) {
        return action.kind == betting_action_kind::all_in;
    });
    BOOST_CHECK_EQUAL(all_in_count, 1u);
    BOOST_CHECK(std::ranges::none_of(actions, [](const betting_action& action) {
        return action.kind == betting_action_kind::bet || action.kind == betting_action_kind::raise;
    }));
    BOOST_CHECK(std::ranges::any_of(actions, [](const betting_action& action) {
        return action.kind == betting_action_kind::all_in && action.target_bet == 200.0;
    }));
}

// Review section 3.3 regressions: each case below was wrong in the original tree.

BOOST_AUTO_TEST_CASE(street_opens_with_a_bet_and_raise_cap_counts_raises_not_bets) {
    holdem_betting_graph_config<2> config{};
    config.initial_stacks = {500.0, 500.0};
    config.initial_committed = {50.0, 50.0};
    config.root_actor = 0;
    config.abstraction.fixed_pot_fractions = {0.5};
    config.abstraction.max_raises = 1;

    // Chips from earlier streets are matched: no live bet, so the first wager is a bet.
    auto state = make_initial_betting_state(config);
    BOOST_CHECK_EQUAL(state.current_bet, 0.0);
    auto actions = legal_betting_actions(state, config.abstraction);
    auto bet = std::ranges::find_if(actions, [](const betting_action& a) { return a.kind == betting_action_kind::bet; });
    BOOST_REQUIRE(bet != actions.end());
    BOOST_CHECK_EQUAL(bet->amount, 50.0);
    BOOST_CHECK(std::ranges::none_of(actions, [](const betting_action& a) { return a.kind == betting_action_kind::raise; }));

    auto after_bet = apply_betting_action(state, *bet, config.abstraction);
    BOOST_REQUIRE(after_bet.has_value());
    BOOST_CHECK_EQUAL(after_bet->raise_count, 0u);
    BOOST_CHECK_EQUAL(after_bet->current_bet, 50.0);
    BOOST_CHECK_EQUAL(after_bet->committed[0], 100.0);

    // With max_raises 1 the bet can still be raised once; raise sizes scale the street bet.
    actions = legal_betting_actions(*after_bet, config.abstraction);
    auto raise = std::ranges::find_if(actions, [](const betting_action& a) { return a.kind == betting_action_kind::raise; });
    BOOST_REQUIRE(raise != actions.end());
    BOOST_CHECK_EQUAL(raise->target_bet, 125.0);

    auto after_raise = apply_betting_action(*after_bet, *raise, config.abstraction);
    BOOST_REQUIRE(after_raise.has_value());
    BOOST_CHECK_EQUAL(after_raise->raise_count, 1u);

    // The cap is reached: the opener may fold or call, and may not shove instead.
    actions = legal_betting_actions(*after_raise, config.abstraction);
    BOOST_REQUIRE_EQUAL(actions.size(), 2u);
    BOOST_CHECK(actions[0].kind == betting_action_kind::fold);
    BOOST_CHECK(actions[1].kind == betting_action_kind::call);
}

BOOST_AUTO_TEST_CASE(all_in_that_matches_the_call_is_offered_once_as_call) {
    holdem_betting_graph_config<2> config{};
    config.initial_stacks = {100.0, 20.0};
    config.initial_committed = {50.0, 50.0};
    config.root_actor = 0;
    config.abstraction.fixed_pot_fractions = {0.5};

    auto state = make_initial_betting_state(config);
    auto actions = legal_betting_actions(state, config.abstraction);
    auto bet = std::ranges::find_if(actions, [](const betting_action& a) { return a.kind == betting_action_kind::bet; });
    BOOST_REQUIRE(bet != actions.end());
    auto facing = apply_betting_action(state, *bet, config.abstraction);
    BOOST_REQUIRE(facing.has_value());

    actions = legal_betting_actions(*facing, config.abstraction);
    BOOST_REQUIRE_EQUAL(actions.size(), 2u);
    BOOST_CHECK(actions[0].kind == betting_action_kind::fold);
    BOOST_CHECK(actions[1].kind == betting_action_kind::call);
    BOOST_CHECK_EQUAL(actions[1].amount, 20.0);

    auto called = apply_betting_action(*facing, actions[1], config.abstraction);
    BOOST_REQUIRE(called.has_value());
    BOOST_CHECK(called->all_in[1]);
    BOOST_CHECK(called->terminal_kind == zeta::holdem::terminal_state_kind::showdown);
}

BOOST_AUTO_TEST_CASE(no_bets_or_shoves_are_offered_against_all_in_opponents) {
    holdem_betting_graph_config<2> config{};
    config.initial_stacks = {20.0, 100.0};
    config.initial_committed = {50.0, 50.0};
    config.root_actor = 0;
    config.abstraction.fixed_pot_fractions = {0.5, 1.0};

    auto state = make_initial_betting_state(config);
    auto actions = legal_betting_actions(state, config.abstraction);
    auto shove = std::ranges::find_if(actions, [](const betting_action& a) { return a.kind == betting_action_kind::all_in; });
    BOOST_REQUIRE(shove != actions.end());
    auto facing = apply_betting_action(state, *shove, config.abstraction);
    BOOST_REQUIRE(facing.has_value());

    // Extra chips over an all-in can't be called, so only fold and call remain.
    actions = legal_betting_actions(*facing, config.abstraction);
    BOOST_REQUIRE_EQUAL(actions.size(), 2u);
    BOOST_CHECK(actions[0].kind == betting_action_kind::fold);
    BOOST_CHECK(actions[1].kind == betting_action_kind::call);
    BOOST_CHECK_EQUAL(actions[1].amount, 20.0);
}

BOOST_AUTO_TEST_CASE(calling_all_in_for_less_never_lowers_the_current_bet) {
    holdem_betting_graph_config<3> config{};
    config.initial_stacks = {100.0, 30.0, 100.0};
    config.initial_committed = {10.0, 10.0, 10.0};
    config.root_actor = 0;
    config.abstraction.fixed_pot_fractions = {2.0};

    auto state = make_initial_betting_state(config);
    auto actions = legal_betting_actions(state, config.abstraction);
    auto bet = std::ranges::find_if(actions, [](const betting_action& a) { return a.kind == betting_action_kind::bet; });
    BOOST_REQUIRE(bet != actions.end());
    BOOST_REQUIRE_EQUAL(bet->target_bet, 60.0);
    auto facing = apply_betting_action(state, *bet, config.abstraction);
    BOOST_REQUIRE(facing.has_value());

    // Seat 1 can only put in 30 of the 60: that is its call, and it is all-in.
    actions = legal_betting_actions(*facing, config.abstraction);
    BOOST_REQUIRE_EQUAL(actions.size(), 2u);
    BOOST_CHECK_EQUAL(actions[1].amount, 30.0);
    auto short_call = apply_betting_action(*facing, actions[1], config.abstraction);
    BOOST_REQUIRE(short_call.has_value());
    BOOST_CHECK(short_call->all_in[1]);
    BOOST_CHECK_EQUAL(short_call->current_bet, 60.0);

    // Seat 2 still faces the full 60.
    BOOST_REQUIRE_EQUAL(short_call->actor, 2u);
    actions = legal_betting_actions(*short_call, config.abstraction);
    auto call = std::ranges::find_if(actions, [](const betting_action& a) { return a.kind == betting_action_kind::call; });
    BOOST_REQUIRE(call != actions.end());
    BOOST_CHECK_EQUAL(call->amount, 60.0);
}

BOOST_AUTO_TEST_CASE(next_street_resets_street_bets_but_keeps_contributions) {
    holdem_betting_graph_config<2> config{};
    config.street = holdem_street::turn;
    config.initial_stacks = {500.0, 500.0};
    config.initial_committed = {50.0, 50.0};
    config.root_actor = 0;
    config.abstraction.fixed_pot_fractions = {0.5};

    auto state = make_initial_betting_state(config);
    auto bet = legal_betting_actions(state, config.abstraction)[1];
    BOOST_REQUIRE(bet.kind == betting_action_kind::bet);
    auto facing = apply_betting_action(state, bet, config.abstraction);
    BOOST_REQUIRE(facing.has_value());
    auto call = legal_betting_actions(*facing, config.abstraction)[1];
    BOOST_REQUIRE(call.kind == betting_action_kind::call);
    auto settled = apply_betting_action(*facing, call, config.abstraction);
    BOOST_REQUIRE(settled.has_value());

    const auto river = detail::make_next_street_state(*settled, holdem_street::river, config.root_actor);
    BOOST_CHECK_EQUAL(river.committed[0], 100.0);
    BOOST_CHECK_EQUAL(river.committed[1], 100.0);
    BOOST_CHECK_EQUAL(river.street_committed[0], 0.0);
    BOOST_CHECK_EQUAL(river.street_committed[1], 0.0);
    BOOST_CHECK_EQUAL(river.current_bet, 0.0);
    BOOST_CHECK(validate_betting_state(river).has_value());
    const auto river_actions = legal_betting_actions(river, config.abstraction);
    BOOST_CHECK(river_actions[0].kind == betting_action_kind::check);
    BOOST_CHECK(river_actions[1].kind == betting_action_kind::bet);
    BOOST_CHECK_EQUAL(river_actions[1].amount, 100.0);
}

BOOST_AUTO_TEST_CASE(betting_transitions_are_pure_and_reach_river_showdown) {
    auto config = tiny_river_betting_config();
    auto state = make_initial_betting_state(config);
    auto actions = legal_betting_actions(state, config.abstraction);

    auto called = apply_betting_action(state, actions[1], config.abstraction);

    BOOST_REQUIRE(called.has_value());
    BOOST_CHECK_EQUAL(state.stacks[0], 99.0);
    BOOST_CHECK_EQUAL(called->stacks[0], 98.0);
    BOOST_CHECK_EQUAL(called->committed[0], 2.0);
    BOOST_CHECK_EQUAL(called->actor, 1u);
    BOOST_CHECK(!called->terminal());

    auto response_actions = legal_betting_actions(*called, config.abstraction);
    BOOST_REQUIRE(!response_actions.empty());
    BOOST_CHECK(response_actions.front().kind == betting_action_kind::check);
    auto checked = apply_betting_action(*called, response_actions.front(), config.abstraction);

    BOOST_REQUIRE(checked.has_value());
    BOOST_CHECK(checked->terminal());
    BOOST_CHECK(checked->terminal_kind == zeta::holdem::terminal_state_kind::showdown);
}

BOOST_AUTO_TEST_CASE(invalid_betting_states_and_illegal_actions_return_typed_errors) {
    auto config = tiny_river_betting_config();
    auto state = make_initial_betting_state(config);
    state.folded.set_folded(0, true);

    auto invalid = validate_betting_state(state);

    BOOST_REQUIRE(!invalid);
    BOOST_CHECK(invalid.error().kind == betting_validation_error_kind::invalid_actor);

    state = make_initial_betting_state(config);
    auto illegal = apply_betting_action(
        state,
        betting_action{
            .kind = betting_action_kind::check,
            .amount = 0.0,
            .target_bet = state.committed[0]
        },
        config.abstraction);

    BOOST_REQUIRE(!illegal);
    BOOST_CHECK(illegal.error().kind == betting_validation_error_kind::illegal_action);
}

BOOST_AUTO_TEST_CASE(generated_river_graph_lowers_to_solver_metadata_and_terminal_states) {
    auto lowered = lower_betting_tree_to_graph(tiny_river_betting_config());

    BOOST_REQUIRE(lowered.has_value());
    BOOST_REQUIRE(validate_solver_graph_view(make_solver_graph_view<2>(lowered->graph, lowered->annotations)).has_value());
    BOOST_CHECK_EQUAL(lowered->graph.root_node, lowered->graph.node_count - 1u);
    BOOST_CHECK_GT(lowered->graph.terminal_count, 0u);
    BOOST_CHECK_EQUAL(lowered->terminal_states.size(), lowered->graph.terminal_count);
    BOOST_CHECK_EQUAL(lowered->terminal_leaves.size(), lowered->graph.node_count);

    uint32_t terminal_leaf_count = 0;
    for (uint32_t node_id = 0; node_id < lowered->graph.node_count; ++node_id) {
        if (!lowered->graph.is_terminal(node_id)) {
            continue;
        }
        const auto terminal_id = lowered->terminal_leaves[node_id].terminal_state_id;
        BOOST_REQUIRE(lowered->terminal_states.contains(terminal_id));
        BOOST_CHECK(lowered->terminal_states[terminal_id].kind != zeta::holdem::terminal_state_kind::none);
        ++terminal_leaf_count;
    }
    BOOST_CHECK_EQUAL(terminal_leaf_count, lowered->graph.terminal_count);
}

BOOST_AUTO_TEST_CASE(generated_graph_hash_is_deterministic_for_identical_configs) {
    auto lhs = lower_betting_tree_to_graph(tiny_river_betting_config());
    auto rhs = lower_betting_tree_to_graph(tiny_river_betting_config());

    BOOST_REQUIRE(lhs.has_value());
    BOOST_REQUIRE(rhs.has_value());
    BOOST_CHECK_EQUAL(lhs->deterministic_hash, rhs->deterministic_hash);
    BOOST_CHECK_NE(lhs->deterministic_hash, 0u);
}

BOOST_AUTO_TEST_CASE(generated_graph_preflight_rejects_trees_too_large_to_count) {
    BOOST_CHECK(betting_tree_node_bound(tiny_river_betting_config()).has_value());

    // With at least three actions per state, a 65535-deep bound overflows 64 bits, so
    // the tree is rejected before any node is built.
    auto config = tiny_river_betting_config();
    config.max_history = std::numeric_limits<uint16_t>::max();
    BOOST_CHECK(!betting_tree_node_bound(config).has_value());

    auto lowered = lower_betting_tree_to_graph(config);
    BOOST_REQUIRE(!lowered);
    BOOST_CHECK(lowered.error().kind == betting_validation_error_kind::memory_plan_failed);
}

BOOST_AUTO_TEST_CASE(stage5_named_preset_factories_round_trip_through_json) {
    auto single = make_single_size_policy();
    BOOST_REQUIRE(single.has_value());
    const auto single_json = serialize_betting_abstraction_policy(*single);
    auto single_round_trip = deserialize_betting_abstraction_policy(single_json);
    BOOST_REQUIRE(single_round_trip.has_value());
    BOOST_CHECK_EQUAL(hash_betting_abstraction_policy(*single), hash_betting_abstraction_policy(*single_round_trip));

    auto multi = make_multi_size_policy();
    BOOST_REQUIRE(multi.has_value());
    const auto multi_json = serialize_betting_abstraction_policy(*multi);
    auto multi_round_trip = deserialize_betting_abstraction_policy(multi_json);
    BOOST_REQUIRE(multi_round_trip.has_value());
    BOOST_CHECK_EQUAL(hash_betting_abstraction_policy(*multi), hash_betting_abstraction_policy(*multi_round_trip));

    auto geometric = make_geometric_policy();
    BOOST_REQUIRE(geometric.has_value());
    const auto geometric_json = serialize_betting_abstraction_policy(*geometric);
    auto geometric_round_trip = deserialize_betting_abstraction_policy(geometric_json);
    BOOST_REQUIRE(geometric_round_trip.has_value());
    BOOST_CHECK_EQUAL(hash_betting_abstraction_policy(*geometric), hash_betting_abstraction_policy(*geometric_round_trip));

    auto overbet = make_overbet_policy();
    BOOST_REQUIRE(overbet.has_value());
    const auto overbet_json = serialize_betting_abstraction_policy(*overbet);
    auto overbet_round_trip = deserialize_betting_abstraction_policy(overbet_json);
    BOOST_REQUIRE(overbet_round_trip.has_value());
    BOOST_CHECK_EQUAL(hash_betting_abstraction_policy(*overbet), hash_betting_abstraction_policy(*overbet_round_trip));

    auto all_in_inclusive = make_all_in_inclusive_policy();
    BOOST_REQUIRE(all_in_inclusive.has_value());
    const auto all_in_inclusive_json = serialize_betting_abstraction_policy(*all_in_inclusive);
    auto all_in_inclusive_round_trip = deserialize_betting_abstraction_policy(all_in_inclusive_json);
    BOOST_REQUIRE(all_in_inclusive_round_trip.has_value());
    BOOST_CHECK_EQUAL(hash_betting_abstraction_policy(*all_in_inclusive), hash_betting_abstraction_policy(*all_in_inclusive_round_trip));

    std::array<std::vector<actor_size_set>, 5> actor_sizes{};
    actor_sizes[static_cast<std::size_t>(holdem_street::river)].resize(2);
    actor_sizes[static_cast<std::size_t>(holdem_street::river)][0].fractions = {0.5, 1.0};
    actor_sizes[static_cast<std::size_t>(holdem_street::river)][1].raise_multiples = {2.0};
    betting_abstraction_policy actor_policy{};
    actor_policy.street_actor_sizes = actor_sizes;
    actor_policy.max_raises = 2;
    BOOST_REQUIRE(validate_betting_abstraction_policy(actor_policy).has_value());
    const auto actor_json = serialize_betting_abstraction_policy(actor_policy);
    auto actor_round_trip = deserialize_betting_abstraction_policy(actor_json);
    BOOST_REQUIRE(actor_round_trip.has_value());
    BOOST_CHECK_EQUAL(hash_betting_abstraction_policy(actor_policy), hash_betting_abstraction_policy(*actor_round_trip));
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(cfr_tables)

BOOST_AUTO_TEST_CASE(action_layout_from_counts_is_infoset_major) {
    constexpr std::array<uint32_t, 3> action_counts{2u, 0u, 3u};

    auto result = make_action_table_layout(std::span<const uint32_t>{action_counts});

    BOOST_REQUIRE(result.has_value());
    const auto& layout = *result;
    BOOST_REQUIRE_EQUAL(layout.action_offsets.size(), 4u);
    BOOST_CHECK_EQUAL(layout.action_offsets[0], 0u);
    BOOST_CHECK_EQUAL(layout.action_offsets[1], 2u);
    BOOST_CHECK_EQUAL(layout.action_offsets[2], 2u);
    BOOST_CHECK_EQUAL(layout.action_offsets[3], 5u);
    BOOST_CHECK_EQUAL(layout.infoset_count(), 3u);
    BOOST_CHECK_EQUAL(layout.value_count(), 5u);
    BOOST_CHECK_EQUAL(layout.action_count(0), 2u);
    BOOST_CHECK_EQUAL(layout.action_count(1), 0u);
    BOOST_CHECK_EQUAL(layout.action_count(2), 3u);
}

BOOST_AUTO_TEST_CASE(action_layout_from_graph_uses_player_infosets) {
    auto graph = create_simple_tree();

    auto result = make_action_table_layout(graph);

    BOOST_REQUIRE(result.has_value());
    const auto& layout = *result;
    BOOST_REQUIRE_EQUAL(layout.action_offsets.size(), 2u);
    BOOST_CHECK_EQUAL(layout.infoset_count(), 1u);
    BOOST_CHECK_EQUAL(layout.value_count(), 2u);
    BOOST_CHECK_EQUAL(layout.action_count(0), 2u);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(cfr_terminal_state)

BOOST_AUTO_TEST_CASE(river_reach_index_copy_characteristics_are_explicit) {
    using reach_index = zeta::holdem::river_reach_index;

    static_assert(std::is_trivially_copyable_v<reach_index>);
    BOOST_CHECK(std::is_trivially_copyable_v<reach_index>);
    BOOST_TEST_MESSAGE("river_reach_index bytes: " << sizeof(reach_index));
    BOOST_CHECK_GT(sizeof(reach_index), 64u * 1024u);
}

BOOST_AUTO_TEST_CASE(terminal_state_table_carries_main_pot_audit_fields) {
    const auto context = zeta::holdem::make_heads_up_context(200.0, 5.0, 75.0, 125.0);

    zeta::holdem::terminal_state_table<2> terminal_states;
    terminal_states.states.push_back(zeta::holdem::make_showdown_terminal_state(context));

    BOOST_REQUIRE_EQUAL(terminal_states.size(), 1u);
    BOOST_REQUIRE(terminal_states.contains(0));
    const auto& state = terminal_states[0];
    BOOST_CHECK(state.kind == zeta::holdem::terminal_state_kind::showdown);
    BOOST_CHECK_EQUAL(state.context.gross_pot, 200.0);
    BOOST_CHECK_EQUAL(state.context.rake, 5.0);
    BOOST_REQUIRE_EQUAL(state.pot_layers.size(), 1u);
    BOOST_CHECK_EQUAL(state.pot_layers[0].amount, 200.0);
    BOOST_CHECK(state.pot_layers[0].eligible_mask[0]);
    BOOST_CHECK(state.pot_layers[0].eligible_mask[1]);
    BOOST_CHECK(state.pot_layers[0].contributors_mask[0]);
    BOOST_CHECK(state.pot_layers[0].contributors_mask[1]);
    BOOST_CHECK(state.active_eligible_mask[0]);
    BOOST_CHECK(state.active_eligible_mask[1]);
}

BOOST_AUTO_TEST_CASE(fold_terminal_state_tracks_folded_and_eligible_players) {
    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);

    const auto state = zeta::holdem::make_fold_terminal_state(
        context,
        zeta::holdem::heads_up_player::ip);

    BOOST_CHECK(state.kind == zeta::holdem::terminal_state_kind::fold);
    BOOST_CHECK(!state.folded[0]);
    BOOST_CHECK(state.folded[1]);
    BOOST_CHECK(state.active_eligible_mask[0]);
    BOOST_CHECK(!state.active_eligible_mask[1]);
    BOOST_REQUIRE_EQUAL(state.pot_layers.size(), 1u);
    BOOST_CHECK(state.pot_layers[0].contributors_mask[0]);
    BOOST_CHECK(state.pot_layers[0].contributors_mask[1]);
}

BOOST_AUTO_TEST_CASE(betting_state_tracks_side_pots_through_all_in_and_fold) {
    holdem_betting_graph_config<3> config{};
    config.initial_stacks = {50.0, 100.0, 100.0};
    config.root_actor = 0;
    auto state = make_initial_betting_state(config);
    betting_abstraction_policy policy{};
    policy.max_raises = 4;

    auto next = apply_betting_action(
        state,
        betting_action{.kind = betting_action_kind::all_in, .amount = 50.0, .target_bet = 50.0},
        policy
    );
    BOOST_REQUIRE(next.has_value());
    next = apply_betting_action(
        *next,
        betting_action{.kind = betting_action_kind::call, .amount = 50.0, .target_bet = 50.0},
        policy
    );
    BOOST_REQUIRE(next.has_value());
    next = apply_betting_action(
        *next,
        betting_action{.kind = betting_action_kind::all_in, .amount = 100.0, .target_bet = 100.0},
        policy
    );
    BOOST_REQUIRE(next.has_value());
    next = apply_betting_action(
        *next,
        betting_action{.kind = betting_action_kind::fold, .amount = 0.0, .target_bet = 50.0},
        policy
    );
    BOOST_REQUIRE(next.has_value());

    BOOST_REQUIRE_EQUAL(next->pot_layers.size(), 2u);
    BOOST_CHECK_EQUAL(next->pot_layers[0].amount, 150.0);
    BOOST_CHECK(next->pot_layers[0].contributors_mask[0]);
    BOOST_CHECK(next->pot_layers[0].contributors_mask[1]);
    BOOST_CHECK(next->pot_layers[0].contributors_mask[2]);
    BOOST_CHECK(next->pot_layers[0].eligible_mask[0]);
    BOOST_CHECK(!next->pot_layers[0].eligible_mask[1]);
    BOOST_CHECK(next->pot_layers[0].eligible_mask[2]);
    BOOST_CHECK_EQUAL(next->pot_layers[1].amount, 50.0);
    BOOST_CHECK(!next->pot_layers[1].contributors_mask[0]);
    BOOST_CHECK(!next->pot_layers[1].contributors_mask[1]);
    BOOST_CHECK(next->pot_layers[1].contributors_mask[2]);
    BOOST_CHECK(next->pot_layers[1].eligible_mask[2]);
}

BOOST_AUTO_TEST_CASE(runout_terminal_table_materializes_river_cache_by_public_state) {
    const auto river_mask = deterministic_river_board().mask;
    const public_board_state root{
        .id = 0,
        .street = holdem_street::flop,
        .board_cards = 0,
        .is_root_state = true
    };
    const public_board_state river_state{
        .id = 1,
        .street = holdem_street::river,
        .board_cards = river_mask,
        .parent_state_id = 0,
        .chance_event_id_from_parent = 5,
        .chance_outcome_id_from_parent = 7,
        .is_terminal_river_state = true
    };

    const auto table = make_runout_terminal_table(std::array<public_board_state, 2>{root, river_state});
    BOOST_REQUIRE_EQUAL(table.entries.size(), 1u);
    BOOST_REQUIRE_EQUAL(table.entry_id_by_public_state[1], 0u);
    BOOST_CHECK_EQUAL(table.entries[0].public_state, 1u);
    BOOST_CHECK_EQUAL(table.entries[0].river_board.mask, river_mask);
    BOOST_CHECK_EQUAL(table.entries[0].cache.board_hash, static_cast<uint64_t>(river_mask));
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(cfr_chance_events)

BOOST_AUTO_TEST_CASE(chance_event_table_validates_graph_child_alignment_and_probability_sum) {
    auto graph = create_chance_tree();
    auto chance_events = make_uniform_chance_event_table(graph);

    auto result = validate_chance_event_table(graph, chance_events);

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_EQUAL(chance_events.events.size(), 2u);
    BOOST_CHECK_EQUAL(chance_events.outcomes.size(), 4u);
    for (const auto& event : chance_events.events) {
        double probability_sum = 0.0;
        for (const auto& outcome : chance_events.event_outcomes(event)) {
            probability_sum += outcome.probability;
            BOOST_CHECK_EQUAL(outcome.board_partition_id, outcome.action_index);
        }
        BOOST_CHECK_SMALL(probability_sum - 1.0, 0.00001);
    }
}

BOOST_AUTO_TEST_CASE(chance_event_table_rejects_dead_card_collisions) {
    auto graph = create_chance_tree();
    auto chance_events = make_uniform_chance_event_table(graph);
    chance_events.outcomes.front().cards = card(0, 0);
    chance_events.outcomes.front().dead_cards = card(0, 0);

    auto result = validate_chance_event_table(graph, chance_events);

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == chance_table_error_kind::dead_card_collision);
}

BOOST_AUTO_TEST_CASE(chance_event_table_rejects_duplicate_board_cards) {
    auto graph = create_chance_tree();
    auto chance_events = make_uniform_chance_event_table(graph);
    chance_events.events.front().board_cards = card(0, 0);
    chance_events.outcomes.front().cards = card(0, 0);

    auto result = validate_chance_event_table(graph, chance_events);

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == chance_table_error_kind::duplicate_board_card);
}

BOOST_AUTO_TEST_CASE(public_card_chance_enumeration_is_blocker_safe) {
    const auto dead_cards = card(0, 0) | card(1, 1);
    const auto flop_outcomes = enumerate_public_card_outcomes(0, dead_cards, 3);
    const auto turn_outcomes = enumerate_public_card_outcomes(card(2, 2) | card(2, 3) | card(2, 4), dead_cards, 1);
    const auto river_outcomes = enumerate_public_card_outcomes(
        card(2, 2) | card(2, 3) | card(2, 4) | card(3, 5),
        dead_cards,
        1);

    BOOST_CHECK_EQUAL(flop_outcomes.size(), 19600u);
    BOOST_CHECK_EQUAL(turn_outcomes.size(), 47u);
    BOOST_CHECK_EQUAL(river_outcomes.size(), 46u);
    for (const auto& outcome : turn_outcomes) {
        BOOST_CHECK_EQUAL(outcome.cards & dead_cards, 0u);
        BOOST_CHECK_EQUAL(outcome.cards & (card(2, 2) | card(2, 3) | card(2, 4)), 0u);
    }
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(cfr_validation)

/** Test graph validation. */
BOOST_AUTO_TEST_CASE(graph_validation) {
    auto graph = create_simple_tree();
    BOOST_CHECK(::zeta::holdem::cfr::graph_validation::validate(graph));
}

BOOST_AUTO_TEST_CASE(validate_terminal_no_out_edges) {
    auto graph = create_simple_tree();
    
    /** Manually verify terminals have no edges. */
    for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
        if (graph.is_terminal(node_id)) {
            BOOST_CHECK_EQUAL(graph.action_count(node_id), 0u);
        }
    }
}

BOOST_AUTO_TEST_CASE(validate_all_player_nodes_have_infosets) {
    auto graph = create_simple_tree();
    
    for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
        if (graph.is_player_node(node_id)) {
            BOOST_CHECK_NE(graph.infoset_id[node_id], ~0u);
            BOOST_CHECK_LT(graph.infoset_id[node_id], graph.infoset_count);
        }
    }
}

BOOST_AUTO_TEST_CASE(deterministic_build) {
    /** Build the same tree twice and verify they are identical. */
    auto graph1 = create_simple_tree();
    auto graph2 = create_simple_tree();
    
    BOOST_CHECK_EQUAL(graph1.node_count, graph2.node_count);
    BOOST_CHECK_EQUAL(graph1.terminal_count, graph2.terminal_count);
    BOOST_CHECK_EQUAL(graph1.edges.size(), graph2.edges.size());
    BOOST_CHECK_EQUAL(graph1.row_offsets.size(), graph2.row_offsets.size());
    
    for (size_t i = 0; i < graph1.edges.size(); ++i) {
        BOOST_CHECK_EQUAL(graph1.edges[i].child_node, graph2.edges[i].child_node);
        BOOST_CHECK_EQUAL(graph1.edges[i].action_index, graph2.edges[i].action_index);
    }
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(cfr_edge_cases)

/** Edge cases and stress tests. */
BOOST_AUTO_TEST_CASE(single_node_graph) {
    graph_builder builder;
    (void) builder.add_node(node_kind::terminal);
    
    auto graph = require_graph(builder.build());
    BOOST_CHECK_EQUAL(graph.node_count, 1u);
    BOOST_CHECK_EQUAL(graph.terminal_count, 1u);
    BOOST_CHECK(::zeta::holdem::cfr::graph_validation::validate(graph));
}

BOOST_AUTO_TEST_CASE(chain_graph) {
    graph_builder builder;
    
    /** Create linear chain: 0 -> 1 -> 2 -> ... -> 9. */
    for (int i = 0; i < 10; ++i) {
        (void) builder.add_node(i == 0 ? node_kind::player :
                                i == 9 ? node_kind::terminal : node_kind::chance);
    }
    
    for (int i = 0; i < 9; ++i) {
        builder.add_edge(i, i + 1, 0);
    }
    
    builder.set_infoset_id(0, 0);
    
    auto graph = require_graph(builder.build());
    BOOST_CHECK_EQUAL(graph.node_count, 10u);
    BOOST_CHECK_EQUAL(graph.terminal_count, 1u);
    BOOST_CHECK(::zeta::holdem::cfr::graph_validation::validate(graph));
}

BOOST_AUTO_TEST_CASE(traversal_scan_throughput) {
    auto graph = create_chance_tree();
    
    /** Verify we can traverse all edges efficiently. */
    uint64_t total_edges = 0;
    for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
        auto edges = graph.out_edges(node_id);
        total_edges += edges.size();
    }
    
    BOOST_CHECK_EQUAL(total_edges, graph.edges.size());
}

BOOST_AUTO_TEST_CASE(build_reports_empty_graph) {
    graph_builder builder;

    auto result = builder.build();

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == graph_build_error_kind::empty_graph);
}

BOOST_AUTO_TEST_CASE(build_reports_uninitialized_infoset) {
    graph_builder builder;
    auto root = builder.add_node(node_kind::player);
    auto terminal = builder.add_node(node_kind::terminal);
    builder.add_edge(root, terminal, 0);

    auto result = builder.build();

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == graph_build_error_kind::uninitialized_infoset);
}

BOOST_AUTO_TEST_CASE(build_reports_invalid_builder_edge_endpoint) {
    graph_builder builder;
    auto root = builder.add_node(node_kind::player);
    (void) builder.add_node(node_kind::terminal);
    builder.add_edge(root, 42, 0);
    builder.set_infoset_id(root, 0);

    auto result = builder.build();

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == graph_build_error_kind::invalid_graph);
}

BOOST_AUTO_TEST_CASE(validate_rejects_degree_larger_than_node_count_without_unsafe_action_indexing) {
    game_graph graph;
    graph.node_count = 3;
    graph.terminal_count = 2;
    graph.infoset_count = 1;
    graph.root_node = 2;
    graph.max_depth = 1;
    graph.row_offsets = {0, 0, 0, 4};
    graph.edges = {
        {0, 0},
        {1, 3},
        {0, 1},
        {1, 2}
    };
    graph.node_types = {
        node_kind::terminal,
        node_kind::terminal,
        node_kind::player
    };
    graph.infoset_id = {
        game_graph::INVALID_INFOSET,
        game_graph::INVALID_INFOSET,
        0
    };
    graph.node_depth = {1, 1, 0};
    graph.subtree_size = {1, 1, 5};

    BOOST_CHECK(!::zeta::holdem::cfr::graph_validation::validate(graph));
}

BOOST_AUTO_TEST_CASE(validate_rejects_multiple_parents) {
    game_graph graph;
    graph.node_count = 4;
    graph.terminal_count = 1;
    graph.infoset_count = 1;
    graph.max_depth = 2;
    graph.row_offsets = {0, 0, 1, 2, 4};
    graph.edges = {
        {0, 0},
        {0, 0},
        {1, 0},
        {2, 1}
    };
    graph.node_types = {
        node_kind::terminal,
        node_kind::chance,
        node_kind::chance,
        node_kind::player
    };
    graph.infoset_id = {
        game_graph::INVALID_INFOSET,
        game_graph::INVALID_INFOSET,
        game_graph::INVALID_INFOSET,
        0
    };
    graph.node_depth = {2, 1, 1, 0};
    graph.subtree_size = {1, 2, 2, 5};

    BOOST_CHECK(!::zeta::holdem::cfr::graph_validation::validate(graph));
}

BOOST_AUTO_TEST_CASE(validate_rejects_duplicate_destinations) {
    game_graph graph;
    graph.node_count = 3;
    graph.terminal_count = 1;
    graph.infoset_count = 1;
    graph.max_depth = 1;
    graph.row_offsets = {0, 0, 0, 2};
    graph.edges = {
        {0, 0},
        {0, 1}
    };
    graph.node_types = {
        node_kind::terminal,
        node_kind::terminal,
        node_kind::player
    };
    graph.infoset_id = {
        game_graph::INVALID_INFOSET,
        game_graph::INVALID_INFOSET,
        0
    };
    graph.node_depth = {1, 1, 0};
    graph.subtree_size = {1, 1, 3};

    BOOST_CHECK(!::zeta::holdem::cfr::graph_validation::validate(graph));
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(cfr_scheduler)

BOOST_AUTO_TEST_CASE(worker_pool_runs_every_task_exactly_once) {
    for (const uint32_t workers : {1u, 2u, 8u}) {
        zeta::holdem::cfr::scheduler::worker_pool pool{workers};
        BOOST_CHECK_EQUAL(pool.worker_count(), workers);
        for (const uint32_t chunk_size : {1u, 3u, 64u}) {
            std::vector<std::atomic<uint32_t>> runs(1000);
            std::atomic<bool> worker_id_out_of_range{false};
            pool.run(runs.size(), [&](const uint32_t worker_id, const uint64_t task_index) {
                if (worker_id >= workers) {
                    worker_id_out_of_range = true;
                }
                runs[task_index].fetch_add(1u);
            }, chunk_size);
            BOOST_CHECK(!worker_id_out_of_range);
            BOOST_CHECK(std::ranges::all_of(runs, [](const auto& count) { return count.load() == 1u; }));
        }
    }
}

BOOST_AUTO_TEST_CASE(worker_pool_is_reusable_across_batches_and_empty_batches) {
    zeta::holdem::cfr::scheduler::worker_pool pool{4};
    std::atomic<bool> empty_batch_ran{false};
    pool.run(0, [&](uint32_t, uint64_t) { empty_batch_ran = true; });
    BOOST_CHECK(!empty_batch_ran);

    std::atomic<uint64_t> sum{0};
    for (int batch = 0; batch < 50; ++batch) {
        pool.run(100, [&](uint32_t, const uint64_t task_index) { sum += task_index; });
    }
    BOOST_CHECK_EQUAL(sum.load(), 50u * 4950u);
}

BOOST_AUTO_TEST_CASE(worker_pool_rethrows_the_first_task_exception_and_stays_usable) {
    for (const uint32_t workers : {1u, 4u}) {
        zeta::holdem::cfr::scheduler::worker_pool pool{workers};
        BOOST_CHECK_THROW(
            pool.run(1000, [](uint32_t, const uint64_t task_index) {
                if (task_index == 500u) {
                    throw std::runtime_error("task failed");
                }
            }),
            std::runtime_error);

        std::atomic<uint64_t> count{0};
        pool.run(10, [&](uint32_t, uint64_t) { ++count; });
        BOOST_CHECK_EQUAL(count.load(), 10u);
    }
}

BOOST_AUTO_TEST_SUITE_END()
