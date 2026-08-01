#include <boost/test/unit_test.hpp>

#include "cfr/graph/builder.h"
#include "cfr/graph/graph.h"
#include "cfr/graph/validation.h"
#include "cfr/scheduler/dfs_partitioner.h"
#include "cfr/scheduler/scheduler.h"
#include "cfr/solver/context.h"
#include "cfr/solver/infoset_planning.h"
#include "cfr/solver/iteration.h"
#include "cfr/solver/metadata.h"
#include "cfr/solver/river_context.h"
#include "cfr/tables/delta_buffer.h"
#include "cfr/traversal/traversal.h"

#include <array>
#include <atomic>
#include <cmath>
#include <future>
#include <sstream>
#include <type_traits>

using namespace zeta::holdem::cfr;
using namespace zeta::holdem::cfr::scheduler;
using namespace zeta::holdem::cfr::solver;
using namespace zeta::holdem::cfr::traversal;

namespace {
    constexpr uint32_t DEFAULT_TEST_PARTITION_COUNT = 8;
    constexpr uint32_t DEFAULT_TEST_WORK_DEPTH_SHIFT = 16;

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

    std::pair<zeta::holdem::combination_index, zeta::holdem::combination_index> first_compatible_live_combos(
        const zeta::holdem::river_terminal_cache& cache)
    {
        for (std::size_t lhs_order = 0; lhs_order < cache.rank_order_count; ++lhs_order) {
            const auto lhs = cache.rank_order[lhs_order];
            for (std::size_t rhs_order = lhs_order + 1; rhs_order < cache.rank_order_count; ++rhs_order) {
                const auto rhs = cache.rank_order[rhs_order];
                if ((cache.masks[lhs] & cache.masks[rhs]) == 0) {
                    return {lhs, rhs};
                }
            }
        }

        BOOST_FAIL("compatible live combos not found");
        return {0, 0};
    }

    std::array<zeta::holdem::combination_index, 3> first_three_compatible_live_combos(
        const zeta::holdem::river_terminal_cache& cache)
    {
        for (std::size_t first_order = 0; first_order < cache.rank_order_count; ++first_order) {
            const auto first = cache.rank_order[first_order];
            for (std::size_t second_order = first_order + 1; second_order < cache.rank_order_count; ++second_order) {
                const auto second = cache.rank_order[second_order];
                if ((cache.masks[first] & cache.masks[second]) != 0) {
                    continue;
                }
                for (std::size_t third_order = second_order + 1; third_order < cache.rank_order_count; ++third_order) {
                    const auto third = cache.rank_order[third_order];
                    if (((cache.masks[first] | cache.masks[second]) & cache.masks[third]) == 0) {
                        return {first, second, third};
                    }
                }
            }
        }

        BOOST_FAIL("three compatible live combos not found");
        return {0, 0, 0};
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

std::vector<graph_partition> require_partitions(
    std::expected<std::vector<graph_partition>, dfs_partitioner_error> result)
{
    if (!result) {
        BOOST_ERROR("partition build failed: " << result.error().kind);
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

void require_prepared_worker(
    worker_context& worker,
    const game_graph& graph,
    const regret_table& regrets)
{
    auto result = prepare_worker_context(worker, graph, regrets);
    if (!result) {
        BOOST_ERROR("worker context prepare failed: " << result.error().kind);
    }
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
 * Helper to create a slightly larger tree for partition testing:
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

BOOST_AUTO_TEST_SUITE(cfr_graph_types)

/** Check that graph types are properly defined. */
BOOST_AUTO_TEST_CASE(edge_structure) {
    edge e{42, 3};
    BOOST_CHECK_EQUAL(e.child_node, 42u);
    BOOST_CHECK_EQUAL(e.action_index, 3u);
}

BOOST_AUTO_TEST_CASE(graph_partition_structure) {
    graph_partition p{};
    p.begin_node = 0;
    p.end_node = 10;
    p.node_count = 10;
    p.terminal_count = 5;
    p.action_count = 20;
    p.min_depth = 1;
    p.max_depth = 3;
    p.estimated_work = 200;
    
    BOOST_CHECK_EQUAL(p.begin_node, 0u);
    BOOST_CHECK_EQUAL(p.end_node, 10u);
    BOOST_CHECK_EQUAL(p.node_count, 10u);
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
    BOOST_CHECK(!graph.is_chance_node(2));
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

BOOST_AUTO_TEST_CASE(solver_graph_view_rejects_sample_chance_mode_until_supported) {
    auto graph = create_chance_tree();
    auto annotations = make_default_annotations(graph);

    auto result = validate_solver_graph_view(make_solver_graph_view<2>(graph, annotations), chance_mode::sample);

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == solver_graph_metadata_error_kind::unsupported_chance_mode);
}

BOOST_AUTO_TEST_CASE(solver_compatibility_key_changes_with_policy_and_player_count) {
    auto graph = create_simple_tree();
    auto annotations = make_default_annotations(graph);
    auto layout = require_layout(make_action_table_layout(graph));
    const auto view = make_solver_graph_view<2>(graph, annotations);

    const auto baseline = make_solver_compatibility_key(view, layout);
    const auto float64_tables = make_solver_compatibility_key(
        view,
        layout,
        numeric_policy{
            .table_storage = table_storage_precision::float64,
            .accumulation = accumulation_precision::float64
        });
    const auto three_player = make_solver_compatibility_key(
        make_solver_graph_view<3>(graph, annotations),
        layout);

    BOOST_CHECK_NE(baseline.numeric_policy_hash, float64_tables.numeric_policy_hash);
    BOOST_CHECK_NE(baseline.graph_metadata_hash, three_player.graph_metadata_hash);
    BOOST_CHECK_NE(baseline.player_count, three_player.player_count);
}

BOOST_AUTO_TEST_CASE(player_mask_backs_generic_terminal_masks) {
    zeta::holdem::player_mask<4> active;
    active.set(1);
    active.set(3);

    zeta::holdem::folded_mask<4> folded;
    folded.set_folded(2, true);
    zeta::holdem::pot_structure<4> pot;
    pot.active = active;
    pot.initialize_main_pot(100.0);

    BOOST_CHECK(active[1]);
    BOOST_CHECK(active[3]);
    BOOST_CHECK_EQUAL(active.count(), 2u);
    BOOST_CHECK(folded[2]);
    BOOST_REQUIRE_EQUAL(pot.pots.size(), 1u);
    BOOST_CHECK(pot.pots[0].eligible[1]);
    BOOST_CHECK(pot.pots[0].eligible[3]);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(cfr_infoset_planning)

holdem_infoset_key test_infoset_key(
    const uint8_t actor,
    const uint16_t player_count,
    const uint32_t legal_action_set_id)
{
    return holdem_infoset_key{
        .actor = actor,
        .street = holdem_street::river,
        .player_count = player_count,
        .private_hand_class_id = 13,
        .public_board_abstraction_id = 17,
        .chance_runout_class_id = 19,
        .betting_history_abstraction_id = 23,
        .stack_pot_abstraction_id = 29,
        .legal_action_set_id = legal_action_set_id,
        .subgame_root_context_id = 31
    };
}

std::vector<uint16_t> legal_action_ids_for_node(const game_graph& graph, const uint32_t node_id)
{
    std::vector<uint16_t> action_ids;
    action_ids.reserve(graph.action_count(node_id));
    for (const auto& edge : graph.out_edges(node_id)) {
        action_ids.push_back(edge.action_index);
    }
    return action_ids;
}

BOOST_AUTO_TEST_CASE(holdem_infoset_key_policy_exposes_explicit_abstraction_hooks) {
    constexpr exact_holdem_abstraction_policy policy;
    constexpr solver_node_state_metadata state{
        .street = holdem_street::river,
        .public_state_id = 42,
        .betting_state_id = 99
    };

    BOOST_CHECK_EQUAL(policy.private_hand_class_id(7), 7u);
    BOOST_CHECK_EQUAL(policy.public_board_abstraction_id(state), 42u);
    BOOST_CHECK_EQUAL(policy.chance_runout_class_id(11), 11u);
}

BOOST_AUTO_TEST_CASE(holdem_infoset_lowering_assigns_dense_ids_before_table_layout) {
    auto graph = create_chance_tree();
    std::vector<holdem_infoset_description> descriptions;
    for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
        if (!graph.is_player_node(node_id)) {
            continue;
        }
        descriptions.push_back(holdem_infoset_description{
            .node_id = node_id,
            .key = test_infoset_key(0, 2, 5),
            .owner_id = 1,
            .legal_action_ids = legal_action_ids_for_node(graph, node_id)
        });
    }

    auto lowering = lower_holdem_infoset_keys<2>(graph, descriptions, 2);

    BOOST_REQUIRE(lowering.has_value());
    BOOST_REQUIRE(validate_holdem_infoset_lowering(graph, *lowering).has_value());
    BOOST_CHECK_EQUAL(lowering->infoset_count(), 1u);
    BOOST_CHECK_EQUAL(lowering->owner_by_infoset[0], 1u);
    BOOST_CHECK_EQUAL(lowering->dense_id_by_node[graph.root_node], 0u);
    BOOST_REQUIRE_EQUAL(lowering->legal_actions(0).size(), 2u);
    BOOST_CHECK_EQUAL(lowering->legal_actions(0)[0], 0u);
    BOOST_CHECK_EQUAL(lowering->legal_actions(0)[1], 1u);
}

BOOST_AUTO_TEST_CASE(holdem_infoset_lowering_rejects_conflicting_shared_infoset_identity) {
    auto graph = create_shared_infoset_tree();
    std::vector<uint32_t> player_nodes;
    for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
        if (graph.is_player_node(node_id)) {
            player_nodes.push_back(node_id);
        }
    }
    BOOST_REQUIRE_EQUAL(player_nodes.size(), 2u);

    std::vector<holdem_infoset_description> descriptions{
        holdem_infoset_description{
            .node_id = player_nodes[0],
            .key = test_infoset_key(0, 2, 5),
            .owner_id = 0,
            .legal_action_ids = legal_action_ids_for_node(graph, player_nodes[0])
        },
        holdem_infoset_description{
            .node_id = player_nodes[1],
            .key = test_infoset_key(1, 2, 5),
            .owner_id = 0,
            .legal_action_ids = legal_action_ids_for_node(graph, player_nodes[1])
        }
    };

    auto lowering = lower_holdem_infoset_keys<2>(graph, descriptions, 1);

    BOOST_REQUIRE(!lowering);
    BOOST_CHECK(lowering.error().kind == holdem_infoset_error_kind::inconsistent_shared_infoset);
    BOOST_CHECK_EQUAL(lowering.error().infoset_id, 0u);
}

BOOST_AUTO_TEST_CASE(cfr_memory_plan_estimates_table_dominated_storage_and_limits) {
    auto graph = create_chance_tree();
    auto layout = require_layout(make_action_table_layout(graph));
    const cfr_memory_plan_options options{
        .worker_count = 4,
        .terminal_state_count = graph.terminal_count,
        .chance_event_count = 2
    };

    auto estimate = estimate_cfr_memory(
        graph,
        layout,
        options);

    BOOST_REQUIRE(estimate.has_value());
    BOOST_CHECK_EQUAL(estimate->action_values, layout.value_count());
    BOOST_CHECK_EQUAL(estimate->regret_bytes, layout.value_count() * sizeof(float));
    BOOST_CHECK_EQUAL(estimate->strategy_sum_bytes, layout.value_count() * sizeof(float));
    BOOST_CHECK_GT(estimate->total_bytes, estimate->regret_bytes + estimate->strategy_sum_bytes);

    auto limited = estimate_cfr_memory(
        graph,
        layout,
        options,
        cfr_memory_plan_limits{.max_total_bytes = estimate->total_bytes - 1u});

    BOOST_REQUIRE(!limited);
    BOOST_CHECK(limited.error().kind == cfr_memory_plan_error_kind::total_byte_limit_exceeded);
    BOOST_CHECK_EQUAL(limited.error().required, estimate->total_bytes);
}

BOOST_AUTO_TEST_CASE(planned_cfr_context_rejects_memory_limits_before_table_allocation) {
    auto graph = create_chance_tree();
    const zeta::holdem::board river = deterministic_river_board();
    std::array<zeta::holdem::reach_vector, 2> ranges{};
    std::vector<river_terminal_leaf> terminal_leaves(graph.node_count);
    auto river_context = make_river_solver_context(
        river,
        ranges,
        zeta::holdem::terminal_state_table<2>{},
        std::move(terminal_leaves));

    auto context = make_planned_cfr_context(
        std::move(graph),
        std::move(river_context),
        cfr_memory_plan_options{.worker_count = 1},
        cfr_memory_plan_limits{.max_action_values = 1});

    BOOST_REQUIRE(!context);
    BOOST_CHECK(context.error().kind == cfr_context_planning_error_kind::memory_plan);
    BOOST_CHECK(context.error().memory_plan.kind == cfr_memory_plan_error_kind::action_value_limit_exceeded);
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
    BOOST_CHECK_EQUAL(layout.offset(2, 2), 4u);
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

BOOST_AUTO_TEST_CASE(regret_and_strategy_tables_have_indexed_accessors) {
    constexpr std::array<uint32_t, 2> action_counts{2u, 3u};
    auto layout_result = make_action_table_layout(std::span<const uint32_t>{action_counts});
    BOOST_REQUIRE(layout_result.has_value());
    const auto& layout = *layout_result;

    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);

    BOOST_CHECK_EQUAL(regrets.infoset_count(), 2u);
    BOOST_CHECK_EQUAL(regrets.value_count(), 5u);
    BOOST_CHECK_EQUAL(strategy_sums.infoset_count(), 2u);
    BOOST_CHECK_EQUAL(strategy_sums.value_count(), 5u);

    regrets.value(0, 1) = 1.5f;
    regrets.value(1, 2) = -2.0f;
    strategy_sums.value(1, 0) = 0.25f;
    strategy_sums.value(1, 2) = 0.75f;

    auto regret_span = regrets.infoset_regrets(1);
    auto strategy_span = strategy_sums.infoset_sums(1);

    BOOST_REQUIRE_EQUAL(regret_span.size(), 3u);
    BOOST_REQUIRE_EQUAL(strategy_span.size(), 3u);
    BOOST_CHECK_EQUAL(regrets.offset(1, 2), 4u);
    BOOST_CHECK_EQUAL(regret_span[2], -2.0f);
    BOOST_CHECK_EQUAL(strategy_span[0], 0.25f);
    BOOST_CHECK_EQUAL(strategy_span[2], 0.75f);
}

BOOST_AUTO_TEST_CASE(delta_buffer_accumulates_sparse_thread_local_entries) {
    constexpr std::array<uint32_t, 2> action_counts{2u, 3u};
    auto layout_result = make_action_table_layout(std::span<const uint32_t>{action_counts});
    BOOST_REQUIRE(layout_result.has_value());

    table_delta_buffer buffer(layout_result->action_offsets);

    BOOST_CHECK_GE(alignof(table_delta_buffer), 64u);
    BOOST_CHECK_EQUAL(buffer.infoset_count(), 2u);
    BOOST_CHECK_EQUAL(buffer.entry_count(), 0u);

    buffer.add_regret_delta(1, 2, 3.0f);
    buffer.add_regret_delta(1, 2, 1.0f);
    buffer.add_strategy_delta(1, 0, 0.5f);

    BOOST_REQUIRE_EQUAL(buffer.entry_count(), 1u);
    const auto entry = buffer.entries()[0];
    BOOST_CHECK_EQUAL(entry.infoset_id, 1u);

    auto regret_deltas = buffer.regret_deltas_for(entry);
    auto strategy_deltas = buffer.strategy_deltas_for(entry);

    BOOST_REQUIRE_EQUAL(regret_deltas.size(), 3u);
    BOOST_REQUIRE_EQUAL(strategy_deltas.size(), 3u);
    BOOST_CHECK_EQUAL(regret_deltas[0], 0.0f);
    BOOST_CHECK_EQUAL(regret_deltas[2], 4.0f);
    BOOST_CHECK_EQUAL(strategy_deltas[0], 0.5f);
    BOOST_CHECK_EQUAL(strategy_deltas[2], 0.0f);
}

BOOST_AUTO_TEST_CASE(delta_buffer_clear_preserves_layout_and_resets_sparse_index) {
    constexpr std::array<uint32_t, 2> action_counts{1u, 2u};
    auto layout_result = make_action_table_layout(std::span<const uint32_t>{action_counts});
    BOOST_REQUIRE(layout_result.has_value());

    table_delta_buffer buffer(layout_result->action_offsets);
    buffer.add_regret_delta(1, 1, 2.0f);
    BOOST_REQUIRE_EQUAL(buffer.entry_count(), 1u);

    buffer.clear();

    BOOST_CHECK_EQUAL(buffer.entry_count(), 0u);
    BOOST_CHECK_EQUAL(buffer.infoset_count(), 2u);

    buffer.add_regret_delta(1, 1, 3.0f);

    BOOST_REQUIRE_EQUAL(buffer.entry_count(), 1u);
    auto deltas = buffer.regret_deltas_for(buffer.entries()[0]);
    BOOST_REQUIRE_EQUAL(deltas.size(), 2u);
    BOOST_CHECK_EQUAL(deltas[1], 3.0f);
}

BOOST_AUTO_TEST_CASE(delta_buffer_reduces_into_global_tables) {
    constexpr std::array<uint32_t, 2> action_counts{2u, 1u};
    auto layout_result = make_action_table_layout(std::span<const uint32_t>{action_counts});
    BOOST_REQUIRE(layout_result.has_value());
    const auto& layout = *layout_result;

    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    table_delta_buffer buffer(layout.action_offsets);

    buffer.add_regret_delta(0, 0, 1.0f);
    buffer.add_regret_delta(0, 1, -0.5f);
    buffer.add_regret_delta(1, 0, 2.0f);
    buffer.add_strategy_delta(0, 1, 0.25f);
    buffer.add_strategy_delta(1, 0, 0.75f);

    apply_delta_buffer(regrets, strategy_sums, buffer);

    BOOST_CHECK_EQUAL(regrets.value(0, 0), 1.0f);
    BOOST_CHECK_EQUAL(regrets.value(0, 1), -0.5f);
    BOOST_CHECK_EQUAL(regrets.value(1, 0), 2.0f);
    BOOST_CHECK_EQUAL(strategy_sums.value(0, 0), 0.0f);
    BOOST_CHECK_EQUAL(strategy_sums.value(0, 1), 0.25f);
    BOOST_CHECK_EQUAL(strategy_sums.value(1, 0), 0.75f);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(cfr_traversal)

BOOST_AUTO_TEST_CASE(traversal_frame_is_compact_trivial_state) {
    BOOST_CHECK(std::is_trivially_copyable_v<traversal_frame>);
    BOOST_CHECK_LE(sizeof(traversal_frame), 32u);

    traversal_frame frame{};
    frame.node_id = 7;
    frame.next_edge_offset = 3;
    frame.reach_oop = 1.0f;
    frame.reach_ip = 0.5f;
    frame.chance_weight = 0.25f;
    frame.phase = traversal_phase::visit_children;

    BOOST_CHECK_EQUAL(frame.node_id, 7u);
    BOOST_CHECK_EQUAL(frame.next_edge_offset, 3u);
    BOOST_CHECK(frame.phase == traversal_phase::visit_children);
}

BOOST_AUTO_TEST_CASE(worker_context_is_cache_line_aligned) {
    BOOST_CHECK_GE(alignof(worker_context), 64u);
    BOOST_CHECK_GE(alignof(table_delta_buffer), 64u);
}

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

BOOST_AUTO_TEST_CASE(worker_context_binds_graph_tables_and_terminal_views) {
    auto graph = create_chance_tree();
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    ::zeta::holdem::river_terminal_cache river_cache{};
    std::array<::zeta::holdem::river_reach_index, 2> reach_indices{};

    worker_context worker;
    auto result = prepare_worker_context(worker, graph, regrets, &river_cache, reach_indices);

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_EQUAL(worker.inputs.graph, &graph);
    BOOST_CHECK_EQUAL(worker.inputs.regrets, &regrets);
    BOOST_CHECK_EQUAL(worker.inputs.river_cache, &river_cache);
    BOOST_CHECK_EQUAL(worker.inputs.river_reach_indices.size(), 2u);
    BOOST_CHECK(worker.inputs.has_graph_tables());
    BOOST_CHECK(worker.inputs.has_river_terminal_views());
}

BOOST_AUTO_TEST_CASE(traversal_rejects_unbound_worker_context) {
    worker_context worker;

    auto result = traverse_game_tree(worker);

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == traversal_error_kind::unbound_worker_context);
}

BOOST_AUTO_TEST_CASE(full_tree_traversal_counts_nodes_and_writes_local_deltas) {
    auto graph = create_chance_tree();
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    regrets.value(0, 0) = 2.0f;
    regrets.value(0, 1) = 1.0f;

    worker_context worker;
    require_prepared_worker(worker, graph, regrets);

    traversal_config config;
    config.initial_reach_oop = 2.0f;
    config.initial_reach_ip = 1.0f;

    auto result = traverse_game_tree(worker, config);

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_EQUAL(result->root_node, graph.root_node);
    BOOST_CHECK_SMALL(result->root_utility - 1.0f, 0.00001f);
    BOOST_CHECK_EQUAL(result->diagnostics.nodes_visited, 7u);
    BOOST_CHECK_EQUAL(result->diagnostics.edges_scanned, 6u);
    BOOST_CHECK_EQUAL(result->diagnostics.terminal_nodes, 4u);
    BOOST_CHECK_EQUAL(result->diagnostics.player_nodes, 1u);
    BOOST_CHECK_EQUAL(result->diagnostics.chance_nodes, 2u);
    BOOST_CHECK_EQUAL(result->diagnostics.max_stack_depth, 3u);
    BOOST_CHECK_EQUAL(result->diagnostics.max_action_count, 2u);
    BOOST_CHECK_EQUAL(result->diagnostics.local_delta_entries_touched, 1u);

    BOOST_REQUIRE_EQUAL(worker.delta_buffer.entry_count(), 1u);
    const auto entry = worker.delta_buffer.entries()[0];
    BOOST_CHECK_EQUAL(entry.infoset_id, 0u);

    auto strategy_deltas = worker.delta_buffer.strategy_deltas_for(entry);
    BOOST_REQUIRE_EQUAL(strategy_deltas.size(), 2u);
    BOOST_CHECK_SMALL(strategy_deltas[0] - 2.0f, 0.00001f);
    BOOST_CHECK_SMALL(strategy_deltas[1] - 1.0f, 0.00001f);
}

BOOST_AUTO_TEST_CASE(traversal_uses_action_index_order_for_strategy_probabilities) {
    graph_builder builder;

    auto root = builder.add_node(node_kind::player);
    auto action_one_terminal = builder.add_node(node_kind::terminal);
    auto action_zero_terminal = builder.add_node(node_kind::terminal);

    builder.add_edge(root, action_one_terminal, 1);
    builder.add_edge(root, action_zero_terminal, 0);
    builder.set_infoset_id(root, 0);

    auto graph = require_graph(builder.build());
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    regrets.value(0, 0) = 0.0f;
    regrets.value(0, 1) = 4.0f;

    worker_context worker;
    require_prepared_worker(worker, graph, regrets);

    auto result = traverse_game_tree(worker);

    BOOST_REQUIRE(result.has_value());
    const auto begin = graph.row_offsets[graph.root_node];
    BOOST_REQUIRE_EQUAL(graph.row_offsets[graph.root_node + 1] - begin, 2u);
    BOOST_CHECK_EQUAL(graph.edges[begin].action_index, 0u);
    BOOST_CHECK_EQUAL(graph.edges[begin + 1].action_index, 1u);
    BOOST_CHECK_SMALL(worker.edge_probability[begin], 0.00001f);
    BOOST_CHECK_SMALL(worker.edge_probability[begin + 1] - 1.0f, 0.00001f);
}

BOOST_AUTO_TEST_CASE(traversal_rejects_too_small_stack_without_overflowing) {
    auto graph = create_chance_tree();
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);

    worker_context worker;
    require_prepared_worker(worker, graph, regrets);
    worker.stack.resize(2);

    auto result = traverse_game_tree(worker);

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == traversal_error_kind::stack_capacity_exceeded);
    BOOST_CHECK_EQUAL(result.error().required_capacity, 3u);
    BOOST_CHECK_EQUAL(result.error().available_capacity, 2u);
}

BOOST_AUTO_TEST_CASE(traversal_keeps_worker_storage_capacity_stable_after_setup) {
    auto graph = create_chance_tree();
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);

    worker_context worker;
    require_prepared_worker(worker, graph, regrets);

    const auto stack_capacity = worker.stack.capacity();
    const auto node_utility_capacity = worker.node_utility.capacity();
    const auto child_action_value_capacity = worker.child_action_value.capacity();
    const auto edge_probability_capacity = worker.edge_probability.capacity();
    const auto delta_entry_capacity = worker.delta_buffer.entry_capacity();
    const auto regret_delta_capacity = worker.delta_buffer.regret_delta_capacity();
    const auto strategy_delta_capacity = worker.delta_buffer.strategy_delta_capacity();

    for (int run = 0; run < 2; ++run) {
        auto result = traverse_game_tree(worker);
        BOOST_REQUIRE(result.has_value());
        BOOST_CHECK_EQUAL(worker.stack.capacity(), stack_capacity);
        BOOST_CHECK_EQUAL(worker.node_utility.capacity(), node_utility_capacity);
        BOOST_CHECK_EQUAL(worker.child_action_value.capacity(), child_action_value_capacity);
        BOOST_CHECK_EQUAL(worker.edge_probability.capacity(), edge_probability_capacity);
        BOOST_CHECK_EQUAL(worker.delta_buffer.entry_capacity(), delta_entry_capacity);
        BOOST_CHECK_EQUAL(worker.delta_buffer.regret_delta_capacity(), regret_delta_capacity);
        BOOST_CHECK_EQUAL(worker.delta_buffer.strategy_delta_capacity(), strategy_delta_capacity);
    }
}

BOOST_AUTO_TEST_CASE(traversal_runs_concurrently_with_worker_local_contexts) {
    auto graph = create_chance_tree();
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    regrets.value(0, 0) = 2.0f;
    regrets.value(0, 1) = 1.0f;

    constexpr size_t worker_count = 4;
    std::array<worker_context, worker_count> workers;
    for (auto& worker : workers) {
        require_prepared_worker(worker, graph, regrets);
    }

    traversal_config config;
    config.initial_reach_oop = 2.0f;
    config.initial_reach_ip = 1.0f;

    std::array<std::future<std::expected<traversal_result, traversal_error>>, worker_count> futures;
    for (size_t worker_id = 0; worker_id < worker_count; ++worker_id) {
        futures[worker_id] = std::async(
            std::launch::async,
            [&workers, config, worker_id] {
                return traverse_game_tree(workers[worker_id], config);
            });
    }

    for (size_t worker_id = 0; worker_id < worker_count; ++worker_id) {
        auto result = futures[worker_id].get();
        BOOST_REQUIRE(result.has_value());
        BOOST_CHECK_SMALL(result->root_utility - 1.0f, 0.00001f);
        BOOST_CHECK_EQUAL(result->diagnostics.nodes_visited, graph.node_count);
        BOOST_CHECK_EQUAL(result->diagnostics.edges_scanned, graph.edges.size());
        BOOST_CHECK_EQUAL(workers[worker_id].delta_buffer.entry_count(), 1u);
    }

    BOOST_CHECK_EQUAL(regrets.value(0, 0), 2.0f);
    BOOST_CHECK_EQUAL(regrets.value(0, 1), 1.0f);
}

BOOST_AUTO_TEST_CASE(traversal_evaluates_river_showdown_terminal_leaf) {
    auto graph = create_simple_tree();
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    regrets.value(0, 0) = 1.0f;
    regrets.value(0, 1) = 0.0f;

    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    const auto [oop_combo, ip_combo] = first_compatible_live_combos(cache);

    zeta::holdem::reach_vector oop_reach{};
    zeta::holdem::reach_vector ip_reach{};
    oop_reach[oop_combo] = 1.0f;
    ip_reach[ip_combo] = 1.0f;

    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);

    zeta::holdem::terminal_state_table<2> terminal_states;
    terminal_states.states.push_back(zeta::holdem::make_showdown_terminal_state(context));

    std::vector<river_terminal_leaf> leaves(graph.node_count);
    leaves[0] = river_terminal_leaf{0};
    leaves[1] = river_terminal_leaf{0};
    const auto terminal_context = make_river_solver_context(
        deterministic_river_board(),
        std::array<zeta::holdem::reach_vector, 2>{oop_reach, ip_reach},
        std::move(terminal_states),
        std::move(leaves));

    worker_context worker;
    require_prepared_worker(worker, graph, regrets);

    const auto policy = terminal_context.terminal_policy(zeta::holdem::heads_up_player::oop, oop_combo);

    auto result = traverse_game_tree(worker, policy);
    const auto values = zeta::holdem::evaluate_showdown_values(
        terminal_context.cache,
        terminal_context.workspace.reach[0],
        terminal_context.workspace.reach[1],
        context);

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_SMALL(result->root_utility - values[zeta::holdem::heads_up_player::oop][oop_combo], 0.00001f);
    BOOST_CHECK_EQUAL(result->diagnostics.terminal_nodes, 2u);
}

BOOST_AUTO_TEST_CASE(traversal_evaluates_river_fold_terminal_leaf) {
    auto graph = create_simple_tree();
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);

    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    const auto [oop_combo, ip_combo] = first_compatible_live_combos(cache);

    zeta::holdem::reach_vector oop_reach{};
    zeta::holdem::reach_vector ip_reach{};
    oop_reach[oop_combo] = 1.0f;
    ip_reach[ip_combo] = 1.0f;

    std::array<zeta::holdem::river_reach_index, 2> reach_indices{
        zeta::holdem::make_river_reach_index(cache, oop_reach),
        zeta::holdem::make_river_reach_index(cache, ip_reach)
    };
    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);

    zeta::holdem::terminal_state_table<2> terminal_states;
    terminal_states.states.push_back(zeta::holdem::make_fold_terminal_state(
        context,
        zeta::holdem::heads_up_player::ip));

    std::vector<river_terminal_leaf> leaves(graph.node_count);
    leaves[0] = river_terminal_leaf{0};
    leaves[1] = river_terminal_leaf{0};

    worker_context worker;
    require_prepared_worker(worker, graph, regrets);

    const river_terminal_leaf_policy policy{
        .river_cache = &cache,
        .reach_indices = reach_indices,
        .terminal_leaves = leaves,
        .terminal_states = terminal_states.view(),
        .perspective = zeta::holdem::heads_up_player::oop,
        .combo = oop_combo
    };

    auto result = traverse_game_tree(worker, policy);
    const auto values = zeta::holdem::evaluate_fold_values(
        cache,
        reach_indices[0],
        reach_indices[1],
        context,
        zeta::holdem::heads_up_player::ip);

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_SMALL(result->root_utility - values[zeta::holdem::heads_up_player::oop][oop_combo], 0.00001f);
}

BOOST_AUTO_TEST_CASE(traversal_rejects_missing_river_terminal_leaf_metadata) {
    auto graph = create_simple_tree();
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    worker_context worker;
    require_prepared_worker(worker, graph, regrets);

    const river_terminal_leaf_policy policy{};

    auto result = traverse_game_tree(worker, policy);

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == traversal_error_kind::invalid_terminal_context);
}

BOOST_AUTO_TEST_CASE(traversal_rejects_invalid_terminal_state_reference) {
    auto graph = create_simple_tree();
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    worker_context worker;
    require_prepared_worker(worker, graph, regrets);

    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    const auto [oop_combo, ip_combo] = first_compatible_live_combos(cache);
    zeta::holdem::reach_vector oop_reach{};
    zeta::holdem::reach_vector ip_reach{};
    oop_reach[oop_combo] = 1.0f;
    ip_reach[ip_combo] = 1.0f;
    const std::array<zeta::holdem::river_reach_index, 2> reach_indices{
        zeta::holdem::make_river_reach_index(cache, oop_reach),
        zeta::holdem::make_river_reach_index(cache, ip_reach)
    };

    zeta::holdem::terminal_state_table<2> terminal_states;
    terminal_states.states.push_back(zeta::holdem::make_showdown_terminal_state(
        zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0)));
    std::vector<river_terminal_leaf> leaves(graph.node_count);
    leaves[0] = river_terminal_leaf{7};
    leaves[1] = river_terminal_leaf{0};

    const river_terminal_leaf_policy policy{
        .river_cache = &cache,
        .reach_indices = reach_indices,
        .terminal_leaves = leaves,
        .terminal_states = terminal_states.view(),
        .perspective = zeta::holdem::heads_up_player::oop,
        .combo = oop_combo
    };

    auto result = traverse_game_tree(worker, policy);

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == traversal_error_kind::invalid_terminal_context);
    BOOST_CHECK_EQUAL(result.error().node_id, 0u);
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

BOOST_AUTO_TEST_CASE(chance_traversal_uses_enumerated_outcome_probabilities) {
    graph_builder builder;
    const auto root = builder.add_node(node_kind::chance);
    const auto low = builder.add_node(node_kind::terminal);
    const auto high = builder.add_node(node_kind::terminal);
    builder.add_edge(root, low, 0);
    builder.add_edge(root, high, 1);
    auto graph = require_graph(builder.build());
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    worker_context worker;
    require_prepared_worker(worker, graph, regrets);

    auto chance_events = make_uniform_chance_event_table(graph);
    auto& first = chance_events.outcomes[chance_events.events.front().first_outcome];
    auto& second = chance_events.outcomes[chance_events.events.front().first_outcome + 1u];
    first.probability = 0.25f;
    second.probability = 0.75f;

    std::vector<float> terminal_utility(graph.node_count, 0.0f);
    terminal_utility[0] = 2.0f;
    terminal_utility[1] = 10.0f;
    traversal_config config;
    config.chance_events = &chance_events;
    config.initial_reach_ip = 0.0f;

    auto result = traverse_game_tree(worker, table_terminal_policy{terminal_utility}, config);

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_SMALL(result->root_utility - 8.0f, 0.00001f);
    BOOST_CHECK_EQUAL(result->diagnostics.chance_outcomes, 2u);
}

BOOST_AUTO_TEST_CASE(public_card_chance_enumeration_is_blocker_safe) {
    const auto dead_cards = card(0, 0) | card(1, 1);
    const auto flop_outcomes = enumerate_flop_outcomes(dead_cards);
    const auto turn_outcomes = enumerate_turn_outcomes(card(2, 2) | card(2, 3) | card(2, 4), dead_cards);

    BOOST_CHECK_EQUAL(flop_outcomes.size(), 19600u);
    BOOST_CHECK_EQUAL(turn_outcomes.size(), 47u);
    for (const auto& outcome : turn_outcomes) {
        BOOST_CHECK_EQUAL(outcome.cards & dead_cards, 0u);
        BOOST_CHECK_EQUAL(outcome.cards & (card(2, 2) | card(2, 3) | card(2, 4)), 0u);
    }
}

BOOST_AUTO_TEST_CASE(chance_board_partition_count_tracks_enumerated_outcomes) {
    auto graph = create_chance_tree();
    auto chance_events = make_uniform_chance_event_table(graph);
    chance_events.outcomes[0].board_partition_id = 4;
    chance_events.outcomes[1].board_partition_id = 7;

    BOOST_CHECK_EQUAL(chance_board_partition_count(chance_events), 8u);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(cfr_solver_iteration)

BOOST_AUTO_TEST_CASE(cfr_context_owns_graph_tables_and_cached_river_reach) {
    auto graph = create_simple_tree();
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    const auto [oop_combo, ip_combo] = first_compatible_live_combos(cache);
    zeta::holdem::reach_vector oop_reach{};
    zeta::holdem::reach_vector ip_reach{};
    oop_reach[oop_combo] = 1.0f;
    ip_reach[ip_combo] = 1.0f;

    const auto terminal_context = make_river_solver_context(
        deterministic_river_board(),
        std::array<zeta::holdem::reach_vector, 2>{oop_reach, ip_reach},
        zeta::holdem::terminal_state_table<2>{},
        std::vector<river_terminal_leaf>(graph.node_count));

    auto context_result = make_cfr_context(std::move(graph), terminal_context);

    BOOST_REQUIRE(context_result.has_value());
    BOOST_CHECK_EQUAL(context_result->graph.node_count, 3u);
    BOOST_CHECK_EQUAL(context_result->layout.infoset_count(), 1u);
    BOOST_CHECK_EQUAL(context_result->regrets.value_count(), 2u);
    BOOST_CHECK_EQUAL(context_result->strategy_sums.value_count(), 2u);
    BOOST_CHECK_EQUAL(context_result->river.workspace.reach[0].active_count, 1u);
    BOOST_CHECK_EQUAL(context_result->river.workspace.reach[1].active_count, 1u);
}

BOOST_AUTO_TEST_CASE(cfr_engine_selects_heads_up_and_nway_kernels_at_compile_time) {
    static_assert(cfr_engine<2>::heads_up);
    static_assert(!cfr_engine<3>::heads_up);
    static_assert(std::is_same_v<cfr_engine<2>::reach_state, hu_reach_state>);
    static_assert(std::is_same_v<cfr_engine<3>::reach_state, nway_reach_state<3>>);
    static_assert(cfr_engine<2>::reach_scratch_width == 3u);
    static_assert(cfr_engine<3>::reach_scratch_width == 4u);
    static_assert(std::is_trivially_copyable_v<cfr_traversal_frame>);

    BOOST_CHECK(cfr_engine<2>::heads_up);
    BOOST_CHECK(!cfr_engine<3>::heads_up);

    std::array<float, 2> hu_reach{0.25f, 0.75f};
    BOOST_CHECK_SMALL(cfr_engine<2>::counterfactual_reach(hu_reach, 2.0f, 0) - 1.5f, 0.00001f);
    cfr_engine<2>::propagate_player_action(hu_reach, 0, 0.5f);
    BOOST_CHECK_SMALL(hu_reach[0] - 0.125f, 0.00001f);

    std::array<float, 3> nway_reach{0.25f, 0.5f, 0.75f};
    BOOST_CHECK_SMALL(cfr_engine<3>::counterfactual_reach(nway_reach, 2.0f, 0) - 0.75f, 0.00001f);
}

BOOST_AUTO_TEST_CASE(run_cfr_iteration_evaluates_heads_up_terminal_state_provider) {
    auto graph = create_simple_tree();
    auto annotations = make_default_annotations(graph);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    regrets.value(0, 0) = 1.0f;
    regrets.value(0, 1) = 0.0f;

    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    const auto [oop_combo, ip_combo] = first_compatible_live_combos(cache);
    zeta::holdem::reach_vector oop_reach{};
    zeta::holdem::reach_vector ip_reach{};
    oop_reach[oop_combo] = 1.0f;
    ip_reach[ip_combo] = 1.0f;
    const std::array<zeta::holdem::river_reach_index, 2> reach_indices{
        zeta::holdem::make_river_reach_index(cache, oop_reach),
        zeta::holdem::make_river_reach_index(cache, ip_reach)
    };
    const auto terminal_context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);
    zeta::holdem::terminal_state_table<2> terminal_states;
    terminal_states.states.push_back(zeta::holdem::make_showdown_terminal_state(terminal_context));
    terminal_states.states.push_back(zeta::holdem::make_fold_terminal_state(
        terminal_context,
        zeta::holdem::heads_up_player::ip));
    std::vector<cfr_terminal_leaf> leaves(graph.node_count);
    leaves[0] = cfr_terminal_leaf{0};
    leaves[1] = cfr_terminal_leaf{1};
    std::array<zeta::holdem::combination_index, 2> combos{oop_combo, ip_combo};

    std::array<worker_context, 1> workers;
    auto context = make_cfr_solver_context<2>(graph, annotations, layout, regrets, strategy_sums);
    context.terminal_provider = make_terminal_state_provider<2>(
        cache,
        reach_indices,
        leaves,
        terminal_states.view(),
        combos);

    auto result = run_cfr_iteration(
        context,
        iteration_config{.updating_player = 0},
        std::span<worker_context>{workers});
    const zeta::holdem::terminal_engine<2> engine{};
    const auto values = engine.evaluate_terminal_values(cache, reach_indices, terminal_states[0]);

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_SMALL(result->root_utility - values[0][oop_combo], 0.00001f);
    BOOST_CHECK_EQUAL(result->diagnostics.terminal_evaluations, 2u);
}

BOOST_AUTO_TEST_CASE(run_cfr_iteration_evaluates_nway_terminal_state_provider) {
    auto graph = create_simple_tree();
    auto annotations = make_default_annotations(graph);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    regrets.value(0, 0) = 1.0f;
    regrets.value(0, 1) = 0.0f;

    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    const auto combos = first_three_compatible_live_combos(cache);
    std::array<zeta::holdem::reach_vector, 3> ranges{};
    for (std::size_t player = 0; player < combos.size(); ++player) {
        ranges[player][combos[player]] = 1.0f;
    }
    const std::array<zeta::holdem::river_reach_index, 3> reach_indices{
        zeta::holdem::make_river_reach_index(cache, ranges[0]),
        zeta::holdem::make_river_reach_index(cache, ranges[1]),
        zeta::holdem::make_river_reach_index(cache, ranges[2])
    };
    zeta::holdem::terminal_context<3> terminal_context{
        .gross_pot = 300.0,
        .rake = 0.0,
        .contribution = {100.0, 100.0, 100.0}
    };
    zeta::holdem::folded_mask<3> folded;
    folded.set_folded(2, true);
    zeta::holdem::terminal_state_table<3> terminal_states;
    terminal_states.states.push_back(zeta::holdem::make_fold_terminal_state(terminal_context, folded));
    std::vector<cfr_terminal_leaf> leaves(graph.node_count);
    leaves[0] = cfr_terminal_leaf{0};
    leaves[1] = cfr_terminal_leaf{0};

    std::array<worker_context, 1> workers;
    auto context = make_cfr_solver_context<3>(graph, annotations, layout, regrets, strategy_sums);
    context.terminal_provider = make_terminal_state_provider<3>(
        cache,
        reach_indices,
        leaves,
        terminal_states.view(),
        combos);

    auto result = run_cfr_iteration(
        context,
        iteration_config{.updating_player = 1},
        std::span<worker_context>{workers});
    const zeta::holdem::terminal_engine<3> engine{};
    const auto values = engine.evaluate_terminal_values(cache, reach_indices, terminal_states[0]);

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_SMALL(result->root_utility - values[1][combos[1]], 0.00001f);
    BOOST_CHECK_EQUAL(result->diagnostics.terminal_evaluations, 2u);
}

BOOST_AUTO_TEST_CASE(run_cfr_iteration_rejects_invalid_terminal_state_reference) {
    auto graph = create_simple_tree();
    auto annotations = make_default_annotations(graph);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);

    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    const auto [oop_combo, ip_combo] = first_compatible_live_combos(cache);
    zeta::holdem::reach_vector oop_reach{};
    zeta::holdem::reach_vector ip_reach{};
    oop_reach[oop_combo] = 1.0f;
    ip_reach[ip_combo] = 1.0f;
    const std::array<zeta::holdem::river_reach_index, 2> reach_indices{
        zeta::holdem::make_river_reach_index(cache, oop_reach),
        zeta::holdem::make_river_reach_index(cache, ip_reach)
    };
    zeta::holdem::terminal_state_table<2> terminal_states;
    terminal_states.states.push_back(zeta::holdem::make_showdown_terminal_state(
        zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0)));
    std::vector<cfr_terminal_leaf> leaves(graph.node_count);
    leaves[0] = cfr_terminal_leaf{9};
    leaves[1] = cfr_terminal_leaf{0};
    std::array<zeta::holdem::combination_index, 2> combos{oop_combo, ip_combo};

    std::array<worker_context, 1> workers;
    auto context = make_cfr_solver_context<2>(graph, annotations, layout, regrets, strategy_sums);
    context.terminal_provider = make_terminal_state_provider<2>(
        cache,
        reach_indices,
        leaves,
        terminal_states.view(),
        combos);

    auto result = run_cfr_iteration(
        context,
        iteration_config{.updating_player = 0},
        std::span<worker_context>{workers});

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == iteration_error_kind::traversal);
    BOOST_CHECK(result.error().traversal.kind == traversal_error_kind::invalid_terminal_context);
    BOOST_CHECK_EQUAL(result.error().traversal.node_id, 0u);
}

BOOST_AUTO_TEST_CASE(run_cfr_iteration_traverses_reduces_and_reports_diagnostics) {
    auto graph = create_simple_tree();
    auto annotations = make_default_annotations(graph);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    std::array<worker_context, 2> workers;
    std::vector<float> terminal_utility(graph.node_count, 0.0f);
    auto context = make_cfr_solver_context<2>(
        graph,
        annotations,
        layout,
        regrets,
        strategy_sums);
    context.terminal_provider = make_fixed_terminal_provider<2>(terminal_utility);

    auto result = run_cfr_iteration(
        context,
        iteration_config{
            .variant = cfr_variant::vanilla,
            .update_mode = cfr_update_mode::alternating,
            .iteration = 1,
            .updating_player = 0,
            .strategy_weight = 1.0f
        },
        std::span<worker_context>{workers});

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_EQUAL(result->traversals_run, 1u);
    BOOST_CHECK_EQUAL(result->workers_used, 1u);
    BOOST_CHECK_EQUAL(result->diagnostics.nodes_visited, graph.node_count);
    BOOST_CHECK_EQUAL(result->diagnostics.local_delta_entries_touched, 1u);
    BOOST_CHECK_SMALL(strategy_sums.value(0, 0) - 0.5f, 0.00001f);
    BOOST_CHECK_SMALL(strategy_sums.value(0, 1) - 0.5f, 0.00001f);
    BOOST_CHECK_EQUAL(result->quality.average_strategy_mass, 1.0);
    BOOST_REQUIRE_EQUAL(result->quality.strategy_sum_mass_by_player.size(), 2u);
    BOOST_CHECK_EQUAL(result->quality.strategy_sum_mass_by_player[0], 1.0);
    BOOST_CHECK_EQUAL(result->quality.max_regret_infoset_id, 0u);
}

BOOST_AUTO_TEST_CASE(quality_diagnostics_identify_regret_and_strategy_locations) {
    auto graph = create_simple_tree();
    auto annotations = make_default_annotations(graph);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    regrets.value(0, 0) = -1.0f;
    regrets.value(0, 1) = 3.0f;
    strategy_sums.value(0, 0) = 1.0f;
    strategy_sums.value(0, 1) = 9.0f;

    const auto diagnostics = compute_quality_diagnostics(graph, annotations, regrets, strategy_sums, 2);

    BOOST_CHECK_SMALL(diagnostics.regret_norm - std::sqrt(10.0), 0.00001);
    BOOST_CHECK_EQUAL(diagnostics.positive_regret_count, 1u);
    BOOST_CHECK_EQUAL(diagnostics.max_regret, 3.0f);
    BOOST_CHECK_EQUAL(diagnostics.max_regret_location.infoset_id, 0u);
    BOOST_CHECK_EQUAL(diagnostics.max_regret_location.action_index, 1u);
    BOOST_CHECK_EQUAL(diagnostics.largest_strategy_change_location.action_index, 1u);
    BOOST_CHECK_GT(diagnostics.largest_strategy_entropy_drop, 0.0);
}

BOOST_AUTO_TEST_CASE(cfr_checkpoint_round_trips_tables_and_resume_metadata) {
    auto graph = create_simple_tree();
    auto annotations = make_default_annotations(graph);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    regrets.value(0, 0) = 1.25f;
    regrets.value(0, 1) = -0.5f;
    strategy_sums.value(0, 0) = 3.0f;
    strategy_sums.value(0, 1) = 7.0f;
    auto owner_map = make_even_infoset_owner_map(layout, 1).value();
    auto context = make_cfr_solver_context<2>(graph, annotations, layout, regrets, strategy_sums);
    context.owner_map = &owner_map;
    context.reduction = reduction_policy{.order = reduction_order::owner_range_then_worker};
    const iteration_config config{
        .variant = cfr_variant::cfr_plus,
        .update_mode = cfr_update_mode::alternating,
        .iteration = 42,
        .updating_player = 0,
        .strategy_weight = 1.0f
    };

    std::stringstream stream(std::ios::in | std::ios::out | std::ios::binary);
    auto saved = save_cfr_checkpoint(stream, context, config);
    BOOST_REQUIRE(saved.has_value());

    regrets.value(0, 0) = 0.0f;
    regrets.value(0, 1) = 0.0f;
    strategy_sums.value(0, 0) = 0.0f;
    strategy_sums.value(0, 1) = 0.0f;
    stream.seekg(0);

    auto loaded = load_cfr_checkpoint(stream, context, config);

    BOOST_REQUIRE(loaded.has_value());
    BOOST_CHECK_EQUAL(loaded->header.iteration, 42u);
    BOOST_CHECK_EQUAL(regrets.value(0, 0), 1.25f);
    BOOST_CHECK_EQUAL(regrets.value(0, 1), -0.5f);
    BOOST_CHECK_EQUAL(strategy_sums.value(0, 0), 3.0f);
    BOOST_CHECK_EQUAL(strategy_sums.value(0, 1), 7.0f);
}

BOOST_AUTO_TEST_CASE(cfr_checkpoint_rejects_incompatible_owner_ranges) {
    auto graph = create_simple_tree();
    auto annotations = make_default_annotations(graph);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    auto owner_map = make_even_infoset_owner_map(layout, 1).value();
    auto saving_context = make_cfr_solver_context<2>(graph, annotations, layout, regrets, strategy_sums);
    saving_context.owner_map = &owner_map;

    std::stringstream stream(std::ios::in | std::ios::out | std::ios::binary);
    BOOST_REQUIRE(save_cfr_checkpoint(stream, saving_context, iteration_config{}).has_value());
    stream.seekg(0);

    auto loading_context = make_cfr_solver_context<2>(graph, annotations, layout, regrets, strategy_sums);
    auto loaded = load_cfr_checkpoint(stream, loading_context, iteration_config{});

    BOOST_REQUIRE(!loaded);
    BOOST_CHECK(loaded.error().kind == checkpoint_error_kind::incompatible_owner_ranges);
}

BOOST_AUTO_TEST_CASE(run_cfr_iteration_writes_counterfactual_regret_only_for_updating_player) {
    auto graph = create_simple_tree();
    auto annotations = make_default_annotations(graph);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    std::array<worker_context, 1> workers;
    std::vector<float> terminal_utility(graph.node_count, 0.0f);
    terminal_utility[0] = 1.0f;
    terminal_utility[1] = -1.0f;
    auto context = make_cfr_solver_context<2>(
        graph,
        annotations,
        layout,
        regrets,
        strategy_sums);
    context.terminal_provider = make_fixed_terminal_provider<2>(terminal_utility);

    auto result = run_cfr_iteration(
        context,
        iteration_config{
            .variant = cfr_variant::vanilla,
            .update_mode = cfr_update_mode::alternating,
            .iteration = 1,
            .updating_player = 0,
            .strategy_weight = 2.0f
        },
        std::span<worker_context>{workers});

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_SMALL(result->root_utility, 0.00001f);
    BOOST_CHECK_SMALL(regrets.value(0, 0) - 1.0f, 0.00001f);
    BOOST_CHECK_SMALL(regrets.value(0, 1) + 1.0f, 0.00001f);
    BOOST_CHECK_SMALL(strategy_sums.value(0, 0) - 1.0f, 0.00001f);
    BOOST_CHECK_SMALL(strategy_sums.value(0, 1) - 1.0f, 0.00001f);
    BOOST_CHECK_EQUAL(result->diagnostics.regret_updates, 2u);
    BOOST_CHECK_EQUAL(result->diagnostics.strategy_updates, 2u);
    BOOST_CHECK_EQUAL(result->diagnostics.terminal_evaluations, 2u);
}

BOOST_AUTO_TEST_CASE(run_cfr_iteration_uses_chance_probabilities_in_counterfactual_values) {
    graph_builder builder;
    const auto root = builder.add_node(node_kind::chance);
    const auto player = builder.add_node(node_kind::player);
    const auto terminal = builder.add_node(node_kind::terminal);
    builder.add_edge(root, player, 0);
    builder.add_edge(player, terminal, 0);
    builder.set_infoset_id(player, 0);
    auto graph = require_graph(builder.build());
    auto annotations = make_default_annotations(graph);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    std::array<worker_context, 1> workers;
    std::vector<float> terminal_utility(graph.node_count, 0.0f);
    terminal_utility[0] = 4.0f;
    auto chance_events = make_uniform_chance_event_table(graph);
    chance_events.outcomes.front().probability = 1.0f;
    auto context = make_cfr_solver_context<2>(
        graph,
        annotations,
        layout,
        regrets,
        strategy_sums);
    context.chance_events = &chance_events;
    context.terminal_provider = make_fixed_terminal_provider<2>(terminal_utility);

    auto result = run_cfr_iteration(context, iteration_config{.updating_player = 0}, std::span<worker_context>{workers});

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_SMALL(result->root_utility - 4.0f, 0.00001f);
    BOOST_CHECK_SMALL(strategy_sums.value(0, 0) - 1.0f, 0.00001f);
    BOOST_CHECK_EQUAL(result->diagnostics.chance_outcomes, 1u);
}

BOOST_AUTO_TEST_CASE(run_cfr_iteration_uses_three_player_counterfactual_reach_product) {
    graph_builder builder;
    const auto root = builder.add_node(node_kind::player);
    const auto updating_player = builder.add_node(node_kind::player);
    const auto skipped_terminal = builder.add_node(node_kind::terminal);
    const auto win_terminal = builder.add_node(node_kind::terminal);
    const auto lose_terminal = builder.add_node(node_kind::terminal);
    builder.add_edge(root, updating_player, 0);
    builder.add_edge(root, skipped_terminal, 1);
    builder.add_edge(updating_player, win_terminal, 0);
    builder.add_edge(updating_player, lose_terminal, 1);
    builder.set_infoset_id(root, 0);
    builder.set_infoset_id(updating_player, 1);
    auto graph = require_graph(builder.build());
    auto annotations = make_default_annotations(graph);
    annotations.actor_by_node[graph.root_node] = 2;

    const auto p0_node = graph.out_edges(graph.root_node)[0].child_node;
    annotations.actor_by_node[p0_node] = 0;
    std::vector<float> terminal_utility(graph.node_count, 0.0f);
    const auto p0_edges = graph.out_edges(p0_node);
    terminal_utility[p0_edges[0].child_node] = 4.0f;
    terminal_utility[p0_edges[1].child_node] = -2.0f;

    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    std::array<worker_context, 1> workers;
    auto context = make_cfr_solver_context<3>(
        graph,
        annotations,
        layout,
        regrets,
        strategy_sums);
    context.terminal_provider = make_fixed_terminal_provider<3>(terminal_utility);

    auto result = run_cfr_iteration(
        context,
        iteration_config{.updating_player = 0},
        std::span<worker_context>{workers});

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_SMALL(regrets.value(1, 0) - 1.5f, 0.00001f);
    BOOST_CHECK_SMALL(regrets.value(1, 1) + 1.5f, 0.00001f);
    BOOST_CHECK_SMALL(strategy_sums.value(1, 0) - 0.5f, 0.00001f);
    BOOST_CHECK_SMALL(strategy_sums.value(1, 1) - 0.5f, 0.00001f);
    BOOST_CHECK_EQUAL(result->diagnostics.regret_updates, 2u);
}

BOOST_AUTO_TEST_CASE(run_cfr_iteration_reuses_worker_scratch_without_growing_after_setup) {
    graph_builder builder;
    const auto root = builder.add_node(node_kind::player);
    const auto p1 = builder.add_node(node_kind::player);
    const auto win_terminal = builder.add_node(node_kind::terminal);
    const auto root_lose_terminal = builder.add_node(node_kind::terminal);
    const auto p1_lose_terminal = builder.add_node(node_kind::terminal);
    builder.add_edge(root, p1, 0);
    builder.add_edge(root, root_lose_terminal, 1);
    builder.add_edge(p1, win_terminal, 0);
    builder.add_edge(p1, p1_lose_terminal, 1);
    builder.set_infoset_id(root, 0);
    builder.set_infoset_id(p1, 1);
    auto graph = require_graph(builder.build());
    auto annotations = make_default_annotations(graph);
    const auto root_node = graph.root_node;
    const auto graph_root_edges = graph.out_edges(root_node);
    const auto p1_node = graph_root_edges[0].child_node;
    const auto graph_root_lose_terminal = graph_root_edges[1].child_node;
    const auto graph_p1_edges = graph.out_edges(p1_node);
    const auto graph_win_terminal = graph_p1_edges[0].child_node;
    const auto graph_p1_lose_terminal = graph_p1_edges[1].child_node;
    annotations.actor_by_node[graph.root_node] = 0;
    annotations.actor_by_node[p1_node] = 1;
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    std::vector<float> terminal_utility(graph.node_count, 0.0f);
    terminal_utility[graph_win_terminal] = 3.0f;
    terminal_utility[graph_root_lose_terminal] = -1.0f;
    terminal_utility[graph_p1_lose_terminal] = -1.0f;
    std::array<worker_context, 1> workers;
    auto context = make_cfr_solver_context<3>(graph, annotations, layout, regrets, strategy_sums);
    context.terminal_provider = make_fixed_terminal_provider<3>(terminal_utility);

    auto first = run_cfr_iteration(context, iteration_config{.updating_player = 1}, std::span<worker_context>{workers});
    BOOST_REQUIRE(first.has_value());

    const auto cfr_node_stack_capacity = workers[0].cfr_frame_node_id.capacity();
    const auto cfr_edge_cursor_capacity = workers[0].cfr_frame_edge_cursor.capacity();
    const auto cfr_reach_slot_capacity = workers[0].cfr_frame_reach_slot.capacity();
    const auto cfr_value_slot_capacity = workers[0].cfr_frame_value_slot.capacity();
    const auto cfr_phase_capacity = workers[0].cfr_frame_phase.capacity();
    const auto cfr_value_scratch_capacity = workers[0].cfr_value_scratch.capacity();
    const auto cfr_reach_scratch_capacity = workers[0].cfr_reach_scratch.capacity();

    auto second = run_cfr_iteration(context, iteration_config{.updating_player = 1}, std::span<worker_context>{workers});
    BOOST_REQUIRE(second.has_value());
    BOOST_CHECK_EQUAL(workers[0].cfr_frame_node_id.capacity(), cfr_node_stack_capacity);
    BOOST_CHECK_EQUAL(workers[0].cfr_frame_edge_cursor.capacity(), cfr_edge_cursor_capacity);
    BOOST_CHECK_EQUAL(workers[0].cfr_frame_reach_slot.capacity(), cfr_reach_slot_capacity);
    BOOST_CHECK_EQUAL(workers[0].cfr_frame_value_slot.capacity(), cfr_value_slot_capacity);
    BOOST_CHECK_EQUAL(workers[0].cfr_frame_phase.capacity(), cfr_phase_capacity);
    BOOST_CHECK_EQUAL(workers[0].cfr_value_scratch.capacity(), cfr_value_scratch_capacity);
    BOOST_CHECK_EQUAL(workers[0].cfr_reach_scratch.capacity(), cfr_reach_scratch_capacity);
    BOOST_CHECK_GE(workers[0].cfr_reach_scratch.size(), workers[0].stack_capacity() * cfr_engine<3>::reach_scratch_width);
}

BOOST_AUTO_TEST_CASE(run_cfr_iteration_rejects_chance_graph_without_enumerated_events) {
    auto graph = create_chance_tree();
    auto annotations = make_default_annotations(graph);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    std::array<worker_context, 1> workers;
    auto context = make_cfr_solver_context<2>(
        graph,
        annotations,
        layout,
        regrets,
        strategy_sums);

    auto result = run_cfr_iteration(context, iteration_config{.updating_player = 0}, std::span<worker_context>{workers});

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == iteration_error_kind::chance_table);
    BOOST_CHECK(result.error().chance_table.kind == chance_table_error_kind::missing_chance_event);
}

BOOST_AUTO_TEST_CASE(run_cfr_iteration_rejects_invalid_updating_player) {
    auto graph = create_simple_tree();
    auto annotations = make_default_annotations(graph);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    std::array<worker_context, 1> workers;
    auto context = make_cfr_solver_context<2>(
        graph,
        annotations,
        layout,
        regrets,
        strategy_sums);

    auto result = run_cfr_iteration(
        context,
        iteration_config{.updating_player = 2},
        std::span<worker_context>{workers});

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == iteration_error_kind::invalid_update_player);
}

BOOST_AUTO_TEST_CASE(deterministic_worker_reduction_applies_workers_in_plan_order) {
    constexpr std::array<uint32_t, 1> action_counts{1u};
    auto layout = require_layout(make_action_table_layout(std::span<const uint32_t>{action_counts}));

    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    std::array<worker_context, 2> workers;
    BOOST_REQUIRE(workers[0].delta_buffer.reset_layout(layout.action_offsets).has_value());
    BOOST_REQUIRE(workers[1].delta_buffer.reset_layout(layout.action_offsets).has_value());

    workers[0].delta_buffer.add_regret_delta(0, 0, 1.0f);
    workers[0].delta_buffer.add_strategy_delta(0, 0, 10.0f);
    workers[1].delta_buffer.add_regret_delta(0, 0, 2.0f);
    workers[1].delta_buffer.add_strategy_delta(0, 0, 20.0f);

    std::array<const worker_context*, 2> worker_ptrs{&workers[1], &workers[0]};
    const auto plan = make_deterministic_reduction_plan(static_cast<uint32_t>(worker_ptrs.size()));

    auto result = apply_worker_reductions(regrets, strategy_sums, plan, std::span<const worker_context* const>{worker_ptrs});

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_EQUAL(regrets.value(0, 0), 3.0f);
    BOOST_CHECK_EQUAL(strategy_sums.value(0, 0), 30.0f);
}

BOOST_AUTO_TEST_CASE(cfr_plus_reduction_clips_once_after_raw_worker_merge) {
    constexpr std::array<uint32_t, 1> action_counts{1u};
    auto layout = require_layout(make_action_table_layout(std::span<const uint32_t>{action_counts}));

    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    std::array<worker_context, 2> workers;
    BOOST_REQUIRE(workers[0].delta_buffer.reset_layout(layout.action_offsets).has_value());
    BOOST_REQUIRE(workers[1].delta_buffer.reset_layout(layout.action_offsets).has_value());

    workers[0].delta_buffer.add_regret_delta(0, 0, 10.0f);
    workers[1].delta_buffer.add_regret_delta(0, 0, -20.0f);

    auto result = apply_worker_reductions(
        regrets,
        strategy_sums,
        std::span<const worker_context>{workers},
        cfr_variant::cfr_plus);

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_EQUAL(regrets.value(0, 0), 0.0f);
}

BOOST_AUTO_TEST_CASE(deterministic_worker_reduction_rejects_duplicate_worker_order) {
    constexpr std::array<uint32_t, 1> action_counts{1u};
    auto layout = require_layout(make_action_table_layout(std::span<const uint32_t>{action_counts}));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    std::array<worker_context, 2> workers;
    BOOST_REQUIRE(workers[0].delta_buffer.reset_layout(layout.action_offsets).has_value());
    BOOST_REQUIRE(workers[1].delta_buffer.reset_layout(layout.action_offsets).has_value());
    std::array<const worker_context*, 2> worker_ptrs{&workers[0], &workers[1]};

    deterministic_reduction_plan plan;
    plan.worker_order = {0, 0};

    auto result = apply_worker_reductions(regrets, strategy_sums, plan, std::span<const worker_context* const>{worker_ptrs});

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == iteration_error_kind::duplicate_worker_id);
}

BOOST_AUTO_TEST_CASE(infoset_owner_map_builds_contiguous_shards) {
    constexpr std::array<uint32_t, 5> action_counts{1u, 2u, 3u, 4u, 5u};
    auto layout = require_layout(make_action_table_layout(std::span<const uint32_t>{action_counts}));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);

    auto owner_map_result = make_even_infoset_owner_map(layout, 2);

    BOOST_REQUIRE(owner_map_result.has_value());
    const auto& owner_map = *owner_map_result;
    BOOST_REQUIRE_EQUAL(owner_map.ranges.size(), 2u);
    BOOST_CHECK_EQUAL(owner_map.owner_for_infoset(0), 0u);
    BOOST_CHECK_EQUAL(owner_map.owner_for_infoset(2), 0u);
    BOOST_CHECK_EQUAL(owner_map.owner_for_infoset(3), 1u);

    const auto shard = make_table_shard_view(regrets, strategy_sums, owner_map.ranges[1]);
    BOOST_CHECK(shard.contains_infoset(3));
    BOOST_CHECK(!shard.contains_infoset(2));
    BOOST_CHECK_EQUAL(shard.begin_value, layout.action_offsets[3]);
    BOOST_CHECK_EQUAL(shard.end_value, layout.action_offsets[5]);
}

BOOST_AUTO_TEST_CASE(owner_routed_reduction_tracks_remote_deltas_and_owner_hits) {
    constexpr std::array<uint32_t, 3> action_counts{1u, 1u, 1u};
    auto layout = require_layout(make_action_table_layout(std::span<const uint32_t>{action_counts}));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    std::array<worker_context, 2> workers;
    BOOST_REQUIRE(workers[0].delta_buffer.reset_layout(layout.action_offsets).has_value());
    BOOST_REQUIRE(workers[1].delta_buffer.reset_layout(layout.action_offsets).has_value());
    workers[0].delta_buffer.add_regret_delta(0, 0, 1.0f);
    workers[0].delta_buffer.add_strategy_delta(0, 0, 10.0f);
    workers[1].delta_buffer.add_regret_delta(0, 0, 2.0f);
    workers[1].delta_buffer.add_strategy_delta(0, 0, 20.0f);
    workers[1].delta_buffer.add_regret_delta(2, 0, 3.0f);
    workers[1].delta_buffer.add_strategy_delta(2, 0, 30.0f);

    auto owner_map = make_even_infoset_owner_map(layout, 2).value();
    std::array<const worker_context*, 2> worker_ptrs{&workers[0], &workers[1]};
    reduction_diagnostics diagnostics;

    auto result = apply_owner_routed_worker_reductions(
        regrets,
        strategy_sums,
        make_deterministic_reduction_plan(2),
        std::span<const worker_context* const>{worker_ptrs},
        owner_map,
        &diagnostics);

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_EQUAL(regrets.value(0, 0), 3.0f);
    BOOST_CHECK_EQUAL(strategy_sums.value(0, 0), 30.0f);
    BOOST_CHECK_EQUAL(regrets.value(2, 0), 3.0f);
    BOOST_CHECK_EQUAL(diagnostics.remote_delta_count, 1u);
    BOOST_CHECK_EQUAL(diagnostics.remote_delta_bytes, sizeof(float) * 2u);
    BOOST_REQUIRE_EQUAL(diagnostics.owner_hit_distribution.size(), 2u);
    BOOST_CHECK_EQUAL(diagnostics.owner_hit_distribution[0], 2u);
    BOOST_CHECK_EQUAL(diagnostics.owner_hit_distribution[1], 1u);
    BOOST_CHECK_EQUAL(diagnostics.owner_remote_hit_distribution[0], 1u);
    BOOST_CHECK_EQUAL(diagnostics.per_owner_touched_values[0], 2u);
}

BOOST_AUTO_TEST_CASE(owner_routed_cfr_plus_clips_after_all_worker_deltas_merge) {
    constexpr std::array<uint32_t, 1> action_counts{1u};
    auto layout = require_layout(make_action_table_layout(std::span<const uint32_t>{action_counts}));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    std::array<worker_context, 2> workers;
    BOOST_REQUIRE(workers[0].delta_buffer.reset_layout(layout.action_offsets).has_value());
    BOOST_REQUIRE(workers[1].delta_buffer.reset_layout(layout.action_offsets).has_value());
    workers[0].delta_buffer.add_regret_delta(0, 0, -5.0f);
    workers[1].delta_buffer.add_regret_delta(0, 0, 3.0f);

    auto owner_map = make_even_infoset_owner_map(layout, 1).value();
    std::array<const worker_context*, 2> worker_ptrs{&workers[0], &workers[1]};
    reduction_diagnostics diagnostics;

    auto result = apply_worker_reductions(
        regrets,
        strategy_sums,
        make_deterministic_reduction_plan(2),
        std::span<const worker_context* const>{worker_ptrs},
        reduction_policy{.order = reduction_order::owner_range_then_worker},
        &owner_map,
        &diagnostics,
        cfr_variant::cfr_plus);

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_EQUAL(regrets.value(0, 0), 0.0f);
    BOOST_CHECK_EQUAL(diagnostics.remote_delta_count, 1u);
}

BOOST_AUTO_TEST_CASE(owner_routed_reduction_preserves_worker_count_determinism) {
    constexpr std::array<uint32_t, 2> action_counts{2u, 1u};
    auto layout = require_layout(make_action_table_layout(std::span<const uint32_t>{action_counts}));
    auto owner_map = make_even_infoset_owner_map(layout, 2).value();

    regret_table one_worker_regrets(layout);
    strategy_sum_table one_worker_strategies(layout);
    std::array<worker_context, 1> one_worker;
    BOOST_REQUIRE(one_worker[0].delta_buffer.reset_layout(layout.action_offsets).has_value());
    one_worker[0].delta_buffer.add_regret_delta(0, 0, 1.0f);
    one_worker[0].delta_buffer.add_regret_delta(0, 1, 2.0f);
    one_worker[0].delta_buffer.add_regret_delta(1, 0, 3.0f);
    one_worker[0].delta_buffer.add_strategy_delta(0, 0, 4.0f);
    one_worker[0].delta_buffer.add_strategy_delta(0, 1, 5.0f);
    one_worker[0].delta_buffer.add_strategy_delta(1, 0, 6.0f);
    std::array<const worker_context*, 1> one_worker_ptrs{&one_worker[0]};
    BOOST_REQUIRE(apply_owner_routed_worker_reductions(
        one_worker_regrets,
        one_worker_strategies,
        make_deterministic_reduction_plan(1),
        std::span<const worker_context* const>{one_worker_ptrs},
        owner_map).has_value());

    regret_table two_worker_regrets(layout);
    strategy_sum_table two_worker_strategies(layout);
    std::array<worker_context, 2> two_workers;
    BOOST_REQUIRE(two_workers[0].delta_buffer.reset_layout(layout.action_offsets).has_value());
    BOOST_REQUIRE(two_workers[1].delta_buffer.reset_layout(layout.action_offsets).has_value());
    two_workers[0].delta_buffer.add_regret_delta(0, 0, 1.0f);
    two_workers[0].delta_buffer.add_regret_delta(1, 0, 3.0f);
    two_workers[0].delta_buffer.add_strategy_delta(0, 0, 4.0f);
    two_workers[0].delta_buffer.add_strategy_delta(1, 0, 6.0f);
    two_workers[1].delta_buffer.add_regret_delta(0, 1, 2.0f);
    two_workers[1].delta_buffer.add_strategy_delta(0, 1, 5.0f);
    std::array<const worker_context*, 2> two_worker_ptrs{&two_workers[0], &two_workers[1]};
    BOOST_REQUIRE(apply_owner_routed_worker_reductions(
        two_worker_regrets,
        two_worker_strategies,
        make_deterministic_reduction_plan(2),
        std::span<const worker_context* const>{two_worker_ptrs},
        owner_map).has_value());

    BOOST_CHECK_EQUAL_COLLECTIONS(
        one_worker_regrets.regrets.begin(),
        one_worker_regrets.regrets.end(),
        two_worker_regrets.regrets.begin(),
        two_worker_regrets.regrets.end());
    BOOST_CHECK_EQUAL_COLLECTIONS(
        one_worker_strategies.sums.begin(),
        one_worker_strategies.sums.end(),
        two_worker_strategies.sums.begin(),
        two_worker_strategies.sums.end());
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(cfr_scheduler_runtime)

BOOST_AUTO_TEST_CASE(board_partition_plan_maps_tasks_in_board_major_order) {
    auto graph = create_chance_tree();
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{2, DEFAULT_TEST_WORK_DEPTH_SHIFT}));

    auto plan_result = make_board_partition_plan(3, partitions);

    BOOST_REQUIRE(plan_result.has_value());
    const auto& plan = *plan_result;
    BOOST_REQUIRE_EQUAL(plan.board_count, 3u);
    BOOST_REQUIRE_EQUAL(plan.partitions.size(), partitions.size());
    BOOST_CHECK_EQUAL(plan.task_count(), 3u * partitions.size());

    const auto first = plan.task_at(0);
    BOOST_CHECK_EQUAL(first.board_index, 0u);
    BOOST_CHECK_EQUAL(first.partition_index, 0u);
    BOOST_CHECK_EQUAL(first.partition, &plan.partitions[0]);

    const auto second_board = plan.task_at(partitions.size());
    BOOST_CHECK_EQUAL(second_board.board_index, 1u);
    BOOST_CHECK_EQUAL(second_board.partition_index, 0u);
}

BOOST_AUTO_TEST_CASE(board_partition_plan_rejects_invalid_inputs) {
    auto graph = create_simple_tree();
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{DEFAULT_TEST_PARTITION_COUNT, DEFAULT_TEST_WORK_DEPTH_SHIFT}));

    auto no_boards = make_board_partition_plan(0, partitions);
    BOOST_REQUIRE(!no_boards);
    BOOST_CHECK(no_boards.error().kind == scheduler_error_kind::invalid_board_count);

    auto no_partitions = make_board_partition_plan(1, std::span<const graph_partition>{});
    BOOST_REQUIRE(!no_partitions);
    BOOST_CHECK(no_partitions.error().kind == scheduler_error_kind::empty_partition_plan);
}

BOOST_AUTO_TEST_CASE(board_partition_scheduler_executes_each_task_once) {
    auto graph = create_chance_tree();
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{2, DEFAULT_TEST_WORK_DEPTH_SHIFT}));
    auto plan = make_board_partition_plan(16, partitions).value();

    std::vector<std::atomic<uint32_t>> task_hits(plan.task_count());
    for (auto& hit : task_hits) {
        hit.store(0, std::memory_order_relaxed);
    }

    auto result = run_board_partition_scheduler(
        plan,
        scheduler_runtime_config{4, 3},
        [&task_hits, &plan](const scheduler_worker_state& worker, const board_partition_task& task) {
            BOOST_CHECK_LT(worker.worker_id, 4u);
            BOOST_CHECK_LT(task.board_index, plan.board_count);
            BOOST_CHECK_LT(task.partition_index, plan.partitions.size());
            BOOST_CHECK_EQUAL(task.partition, &plan.partitions[task.partition_index]);
            task_hits[task.task_index].fetch_add(1, std::memory_order_relaxed);
        });

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_EQUAL(result->tasks_executed, plan.task_count());
    BOOST_CHECK_EQUAL(result->workers.size(), 4u);
    BOOST_CHECK_EQUAL(result->estimated_work, plan.estimated_work());
    for (const auto& hit : task_hits) {
        BOOST_CHECK_EQUAL(hit.load(std::memory_order_relaxed), 1u);
    }
}

BOOST_AUTO_TEST_CASE(static_board_partition_scheduler_executes_each_task_once) {
    auto graph = create_chance_tree();
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{2, DEFAULT_TEST_WORK_DEPTH_SHIFT}));
    auto plan = make_board_partition_plan(16, partitions).value();

    std::vector<std::atomic<uint32_t>> task_hits(plan.task_count());
    for (auto& hit : task_hits) {
        hit.store(0, std::memory_order_relaxed);
    }

    auto result = run_static_board_partition_scheduler(
        plan,
        scheduler_runtime_config{4},
        [&task_hits, &plan](const scheduler_worker_state& worker, const board_partition_task& task) {
            BOOST_CHECK_LT(worker.worker_id, 4u);
            BOOST_CHECK_LT(task.board_index, plan.board_count);
            BOOST_CHECK_LT(task.partition_index, plan.partitions.size());
            BOOST_CHECK_EQUAL(task.partition, &plan.partitions[task.partition_index]);
            task_hits[task.task_index].fetch_add(1, std::memory_order_relaxed);
        });

    BOOST_REQUIRE(result.has_value());
    BOOST_CHECK_EQUAL(result->tasks_executed, plan.task_count());
    BOOST_CHECK_EQUAL(result->workers.size(), 4u);
    BOOST_CHECK_EQUAL(result->estimated_work, plan.estimated_work());
    for (const auto& hit : task_hits) {
        BOOST_CHECK_EQUAL(hit.load(std::memory_order_relaxed), 1u);
    }
}

BOOST_AUTO_TEST_CASE(static_board_partition_scheduler_reports_task_failure_context) {
    auto graph = create_chance_tree();
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{2, DEFAULT_TEST_WORK_DEPTH_SHIFT}));
    auto plan = make_board_partition_plan(3, partitions).value();

    auto result = run_static_board_partition_scheduler(
        plan,
        scheduler_runtime_config{2},
        [](const scheduler_worker_state&, const board_partition_task& task) -> std::expected<void, scheduler_error> {
            if (task.board_index == 2u && task.partition_index == 1u) {
                return std::unexpected(scheduler_error{scheduler_error_kind::task_failed});
            }
            return {};
        });

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == scheduler_error_kind::task_failed);
    BOOST_CHECK_EQUAL(result.error().task_index, 2u * plan.partitions.size() + 1u);
    BOOST_CHECK_EQUAL(result.error().board_index, 2u);
    BOOST_CHECK_EQUAL(result.error().partition_index, 1u);
}

BOOST_AUTO_TEST_CASE(board_partition_scheduler_rejects_zero_workers) {
    auto graph = create_simple_tree();
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{DEFAULT_TEST_PARTITION_COUNT, DEFAULT_TEST_WORK_DEPTH_SHIFT}));
    auto plan = make_board_partition_plan(1, partitions).value();

    auto result = run_board_partition_scheduler(
        plan,
        scheduler_runtime_config{0},
        [](const scheduler_worker_state&, const board_partition_task&) {});

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == scheduler_error_kind::invalid_worker_count);
}

BOOST_AUTO_TEST_CASE(board_partition_scheduler_reports_task_failure_context) {
    auto graph = create_chance_tree();
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{2, DEFAULT_TEST_WORK_DEPTH_SHIFT}));
    auto plan = make_board_partition_plan(2, partitions).value();

    auto result = run_board_partition_scheduler(
        plan,
        scheduler_runtime_config{1},
        [](const scheduler_worker_state&, const board_partition_task& task) -> std::expected<void, scheduler_error> {
            if (task.task_index == 2u) {
                return std::unexpected(scheduler_error{scheduler_error_kind::task_failed});
            }
            return {};
        });

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == scheduler_error_kind::task_failed);
    BOOST_CHECK_EQUAL(result.error().task_index, 2u);
    BOOST_CHECK_EQUAL(result.error().board_index, 1u);
    BOOST_CHECK_EQUAL(result.error().partition_index, 0u);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_AUTO_TEST_SUITE(cfr_partitions)

/** Test partition construction and metadata. */
BOOST_AUTO_TEST_CASE(partition_coverage) {
    auto graph = create_chance_tree();
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{DEFAULT_TEST_PARTITION_COUNT, DEFAULT_TEST_WORK_DEPTH_SHIFT}));
    
    BOOST_CHECK(!partitions.empty());
    
    /** Check coverage: all nodes should be in exactly one partition. */
    uint32_t prev_end = 0;
    for (const auto& p : partitions) {
        BOOST_CHECK_EQUAL(p.begin_node, prev_end);
        BOOST_CHECK_LT(p.begin_node, p.end_node);
        BOOST_CHECK_LE(p.end_node, graph.node_count);
        prev_end = p.end_node;
    }
    BOOST_CHECK_EQUAL(prev_end, graph.node_count);
}

BOOST_AUTO_TEST_CASE(partition_no_overlap) {
    auto graph = create_chance_tree();
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{DEFAULT_TEST_PARTITION_COUNT, DEFAULT_TEST_WORK_DEPTH_SHIFT}));
    
    for (size_t i = 1; i < partitions.size(); ++i) {
        BOOST_CHECK_EQUAL(partitions[i].begin_node, partitions[i - 1].end_node);
    }
}

BOOST_AUTO_TEST_CASE(partition_metadata_consistency) {
    auto graph = create_chance_tree();
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{DEFAULT_TEST_PARTITION_COUNT, DEFAULT_TEST_WORK_DEPTH_SHIFT}));
    
    for (const auto& p : partitions) {
        /** node_count should be consistent. */
        uint32_t expected_size = p.end_node - p.begin_node;
        BOOST_CHECK_EQUAL(p.node_count, expected_size);
        
        /** terminal_count should not exceed node_count. */
        BOOST_CHECK_LE(p.terminal_count, p.node_count);
        
        /** action_count should be reasonable. */
        BOOST_CHECK_GE(p.action_count, 0u);
    }
}

BOOST_AUTO_TEST_CASE(validate_rejects_partition_estimated_work_mismatch) {
    auto graph = create_simple_tree();
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{DEFAULT_TEST_PARTITION_COUNT, DEFAULT_TEST_WORK_DEPTH_SHIFT}));
    BOOST_REQUIRE(!partitions.empty());

    ++partitions.front().estimated_work;

    BOOST_CHECK(!::zeta::holdem::cfr::scheduler::validate_dfs_partitions(
        graph,
        partitions,
        dfs_partition_strategy{DEFAULT_TEST_PARTITION_COUNT, DEFAULT_TEST_WORK_DEPTH_SHIFT}));
}

BOOST_AUTO_TEST_CASE(dfs_partition_strategy_rejects_zero_count) {
    auto graph = create_simple_tree();

    auto result = compute_dfs_partitions(graph, dfs_partition_strategy{0, DEFAULT_TEST_WORK_DEPTH_SHIFT});

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == dfs_partitioner_error_kind::invalid_partition_count);
}

BOOST_AUTO_TEST_CASE(dfs_partition_strategy_rejects_unrepresentable_depth_shift) {
    auto graph = create_simple_tree();

    auto result = compute_dfs_partitions(
        graph,
        dfs_partition_strategy{DEFAULT_TEST_PARTITION_COUNT, MAX_REPRESENTABLE_WORK_DEPTH_SHIFT + 1});

    BOOST_REQUIRE(!result);
    BOOST_CHECK(result.error().kind == dfs_partitioner_error_kind::invalid_work_depth_shift);
}

BOOST_AUTO_TEST_CASE(dfs_partition_strategy_accepts_larger_representable_depth_shift) {
    auto graph = create_simple_tree();

    auto result = compute_dfs_partitions(
        graph,
        dfs_partition_strategy{DEFAULT_TEST_PARTITION_COUNT, 32});

    BOOST_CHECK(result.has_value());
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

BOOST_AUTO_TEST_SUITE(cfr_balance)

/** Test partition balance metric. */
BOOST_AUTO_TEST_CASE(dfs_partition_balance_metric) {
    auto graph = create_chance_tree();
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{DEFAULT_TEST_PARTITION_COUNT, DEFAULT_TEST_WORK_DEPTH_SHIFT}));
    
    double balance = ::zeta::holdem::cfr::scheduler::dfs_partition_balance_metric(partitions);
    /** Balance metric should be non-negative; coefficient of variation. */
    BOOST_CHECK_GE(balance, 0.0);
    /** For small graphs, balance might be perfect or close to 0. */
    BOOST_CHECK_LT(balance, 10.0);  /**< Sanity check. */
}

BOOST_AUTO_TEST_CASE(multiple_partitions_balance) {
    graph_builder builder;
    
    /** Create a connected tree with 20 nodes to get multiple partitions. */
    for (int i = 0; i < 20; ++i) {
        (void) builder.add_node(i == 0 ? node_kind::player :
                                i == 19 ? node_kind::terminal : node_kind::chance);
    }
    
    for (int i = 0; i < 19; ++i) {
        builder.add_edge(i, i + 1, 0);
    }
    
    builder.set_infoset_id(0, 0);
    
    auto graph = require_graph(builder.build());
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{DEFAULT_TEST_PARTITION_COUNT, DEFAULT_TEST_WORK_DEPTH_SHIFT}));
    double balance = ::zeta::holdem::cfr::scheduler::dfs_partition_balance_metric(partitions);
    BOOST_CHECK_GE(balance, 0.0);
    BOOST_CHECK_LT(balance, 10.0);
}

BOOST_AUTO_TEST_CASE(dfs_partition_strategy_custom_count) {
    auto graph = create_chance_tree();
    
    constexpr uint32_t target_partition_count = 2;
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{target_partition_count, DEFAULT_TEST_WORK_DEPTH_SHIFT}));
    BOOST_CHECK_LE(partitions.size(), target_partition_count + 1u);
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
