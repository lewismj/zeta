#include <boost/test/unit_test.hpp>

#include "cfr/graph/builder.h"
#include "cfr/graph/graph.h"
#include "cfr/graph/validation.h"
#include "cfr/scheduler/dfs_partitioner.h"
#include "cfr/scheduler/scheduler.h"
#include "cfr/solver/context.h"
#include "cfr/solver/iteration.h"
#include "cfr/solver/river_context.h"
#include "cfr/tables/delta_buffer.h"
#include "cfr/traversal/traversal.h"

#include <array>
#include <atomic>
#include <future>
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
    const auto edge_probability_capacity = worker.edge_probability.capacity();
    const auto delta_entry_capacity = worker.delta_buffer.entry_capacity();
    const auto regret_delta_capacity = worker.delta_buffer.regret_delta_capacity();
    const auto strategy_delta_capacity = worker.delta_buffer.strategy_delta_capacity();

    for (int run = 0; run < 2; ++run) {
        auto result = traverse_game_tree(worker);
        BOOST_REQUIRE(result.has_value());
        BOOST_CHECK_EQUAL(worker.stack.capacity(), stack_capacity);
        BOOST_CHECK_EQUAL(worker.node_utility.capacity(), node_utility_capacity);
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

    std::vector<river_terminal_leaf> leaves(graph.node_count);
    leaves[0] = river_terminal_leaf{river_terminal_leaf_kind::showdown, context};
    leaves[1] = river_terminal_leaf{river_terminal_leaf_kind::showdown, context};
    const auto terminal_context = make_river_solver_context(
        deterministic_river_board(),
        std::array<zeta::holdem::reach_vector, 2>{oop_reach, ip_reach},
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

    std::vector<river_terminal_leaf> leaves(graph.node_count);
    leaves[0] = river_terminal_leaf{river_terminal_leaf_kind::fold, context, zeta::holdem::heads_up_player::ip};
    leaves[1] = river_terminal_leaf{river_terminal_leaf_kind::fold, context, zeta::holdem::heads_up_player::ip};

    worker_context worker;
    require_prepared_worker(worker, graph, regrets);

    const river_terminal_leaf_policy policy{
        .river_cache = &cache,
        .reach_indices = reach_indices,
        .terminal_leaves = leaves,
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
