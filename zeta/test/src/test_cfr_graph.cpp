#include <boost/test/unit_test.hpp>

#include "cfr/graph/builder.h"
#include "cfr/graph/graph.h"
#include "cfr/graph/validation.h"
#include "cfr/scheduler/dfs_partitioner.h"
#include "cfr/tables/delta_buffer.h"

#include <array>

using namespace zeta::holdem::cfr;
using namespace zeta::holdem::cfr::scheduler;

namespace {
    constexpr uint32_t DEFAULT_TEST_PARTITION_COUNT = 8;
    constexpr uint32_t DEFAULT_TEST_WORK_DEPTH_SHIFT = 16;
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
