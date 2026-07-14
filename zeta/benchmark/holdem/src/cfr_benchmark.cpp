#include <benchmark/benchmark.h>
#include "cfr/graph.h"
#include <cstdlib>
#include <random>

using namespace zeta::holdem::cfr;

game_graph require_graph(std::expected<game_graph, graph_build_error> result)
{
    if (!result) {
        std::abort();
    }
    return std::move(*result);
}

/**
 * Helper to create a tree with N nodes in a specific topology.
 * Creates a branching tree where each non-terminal has `branching_factor` children.
 */
game_graph create_benchmark_tree(uint32_t branching_factor, uint32_t depth)
{
    graph_builder builder;
    
    // Calculate total nodes needed
    uint32_t total_nodes = 0;
    uint32_t nodes_at_depth = 1;
    for (uint16_t d = 0; d <= depth; ++d) {
        total_nodes += nodes_at_depth;
        nodes_at_depth *= branching_factor;
    }
    
    builder = graph_builder(total_nodes);
    
    // Node ID counter
    uint32_t node_id = 0;
    
    // Add nodes and compute tree structure
    struct node_info {
        uint32_t id;
        uint32_t depth;
    };
    
    std::vector<node_info> level;
    level.push_back({node_id++, 0});
    
    // Root is player node
    (void) builder.add_node(node_kind::player);
    builder.set_infoset_id(0, 0);
    
    uint32_t infoset_count = 1;
    
    for (uint16_t d = 0; d < depth; ++d) {
        std::vector<node_info> next_level;
        
        for (const auto& parent : level) {
            if (parent.depth >= depth) {
                continue;
            }
            
            // Add children
            for (uint16_t child_idx = 0; child_idx < branching_factor; ++child_idx) {
                uint32_t child_id = node_id++;
                
                // Alternate node types: player, chance, player, ...
                node_kind kind;
                if (d == depth - 1) {
                    kind = node_kind::terminal;
                } else if ((d + child_idx) % 2 == 0) {
                    kind = node_kind::player;
                } else {
                    kind = node_kind::chance;
                }
                
                (void) builder.add_node(kind);
                
                // Add edge from parent to child
                builder.add_edge(parent.id, child_id, child_idx);
                
                // Set infoset for player nodes
                if (kind == node_kind::player) {
                    builder.set_infoset_id(child_id, infoset_count++);
                }
                
                next_level.push_back({child_id, static_cast<uint16_t>(d + 1)});
            }
        }
        
        level = next_level;
    }
    
    return require_graph(builder.build());
}

// ============================================================================
// S1.6 - Graph build time benchmark
// ============================================================================

static void BM_GraphBuildSmallTree(benchmark::State& state)
{
    for (auto _ : state) {
        graph_builder builder;
        
        auto root = builder.add_node(node_kind::player);
        (void) builder.add_node(node_kind::terminal);
        (void) builder.add_node(node_kind::terminal);
        
        builder.add_edge(root, 1, 0);
        builder.add_edge(root, 2, 1);
        builder.set_infoset_id(root, 0);
        
        auto graph = require_graph(builder.build());
        benchmark::DoNotOptimize(graph);
    }
}

static void BM_GraphBuildMediumTree(benchmark::State& state)
{
    for (auto _ : state) {
        auto graph = create_benchmark_tree(4, 4);
        benchmark::DoNotOptimize(graph);
    }
}

static void BM_GraphBuildLargeTree(benchmark::State& state)
{
    for (auto _ : state) {
        auto graph = create_benchmark_tree(3, 6);
        benchmark::DoNotOptimize(graph);
    }
}

BENCHMARK(BM_GraphBuildSmallTree);
BENCHMARK(BM_GraphBuildMediumTree);
BENCHMARK(BM_GraphBuildLargeTree);

// ============================================================================
// Partition build time benchmark
// ============================================================================

static void BM_PartitionComputeSmallTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(2, 2);
    
    for (auto _ : state) {
        partition_strategy strategy;
        strategy.target_partition_count = 2;
        
        auto partitions = compute_partitions(graph, strategy);
        benchmark::DoNotOptimize(partitions);
    }
}

static void BM_PartitionComputeMediumTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    
    for (auto _ : state) {
        partition_strategy strategy;
        strategy.target_partition_count = 8;
        
        auto partitions = compute_partitions(graph, strategy);
        benchmark::DoNotOptimize(partitions);
    }
}

static void BM_PartitionComputeLargeTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(3, 6);
    
    for (auto _ : state) {
        partition_strategy strategy;
        strategy.target_partition_count = 16;
        
        auto partitions = compute_partitions(graph, strategy);
        benchmark::DoNotOptimize(partitions);
    }
}

BENCHMARK(BM_PartitionComputeSmallTree);
BENCHMARK(BM_PartitionComputeMediumTree);
BENCHMARK(BM_PartitionComputeLargeTree);

// ============================================================================
// Traversal scan throughput benchmark
// ============================================================================

static void BM_TraversalScanSmallTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(2, 2);
    
    for (auto _ : state) {
        uint64_t total_edges = 0;
        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            auto edges = graph.out_edges(node_id);
            total_edges += edges.size();
        }
        benchmark::DoNotOptimize(total_edges);
    }
}

static void BM_TraversalScanMediumTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    
    for (auto _ : state) {
        uint64_t total_edges = 0;
        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            auto edges = graph.out_edges(node_id);
            total_edges += edges.size();
        }
        benchmark::DoNotOptimize(total_edges);
    }
}

static void BM_TraversalScanLargeTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(3, 6);
    
    for (auto _ : state) {
        uint64_t total_edges = 0;
        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            auto edges = graph.out_edges(node_id);
            total_edges += edges.size();
        }
        benchmark::DoNotOptimize(total_edges);
    }
}

BENCHMARK(BM_TraversalScanSmallTree);
BENCHMARK(BM_TraversalScanMediumTree);
BENCHMARK(BM_TraversalScanLargeTree);

// ============================================================================
// Validation throughput benchmark
// ============================================================================

static void BM_ValidateSmallTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(2, 2);
    
    for (auto _ : state) {
        bool valid = graph_validator::validate(graph);
        benchmark::DoNotOptimize(valid);
    }
}

static void BM_ValidateMediumTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    
    for (auto _ : state) {
        bool valid = graph_validator::validate(graph);
        benchmark::DoNotOptimize(valid);
    }
}

static void BM_ValidateLargeTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(3, 6);
    
    for (auto _ : state) {
        bool valid = graph_validator::validate(graph);
        benchmark::DoNotOptimize(valid);
    }
}

BENCHMARK(BM_ValidateSmallTree);
BENCHMARK(BM_ValidateMediumTree);
BENCHMARK(BM_ValidateLargeTree);

// ============================================================================
// Partition balance metric benchmark
// ============================================================================

static void BM_BalanceMetricSmallTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(2, 2);
    
    for (auto _ : state) {
        double balance = graph.partition_balance_metric();
        benchmark::DoNotOptimize(balance);
    }
}

static void BM_BalanceMetricMediumTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    
    for (auto _ : state) {
        double balance = graph.partition_balance_metric();
        benchmark::DoNotOptimize(balance);
    }
}

static void BM_BalanceMetricLargeTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(3, 6);
    
    for (auto _ : state) {
        double balance = graph.partition_balance_metric();
        benchmark::DoNotOptimize(balance);
    }
}

BENCHMARK(BM_BalanceMetricSmallTree);
BENCHMARK(BM_BalanceMetricMediumTree);
BENCHMARK(BM_BalanceMetricLargeTree);

// Main entry point
BENCHMARK_MAIN();
