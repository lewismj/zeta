#include <benchmark/benchmark.h>
#include "cfr/graph/builder.h"
#include "cfr/graph/graph.h"
#include "cfr/graph/validation.h"
#include "cfr/scheduler/dfs_partitioner.h"
#include "cfr/scheduler/scheduler.h"
#include "cfr/solver/iteration.h"
#include "cfr/tables/regret_table.h"
#include "cfr/tables/strategy_table.h"
#include "cfr/traversal/traversal.h"
#include <atomic>
#include <cstdlib>
#include <random>

using namespace zeta::holdem::cfr;
using namespace zeta::holdem::cfr::scheduler;
using namespace zeta::holdem::cfr::solver;
using namespace zeta::holdem::cfr::traversal;

namespace {
    constexpr uint32_t SMALL_BENCHMARK_PARTITION_COUNT = 2;
    constexpr uint32_t MEDIUM_BENCHMARK_PARTITION_COUNT = 8;
    constexpr uint32_t LARGE_BENCHMARK_PARTITION_COUNT = 16;
    constexpr uint32_t BENCHMARK_WORK_DEPTH_SHIFT = 16;

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
        std::abort();
    }
}

game_graph require_graph(std::expected<game_graph, graph_build_error> result)
{
    if (!result) {
        std::abort();
    }
    return std::move(*result);
}

std::vector<graph_partition> require_partitions(
    std::expected<std::vector<graph_partition>, dfs_partitioner_error> result)
{
    if (!result) {
        std::abort();
    }
    return std::move(*result);
}

scheduler_run_summary require_schedule_summary(
    std::expected<scheduler_run_summary, scheduler_error> result)
{
    if (!result) {
        std::abort();
    }
    return std::move(*result);
}

action_table_layout require_layout(std::expected<action_table_layout, table_layout_error> result)
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
    
    /** Calculate total nodes needed. */
    uint32_t total_nodes = 0;
    uint32_t nodes_at_depth = 1;
    for (uint16_t d = 0; d <= depth; ++d) {
        total_nodes += nodes_at_depth;
        nodes_at_depth *= branching_factor;
    }
    
    builder = graph_builder(total_nodes);
    
    /** Node ID counter. */
    uint32_t node_id = 0;
    
    /** Add nodes and compute tree structure. */
    struct node_info {
        uint32_t id;
        uint32_t depth;
    };
    
    std::vector<node_info> level;
    level.push_back({node_id++, 0});
    
    /** Root is player node. */
    (void) builder.add_node(node_kind::player);
    builder.set_infoset_id(0, 0);
    
    uint32_t infoset_count = 1;
    
    for (uint16_t d = 0; d < depth; ++d) {
        std::vector<node_info> next_level;
        
        for (const auto& parent : level) {
            if (parent.depth >= depth) {
                continue;
            }
            
            /** Add children. */
            for (uint16_t child_idx = 0; child_idx < branching_factor; ++child_idx) {
                uint32_t child_id = node_id++;
                
                /** Alternate node types: player, chance, player, and so on. */
                node_kind kind;
                if (d == depth - 1) {
                    kind = node_kind::terminal;
                } else if ((d + child_idx) % 2 == 0) {
                    kind = node_kind::player;
                } else {
                    kind = node_kind::chance;
                }
                
                (void) builder.add_node(kind);
                
                /** Add edge from parent to child. */
                builder.add_edge(parent.id, child_id, child_idx);
                
                /** Set infoset for player nodes. */
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

/**
 * Graph build time benchmark.
 */

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

/**
 * Partition build time benchmark.
 */

static void BM_PartitionComputeSmallTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(2, 2);
    
    for (auto _ : state) {
        auto partitions = require_partitions(
            compute_dfs_partitions(
                graph,
                dfs_partition_strategy{SMALL_BENCHMARK_PARTITION_COUNT, BENCHMARK_WORK_DEPTH_SHIFT}));
        benchmark::DoNotOptimize(partitions);
    }
}

static void BM_PartitionComputeMediumTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    
    for (auto _ : state) {
        auto partitions = require_partitions(
            compute_dfs_partitions(
                graph,
                dfs_partition_strategy{MEDIUM_BENCHMARK_PARTITION_COUNT, BENCHMARK_WORK_DEPTH_SHIFT}));
        benchmark::DoNotOptimize(partitions);
    }
}

static void BM_PartitionComputeLargeTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(3, 6);
    
    for (auto _ : state) {
        auto partitions = require_partitions(
            compute_dfs_partitions(
                graph,
                dfs_partition_strategy{LARGE_BENCHMARK_PARTITION_COUNT, BENCHMARK_WORK_DEPTH_SHIFT}));
        benchmark::DoNotOptimize(partitions);
    }
}

BENCHMARK(BM_PartitionComputeSmallTree);
BENCHMARK(BM_PartitionComputeMediumTree);
BENCHMARK(BM_PartitionComputeLargeTree);

static void BM_BoardPartitionSchedulerRuntime(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{MEDIUM_BENCHMARK_PARTITION_COUNT, BENCHMARK_WORK_DEPTH_SHIFT}));
    auto plan = make_board_partition_plan(32, partitions).value();
    const scheduler_runtime_config config{static_cast<uint32_t>(state.range(0))};

    scheduler_run_summary last_summary;
    for (auto _ : state) {
        std::atomic<uint64_t> scanned_actions{0};
        auto summary = require_schedule_summary(
            run_board_partition_scheduler(
                plan,
                config,
                [&graph, &scanned_actions](const scheduler_worker_state&, const board_partition_task& task) {
                    uint64_t local_actions = 0;
                    for (uint32_t node_id = task.partition->begin_node; node_id < task.partition->end_node; ++node_id) {
                        local_actions += graph.action_count(node_id);
                    }
                    scanned_actions.fetch_add(local_actions, std::memory_order_relaxed);
                }));
        benchmark::DoNotOptimize(scanned_actions.load(std::memory_order_relaxed));
        benchmark::ClobberMemory();
        last_summary = std::move(summary);
    }

    state.counters["workers"] = static_cast<double>(config.worker_count);
    state.counters["boards"] = static_cast<double>(plan.board_count);
    state.counters["partitions"] = static_cast<double>(plan.partitions.size());
    state.counters["tasks/s"] = benchmark::Counter(
        static_cast<double>(last_summary.tasks_executed),
        benchmark::Counter::kIsIterationInvariantRate);
}

BENCHMARK(BM_BoardPartitionSchedulerRuntime)->Arg(1)->Arg(2)->Arg(4)->Arg(8);

/**
 * Traversal scan throughput benchmark.
 */

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

/**
 * Worker-local traversal kernel benchmark.
 */

void set_traversal_counters(
    benchmark::State& state,
    const game_graph& graph,
    const traversal_result& result,
    const worker_context& worker)
{
    state.counters["nodes/s"] = benchmark::Counter(
        static_cast<double>(graph.node_count),
        benchmark::Counter::kIsIterationInvariantRate);
    state.counters["edges/s"] = benchmark::Counter(
        static_cast<double>(graph.edges.size()),
        benchmark::Counter::kIsIterationInvariantRate);
    state.counters["stack_high_water"] = static_cast<double>(result.diagnostics.max_stack_depth);
    state.counters["delta_entries"] = static_cast<double>(worker.delta_buffer.entry_count());
    state.counters["node_scratch"] = static_cast<double>(worker.node_utility.size());
}

static void BM_WorkerTraversalSmallTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(2, 2);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    worker_context worker;
    if (!prepare_worker_context(worker, graph, regrets)) {
        std::abort();
    }

    traversal_config config;
    config.initial_reach_oop = 2.0f;
    config.initial_reach_ip = 1.0f;

    traversal_result last_result;
    for (auto _ : state) {
        auto result = traverse_game_tree(worker, config);
        if (!result) {
            state.SkipWithError(to_string(result.error().kind));
            break;
        }
        last_result = *result;
        benchmark::DoNotOptimize(last_result.root_utility);
        benchmark::ClobberMemory();
    }
    set_traversal_counters(state, graph, last_result, worker);
}

static void BM_WorkerTraversalMediumTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    worker_context worker;
    if (!prepare_worker_context(worker, graph, regrets)) {
        std::abort();
    }

    traversal_config config;
    config.initial_reach_oop = 2.0f;
    config.initial_reach_ip = 1.0f;

    traversal_result last_result;
    for (auto _ : state) {
        auto result = traverse_game_tree(worker, config);
        if (!result) {
            state.SkipWithError(to_string(result.error().kind));
            break;
        }
        last_result = *result;
        benchmark::DoNotOptimize(last_result.root_utility);
        benchmark::ClobberMemory();
    }
    set_traversal_counters(state, graph, last_result, worker);
}

static void BM_WorkerTraversalLargeTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(3, 6);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    worker_context worker;
    if (!prepare_worker_context(worker, graph, regrets)) {
        std::abort();
    }

    traversal_config config;
    config.initial_reach_oop = 2.0f;
    config.initial_reach_ip = 1.0f;

    traversal_result last_result;
    for (auto _ : state) {
        auto result = traverse_game_tree(worker, config);
        if (!result) {
            state.SkipWithError(to_string(result.error().kind));
            break;
        }
        last_result = *result;
        benchmark::DoNotOptimize(last_result.root_utility);
        benchmark::ClobberMemory();
    }
    set_traversal_counters(state, graph, last_result, worker);
}

BENCHMARK(BM_WorkerTraversalSmallTree);
BENCHMARK(BM_WorkerTraversalMediumTree);
BENCHMARK(BM_WorkerTraversalLargeTree);

static void BM_RiverTerminalLeafTraversal(benchmark::State& state)
{
    auto graph = require_graph([] {
        graph_builder builder;
        auto root = builder.add_node(node_kind::player);
        auto showdown = builder.add_node(node_kind::terminal);
        auto fold = builder.add_node(node_kind::terminal);
        builder.add_edge(root, showdown, 0);
        builder.add_edge(root, fold, 1);
        builder.set_infoset_id(root, 0);
        return builder.build();
    }());
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    worker_context worker;
    if (!prepare_worker_context(worker, graph, regrets)) {
        std::abort();
    }

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
    leaves[0] = river_terminal_leaf{river_terminal_leaf_kind::showdown, context};
    leaves[1] = river_terminal_leaf{river_terminal_leaf_kind::fold, context, zeta::holdem::heads_up_player::ip};
    const river_terminal_leaf_policy policy{
        .river_cache = &cache,
        .reach_indices = reach_indices,
        .terminal_leaves = leaves,
        .perspective = zeta::holdem::heads_up_player::oop,
        .combo = oop_combo
    };

    traversal_result last_result;
    for (auto _ : state) {
        auto result = traverse_game_tree(worker, policy);
        if (!result) {
            state.SkipWithError(to_string(result.error().kind));
            break;
        }
        last_result = *result;
        benchmark::DoNotOptimize(last_result.root_utility);
        benchmark::ClobberMemory();
    }
    set_traversal_counters(state, graph, last_result, worker);
}

static void BM_DeterministicWorkerReduction(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    auto layout = require_layout(make_action_table_layout(graph));
    const auto worker_count = static_cast<std::size_t>(state.range(0));

    std::vector<worker_context> workers(worker_count);
    for (auto& worker : workers) {
        if (auto result = worker.delta_buffer.reset_layout(layout.action_offsets); !result) {
            std::abort();
        }
        for (uint32_t infoset_id = 0; infoset_id < layout.infoset_count(); ++infoset_id) {
            if (layout.action_count(infoset_id) > 0u) {
                worker.delta_buffer.add_regret_delta(infoset_id, 0, 1.0f);
                worker.delta_buffer.add_strategy_delta(infoset_id, 0, 1.0f);
            }
        }
    }

    for (auto _ : state) {
        regret_table regrets(layout);
        strategy_sum_table strategy_sums(layout);
        auto result = apply_worker_reductions(regrets, strategy_sums, std::span<const worker_context>{workers});
        if (!result) {
            state.SkipWithError(to_string(result.error().kind));
            break;
        }
        benchmark::DoNotOptimize(regrets.regrets.data());
        benchmark::DoNotOptimize(strategy_sums.sums.data());
        benchmark::ClobberMemory();
    }

    state.counters["workers"] = static_cast<double>(worker_count);
    state.counters["delta_entries"] = static_cast<double>(workers.front().delta_buffer.entry_count() * worker_count);
}

BENCHMARK(BM_RiverTerminalLeafTraversal);
BENCHMARK(BM_DeterministicWorkerReduction)->Arg(2)->Arg(4)->Arg(8);

/**
 * Validation throughput benchmark.
 */

static void BM_ValidateSmallTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(2, 2);
    
    for (auto _ : state) {
        bool valid = ::zeta::holdem::cfr::graph_validation::validate(graph);
        benchmark::DoNotOptimize(valid);
    }
}

static void BM_ValidateMediumTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    
    for (auto _ : state) {
        bool valid = ::zeta::holdem::cfr::graph_validation::validate(graph);
        benchmark::DoNotOptimize(valid);
    }
}

static void BM_ValidateLargeTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(3, 6);
    
    for (auto _ : state) {
        bool valid = ::zeta::holdem::cfr::graph_validation::validate(graph);
        benchmark::DoNotOptimize(valid);
    }
}

BENCHMARK(BM_ValidateSmallTree);
BENCHMARK(BM_ValidateMediumTree);
BENCHMARK(BM_ValidateLargeTree);

/**
 * Partition balance metric benchmark.
 */

static void BM_BalanceMetricSmallTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(2, 2);
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{SMALL_BENCHMARK_PARTITION_COUNT, BENCHMARK_WORK_DEPTH_SHIFT}));
    
    for (auto _ : state) {
        double balance = dfs_partition_balance_metric(partitions);
        benchmark::DoNotOptimize(balance);
    }
}

static void BM_BalanceMetricMediumTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{MEDIUM_BENCHMARK_PARTITION_COUNT, BENCHMARK_WORK_DEPTH_SHIFT}));
    
    for (auto _ : state) {
        double balance = dfs_partition_balance_metric(partitions);
        benchmark::DoNotOptimize(balance);
    }
}

static void BM_BalanceMetricLargeTree(benchmark::State& state)
{
    auto graph = create_benchmark_tree(3, 6);
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{LARGE_BENCHMARK_PARTITION_COUNT, BENCHMARK_WORK_DEPTH_SHIFT}));
    
    for (auto _ : state) {
        double balance = dfs_partition_balance_metric(partitions);
        benchmark::DoNotOptimize(balance);
    }
}

BENCHMARK(BM_BalanceMetricSmallTree);
BENCHMARK(BM_BalanceMetricMediumTree);
BENCHMARK(BM_BalanceMetricLargeTree);

/** Main entry point. */
BENCHMARK_MAIN();
