#include <benchmark/benchmark.h>
#include "cfr/graph/builder.h"
#include "cfr/graph/graph.h"
#include "cfr/graph/validation.h"
#include "cfr/scheduler/dfs_partitioner.h"
#include "cfr/scheduler/scheduler.h"
#include "cfr/solver/context.h"
#include "cfr/solver/iteration.h"
#include "cfr/solver/river_context.h"
#include "cfr/tables/regret_table.h"
#include "cfr/tables/strategy_table.h"
#include "cfr/traversal/traversal.h"
#include <algorithm>
#include <array>
#include <atomic>
#include <cerrno>
#include <condition_variable>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <mutex>
#include <optional>
#include <random>
#include <span>
#include <string>
#include <thread>
#include <type_traits>
#include <vector>
#if defined(__linux__)
#include <linux/perf_event.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <unistd.h>
#endif

using namespace zeta::holdem::cfr;
using namespace zeta::holdem::cfr::scheduler;
using namespace zeta::holdem::cfr::solver;
using namespace zeta::holdem::cfr::traversal;

namespace {
    constexpr uint32_t SMALL_BENCHMARK_PARTITION_COUNT = 2;
    constexpr uint32_t MEDIUM_BENCHMARK_PARTITION_COUNT = 8;
    constexpr uint32_t LARGE_BENCHMARK_PARTITION_COUNT = 16;
    constexpr uint32_t REALISTIC_BENCHMARK_BOARD_COUNT = 8192;
    constexpr uint32_t REALISTIC_BENCHMARK_PARTITION_COUNT = 512;
    constexpr uint32_t REALISTIC_BENCHMARK_TASK_CHUNK_SIZE = 64;
    constexpr uint32_t REALISTIC_BENCHMARK_TASK_WORK_REPEATS = 8;
    constexpr uint32_t CFR_ITERATION_BENCHMARK_BOARD_COUNT = 64;
    constexpr uint32_t CACHED_TERMINAL_BATCH_SIZE = 1024;
    constexpr uint32_t BENCHMARK_WORK_DEPTH_SHIFT = 16;
    constexpr uint64_t HARDWARE_COUNTER_CACHE_LINE_BYTES = 64;

    struct terminal_combo_work_item {
        zeta::holdem::combination_index combo = 0;
        float reach = 0.0f;
        uint16_t payoff_index = 0;
    };

    struct regret_strategy_value {
        float regret = 0.0f;
        float strategy_sum = 0.0f;
    };

    struct alignas(64) benchmark_worker_counter {
        uint64_t actions = 0;
        uint64_t nodes = 0;
        uint64_t terminal_leaves = 0;
        uint64_t regret_updates = 0;
        uint64_t strategy_updates = 0;
    };

    enum class cfr_iteration_hardware_measurement {
        l1_miss_rate,
        llc_miss_rate,
        memory_bandwidth,
    };

    struct hardware_counter_sample {
        uint64_t references = 0;
        uint64_t misses = 0;
        uint64_t cycles = 0;
        uint64_t instructions = 0;
    };

#if defined(__linux__)
    struct perf_counter_spec {
        uint32_t type = 0;
        uint64_t config = 0;
    };

    uint64_t perf_cache_config(const uint64_t cache_id, const uint64_t operation, const uint64_t result)
    {
        return cache_id | (operation << 8u) | (result << 16u);
    }

    int perf_event_open(perf_event_attr& attr, const pid_t pid, const int cpu, const int group_fd, const unsigned long flags)
    {
        return static_cast<int>(::syscall(__NR_perf_event_open, &attr, pid, cpu, group_fd, flags));
    }

    class perf_counter_group {
    public:
        explicit perf_counter_group(std::span<const perf_counter_spec> specs)
        {
            fds_.reserve(specs.size());

            for (const auto& spec : specs) {
                perf_event_attr attr{};
                attr.size = sizeof(attr);
                attr.type = spec.type;
                attr.config = spec.config;
                attr.disabled = fds_.empty() ? 1u : 0u;
                attr.exclude_kernel = 1u;
                attr.exclude_hv = 1u;
                attr.read_format = PERF_FORMAT_GROUP;

                const int group_fd = fds_.empty() ? -1 : fds_.front();
                const int fd = perf_event_open(attr, 0, -1, group_fd, 0);
                if (fd == -1) {
                    error_ = std::strerror(errno);
                    close_fds();
                    return;
                }

                fds_.push_back(fd);
            }
        }

        perf_counter_group(const perf_counter_group&) = delete;
        perf_counter_group& operator=(const perf_counter_group&) = delete;

        ~perf_counter_group()
        {
            close_fds();
        }

        [[nodiscard]] bool valid() const
        {
            return !fds_.empty() && error_.empty();
        }

        [[nodiscard]] const std::string& error() const
        {
            return error_;
        }

        [[nodiscard]] bool start()
        {
            if (::ioctl(fds_.front(), PERF_EVENT_IOC_RESET, PERF_IOC_FLAG_GROUP) == -1) {
                error_ = std::strerror(errno);
                return false;
            }
            if (::ioctl(fds_.front(), PERF_EVENT_IOC_ENABLE, PERF_IOC_FLAG_GROUP) == -1) {
                error_ = std::strerror(errno);
                return false;
            }
            return true;
        }

        [[nodiscard]] std::optional<std::vector<uint64_t>> stop_and_read()
        {
            if (::ioctl(fds_.front(), PERF_EVENT_IOC_DISABLE, PERF_IOC_FLAG_GROUP) == -1) {
                error_ = std::strerror(errno);
                return std::nullopt;
            }

            std::vector<uint64_t> read_buffer(fds_.size() + 1u, 0);
            const auto bytes_to_read = static_cast<ssize_t>(read_buffer.size() * sizeof(uint64_t));
            const auto bytes_read = ::read(fds_.front(), read_buffer.data(), static_cast<size_t>(bytes_to_read));
            if (bytes_read != bytes_to_read) {
                error_ = bytes_read == -1 ? std::strerror(errno) : "short perf counter read";
                return std::nullopt;
            }
            if (read_buffer.front() != fds_.size()) {
                error_ = "unexpected perf counter group size";
                return std::nullopt;
            }

            return std::vector<uint64_t>{read_buffer.begin() + 1, read_buffer.end()};
        }

    private:
        std::vector<int> fds_;
        std::string error_;

        void close_fds()
        {
            for (const auto fd : fds_) {
                ::close(fd);
            }
            fds_.clear();
        }
    };

    std::array<perf_counter_spec, 4> make_miss_rate_perf_specs(const bool llc)
    {
        if (llc) {
            return {{
                {PERF_TYPE_HARDWARE, PERF_COUNT_HW_CACHE_REFERENCES},
                {PERF_TYPE_HARDWARE, PERF_COUNT_HW_CACHE_MISSES},
                {PERF_TYPE_HARDWARE, PERF_COUNT_HW_CPU_CYCLES},
                {PERF_TYPE_HARDWARE, PERF_COUNT_HW_INSTRUCTIONS},
            }};
        }

        return {{
            {PERF_TYPE_HW_CACHE, perf_cache_config(
                PERF_COUNT_HW_CACHE_L1D,
                PERF_COUNT_HW_CACHE_OP_READ,
                PERF_COUNT_HW_CACHE_RESULT_ACCESS)},
            {PERF_TYPE_HW_CACHE, perf_cache_config(
                PERF_COUNT_HW_CACHE_L1D,
                PERF_COUNT_HW_CACHE_OP_READ,
                PERF_COUNT_HW_CACHE_RESULT_MISS)},
            {PERF_TYPE_HARDWARE, PERF_COUNT_HW_CPU_CYCLES},
            {PERF_TYPE_HARDWARE, PERF_COUNT_HW_INSTRUCTIONS},
        }};
    }

    std::array<perf_counter_spec, 3> make_memory_bandwidth_perf_specs()
    {
        return {{
            {PERF_TYPE_HARDWARE, PERF_COUNT_HW_CACHE_MISSES},
            {PERF_TYPE_HARDWARE, PERF_COUNT_HW_CPU_CYCLES},
            {PERF_TYPE_HARDWARE, PERF_COUNT_HW_INSTRUCTIONS},
        }};
    }
#endif

    class benchmark_scheduler_pool {
    public:
        explicit benchmark_scheduler_pool(const uint32_t worker_count) :
            workers_(worker_count)
        {
            for (uint32_t worker_id = 0; worker_id < worker_count; ++worker_id) {
                workers_[worker_id].worker_id = worker_id;
                threads_.emplace_back([this, worker_id] {
                    worker_loop(worker_id);
                });
            }
        }

        benchmark_scheduler_pool(const benchmark_scheduler_pool&) = delete;
        benchmark_scheduler_pool& operator=(const benchmark_scheduler_pool&) = delete;

        ~benchmark_scheduler_pool()
        {
            {
                const std::lock_guard lock{mutex_};
                stop_ = true;
                ++generation_;
            }
            work_cv_.notify_all();
            for (auto& thread : threads_) {
                thread.join();
            }
        }

        template <typename TaskCallback>
        [[nodiscard]] std::expected<scheduler_run_summary, scheduler_error> run(
            const board_partition_plan& plan,
            TaskCallback&& task_callback,
            const uint32_t task_chunk_size = 1)
        {
            if (plan.board_count == 0u) {
                return std::unexpected(scheduler_error{scheduler_error_kind::invalid_board_count});
            }
            if (plan.partitions.empty()) {
                return std::unexpected(scheduler_error{scheduler_error_kind::empty_partition_plan});
            }
            if (workers_.empty()) {
                return std::unexpected(scheduler_error{scheduler_error_kind::invalid_worker_count});
            }

            {
                const std::lock_guard lock{mutex_};
                for (auto& worker : workers_) {
                    worker.tasks_executed = 0;
                    worker.estimated_work = 0;
                }

                plan_ = &plan;
                callback_ = [&task_callback](
                    const scheduler_worker_state& worker,
                    const board_partition_task& task) -> std::expected<void, scheduler_error> {
                    using result_type = std::invoke_result_t<
                        TaskCallback&,
                        const scheduler_worker_state&,
                        const board_partition_task&>;
                    if constexpr (std::is_void_v<result_type>) {
                        std::invoke(task_callback, worker, task);
                        return {};
                    } else {
                        return std::invoke(task_callback, worker, task);
                    }
                };

                next_task_.store(0, std::memory_order_relaxed);
                completed_workers_ = 0;
                first_error_ = {};
                stop_requested_.store(false, std::memory_order_release);
                task_chunk_size_ = std::max<uint32_t>(task_chunk_size, 1u);
                ++generation_;
            }

            work_cv_.notify_all();

            {
                std::unique_lock lock{mutex_};
                done_cv_.wait(lock, [this] {
                    return completed_workers_ == workers_.size();
                });
            }

            scheduler_run_summary summary;
            summary.workers = workers_;
            for (const auto& worker : summary.workers) {
                summary.tasks_executed += worker.tasks_executed;
                summary.estimated_work += worker.estimated_work;
            }

            if (!first_error_) {
                return std::unexpected(first_error_.error());
            }
            return summary;
        }

    private:
        std::vector<scheduler_worker_state> workers_;
        std::vector<std::thread> threads_;
        std::mutex mutex_;
        std::condition_variable work_cv_;
        std::condition_variable done_cv_;
        std::function<std::expected<void, scheduler_error>(
            const scheduler_worker_state&,
            const board_partition_task&)> callback_;
        const board_partition_plan* plan_ = nullptr;
        std::atomic<uint64_t> next_task_{0};
        std::size_t completed_workers_ = 0;
        uint64_t generation_ = 0;
        uint32_t task_chunk_size_ = 1;
        bool stop_ = false;
        std::atomic<bool> stop_requested_{false};
        std::expected<void, scheduler_error> first_error_{};

        void worker_loop(const uint32_t worker_id)
        {
            uint64_t observed_generation = 0;
            while (true) {
                const board_partition_plan* plan = nullptr;
                {
                    std::unique_lock lock{mutex_};
                    work_cv_.wait(lock, [this, observed_generation] {
                        return stop_ || generation_ != observed_generation;
                    });
                    if (stop_) {
                        break;
                    }
                    observed_generation = generation_;
                    plan = plan_;
                }

                auto& worker = workers_[worker_id];
                while (true) {
                    if (stop_requested_.load(std::memory_order_acquire)) {
                        break;
                    }
                    const auto chunk_begin = next_task_.fetch_add(task_chunk_size_, std::memory_order_relaxed);
                    if (chunk_begin >= plan->task_count()) {
                        break;
                    }

                    const auto chunk_end = std::min<uint64_t>(chunk_begin + task_chunk_size_, plan->task_count());
                    for (uint64_t task_index = chunk_begin; task_index < chunk_end; ++task_index) {
                        if (stop_requested_.load(std::memory_order_acquire)) {
                            break;
                        }

                        const auto task = plan->task_at(task_index);
                        if (auto result = callback_(worker, task); !result) {
                            const std::lock_guard lock{mutex_};
                            if (first_error_.has_value()) {
                                auto error = result.error();
                                error.worker_id = worker.worker_id;
                                error.task_index = task.task_index;
                                error.board_index = task.board_index;
                                error.partition_index = task.partition_index;
                                first_error_ = std::unexpected(error);
                            }
                            stop_requested_.store(true, std::memory_order_release);
                            break;
                        }
                        ++worker.tasks_executed;
                        worker.estimated_work += task.partition->estimated_work;
                    }
                }

                {
                    const std::lock_guard lock{mutex_};
                    ++completed_workers_;
                }
                done_cv_.notify_one();
            }
        }
    };

    class benchmark_board_range_pool {
    public:
        explicit benchmark_board_range_pool(const uint32_t worker_count) :
            worker_count_(worker_count)
        {
            for (uint32_t worker_id = 0; worker_id < worker_count; ++worker_id) {
                threads_.emplace_back([this, worker_id] {
                    worker_loop(worker_id);
                });
            }
        }

        benchmark_board_range_pool(const benchmark_board_range_pool&) = delete;
        benchmark_board_range_pool& operator=(const benchmark_board_range_pool&) = delete;

        ~benchmark_board_range_pool()
        {
            {
                const std::lock_guard lock{mutex_};
                stop_ = true;
                ++generation_;
            }
            work_cv_.notify_all();
            for (auto& thread : threads_) {
                thread.join();
            }
        }

        template <typename TaskCallback>
        void run(const uint32_t board_count, TaskCallback&& task_callback)
        {
            {
                const std::lock_guard lock{mutex_};
                board_count_ = board_count;
                completed_workers_ = 0;
                callback_ = [&task_callback](const uint32_t worker_id, const uint32_t begin_board, const uint32_t end_board) {
                    std::invoke(task_callback, worker_id, begin_board, end_board);
                };
                ++generation_;
            }

            work_cv_.notify_all();

            std::unique_lock lock{mutex_};
            done_cv_.wait(lock, [this] {
                return completed_workers_ == worker_count_;
            });
        }

    private:
        uint32_t worker_count_ = 0;
        uint32_t board_count_ = 0;
        std::vector<std::thread> threads_;
        std::mutex mutex_;
        std::condition_variable work_cv_;
        std::condition_variable done_cv_;
        std::function<void(uint32_t, uint32_t, uint32_t)> callback_;
        std::size_t completed_workers_ = 0;
        uint64_t generation_ = 0;
        bool stop_ = false;

        void worker_loop(const uint32_t worker_id)
        {
            uint64_t observed_generation = 0;
            while (true) {
                uint32_t board_count = 0;
                std::function<void(uint32_t, uint32_t, uint32_t)> callback;
                {
                    std::unique_lock lock{mutex_};
                    work_cv_.wait(lock, [this, observed_generation] {
                        return stop_ || generation_ != observed_generation;
                    });
                    if (stop_) {
                        break;
                    }
                    observed_generation = generation_;
                    board_count = board_count_;
                    callback = callback_;
                }

                const auto base = board_count / worker_count_;
                const auto remainder = board_count % worker_count_;
                const auto begin_board = worker_id * base + std::min(worker_id, remainder);
                const auto end_board = begin_board + base + (worker_id < remainder ? 1u : 0u);
                callback(worker_id, begin_board, end_board);

                {
                    const std::lock_guard lock{mutex_};
                    ++completed_workers_;
                }
                done_cv_.notify_one();
            }
        }
    };

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

void set_scheduler_counters(
    benchmark::State& state,
    const board_partition_plan& plan,
    const scheduler_runtime_config config,
    const scheduler_run_summary& summary,
    const uint64_t actions_scanned,
    const uint32_t task_work_repeats,
    const uint32_t task_chunk_size)
{
    state.counters["workers"] = static_cast<double>(config.worker_count);
    state.counters["boards"] = static_cast<double>(plan.board_count);
    state.counters["partitions"] = static_cast<double>(plan.partitions.size());
    state.counters["task_chunk_size"] = static_cast<double>(task_chunk_size);
    state.counters["task_work_repeats"] = static_cast<double>(task_work_repeats);
    state.counters["actions/s"] = benchmark::Counter(
        static_cast<double>(actions_scanned),
        benchmark::Counter::kIsIterationInvariantRate);
    state.counters["tasks/s"] = benchmark::Counter(
        static_cast<double>(summary.tasks_executed),
        benchmark::Counter::kIsIterationInvariantRate);
}

static void BM_BoardPartitionSchedulerOverhead(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{MEDIUM_BENCHMARK_PARTITION_COUNT, BENCHMARK_WORK_DEPTH_SHIFT}));
    auto plan = make_board_partition_plan(32, partitions).value();
    const scheduler_runtime_config config{static_cast<uint32_t>(state.range(0))};

    scheduler_run_summary last_summary;
    uint64_t last_actions_scanned = 0;
    for (auto _ : state) {
        std::vector<benchmark_worker_counter> counters(config.worker_count);
        auto summary = require_schedule_summary(
            run_board_partition_scheduler(
                plan,
                config,
                [&graph, &counters](const scheduler_worker_state& worker, const board_partition_task& task) {
                    uint64_t local_actions = 0;
                    for (uint32_t node_id = task.partition->begin_node; node_id < task.partition->end_node; ++node_id) {
                        local_actions += graph.action_count(node_id);
                    }
                    counters[worker.worker_id].actions += local_actions;
                }));
        last_actions_scanned = 0;
        for (const auto& counter : counters) {
            last_actions_scanned += counter.actions;
        }
        benchmark::DoNotOptimize(last_actions_scanned);
        benchmark::ClobberMemory();
        last_summary = std::move(summary);
    }

    set_scheduler_counters(state, plan, config, last_summary, last_actions_scanned, 1, config.task_chunk_size);
}

static void BM_BoardPartitionSchedulerRealistic(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 5);
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{REALISTIC_BENCHMARK_PARTITION_COUNT, BENCHMARK_WORK_DEPTH_SHIFT}));
    auto plan = make_board_partition_plan(REALISTIC_BENCHMARK_BOARD_COUNT, partitions).value();
    const scheduler_runtime_config config{static_cast<uint32_t>(state.range(0))};
    benchmark_scheduler_pool pool(config.worker_count);

    scheduler_run_summary last_summary;
    uint64_t last_actions_scanned = 0;
    for (auto _ : state) {
        std::vector<benchmark_worker_counter> counters(config.worker_count);
        auto summary = require_schedule_summary(
            pool.run(
                plan,
                [&graph, &counters](const scheduler_worker_state& worker, const board_partition_task& task) {
                    uint64_t local_actions = 0;
                    for (uint32_t repeat = 0; repeat < REALISTIC_BENCHMARK_TASK_WORK_REPEATS; ++repeat) {
                        for (uint32_t node_id = task.partition->begin_node; node_id < task.partition->end_node; ++node_id) {
                            local_actions += graph.action_count(node_id);
                        }
                    }
                    counters[worker.worker_id].actions += local_actions;
                },
                REALISTIC_BENCHMARK_TASK_CHUNK_SIZE));
        last_actions_scanned = 0;
        for (const auto& counter : counters) {
            last_actions_scanned += counter.actions;
        }
        benchmark::DoNotOptimize(last_actions_scanned);
        benchmark::ClobberMemory();
        last_summary = std::move(summary);
    }

    set_scheduler_counters(
        state,
        plan,
        config,
        last_summary,
        last_actions_scanned,
        REALISTIC_BENCHMARK_TASK_WORK_REPEATS,
        REALISTIC_BENCHMARK_TASK_CHUNK_SIZE);
}

static void BM_BoardPartitionStaticRangeRealistic(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 5);
    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{REALISTIC_BENCHMARK_PARTITION_COUNT, BENCHMARK_WORK_DEPTH_SHIFT}));
    const auto worker_count = static_cast<uint32_t>(state.range(0));
    benchmark_board_range_pool pool(worker_count);

    uint64_t last_actions_scanned = 0;
    for (auto _ : state) {
        std::vector<benchmark_worker_counter> counters(worker_count);
        pool.run(
            REALISTIC_BENCHMARK_BOARD_COUNT,
            [&graph, &partitions, &counters](const uint32_t worker_id, const uint32_t begin_board, const uint32_t end_board) {
                uint64_t local_actions = 0;
                for (uint32_t board = begin_board; board < end_board; ++board) {
                    benchmark::DoNotOptimize(board);
                    for (const auto& partition : partitions) {
                        for (uint32_t repeat = 0; repeat < REALISTIC_BENCHMARK_TASK_WORK_REPEATS; ++repeat) {
                            for (uint32_t node_id = partition.begin_node; node_id < partition.end_node; ++node_id) {
                                local_actions += graph.action_count(node_id);
                            }
                        }
                    }
                }
                counters[worker_id].actions += local_actions;
            });

        last_actions_scanned = 0;
        for (const auto& counter : counters) {
            last_actions_scanned += counter.actions;
        }
        benchmark::DoNotOptimize(last_actions_scanned);
        benchmark::ClobberMemory();
    }

    state.counters["workers"] = static_cast<double>(worker_count);
    state.counters["boards"] = static_cast<double>(REALISTIC_BENCHMARK_BOARD_COUNT);
    state.counters["partitions"] = static_cast<double>(partitions.size());
    state.counters["task_work_repeats"] = static_cast<double>(REALISTIC_BENCHMARK_TASK_WORK_REPEATS);
    state.counters["actions/s"] = benchmark::Counter(
        static_cast<double>(last_actions_scanned),
        benchmark::Counter::kIsIterationInvariantRate);
}

BENCHMARK(BM_BoardPartitionSchedulerOverhead)->Arg(1)->Arg(2)->Arg(4)->Arg(8)->Arg(12)->UseRealTime();
BENCHMARK(BM_BoardPartitionSchedulerRealistic)->Arg(1)->Arg(2)->Arg(4)->Arg(8)->Arg(12)->UseRealTime();
BENCHMARK(BM_BoardPartitionStaticRangeRealistic)->Arg(1)->Arg(2)->Arg(4)->Arg(8)->Arg(12)->UseRealTime();

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

void set_cfr_iteration_counters(
    benchmark::State& state,
    const game_graph& graph,
    const uint32_t worker_count,
    const uint32_t board_count,
    std::span<const benchmark_worker_counter> counters)
{
    benchmark_worker_counter total{};
    for (const auto& counter : counters) {
        total.nodes += counter.nodes;
        total.terminal_leaves += counter.terminal_leaves;
        total.regret_updates += counter.regret_updates;
        total.strategy_updates += counter.strategy_updates;
    }

    state.counters["workers"] = static_cast<double>(worker_count);
    state.counters["boards"] = static_cast<double>(board_count);
    state.counters["iterations/s"] = benchmark::Counter(1.0, benchmark::Counter::kIsIterationInvariantRate);
    state.counters["nodes/s"] = benchmark::Counter(
        static_cast<double>(total.nodes),
        benchmark::Counter::kIsIterationInvariantRate);
    state.counters["terminal_leaves/s"] = benchmark::Counter(
        static_cast<double>(total.terminal_leaves),
        benchmark::Counter::kIsIterationInvariantRate);
    state.counters["regret_updates/s"] = benchmark::Counter(
        static_cast<double>(total.regret_updates),
        benchmark::Counter::kIsIterationInvariantRate);
    state.counters["strategy_updates/s"] = benchmark::Counter(
        static_cast<double>(total.strategy_updates),
        benchmark::Counter::kIsIterationInvariantRate);
    state.counters["graph_nodes"] = static_cast<double>(graph.node_count);
}

template <typename BeforeBenchmark, typename AfterBenchmark>
void run_cfr_iteration_benchmark_impl(
    benchmark::State& state,
    const game_graph& graph,
    const uint32_t board_count = CFR_ITERATION_BENCHMARK_BOARD_COUNT,
    const uint32_t task_chunk_size = 1,
    BeforeBenchmark&& before_benchmark = [] {},
    AfterBenchmark&& after_benchmark = [] {})
{
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    const auto worker_count = static_cast<uint32_t>(state.range(0));
    benchmark_scheduler_pool pool(worker_count);

    auto partitions = require_partitions(
        compute_dfs_partitions(
            graph,
            dfs_partition_strategy{1, BENCHMARK_WORK_DEPTH_SHIFT}));
    auto plan = make_board_partition_plan(board_count, partitions).value();

    std::vector<worker_context> workers(worker_count);
    for (auto& worker : workers) {
        if (!prepare_worker_context(worker, graph, regrets)) {
            std::abort();
        }
    }

    traversal_config traversal_cfg;
    traversal_cfg.initial_reach_oop = 2.0f;
    traversal_cfg.initial_reach_ip = 1.0f;

    std::vector<benchmark_worker_counter> counters(worker_count);
    const auto should_run = [&] {
        if constexpr (std::is_same_v<std::invoke_result_t<BeforeBenchmark>, bool>) {
            return std::invoke(before_benchmark);
        } else {
            std::invoke(before_benchmark);
            return true;
        }
    }();
    if (!should_run) {
        return;
    }

    for (auto _ : state) {
        std::fill(counters.begin(), counters.end(), benchmark_worker_counter{});

        auto schedule_result = pool.run(
            plan,
            [&workers, &counters, traversal_cfg](
                const scheduler_worker_state& worker_state,
                const board_partition_task&) -> std::expected<void, scheduler_error> {
                auto& worker = workers[worker_state.worker_id];
                auto result = traverse_game_tree(worker, traversal_cfg);
                if (!result) {
                    return std::unexpected(scheduler_error{scheduler_error_kind::task_failed});
                }

                auto& counter = counters[worker_state.worker_id];
                counter.nodes += result->diagnostics.nodes_visited;
                counter.terminal_leaves += result->diagnostics.terminal_nodes;
                counter.strategy_updates += result->diagnostics.local_delta_entries_touched;
                return std::expected<void, scheduler_error>{};
            },
            task_chunk_size);
        if (!schedule_result) {
            state.SkipWithError(to_string(schedule_result.error().kind));
            break;
        }

        if (auto reduction_result = apply_worker_reductions(regrets, strategy_sums, std::span<const worker_context>{workers});
            !reduction_result) {
            state.SkipWithError(to_string(reduction_result.error().kind));
            break;
        }

        benchmark::DoNotOptimize(regrets.regrets.data());
        benchmark::DoNotOptimize(strategy_sums.sums.data());
        benchmark::ClobberMemory();
    }
    std::invoke(after_benchmark);

    set_cfr_iteration_counters(
        state,
        graph,
        worker_count,
        board_count,
        counters);
    state.counters["task_chunk_size"] = static_cast<double>(task_chunk_size);
}

void run_cfr_iteration_benchmark(
    benchmark::State& state,
    const game_graph& graph,
    const uint32_t board_count = CFR_ITERATION_BENCHMARK_BOARD_COUNT,
    const uint32_t task_chunk_size = 1)
{
    run_cfr_iteration_benchmark_impl(state, graph, board_count, task_chunk_size, [] {}, [] {});
}

#if defined(__linux__)
void set_common_hardware_counters(benchmark::State& state, const hardware_counter_sample& sample)
{
    state.counters["cycles/iter"] = benchmark::Counter(
        static_cast<double>(sample.cycles),
        benchmark::Counter::kAvgIterations);
    state.counters["instructions/iter"] = benchmark::Counter(
        static_cast<double>(sample.instructions),
        benchmark::Counter::kAvgIterations);
    state.counters["instructions/cycle"] = sample.cycles == 0u
        ? 0.0
        : static_cast<double>(sample.instructions) / static_cast<double>(sample.cycles);
}

void set_miss_rate_counters(
    benchmark::State& state,
    const hardware_counter_sample& sample,
    const char* reference_counter_name,
    const char* miss_counter_name,
    const char* miss_rate_counter_name)
{
    state.counters[reference_counter_name] = benchmark::Counter(
        static_cast<double>(sample.references),
        benchmark::Counter::kAvgIterations);
    state.counters[miss_counter_name] = benchmark::Counter(
        static_cast<double>(sample.misses),
        benchmark::Counter::kAvgIterations);
    state.counters[miss_rate_counter_name] = sample.references == 0u
        ? 0.0
        : static_cast<double>(sample.misses) / static_cast<double>(sample.references);
    set_common_hardware_counters(state, sample);
}
#endif

void run_cfr_iteration_hardware_benchmark(
    benchmark::State& state,
    const cfr_iteration_hardware_measurement measurement)
{
#if defined(__linux__)
    std::optional<hardware_counter_sample> sample;
    auto graph = create_benchmark_tree(3, 6);

    switch (measurement) {
    case cfr_iteration_hardware_measurement::l1_miss_rate: {
        const auto specs = make_miss_rate_perf_specs(false);
        perf_counter_group counters{specs};
        if (!counters.valid()) {
            state.SkipWithError(counters.error().c_str());
            return;
        }
        run_cfr_iteration_benchmark_impl(
            state,
            graph,
            REALISTIC_BENCHMARK_BOARD_COUNT,
            REALISTIC_BENCHMARK_TASK_CHUNK_SIZE,
            [&] {
                if (!counters.start()) {
                    state.SkipWithError(counters.error().c_str());
                    return false;
                }
                return true;
            },
            [&] {
                if (auto values = counters.stop_and_read()) {
                    sample = hardware_counter_sample{(*values)[0], (*values)[1], (*values)[2], (*values)[3]};
                } else {
                    state.SkipWithError(counters.error().c_str());
                }
            });
        if (sample) {
            set_miss_rate_counters(state, *sample, "L1_loads/iter", "L1_load_misses/iter", "L1_miss_rate");
        }
        return;
    }
    case cfr_iteration_hardware_measurement::llc_miss_rate: {
        const auto specs = make_miss_rate_perf_specs(true);
        perf_counter_group counters{specs};
        if (!counters.valid()) {
            state.SkipWithError(counters.error().c_str());
            return;
        }
        run_cfr_iteration_benchmark_impl(
            state,
            graph,
            REALISTIC_BENCHMARK_BOARD_COUNT,
            REALISTIC_BENCHMARK_TASK_CHUNK_SIZE,
            [&] {
                if (!counters.start()) {
                    state.SkipWithError(counters.error().c_str());
                    return false;
                }
                return true;
            },
            [&] {
                if (auto values = counters.stop_and_read()) {
                    sample = hardware_counter_sample{(*values)[0], (*values)[1], (*values)[2], (*values)[3]};
                } else {
                    state.SkipWithError(counters.error().c_str());
                }
            });
        if (sample) {
            set_miss_rate_counters(state, *sample, "cache_references/iter", "cache_misses/iter", "cache_miss_rate");
        }
        return;
    }
    case cfr_iteration_hardware_measurement::memory_bandwidth: {
        const auto specs = make_memory_bandwidth_perf_specs();
        perf_counter_group counters{specs};
        if (!counters.valid()) {
            state.SkipWithError(counters.error().c_str());
            return;
        }
        run_cfr_iteration_benchmark_impl(
            state,
            graph,
            REALISTIC_BENCHMARK_BOARD_COUNT,
            REALISTIC_BENCHMARK_TASK_CHUNK_SIZE,
            [&] {
                if (!counters.start()) {
                    state.SkipWithError(counters.error().c_str());
                    return false;
                }
                return true;
            },
            [&] {
                if (auto values = counters.stop_and_read()) {
                    sample = hardware_counter_sample{
                        0,
                        (*values)[0],
                        (*values)[1],
                        (*values)[2],
                    };
                } else {
                    state.SkipWithError(counters.error().c_str());
                }
            });
        if (sample) {
            const auto bytes = sample->misses * HARDWARE_COUNTER_CACHE_LINE_BYTES;
            state.counters["cache_miss_bytes/s"] = benchmark::Counter(
                static_cast<double>(bytes),
                benchmark::Counter::kIsRate);
            state.counters["cache_misses/iter"] = benchmark::Counter(
                static_cast<double>(sample->misses),
                benchmark::Counter::kAvgIterations);
            set_common_hardware_counters(state, *sample);
        }
        return;
    }
    }
#else
    (void) measurement;
    state.SkipWithError("CFR iteration hardware measurements require Linux perf_event_open");
#endif
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

static void BM_CFRIterationSmall(benchmark::State& state)
{
    run_cfr_iteration_benchmark(state, create_benchmark_tree(2, 2));
}

static void BM_CFRIterationMedium(benchmark::State& state)
{
    run_cfr_iteration_benchmark(state, create_benchmark_tree(4, 4));
}

static void BM_CFRIterationLarge(benchmark::State& state)
{
    run_cfr_iteration_benchmark(state, create_benchmark_tree(3, 6));
}

static void BM_CFRIterationLargeRealistic(benchmark::State& state)
{
    run_cfr_iteration_benchmark(
        state,
        create_benchmark_tree(3, 6),
        REALISTIC_BENCHMARK_BOARD_COUNT,
        REALISTIC_BENCHMARK_TASK_CHUNK_SIZE);
}

#if defined(__linux__)
static void BM_CFRIteration_L1MissRate(benchmark::State& state)
{
    run_cfr_iteration_hardware_benchmark(state, cfr_iteration_hardware_measurement::l1_miss_rate);
}

static void BM_CFRIteration_LLCMissRate(benchmark::State& state)
{
    run_cfr_iteration_hardware_benchmark(state, cfr_iteration_hardware_measurement::llc_miss_rate);
}

static void BM_CFRIteration_MemoryBandwidth(benchmark::State& state)
{
    run_cfr_iteration_hardware_benchmark(state, cfr_iteration_hardware_measurement::memory_bandwidth);
}
#endif

BENCHMARK(BM_CFRIterationSmall)->Arg(1)->Arg(2)->Arg(4)->Arg(8)->Arg(12)->UseRealTime();
BENCHMARK(BM_CFRIterationMedium)->Arg(1)->Arg(2)->Arg(4)->Arg(8)->Arg(12)->UseRealTime();
BENCHMARK(BM_CFRIterationLarge)->Arg(1)->Arg(2)->Arg(4)->Arg(8)->Arg(12)->UseRealTime();
BENCHMARK(BM_CFRIterationLargeRealistic)->Arg(1)->Arg(2)->Arg(4)->Arg(8)->Arg(12)->UseRealTime();
#if defined(__linux__)
BENCHMARK(BM_CFRIteration_L1MissRate)->Arg(1)->Arg(2)->Arg(4)->Arg(8)->Arg(12)->UseRealTime();
BENCHMARK(BM_CFRIteration_LLCMissRate)->Arg(1)->Arg(2)->Arg(4)->Arg(8)->Arg(12)->UseRealTime();
BENCHMARK(BM_CFRIteration_MemoryBandwidth)->Arg(1)->Arg(2)->Arg(4)->Arg(8)->Arg(12)->UseRealTime();
#endif

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
    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);
    std::vector<river_terminal_leaf> leaves(graph.node_count);
    leaves[0] = river_terminal_leaf{river_terminal_leaf_kind::showdown, context};
    leaves[1] = river_terminal_leaf{river_terminal_leaf_kind::fold, context, zeta::holdem::heads_up_player::ip};
    const auto terminal_context = make_river_solver_context(
        deterministic_river_board(),
        std::array<zeta::holdem::reach_vector, 2>{oop_reach, ip_reach},
        std::move(leaves));
    const auto policy = terminal_context.terminal_policy(zeta::holdem::heads_up_player::oop, oop_combo);

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

static void BM_RiverTerminalLeafTraversalCachedReach(benchmark::State& state)
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

    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    const auto [oop_combo, ip_combo] = first_compatible_live_combos(cache);
    zeta::holdem::reach_vector oop_reach{};
    zeta::holdem::reach_vector ip_reach{};
    oop_reach[oop_combo] = 1.0f;
    ip_reach[ip_combo] = 1.0f;
    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);
    std::vector<river_terminal_leaf> leaves(graph.node_count);
    leaves[0] = river_terminal_leaf{river_terminal_leaf_kind::showdown, context};
    leaves[1] = river_terminal_leaf{river_terminal_leaf_kind::fold, context, zeta::holdem::heads_up_player::ip};
    const auto terminal_context = make_river_solver_context(
        deterministic_river_board(),
        std::array<zeta::holdem::reach_vector, 2>{oop_reach, ip_reach},
        std::move(leaves));
    const auto policy = terminal_context.terminal_policy(zeta::holdem::heads_up_player::oop, oop_combo);

    const traversal_frame frame{
        .node_id = 0,
        .next_edge_offset = 0,
        .reach_oop = 1.0f,
        .reach_ip = 1.0f,
        .chance_weight = 1.0f,
        .accumulated_utility = 0.0f,
        .phase = traversal_phase::enter,
        .reserved = {}
    };

    for (auto _ : state) {
        auto value = policy(0, frame);
        benchmark::DoNotOptimize(value);
        benchmark::ClobberMemory();
    }
}

static void BM_RiverTerminalLeafTraversalCachedReachBatch(benchmark::State& state)
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

    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    const auto [oop_combo, ip_combo] = first_compatible_live_combos(cache);
    zeta::holdem::reach_vector oop_reach{};
    zeta::holdem::reach_vector ip_reach{};
    oop_reach[oop_combo] = 1.0f;
    ip_reach[ip_combo] = 1.0f;
    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);
    std::vector<river_terminal_leaf> leaves(graph.node_count);
    leaves[0] = river_terminal_leaf{river_terminal_leaf_kind::showdown, context};
    leaves[1] = river_terminal_leaf{river_terminal_leaf_kind::fold, context, zeta::holdem::heads_up_player::ip};
    const auto terminal_context = make_river_solver_context(
        deterministic_river_board(),
        std::array<zeta::holdem::reach_vector, 2>{oop_reach, ip_reach},
        std::move(leaves));
    const auto policy = terminal_context.terminal_policy(zeta::holdem::heads_up_player::oop, oop_combo);

    const traversal_frame frame{
        .node_id = 0,
        .next_edge_offset = 0,
        .reach_oop = 1.0f,
        .reach_ip = 1.0f,
        .chance_weight = 1.0f,
        .accumulated_utility = 0.0f,
        .phase = traversal_phase::enter,
        .reserved = {}
    };

    for (auto _ : state) {
        float total = 0.0f;
        for (uint32_t leaf = 0; leaf < CACHED_TERMINAL_BATCH_SIZE; ++leaf) {
            const auto node_id = leaf & 1u;
            total += policy(node_id, frame);
        }
        benchmark::DoNotOptimize(total);
        benchmark::ClobberMemory();
    }

    state.counters["terminal_leaves/s"] = benchmark::Counter(
        static_cast<double>(CACHED_TERMINAL_BATCH_SIZE),
        benchmark::Counter::kIsIterationInvariantRate);
}

static void BM_RebuildReachIndex(benchmark::State& state)
{
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    const auto [oop_combo, ip_combo] = first_compatible_live_combos(cache);
    zeta::holdem::reach_vector oop_reach{};
    zeta::holdem::reach_vector ip_reach{};
    oop_reach[oop_combo] = 1.0f;
    ip_reach[ip_combo] = 1.0f;

    for (auto _ : state) {
        auto oop_index = zeta::holdem::make_river_reach_index(cache, oop_reach);
        auto ip_index = zeta::holdem::make_river_reach_index(cache, ip_reach);
        benchmark::DoNotOptimize(oop_index.active_count);
        benchmark::DoNotOptimize(ip_index.active_count);
        benchmark::ClobberMemory();
    }
}

static void BM_FilterDeadCards(benchmark::State& state)
{
    const auto river = deterministic_river_board();

    for (auto _ : state) {
        uint32_t live_count = 0;
        for (zeta::holdem::combination_index combo = 0; combo < zeta::holdem::combination_count; ++combo) {
            live_count += (zeta::holdem::combination_mask(combo) & river.mask) == 0 ? 1u : 0u;
        }
        benchmark::DoNotOptimize(live_count);
    }
}

static void BM_IterateActiveCombos(benchmark::State& state)
{
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    zeta::holdem::reach_vector reach{};
    for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
        reach[cache.rank_order[order]] = 1.0f;
    }
    const auto index = zeta::holdem::make_river_reach_index(cache, reach);

    for (auto _ : state) {
        uint32_t checksum = 0;
        for (uint16_t active = 0; active < index.active_count; ++active) {
            checksum += index.active_indices[active];
        }
        benchmark::DoNotOptimize(checksum);
    }

    state.counters["active_combos"] = static_cast<double>(index.active_count);
}

static void BM_LoadWeights(benchmark::State& state)
{
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    zeta::holdem::reach_vector reach{};
    for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
        reach[cache.rank_order[order]] = 1.0f;
    }
    const auto index = zeta::holdem::make_river_reach_index(cache, reach);

    for (auto _ : state) {
        float total = 0.0f;
        for (zeta::holdem::combination_index combo = 0; combo < zeta::holdem::combination_count; ++combo) {
            total += index.weights[combo];
        }
        benchmark::DoNotOptimize(total);
    }
}

static void BM_CompatibleMass(benchmark::State& state)
{
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    const auto [oop_combo, ip_combo] = first_compatible_live_combos(cache);
    zeta::holdem::reach_vector opponent_reach{};
    opponent_reach[ip_combo] = 1.0f;
    const auto opponent_index = zeta::holdem::make_river_reach_index(cache, opponent_reach);

    for (auto _ : state) {
        auto compatible_mass = zeta::holdem::compatible_reach_mass(cache, opponent_index, oop_combo);
        benchmark::DoNotOptimize(compatible_mass);
    }
}

static void BM_TerminalActiveReachBatchLookup(benchmark::State& state)
{
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    zeta::holdem::reach_vector reach{};
    for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
        reach[cache.rank_order[order]] = 1.0f;
    }
    const auto index = zeta::holdem::make_river_reach_index(cache, reach);

    for (auto _ : state) {
        float total = 0.0f;
        for (uint16_t active = 0; active < index.active_count; ++active) {
            const auto combo = index.active_indices[active];
            total += index.weights[combo];
        }
        benchmark::DoNotOptimize(total);
    }

    state.counters["active_combos"] = static_cast<double>(index.active_count);
}

static void BM_TerminalShowdown(benchmark::State& state)
{
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
    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);

    for (auto _ : state) {
        auto values = zeta::holdem::evaluate_showdown_values(cache, reach_indices[0], reach_indices[1], context);
        benchmark::DoNotOptimize(values[zeta::holdem::heads_up_player::oop][oop_combo]);
        benchmark::ClobberMemory();
    }
}

static void BM_TerminalFold(benchmark::State& state)
{
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
    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);

    for (auto _ : state) {
        auto values = zeta::holdem::evaluate_fold_values(
            cache,
            reach_indices[0],
            reach_indices[1],
            context,
            zeta::holdem::heads_up_player::ip);
        benchmark::DoNotOptimize(values[zeta::holdem::heads_up_player::oop][oop_combo]);
        benchmark::ClobberMemory();
    }
}

static void BM_TerminalAccumulate(benchmark::State& state)
{
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
    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);
    const auto values = zeta::holdem::evaluate_showdown_values(cache, reach_indices[0], reach_indices[1], context);

    for (auto _ : state) {
        float total = 0.0f;
        for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
            const auto combo = cache.rank_order[order];
            total += values[zeta::holdem::heads_up_player::oop][combo] * oop_reach[combo];
        }
        benchmark::DoNotOptimize(total);
        benchmark::ClobberMemory();
    }
}

static void BM_TerminalFusedLoadAccumulate(benchmark::State& state)
{
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    zeta::holdem::reach_vector reach{};
    for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
        reach[cache.rank_order[order]] = 1.0f;
    }
    const auto index = zeta::holdem::make_river_reach_index(cache, reach);
    const std::array<zeta::holdem::river_reach_index, 2> reach_indices{index, index};
    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);
    const auto values = zeta::holdem::evaluate_showdown_values(cache, reach_indices[0], reach_indices[1], context);

    for (auto _ : state) {
        float total = 0.0f;
        for (uint16_t active = 0; active < index.active_count; ++active) {
            const auto combo = index.active_indices[active];
            total += index.weights[combo] * values[zeta::holdem::heads_up_player::oop][combo];
        }
        benchmark::DoNotOptimize(total);
        benchmark::ClobberMemory();
    }

    state.counters["active_combos"] = static_cast<double>(index.active_count);
}

static void BM_TerminalAccumulate_NoLookup(benchmark::State& state)
{
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    zeta::holdem::reach_vector reach{};
    for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
        reach[cache.rank_order[order]] = 1.0f;
    }
    const auto index = zeta::holdem::make_river_reach_index(cache, reach);
    const std::array<zeta::holdem::river_reach_index, 2> reach_indices{index, index};
    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);
    const auto values = zeta::holdem::evaluate_showdown_values(cache, reach_indices[0], reach_indices[1], context);

    std::vector<terminal_combo_work_item> work_items;
    work_items.reserve(index.active_count);
    for (uint16_t active = 0; active < index.active_count; ++active) {
        const auto combo = index.active_indices[active];
        work_items.push_back(terminal_combo_work_item{
            .combo = combo,
            .reach = index.weights[combo],
            .payoff_index = combo
        });
    }

    for (auto _ : state) {
        float total = 0.0f;
        for (const auto& item : work_items) {
            total += item.reach * values[zeta::holdem::heads_up_player::oop][item.payoff_index];
        }
        benchmark::DoNotOptimize(total);
        benchmark::ClobberMemory();
    }

    state.counters["active_combos"] = static_cast<double>(work_items.size());
}

static void BM_TerminalAccumulate_WithLookup(benchmark::State& state)
{
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    zeta::holdem::reach_vector reach{};
    for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
        reach[cache.rank_order[order]] = 1.0f;
    }
    const auto index = zeta::holdem::make_river_reach_index(cache, reach);
    const std::array<zeta::holdem::river_reach_index, 2> reach_indices{index, index};
    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);
    const auto values = zeta::holdem::evaluate_showdown_values(cache, reach_indices[0], reach_indices[1], context);

    for (auto _ : state) {
        float total = 0.0f;
        for (uint16_t active = 0; active < index.active_count; ++active) {
            const auto combo = index.active_indices[active];
            total += index.weights[combo] * values[zeta::holdem::heads_up_player::oop][combo];
        }
        benchmark::DoNotOptimize(total);
        benchmark::ClobberMemory();
    }

    state.counters["active_combos"] = static_cast<double>(index.active_count);
}

static void BM_TerminalAccumulate_WithPrefetchedReach(benchmark::State& state)
{
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    zeta::holdem::reach_vector reach{};
    for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
        reach[cache.rank_order[order]] = 1.0f;
    }
    const auto index = zeta::holdem::make_river_reach_index(cache, reach);
    const std::array<zeta::holdem::river_reach_index, 2> reach_indices{index, index};
    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);
    const auto values = zeta::holdem::evaluate_showdown_values(cache, reach_indices[0], reach_indices[1], context);

    std::vector<float> active_reach;
    std::vector<zeta::holdem::combination_index> active_combos;
    active_reach.reserve(index.active_count);
    active_combos.reserve(index.active_count);
    for (uint16_t active = 0; active < index.active_count; ++active) {
        const auto combo = index.active_indices[active];
        active_combos.push_back(combo);
        active_reach.push_back(index.weights[combo]);
    }

    for (auto _ : state) {
        float total = 0.0f;
        for (std::size_t active = 0; active < active_combos.size(); ++active) {
            total += active_reach[active] * values[zeta::holdem::heads_up_player::oop][active_combos[active]];
        }
        benchmark::DoNotOptimize(total);
        benchmark::ClobberMemory();
    }

    state.counters["active_combos"] = static_cast<double>(active_combos.size());
}

static void BM_TerminalAccumulate_WithStackLocalReach(benchmark::State& state)
{
    const auto cache = zeta::holdem::make_river_terminal_cache(deterministic_river_board());
    zeta::holdem::reach_vector reach{};
    for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
        reach[cache.rank_order[order]] = 1.0f;
    }
    const auto index = zeta::holdem::make_river_reach_index(cache, reach);
    const std::array<zeta::holdem::river_reach_index, 2> reach_indices{index, index};
    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);
    const auto values = zeta::holdem::evaluate_showdown_values(cache, reach_indices[0], reach_indices[1], context);

    std::array<float, zeta::holdem::river_live_combination_count> active_reach{};
    std::array<zeta::holdem::combination_index, zeta::holdem::river_live_combination_count> active_combos{};
    for (uint16_t active = 0; active < index.active_count; ++active) {
        const auto combo = index.active_indices[active];
        active_combos[active] = combo;
        active_reach[active] = index.weights[combo];
    }

    for (auto _ : state) {
        float total = 0.0f;
        for (uint16_t active = 0; active < index.active_count; ++active) {
            total += active_reach[active] * values[zeta::holdem::heads_up_player::oop][active_combos[active]];
        }
        benchmark::DoNotOptimize(total);
        benchmark::ClobberMemory();
    }

    state.counters["active_combos"] = static_cast<double>(index.active_count);
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

static void BM_RegretMatching(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    std::vector<float> strategy(layout.value_count(), 0.0f);

    for (uint32_t infoset_id = 0; infoset_id < regrets.infoset_count(); ++infoset_id) {
        for (uint32_t action = 0; action < regrets.action_count(infoset_id); ++action) {
            regrets.value(infoset_id, action) = action % 2u == 0u
                ? static_cast<float>(action + 1u)
                : -static_cast<float>(action + 1u);
        }
    }

    for (auto _ : state) {
        for (uint32_t infoset_id = 0; infoset_id < regrets.infoset_count(); ++infoset_id) {
            const auto regrets_span = regrets.infoset_regrets(infoset_id);
            const auto begin = regrets.action_offsets[infoset_id];
            float positive_sum = 0.0f;
            for (const auto regret : regrets_span) {
                positive_sum += std::max(regret, 0.0f);
            }

            const auto action_count = static_cast<uint32_t>(regrets_span.size());
            const auto uniform = action_count == 0u ? 0.0f : 1.0f / static_cast<float>(action_count);
            for (uint32_t action = 0; action < action_count; ++action) {
                strategy[begin + action] = positive_sum > 0.0f
                    ? std::max(regrets_span[action], 0.0f) / positive_sum
                    : uniform;
            }
        }
        benchmark::DoNotOptimize(strategy.data());
        benchmark::ClobberMemory();
    }

    state.counters["infosets/s"] = benchmark::Counter(
        static_cast<double>(regrets.infoset_count()),
        benchmark::Counter::kIsIterationInvariantRate);
    state.counters["actions/s"] = benchmark::Counter(
        static_cast<double>(regrets.value_count()),
        benchmark::Counter::kIsIterationInvariantRate);
}

static void BM_RegretUpdate(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    std::vector<float> deltas(layout.value_count(), 0.125f);

    for (auto _ : state) {
        for (uint32_t value = 0; value < regrets.value_count(); ++value) {
            regrets.regrets[value] = std::max(regrets.regrets[value] + deltas[value], 0.0f);
        }
        benchmark::DoNotOptimize(regrets.regrets.data());
        benchmark::ClobberMemory();
    }

    state.counters["regret_updates/s"] = benchmark::Counter(
        static_cast<double>(regrets.value_count()),
        benchmark::Counter::kIsIterationInvariantRate);
}

static void BM_StrategyAverage(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    auto layout = require_layout(make_action_table_layout(graph));
    strategy_sum_table strategy_sums(layout);
    std::vector<float> average(layout.value_count(), 0.0f);

    for (uint32_t value = 0; value < strategy_sums.value_count(); ++value) {
        strategy_sums.sums[value] = static_cast<float>((value % 7u) + 1u);
    }

    for (auto _ : state) {
        for (uint32_t infoset_id = 0; infoset_id < strategy_sums.infoset_count(); ++infoset_id) {
            const auto sums = strategy_sums.infoset_sums(infoset_id);
            const auto begin = strategy_sums.action_offsets[infoset_id];
            float total = 0.0f;
            for (const auto value : sums) {
                total += value;
            }
            const auto scale = total > 0.0f ? 1.0f / total : 0.0f;
            for (uint32_t action = 0; action < sums.size(); ++action) {
                average[begin + action] = sums[action] * scale;
            }
        }
        benchmark::DoNotOptimize(average.data());
        benchmark::ClobberMemory();
    }

    state.counters["strategy_values/s"] = benchmark::Counter(
        static_cast<double>(strategy_sums.value_count()),
        benchmark::Counter::kIsIterationInvariantRate);
}

static void BM_CFRIterationWithUpdates(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    std::vector<float> strategy(layout.value_count(), 0.0f);

    for (auto _ : state) {
        for (uint32_t infoset_id = 0; infoset_id < layout.infoset_count(); ++infoset_id) {
            auto regrets_span = regrets.infoset_regrets(infoset_id);
            auto strategy_span = strategy_sums.infoset_sums(infoset_id);
            const auto begin = layout.action_offsets[infoset_id];

            float positive_sum = 0.0f;
            for (const auto regret : regrets_span) {
                positive_sum += std::max(regret, 0.0f);
            }

            const auto action_count = static_cast<uint32_t>(regrets_span.size());
            const auto uniform = action_count == 0u ? 0.0f : 1.0f / static_cast<float>(action_count);
            for (uint32_t action = 0; action < action_count; ++action) {
                const auto probability = positive_sum > 0.0f
                    ? std::max(regrets_span[action], 0.0f) / positive_sum
                    : uniform;
                strategy[begin + action] = probability;
                strategy_span[action] += probability;
                const auto action_delta = static_cast<float>(action + 1u) * 0.01f;
                regrets_span[action] = std::max(regrets_span[action] + action_delta - probability, 0.0f);
            }
        }
        benchmark::DoNotOptimize(regrets.regrets.data());
        benchmark::DoNotOptimize(strategy_sums.sums.data());
        benchmark::ClobberMemory();
    }

    state.counters["infosets/s"] = benchmark::Counter(
        static_cast<double>(layout.infoset_count()),
        benchmark::Counter::kIsIterationInvariantRate);
    state.counters["regret_updates/s"] = benchmark::Counter(
        static_cast<double>(layout.value_count()),
        benchmark::Counter::kIsIterationInvariantRate);
    state.counters["strategy_updates/s"] = benchmark::Counter(
        static_cast<double>(layout.value_count()),
        benchmark::Counter::kIsIterationInvariantRate);
}

static void BM_CFRUpdateSeparateStorage(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    auto layout = require_layout(make_action_table_layout(graph));
    regret_table regrets(layout);
    strategy_sum_table strategy_sums(layout);
    std::vector<float> deltas(layout.value_count(), 0.125f);
    std::vector<float> probabilities(layout.value_count(), 0.25f);

    for (auto _ : state) {
        for (uint32_t value = 0; value < layout.value_count(); ++value) {
            regrets.regrets[value] = std::max(regrets.regrets[value] + deltas[value], 0.0f);
        }
        for (uint32_t value = 0; value < layout.value_count(); ++value) {
            strategy_sums.sums[value] += probabilities[value];
        }
        benchmark::DoNotOptimize(regrets.regrets.data());
        benchmark::DoNotOptimize(strategy_sums.sums.data());
        benchmark::ClobberMemory();
    }

    state.counters["values/s"] = benchmark::Counter(
        static_cast<double>(layout.value_count()),
        benchmark::Counter::kIsIterationInvariantRate);
}

static void BM_CFRUpdateInterleavedStorage(benchmark::State& state)
{
    auto graph = create_benchmark_tree(4, 4);
    auto layout = require_layout(make_action_table_layout(graph));
    std::vector<regret_strategy_value> values(layout.value_count());
    std::vector<float> deltas(layout.value_count(), 0.125f);
    std::vector<float> probabilities(layout.value_count(), 0.25f);

    for (auto _ : state) {
        for (uint32_t value = 0; value < layout.value_count(); ++value) {
            auto& entry = values[value];
            entry.regret = std::max(entry.regret + deltas[value], 0.0f);
            entry.strategy_sum += probabilities[value];
        }
        benchmark::DoNotOptimize(values.data());
        benchmark::ClobberMemory();
    }

    state.counters["values/s"] = benchmark::Counter(
        static_cast<double>(layout.value_count()),
        benchmark::Counter::kIsIterationInvariantRate);
}

BENCHMARK(BM_RiverTerminalLeafTraversal);
BENCHMARK(BM_RiverTerminalLeafTraversalCachedReach);
BENCHMARK(BM_RiverTerminalLeafTraversalCachedReachBatch);
BENCHMARK(BM_RebuildReachIndex);
BENCHMARK(BM_FilterDeadCards);
BENCHMARK(BM_IterateActiveCombos);
BENCHMARK(BM_LoadWeights);
BENCHMARK(BM_CompatibleMass);
BENCHMARK(BM_TerminalActiveReachBatchLookup);
BENCHMARK(BM_TerminalShowdown);
BENCHMARK(BM_TerminalFold);
BENCHMARK(BM_TerminalAccumulate);
BENCHMARK(BM_TerminalFusedLoadAccumulate);
BENCHMARK(BM_TerminalAccumulate_NoLookup);
BENCHMARK(BM_TerminalAccumulate_WithLookup);
BENCHMARK(BM_TerminalAccumulate_WithPrefetchedReach);
BENCHMARK(BM_TerminalAccumulate_WithStackLocalReach);
BENCHMARK(BM_DeterministicWorkerReduction)->Arg(2)->Arg(4)->Arg(8)->Arg(12);
BENCHMARK(BM_RegretMatching);
BENCHMARK(BM_RegretUpdate);
BENCHMARK(BM_StrategyAverage);
BENCHMARK(BM_CFRIterationWithUpdates);
BENCHMARK(BM_CFRUpdateSeparateStorage);
BENCHMARK(BM_CFRUpdateInterleavedStorage);

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
