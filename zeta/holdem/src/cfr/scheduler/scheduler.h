#pragma once

#include "cfr/scheduler/dfs_partitioner.h"

#include <algorithm>
#include <atomic>
#include <cassert>
#include <cstdint>
#include <expected>
#include <functional>
#include <limits>
#include <mutex>
#include <ostream>
#include <span>
#include <thread>
#include <type_traits>
#include <vector>

namespace zeta::holdem::cfr::scheduler {

    enum class scheduler_error_kind : uint8_t {
        invalid_board_count,
        empty_partition_plan,
        invalid_worker_count,
        task_failed
    };

    struct scheduler_error {
        scheduler_error_kind kind{};
        uint32_t worker_id = 0;
        uint64_t task_index = 0;
        uint32_t board_index = 0;
        uint32_t partition_index = 0;
    };

    [[nodiscard]] constexpr const char* to_string(const scheduler_error_kind kind) noexcept
    {
        using enum scheduler_error_kind;
        switch (kind) {
            case invalid_board_count:   return "scheduler_error_kind::invalid_board_count";
            case empty_partition_plan:  return "scheduler_error_kind::empty_partition_plan";
            case invalid_worker_count:  return "scheduler_error_kind::invalid_worker_count";
            case task_failed:           return "scheduler_error_kind::task_failed";
        }
        return "scheduler_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const scheduler_error_kind kind)
    {
        return os << to_string(kind);
    }

    struct board_partition_task {
        uint64_t task_index = 0;                         /**< Deterministic index in board-major order. */
        uint32_t board_index = 0;                        /**< River board/cache index supplied by the solver. */
        uint32_t partition_index = 0;                    /**< Index into board_partition_plan::partitions. */
        const graph_partition* partition = nullptr;      /**< Immutable graph partition metadata for this task. */
    };

    /**
     * Immutable task plan pairing every board with every graph partition.
     *
     * The runtime consumes this compact plan by deriving task descriptors from
     * task indices. This keeps queue entries small and leaves board/cache storage
     * ownership with the caller.
     */
    struct board_partition_plan {
        uint32_t board_count = 0;
        std::vector<graph_partition> partitions;

        [[nodiscard]] uint64_t task_count() const noexcept
        {
            return static_cast<uint64_t>(board_count) * static_cast<uint64_t>(partitions.size());
        }

        [[nodiscard]] uint64_t estimated_work() const noexcept
        {
            uint64_t partition_work = 0;
            for (const auto& partition : partitions) {
                partition_work += partition.estimated_work;
            }
            return partition_work * static_cast<uint64_t>(board_count);
        }

        [[nodiscard]] board_partition_task task_at(const uint64_t task_index) const noexcept
        {
            assert(!partitions.empty());
            assert(task_index < task_count());

            const auto partition_count = static_cast<uint64_t>(partitions.size());
            const auto partition_index = static_cast<uint32_t>(task_index % partition_count);
            const auto board_index = static_cast<uint32_t>(task_index / partition_count);

            return board_partition_task{
                .task_index = task_index,
                .board_index = board_index,
                .partition_index = partition_index,
                .partition = &partitions[partition_index]
            };
        }
    };

    [[nodiscard]] inline std::expected<board_partition_plan, scheduler_error> make_board_partition_plan(
        const uint32_t board_count,
        const std::span<const graph_partition> partitions)
    {
        if (board_count == 0u) {
            return std::unexpected(scheduler_error{scheduler_error_kind::invalid_board_count});
        }
        if (partitions.empty()) {
            return std::unexpected(scheduler_error{scheduler_error_kind::empty_partition_plan});
        }

        board_partition_plan plan;
        plan.board_count = board_count;
        plan.partitions.assign(partitions.begin(), partitions.end());
        return plan;
    }

    struct scheduler_runtime_config {
        uint32_t worker_count = 1;       /**< Number of OS worker threads to launch. */
        uint32_t task_chunk_size = 1;    /**< Number of contiguous tasks claimed per queue operation. */
    };

    struct alignas(64) scheduler_worker_state {
        uint32_t worker_id = 0;
        uint64_t tasks_executed = 0;
        uint64_t estimated_work = 0;
    };

    struct scheduler_run_summary {
        std::vector<scheduler_worker_state> workers;
        uint64_t tasks_executed = 0;
        uint64_t estimated_work = 0;
    };

    namespace detail {

        template <typename TaskCallback>
        using scheduler_callback_result_t = std::invoke_result_t<
            TaskCallback&,
            const scheduler_worker_state&,
            const board_partition_task&>;

        template <typename TaskCallback>
        [[nodiscard]] std::expected<void, scheduler_error> invoke_scheduler_task(
            TaskCallback& task_callback,
            const scheduler_worker_state& worker,
            const board_partition_task& task)
        {
            using result_type = scheduler_callback_result_t<TaskCallback>;
            if constexpr (std::is_void_v<result_type>) {
                std::invoke(task_callback, worker, task);
                return {};
            } else {
                static_assert(
                    std::is_same_v<result_type, std::expected<void, scheduler_error>>,
                    "Scheduler callbacks must return void or std::expected<void, scheduler_error>.");
                return std::invoke(task_callback, worker, task);
            }
        }

        [[nodiscard]] inline scheduler_error contextualize_error(
            scheduler_error error,
            const scheduler_worker_state& worker,
            const board_partition_task& task) noexcept
        {
            error.worker_id = worker.worker_id;
            error.task_index = task.task_index;
            error.board_index = task.board_index;
            error.partition_index = task.partition_index;
            return error;
        }
    }

    /**
     * Execute board/partition tasks with a dynamic atomic queue.
     *
     * Each task callback receives the stable worker id and immutable task
     * descriptor. Mutable traversal, terminal, and delta-buffer state remains
     * caller-owned and should be indexed by worker_id.
     */
    template <typename TaskCallback>
    [[nodiscard]] std::expected<scheduler_run_summary, scheduler_error> run_board_partition_scheduler(
        const board_partition_plan& plan,
        scheduler_runtime_config config,
        TaskCallback&& task_callback)
    {
        if (plan.board_count == 0u) {
            return std::unexpected(scheduler_error{scheduler_error_kind::invalid_board_count});
        }
        if (plan.partitions.empty()) {
            return std::unexpected(scheduler_error{scheduler_error_kind::empty_partition_plan});
        }
        if (config.worker_count == 0u) {
            return std::unexpected(scheduler_error{scheduler_error_kind::invalid_worker_count});
        }

        const auto total_tasks = plan.task_count();
        const auto task_chunk_size = std::max<uint32_t>(config.task_chunk_size, 1u);
        const auto active_worker_count = std::min<uint32_t>(
            config.worker_count,
            static_cast<uint32_t>(std::min<uint64_t>(total_tasks, std::numeric_limits<uint32_t>::max())));

        scheduler_run_summary summary;
        summary.workers.resize(active_worker_count);
        for (uint32_t worker_id = 0; worker_id < active_worker_count; ++worker_id) {
            summary.workers[worker_id].worker_id = worker_id;
        }

        std::atomic<uint64_t> next_task{0};
        std::atomic<bool> stop_requested{false};
        std::mutex error_mutex;
        std::expected<void, scheduler_error> first_error{};

        auto worker_main = [&](const uint32_t worker_id) {
            auto& worker = summary.workers[worker_id];
            while (!stop_requested.load(std::memory_order_acquire)) {
                const auto chunk_begin = next_task.fetch_add(task_chunk_size, std::memory_order_relaxed);
                if (chunk_begin >= total_tasks) {
                    break;
                }

                const auto chunk_end = std::min<uint64_t>(chunk_begin + task_chunk_size, total_tasks);
                for (uint64_t task_index = chunk_begin; task_index < chunk_end; ++task_index) {
                    if (stop_requested.load(std::memory_order_acquire)) {
                        break;
                    }

                    const auto task = plan.task_at(task_index);
                    if (auto result = detail::invoke_scheduler_task(task_callback, worker, task); !result) {
                        const std::lock_guard lock{error_mutex};
                        if (first_error.has_value()) {
                            first_error = std::unexpected(detail::contextualize_error(result.error(), worker, task));
                        }
                        stop_requested.store(true, std::memory_order_release);
                        break;
                    }

                    ++worker.tasks_executed;
                    worker.estimated_work += task.partition->estimated_work;
                }
            }
        };

        std::vector<std::thread> threads;
        threads.reserve(active_worker_count);
        for (uint32_t worker_id = 0; worker_id < active_worker_count; ++worker_id) {
            threads.emplace_back(worker_main, worker_id);
        }
        for (auto& thread : threads) {
            thread.join();
        }

        if (!first_error) {
            return std::unexpected(first_error.error());
        }

        for (const auto& worker : summary.workers) {
            summary.tasks_executed += worker.tasks_executed;
            summary.estimated_work += worker.estimated_work;
        }

        return summary;
    }

}
