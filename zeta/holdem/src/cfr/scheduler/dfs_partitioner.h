#pragma once

#include "cfr/graph/graph.h"
#include <cstdint>
#include <expected>
#include <limits>
#include <ostream>
#include <span>
#include <vector>

namespace zeta::holdem::cfr::scheduler {

    inline constexpr uint32_t MAX_REPRESENTABLE_WORK_DEPTH_SHIFT =
        std::numeric_limits<uint64_t>::digits - 1u;

    struct graph_partition {
        uint32_t begin_node;        /**< Inclusive: first node in DFS post-order for this partition. */
        uint32_t end_node;          /**< Exclusive: last node + 1 in DFS post-order for this partition. */
        uint32_t node_count;        /**< Number of nodes in partition. */
        uint32_t terminal_count;    /**< Terminal nodes in partition. */
        uint32_t action_count;      /**< Total action count in partition. */
        uint16_t min_depth;
        uint16_t max_depth;
        uint64_t estimated_work;    /**< Heuristic cost metric for scheduling. */
    };

    enum class dfs_partitioner_error_kind : uint8_t {
        invalid_partition_count,
        invalid_work_depth_shift,
        invalid_partitions
    };

    struct dfs_partitioner_error {
        dfs_partitioner_error_kind kind{};
        uint32_t node_id = 0;
        uint32_t related_node_id = 0;
    };

    [[nodiscard]] constexpr const char* to_string(const dfs_partitioner_error_kind kind) noexcept
    {
        using enum dfs_partitioner_error_kind;
        switch (kind) {
            case invalid_partition_count:   return "dfs_partitioner_error_kind::invalid_partition_count";
            case invalid_work_depth_shift:  return "dfs_partitioner_error_kind::invalid_work_depth_shift";
            case invalid_partitions:        return "dfs_partitioner_error_kind::invalid_partitions";
        }
        return "dfs_partitioner_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const dfs_partitioner_error_kind kind)
    {
        return os << to_string(kind);
    }

    /**
     * Scheduling policy for dividing a graph into contiguous DFS-order work ranges.
     *
     * Both knobs are intentionally explicit. There is no universal correct default:
     * callers should pass the worker count and depth weighting cap they intend to
     * schedule with.
     */
    struct dfs_partition_strategy {
        explicit constexpr dfs_partition_strategy(
            const uint32_t partition_count,
            const uint32_t work_depth_shift_cap) noexcept :
            target_partition_count(partition_count),
            max_work_depth_shift(work_depth_shift_cap)
        {
        }

        uint32_t target_partition_count;

        /** Caps the 2^depth multiplier used by the provisional work heuristic. */
        uint32_t max_work_depth_shift;
    };

    /**
     * Compute partitions for a game graph using the given strategy.
     * Partitions preserve DFS order, but the greedy work balancer can split subtrees.
     */
    [[nodiscard]] std::expected<std::vector<graph_partition>, dfs_partitioner_error> compute_dfs_partitions(
        const game_graph& graph,
        dfs_partition_strategy strategy);

    /**
     * Validate partition coverage and metadata against a graph.
     */
    [[nodiscard]] std::expected<void, dfs_partitioner_error> validate_dfs_partitions(
        const game_graph& graph,
        std::span<const graph_partition> partitions,
        dfs_partition_strategy strategy) noexcept;

    /**
     * Coefficient of variation (stddev/mean) of estimated_work. Lower is better.
     */
    [[nodiscard]] double dfs_partition_balance_metric(std::span<const graph_partition> partitions) noexcept;

}
