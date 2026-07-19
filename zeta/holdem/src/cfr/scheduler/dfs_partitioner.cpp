#include "dfs_partitioner.h"
#include <algorithm>
#include <cmath>
#include <limits>

namespace zeta::holdem::cfr::scheduler {

    namespace {

        [[nodiscard]] std::unexpected<dfs_partitioner_error> partition_failure(
            const dfs_partitioner_error_kind kind,
            const uint32_t node_id = 0,
            const uint32_t related_node_id = 0) noexcept
        {
            return std::unexpected(dfs_partitioner_error{kind, node_id, related_node_id});
        }

        [[nodiscard]] uint64_t saturating_add(const uint64_t lhs, const uint64_t rhs) noexcept
        {
            constexpr auto max = std::numeric_limits<uint64_t>::max();
            return lhs > max - rhs ? max : lhs + rhs;
        }

        [[nodiscard]] uint64_t saturating_multiply(const uint64_t lhs, const uint64_t rhs) noexcept
        {
            constexpr auto max = std::numeric_limits<uint64_t>::max();
            if (lhs == 0 || rhs == 0) {
                return 0;
            }
            return lhs > max / rhs ? max : lhs * rhs;
        }

        [[nodiscard]] uint64_t ceil_divide(const uint64_t value, const uint64_t divisor) noexcept
        {
            return value / divisor + (value % divisor == 0 ? 0u : 1u);
        }

        [[nodiscard]] uint64_t estimate_node_work(
            const game_graph& graph,
            const uint32_t node_id,
            const uint32_t max_work_depth_shift) noexcept
        {
            const auto actions = static_cast<uint64_t>(graph.action_count(node_id));
            const auto depth = std::min<uint32_t>(graph.node_depth[node_id], max_work_depth_shift);
            const auto depth_factor = uint64_t{1} << depth;
            return saturating_multiply(actions, depth_factor);
        }

        [[nodiscard]] graph_partition compute_partition_metadata(
            const game_graph& graph,
            const uint32_t begin_node,
            const uint32_t end_node,
            const uint32_t max_work_depth_shift,
            const std::span<const uint64_t> node_work = {})
        {
            graph_partition p{};
            p.begin_node = begin_node;
            p.end_node = end_node;
            p.node_count = end_node - begin_node;
            p.terminal_count = 0;
            p.action_count = 0;
            p.min_depth = std::numeric_limits<uint16_t>::max();
            p.max_depth = 0;
            p.estimated_work = 0;

            for (uint32_t node_id = begin_node; node_id < end_node; ++node_id) {
                if (graph.is_terminal(node_id)) {
                    p.terminal_count++;
                }

                const auto num_actions = graph.action_count(node_id);
                p.action_count += num_actions;
                p.estimated_work = saturating_add(
                    p.estimated_work,
                    node_work.empty()
                        ? estimate_node_work(graph, node_id, max_work_depth_shift)
                        : node_work[node_id]);

                p.min_depth = std::min(p.min_depth, static_cast<uint16_t>(graph.node_depth[node_id]));
                p.max_depth = std::max(p.max_depth, static_cast<uint16_t>(graph.node_depth[node_id]));
            }

            if (p.min_depth == std::numeric_limits<uint16_t>::max()) {
                p.min_depth = 0;
            }

            return p;
        }

        [[nodiscard]] std::vector<graph_partition> compute_even_node_partitions(
            const game_graph& graph,
            const uint32_t target_count,
            const uint32_t max_work_depth_shift)
        {
            std::vector<graph_partition> partitions;
            partitions.reserve(target_count);

            const auto base_size = graph.node_count / target_count;
            const auto remainder = graph.node_count % target_count;

            uint32_t begin_node = 0;
            for (uint32_t partition_id = 0; partition_id < target_count; ++partition_id) {
                const auto size = base_size + (partition_id < remainder ? 1u : 0u);
                const auto end_node = begin_node + size;
                partitions.push_back(
                    compute_partition_metadata(graph, begin_node, end_node, max_work_depth_shift));
                begin_node = end_node;
            }

            return partitions;
        }
    }

    std::expected<std::vector<graph_partition>, dfs_partitioner_error> compute_dfs_partitions(
        const game_graph& graph,
        const dfs_partition_strategy strategy)
    {
        if (strategy.target_partition_count == 0) {
            return partition_failure(dfs_partitioner_error_kind::invalid_partition_count);
        }
        if (strategy.max_work_depth_shift > MAX_REPRESENTABLE_WORK_DEPTH_SHIFT) {
            return partition_failure(dfs_partitioner_error_kind::invalid_work_depth_shift);
        }

        std::vector<graph_partition> partitions;

        if (graph.node_count == 0) {
            return partitions;
        }

        const auto target_count = std::min(strategy.target_partition_count, graph.node_count);
        if (target_count == 1) {
            partitions.push_back(
                compute_partition_metadata(graph, 0, graph.node_count, strategy.max_work_depth_shift));
            return partitions;
        }

        std::vector<uint64_t> node_work(graph.node_count);
        uint64_t total_work = 0;
        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            node_work[node_id] = estimate_node_work(graph, node_id, strategy.max_work_depth_shift);
            total_work = saturating_add(total_work, node_work[node_id]);
        }

        if (total_work == 0) {
            return compute_even_node_partitions(graph, target_count, strategy.max_work_depth_shift);
        }

        const auto target_work = ceil_divide(total_work, target_count);

        uint32_t partition_start = 0;
        uint64_t current_work = 0;

        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            current_work = saturating_add(current_work, node_work[node_id]);

            if (const bool last_node = (node_id == graph.node_count - 1);
                current_work >= target_work || last_node) {
                partitions.push_back(
                    compute_partition_metadata(
                        graph,
                        partition_start,
                        node_id + 1,
                        strategy.max_work_depth_shift,
                        node_work));
                partition_start = node_id + 1;
                current_work = 0;
            }
        }

        return partitions;
    }

    std::expected<void, dfs_partitioner_error> validate_dfs_partitions(
        const game_graph& graph,
        const std::span<const graph_partition> partitions,
        const dfs_partition_strategy strategy) noexcept
    {
        if (strategy.target_partition_count == 0) {
            return partition_failure(dfs_partitioner_error_kind::invalid_partition_count);
        }
        if (strategy.max_work_depth_shift > MAX_REPRESENTABLE_WORK_DEPTH_SHIFT) {
            return partition_failure(dfs_partitioner_error_kind::invalid_work_depth_shift);
        }

        if (partitions.empty()) {
            if (graph.node_count == 0) {
                return {};
            }
            return partition_failure(dfs_partitioner_error_kind::invalid_partitions);
        }

        uint32_t prev_end = 0;
        for (const auto& p : partitions) {
            if (p.begin_node != prev_end || p.end_node <= p.begin_node || p.end_node > graph.node_count) {
                return partition_failure(dfs_partitioner_error_kind::invalid_partitions, p.begin_node, p.end_node);
            }

            uint32_t terminal_count = 0;
            uint32_t action_count = 0;
            uint64_t estimated_work = 0;
            auto min_depth = std::numeric_limits<uint16_t>::max();
            uint16_t max_depth = 0;

            for (uint32_t node_id = p.begin_node; node_id < p.end_node; ++node_id) {
                if (graph.is_terminal(node_id)) {
                    ++terminal_count;
                }
                action_count += graph.action_count(node_id);
                estimated_work = saturating_add(
                    estimated_work,
                    estimate_node_work(graph, node_id, strategy.max_work_depth_shift));
                min_depth = std::min(min_depth, static_cast<uint16_t>(graph.node_depth[node_id]));
                max_depth = std::max(max_depth, static_cast<uint16_t>(graph.node_depth[node_id]));
            }

            if (p.node_count != p.end_node - p.begin_node
                || p.terminal_count != terminal_count
                || p.action_count != action_count
                || p.estimated_work != estimated_work
                || p.min_depth != min_depth
                || p.max_depth != max_depth) {
                return partition_failure(dfs_partitioner_error_kind::invalid_partitions, p.begin_node, p.end_node);
            }

            prev_end = p.end_node;
        }

        if (prev_end != graph.node_count) {
            return partition_failure(dfs_partitioner_error_kind::invalid_partitions, prev_end);
        }

        return {};
    }

    double dfs_partition_balance_metric(const std::span<const graph_partition> partitions) noexcept
    {
        if (partitions.empty()) {
            return 0.0;
        }

        const auto n = static_cast<double>(partitions.size());

        double sum = 0.0;
        for (const auto& p : partitions) {
            sum += static_cast<double>(p.estimated_work);
        }
        const auto mean = sum / n;

        if (mean == 0.0) {
            return 0.0;
        }

        double sum_sq_dev = 0.0;
        for (const auto& p : partitions) {
            const auto dev = static_cast<double>(p.estimated_work) - mean;
            sum_sq_dev += dev * dev;
        }
        const auto stddev = std::sqrt(sum_sq_dev / n);

        return stddev / mean;
    }

}
