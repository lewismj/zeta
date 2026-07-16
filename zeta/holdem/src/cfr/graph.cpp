#include "graph.h"
#include <cmath>
#include <numeric>
#include <algorithm>
#include <limits>

namespace zeta::holdem::cfr {

    namespace {

        [[nodiscard]] std::unexpected<graph_build_error> validation_error(
            const graph_build_error_kind kind,
            const uint32_t node_id = 0,
            const uint32_t related_node_id = 0) noexcept
        {
            return std::unexpected(graph_build_error{kind, node_id, related_node_id});
        }

        [[nodiscard]] uint64_t estimate_node_work(const game_graph& graph, const uint32_t node_id) noexcept
        {
            const auto actions = graph.action_count(node_id);
            const auto depth = std::min<uint32_t>(graph.node_depth[node_id], 16u);
            const auto depth_factor = uint64_t{1} << depth;
            return static_cast<uint64_t>(actions) * depth_factor;
        }

        [[nodiscard]] std::expected<void, graph_build_error> validate_sizes(const game_graph& graph) noexcept
        {
            if (graph.row_offsets.size() != static_cast<size_t>(graph.node_count + 1)
                || graph.node_types.size() != static_cast<size_t>(graph.node_count)
                || graph.infoset_id.size() != static_cast<size_t>(graph.node_count)
                || graph.node_depth.size() != static_cast<size_t>(graph.node_count)
                || graph.subtree_size.size() != static_cast<size_t>(graph.node_count)) {
                return validation_error(graph_build_error_kind::invalid_graph);
            }

            return {};
        }

        [[nodiscard]] std::expected<void, graph_build_error> validate_csr(const game_graph& graph) noexcept
        {
            if (graph.row_offsets.empty() || graph.row_offsets[0] != 0) {
                return validation_error(graph_build_error_kind::invalid_graph);
            }

            for (uint32_t i = 1; i < static_cast<uint32_t>(graph.row_offsets.size()); ++i) {
                if (graph.row_offsets[i] < graph.row_offsets[i - 1]) {
                    return validation_error(graph_build_error_kind::invalid_graph, i);
                }
            }

            if (graph.row_offsets.back() != static_cast<uint32_t>(graph.edges.size())) {
                return validation_error(graph_build_error_kind::invalid_graph);
            }

            for (const auto& e : graph.edges) {
                if (e.child_node >= graph.node_count) {
                    return validation_error(graph_build_error_kind::invalid_graph, e.child_node);
                }
            }

            return {};
        }

        [[nodiscard]] std::expected<void, graph_build_error> validate_sizes_and_csr(const game_graph& graph) noexcept
        {
            if (auto result = validate_sizes(graph); !result) {
                return result;
            }
            return validate_csr(graph);
        }

        [[nodiscard]] std::expected<void, graph_build_error> validate_tree_metadata(const game_graph& graph) noexcept
        {
            if (auto result = validate_sizes_and_csr(graph); !result) {
                return result;
            }
            if (graph.node_count == 0) {
                return {};
            }

            if (graph.root_node >= graph.node_count) {
                return validation_error(graph_build_error_kind::invalid_graph, graph.root_node);
            }

            std::vector<uint32_t> indegree(graph.node_count, 0);
            std::vector<uint32_t> child_seen(graph.node_count, 0);
            std::vector<uint32_t> action_seen;

            for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
                const auto edges_span = graph.out_edges(node_id);
                const auto degree = static_cast<uint32_t>(edges_span.size());
                const auto source_stamp = node_id + 1;
                if (action_seen.size() < degree) {
                    action_seen.resize(degree, 0);
                }

                for (const auto& e : edges_span) {
                    if (e.action_index >= degree) {
                        return validation_error(graph_build_error_kind::invalid_graph, node_id, e.child_node);
                    }
                    if (action_seen[e.action_index] == source_stamp) {
                        return validation_error(graph_build_error_kind::invalid_graph, node_id, e.child_node);
                    }
                    action_seen[e.action_index] = source_stamp;

                    if (child_seen[e.child_node] == source_stamp) {
                        return validation_error(graph_build_error_kind::invalid_graph, node_id, e.child_node);
                    }
                    child_seen[e.child_node] = source_stamp;

                    ++indegree[e.child_node];

                    if (e.child_node >= node_id) {
                        return validation_error(graph_build_error_kind::invalid_graph, node_id, e.child_node);
                    }
                }
            }

            if (indegree[graph.root_node] != 0) {
                return validation_error(graph_build_error_kind::invalid_graph, graph.root_node);
            }
            for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
                if (node_id != graph.root_node && indegree[node_id] != 1) {
                    return validation_error(graph_build_error_kind::invalid_graph, node_id);
                }
            }

            for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
                uint32_t expected_subtree_size = 1;
                for (const auto& e : graph.out_edges(node_id)) {
                    expected_subtree_size += graph.subtree_size[e.child_node];
                }
                if (graph.subtree_size[node_id] != expected_subtree_size) {
                    return validation_error(graph_build_error_kind::invalid_graph, node_id);
                }
            }

            return {};
        }
    }

    std::expected<void, graph_build_error> graph_validation::validate_infosets(const game_graph& graph) noexcept
    {
        if (auto result = validate_sizes(graph); !result) {
            return result;
        }

        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            if (!graph.is_player_node(node_id)) {
                continue;
            }
            if (graph.infoset_id[node_id] == game_graph::INVALID_INFOSET) {
                return validation_error(graph_build_error_kind::uninitialized_infoset, node_id);
            }
            if (graph.infoset_id[node_id] >= graph.infoset_count) {
                return validation_error(graph_build_error_kind::invalid_graph, node_id);
            }
        }

        return {};
    }

    std::expected<void, graph_build_error> graph_validation::validate_metadata(const game_graph& graph) noexcept
    {
        if (auto result = validate_sizes_and_csr(graph); !result) {
            return result;
        }

        if (graph.node_count == 0) {
            if (graph.root_node == 0
                && graph.terminal_count == 0
                && graph.infoset_count == 0
                && graph.max_depth == 0) {
                return {};
            }
            return validation_error(graph_build_error_kind::invalid_graph);
        }

        if (graph.root_node >= graph.node_count) {
            return validation_error(graph_build_error_kind::invalid_graph, graph.root_node);
        }

        uint32_t terminal_count = 0;
        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            if (graph.node_depth[node_id] > graph.max_depth) {
                return validation_error(graph_build_error_kind::invalid_graph, node_id);
            }
            if (graph.subtree_size[node_id] == 0) {
                return validation_error(graph_build_error_kind::invalid_graph, node_id);
            }
            if (graph.is_terminal(node_id)) {
                ++terminal_count;
                if (graph.row_offsets[node_id] != graph.row_offsets[node_id + 1]) {
                    return validation_error(graph_build_error_kind::invalid_graph, node_id);
                }
            }
        }

        if (terminal_count != graph.terminal_count) {
            return validation_error(graph_build_error_kind::invalid_graph);
        }

        return {};
    }

    std::expected<void, graph_build_error> graph_validation::validate_structure(const game_graph& graph) noexcept
    {
        return validate_tree_metadata(graph);
    }

    std::expected<void, graph_build_error> graph_validation::validate_partitions(const game_graph& graph) noexcept
    {
        if (auto result = validate_sizes_and_csr(graph); !result) {
            return result;
        }

        if (graph.partitions.empty()) {
            return {};
        }

        uint32_t prev_end = 0;
        for (const auto& p : graph.partitions) {
            if (p.begin_node != prev_end || p.end_node <= p.begin_node || p.end_node > graph.node_count) {
                return validation_error(graph_build_error_kind::invalid_graph_partitions, p.begin_node, p.end_node);
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
                estimated_work += estimate_node_work(graph, node_id);
                min_depth = std::min(min_depth, static_cast<uint16_t>(graph.node_depth[node_id]));
                max_depth = std::max(max_depth, static_cast<uint16_t>(graph.node_depth[node_id]));
            }

            if (p.node_count != p.end_node - p.begin_node
                || p.terminal_count != terminal_count
                || p.action_count != action_count
                || p.estimated_work != estimated_work
                || p.min_depth != min_depth
                || p.max_depth != max_depth) {
                return validation_error(graph_build_error_kind::invalid_graph_partitions, p.begin_node, p.end_node);
            }

            prev_end = p.end_node;
        }

        if (prev_end != graph.node_count) {
            return validation_error(graph_build_error_kind::invalid_graph_partitions, prev_end);
        }

        return {};
    }

    std::expected<void, graph_build_error> graph_validation::validate_all(const game_graph& graph) noexcept
    {
        if (auto result = validate_structure(graph); !result) {
            return result;
        }
        if (auto result = validate_metadata(graph); !result) {
            return result;
        }
        if (auto result = validate_infosets(graph); !result) {
            return result;
        }
        return validate_partitions(graph);
    }

    bool graph_validation::validate(const game_graph& graph) noexcept
    {
        return validate_all(graph).has_value();
    }

    void graph_builder::record_error_(
        const graph_build_error_kind kind,
        const uint32_t node_id,
        const uint32_t related_node_id) noexcept
    {
        if (!has_pending_error_) {
            pending_error_ = graph_build_error{kind, node_id, related_node_id};
            has_pending_error_ = true;
        }
    }

    uint32_t graph_builder::add_node(cfr::node_kind kind)
    {
        assert(!finalized_);
        if (finalized_) {
            record_error_(graph_build_error_kind::already_finalized);
            return game_graph::INVALID_INFOSET;
        }
        const auto node_id = static_cast<uint32_t>(node_types_.size());
        node_types_.push_back(kind);
        edges_by_node_.emplace_back();
        infoset_ids_.push_back(game_graph::INVALID_INFOSET);  /**< Allocate per node. */
        return node_id;
    }

    void graph_builder::add_edge(const uint32_t source_node, const uint32_t dest_node, const uint16_t action_index)
    {
        assert(!finalized_);
        assert(source_node < node_types_.size());
        assert(dest_node < node_types_.size());
        if (finalized_) {
            record_error_(graph_build_error_kind::already_finalized, source_node, dest_node);
            return;
        }
        if (source_node >= node_types_.size() || dest_node >= node_types_.size()) {
            record_error_(graph_build_error_kind::invalid_graph, source_node, dest_node);
            return;
        }
        edges_by_node_[source_node].push_back({dest_node, action_index});
    }

    void graph_builder::set_infoset_id(const uint32_t node_id, const uint32_t infoset_id) noexcept
    {
        assert(!finalized_);
        assert(node_id < node_types_.size());
        assert(node_types_[node_id] == cfr::node_kind::player || 
               node_types_[node_id] == cfr::node_kind::player_chance);
        if (finalized_) {
            record_error_(graph_build_error_kind::already_finalized, node_id);
            return;
        }
        if (node_id >= node_types_.size()) {
            record_error_(graph_build_error_kind::invalid_graph, node_id);
            return;
        }
        if (node_types_[node_id] != cfr::node_kind::player
            && node_types_[node_id] != cfr::node_kind::player_chance) {
            record_error_(graph_build_error_kind::invalid_graph, node_id);
            return;
        }
        infoset_ids_[node_id] = infoset_id;  /**< O(1) direct assignment. */
    }

    std::expected<graph_builder::dfs_result, graph_build_error> graph_builder::compute_tree_metadata_() const {
        const auto node_count = static_cast<uint32_t>(node_types_.size());
        if (root_ >= node_count) {
            return std::unexpected(graph_build_error{graph_build_error_kind::root_out_of_range, root_});
        }

        dfs_result result;
        result.depth.assign(node_count, 0);
        result.subtree_size.assign(node_count, 0);
        result.dfs_order.assign(node_count, game_graph::INVALID_INFOSET);  /**< old_id -> new_id. */
        result.inverse_order.assign(node_count, game_graph::INVALID_INFOSET);  /**< new_id -> old_id. */
        result.max_depth = 0;

        std::vector<uint8_t> visited(node_count, 0);  /**< Use uint8_t for consistency. */
        uint32_t dfs_counter = 0;  /**< Post-order counter. */

        /** Iterative DFS to avoid stack overflow on deep trees. */
        struct StackFrame {
            uint32_t node_id;
            uint16_t depth;
            uint8_t phase;  /**< 0=entry, 1=return. */
        };
        std::vector<StackFrame> stack;
        stack.push_back({root_, 0, 0});

        while (!stack.empty()) {
            auto& frame = stack.back();
            
            if (frame.phase == 0) {
                /** Entry phase. */
                if (visited[frame.node_id]) {
                    stack.pop_back();
                    continue;
                }
                visited[frame.node_id] = 1;
                result.depth[frame.node_id] = frame.depth;
                result.max_depth = std::max(result.max_depth, frame.depth);
                 
                frame.phase = 1;  /**< Move to return phase. */
                 
                const auto node_id = frame.node_id;
                const auto depth = frame.depth;

                /**
                 * Push children in reverse action order so post-order remains
                 * deterministic with lower action subtrees receiving lower node IDs.
                 */
                const auto& node_edges = edges_by_node_[node_id];
                for (auto it = node_edges.rbegin();
                     it != node_edges.rend();
                     ++it) {
                    const auto& e = *it;
                    if (!visited[e.child_node]) {
                        if (depth == std::numeric_limits<uint16_t>::max()) {
                            return std::unexpected(graph_build_error{
                                graph_build_error_kind::depth_overflow,
                                node_id,
                                e.child_node
                            });
                        }
                        stack.push_back({e.child_node, static_cast<uint16_t>(depth + 1), 0});
                    }
                }
            } else {
                /** Return phase: post-order processing. */
                uint32_t size = 1;
                for (const auto& e : edges_by_node_[frame.node_id]) {
                    size += result.subtree_size[e.child_node];
                }
                result.subtree_size[frame.node_id] = size;
                
                /** Assign post-order number. */
                result.dfs_order[frame.node_id] = dfs_counter;
                result.inverse_order[dfs_counter] = frame.node_id;
                ++dfs_counter;
                
                stack.pop_back();
            }
        }

        /** Verify all nodes were visited; connected tree from root. */
        for (uint32_t i = 0; i < node_count; ++i) {
            if (!visited[i]) {
                return std::unexpected(graph_build_error{graph_build_error_kind::disconnected_tree, i});
            }
        }

        return result;
    }

    void graph_builder::sort_edges_by_action_()
    {
        for (auto& node_edges : edges_by_node_) {
            std::sort(
                node_edges.begin(),
                node_edges.end(),
                [](const edge lhs, const edge rhs) noexcept {
                    return lhs.action_index < rhs.action_index;
                });
        }
    }

    void graph_builder::build_node_arrays_(game_graph& graph, const dfs_result& metadata) const
    {
        const auto node_count = static_cast<uint32_t>(node_types_.size());
        std::vector<cfr::node_kind> reordered_node_types(node_count);
        std::vector<uint32_t> reordered_infoset_ids(node_count);
        std::vector<uint16_t> reordered_depth(node_count);
        std::vector<uint32_t> reordered_subtree_size(node_count);

        for (uint32_t new_id = 0; new_id < node_count; ++new_id) {
            const auto old_id = metadata.inverse_order[new_id];
            reordered_node_types[new_id] = node_types_[old_id];
            reordered_infoset_ids[new_id] = infoset_ids_[old_id];
            reordered_depth[new_id] = metadata.depth[old_id];
            reordered_subtree_size[new_id] = metadata.subtree_size[old_id];
        }

        graph.node_types = std::move(reordered_node_types);
        graph.infoset_id = std::move(reordered_infoset_ids);
        graph.node_depth = std::move(reordered_depth);
        graph.subtree_size = std::move(reordered_subtree_size);
        graph.max_depth = metadata.max_depth;
        graph.root_node = metadata.dfs_order[root_];
    }

    void graph_builder::build_csr_(game_graph& graph, const dfs_result& metadata) const
    {
        const auto node_count = static_cast<uint32_t>(node_types_.size());
        uint32_t edge_count = 0;
        for (const auto& node_edges : edges_by_node_) {
            edge_count += static_cast<uint32_t>(node_edges.size());
        }

        std::vector<uint32_t> row_offsets;
        std::vector<edge> edges;
        row_offsets.reserve(static_cast<size_t>(node_count) + 1);
        edges.reserve(edge_count);

        row_offsets.push_back(0);
        for (uint32_t new_id = 0; new_id < node_count; ++new_id) {
            const auto old_id = metadata.inverse_order[new_id];
            for (const auto& e : edges_by_node_[old_id]) {
                edges.push_back({metadata.dfs_order[e.child_node], e.action_index});
            }
            row_offsets.push_back(static_cast<uint32_t>(edges.size()));
        }

        graph.row_offsets = std::move(row_offsets);
        graph.edges = std::move(edges);
    }

    void graph_builder::compute_graph_counts_(game_graph& graph) const noexcept
    {
        graph.infoset_count = 0;
        graph.terminal_count = 0;

        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            if (graph.node_types[node_id] == cfr::node_kind::player || 
                graph.node_types[node_id] == cfr::node_kind::player_chance) {
                if (graph.infoset_id[node_id] != game_graph::INVALID_INFOSET) {
                    graph.infoset_count = std::max(graph.infoset_count, graph.infoset_id[node_id] + 1);
                }
            }

            if (graph.node_types[node_id] == cfr::node_kind::terminal) {
                graph.terminal_count++;
            }
        }
    }

    std::expected<void, graph_build_error> graph_builder::validate_complete_(const game_graph& graph) noexcept
    {
        return graph_validation::validate_all(graph);
    }

    std::expected<game_graph, graph_build_error> graph_builder::build()
    {
        if (finalized_) {
            return std::unexpected(graph_build_error{graph_build_error_kind::already_finalized});
        }
        if (has_pending_error_) {
            return std::unexpected(pending_error_);
        }

        const auto node_count = static_cast<uint32_t>(node_types_.size());
        if (node_count == 0) {
            return std::unexpected(graph_build_error{graph_build_error_kind::empty_graph});
        }

        sort_edges_by_action_();

        auto metadata_result = compute_tree_metadata_();
        if (!metadata_result) {
            return std::unexpected(metadata_result.error());
        }

        game_graph graph;
        graph.node_count = node_count;
        build_node_arrays_(graph, *metadata_result);
        build_csr_(graph, *metadata_result);
        compute_graph_counts_(graph);

        partition_strategy strategy;
        graph.partitions = compute_partitions(graph, strategy);

        if (auto result = validate_complete_(graph); !result) {
            return std::unexpected(result.error());
        }

        finalized_ = true;
        return graph;
    }

    double game_graph::partition_balance_metric() const noexcept
    {
        if (partitions.empty()) {
            return 0.0;
        }

        const auto n = static_cast<double>(partitions.size());
        
        /** Compute mean estimated_work. */
        double sum = 0.0;
        for (const auto& p : partitions) {
            sum += static_cast<double>(p.estimated_work);
        }
        const auto mean = sum / n;

        if (mean == 0.0) {
            return 0.0;  /**< All partitions have zero work. */
        }

        /** Compute standard deviation. */
        double sum_sq_dev = 0.0;
        for (const auto& p : partitions) {
            const auto dev = static_cast<double>(p.estimated_work) - mean;
            sum_sq_dev += dev * dev;
        }
        const auto stddev = std::sqrt(sum_sq_dev / n);

        /** Coefficient of variation. */
        return stddev / mean;
    }

    /**
     * Compute partition metadata for a single partition range.
     */
    static graph_partition compute_partition_metadata(
        const game_graph& graph,
        const uint32_t begin_node,
        const uint32_t end_node,
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

        /** Scan nodes in partition. */
        for (uint32_t node_id = begin_node; node_id < end_node; ++node_id) {
            if (graph.is_terminal(node_id)) {
                p.terminal_count++;
            }
            
            const auto num_actions = graph.action_count(node_id);
            p.action_count += num_actions;
            p.estimated_work += node_work.empty()
                ? estimate_node_work(graph, node_id)
                : node_work[node_id];
            
            p.min_depth = std::min(p.min_depth, static_cast<uint16_t>(graph.node_depth[node_id]));
            p.max_depth = std::max(p.max_depth, static_cast<uint16_t>(graph.node_depth[node_id]));
        }

        if (p.min_depth == std::numeric_limits<uint16_t>::max()) {
            p.min_depth = 0;
        }

        return p;
    }

    std::vector<graph_partition> compute_partitions(
        const game_graph& graph,
        const partition_strategy& strategy)
    {
        std::vector<graph_partition> partitions;

        if (graph.node_count == 0) {
            return partitions;
        }

        const auto target_count = std::min(strategy.target_partition_count, graph.node_count);
        if (target_count == 0) {
            return partitions;
        }

        if (target_count == 1) {
            partitions.push_back(compute_partition_metadata(graph, 0, graph.node_count));
            return partitions;
        }

        /** Compute metadata for all nodes first using work heuristic. */
        std::vector<uint64_t> node_work(graph.node_count);
        uint64_t total_work = 0;
        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            node_work[node_id] = estimate_node_work(graph, node_id);
            total_work += node_work[node_id];
        }

        const auto target_work = (total_work + target_count - 1) / target_count;

        /**
         * Greedily partition by accumulated work, respecting DFS post-order.
         * WARNING: This algorithm CAN split subtrees across partitions.
         * Example: if subtree B (nodes 1-3) has high work, the algorithm might cut
         * after node 2, leaving nodes 2 and 3 in different partitions.
         * This is a tradeoff: perfect work balance vs perfect subtree locality.
         * For better subtree preservation, would need to penalize cuts within subtrees.
         */
        uint32_t partition_start = 0;
        uint64_t current_work = 0;

        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            current_work += node_work[node_id];

            if (const bool last_node = (node_id == graph.node_count - 1); (current_work >= target_work) || last_node) {
                partitions.push_back(
                    compute_partition_metadata(graph, partition_start, node_id + 1, node_work)
                );
                partition_start = node_id + 1;
                current_work = 0;
            }
        }

        return partitions;
    }

}
