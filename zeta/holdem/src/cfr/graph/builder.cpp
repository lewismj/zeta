#include "cfr/graph/builder.h"
#include "cfr/graph/validation.h"
#include <algorithm>
#include <limits>

namespace zeta::holdem::cfr {

    namespace {

        using adjacency_list = std::vector<std::vector<edge>>;

        struct tree_metadata {
            std::vector<uint16_t> depth;
            std::vector<uint32_t> subtree_size;
            std::vector<uint32_t> dfs_order;      /**< dfs_order[old_id] = new post-order id. */
            std::vector<uint32_t> inverse_order;  /**< inverse_order[new_id] = old id. */
            uint16_t max_depth = 0;
        };

        void sort_edges_by_action(adjacency_list& edges_by_node)
        {
            for (auto& node_edges : edges_by_node) {
                std::sort(
                    node_edges.begin(),
                    node_edges.end(),
                    [](const edge lhs, const edge rhs) noexcept {
                        return lhs.action_index < rhs.action_index;
                    });
            }
        }

        class tree_metadata_builder {
        public:
            tree_metadata_builder(
                const adjacency_list& edges_by_node,
                const std::vector<node_kind>& node_types,
                const uint32_t root) noexcept :
                edges_by_node_(edges_by_node),
                node_types_(node_types),
                root_(root)
            {
            }

            [[nodiscard]] std::expected<tree_metadata, graph_build_error> build() const
            {
                const auto node_count = static_cast<uint32_t>(node_types_.size());
                if (root_ >= node_count) {
                    return std::unexpected(graph_build_error{graph_build_error_kind::root_out_of_range, root_});
                }

                tree_metadata result;
                result.depth.assign(node_count, 0);
                result.subtree_size.assign(node_count, 0);
                result.dfs_order.assign(node_count, game_graph::INVALID_NODE);
                result.inverse_order.assign(node_count, game_graph::INVALID_NODE);

                std::vector<uint8_t> visited(node_count, 0);
                uint32_t dfs_counter = 0;

                struct stack_frame {
                    uint32_t node_id;
                    uint16_t depth;
                    uint8_t phase;  /**< 0=entry, 1=return. */
                };

                std::vector<stack_frame> stack;
                stack.push_back({root_, 0, 0});

                while (!stack.empty()) {
                    auto& frame = stack.back();

                    if (frame.phase == 0) {
                        if (visited[frame.node_id]) {
                            stack.pop_back();
                            continue;
                        }
                        visited[frame.node_id] = 1;
                        result.depth[frame.node_id] = frame.depth;
                        result.max_depth = std::max(result.max_depth, frame.depth);

                        frame.phase = 1;

                        const auto node_id = frame.node_id;
                        const auto depth = frame.depth;
                        const auto& node_edges = edges_by_node_[node_id];

                        for (auto it = node_edges.rbegin(); it != node_edges.rend(); ++it) {
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
                        uint32_t size = 1;
                        for (const auto& e : edges_by_node_[frame.node_id]) {
                            size += result.subtree_size[e.child_node];
                        }
                        result.subtree_size[frame.node_id] = size;

                        result.dfs_order[frame.node_id] = dfs_counter;
                        result.inverse_order[dfs_counter] = frame.node_id;
                        ++dfs_counter;

                        stack.pop_back();
                    }
                }

                for (uint32_t i = 0; i < node_count; ++i) {
                    if (!visited[i]) {
                        return std::unexpected(graph_build_error{graph_build_error_kind::disconnected_tree, i});
                    }
                }

                return result;
            }

        private:
            const adjacency_list& edges_by_node_;
            const std::vector<node_kind>& node_types_;
            uint32_t root_;
        };

        class csr_builder {
        public:
            csr_builder(
                const adjacency_list& edges_by_node,
                const std::vector<node_kind>& node_types,
                const std::vector<uint32_t>& infoset_ids,
                const uint32_t root) noexcept :
                edges_by_node_(edges_by_node),
                node_types_(node_types),
                infoset_ids_(infoset_ids),
                root_(root)
            {
            }

            [[nodiscard]] game_graph build(const tree_metadata& metadata) const
            {
                game_graph graph;
                graph.node_count = static_cast<uint32_t>(node_types_.size());

                build_node_arrays(graph, metadata);
                build_csr(graph, metadata);

                return graph;
            }

        private:
            void build_node_arrays(game_graph& graph, const tree_metadata& metadata) const
            {
                const auto node_count = graph.node_count;
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

                const auto postorder_root = metadata.dfs_order[root_];
                graph.root_node = postorder_root;
            }

            void build_csr(game_graph& graph, const tree_metadata& metadata) const
            {
                const auto node_count = graph.node_count;
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

            const adjacency_list& edges_by_node_;
            const std::vector<node_kind>& node_types_;
            const std::vector<uint32_t>& infoset_ids_;
            uint32_t root_;
        };

        class graph_statistics {
        public:
            static void compute_counts(game_graph& graph) noexcept
            {
                graph.infoset_count = 0;
                graph.terminal_count = 0;

                for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
                    if (graph.node_types[node_id] == cfr::node_kind::player
                        || graph.node_types[node_id] == cfr::node_kind::player_chance) {
                        if (graph.infoset_id[node_id] != game_graph::INVALID_INFOSET) {
                            graph.infoset_count = std::max(graph.infoset_count, graph.infoset_id[node_id] + 1);
                        }
                    }

                    if (graph.node_types[node_id] == cfr::node_kind::terminal) {
                        graph.terminal_count++;
                    }
                }
            }
        };
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
        if (finalized_) {
            record_error_(graph_build_error_kind::already_finalized);
            return game_graph::INVALID_NODE;
        }
        const auto node_id = static_cast<uint32_t>(node_types_.size());
        node_types_.push_back(kind);
        edges_by_node_.emplace_back();
        infoset_ids_.push_back(game_graph::INVALID_INFOSET);
        return node_id;
    }

    void graph_builder::add_edge(const uint32_t source_node, const uint32_t dest_node, const uint16_t action_index)
    {
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
        infoset_ids_[node_id] = infoset_id;
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

        sort_edges_by_action(edges_by_node_);

        auto metadata_result = tree_metadata_builder(edges_by_node_, node_types_, root_).build();
        if (!metadata_result) {
            return std::unexpected(metadata_result.error());
        }

        auto graph = csr_builder(edges_by_node_, node_types_, infoset_ids_, root_).build(*metadata_result);
        graph_statistics::compute_counts(graph);

        if (auto result = graph_validation::validate_all(graph); !result) {
            return std::unexpected(result.error());
        }

        finalized_ = true;
        return graph;
    }

}
