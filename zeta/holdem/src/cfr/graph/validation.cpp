#include "cfr/graph/validation.h"
#include <vector>

namespace zeta::holdem::cfr {

    namespace {

        struct layout_validated_t {
        };
        constexpr layout_validated_t layout_validated{};

        [[nodiscard]] std::unexpected<graph_build_error> validation_error(
            const graph_build_error_kind kind,
            const uint32_t node_id = 0,
            const uint32_t related_node_id = 0) noexcept
        {
            return std::unexpected(graph_build_error{kind, node_id, related_node_id});
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

        [[nodiscard]] std::expected<void, graph_build_error> validate_core_layout(const game_graph& graph) noexcept
        {
            if (auto result = validate_sizes(graph); !result) {
                return result;
            }
            return validate_csr(graph);
        }

        class graph_validator {
        public:
            [[nodiscard]] static std::expected<void, graph_build_error> validate_all(
                const game_graph& graph) noexcept
            {
                if (auto result = validate_core_layout(graph); !result) {
                    return result;
                }
                if (auto result = validate_structure(graph, layout_validated); !result) {
                    return result;
                }
                if (auto result = validate_metadata(graph, layout_validated); !result) {
                    return result;
                }
                if (auto result = validate_infosets(graph, layout_validated); !result) {
                    return result;
                }
                return {};
            }

            [[nodiscard]] static std::expected<void, graph_build_error> validate_structure(
                const game_graph& graph) noexcept
            {
                if (auto result = validate_core_layout(graph); !result) {
                    return result;
                }
                return validate_structure(graph, layout_validated);
            }

            [[nodiscard]] static std::expected<void, graph_build_error> validate_metadata(
                const game_graph& graph) noexcept
            {
                if (auto result = validate_core_layout(graph); !result) {
                    return result;
                }
                return validate_metadata(graph, layout_validated);
            }

            [[nodiscard]] static std::expected<void, graph_build_error> validate_infosets(
                const game_graph& graph) noexcept
            {
                if (auto result = validate_sizes(graph); !result) {
                    return result;
                }
                return validate_infosets(graph, layout_validated);
            }

        private:
            [[nodiscard]] static std::expected<void, graph_build_error> validate_structure(
                const game_graph& graph,
                layout_validated_t) noexcept
            {
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

            [[nodiscard]] static std::expected<void, graph_build_error> validate_metadata(
                const game_graph& graph,
                layout_validated_t) noexcept
            {
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

            [[nodiscard]] static std::expected<void, graph_build_error> validate_infosets(
                const game_graph& graph,
                layout_validated_t) noexcept
            {
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
        };
    }

    std::expected<void, graph_build_error> graph_validation::validate_infosets(const game_graph& graph) noexcept
    {
        return graph_validator::validate_infosets(graph);
    }

    std::expected<void, graph_build_error> graph_validation::validate_metadata(const game_graph& graph) noexcept
    {
        return graph_validator::validate_metadata(graph);
    }

    std::expected<void, graph_build_error> graph_validation::validate_structure(const game_graph& graph) noexcept
    {
        return graph_validator::validate_structure(graph);
    }

    std::expected<void, graph_build_error> graph_validation::validate_all(const game_graph& graph) noexcept
    {
        return graph_validator::validate_all(graph);
    }

    bool graph_validation::validate(const game_graph& graph) noexcept
    {
        return validate_all(graph).has_value();
    }

}
