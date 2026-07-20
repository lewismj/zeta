#pragma once

#include "cfr/graph/graph.h"
#include "cfr/tables/table_layout.h"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <limits>
#include <ostream>
#include <span>
#include <vector>

namespace zeta::holdem::cfr::solver {

    inline constexpr uint8_t INVALID_PLAYER = std::numeric_limits<uint8_t>::max();
    inline constexpr uint32_t INVALID_METADATA_ID = std::numeric_limits<uint32_t>::max();

    enum class holdem_street : uint8_t {
        invalid = 0,
        preflop = 1,
        flop = 2,
        turn = 3,
        river = 4
    };

    enum class chance_mode : uint8_t {
        enumerate = 0,
        sample = 1
    };

    enum class table_storage_precision : uint8_t {
        float32 = 0,
        float64 = 1
    };

    enum class accumulation_precision : uint8_t {
        float32 = 0,
        float64 = 1
    };

    enum class reduction_order : uint8_t {
        deterministic_worker_order = 0,
        owner_range_then_worker = 1
    };

    struct numeric_policy {
        table_storage_precision table_storage = table_storage_precision::float32;
        accumulation_precision accumulation = accumulation_precision::float64;
    };

    struct reduction_policy {
        reduction_order order = reduction_order::deterministic_worker_order;
    };

    struct solver_node_state_metadata {
        holdem_street street = holdem_street::invalid;
        uint32_t public_state_id = INVALID_METADATA_ID;
        uint32_t betting_state_id = INVALID_METADATA_ID;
    };

    struct solver_graph_annotations {
        std::vector<uint8_t> actor_by_node;
        std::vector<uint32_t> chance_event_id_by_node;
        std::vector<uint32_t> terminal_leaf_id_by_node;
        std::vector<solver_node_state_metadata> state_by_node;
    };

    template <std::size_t N>
    struct solver_graph_view {
        static constexpr std::size_t player_count = N;

        const game_graph* graph = nullptr;
        std::span<const uint8_t> actor_by_node{};
        std::span<const uint32_t> chance_event_id_by_node{};
        std::span<const uint32_t> terminal_leaf_id_by_node{};
        std::span<const solver_node_state_metadata> state_by_node{};
    };

    enum class solver_graph_metadata_error_kind : uint8_t {
        missing_graph,
        player_count_out_of_range,
        side_array_size_mismatch,
        invalid_actor,
        invalid_chance_event_id,
        invalid_terminal_leaf_id,
        invalid_state_metadata,
        incompatible_infoset_actor,
        unsupported_chance_mode
    };

    struct solver_graph_metadata_error {
        solver_graph_metadata_error_kind kind{};
        uint32_t node_id = game_graph::INVALID_NODE;
        uint32_t infoset_id = game_graph::INVALID_INFOSET;
        uint32_t related_node_id = game_graph::INVALID_NODE;
    };

    [[nodiscard]] constexpr const char* to_string(const solver_graph_metadata_error_kind kind) noexcept
    {
        using enum solver_graph_metadata_error_kind;
        switch (kind) {
            case missing_graph:                 return "solver_graph_metadata_error_kind::missing_graph";
            case player_count_out_of_range:     return "solver_graph_metadata_error_kind::player_count_out_of_range";
            case side_array_size_mismatch:      return "solver_graph_metadata_error_kind::side_array_size_mismatch";
            case invalid_actor:                 return "solver_graph_metadata_error_kind::invalid_actor";
            case invalid_chance_event_id:       return "solver_graph_metadata_error_kind::invalid_chance_event_id";
            case invalid_terminal_leaf_id:      return "solver_graph_metadata_error_kind::invalid_terminal_leaf_id";
            case invalid_state_metadata:        return "solver_graph_metadata_error_kind::invalid_state_metadata";
            case incompatible_infoset_actor:    return "solver_graph_metadata_error_kind::incompatible_infoset_actor";
            case unsupported_chance_mode:       return "solver_graph_metadata_error_kind::unsupported_chance_mode";
        }
        return "solver_graph_metadata_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const solver_graph_metadata_error_kind kind)
    {
        return os << to_string(kind);
    }

    /**
     * Create a non-owning solver graph view from owning side-array annotations.
     */
    template <std::size_t N>
    [[nodiscard]] solver_graph_view<N> make_solver_graph_view(
        const game_graph& graph,
        const solver_graph_annotations& annotations) noexcept
    {
        return solver_graph_view<N>{
            .graph = &graph,
            .actor_by_node = annotations.actor_by_node,
            .chance_event_id_by_node = annotations.chance_event_id_by_node,
            .terminal_leaf_id_by_node = annotations.terminal_leaf_id_by_node,
            .state_by_node = annotations.state_by_node
        };
    }

    /**
     * Validate side-array metadata against immutable graph topology.
     */
    template <std::size_t N>
    [[nodiscard]] std::expected<void, solver_graph_metadata_error> validate_solver_graph_view(
        const solver_graph_view<N>& view,
        const chance_mode mode = chance_mode::enumerate)
    {
        if (view.graph == nullptr) {
            return std::unexpected(solver_graph_metadata_error{solver_graph_metadata_error_kind::missing_graph});
        }
        if constexpr (N == 0 || N > static_cast<std::size_t>(INVALID_PLAYER)) {
            return std::unexpected(solver_graph_metadata_error{
                solver_graph_metadata_error_kind::player_count_out_of_range
            });
        }
        if (mode != chance_mode::enumerate) {
            return std::unexpected(solver_graph_metadata_error{
                solver_graph_metadata_error_kind::unsupported_chance_mode
            });
        }

        const auto& graph = *view.graph;
        const auto node_count = static_cast<std::size_t>(graph.node_count);
        if (view.actor_by_node.size() != node_count
            || view.chance_event_id_by_node.size() != node_count
            || view.terminal_leaf_id_by_node.size() != node_count
            || view.state_by_node.size() != node_count) {
            return std::unexpected(solver_graph_metadata_error{
                solver_graph_metadata_error_kind::side_array_size_mismatch
            });
        }

        std::vector<uint8_t> actor_by_infoset(graph.infoset_count, INVALID_PLAYER);
        std::vector<uint32_t> first_node_by_infoset(graph.infoset_count, game_graph::INVALID_NODE);

        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            const auto kind = graph.node_types[node_id];
            const auto actor = view.actor_by_node[node_id];
            const auto chance_event_id = view.chance_event_id_by_node[node_id];
            const auto terminal_leaf_id = view.terminal_leaf_id_by_node[node_id];
            const auto& state = view.state_by_node[node_id];
            const auto is_player_like = graph.is_player_node(node_id);
            const auto is_chance_like = kind == node_kind::chance || kind == node_kind::player_chance;
            const auto is_terminal = kind == node_kind::terminal;

            if (is_player_like) {
                if (actor >= N) {
                    return std::unexpected(solver_graph_metadata_error{
                        solver_graph_metadata_error_kind::invalid_actor,
                        node_id,
                        graph.infoset_id[node_id]
                    });
                }

                const auto infoset_id = graph.infoset_id[node_id];
                auto& infoset_actor = actor_by_infoset[infoset_id];
                if (infoset_actor == INVALID_PLAYER) {
                    infoset_actor = actor;
                    first_node_by_infoset[infoset_id] = node_id;
                } else if (infoset_actor != actor) {
                    return std::unexpected(solver_graph_metadata_error{
                        solver_graph_metadata_error_kind::incompatible_infoset_actor,
                        node_id,
                        infoset_id,
                        first_node_by_infoset[infoset_id]
                    });
                }
            } else if (actor != INVALID_PLAYER) {
                return std::unexpected(solver_graph_metadata_error{
                    solver_graph_metadata_error_kind::invalid_actor,
                    node_id
                });
            }

            if (is_chance_like) {
                if (chance_event_id == INVALID_METADATA_ID) {
                    return std::unexpected(solver_graph_metadata_error{
                        solver_graph_metadata_error_kind::invalid_chance_event_id,
                        node_id
                    });
                }
            } else if (chance_event_id != INVALID_METADATA_ID) {
                return std::unexpected(solver_graph_metadata_error{
                    solver_graph_metadata_error_kind::invalid_chance_event_id,
                    node_id
                });
            }

            if (is_terminal) {
                if (terminal_leaf_id == INVALID_METADATA_ID) {
                    return std::unexpected(solver_graph_metadata_error{
                        solver_graph_metadata_error_kind::invalid_terminal_leaf_id,
                        node_id
                    });
                }
            } else if (terminal_leaf_id != INVALID_METADATA_ID) {
                return std::unexpected(solver_graph_metadata_error{
                    solver_graph_metadata_error_kind::invalid_terminal_leaf_id,
                    node_id
                });
            }

            if (state.street == holdem_street::invalid) {
                return std::unexpected(solver_graph_metadata_error{
                    solver_graph_metadata_error_kind::invalid_state_metadata,
                    node_id
                });
            }

            for (const auto& edge : graph.out_edges(node_id)) {
                const auto child_street = view.state_by_node[edge.child_node].street;
                if (child_street == holdem_street::invalid
                    || static_cast<uint8_t>(child_street) < static_cast<uint8_t>(state.street)) {
                    return std::unexpected(solver_graph_metadata_error{
                        solver_graph_metadata_error_kind::invalid_state_metadata,
                        edge.child_node,
                        graph.infoset_id[edge.child_node],
                        node_id
                    });
                }
            }
        }

        return {};
    }

    /**
     * FNV-1a hash helper used for stable compatibility keys.
     */
    struct compatibility_hasher {
        static constexpr uint64_t OFFSET = 14695981039346656037ull;
        static constexpr uint64_t PRIME = 1099511628211ull;

        uint64_t value = OFFSET;

        constexpr void add_u64(const uint64_t input) noexcept
        {
            for (uint32_t shift = 0; shift < 64; shift += 8) {
                value ^= (input >> shift) & 0xffu;
                value *= PRIME;
            }
        }

        template <class T>
        constexpr void add_enum(const T input) noexcept
        {
            add_u64(static_cast<uint64_t>(input));
        }
    };

    [[nodiscard]] inline uint64_t hash_numeric_policy(const numeric_policy policy) noexcept
    {
        compatibility_hasher hash;
        hash.add_enum(policy.table_storage);
        hash.add_enum(policy.accumulation);
        return hash.value;
    }

    [[nodiscard]] inline uint64_t hash_reduction_policy(const reduction_policy policy) noexcept
    {
        compatibility_hasher hash;
        hash.add_enum(policy.order);
        return hash.value;
    }

    [[nodiscard]] inline uint64_t hash_chance_mode(const chance_mode mode) noexcept
    {
        compatibility_hasher hash;
        hash.add_enum(mode);
        return hash.value;
    }

    [[nodiscard]] inline uint64_t hash_action_layout(const action_table_layout& layout) noexcept
    {
        compatibility_hasher hash;
        hash.add_u64(layout.infoset_count());
        hash.add_u64(layout.value_count());
        for (const auto offset : layout.action_offsets) {
            hash.add_u64(offset);
        }
        return hash.value;
    }

    template <std::size_t N>
    [[nodiscard]] uint64_t hash_solver_graph_metadata(const solver_graph_view<N>& view) noexcept
    {
        compatibility_hasher hash;
        if (view.graph == nullptr) {
            return hash.value;
        }

        const auto& graph = *view.graph;
        const auto node_count = static_cast<std::size_t>(graph.node_count);
        hash.add_u64(N);
        hash.add_u64(graph.node_count);
        hash.add_u64(graph.root_node);
        hash.add_u64(graph.terminal_count);
        hash.add_u64(graph.infoset_count);
        hash.add_u64(graph.max_depth);
        hash.add_u64(view.actor_by_node.size());
        hash.add_u64(view.chance_event_id_by_node.size());
        hash.add_u64(view.terminal_leaf_id_by_node.size());
        hash.add_u64(view.state_by_node.size());

        if (view.actor_by_node.size() < node_count
            || view.chance_event_id_by_node.size() < node_count
            || view.terminal_leaf_id_by_node.size() < node_count
            || view.state_by_node.size() < node_count) {
            return hash.value;
        }

        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            hash.add_enum(graph.node_types[node_id]);
            hash.add_u64(graph.infoset_id[node_id]);
            hash.add_u64(view.actor_by_node[node_id]);
            hash.add_u64(view.chance_event_id_by_node[node_id]);
            hash.add_u64(view.terminal_leaf_id_by_node[node_id]);
            hash.add_enum(view.state_by_node[node_id].street);
            hash.add_u64(view.state_by_node[node_id].public_state_id);
            hash.add_u64(view.state_by_node[node_id].betting_state_id);
            hash.add_u64(graph.row_offsets[node_id + 1] - graph.row_offsets[node_id]);
        }

        return hash.value;
    }

    struct solver_compatibility_key {
        uint64_t graph_metadata_hash = 0;
        uint64_t action_layout_hash = 0;
        uint64_t numeric_policy_hash = 0;
        uint64_t reduction_policy_hash = 0;
        uint64_t chance_mode_hash = 0;
        uint32_t player_count = 0;
    };

    template <std::size_t N>
    [[nodiscard]] solver_compatibility_key make_solver_compatibility_key(
        const solver_graph_view<N>& view,
        const action_table_layout& layout,
        const numeric_policy numeric = {},
        const reduction_policy reduction = {},
        const chance_mode mode = chance_mode::enumerate) noexcept
    {
        return solver_compatibility_key{
            .graph_metadata_hash = hash_solver_graph_metadata(view),
            .action_layout_hash = hash_action_layout(layout),
            .numeric_policy_hash = hash_numeric_policy(numeric),
            .reduction_policy_hash = hash_reduction_policy(reduction),
            .chance_mode_hash = hash_chance_mode(mode),
            .player_count = static_cast<uint32_t>(N)
        };
    }

}
