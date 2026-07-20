#pragma once

#include "cfr/graph/graph.h"
#include "cfr/solver/metadata.h"
#include "cfr/tables/table_layout.h"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <limits>
#include <iterator>
#include <ostream>
#include <span>
#include <vector>

namespace zeta::holdem::cfr::solver {

    inline constexpr uint32_t INVALID_INFOSET_OWNER = std::numeric_limits<uint32_t>::max();
    inline constexpr uint32_t INVALID_ABSTRACTION_ID = INVALID_METADATA_ID;

    /**
     * Stable Hold'em information-set identity before dense table IDs are assigned.
     */
    struct holdem_infoset_key {
        uint8_t actor = INVALID_PLAYER;
        holdem_street street = holdem_street::invalid;
        uint16_t player_count = 0;
        uint32_t private_hand_class_id = INVALID_ABSTRACTION_ID;
        uint32_t public_board_abstraction_id = INVALID_ABSTRACTION_ID;
        uint32_t chance_runout_class_id = INVALID_ABSTRACTION_ID;
        uint32_t betting_history_abstraction_id = INVALID_ABSTRACTION_ID;
        uint32_t stack_pot_abstraction_id = INVALID_ABSTRACTION_ID;
        uint32_t legal_action_set_id = INVALID_ABSTRACTION_ID;
        uint32_t subgame_root_context_id = INVALID_ABSTRACTION_ID;

        [[nodiscard]] constexpr bool operator==(const holdem_infoset_key&) const noexcept = default;
    };

    /**
     * Explicit abstraction hooks used while generating Hold'em infoset keys.
     */
    struct exact_holdem_abstraction_policy {
        [[nodiscard]] constexpr uint32_t private_hand_class_id(const uint32_t exact_combo_class_id) const noexcept
        {
            return exact_combo_class_id;
        }

        [[nodiscard]] constexpr uint32_t public_board_abstraction_id(
            const solver_node_state_metadata state) const noexcept
        {
            return state.public_state_id;
        }

        [[nodiscard]] constexpr uint32_t chance_runout_class_id(const uint32_t chance_event_id) const noexcept
        {
            return chance_event_id;
        }
    };

    /**
     * Per-player-node infoset metadata before lowering to dense table IDs.
     */
    struct holdem_infoset_description {
        uint32_t node_id = game_graph::INVALID_NODE;
        holdem_infoset_key key{};
        uint32_t owner_id = INVALID_INFOSET_OWNER;
        std::vector<uint16_t> legal_action_ids;
    };

    /**
     * Dense infoset mapping and compact side arrays produced by key lowering.
     */
    struct holdem_infoset_lowering {
        std::vector<uint32_t> dense_id_by_node;
        std::vector<holdem_infoset_key> key_by_infoset;
        std::vector<uint32_t> owner_by_infoset;
        std::vector<uint32_t> legal_action_offsets;
        std::vector<uint16_t> legal_action_ids;
        std::vector<uint32_t> source_graph_infoset_by_infoset;

        [[nodiscard]] uint32_t infoset_count() const noexcept
        {
            return static_cast<uint32_t>(key_by_infoset.size());
        }

        [[nodiscard]] uint32_t action_count(const uint32_t infoset_id) const noexcept
        {
            return legal_action_offsets[infoset_id + 1u] - legal_action_offsets[infoset_id];
        }

        [[nodiscard]] std::span<const uint16_t> legal_actions(const uint32_t infoset_id) const noexcept
        {
            const auto begin = legal_action_offsets[infoset_id];
            const auto count = action_count(infoset_id);
            return count == 0u ? std::span<const uint16_t>{} : std::span<const uint16_t>{
                legal_action_ids.data() + begin,
                count
            };
        }
    };

    enum class holdem_infoset_error_kind : uint8_t {
        side_array_size_mismatch,
        invalid_player_node,
        duplicate_player_description,
        missing_player_description,
        invalid_actor,
        player_count_mismatch,
        invalid_street,
        invalid_abstraction_id,
        invalid_owner,
        legal_action_count_mismatch,
        duplicate_legal_action_id,
        inconsistent_shared_infoset,
        dense_id_overflow,
        invalid_lowering
    };

    struct holdem_infoset_error {
        holdem_infoset_error_kind kind{};
        uint32_t node_id = game_graph::INVALID_NODE;
        uint32_t infoset_id = game_graph::INVALID_INFOSET;
        uint32_t related_node_id = game_graph::INVALID_NODE;
    };

    [[nodiscard]] constexpr const char* to_string(const holdem_infoset_error_kind kind) noexcept
    {
        using enum holdem_infoset_error_kind;
        switch (kind) {
            case side_array_size_mismatch:      return "holdem_infoset_error_kind::side_array_size_mismatch";
            case invalid_player_node:          return "holdem_infoset_error_kind::invalid_player_node";
            case duplicate_player_description: return "holdem_infoset_error_kind::duplicate_player_description";
            case missing_player_description:   return "holdem_infoset_error_kind::missing_player_description";
            case invalid_actor:                return "holdem_infoset_error_kind::invalid_actor";
            case player_count_mismatch:        return "holdem_infoset_error_kind::player_count_mismatch";
            case invalid_street:               return "holdem_infoset_error_kind::invalid_street";
            case invalid_abstraction_id:       return "holdem_infoset_error_kind::invalid_abstraction_id";
            case invalid_owner:                return "holdem_infoset_error_kind::invalid_owner";
            case legal_action_count_mismatch:  return "holdem_infoset_error_kind::legal_action_count_mismatch";
            case duplicate_legal_action_id:    return "holdem_infoset_error_kind::duplicate_legal_action_id";
            case inconsistent_shared_infoset:  return "holdem_infoset_error_kind::inconsistent_shared_infoset";
            case dense_id_overflow:            return "holdem_infoset_error_kind::dense_id_overflow";
            case invalid_lowering:             return "holdem_infoset_error_kind::invalid_lowering";
        }
        return "holdem_infoset_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const holdem_infoset_error_kind kind)
    {
        return os << to_string(kind);
    }

    [[nodiscard]] constexpr bool valid_infoset_key(const holdem_infoset_key& key) noexcept
    {
        return key.actor != INVALID_PLAYER
            && key.street != holdem_street::invalid
            && key.player_count > 0u
            && key.private_hand_class_id != INVALID_ABSTRACTION_ID
            && key.public_board_abstraction_id != INVALID_ABSTRACTION_ID
            && key.chance_runout_class_id != INVALID_ABSTRACTION_ID
            && key.betting_history_abstraction_id != INVALID_ABSTRACTION_ID
            && key.stack_pot_abstraction_id != INVALID_ABSTRACTION_ID
            && key.legal_action_set_id != INVALID_ABSTRACTION_ID
            && key.subgame_root_context_id != INVALID_ABSTRACTION_ID;
    }

    [[nodiscard]] inline bool same_legal_actions(
        std::span<const uint16_t> lhs,
        std::span<const uint16_t> rhs) noexcept
    {
        return lhs.size() == rhs.size() && std::equal(lhs.begin(), lhs.end(), rhs.begin());
    }

    [[nodiscard]] inline std::expected<void, holdem_infoset_error> validate_legal_action_ids(
        const game_graph& graph,
        const holdem_infoset_description& description)
    {
        if (description.legal_action_ids.size() != graph.action_count(description.node_id)) {
            return std::unexpected(holdem_infoset_error{
                holdem_infoset_error_kind::legal_action_count_mismatch,
                description.node_id,
                graph.infoset_id[description.node_id]
            });
        }

        for (auto first = description.legal_action_ids.begin(); first != description.legal_action_ids.end(); ++first) {
            if (std::find(std::next(first), description.legal_action_ids.end(), *first)
                != description.legal_action_ids.end()) {
                return std::unexpected(holdem_infoset_error{
                    holdem_infoset_error_kind::duplicate_legal_action_id,
                    description.node_id,
                    graph.infoset_id[description.node_id]
                });
            }
        }

        return {};
    }

    [[nodiscard]] inline bool same_infoset_description(
        const holdem_infoset_description& lhs,
        const holdem_infoset_description& rhs) noexcept
    {
        return lhs.key == rhs.key
            && lhs.owner_id == rhs.owner_id
            && same_legal_actions(lhs.legal_action_ids, rhs.legal_action_ids);
    }

    /**
     * Lower per-node Hold'em infoset keys to dense table IDs.
     */
    template <std::size_t N>
    [[nodiscard]] std::expected<holdem_infoset_lowering, holdem_infoset_error> lower_holdem_infoset_keys(
        const game_graph& graph,
        std::span<const holdem_infoset_description> descriptions,
        const uint32_t owner_count)
    {
        if constexpr (N == 0 || N > static_cast<std::size_t>(INVALID_PLAYER)) {
            return std::unexpected(holdem_infoset_error{holdem_infoset_error_kind::player_count_mismatch});
        }

        std::vector<uint32_t> description_by_node(graph.node_count, game_graph::INVALID_NODE);
        for (uint32_t description_index = 0; description_index < descriptions.size(); ++description_index) {
            const auto& description = descriptions[description_index];
            if (description.node_id >= graph.node_count || !graph.is_player_node(description.node_id)) {
                return std::unexpected(holdem_infoset_error{
                    holdem_infoset_error_kind::invalid_player_node,
                    description.node_id
                });
            }
            if (description_by_node[description.node_id] != game_graph::INVALID_NODE) {
                return std::unexpected(holdem_infoset_error{
                    holdem_infoset_error_kind::duplicate_player_description,
                    description.node_id,
                    graph.infoset_id[description.node_id]
                });
            }
            description_by_node[description.node_id] = description_index;

            if (description.key.actor >= N) {
                return std::unexpected(holdem_infoset_error{
                    holdem_infoset_error_kind::invalid_actor,
                    description.node_id,
                    graph.infoset_id[description.node_id]
                });
            }
            if (description.key.player_count != N) {
                return std::unexpected(holdem_infoset_error{
                    holdem_infoset_error_kind::player_count_mismatch,
                    description.node_id,
                    graph.infoset_id[description.node_id]
                });
            }
            if (description.key.street == holdem_street::invalid) {
                return std::unexpected(holdem_infoset_error{
                    holdem_infoset_error_kind::invalid_street,
                    description.node_id,
                    graph.infoset_id[description.node_id]
                });
            }
            if (!valid_infoset_key(description.key)) {
                return std::unexpected(holdem_infoset_error{
                    holdem_infoset_error_kind::invalid_abstraction_id,
                    description.node_id,
                    graph.infoset_id[description.node_id]
                });
            }
            if (description.owner_id >= owner_count) {
                return std::unexpected(holdem_infoset_error{
                    holdem_infoset_error_kind::invalid_owner,
                    description.node_id,
                    graph.infoset_id[description.node_id]
                });
            }
            if (auto result = validate_legal_action_ids(graph, description); !result) {
                return std::unexpected(result.error());
            }
        }

        holdem_infoset_lowering lowering;
        lowering.dense_id_by_node.assign(graph.node_count, game_graph::INVALID_INFOSET);
        lowering.legal_action_offsets.push_back(0u);

        std::vector<uint32_t> first_description_by_graph_infoset(graph.infoset_count, game_graph::INVALID_NODE);
        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            if (!graph.is_player_node(node_id)) {
                continue;
            }

            const auto description_index = description_by_node[node_id];
            if (description_index == game_graph::INVALID_NODE) {
                return std::unexpected(holdem_infoset_error{
                    holdem_infoset_error_kind::missing_player_description,
                    node_id,
                    graph.infoset_id[node_id]
                });
            }

            const auto graph_infoset_id = graph.infoset_id[node_id];
            if (graph_infoset_id >= graph.infoset_count) {
                return std::unexpected(holdem_infoset_error{
                    holdem_infoset_error_kind::invalid_player_node,
                    node_id,
                    graph_infoset_id
                });
            }

            const auto& description = descriptions[description_index];
            auto& first_description_index = first_description_by_graph_infoset[graph_infoset_id];
            if (first_description_index == game_graph::INVALID_NODE) {
                first_description_index = description_index;
            } else if (!same_infoset_description(description, descriptions[first_description_index])) {
                return std::unexpected(holdem_infoset_error{
                    holdem_infoset_error_kind::inconsistent_shared_infoset,
                    node_id,
                    graph_infoset_id,
                    descriptions[first_description_index].node_id
                });
            }

            uint32_t dense_id = game_graph::INVALID_INFOSET;
            for (uint32_t existing = 0; existing < lowering.infoset_count(); ++existing) {
                if (lowering.key_by_infoset[existing] == description.key
                    && lowering.owner_by_infoset[existing] == description.owner_id
                    && same_legal_actions(lowering.legal_actions(existing), description.legal_action_ids)) {
                    dense_id = existing;
                    break;
                }
            }

            if (dense_id == game_graph::INVALID_INFOSET) {
                if (lowering.key_by_infoset.size() == std::numeric_limits<uint32_t>::max()) {
                    return std::unexpected(holdem_infoset_error{
                        holdem_infoset_error_kind::dense_id_overflow,
                        node_id,
                        graph_infoset_id
                    });
                }

                dense_id = lowering.infoset_count();
                lowering.key_by_infoset.push_back(description.key);
                lowering.owner_by_infoset.push_back(description.owner_id);
                lowering.legal_action_ids.insert(
                    lowering.legal_action_ids.end(),
                    description.legal_action_ids.begin(),
                    description.legal_action_ids.end());
                lowering.legal_action_offsets.push_back(static_cast<uint32_t>(lowering.legal_action_ids.size()));
                lowering.source_graph_infoset_by_infoset.push_back(graph_infoset_id);
            }

            lowering.dense_id_by_node[node_id] = dense_id;
        }

        return lowering;
    }

    /**
     * Validate lowered IDs against graph topology and shared infoset metadata.
     */
    [[nodiscard]] inline std::expected<void, holdem_infoset_error> validate_holdem_infoset_lowering(
        const game_graph& graph,
        const holdem_infoset_lowering& lowering)
    {
        if (lowering.dense_id_by_node.size() != graph.node_count
            || lowering.key_by_infoset.size() != lowering.owner_by_infoset.size()
            || lowering.key_by_infoset.size() != lowering.source_graph_infoset_by_infoset.size()
            || lowering.legal_action_offsets.size() != lowering.key_by_infoset.size() + 1u
            || lowering.legal_action_offsets.empty()
            || lowering.legal_action_offsets.front() != 0u
            || lowering.legal_action_offsets.back() != lowering.legal_action_ids.size()) {
            return std::unexpected(holdem_infoset_error{holdem_infoset_error_kind::side_array_size_mismatch});
        }

        for (uint32_t offset = 1; offset < lowering.legal_action_offsets.size(); ++offset) {
            if (lowering.legal_action_offsets[offset] < lowering.legal_action_offsets[offset - 1u]) {
                return std::unexpected(holdem_infoset_error{holdem_infoset_error_kind::invalid_lowering});
            }
        }

        std::vector<uint32_t> first_node_by_graph_infoset(graph.infoset_count, game_graph::INVALID_NODE);
        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            const auto dense_id = lowering.dense_id_by_node[node_id];
            if (!graph.is_player_node(node_id)) {
                if (dense_id != game_graph::INVALID_INFOSET) {
                    return std::unexpected(holdem_infoset_error{
                        holdem_infoset_error_kind::invalid_lowering,
                        node_id
                    });
                }
                continue;
            }

            if (dense_id >= lowering.infoset_count()) {
                return std::unexpected(holdem_infoset_error{
                    holdem_infoset_error_kind::missing_player_description,
                    node_id,
                    graph.infoset_id[node_id]
                });
            }
            if (lowering.action_count(dense_id) != graph.action_count(node_id)) {
                return std::unexpected(holdem_infoset_error{
                    holdem_infoset_error_kind::legal_action_count_mismatch,
                    node_id,
                    graph.infoset_id[node_id]
                });
            }

            const auto graph_infoset_id = graph.infoset_id[node_id];
            auto& first_node = first_node_by_graph_infoset[graph_infoset_id];
            if (first_node == game_graph::INVALID_NODE) {
                first_node = node_id;
            } else if (lowering.dense_id_by_node[first_node] != dense_id) {
                return std::unexpected(holdem_infoset_error{
                    holdem_infoset_error_kind::inconsistent_shared_infoset,
                    node_id,
                    graph_infoset_id,
                    first_node
                });
            }
        }

        return {};
    }

    enum class cfr_memory_plan_error_kind : uint8_t {
        table_layout_mismatch,
        node_limit_exceeded,
        infoset_limit_exceeded,
        action_value_limit_exceeded,
        total_byte_limit_exceeded,
        checkpoint_byte_limit_exceeded,
        estimate_overflow
    };

    struct cfr_memory_plan_error {
        cfr_memory_plan_error_kind kind{};
        uint64_t required = 0;
        uint64_t limit = 0;
    };

    [[nodiscard]] constexpr const char* to_string(const cfr_memory_plan_error_kind kind) noexcept
    {
        using enum cfr_memory_plan_error_kind;
        switch (kind) {
            case table_layout_mismatch:          return "cfr_memory_plan_error_kind::table_layout_mismatch";
            case node_limit_exceeded:           return "cfr_memory_plan_error_kind::node_limit_exceeded";
            case infoset_limit_exceeded:        return "cfr_memory_plan_error_kind::infoset_limit_exceeded";
            case action_value_limit_exceeded:   return "cfr_memory_plan_error_kind::action_value_limit_exceeded";
            case total_byte_limit_exceeded:     return "cfr_memory_plan_error_kind::total_byte_limit_exceeded";
            case checkpoint_byte_limit_exceeded: return "cfr_memory_plan_error_kind::checkpoint_byte_limit_exceeded";
            case estimate_overflow:             return "cfr_memory_plan_error_kind::estimate_overflow";
        }
        return "cfr_memory_plan_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const cfr_memory_plan_error_kind kind)
    {
        return os << to_string(kind);
    }

    struct cfr_memory_plan_limits {
        uint64_t max_nodes = std::numeric_limits<uint64_t>::max();
        uint64_t max_infosets = std::numeric_limits<uint64_t>::max();
        uint64_t max_action_values = std::numeric_limits<uint64_t>::max();
        uint64_t max_total_bytes = std::numeric_limits<uint64_t>::max();
        uint64_t max_checkpoint_bytes = std::numeric_limits<uint64_t>::max();
    };

    struct cfr_memory_plan_options {
        uint32_t worker_count = 1;
        uint32_t terminal_state_count = 0;
        uint32_t chance_event_count = 0;
        uint32_t bytes_per_terminal_state = 64;
        uint32_t bytes_per_chance_event = 32;
        numeric_policy numeric{};
    };

    struct cfr_memory_estimate {
        uint64_t node_bytes = 0;
        uint64_t infoset_bytes = 0;
        uint64_t action_values = 0;
        uint64_t regret_bytes = 0;
        uint64_t strategy_sum_bytes = 0;
        uint64_t owner_map_bytes = 0;
        uint64_t worker_delta_bytes = 0;
        uint64_t terminal_state_bytes = 0;
        uint64_t chance_event_bytes = 0;
        uint64_t scratch_bytes = 0;
        uint64_t checkpoint_bytes = 0;
        uint64_t total_bytes = 0;
    };

    [[nodiscard]] constexpr uint64_t table_storage_bytes(const table_storage_precision precision) noexcept
    {
        switch (precision) {
            case table_storage_precision::float32: return sizeof(float);
            case table_storage_precision::float64: return sizeof(double);
        }
        return sizeof(float);
    }

    [[nodiscard]] constexpr uint64_t accumulation_storage_bytes(const accumulation_precision precision) noexcept
    {
        switch (precision) {
            case accumulation_precision::float32: return sizeof(float);
            case accumulation_precision::float64: return sizeof(double);
        }
        return sizeof(float);
    }

    [[nodiscard]] inline bool checked_add(uint64_t& total, const uint64_t value) noexcept
    {
        if (value > std::numeric_limits<uint64_t>::max() - total) {
            return false;
        }
        total += value;
        return true;
    }

    [[nodiscard]] inline bool checked_mul(uint64_t lhs, uint64_t rhs, uint64_t& result) noexcept
    {
        if (lhs != 0u && rhs > std::numeric_limits<uint64_t>::max() / lhs) {
            return false;
        }
        result = lhs * rhs;
        return true;
    }

    [[nodiscard]] inline std::expected<cfr_memory_estimate, cfr_memory_plan_error> estimate_cfr_memory(
        const game_graph& graph,
        const action_table_layout& layout,
        const cfr_memory_plan_options options = {},
        const cfr_memory_plan_limits limits = {})
    {
        if (layout.infoset_count() != graph.infoset_count) {
            return std::unexpected(cfr_memory_plan_error{
                cfr_memory_plan_error_kind::table_layout_mismatch,
                layout.infoset_count(),
                graph.infoset_count
            });
        }

        cfr_memory_estimate estimate;
        estimate.action_values = layout.value_count();

        const auto worker_count = std::max(options.worker_count, 1u);
        const auto table_value_bytes = table_storage_bytes(options.numeric.table_storage);
        const auto accumulation_bytes = accumulation_storage_bytes(options.numeric.accumulation);

        uint64_t edge_bytes = 0;
        uint64_t node_table_bytes = 0;
        uint64_t owner_bytes = 0;
        uint64_t worker_entry_index_bytes = 0;
        uint64_t worker_value_bytes = 0;
        uint64_t terminal_bytes = 0;
        uint64_t chance_bytes = 0;
        uint64_t scratch_per_worker = 0;
        if (!checked_mul(graph.edges.size(), sizeof(edge), edge_bytes)
            || !checked_mul(graph.node_count, sizeof(uint32_t) * 3u + sizeof(node_kind) + sizeof(uint16_t), node_table_bytes)
            || !checked_mul(graph.infoset_count, sizeof(uint32_t), owner_bytes)
            || !checked_mul(graph.infoset_count, sizeof(uint32_t), worker_entry_index_bytes)
            || !checked_mul(estimate.action_values, accumulation_bytes * 2u, worker_value_bytes)
            || !checked_mul(options.terminal_state_count, options.bytes_per_terminal_state, terminal_bytes)
            || !checked_mul(options.chance_event_count, options.bytes_per_chance_event, chance_bytes)
            || !checked_mul(graph.node_count, sizeof(float), scratch_per_worker)) {
            return std::unexpected(cfr_memory_plan_error{cfr_memory_plan_error_kind::estimate_overflow});
        }

        estimate.node_bytes = sizeof(uint32_t) + edge_bytes + node_table_bytes;
        estimate.owner_map_bytes = owner_bytes;
        estimate.infoset_bytes = graph.infoset_count * sizeof(holdem_infoset_key)
            + owner_bytes
            + (static_cast<uint64_t>(graph.infoset_count) + 1u) * sizeof(uint32_t)
            + estimate.action_values * sizeof(uint16_t);
        estimate.regret_bytes = estimate.action_values * table_value_bytes;
        estimate.strategy_sum_bytes = estimate.action_values * table_value_bytes;
        estimate.worker_delta_bytes = static_cast<uint64_t>(worker_count)
            * (worker_entry_index_bytes + graph.infoset_count * sizeof(uint32_t) * 3u + worker_value_bytes);
        estimate.terminal_state_bytes = terminal_bytes;
        estimate.chance_event_bytes = chance_bytes;
        estimate.scratch_bytes = static_cast<uint64_t>(worker_count)
            * (scratch_per_worker + graph.edges.size() * sizeof(float) + (static_cast<uint64_t>(graph.max_depth) + 1u) * 32u);
        estimate.checkpoint_bytes = estimate.regret_bytes
            + estimate.strategy_sum_bytes
            + estimate.infoset_bytes
            + estimate.terminal_state_bytes
            + estimate.chance_event_bytes
            + 256u;

        if (!checked_add(estimate.total_bytes, estimate.node_bytes)
            || !checked_add(estimate.total_bytes, estimate.infoset_bytes)
            || !checked_add(estimate.total_bytes, estimate.regret_bytes)
            || !checked_add(estimate.total_bytes, estimate.strategy_sum_bytes)
            || !checked_add(estimate.total_bytes, estimate.worker_delta_bytes)
            || !checked_add(estimate.total_bytes, estimate.terminal_state_bytes)
            || !checked_add(estimate.total_bytes, estimate.chance_event_bytes)
            || !checked_add(estimate.total_bytes, estimate.scratch_bytes)
            || !checked_add(estimate.total_bytes, estimate.checkpoint_bytes)) {
            return std::unexpected(cfr_memory_plan_error{cfr_memory_plan_error_kind::estimate_overflow});
        }

        if (graph.node_count > limits.max_nodes) {
            return std::unexpected(cfr_memory_plan_error{
                cfr_memory_plan_error_kind::node_limit_exceeded,
                graph.node_count,
                limits.max_nodes
            });
        }
        if (graph.infoset_count > limits.max_infosets) {
            return std::unexpected(cfr_memory_plan_error{
                cfr_memory_plan_error_kind::infoset_limit_exceeded,
                graph.infoset_count,
                limits.max_infosets
            });
        }
        if (estimate.action_values > limits.max_action_values) {
            return std::unexpected(cfr_memory_plan_error{
                cfr_memory_plan_error_kind::action_value_limit_exceeded,
                estimate.action_values,
                limits.max_action_values
            });
        }
        if (estimate.total_bytes > limits.max_total_bytes) {
            return std::unexpected(cfr_memory_plan_error{
                cfr_memory_plan_error_kind::total_byte_limit_exceeded,
                estimate.total_bytes,
                limits.max_total_bytes
            });
        }
        if (estimate.checkpoint_bytes > limits.max_checkpoint_bytes) {
            return std::unexpected(cfr_memory_plan_error{
                cfr_memory_plan_error_kind::checkpoint_byte_limit_exceeded,
                estimate.checkpoint_bytes,
                limits.max_checkpoint_bytes
            });
        }

        return estimate;
    }

}
