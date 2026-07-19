#pragma once

#include "cfr/graph/graph.h"
#include <algorithm>
#include <cassert>
#include <cstdint>
#include <expected>
#include <limits>
#include <ostream>
#include <span>
#include <vector>

namespace zeta::holdem::cfr {

    enum class table_layout_error_kind : uint8_t {
        invalid_action_offsets,
        offset_overflow,
        invalid_infoset,
        inconsistent_infoset_action_count
    };

    struct table_layout_error {
        table_layout_error_kind kind{};
        uint32_t infoset_id = 0;
        uint32_t related_node_id = 0;
    };

    [[nodiscard]] constexpr const char* to_string(const table_layout_error_kind kind) noexcept
    {
        using enum table_layout_error_kind;
        switch (kind) {
            case invalid_action_offsets:           return "table_layout_error_kind::invalid_action_offsets";
            case offset_overflow:                  return "table_layout_error_kind::offset_overflow";
            case invalid_infoset:                  return "table_layout_error_kind::invalid_infoset";
            case inconsistent_infoset_action_count: return "table_layout_error_kind::inconsistent_infoset_action_count";
        }
        return "table_layout_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const table_layout_error_kind kind)
    {
        return os << to_string(kind);
    }

    /**
     * Contiguous action-addressing layout shared by regret and strategy tables.
     *
     * Offsets are infoset-major:
     *   - action_offsets[infoset] is the first action value for an infoset.
     *   - action_offsets[infoset + 1] is one-past-the-last action value.
     */
    struct action_table_layout {
        std::vector<uint32_t> action_offsets;  /**< Size: infoset_count + 1. */

        /**
         * Number of infosets represented by this layout.
         */
        [[nodiscard]] uint32_t infoset_count() const noexcept
        {
            return action_offsets.empty() ? 0u : static_cast<uint32_t>(action_offsets.size() - 1u);
        }

        /**
         * Total number of action slots across all infosets.
         */
        [[nodiscard]] uint32_t value_count() const noexcept
        {
            return action_offsets.empty() ? 0u : action_offsets.back();
        }

        /**
         * Number of actions in a single infoset.
         */
        [[nodiscard]] uint32_t action_count(const uint32_t infoset_id) const noexcept
        {
            assert(infoset_id < infoset_count());
            return action_offsets[infoset_id + 1u] - action_offsets[infoset_id];
        }

        /**
         * Flat value offset for an infoset/action pair.
         */
        [[nodiscard]] uint32_t offset(const uint32_t infoset_id, const uint32_t action_index) const noexcept
        {
            assert(action_index < action_count(infoset_id));
            return action_offsets[infoset_id] + action_index;
        }
    };

    /**
     * Number of infosets represented by raw action offsets.
     */
    [[nodiscard]] inline uint32_t table_infoset_count(std::span<const uint32_t> action_offsets) noexcept
    {
        return action_offsets.empty() ? 0u : static_cast<uint32_t>(action_offsets.size() - 1u);
    }

    /**
     * Total number of table values represented by raw action offsets.
     */
    [[nodiscard]] inline uint32_t table_value_count(std::span<const uint32_t> action_offsets) noexcept
    {
        return action_offsets.empty() ? 0u : action_offsets.back();
    }

    /**
     * Number of actions in a single infoset for raw action offsets.
     */
    [[nodiscard]] inline uint32_t table_action_count(
        std::span<const uint32_t> action_offsets,
        const uint32_t infoset_id) noexcept
    {
        assert(infoset_id < table_infoset_count(action_offsets));
        return action_offsets[infoset_id + 1u] - action_offsets[infoset_id];
    }

    /**
     * Flat value offset for raw action offsets.
     */
    [[nodiscard]] inline uint32_t table_value_offset(
        std::span<const uint32_t> action_offsets,
        const uint32_t infoset_id,
        const uint32_t action_index) noexcept
    {
        assert(action_index < table_action_count(action_offsets, infoset_id));
        return action_offsets[infoset_id] + action_index;
    }

    /**
     * Compare two table layouts by raw offsets.
     */
    [[nodiscard]] inline bool same_action_offsets(
        std::span<const uint32_t> lhs,
        std::span<const uint32_t> rhs) noexcept
    {
        return lhs.size() == rhs.size() && std::equal(lhs.begin(), lhs.end(), rhs.begin());
    }

    /**
     * Validate monotonic action offsets.
     */
    [[nodiscard]] inline std::expected<void, table_layout_error> validate_action_offsets(
        std::span<const uint32_t> action_offsets) noexcept
    {
        if (action_offsets.empty() || action_offsets.front() != 0u) {
            return std::unexpected(table_layout_error{table_layout_error_kind::invalid_action_offsets});
        }

        for (uint32_t i = 1; i < static_cast<uint32_t>(action_offsets.size()); ++i) {
            if (action_offsets[i] < action_offsets[i - 1u]) {
                return std::unexpected(table_layout_error{table_layout_error_kind::invalid_action_offsets, i});
            }
        }

        return {};
    }

    /**
     * Build a contiguous layout from per-infoset action counts.
     */
    [[nodiscard]] inline std::expected<action_table_layout, table_layout_error> make_action_table_layout(
        std::span<const uint32_t> action_counts)
    {
        action_table_layout layout;
        layout.action_offsets.reserve(action_counts.size() + 1u);
        layout.action_offsets.push_back(0u);

        uint64_t running_offset = 0;
        for (uint32_t infoset_id = 0; infoset_id < static_cast<uint32_t>(action_counts.size()); ++infoset_id) {
            running_offset += action_counts[infoset_id];
            if (running_offset > std::numeric_limits<uint32_t>::max()) {
                return std::unexpected(table_layout_error{
                    table_layout_error_kind::offset_overflow,
                    infoset_id
                });
            }
            layout.action_offsets.push_back(static_cast<uint32_t>(running_offset));
        }

        return layout;
    }

    /**
     * Build a contiguous table layout from player-node infosets in a graph.
     *
     * Repeated nodes in the same infoset must expose the same action count.
     */
    [[nodiscard]] inline std::expected<action_table_layout, table_layout_error> make_action_table_layout(
        const game_graph& graph)
    {
        constexpr uint32_t uninitialized_action_count = std::numeric_limits<uint32_t>::max();

        std::vector<uint32_t> action_counts(graph.infoset_count, uninitialized_action_count);

        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            if (!graph.is_player_node(node_id)) {
                continue;
            }

            const auto infoset_id = graph.infoset_id[node_id];
            if (infoset_id == game_graph::INVALID_INFOSET || infoset_id >= graph.infoset_count) {
                return std::unexpected(table_layout_error{
                    table_layout_error_kind::invalid_infoset,
                    infoset_id,
                    node_id
                });
            }

            const auto count = graph.action_count(node_id);
            auto& existing_count = action_counts[infoset_id];
            if (existing_count == uninitialized_action_count) {
                existing_count = count;
            } else if (existing_count != count) {
                return std::unexpected(table_layout_error{
                    table_layout_error_kind::inconsistent_infoset_action_count,
                    infoset_id,
                    node_id
                });
            }
        }

        for (auto& count : action_counts) {
            if (count == uninitialized_action_count) {
                count = 0u;
            }
        }

        return make_action_table_layout(action_counts);
    }

}
