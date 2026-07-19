#pragma once

#include "cfr/tables/table_layout.h"
#include <cassert>
#include <cstdint>
#include <span>
#include <vector>

namespace zeta::holdem::cfr {

    /**
     * Global CFR regret table using contiguous infoset-major storage.
     */
    struct regret_table {
        std::vector<float> regrets;           /**< Flat storage for regret values. */
        std::vector<uint32_t> action_offsets; /**< Infoset-major offsets; size infoset_count + 1. */

        regret_table() = default;

        explicit regret_table(const action_table_layout& layout) :
            regrets(layout.value_count(), 0.0f),
            action_offsets(layout.action_offsets)
        {
        }

        /**
         * Number of infosets represented by this table.
         */
        [[nodiscard]] uint32_t infoset_count() const noexcept
        {
            return table_infoset_count(action_offsets);
        }

        /**
         * Total number of action values in flat storage.
         */
        [[nodiscard]] uint32_t value_count() const noexcept
        {
            return table_value_count(action_offsets);
        }

        /**
         * Number of actions in one infoset.
         */
        [[nodiscard]] uint32_t action_count(const uint32_t infoset_id) const noexcept
        {
            return table_action_count(action_offsets, infoset_id);
        }

        /**
         * Flat storage offset for an infoset/action pair.
         */
        [[nodiscard]] uint32_t offset(const uint32_t infoset_id, const uint32_t action_index) const noexcept
        {
            return table_value_offset(action_offsets, infoset_id, action_index);
        }

        /**
         * Mutable regret span for an infoset.
         */
        [[nodiscard]] std::span<float> infoset_regrets(const uint32_t infoset_id) noexcept
        {
            const auto begin = action_offsets[infoset_id];
            const auto count = action_count(infoset_id);
            return count == 0u ? std::span<float>{} : std::span<float>{regrets.data() + begin, count};
        }

        /**
         * Read-only regret span for an infoset.
         */
        [[nodiscard]] std::span<const float> infoset_regrets(const uint32_t infoset_id) const noexcept
        {
            const auto begin = action_offsets[infoset_id];
            const auto count = action_count(infoset_id);
            return count == 0u ? std::span<const float>{} : std::span<const float>{regrets.data() + begin, count};
        }

        /**
         * Mutable indexed regret accessor.
         */
        [[nodiscard]] float& value(const uint32_t infoset_id, const uint32_t action_index) noexcept
        {
            const auto flat_offset = offset(infoset_id, action_index);
            assert(flat_offset < regrets.size());
            return regrets[flat_offset];
        }

        /**
         * Read-only indexed regret accessor.
         */
        [[nodiscard]] const float& value(const uint32_t infoset_id, const uint32_t action_index) const noexcept
        {
            const auto flat_offset = offset(infoset_id, action_index);
            assert(flat_offset < regrets.size());
            return regrets[flat_offset];
        }
    };

}
