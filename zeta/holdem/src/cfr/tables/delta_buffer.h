#pragma once

#include "cfr/tables/regret_table.h"
#include "cfr/tables/strategy_table.h"
#include <cassert>
#include <cstdint>
#include <expected>
#include <limits>
#include <span>
#include <vector>

namespace zeta::holdem::cfr {

    struct table_delta_entry {
        uint32_t infoset_id;  /**< Infoset that owns this sparse delta slice. */
        uint32_t begin;       /**< Inclusive offset into delta arrays. */
        uint32_t end;         /**< Exclusive offset into delta arrays. */
    };

    /**
     * Thread-local sparse delta buffer for regret and strategy updates.
     *
     * The buffer stores only infosets touched by the owning worker. The
     * entry_by_infoset index is intentionally small metadata, not duplicated
     * full regret/strategy storage.
     */
    struct alignas(64) table_delta_buffer {
        static constexpr uint32_t INVALID_ENTRY = std::numeric_limits<uint32_t>::max();

        table_delta_buffer() = default;

        explicit table_delta_buffer(std::span<const uint32_t> action_offsets)
        {
            [[maybe_unused]] const auto result = reset_layout(action_offsets);
            assert(result.has_value());
        }

        /**
         * Replace the layout used by this buffer and discard pending deltas.
         */
        [[nodiscard]] std::expected<void, table_layout_error> reset_layout(
            std::span<const uint32_t> action_offsets)
        {
            if (auto result = validate_action_offsets(action_offsets); !result) {
                return result;
            }

            entries_.clear();
            regret_deltas_.clear();
            strategy_deltas_.clear();
            action_offsets_.assign(action_offsets.begin(), action_offsets.end());
            entry_by_infoset_.assign(table_infoset_count(action_offsets_), INVALID_ENTRY);
            return {};
        }

        /**
         * Clear all accumulated deltas while preserving layout capacity.
         */
        void clear() noexcept
        {
            for (const auto& entry : entries_) {
                entry_by_infoset_[entry.infoset_id] = INVALID_ENTRY;
            }
            entries_.clear();
            regret_deltas_.clear();
            strategy_deltas_.clear();
        }

        /**
         * Number of infosets represented by this buffer's layout.
         */
        [[nodiscard]] uint32_t infoset_count() const noexcept
        {
            return table_infoset_count(action_offsets_);
        }

        /**
         * Number of sparse entries currently holding deltas.
         */
        [[nodiscard]] uint32_t entry_count() const noexcept
        {
            return static_cast<uint32_t>(entries_.size());
        }

        /**
         * Raw action-offset layout used by this buffer.
         */
        [[nodiscard]] std::span<const uint32_t> action_offsets() const noexcept
        {
            return action_offsets_;
        }

        /**
         * Sparse delta entries in deterministic insertion order.
         */
        [[nodiscard]] std::span<const table_delta_entry> entries() const noexcept
        {
            return entries_;
        }

        /**
         * Mutable regret-delta span for an infoset, creating it if needed.
         */
        [[nodiscard]] std::span<float> regret_deltas(const uint32_t infoset_id)
        {
            auto& entry = ensure_entry_(infoset_id);
            return mutable_span_(regret_deltas_, entry);
        }

        /**
         * Mutable strategy-delta span for an infoset, creating it if needed.
         */
        [[nodiscard]] std::span<float> strategy_deltas(const uint32_t infoset_id)
        {
            auto& entry = ensure_entry_(infoset_id);
            return mutable_span_(strategy_deltas_, entry);
        }

        /**
         * Read-only regret-delta span for a sparse entry.
         */
        [[nodiscard]] std::span<const float> regret_deltas_for(const table_delta_entry& entry) const noexcept
        {
            return const_span_(regret_deltas_, entry);
        }

        /**
         * Read-only strategy-delta span for a sparse entry.
         */
        [[nodiscard]] std::span<const float> strategy_deltas_for(const table_delta_entry& entry) const noexcept
        {
            return const_span_(strategy_deltas_, entry);
        }

        /**
         * Add a regret delta to one indexed action.
         */
        void add_regret_delta(
            const uint32_t infoset_id,
            const uint32_t action_index,
            const float delta)
        {
            auto& entry = ensure_entry_(infoset_id);
            assert(action_index < entry.end - entry.begin);
            regret_deltas_[entry.begin + action_index] += delta;
        }

        /**
         * Add a strategy-sum delta to one indexed action.
         */
        void add_strategy_delta(
            const uint32_t infoset_id,
            const uint32_t action_index,
            const float delta)
        {
            auto& entry = ensure_entry_(infoset_id);
            assert(action_index < entry.end - entry.begin);
            strategy_deltas_[entry.begin + action_index] += delta;
        }

    private:
        std::vector<uint32_t> action_offsets_;
        std::vector<uint32_t> entry_by_infoset_;
        std::vector<table_delta_entry> entries_;
        std::vector<float> regret_deltas_;
        std::vector<float> strategy_deltas_;

        [[nodiscard]] table_delta_entry& ensure_entry_(const uint32_t infoset_id)
        {
            assert(infoset_id < infoset_count());
            auto& entry_index = entry_by_infoset_[infoset_id];
            if (entry_index != INVALID_ENTRY) {
                return entries_[entry_index];
            }

            assert(entries_.size() < std::numeric_limits<uint32_t>::max());
            assert(regret_deltas_.size() < std::numeric_limits<uint32_t>::max());

            const auto begin = static_cast<uint32_t>(regret_deltas_.size());
            const auto count = table_action_count(action_offsets_, infoset_id);
            const auto end = begin + count;

            regret_deltas_.resize(end, 0.0f);
            strategy_deltas_.resize(end, 0.0f);

            entry_index = static_cast<uint32_t>(entries_.size());
            entries_.push_back(table_delta_entry{infoset_id, begin, end});
            return entries_.back();
        }

        [[nodiscard]] static std::span<float> mutable_span_(
            std::vector<float>& values,
            const table_delta_entry& entry) noexcept
        {
            const auto count = entry.end - entry.begin;
            return count == 0u ? std::span<float>{} : std::span<float>{values.data() + entry.begin, count};
        }

        [[nodiscard]] static std::span<const float> const_span_(
            const std::vector<float>& values,
            const table_delta_entry& entry) noexcept
        {
            const auto count = entry.end - entry.begin;
            return count == 0u ? std::span<const float>{} : std::span<const float>{values.data() + entry.begin, count};
        }
    };

    /**
     * Apply regret deltas to a global regret table in buffer insertion order.
     */
    inline void apply_regret_deltas(regret_table& table, const table_delta_buffer& buffer) noexcept
    {
        assert(same_action_offsets(table.action_offsets, buffer.action_offsets()));

        for (const auto& entry : buffer.entries()) {
            const auto global_begin = table.action_offsets[entry.infoset_id];
            const auto deltas = buffer.regret_deltas_for(entry);
            for (uint32_t action_index = 0; action_index < deltas.size(); ++action_index) {
                table.regrets[global_begin + action_index] += deltas[action_index];
            }
        }
    }

    /**
     * Apply strategy deltas to a global strategy-sum table in buffer insertion order.
     */
    inline void apply_strategy_deltas(strategy_sum_table& table, const table_delta_buffer& buffer) noexcept
    {
        assert(same_action_offsets(table.action_offsets, buffer.action_offsets()));

        for (const auto& entry : buffer.entries()) {
            const auto global_begin = table.action_offsets[entry.infoset_id];
            const auto deltas = buffer.strategy_deltas_for(entry);
            for (uint32_t action_index = 0; action_index < deltas.size(); ++action_index) {
                table.sums[global_begin + action_index] += deltas[action_index];
            }
        }
    }

    /**
     * Apply both regret and strategy deltas to global tables.
     */
    inline void apply_delta_buffer(
        regret_table& regrets,
        strategy_sum_table& strategy_sums,
        const table_delta_buffer& buffer) noexcept
    {
        apply_regret_deltas(regrets, buffer);
        apply_strategy_deltas(strategy_sums, buffer);
    }

}
