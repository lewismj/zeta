#pragma once

#include "cfr/tables/delta_buffer.h"
#include "cfr/traversal/traversal.h"

#include <cassert>
#include <cstdint>
#include <expected>
#include <ostream>
#include <span>
#include <vector>

namespace zeta::holdem::cfr::solver {

    enum class iteration_error_kind : uint8_t {
        invalid_worker_id,
        duplicate_worker_id,
        missing_worker_context,
        table_layout_mismatch
    };

    struct iteration_error {
        iteration_error_kind kind{};
        uint32_t worker_id = 0;
    };

    [[nodiscard]] constexpr const char* to_string(const iteration_error_kind kind) noexcept
    {
        using enum iteration_error_kind;
        switch (kind) {
            case invalid_worker_id:       return "iteration_error_kind::invalid_worker_id";
            case duplicate_worker_id:     return "iteration_error_kind::duplicate_worker_id";
            case missing_worker_context:  return "iteration_error_kind::missing_worker_context";
            case table_layout_mismatch:   return "iteration_error_kind::table_layout_mismatch";
        }
        return "iteration_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const iteration_error_kind kind)
    {
        return os << to_string(kind);
    }

    /**
     * Stable worker-order plan used to merge local CFR deltas into global tables.
     */
    struct deterministic_reduction_plan {
        std::vector<uint32_t> worker_order;
    };

    /**
     * Build the default deterministic order: worker 0, worker 1, ...
     */
    [[nodiscard]] inline deterministic_reduction_plan make_deterministic_reduction_plan(const uint32_t worker_count)
    {
        deterministic_reduction_plan plan;
        plan.worker_order.reserve(worker_count);
        for (uint32_t worker_id = 0; worker_id < worker_count; ++worker_id) {
            plan.worker_order.push_back(worker_id);
        }
        return plan;
    }

    /**
     * Validate that a reduction plan is a permutation of the worker array.
     */
    [[nodiscard]] inline std::expected<void, iteration_error> validate_reduction_plan(
        const deterministic_reduction_plan& plan,
        const uint32_t worker_count)
    {
        if (plan.worker_order.size() != worker_count) {
            return std::unexpected(iteration_error{iteration_error_kind::invalid_worker_id, worker_count});
        }

        std::vector<uint8_t> seen(worker_count, 0);
        for (const auto worker_id : plan.worker_order) {
            if (worker_id >= worker_count) {
                return std::unexpected(iteration_error{iteration_error_kind::invalid_worker_id, worker_id});
            }
            if (seen[worker_id] != 0u) {
                return std::unexpected(iteration_error{iteration_error_kind::duplicate_worker_id, worker_id});
            }
            seen[worker_id] = 1u;
        }

        return {};
    }

    /**
     * Merge worker-local deltas into global tables in the exact order specified by the plan.
     */
    [[nodiscard]] inline std::expected<void, iteration_error> apply_worker_reductions(
        regret_table& regrets,
        strategy_sum_table& strategy_sums,
        const deterministic_reduction_plan& plan,
        std::span<const traversal::worker_context* const> workers)
    {
        if (auto result = validate_reduction_plan(plan, static_cast<uint32_t>(workers.size())); !result) {
            return std::unexpected(result.error());
        }
        if (!same_action_offsets(regrets.action_offsets, strategy_sums.action_offsets)) {
            return std::unexpected(iteration_error{iteration_error_kind::table_layout_mismatch});
        }

        for (const auto worker_id : plan.worker_order) {
            const auto* worker = workers[worker_id];
            if (worker == nullptr) {
                return std::unexpected(iteration_error{iteration_error_kind::missing_worker_context, worker_id});
            }
            if (!same_action_offsets(regrets.action_offsets, worker->delta_buffer.action_offsets())) {
                return std::unexpected(iteration_error{iteration_error_kind::table_layout_mismatch, worker_id});
            }

            apply_delta_buffer(regrets, strategy_sums, worker->delta_buffer);
        }

        return {};
    }

    /**
     * Merge contiguous worker contexts by ascending worker index.
     */
    [[nodiscard]] inline std::expected<void, iteration_error> apply_worker_reductions(
        regret_table& regrets,
        strategy_sum_table& strategy_sums,
        std::span<const traversal::worker_context> workers)
    {
        const auto plan = make_deterministic_reduction_plan(static_cast<uint32_t>(workers.size()));

        std::vector<const traversal::worker_context*> worker_ptrs;
        worker_ptrs.reserve(workers.size());
        for (const auto& worker : workers) {
            worker_ptrs.push_back(&worker);
        }

        return apply_worker_reductions(regrets, strategy_sums, plan, std::span<const traversal::worker_context* const>{worker_ptrs});
    }

}
