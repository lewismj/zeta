#pragma once

#include "cfr/scheduler/dfs_partitioner.h"
#include "cfr/scheduler/scheduler.h"
#include "cfr/solver/metadata.h"
#include "cfr/tables/delta_buffer.h"
#include "cfr/traversal/traversal.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <expected>
#include <functional>
#include <istream>
#include <limits>
#include <numeric>
#include <ostream>
#include <span>
#include <type_traits>
#include <vector>

namespace zeta::holdem::cfr::solver {

    enum class cfr_variant : uint8_t {
        vanilla,
        cfr_plus,
        linear_cfr,
        discounted_cfr
    };

    enum class cfr_update_mode : uint8_t {
        alternating,
        simultaneous
    };

    struct iteration_config {
        cfr_variant variant = cfr_variant::vanilla;
        cfr_update_mode update_mode = cfr_update_mode::alternating;
        uint64_t iteration = 0;
        uint8_t updating_player = 0;
        float strategy_weight = 1.0f;
    };

    /**
     * Reduction locality and owner-routing counters.
     */
    struct reduction_diagnostics {
        uint64_t remote_delta_count = 0;
        uint64_t remote_delta_bytes = 0;
        uint64_t reduction_entries = 0;
        uint64_t reduction_values = 0;
        uint64_t cfr_plus_clipped_values = 0;
        std::vector<uint64_t> owner_hit_distribution;
        std::vector<uint64_t> owner_remote_hit_distribution;
        std::vector<uint64_t> per_owner_touched_values;
        std::vector<uint64_t> per_owner_reduction_time_ns;
    };

    struct infoset_diagnostic_location {
        uint32_t infoset_id = game_graph::INVALID_INFOSET;
        uint32_t begin_action = 0;
        uint32_t end_action = 0;
        uint32_t action_index = 0;
    };

    /**
     * Table-wide quality metrics derived from regret and average-strategy accumulators.
     */
    struct quality_diagnostics {
        double exploitability_estimate = 0.0;
        double average_strategy_mass = 0.0;
        double regret_norm = 0.0;
        float max_regret = 0.0f;
        uint32_t max_regret_infoset_id = game_graph::INVALID_INFOSET;
        double mean_regret = 0.0;
        uint64_t positive_regret_count = 0;
        double largest_strategy_entropy_drop = 0.0;
        double largest_strategy_change = 0.0;
        infoset_diagnostic_location max_regret_location{};
        infoset_diagnostic_location largest_strategy_change_location{};
        std::vector<double> strategy_sum_mass_by_player;
    };

    struct iteration_result {
        float root_utility = 0.0f;
        traversal::traversal_diagnostics diagnostics{};
        reduction_diagnostics reduction{};
        quality_diagnostics quality{};
        std::vector<uint64_t> per_worker_traversal_time_ns;
        uint64_t reduction_time_ns = 0;
        uint64_t scheduler_tasks_executed = 0;
        uint64_t scheduler_task_count = 0;
        uint64_t scheduler_estimated_work = 0;
        uint32_t traversals_run = 0;
        uint32_t workers_used = 0;
    };

    enum class iteration_error_kind : uint8_t {
        invalid_context,
        invalid_update_player,
        unsupported_update_mode,
        invalid_worker_id,
        duplicate_worker_id,
        missing_worker_context,
        table_layout_mismatch,
        graph_metadata,
        chance_table,
        scheduler,
        traversal,
        checkpoint
    };

    struct iteration_error {
        iteration_error_kind kind{};
        uint32_t worker_id = 0;
        solver_graph_metadata_error graph_metadata{};
        chance_table_error chance_table{};
        scheduler::scheduler_error scheduler{};
        traversal::traversal_error traversal{};
    };

    [[nodiscard]] constexpr const char* to_string(const iteration_error_kind kind) noexcept
    {
        using enum iteration_error_kind;
        switch (kind) {
            case invalid_context:       return "iteration_error_kind::invalid_context";
            case invalid_update_player: return "iteration_error_kind::invalid_update_player";
            case unsupported_update_mode: return "iteration_error_kind::unsupported_update_mode";
            case invalid_worker_id:       return "iteration_error_kind::invalid_worker_id";
            case duplicate_worker_id:     return "iteration_error_kind::duplicate_worker_id";
            case missing_worker_context:  return "iteration_error_kind::missing_worker_context";
            case table_layout_mismatch:   return "iteration_error_kind::table_layout_mismatch";
            case graph_metadata:          return "iteration_error_kind::graph_metadata";
            case chance_table:            return "iteration_error_kind::chance_table";
            case scheduler:               return "iteration_error_kind::scheduler";
            case traversal:               return "iteration_error_kind::traversal";
            case checkpoint:              return "iteration_error_kind::checkpoint";
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
     * Contiguous infoset interval owned by one reduction owner.
     */
    struct infoset_owner_range {
        uint32_t owner_worker = 0;
        uint32_t begin_infoset = 0;
        uint32_t end_infoset = 0;

        /**
         * Return whether this half-open range owns the infoset.
         */
        [[nodiscard]] bool contains(const uint32_t infoset_id) const noexcept
        {
            return begin_infoset <= infoset_id && infoset_id < end_infoset;
        }
    };

    /**
     * Dense lookup table backed by contiguous owner ranges.
     */
    struct infoset_owner_map {
        uint32_t owner_count = 0;
        std::vector<infoset_owner_range> ranges;
        std::vector<uint32_t> owner_by_infoset;

        /**
         * Number of infosets covered by the map.
         */
        [[nodiscard]] uint32_t infoset_count() const noexcept
        {
            return static_cast<uint32_t>(owner_by_infoset.size());
        }

        /**
         * Owner for an infoset in O(1) after range validation.
         */
        [[nodiscard]] uint32_t owner_for_infoset(const uint32_t infoset_id) const noexcept
        {
            assert(infoset_id < owner_by_infoset.size());
            return owner_by_infoset[infoset_id];
        }
    };

    /**
     * Non-owning table slice for one infoset-owner range.
     */
    struct table_shard_view {
        regret_table* regrets = nullptr;
        strategy_sum_table* strategy_sums = nullptr;
        uint32_t owner_worker = 0;
        uint32_t begin_infoset = 0;
        uint32_t end_infoset = 0;
        uint32_t begin_value = 0;
        uint32_t end_value = 0;

        /**
         * Return whether this view covers the infoset.
         */
        [[nodiscard]] bool contains_infoset(const uint32_t infoset_id) const noexcept
        {
            return begin_infoset <= infoset_id && infoset_id < end_infoset;
        }
    };

    enum class cfr_terminal_provider_kind : uint8_t {
        none,
        terminal_state_table,
        fixed_utility_fixture
    };

    /**
     * Terminal leaf metadata consumed by production CFR iteration.
     */
    struct cfr_terminal_leaf {
        uint32_t terminal_state_id = INVALID_METADATA_ID;
    };

    /**
     * Non-owning terminal evaluator inputs for CFR leaf evaluation.
     */
    template <std::size_t N>
    struct cfr_terminal_provider {
        cfr_terminal_provider_kind kind = cfr_terminal_provider_kind::none;
        const ::zeta::holdem::river_terminal_cache* river_cache = nullptr;
        const std::array<::zeta::holdem::river_reach_index, N>* reach_indices = nullptr;
        std::span<const cfr_terminal_leaf> terminal_leaves{};
        std::span<const ::zeta::holdem::terminal_state<N>> terminal_states{};
        std::array<::zeta::holdem::combination_index, N> combo_by_player{};
        std::span<const float> fixed_terminal_utility_by_node{};
        uint16_t samples_per_combo = 64;
    };

    /**
     * Build a production terminal-state provider.
     */
    template <std::size_t N>
    [[nodiscard]] cfr_terminal_provider<N> make_terminal_state_provider(
        const ::zeta::holdem::river_terminal_cache& river_cache,
        const std::array<::zeta::holdem::river_reach_index, N>& reach_indices,
        const std::span<const cfr_terminal_leaf> terminal_leaves,
        const std::span<const ::zeta::holdem::terminal_state<N>> terminal_states,
        const std::array<::zeta::holdem::combination_index, N>& combo_by_player,
        const uint16_t samples_per_combo = 64) noexcept
    {
        return cfr_terminal_provider<N>{
            .kind = cfr_terminal_provider_kind::terminal_state_table,
            .river_cache = &river_cache,
            .reach_indices = &reach_indices,
            .terminal_leaves = terminal_leaves,
            .terminal_states = terminal_states,
            .combo_by_player = combo_by_player,
            .samples_per_combo = samples_per_combo
        };
    }

    /**
     * Build an explicit fixed-utility fixture for reference tests and benchmarks.
     */
    template <std::size_t N>
    [[nodiscard]] cfr_terminal_provider<N> make_fixed_terminal_provider(
        const std::span<const float> terminal_utility_by_node) noexcept
    {
        return cfr_terminal_provider<N>{
            .kind = cfr_terminal_provider_kind::fixed_utility_fixture,
            .fixed_terminal_utility_by_node = terminal_utility_by_node
        };
    }

    /**
     * Non-owning solver inputs shared by the public CFR iteration entry point.
     */
    template <std::size_t N>
    struct cfr_solver_context {
        static constexpr std::size_t player_count = N;

        game_graph* graph = nullptr;
        solver_graph_annotations* graph_annotations = nullptr;
        action_table_layout* layout = nullptr;
        regret_table* regrets = nullptr;
        strategy_sum_table* strategy_sums = nullptr;
        const chance_event_table* chance_events = nullptr;
        cfr_terminal_provider<N> terminal_provider{};
        numeric_policy numeric{};
        reduction_policy reduction{};
        const infoset_owner_map* owner_map = nullptr;
        chance_mode chance = chance_mode::enumerate;
    };

    /**
     * Build a non-owning CFR solver context from existing graph, metadata, and table storage.
     */
    template <std::size_t N>
    [[nodiscard]] cfr_solver_context<N> make_cfr_solver_context(
        game_graph& graph,
        solver_graph_annotations& annotations,
        action_table_layout& layout,
        regret_table& regrets,
        strategy_sum_table& strategy_sums) noexcept
    {
        return cfr_solver_context<N>{
            .graph = &graph,
            .graph_annotations = &annotations,
            .layout = &layout,
            .regrets = &regrets,
            .strategy_sums = &strategy_sums
        };
    }

    enum class checkpoint_error_kind : uint8_t {
        stream_write_failed,
        stream_read_failed,
        invalid_magic,
        unsupported_version,
        incompatible_endianness,
        incompatible_player_count,
        incompatible_variant,
        incompatible_numeric_policy,
        incompatible_reduction_policy,
        incompatible_chance_mode,
        incompatible_graph_metadata,
        incompatible_action_layout,
        incompatible_owner_ranges,
        incompatible_table_size
    };

    struct checkpoint_error {
        checkpoint_error_kind kind{};
    };

    [[nodiscard]] constexpr const char* to_string(const checkpoint_error_kind kind) noexcept
    {
        using enum checkpoint_error_kind;
        switch (kind) {
            case stream_write_failed:           return "checkpoint_error_kind::stream_write_failed";
            case stream_read_failed:            return "checkpoint_error_kind::stream_read_failed";
            case invalid_magic:                 return "checkpoint_error_kind::invalid_magic";
            case unsupported_version:           return "checkpoint_error_kind::unsupported_version";
            case incompatible_endianness:       return "checkpoint_error_kind::incompatible_endianness";
            case incompatible_player_count:     return "checkpoint_error_kind::incompatible_player_count";
            case incompatible_variant:          return "checkpoint_error_kind::incompatible_variant";
            case incompatible_numeric_policy:   return "checkpoint_error_kind::incompatible_numeric_policy";
            case incompatible_reduction_policy: return "checkpoint_error_kind::incompatible_reduction_policy";
            case incompatible_chance_mode:      return "checkpoint_error_kind::incompatible_chance_mode";
            case incompatible_graph_metadata:   return "checkpoint_error_kind::incompatible_graph_metadata";
            case incompatible_action_layout:    return "checkpoint_error_kind::incompatible_action_layout";
            case incompatible_owner_ranges:     return "checkpoint_error_kind::incompatible_owner_ranges";
            case incompatible_table_size:       return "checkpoint_error_kind::incompatible_table_size";
        }
        return "checkpoint_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const checkpoint_error_kind kind)
    {
        return os << to_string(kind);
    }

    struct cfr_checkpoint_header {
        uint64_t magic = 0x5a45544143465231ull;
        uint32_t version = 1;
        uint32_t endian_marker = 0x01020304u;
        uint32_t player_count = 0;
        uint32_t infoset_count = 0;
        uint32_t value_count = 0;
        uint64_t iteration = 0;
        cfr_variant variant = cfr_variant::vanilla;
        numeric_policy numeric{};
        reduction_policy reduction{};
        chance_mode chance = chance_mode::enumerate;
        solver_compatibility_key compatibility{};
        uint64_t owner_range_hash = compatibility_hasher::OFFSET;
        uint64_t terminal_state_layout_hash = compatibility_hasher::OFFSET;
        uint64_t rng_stream_policy_hash = compatibility_hasher::OFFSET;
    };

    struct cfr_checkpoint_resume {
        cfr_checkpoint_header header{};
    };

    /**
     * Regret-matching policy used by vanilla CFR.
     */
    struct vanilla_regret_matching_policy {
        [[nodiscard]] static float positive_regret(const float regret) noexcept
        {
            return std::max(regret, 0.0f);
        }
    };

    /**
     * Regret-matching policy used while traversing CFR+.
     */
    struct cfr_plus_regret_matching_policy {
        [[nodiscard]] static float positive_regret(const float regret) noexcept
        {
            return std::max(regret, 0.0f);
        }
    };

    template <typename StrategyPolicy>
    inline void compute_regret_matching_strategy(
        const std::span<const float> regrets,
        const std::span<const edge> edges,
        const std::span<float> edge_probabilities)
    {
        float positive_sum = 0.0f;
        for (uint32_t i = 0; i < edges.size(); ++i) {
            const auto action_index = edges[i].action_index;
            positive_sum += StrategyPolicy::positive_regret(regrets[action_index]);
        }

        const auto uniform = edges.empty() ? 0.0f : 1.0f / static_cast<float>(edges.size());
        for (uint32_t i = 0; i < edges.size(); ++i) {
            const auto action_index = edges[i].action_index;
            edge_probabilities[i] = positive_sum > 0.0f
                ? StrategyPolicy::positive_regret(regrets[action_index]) / positive_sum
                : uniform;
        }
    }

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
     * Build a contiguous, nearly-even infoset owner map.
     */
    [[nodiscard]] inline std::expected<infoset_owner_map, iteration_error> make_even_infoset_owner_map(
        const uint32_t infoset_count,
        const uint32_t owner_count)
    {
        if (owner_count == 0u) {
            return std::unexpected(iteration_error{iteration_error_kind::invalid_worker_id});
        }

        infoset_owner_map map;
        map.owner_count = owner_count;
        map.ranges.reserve(owner_count);
        map.owner_by_infoset.assign(infoset_count, 0u);

        const auto base = infoset_count / owner_count;
        const auto remainder = infoset_count % owner_count;
        uint32_t begin_infoset = 0;
        for (uint32_t owner = 0; owner < owner_count; ++owner) {
            const auto count = base + (owner < remainder ? 1u : 0u);
            const auto end_infoset = begin_infoset + count;
            map.ranges.push_back(infoset_owner_range{owner, begin_infoset, end_infoset});
            for (uint32_t infoset_id = begin_infoset; infoset_id < end_infoset; ++infoset_id) {
                map.owner_by_infoset[infoset_id] = owner;
            }
            begin_infoset = end_infoset;
        }

        return map;
    }

    /**
     * Build a contiguous, nearly-even owner map for a table layout.
     */
    [[nodiscard]] inline std::expected<infoset_owner_map, iteration_error> make_even_infoset_owner_map(
        const action_table_layout& layout,
        const uint32_t owner_count)
    {
        return make_even_infoset_owner_map(layout.infoset_count(), owner_count);
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
     * Validate that owner ranges cover the table infosets contiguously.
     */
    [[nodiscard]] inline std::expected<void, iteration_error> validate_infoset_owner_map(
        const infoset_owner_map& owner_map,
        const uint32_t infoset_count)
    {
        if (owner_map.owner_count == 0u || owner_map.owner_by_infoset.size() != infoset_count) {
            return std::unexpected(iteration_error{iteration_error_kind::invalid_context});
        }

        uint32_t expected_begin = 0;
        for (const auto& range : owner_map.ranges) {
            if (range.owner_worker >= owner_map.owner_count
                || range.begin_infoset != expected_begin
                || range.end_infoset < range.begin_infoset
                || range.end_infoset > infoset_count) {
                return std::unexpected(iteration_error{iteration_error_kind::invalid_worker_id, range.owner_worker});
            }
            for (uint32_t infoset_id = range.begin_infoset; infoset_id < range.end_infoset; ++infoset_id) {
                if (owner_map.owner_by_infoset[infoset_id] != range.owner_worker) {
                    return std::unexpected(iteration_error{iteration_error_kind::invalid_worker_id, range.owner_worker});
                }
            }
            expected_begin = range.end_infoset;
        }

        if (expected_begin != infoset_count) {
            return std::unexpected(iteration_error{iteration_error_kind::invalid_context});
        }

        return {};
    }

    /**
     * Build a non-owning table shard view for one validated owner range.
     */
    [[nodiscard]] inline table_shard_view make_table_shard_view(
        regret_table& regrets,
        strategy_sum_table& strategy_sums,
        const infoset_owner_range& range) noexcept
    {
        const auto begin_value = regrets.action_offsets[range.begin_infoset];
        const auto end_value = regrets.action_offsets[range.end_infoset];
        return table_shard_view{
            .regrets = &regrets,
            .strategy_sums = &strategy_sums,
            .owner_worker = range.owner_worker,
            .begin_infoset = range.begin_infoset,
            .end_infoset = range.end_infoset,
            .begin_value = begin_value,
            .end_value = end_value
        };
    }

    /**
     * Apply CFR+ clipping after all worker-local raw regret deltas have been merged.
     */
    [[nodiscard]] inline uint64_t clip_cfr_plus_regrets(regret_table& regrets) noexcept
    {
        uint64_t clipped_values = 0;
        for (auto& regret : regrets.regrets) {
            clipped_values += regret < 0.0f ? 1u : 0u;
            regret = std::max(regret, 0.0f);
        }
        return clipped_values;
    }

    namespace detail {

        inline void reset_reduction_diagnostics(
            reduction_diagnostics& diagnostics,
            const uint32_t owner_count)
        {
            diagnostics = {};
            diagnostics.owner_hit_distribution.assign(owner_count, 0u);
            diagnostics.owner_remote_hit_distribution.assign(owner_count, 0u);
            diagnostics.per_owner_touched_values.assign(owner_count, 0u);
            diagnostics.per_owner_reduction_time_ns.assign(owner_count, 0u);
        }

        inline void apply_delta_entry(
            regret_table& regrets,
            strategy_sum_table& strategy_sums,
            const table_delta_buffer& buffer,
            const table_delta_entry& entry) noexcept
        {
            const auto global_begin = regrets.action_offsets[entry.infoset_id];
            const auto regret_deltas = buffer.regret_deltas_for(entry);
            const auto strategy_deltas = buffer.strategy_deltas_for(entry);
            for (uint32_t action_index = 0; action_index < regret_deltas.size(); ++action_index) {
                regrets.regrets[global_begin + action_index] += regret_deltas[action_index];
                strategy_sums.sums[global_begin + action_index] += strategy_deltas[action_index];
            }
        }

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
     * Route worker-local sparse deltas by infoset owner, then by deterministic worker order.
     */
    [[nodiscard]] inline std::expected<void, iteration_error> apply_owner_routed_worker_reductions(
        regret_table& regrets,
        strategy_sum_table& strategy_sums,
        const deterministic_reduction_plan& plan,
        std::span<const traversal::worker_context* const> workers,
        const infoset_owner_map& owner_map,
        reduction_diagnostics* diagnostics = nullptr)
    {
        if (auto result = validate_reduction_plan(plan, static_cast<uint32_t>(workers.size())); !result) {
            return std::unexpected(result.error());
        }
        if (!same_action_offsets(regrets.action_offsets, strategy_sums.action_offsets)) {
            return std::unexpected(iteration_error{iteration_error_kind::table_layout_mismatch});
        }
        if (auto result = validate_infoset_owner_map(owner_map, regrets.infoset_count()); !result) {
            return std::unexpected(result.error());
        }

        reduction_diagnostics local_diagnostics;
        auto& reduction_stats = diagnostics == nullptr ? local_diagnostics : *diagnostics;
        detail::reset_reduction_diagnostics(reduction_stats, owner_map.owner_count);

        for (const auto worker_id : plan.worker_order) {
            const auto* worker = workers[worker_id];
            if (worker == nullptr) {
                return std::unexpected(iteration_error{iteration_error_kind::missing_worker_context, worker_id});
            }
            if (!same_action_offsets(regrets.action_offsets, worker->delta_buffer.action_offsets())) {
                return std::unexpected(iteration_error{iteration_error_kind::table_layout_mismatch, worker_id});
            }
        }

        for (const auto& range : owner_map.ranges) {
            const auto owner_start = std::chrono::steady_clock::now();
            for (const auto worker_id : plan.worker_order) {
                const auto* worker = workers[worker_id];
                for (const auto& entry : worker->delta_buffer.entries()) {
                    if (entry.infoset_id < range.begin_infoset || entry.infoset_id >= range.end_infoset) {
                        continue;
                    }

                    const auto owner = range.owner_worker;
                    const auto value_count = static_cast<uint64_t>(entry.end - entry.begin);
                    ++reduction_stats.owner_hit_distribution[owner];
                    ++reduction_stats.reduction_entries;
                    reduction_stats.reduction_values += value_count;
                    reduction_stats.per_owner_touched_values[owner] += value_count;
                    if (worker_id != owner) {
                        ++reduction_stats.remote_delta_count;
                        ++reduction_stats.owner_remote_hit_distribution[owner];
                        reduction_stats.remote_delta_bytes += value_count * sizeof(float) * 2u;
                    }
                    detail::apply_delta_entry(regrets, strategy_sums, worker->delta_buffer, entry);
                }
            }
            const auto owner_end = std::chrono::steady_clock::now();
            reduction_stats.per_owner_reduction_time_ns[range.owner_worker] += static_cast<uint64_t>(
                std::chrono::duration_cast<std::chrono::nanoseconds>(owner_end - owner_start).count());
        }

        return {};
    }

    /**
     * Merge worker-local deltas and apply variant-level post-merge regret rules.
     */
    [[nodiscard]] inline std::expected<void, iteration_error> apply_worker_reductions(
        regret_table& regrets,
        strategy_sum_table& strategy_sums,
        const deterministic_reduction_plan& plan,
        std::span<const traversal::worker_context* const> workers,
        const cfr_variant variant)
    {
        if (auto result = apply_worker_reductions(regrets, strategy_sums, plan, workers); !result) {
            return result;
        }
        if (variant == cfr_variant::cfr_plus) {
            (void) clip_cfr_plus_regrets(regrets);
        }
        return {};
    }

    /**
     * Apply a selected deterministic reduction policy.
     */
    [[nodiscard]] inline std::expected<void, iteration_error> apply_worker_reductions(
        regret_table& regrets,
        strategy_sum_table& strategy_sums,
        const deterministic_reduction_plan& plan,
        std::span<const traversal::worker_context* const> workers,
        const reduction_policy policy,
        const infoset_owner_map* owner_map,
        reduction_diagnostics* diagnostics,
        const cfr_variant variant)
    {
        std::expected<void, iteration_error> result;
        if (policy.order == reduction_order::owner_range_then_worker) {
            if (owner_map == nullptr) {
                return std::unexpected(iteration_error{iteration_error_kind::invalid_context});
            }
            result = apply_owner_routed_worker_reductions(
                regrets,
                strategy_sums,
                plan,
                workers,
                *owner_map,
                diagnostics);
        } else {
            if (diagnostics != nullptr) {
                detail::reset_reduction_diagnostics(*diagnostics, static_cast<uint32_t>(workers.size()));
                for (const auto* worker : workers) {
                    if (worker == nullptr) {
                        continue;
                    }
                    for (const auto& entry : worker->delta_buffer.entries()) {
                        ++diagnostics->reduction_entries;
                        diagnostics->reduction_values += entry.end - entry.begin;
                    }
                }
            }
            result = apply_worker_reductions(regrets, strategy_sums, plan, workers);
        }

        if (!result) {
            return result;
        }
        if (variant == cfr_variant::cfr_plus) {
            if (diagnostics != nullptr) {
                diagnostics->cfr_plus_clipped_values = clip_cfr_plus_regrets(regrets);
            } else {
                (void) clip_cfr_plus_regrets(regrets);
            }
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

    /**
     * Merge contiguous worker contexts and apply variant-level post-merge regret rules.
     */
    [[nodiscard]] inline std::expected<void, iteration_error> apply_worker_reductions(
        regret_table& regrets,
        strategy_sum_table& strategy_sums,
        std::span<const traversal::worker_context> workers,
        const cfr_variant variant)
    {
        const auto plan = make_deterministic_reduction_plan(static_cast<uint32_t>(workers.size()));

        std::vector<const traversal::worker_context*> worker_ptrs;
        worker_ptrs.reserve(workers.size());
        for (const auto& worker : workers) {
            worker_ptrs.push_back(&worker);
        }

        return apply_worker_reductions(
            regrets,
            strategy_sums,
            plan,
            std::span<const traversal::worker_context* const>{worker_ptrs},
            variant);
    }

    [[nodiscard]] inline uint64_t hash_owner_ranges(const infoset_owner_map* owner_map) noexcept
    {
        compatibility_hasher hash;
        if (owner_map == nullptr) {
            return hash.value;
        }

        hash.add_u64(owner_map->owner_count);
        hash.add_u64(owner_map->ranges.size());
        hash.add_u64(owner_map->owner_by_infoset.size());
        for (const auto& range : owner_map->ranges) {
            hash.add_u64(range.owner_worker);
            hash.add_u64(range.begin_infoset);
            hash.add_u64(range.end_infoset);
        }
        return hash.value;
    }

    template <std::size_t N>
    [[nodiscard]] inline uint64_t hash_terminal_state_layout(
        const cfr_terminal_provider<N>& terminal_provider) noexcept
    {
        compatibility_hasher hash;
        hash.add_u64(static_cast<uint64_t>(terminal_provider.kind));
        hash.add_u64(terminal_provider.terminal_leaves.size());
        hash.add_u64(terminal_provider.terminal_states.size());
        hash.add_u64(terminal_provider.fixed_terminal_utility_by_node.size());
        return hash.value;
    }

    template <std::size_t N>
    [[nodiscard]] cfr_checkpoint_header make_cfr_checkpoint_header(
        const cfr_solver_context<N>& context,
        const iteration_config config) noexcept
    {
        const auto view = make_solver_graph_view<N>(*context.graph, *context.graph_annotations);
        return cfr_checkpoint_header{
            .magic = 0x5a45544143465231ull,
            .version = 1,
            .endian_marker = 0x01020304u,
            .player_count = static_cast<uint32_t>(N),
            .infoset_count = context.layout->infoset_count(),
            .value_count = context.layout->value_count(),
            .iteration = config.iteration,
            .variant = config.variant,
            .numeric = context.numeric,
            .reduction = context.reduction,
            .chance = context.chance,
            .compatibility = make_solver_compatibility_key(
                view,
                *context.layout,
                context.numeric,
                context.reduction,
                context.chance),
            .owner_range_hash = hash_owner_ranges(context.owner_map),
            .terminal_state_layout_hash = hash_terminal_state_layout(context.terminal_provider),
            .rng_stream_policy_hash = compatibility_hasher::OFFSET
        };
    }

    namespace detail {

        template <typename T>
        [[nodiscard]] std::expected<void, checkpoint_error> write_binary(std::ostream& os, const T& value)
        {
            os.write(reinterpret_cast<const char*>(&value), sizeof(T));
            if (!os) {
                return std::unexpected(checkpoint_error{checkpoint_error_kind::stream_write_failed});
            }
            return {};
        }

        template <typename T>
        [[nodiscard]] std::expected<void, checkpoint_error> read_binary(std::istream& is, T& value)
        {
            is.read(reinterpret_cast<char*>(&value), sizeof(T));
            if (!is) {
                return std::unexpected(checkpoint_error{checkpoint_error_kind::stream_read_failed});
            }
            return {};
        }

        [[nodiscard]] inline std::expected<void, checkpoint_error> write_float_vector(
            std::ostream& os,
            const std::vector<float>& values)
        {
            const auto count = static_cast<uint64_t>(values.size());
            if (auto result = write_binary(os, count); !result) {
                return result;
            }
            if (!values.empty()) {
                os.write(reinterpret_cast<const char*>(values.data()), static_cast<std::streamsize>(values.size() * sizeof(float)));
                if (!os) {
                    return std::unexpected(checkpoint_error{checkpoint_error_kind::stream_write_failed});
                }
            }
            return {};
        }

        [[nodiscard]] inline std::expected<void, checkpoint_error> read_float_vector(
            std::istream& is,
            std::vector<float>& values,
            const uint64_t expected_count)
        {
            uint64_t count = 0;
            if (auto result = read_binary(is, count); !result) {
                return result;
            }
            if (count != expected_count) {
                return std::unexpected(checkpoint_error{checkpoint_error_kind::incompatible_table_size});
            }
            if (!values.empty()) {
                is.read(reinterpret_cast<char*>(values.data()), static_cast<std::streamsize>(values.size() * sizeof(float)));
                if (!is) {
                    return std::unexpected(checkpoint_error{checkpoint_error_kind::stream_read_failed});
                }
            }
            return {};
        }

        [[nodiscard]] inline std::expected<void, checkpoint_error> validate_checkpoint_header(
            const cfr_checkpoint_header& loaded,
            const cfr_checkpoint_header& expected) noexcept
        {
            if (loaded.magic != expected.magic) {
                return std::unexpected(checkpoint_error{checkpoint_error_kind::invalid_magic});
            }
            if (loaded.version != expected.version) {
                return std::unexpected(checkpoint_error{checkpoint_error_kind::unsupported_version});
            }
            if (loaded.endian_marker != expected.endian_marker) {
                return std::unexpected(checkpoint_error{checkpoint_error_kind::incompatible_endianness});
            }
            if (loaded.player_count != expected.player_count) {
                return std::unexpected(checkpoint_error{checkpoint_error_kind::incompatible_player_count});
            }
            if (loaded.variant != expected.variant) {
                return std::unexpected(checkpoint_error{checkpoint_error_kind::incompatible_variant});
            }
            if (loaded.numeric.table_storage != expected.numeric.table_storage
                || loaded.numeric.accumulation != expected.numeric.accumulation) {
                return std::unexpected(checkpoint_error{checkpoint_error_kind::incompatible_numeric_policy});
            }
            if (loaded.reduction.order != expected.reduction.order) {
                return std::unexpected(checkpoint_error{checkpoint_error_kind::incompatible_reduction_policy});
            }
            if (loaded.chance != expected.chance) {
                return std::unexpected(checkpoint_error{checkpoint_error_kind::incompatible_chance_mode});
            }
            if (loaded.compatibility.graph_metadata_hash != expected.compatibility.graph_metadata_hash) {
                return std::unexpected(checkpoint_error{checkpoint_error_kind::incompatible_graph_metadata});
            }
            if (loaded.compatibility.action_layout_hash != expected.compatibility.action_layout_hash
                || loaded.infoset_count != expected.infoset_count
                || loaded.value_count != expected.value_count) {
                return std::unexpected(checkpoint_error{checkpoint_error_kind::incompatible_action_layout});
            }
            if (loaded.owner_range_hash != expected.owner_range_hash) {
                return std::unexpected(checkpoint_error{checkpoint_error_kind::incompatible_owner_ranges});
            }
            return {};
        }
    }

    template <std::size_t N>
    [[nodiscard]] std::expected<void, checkpoint_error> save_cfr_checkpoint(
        std::ostream& os,
        const cfr_solver_context<N>& context,
        const iteration_config config)
    {
        const auto header = make_cfr_checkpoint_header(context, config);
        if (auto result = detail::write_binary(os, header); !result) {
            return result;
        }
        if (auto result = detail::write_float_vector(os, context.regrets->regrets); !result) {
            return result;
        }
        return detail::write_float_vector(os, context.strategy_sums->sums);
    }

    template <std::size_t N>
    [[nodiscard]] std::expected<cfr_checkpoint_resume, checkpoint_error> load_cfr_checkpoint(
        std::istream& is,
        cfr_solver_context<N>& context,
        const iteration_config expected_config)
    {
        cfr_checkpoint_header loaded_header;
        if (auto result = detail::read_binary(is, loaded_header); !result) {
            return std::unexpected(result.error());
        }

        const auto expected_header = make_cfr_checkpoint_header(context, expected_config);
        if (auto result = detail::validate_checkpoint_header(loaded_header, expected_header); !result) {
            return std::unexpected(result.error());
        }
        if (auto result = detail::read_float_vector(
                is,
                context.regrets->regrets,
                context.regrets->value_count());
            !result) {
            return std::unexpected(result.error());
        }
        if (auto result = detail::read_float_vector(
                is,
                context.strategy_sums->sums,
                context.strategy_sums->value_count());
            !result) {
            return std::unexpected(result.error());
        }

        return cfr_checkpoint_resume{loaded_header};
    }

    [[nodiscard]] inline quality_diagnostics compute_quality_diagnostics(
        const game_graph& graph,
        const solver_graph_annotations& annotations,
        const regret_table& regrets,
        const strategy_sum_table& strategy_sums,
        const uint32_t player_count)
    {
        quality_diagnostics diagnostics;
        diagnostics.strategy_sum_mass_by_player.assign(player_count, 0.0);

        double regret_sum = 0.0;
        double regret_square_sum = 0.0;
        uint64_t regret_value_count = 0;
        for (uint32_t infoset_id = 0; infoset_id < regrets.infoset_count(); ++infoset_id) {
            const auto begin = regrets.action_offsets[infoset_id];
            const auto end = regrets.action_offsets[infoset_id + 1u];
            for (uint32_t action_index = 0; action_index < end - begin; ++action_index) {
                const auto regret = regrets.regrets[begin + action_index];
                regret_sum += regret;
                regret_square_sum += static_cast<double>(regret) * static_cast<double>(regret);
                ++regret_value_count;
                if (regret > 0.0f) {
                    ++diagnostics.positive_regret_count;
                    diagnostics.exploitability_estimate += regret;
                }
                if (diagnostics.max_regret_infoset_id == game_graph::INVALID_INFOSET || regret > diagnostics.max_regret) {
                    diagnostics.max_regret = regret;
                    diagnostics.max_regret_infoset_id = infoset_id;
                    diagnostics.max_regret_location = infoset_diagnostic_location{
                        .infoset_id = infoset_id,
                        .begin_action = 0,
                        .end_action = end - begin,
                        .action_index = action_index
                    };
                }
            }
        }
        diagnostics.mean_regret = regret_value_count == 0u ? 0.0 : regret_sum / static_cast<double>(regret_value_count);
        diagnostics.regret_norm = std::sqrt(regret_square_sum);

        std::vector<uint8_t> actor_by_infoset(regrets.infoset_count(), INVALID_PLAYER);
        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            if (graph.is_player_node(node_id)) {
                actor_by_infoset[graph.infoset_id[node_id]] = annotations.actor_by_node[node_id];
            }
        }

        for (uint32_t infoset_id = 0; infoset_id < strategy_sums.infoset_count(); ++infoset_id) {
            const auto sums = strategy_sums.infoset_sums(infoset_id);
            double mass = 0.0;
            double max_action_sum = 0.0;
            uint32_t max_action_index = 0;
            for (uint32_t action_index = 0; action_index < sums.size(); ++action_index) {
                const auto value = static_cast<double>(sums[action_index]);
                mass += value;
                if (value > max_action_sum) {
                    max_action_sum = value;
                    max_action_index = action_index;
                }
            }

            diagnostics.average_strategy_mass += mass;
            const auto actor = actor_by_infoset[infoset_id];
            if (actor < diagnostics.strategy_sum_mass_by_player.size()) {
                diagnostics.strategy_sum_mass_by_player[actor] += mass;
            }
            if (sums.empty() || mass <= 0.0) {
                continue;
            }

            double entropy = 0.0;
            for (const auto sum : sums) {
                const auto probability = static_cast<double>(sum) / mass;
                if (probability > 0.0) {
                    entropy -= probability * std::log(probability);
                }
            }
            const auto uniform_entropy = std::log(static_cast<double>(sums.size()));
            diagnostics.largest_strategy_entropy_drop = std::max(
                diagnostics.largest_strategy_entropy_drop,
                uniform_entropy - entropy);

            const auto uniform_probability = 1.0 / static_cast<double>(sums.size());
            const auto largest_change = std::abs(max_action_sum / mass - uniform_probability);
            if (largest_change > diagnostics.largest_strategy_change) {
                diagnostics.largest_strategy_change = largest_change;
                diagnostics.largest_strategy_change_location = infoset_diagnostic_location{
                    .infoset_id = infoset_id,
                    .begin_action = 0,
                    .end_action = static_cast<uint32_t>(sums.size()),
                    .action_index = max_action_index
                };
            }
        }

        return diagnostics;
    }

    template <std::size_t N>
    struct cfr_engine;

    /**
     * Compact DFS control frame for CFR kernels that keep reach/value state in side scratch.
     */
    struct cfr_traversal_frame {
        uint32_t node_id = game_graph::INVALID_NODE;
        uint32_t edge_cursor = 0;
        uint32_t reach_slot = 0;
        uint32_t value_slot = 0;
        traversal::traversal_phase phase = traversal::traversal_phase::enter;
    };

    static_assert(std::is_trivially_copyable_v<cfr_traversal_frame>);

    struct hu_reach_state {
        float oop = 1.0f;
        float ip = 1.0f;
        float chance = 1.0f;
    };

    template <std::size_t N>
    struct nway_reach_state {
        std::array<float, N> player{};
        float chance = 1.0f;
    };

    namespace detail {
        template <std::size_t N, typename StrategyPolicy>
        [[nodiscard]] std::expected<traversal::traversal_result, iteration_error> traverse_cfr_tree(
            const game_graph& graph,
            const solver_graph_annotations& annotations,
            const chance_event_table* chance_events,
            const regret_table& regrets,
            traversal::worker_context& worker,
            const iteration_config& config,
            const cfr_terminal_provider<N>& terminal_provider);

        template <typename StrategyPolicy>
        [[nodiscard]] std::expected<traversal::traversal_result, iteration_error> traverse_heads_up_cfr_tree(
            const game_graph& graph,
            const solver_graph_annotations& annotations,
            const chance_event_table* chance_events,
            const regret_table& regrets,
            traversal::worker_context& worker,
            const iteration_config& config,
            const cfr_terminal_provider<2>& terminal_provider);

        template <std::size_t N, typename StrategyPolicy>
            requires (N >= 3)
        [[nodiscard]] std::expected<traversal::traversal_result, iteration_error> traverse_nway_cfr_tree(
            const game_graph& graph,
            const solver_graph_annotations& annotations,
            const chance_event_table* chance_events,
            const regret_table& regrets,
            traversal::worker_context& worker,
            const iteration_config& config,
            const cfr_terminal_provider<N>& terminal_provider);
    }

    template <>
    struct cfr_engine<2> {
        static constexpr bool heads_up = true;
        using reach_state = hu_reach_state;

        [[nodiscard]] static reach_state initial_reach() noexcept
        {
            return reach_state{};
        }

        static constexpr uint32_t reach_scratch_width = 3;

        [[nodiscard]] static float chance_reach(const reach_state& reach) noexcept
        {
            return reach.chance;
        }

        [[nodiscard]] static reach_state load_reach(
            const std::span<const float> scratch,
            const uint32_t slot) noexcept
        {
            const auto offset = slot * reach_scratch_width;
            return reach_state{
                .oop = scratch[offset],
                .ip = scratch[offset + 1u],
                .chance = scratch[offset + 2u]
            };
        }

        static void store_reach(
            const std::span<float> scratch,
            const uint32_t slot,
            const reach_state reach) noexcept
        {
            const auto offset = slot * reach_scratch_width;
            scratch[offset] = reach.oop;
            scratch[offset + 1u] = reach.ip;
            scratch[offset + 2u] = reach.chance;
        }

        static void propagate_chance(reach_state& reach, const float probability) noexcept
        {
            reach.chance *= probability;
        }

        [[nodiscard]] static float counterfactual_reach(
            const reach_state& reach,
            const uint8_t actor) noexcept
        {
            return reach.chance * (actor == 0u ? reach.ip : reach.oop);
        }

        /**
         * Counterfactual reach for heads-up CFR without an N-way product loop.
         */
        [[nodiscard]] static float counterfactual_reach(
            const std::array<float, 2>& reach,
            const float chance_reach,
            const uint8_t actor) noexcept
        {
            return chance_reach * (actor == 0u ? reach[1] : reach[0]);
        }

        /**
         * Own reach for heads-up average-strategy accumulation.
         */
        [[nodiscard]] static float own_reach(
            const std::array<float, 2>& reach,
            const uint8_t actor) noexcept
        {
            return actor == 0u ? reach[0] : reach[1];
        }

        [[nodiscard]] static float own_reach(
            const reach_state& reach,
            const uint8_t actor) noexcept
        {
            return actor == 0u ? reach.oop : reach.ip;
        }

        /**
         * Propagate a heads-up player action by direct scalar seat update.
         */
        static void propagate_player_action(
            std::array<float, 2>& reach,
            const uint8_t actor,
            const float probability) noexcept
        {
            if (actor == 0u) {
                reach[0] *= probability;
            } else {
                reach[1] *= probability;
            }
        }

        static void propagate_player_action(
            reach_state& reach,
            const uint8_t actor,
            const float probability) noexcept
        {
            if (actor == 0u) {
                reach.oop *= probability;
            } else {
                reach.ip *= probability;
            }
        }

        /**
         * Execute one heads-up traversal using the scalar OOP/IP CFR kernel.
         */
        template <typename StrategyPolicy>
        [[nodiscard]] static std::expected<traversal::traversal_result, iteration_error> traverse(
            const game_graph& graph,
            const solver_graph_annotations& annotations,
            const chance_event_table* chance_events,
            const regret_table& regrets,
            traversal::worker_context& worker,
            const iteration_config& config,
            const cfr_terminal_provider<2>& terminal_provider)
        {
            return detail::traverse_heads_up_cfr_tree<StrategyPolicy>(
                graph,
                annotations,
                chance_events,
                regrets,
                worker,
                config,
                terminal_provider);
        }
    };

    template <std::size_t N>
        requires (N >= 3)
    struct cfr_engine<N> {
        static constexpr bool heads_up = false;
        using reach_state = nway_reach_state<N>;

        [[nodiscard]] static reach_state initial_reach() noexcept
        {
            reach_state reach{};
            reach.player.fill(1.0f);
            reach.chance = 1.0f;
            return reach;
        }

        static constexpr uint32_t reach_scratch_width = static_cast<uint32_t>(N + 1u);

        [[nodiscard]] static float chance_reach(const reach_state& reach) noexcept
        {
            return reach.chance;
        }

        [[nodiscard]] static reach_state load_reach(
            const std::span<const float> scratch,
            const uint32_t slot) noexcept
        {
            const auto offset = slot * reach_scratch_width;
            reach_state reach{};
            for (uint8_t player = 0; player < N; ++player) {
                reach.player[player] = scratch[offset + player];
            }
            reach.chance = scratch[offset + N];
            return reach;
        }

        static void store_reach(
            const std::span<float> scratch,
            const uint32_t slot,
            const reach_state& reach) noexcept
        {
            const auto offset = slot * reach_scratch_width;
            for (uint8_t player = 0; player < N; ++player) {
                scratch[offset + player] = reach.player[player];
            }
            scratch[offset + N] = reach.chance;
        }

        static void propagate_chance(reach_state& reach, const float probability) noexcept
        {
            reach.chance *= probability;
        }

        [[nodiscard]] static float counterfactual_reach(
            const reach_state& reach,
            const uint8_t actor) noexcept
        {
            float result = reach.chance;
            for (uint8_t player = 0; player < N; ++player) {
                if (player != actor) {
                    result *= reach.player[player];
                }
            }
            return result;
        }

        /**
         * Counterfactual reach for an N-way CFR update.
         */
        [[nodiscard]] static float counterfactual_reach(
            const std::array<float, N>& reach,
            const float chance_reach,
            const uint8_t actor) noexcept
        {
            float result = chance_reach;
            for (uint8_t player = 0; player < N; ++player) {
                if (player != actor) {
                    result *= reach[player];
                }
            }
            return result;
        }

        /**
         * Own reach for N-way average-strategy accumulation.
         */
        [[nodiscard]] static float own_reach(
            const std::array<float, N>& reach,
            const uint8_t actor) noexcept
        {
            return reach[actor];
        }

        [[nodiscard]] static float own_reach(
            const reach_state& reach,
            const uint8_t actor) noexcept
        {
            return reach.player[actor];
        }

        /**
         * Propagate one acting player's reach through an action probability.
         */
        static void propagate_player_action(
            std::array<float, N>& reach,
            const uint8_t actor,
            const float probability) noexcept
        {
            reach[actor] *= probability;
        }

        static void propagate_player_action(
            reach_state& reach,
            const uint8_t actor,
            const float probability) noexcept
        {
            reach.player[actor] *= probability;
        }

        /**
         * Execute one N-way traversal through the shared graph/table storage.
         */
        template <typename StrategyPolicy>
        [[nodiscard]] static std::expected<traversal::traversal_result, iteration_error> traverse(
            const game_graph& graph,
            const solver_graph_annotations& annotations,
            const chance_event_table* chance_events,
            const regret_table& regrets,
            traversal::worker_context& worker,
            const iteration_config& config,
            const cfr_terminal_provider<N>& terminal_provider)
        {
            return detail::traverse_nway_cfr_tree<N, StrategyPolicy>(
                graph,
                annotations,
                chance_events,
                regrets,
                worker,
                config,
                terminal_provider);
        }
    };

    namespace detail {

        inline void add_diagnostics(
            traversal::traversal_diagnostics& aggregate,
            const traversal::traversal_diagnostics& delta) noexcept
        {
            aggregate.nodes_visited += delta.nodes_visited;
            aggregate.edges_scanned += delta.edges_scanned;
            aggregate.terminal_nodes += delta.terminal_nodes;
            aggregate.player_nodes += delta.player_nodes;
            aggregate.player_chance_nodes += delta.player_chance_nodes;
            aggregate.chance_nodes += delta.chance_nodes;
            aggregate.max_stack_depth = std::max(aggregate.max_stack_depth, delta.max_stack_depth);
            aggregate.max_action_count = std::max(aggregate.max_action_count, delta.max_action_count);
            aggregate.local_delta_entries_touched += delta.local_delta_entries_touched;
            aggregate.chance_outcomes += delta.chance_outcomes;
            aggregate.regret_updates += delta.regret_updates;
            aggregate.strategy_updates += delta.strategy_updates;
            aggregate.terminal_evaluations += delta.terminal_evaluations;
            aggregate.reduction_values += delta.reduction_values;
            aggregate.zero_reach_skips += delta.zero_reach_skips;
        }

        template <std::size_t N>
        [[nodiscard]] std::expected<void, iteration_error> validate_terminal_provider(
            const game_graph& graph,
            const cfr_terminal_provider<N>& provider) noexcept
        {
            using enum cfr_terminal_provider_kind;
            switch (provider.kind) {
                case fixed_utility_fixture:
                    if (provider.fixed_terminal_utility_by_node.size() < graph.node_count) {
                        return std::unexpected(iteration_error{
                            .kind = iteration_error_kind::traversal,
                            .traversal = traversal::traversal_error{traversal::traversal_error_kind::invalid_terminal_context}
                        });
                    }
                    return {};
                case terminal_state_table:
                    if (provider.river_cache == nullptr
                        || provider.reach_indices == nullptr
                        || provider.terminal_leaves.size() < graph.node_count
                        || provider.terminal_states.empty()) {
                        return std::unexpected(iteration_error{
                            .kind = iteration_error_kind::traversal,
                            .traversal = traversal::traversal_error{traversal::traversal_error_kind::invalid_terminal_context}
                        });
                    }
                    for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
                        if (graph.node_types[node_id] != node_kind::terminal) {
                            continue;
                        }
                        if (provider.terminal_leaves[node_id].terminal_state_id >= provider.terminal_states.size()) {
                            return std::unexpected(iteration_error{
                                .kind = iteration_error_kind::traversal,
                                .traversal = traversal::traversal_error{
                                    traversal::traversal_error_kind::invalid_terminal_context,
                                    node_id
                                }
                            });
                        }
                    }
                    return {};
                case none:
                    return std::unexpected(iteration_error{
                        .kind = iteration_error_kind::traversal,
                        .traversal = traversal::traversal_error{traversal::traversal_error_kind::invalid_terminal_context}
                    });
            }
            return std::unexpected(iteration_error{
                .kind = iteration_error_kind::traversal,
                .traversal = traversal::traversal_error{traversal::traversal_error_kind::invalid_terminal_context}
            });
        }

        template <std::size_t N>
        [[nodiscard]] std::expected<float, iteration_error> terminal_utility_for_node(
            const cfr_terminal_provider<N>& provider,
            const iteration_config& config,
            const uint32_t node_id) noexcept
        {
            using enum cfr_terminal_provider_kind;
            switch (provider.kind) {
                case fixed_utility_fixture:
                    assert(node_id < provider.fixed_terminal_utility_by_node.size());
                    return provider.fixed_terminal_utility_by_node[node_id];
                case terminal_state_table: {
                    assert(provider.river_cache != nullptr);
                    assert(provider.reach_indices != nullptr);
                    assert(node_id < provider.terminal_leaves.size());
                    const auto terminal_state_id = provider.terminal_leaves[node_id].terminal_state_id;
                    assert(terminal_state_id < provider.terminal_states.size());

                    const ::zeta::holdem::terminal_engine<N> engine{};
                    const auto values = engine.evaluate_terminal_values(
                        *provider.river_cache,
                        *provider.reach_indices,
                        provider.terminal_states[terminal_state_id],
                        provider.samples_per_combo);
                    const auto player = static_cast<std::size_t>(config.updating_player);
                    return values[player][provider.combo_by_player[player]];
                }
                case none:
                    break;
            }
            return std::unexpected(iteration_error{
                .kind = iteration_error_kind::traversal,
                .traversal = traversal::traversal_error{traversal::traversal_error_kind::invalid_terminal_context, node_id}
            });
        }

        template <std::size_t N>
        inline void record_cfr_node_entry(
            const game_graph& graph,
            traversal::worker_context& worker,
            const uint32_t node_id) noexcept
        {
            ++worker.diagnostics.nodes_visited;
            worker.diagnostics.max_action_count = std::max(worker.diagnostics.max_action_count, graph.action_count(node_id));

            const auto kind = graph.node_types[node_id];
            using enum node_kind;
            switch (kind) {
                case player_chance:
                    ++worker.diagnostics.player_chance_nodes;
                    break;
                case player:
                    ++worker.diagnostics.player_nodes;
                    break;
                case chance:
                    ++worker.diagnostics.chance_nodes;
                    break;
                case terminal:
                    ++worker.diagnostics.terminal_nodes;
                    break;
            }
        }

        template <std::size_t N>
        [[nodiscard]] std::expected<void, iteration_error> prepare_cfr_kernel_scratch(
            const game_graph& graph,
            traversal::worker_context& worker)
        {
            using engine = cfr_engine<N>;
            const auto frame_capacity = worker.stack_capacity();
            const auto required_reach_scratch = frame_capacity * engine::reach_scratch_width;
            worker.cfr_reach_scratch.resize(required_reach_scratch);

            if (worker.cfr_frame_capacity() < frame_capacity
                || worker.cfr_value_scratch_capacity() < frame_capacity
                || worker.cfr_reach_scratch_capacity() < required_reach_scratch) {
                return std::unexpected(iteration_error{
                    .kind = iteration_error_kind::traversal,
                    .traversal = traversal::traversal_error{
                        traversal::traversal_error_kind::scratch_capacity_exceeded,
                        graph.root_node,
                        std::max(frame_capacity, required_reach_scratch),
                        std::min({
                            worker.cfr_frame_capacity(),
                            worker.cfr_value_scratch_capacity(),
                            worker.cfr_reach_scratch_capacity()
                        })
                    }
                });
            }

            return {};
        }

        template <std::size_t N, typename StrategyPolicy>
        [[nodiscard]] std::expected<traversal::traversal_result, iteration_error> traverse_cfr_tree(
            const game_graph& graph,
            const solver_graph_annotations& annotations,
            const chance_event_table* chance_events,
            const regret_table& regrets,
            traversal::worker_context& worker,
            const iteration_config& config,
            const cfr_terminal_provider<N>& terminal_provider)
        {
            using engine = cfr_engine<N>;
            using reach_state = typename engine::reach_state;

            const auto required_edge_scratch = static_cast<uint32_t>(graph.edges.size());
            if (worker.stack_capacity() < static_cast<uint32_t>(graph.max_depth) + 1u) {
                return std::unexpected(iteration_error{
                    .kind = iteration_error_kind::traversal,
                    .traversal = traversal::traversal_error{
                        traversal::traversal_error_kind::stack_capacity_exceeded,
                        graph.root_node,
                        static_cast<uint32_t>(graph.max_depth) + 1u,
                        worker.stack_capacity()
                    }
                });
            }
            if (worker.node_utility_capacity() < graph.node_count
                || worker.child_action_value_capacity() < required_edge_scratch
                || worker.edge_probability_capacity() < required_edge_scratch) {
                return std::unexpected(iteration_error{
                    .kind = iteration_error_kind::traversal,
                    .traversal = traversal::traversal_error{
                        traversal::traversal_error_kind::scratch_capacity_exceeded,
                        graph.root_node,
                        std::max(graph.node_count, required_edge_scratch),
                        std::min(worker.node_utility_capacity(), worker.edge_probability_capacity())
                    }
                });
            }

            std::fill(worker.node_utility.begin(), worker.node_utility.end(), 0.0f);
            std::fill(worker.edge_probability.begin(), worker.edge_probability.end(), 0.0f);
            std::fill(worker.child_action_value.begin(), worker.child_action_value.end(), 0.0f);
            worker.delta_buffer.clear();
            worker.diagnostics = {};

            const auto frame_capacity = worker.stack_capacity();
            const auto required_reach_scratch = frame_capacity * engine::reach_scratch_width;
            if (worker.cfr_frame_capacity() < frame_capacity
                || worker.cfr_value_scratch_capacity() < frame_capacity
                || worker.cfr_reach_scratch_capacity() < required_reach_scratch) {
                return std::unexpected(iteration_error{
                    .kind = iteration_error_kind::traversal,
                    .traversal = traversal::traversal_error{
                        traversal::traversal_error_kind::scratch_capacity_exceeded,
                        graph.root_node,
                        std::max(frame_capacity, required_reach_scratch),
                        std::min({
                            worker.cfr_frame_capacity(),
                            worker.cfr_value_scratch_capacity(),
                            worker.cfr_reach_scratch_capacity()
                        })
                    }
                });
            }

            std::fill(worker.cfr_value_scratch.begin(), worker.cfr_value_scratch.end(), 0.0f);
            uint32_t stack_size = 1;
            worker.cfr_frame_node_id[0] = graph.root_node;
            worker.cfr_frame_edge_cursor[0] = 0;
            worker.cfr_frame_reach_slot[0] = 0;
            worker.cfr_frame_value_slot[0] = 0;
            worker.cfr_frame_phase[0] = traversal::traversal_phase::enter;
            engine::store_reach(worker.cfr_reach_scratch, 0, engine::initial_reach());

            auto propagate_child_value = [&](const uint32_t child_value_slot) noexcept {
                if (stack_size == 0u) {
                    return;
                }
                const auto parent_index = stack_size - 1u;
                const auto parent_node_id = worker.cfr_frame_node_id[parent_index];
                const auto parent_kind = graph.node_types[parent_node_id];
                if (worker.cfr_frame_edge_cursor[parent_index] == 0u) {
                    return;
                }

                const auto local_index = worker.cfr_frame_edge_cursor[parent_index] - 1u;
                const auto edge_offset = graph.row_offsets[parent_node_id] + local_index;
                const auto child_value = worker.cfr_value_scratch[child_value_slot];
                const auto parent_value_slot = worker.cfr_frame_value_slot[parent_index];
                if (parent_kind == node_kind::chance) {
                    worker.cfr_value_scratch[parent_value_slot] += worker.edge_probability[edge_offset] * child_value;
                } else {
                    worker.child_action_value[edge_offset] = child_value;
                    worker.cfr_value_scratch[parent_value_slot] += worker.edge_probability[edge_offset] * child_value;
                }
            };

            while (stack_size != 0u) {
                worker.diagnostics.max_stack_depth = std::max(
                    worker.diagnostics.max_stack_depth,
                    stack_size);
                const auto frame_index = stack_size - 1u;
                const auto node_id = worker.cfr_frame_node_id[frame_index];
                const auto kind = graph.node_types[node_id];
                auto& phase = worker.cfr_frame_phase[frame_index];

                if (phase == traversal::traversal_phase::enter) {
                    record_cfr_node_entry<N>(graph, worker, node_id);
                    const auto value_slot = worker.cfr_frame_value_slot[frame_index];
                    worker.cfr_value_scratch[value_slot] = 0.0f;

                    if (kind == node_kind::terminal) {
                        ++worker.diagnostics.terminal_evaluations;
                        auto terminal_value = terminal_utility_for_node(terminal_provider, config, node_id);
                        if (!terminal_value) {
                            return std::unexpected(terminal_value.error());
                        }
                        worker.cfr_value_scratch[value_slot] = *terminal_value;
                        worker.node_utility[node_id] = worker.cfr_value_scratch[value_slot];
                        phase = traversal::traversal_phase::exit;
                        continue;
                    }

                    if (kind != node_kind::chance) {
                        const auto infoset_id = graph.infoset_id[node_id];
                        const auto begin = graph.row_offsets[node_id];
                        const auto edges = graph.out_edges(node_id);
                        auto edge_probabilities = std::span<float>{worker.edge_probability.data() + begin, edges.size()};
                        compute_regret_matching_strategy<StrategyPolicy>(
                            regrets.infoset_regrets(infoset_id),
                            edges,
                            edge_probabilities);
                    }

                    worker.cfr_frame_edge_cursor[frame_index] = 0;
                    phase = traversal::traversal_phase::visit_children;
                    continue;
                }

                if (phase == traversal::traversal_phase::visit_children) {
                    const auto edges = graph.out_edges(node_id);
                    if (worker.cfr_frame_edge_cursor[frame_index] < edges.size()) {
                        const auto local_index = worker.cfr_frame_edge_cursor[frame_index]++;
                        const auto edge_offset = graph.row_offsets[node_id] + local_index;
                        const auto& child_edge = edges[local_index];
                        ++worker.diagnostics.edges_scanned;

                        auto child_reach = engine::load_reach(
                            worker.cfr_reach_scratch,
                            worker.cfr_frame_reach_slot[frame_index]);
                        float edge_probability = 0.0f;
                        if (kind == node_kind::chance) {
                            ++worker.diagnostics.chance_outcomes;
                            edge_probability = chance_events == nullptr
                                ? 0.0f
                                : chance_events->probability_for_edge(node_id, child_edge);
                            worker.edge_probability[edge_offset] = edge_probability;
                            if (edge_probability == 0.0f || engine::chance_reach(child_reach) == 0.0f) {
                                ++worker.diagnostics.zero_reach_skips;
                                continue;
                            }
                            engine::propagate_chance(child_reach, edge_probability);
                        } else {
                            edge_probability = worker.edge_probability[edge_offset];
                            const auto actor = annotations.actor_by_node[node_id];
                            engine::propagate_player_action(child_reach, actor, edge_probability);
                        }

                        if (stack_size >= frame_capacity) {
                            return std::unexpected(iteration_error{
                                .kind = iteration_error_kind::traversal,
                                .traversal = traversal::traversal_error{
                                    traversal::traversal_error_kind::stack_capacity_exceeded,
                                    child_edge.child_node,
                                    stack_size + 1u,
                                    frame_capacity
                                }
                            });
                        }

                        const auto child_slot = stack_size;
                        engine::store_reach(worker.cfr_reach_scratch, child_slot, child_reach);
                        worker.cfr_value_scratch[child_slot] = 0.0f;
                        worker.cfr_frame_node_id[child_slot] = child_edge.child_node;
                        worker.cfr_frame_edge_cursor[child_slot] = 0;
                        worker.cfr_frame_reach_slot[child_slot] = child_slot;
                        worker.cfr_frame_value_slot[child_slot] = child_slot;
                        worker.cfr_frame_phase[child_slot] = traversal::traversal_phase::enter;
                        ++stack_size;
                        continue;
                    }

                    phase = traversal::traversal_phase::exit;
                    continue;
                }

                if (kind != node_kind::terminal && kind != node_kind::chance) {
                    const auto infoset_id = graph.infoset_id[node_id];
                    const auto actor = annotations.actor_by_node[node_id];
                    const auto edges = graph.out_edges(node_id);
                    const auto begin = graph.row_offsets[node_id];
                    auto edge_probabilities = std::span<const float>{worker.edge_probability.data() + begin, edges.size()};
                    auto child_values = std::span<const float>{worker.child_action_value.data() + begin, edges.size()};
                    auto strategy_deltas = worker.delta_buffer.strategy_deltas(infoset_id);
                    auto regret_deltas = actor == config.updating_player
                        ? worker.delta_buffer.regret_deltas(infoset_id)
                        : std::span<float>{};
                    const auto reach = engine::load_reach(
                        worker.cfr_reach_scratch,
                        worker.cfr_frame_reach_slot[frame_index]);
                    const auto node_value = worker.cfr_value_scratch[worker.cfr_frame_value_slot[frame_index]];
                    const auto strategy_scale =
                        config.strategy_weight * engine::chance_reach(reach) * engine::own_reach(reach, actor);
                    const auto regret_scale = actor == config.updating_player
                        ? engine::counterfactual_reach(reach, actor)
                        : 0.0f;

                    for (uint32_t local_index = 0; local_index < edges.size(); ++local_index) {
                        const auto action_index = edges[local_index].action_index;
                        strategy_deltas[action_index] += strategy_scale * edge_probabilities[local_index];
                        ++worker.diagnostics.strategy_updates;
                        if (actor == config.updating_player) {
                            regret_deltas[action_index] += regret_scale * (child_values[local_index] - node_value);
                            ++worker.diagnostics.regret_updates;
                        }
                    }
                    worker.node_utility[node_id] = node_value;
                } else if (kind == node_kind::chance) {
                    worker.node_utility[node_id] = worker.cfr_value_scratch[worker.cfr_frame_value_slot[frame_index]];
                }

                const auto completed_value_slot = worker.cfr_frame_value_slot[frame_index];
                --stack_size;
                propagate_child_value(completed_value_slot);
            }

            const auto root_utility = worker.node_utility[graph.root_node];
            worker.diagnostics.local_delta_entries_touched = worker.delta_buffer.entry_count();

            return traversal::traversal_result{
                .root_utility = root_utility,
                .diagnostics = worker.diagnostics,
                .root_node = graph.root_node,
                .scope_begin_node = 0,
                .scope_end_node = graph.node_count
            };
        }

        template <typename StrategyPolicy>
        [[nodiscard]] std::expected<traversal::traversal_result, iteration_error> traverse_heads_up_cfr_tree(
            const game_graph& graph,
            const solver_graph_annotations& annotations,
            const chance_event_table* chance_events,
            const regret_table& regrets,
            traversal::worker_context& worker,
            const iteration_config& config,
            const cfr_terminal_provider<2>& terminal_provider)
        {
            return traverse_cfr_tree<2, StrategyPolicy>(
                graph,
                annotations,
                chance_events,
                regrets,
                worker,
                config,
                terminal_provider);
        }

        template <std::size_t N, typename StrategyPolicy>
            requires (N >= 3)
        [[nodiscard]] std::expected<traversal::traversal_result, iteration_error> traverse_nway_cfr_tree(
            const game_graph& graph,
            const solver_graph_annotations& annotations,
            const chance_event_table* chance_events,
            const regret_table& regrets,
            traversal::worker_context& worker,
            const iteration_config& config,
            const cfr_terminal_provider<N>& terminal_provider)
        {
            return traverse_cfr_tree<N, StrategyPolicy>(
                graph,
                annotations,
                chance_events,
                regrets,
                worker,
                config,
                terminal_provider);
        }

        template <std::size_t N>
        [[nodiscard]] std::expected<void, iteration_error> validate_iteration_context(
            const cfr_solver_context<N>& context,
            const iteration_config& config,
            const std::span<traversal::worker_context> workers)
        {
            if (context.graph == nullptr
                || context.graph_annotations == nullptr
                || context.layout == nullptr
                || context.regrets == nullptr
                || context.strategy_sums == nullptr
                || workers.empty()) {
                return std::unexpected(iteration_error{iteration_error_kind::invalid_context});
            }
            if (config.update_mode != cfr_update_mode::alternating) {
                return std::unexpected(iteration_error{iteration_error_kind::unsupported_update_mode});
            }
            if (config.updating_player >= N) {
                return std::unexpected(iteration_error{iteration_error_kind::invalid_update_player});
            }
            if (!same_action_offsets(context.layout->action_offsets, context.regrets->action_offsets)
                || !same_action_offsets(context.layout->action_offsets, context.strategy_sums->action_offsets)) {
                return std::unexpected(iteration_error{iteration_error_kind::table_layout_mismatch});
            }

            const auto view = make_solver_graph_view<N>(*context.graph, *context.graph_annotations);
            if (auto metadata_result = validate_solver_graph_view(view, context.chance); !metadata_result) {
                return std::unexpected(iteration_error{
                    .kind = iteration_error_kind::graph_metadata,
                    .graph_metadata = metadata_result.error()
                });
            }
            if (context.chance != chance_mode::enumerate) {
                return std::unexpected(iteration_error{iteration_error_kind::unsupported_update_mode});
            }
            if (context.chance_events != nullptr) {
                if (auto chance_result = validate_chance_event_table(*context.graph, *context.chance_events); !chance_result) {
                    return std::unexpected(iteration_error{
                        .kind = iteration_error_kind::chance_table,
                        .chance_table = chance_result.error()
                    });
                }
            } else {
                for (uint32_t node_id = 0; node_id < context.graph->node_count; ++node_id) {
                    if (context.graph->node_types[node_id] == node_kind::chance) {
                        return std::unexpected(iteration_error{
                            .kind = iteration_error_kind::chance_table,
                            .chance_table = chance_table_error{chance_table_error_kind::missing_chance_event, node_id}
                        });
                    }
                }
            }
            if (auto terminal_result = validate_terminal_provider(*context.graph, context.terminal_provider); !terminal_result) {
                return std::unexpected(terminal_result.error());
            }

            return {};
        }

        [[nodiscard]] inline std::vector<scheduler::graph_partition> make_even_iteration_partitions(
            const game_graph& graph,
            const uint32_t target_count)
        {
            std::vector<scheduler::graph_partition> partitions;
            if (graph.node_count == 0u || target_count == 0u) {
                return partitions;
            }

            const auto partition_count = std::min(target_count, graph.node_count);
            partitions.reserve(partition_count);
            const auto base_size = graph.node_count / partition_count;
            const auto remainder = graph.node_count % partition_count;
            uint32_t begin_node = 0;

            for (uint32_t partition_id = 0; partition_id < partition_count; ++partition_id) {
                const auto node_count = base_size + (partition_id < remainder ? 1u : 0u);
                const auto end_node = begin_node + node_count;

                scheduler::graph_partition partition{};
                partition.begin_node = begin_node;
                partition.end_node = end_node;
                partition.node_count = node_count;
                partition.min_depth = std::numeric_limits<uint16_t>::max();

                for (uint32_t node_id = begin_node; node_id < end_node; ++node_id) {
                    partition.terminal_count += graph.is_terminal(node_id) ? 1u : 0u;
                    partition.action_count += graph.action_count(node_id);
                    partition.min_depth = std::min(partition.min_depth, graph.node_depth[node_id]);
                    partition.max_depth = std::max(partition.max_depth, graph.node_depth[node_id]);
                }

                if (partition.min_depth == std::numeric_limits<uint16_t>::max()) {
                    partition.min_depth = 0;
                }
                partition.estimated_work = std::max<uint64_t>(partition.action_count, partition.node_count);
                partitions.push_back(partition);
                begin_node = end_node;
            }

            return partitions;
        }
    }

    /**
     * Execute one CFR iteration through the shared graph, table, traversal, and reduction surfaces.
     */
    template <std::size_t N>
    [[nodiscard]] std::expected<iteration_result, iteration_error> run_cfr_iteration(
        cfr_solver_context<N>& context,
        const iteration_config config,
        std::span<traversal::worker_context> workers)
    {
        if (auto validation = detail::validate_iteration_context(context, config, workers); !validation) {
            return std::unexpected(validation.error());
        }

        auto& graph = *context.graph;
        auto& regrets = *context.regrets;
        auto& strategy_sums = *context.strategy_sums;
        using engine = cfr_engine<N>;

        std::vector<table_delta_buffer> scheduled_deltas(workers.size());
        for (uint32_t worker_id = 0; worker_id < workers.size(); ++worker_id) {
            auto& worker = workers[worker_id];
            if (auto prepared = traversal::prepare_worker_context(worker, graph, regrets); !prepared) {
                return std::unexpected(iteration_error{iteration_error_kind::table_layout_mismatch, worker_id});
            }
            if (auto prepared = detail::prepare_cfr_kernel_scratch<N>(graph, worker); !prepared) {
                return std::unexpected(prepared.error());
            }
            if (auto prepared = scheduled_deltas[worker_id].reset_layout(regrets.action_offsets); !prepared) {
                return std::unexpected(iteration_error{iteration_error_kind::table_layout_mismatch, worker_id});
            }
            scheduled_deltas[worker_id].reserve_sparse_entries(graph.infoset_count, regrets.value_count());
        }

        auto partitions = detail::make_even_iteration_partitions(graph, static_cast<uint32_t>(workers.size()));
        auto plan_result = scheduler::make_board_partition_plan(
            context.chance_events == nullptr
                ? 1u
                : std::max<uint32_t>(1u, chance_board_partition_count(*context.chance_events)),
            std::span<const scheduler::graph_partition>{partitions});
        if (!plan_result) {
            return std::unexpected(iteration_error{
                .kind = iteration_error_kind::scheduler,
                .scheduler = plan_result.error()
            });
        }
        const auto& plan = *plan_result;
        const auto delta_scale = 1.0f / static_cast<float>(plan.task_count());

        iteration_result result;
        result.per_worker_traversal_time_ns.assign(workers.size(), 0u);
        result.scheduler_task_count = plan.task_count();
        std::vector<float> root_utility_by_worker(workers.size(), 0.0f);
        std::vector<traversal::traversal_diagnostics> diagnostics_by_worker(workers.size());
        std::vector<uint32_t> traversals_by_worker(workers.size(), 0u);

        auto schedule_result = scheduler::run_board_partition_scheduler(
            plan,
            scheduler::scheduler_runtime_config{
                .worker_count = static_cast<uint32_t>(workers.size()),
                .task_chunk_size = 1u
            },
            [&](const scheduler::scheduler_worker_state& worker_state, const scheduler::board_partition_task&)
                -> std::expected<void, scheduler::scheduler_error> {
                auto& worker = workers[worker_state.worker_id];
                const auto traversal_start = std::chrono::steady_clock::now();
                auto traversal_result = config.variant == cfr_variant::cfr_plus
                    ? engine::template traverse<cfr_plus_regret_matching_policy>(
                        graph,
                        *context.graph_annotations,
                        context.chance_events,
                        regrets,
                        worker,
                        config,
                        context.terminal_provider)
                    : engine::template traverse<vanilla_regret_matching_policy>(
                        graph,
                        *context.graph_annotations,
                        context.chance_events,
                        regrets,
                        worker,
                        config,
                        context.terminal_provider);
                const auto traversal_end = std::chrono::steady_clock::now();
                result.per_worker_traversal_time_ns[worker_state.worker_id] += static_cast<uint64_t>(
                    std::chrono::duration_cast<std::chrono::nanoseconds>(traversal_end - traversal_start).count());

                if (!traversal_result) {
                    return std::unexpected(scheduler::scheduler_error{scheduler::scheduler_error_kind::task_failed});
                }

                root_utility_by_worker[worker_state.worker_id] += traversal_result->root_utility * delta_scale;
                detail::add_diagnostics(diagnostics_by_worker[worker_state.worker_id], traversal_result->diagnostics);
                add_scaled_delta_buffer(scheduled_deltas[worker_state.worker_id], worker.delta_buffer, delta_scale);
                ++traversals_by_worker[worker_state.worker_id];
                return std::expected<void, scheduler::scheduler_error>{};
            });
        if (!schedule_result) {
            return std::unexpected(iteration_error{
                .kind = iteration_error_kind::scheduler,
                .scheduler = schedule_result.error()
            });
        }

        result.scheduler_tasks_executed = schedule_result->tasks_executed;
        result.scheduler_estimated_work = schedule_result->estimated_work;
        for (uint32_t worker_id = 0; worker_id < workers.size(); ++worker_id) {
            result.root_utility += root_utility_by_worker[worker_id];
            detail::add_diagnostics(result.diagnostics, diagnostics_by_worker[worker_id]);
            result.traversals_run += traversals_by_worker[worker_id];
        }
        for (uint32_t worker_id = 0; worker_id < workers.size(); ++worker_id) {
            workers[worker_id].delta_buffer = std::move(scheduled_deltas[worker_id]);
        }

        std::vector<const traversal::worker_context*> worker_ptrs;
        worker_ptrs.reserve(workers.size());
        for (const auto& worker : workers) {
            worker_ptrs.push_back(&worker);
        }

        const auto reduction_start = std::chrono::steady_clock::now();
        if (auto reduction = apply_worker_reductions(
                regrets,
                strategy_sums,
                make_deterministic_reduction_plan(static_cast<uint32_t>(worker_ptrs.size())),
                std::span<const traversal::worker_context* const>{worker_ptrs},
                context.reduction,
                context.owner_map,
                &result.reduction,
                config.variant);
            !reduction) {
            return std::unexpected(reduction.error());
        }
        const auto reduction_end = std::chrono::steady_clock::now();
        result.reduction_time_ns = static_cast<uint64_t>(
            std::chrono::duration_cast<std::chrono::nanoseconds>(reduction_end - reduction_start).count());

        result.diagnostics.reduction_values = result.reduction.reduction_values;
        result.quality = compute_quality_diagnostics(
            graph,
            *context.graph_annotations,
            regrets,
            strategy_sums,
            static_cast<uint32_t>(N));
        result.workers_used = static_cast<uint32_t>(workers.size());
        return result;
    }

}
