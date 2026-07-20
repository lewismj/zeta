#pragma once

#include "cfr/graph/graph.h"
#include "cfr/solver/infoset_planning.h"
#include "cfr/solver/river_context.h"
#include "cfr/tables/regret_table.h"
#include "cfr/tables/strategy_table.h"

#include <cstdint>
#include <expected>
#include <ostream>
#include <utility>

namespace zeta::holdem::cfr::solver {

    /**
     * Owning CFR state for repeated traversal over one river-board context.
     *
     * The graph, terminal cache, reach indices, and global tables have stable
     * lifetimes for the solver loop. Iterations should traverse this context
     * directly rather than rebuilding terminal reach indices or table layouts.
     */
    struct cfr_context {
        game_graph graph;
        river_solver_context river;
        action_table_layout layout;
        regret_table regrets;
        strategy_sum_table strategy_sums;
    };

    enum class cfr_context_planning_error_kind : uint8_t {
        table_layout,
        memory_plan
    };

    struct cfr_context_planning_error {
        cfr_context_planning_error_kind kind{};
        table_layout_error table_layout{};
        cfr_memory_plan_error memory_plan{};
    };

    [[nodiscard]] constexpr const char* to_string(const cfr_context_planning_error_kind kind) noexcept
    {
        using enum cfr_context_planning_error_kind;
        switch (kind) {
            case table_layout: return "cfr_context_planning_error_kind::table_layout";
            case memory_plan:  return "cfr_context_planning_error_kind::memory_plan";
        }
        return "cfr_context_planning_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const cfr_context_planning_error_kind kind)
    {
        return os << to_string(kind);
    }

    struct cfr_context_plan {
        action_table_layout layout;
        cfr_memory_estimate memory;
    };

    /**
     * Plan table layout and memory use before allocating CFR tables.
     */
    [[nodiscard]] inline std::expected<cfr_context_plan, cfr_context_planning_error> plan_cfr_context(
        const game_graph& graph,
        const cfr_memory_plan_options options = {},
        const cfr_memory_plan_limits limits = {})
    {
        auto layout_result = make_action_table_layout(graph);
        if (!layout_result) {
            return std::unexpected(cfr_context_planning_error{
                .kind = cfr_context_planning_error_kind::table_layout,
                .table_layout = layout_result.error()
            });
        }

        auto memory_result = estimate_cfr_memory(graph, *layout_result, options, limits);
        if (!memory_result) {
            return std::unexpected(cfr_context_planning_error{
                .kind = cfr_context_planning_error_kind::memory_plan,
                .memory_plan = memory_result.error()
            });
        }

        return cfr_context_plan{
            .layout = std::move(*layout_result),
            .memory = *memory_result
        };
    }

    /**
     * Build an owning CFR context and derive table storage from the graph.
     */
    [[nodiscard]] inline std::expected<cfr_context, table_layout_error> make_cfr_context(
        game_graph graph,
        river_solver_context river)
    {
        auto layout_result = make_action_table_layout(graph);
        if (!layout_result) {
            return std::unexpected(layout_result.error());
        }

        cfr_context context;
        context.graph = std::move(graph);
        context.river = std::move(river);
        context.layout = std::move(*layout_result);
        context.regrets = regret_table(context.layout);
        context.strategy_sums = strategy_sum_table(context.layout);
        return context;
    }

    /**
     * Build an owning CFR context only after the memory plan satisfies limits.
     */
    [[nodiscard]] inline std::expected<cfr_context, cfr_context_planning_error> make_planned_cfr_context(
        game_graph graph,
        river_solver_context river,
        const cfr_memory_plan_options options = {},
        const cfr_memory_plan_limits limits = {})
    {
        auto plan_result = plan_cfr_context(graph, options, limits);
        if (!plan_result) {
            return std::unexpected(plan_result.error());
        }

        cfr_context context;
        context.graph = std::move(graph);
        context.river = std::move(river);
        context.layout = std::move(plan_result->layout);
        context.regrets = regret_table(context.layout);
        context.strategy_sums = strategy_sum_table(context.layout);
        return context;
    }

}
