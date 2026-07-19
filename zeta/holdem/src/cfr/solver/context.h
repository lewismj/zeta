#pragma once

#include "cfr/graph/graph.h"
#include "cfr/solver/river_context.h"
#include "cfr/tables/regret_table.h"
#include "cfr/tables/strategy_table.h"

#include <expected>
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

}
