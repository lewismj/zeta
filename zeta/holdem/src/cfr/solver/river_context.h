#pragma once

#include "cfr/traversal/traversal.h"
#include "terminal/terminal.h"

#include <array>
#include <utility>
#include <vector>

namespace zeta::holdem::cfr::solver {

    /**
     * River-board terminal state shared across CFR traversals.
     *
     * The reach indices are derived from ranges and board blockers. They are
     * intentionally materialized once for a river board and then reused by
     * traversal policies; rebuilding them inside the terminal-node loop would
     * dominate terminal evaluation cost.
     */
    struct river_solver_context {
        river_terminal_cache cache{};
        terminal_workspace<2> workspace{};
        terminal_state_table<2> terminal_states{};
        std::vector<traversal::river_terminal_leaf> terminal_leaves;

        /**
         * Create a river terminal policy view over the cached reach indices.
         */
        [[nodiscard]] traversal::river_terminal_leaf_policy terminal_policy(
            const ::zeta::holdem::heads_up_player perspective,
            const ::zeta::holdem::combination_index combo) const noexcept
        {
            return traversal::river_terminal_leaf_policy{
                .river_cache = &cache,
                .reach_indices = workspace.reach,
                .terminal_leaves = terminal_leaves,
                .terminal_states = terminal_states.view(),
                .perspective = perspective,
                .combo = combo
            };
        }
    };

    /**
     * Build cached river terminal state from immutable board/range inputs.
     */
    [[nodiscard]] inline river_solver_context make_river_solver_context(
        const ::zeta::holdem::board river,
        const std::array<::zeta::holdem::reach_vector, 2>& ranges,
        ::zeta::holdem::terminal_state_table<2> terminal_states,
        std::vector<traversal::river_terminal_leaf> terminal_leaves)
    {
        river_solver_context context;
        context.cache = ::zeta::holdem::make_river_terminal_cache(river);
        context.workspace.materialize(context.cache, ranges);
        context.terminal_states = std::move(terminal_states);
        context.terminal_leaves = std::move(terminal_leaves);
        return context;
    }

}
