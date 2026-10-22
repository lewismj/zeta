#pragma once

#include "terminal/fold.h"

namespace zeta::holdem {

    /**
     * Dispatch layer: select kernel family at compile-time by player count.
     * - N == 2: heads-up exact kernel family
     * - N > 2 : multiplayer exact kernel family
     */
    template <std::size_t N>
    struct terminal_engine {
        [[nodiscard]] static constexpr terminal_kernel_family kernel_family() noexcept {
            if constexpr (N == 2) {
                return terminal_kernel_family::heads_up_exact;
            } else {
                return terminal_kernel_family::multiplayer;
            }
        }

        [[nodiscard]] static constexpr bool is_heads_up_exact() noexcept {
            return kernel_family() == terminal_kernel_family::heads_up_exact;
        }

        /** Showdown dispatch to the exact kernel family (heads-up and N-way). */
        [[nodiscard]] auto evaluate_showdown(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& reach,
            const terminal_context<N>& context
        ) const noexcept {
            return ::zeta::holdem::evaluate_showdown(cache, reach, context);
        }

        [[nodiscard]] terminal_values<N> evaluate_showdown_values(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& reach,
            const terminal_context<N>& context
        ) const noexcept {
            return ::zeta::holdem::evaluate_showdown_values(cache, reach, context);
        }

        /** Terminal-state dispatch returns player-indexed utility vectors. */
        [[nodiscard]] terminal_values<N> evaluate_terminal_values(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& reach,
            const terminal_state<N>& state
        ) const noexcept {
            switch (state.kind) {
                case terminal_state_kind::showdown:
                    if constexpr (N == 2) {
                        return evaluate_showdown_values(cache, reach, state.context);
                    } else {
                        return ::zeta::holdem::evaluate_showdown_values_exact(cache, reach, state);
                    }
                case terminal_state_kind::fold:
                    if constexpr (N == 2) {
                        return evaluate_fold_values(cache, reach, state.context, state.folded);
                    } else {
                        return ::zeta::holdem::evaluate_fold_values_exact(cache, reach, state);
                    }
                case terminal_state_kind::none:
                    assert(false);
                    return {};
            }
            assert(false);
            return {};
        }

        /**
         * Heads-up terminal dispatch over two separately held reach indices, so callers
         * can pass a long-lived index by reference instead of copying it into an array.
         */
        [[nodiscard]] terminal_values<N> evaluate_terminal_values(
            const river_terminal_cache& cache,
            const river_reach_index& oop_index,
            const river_reach_index& ip_index,
            const terminal_state<N>& state
        ) const noexcept requires (N == 2) {
            switch (state.kind) {
                case terminal_state_kind::showdown:
                    return ::zeta::holdem::evaluate_showdown_values(cache, oop_index, ip_index, state.context);
                case terminal_state_kind::fold:
                    return ::zeta::holdem::evaluate_fold_values(
                        cache, oop_index, ip_index, state.context,
                        state.folded.oop_folded ? heads_up_player::oop : heads_up_player::ip);
                case terminal_state_kind::none:
                    assert(false);
                    return {};
            }
            assert(false);
            return {};
        }

        /** Fold dispatch: heads-up exact path for N == 2, exact N-way fold for N > 2. */
        [[nodiscard]] terminal_values<N> evaluate_fold_values(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& reach,
            const terminal_context<N>& context,
            const folded_mask<N>& folded
        ) const noexcept {
            return ::zeta::holdem::evaluate_fold_values(cache, reach, context, folded);
        }
    };
}
