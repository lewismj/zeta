#pragma once

#include "terminal/fold.h"

namespace zeta::holdem {

    // Stage 10 dispatch layer: select kernel family at compile-time by player count.
    // - N == 2: heads-up exact kernel family
    // - N > 2 : multiplayer kernel family (current implementation under this family is sampled)
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

        // Showdown dispatch: currently only the heads-up exact kernel is implemented.
        [[nodiscard]] auto evaluate_showdown(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& reach,
            const terminal_context<N>& context
        ) const noexcept {
            if constexpr (N == 2) {
                return ::zeta::holdem::evaluate_showdown(cache, reach, context);
            } else {
                static_assert(N == 2, "multiplayer showdown kernel not implemented");
            }
        }

        [[nodiscard]] auto evaluate_showdown(
            terminal_workspace<N>& workspace,
            const river_terminal_cache& cache,
            const std::array<reach_vector, N>& ranges,
            const terminal_context<N>& context
        ) const noexcept {
            if constexpr (N == 2) {
                return ::zeta::holdem::evaluate_showdown(workspace, cache, ranges, context);
            } else {
                static_assert(N == 2, "multiplayer showdown kernel not implemented");
            }
        }

        [[nodiscard]] auto evaluate_showdown_values(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& reach,
            const terminal_context<N>& context,
            const uint16_t samples_per_combo = 64
        ) const noexcept {
            if constexpr (N == 2) {
                return ::zeta::holdem::evaluate_showdown(cache, reach, context).values;
            } else {
                return ::zeta::holdem::evaluate_showdown_values_multiplayer_sampled(cache, reach, context, samples_per_combo);
            }
        }

        [[nodiscard]] auto evaluate_showdown_values(
            terminal_workspace<N>& workspace,
            const river_terminal_cache& cache,
            const std::array<reach_vector, N>& ranges,
            const terminal_context<N>& context,
            const uint16_t samples_per_combo = 64
        ) const noexcept {
            if constexpr (N == 2) {
                return ::zeta::holdem::evaluate_showdown_values(workspace, cache, ranges, context);
            } else {
                workspace.materialize(cache, ranges);
                return ::zeta::holdem::evaluate_showdown_values_multiplayer_sampled(cache, workspace.reach, context, samples_per_combo);
            }
        }

        // Fold dispatch: heads-up exact path for N == 2, generic N-way fold for N > 2.
        [[nodiscard]] terminal_values<N> evaluate_fold_values(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& reach,
            const terminal_context<N>& context,
            const folded_mask<N>& folded
        ) const noexcept {
            return ::zeta::holdem::evaluate_fold_values(cache, reach, context, folded);
        }

        [[nodiscard]] terminal_values<N> evaluate_fold_values(
            terminal_workspace<N>& workspace,
            const river_terminal_cache& cache,
            const std::array<reach_vector, N>& ranges,
            const terminal_context<N>& context,
            const folded_mask<N>& folded
        ) const noexcept {
            return ::zeta::holdem::evaluate_fold_values(workspace, cache, ranges, context, folded);
        }
    };

    // Heads-up convenience overloads for engine callers that keep the legacy two-range style.
    template <>
    struct terminal_engine<2> {
        [[nodiscard]] static constexpr terminal_kernel_family kernel_family() noexcept {
            return terminal_kernel_family::heads_up_exact;
        }

        [[nodiscard]] static constexpr bool is_heads_up_exact() noexcept {
            return true;
        }

        [[nodiscard]] terminal_result<2> evaluate_showdown(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, 2>& reach,
            const terminal_context<2>& context
        ) const noexcept {
            return ::zeta::holdem::evaluate_showdown(cache, reach, context);
        }

        [[nodiscard]] terminal_result<2> evaluate_showdown(
            terminal_workspace<2>& workspace,
            const river_terminal_cache& cache,
            const std::array<reach_vector, 2>& ranges,
            const terminal_context<2>& context
        ) const noexcept {
            return ::zeta::holdem::evaluate_showdown(workspace, cache, ranges, context);
        }

        [[nodiscard]] terminal_result<2> evaluate_showdown(
            terminal_workspace<2>& workspace,
            const river_terminal_cache& cache,
            const reach_vector& oop_reach,
            const reach_vector& ip_reach,
            const terminal_context<2>& context
        ) const noexcept {
            return ::zeta::holdem::evaluate_showdown(workspace, cache, oop_reach, ip_reach, context);
        }

        [[nodiscard]] terminal_values<2> evaluate_showdown_values(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, 2>& reach,
            const terminal_context<2>& context,
            const uint16_t samples_per_combo = 64
        ) const noexcept {
            (void)samples_per_combo;
            return ::zeta::holdem::evaluate_showdown(cache, reach, context).values;
        }

        [[nodiscard]] terminal_values<2> evaluate_showdown_values(
            const river_terminal_cache& cache,
            const river_reach_index& oop_index,
            const river_reach_index& ip_index,
            const terminal_context<2>& context,
            const uint16_t samples_per_combo = 64
        ) const noexcept {
            (void)samples_per_combo;
            return ::zeta::holdem::evaluate_showdown_values(cache, oop_index, ip_index, context);
        }

        [[nodiscard]] terminal_values<2> evaluate_showdown_values(
            terminal_workspace<2>& workspace,
            const river_terminal_cache& cache,
            const std::array<reach_vector, 2>& ranges,
            const terminal_context<2>& context,
            const uint16_t samples_per_combo = 64
        ) const noexcept {
            (void)samples_per_combo;
            return ::zeta::holdem::evaluate_showdown_values(workspace, cache, ranges, context);
        }

        [[nodiscard]] terminal_values<2> evaluate_showdown_values(
            terminal_workspace<2>& workspace,
            const river_terminal_cache& cache,
            const reach_vector& oop_reach,
            const reach_vector& ip_reach,
            const terminal_context<2>& context,
            const uint16_t samples_per_combo = 64
        ) const noexcept {
            (void)samples_per_combo;
            return ::zeta::holdem::evaluate_showdown_values(workspace, cache, std::array<reach_vector, 2>{oop_reach, ip_reach}, context);
        }

        [[nodiscard]] terminal_values<2> evaluate_fold_values(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, 2>& reach,
            const terminal_context<2>& context,
            const folded_mask<2>& folded
        ) const noexcept {
            return ::zeta::holdem::evaluate_fold_values(cache, reach, context, folded);
        }

        [[nodiscard]] terminal_values<2> evaluate_fold_values(
            terminal_workspace<2>& workspace,
            const river_terminal_cache& cache,
            const std::array<reach_vector, 2>& ranges,
            const terminal_context<2>& context,
            const folded_mask<2>& folded
        ) const noexcept {
            return ::zeta::holdem::evaluate_fold_values(workspace, cache, ranges, context, folded);
        }
    };
}
