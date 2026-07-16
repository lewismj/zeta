#pragma once

#include "terminal/showdown.h"

namespace zeta::holdem {
    /**
     * Generic N-way fold kernel: for each active player, accumulate compatible
     * mass from all other active opponents * constant payoff per opponent.
     *
     * Pseudocode:
     * for active_player in players:
     *     if folded[active_player]:
     *         values[active_player][:] = 0
     *     else:
     *         for combo in active_player_combos:
     *             for opponent in active_opponents excluding active_player:
     *                 total_compatible += compatible_mass(opponent, combo)
     *             value[combo] = total_compatible * payoff_per_compatible_unit
     *
     * For heads-up: this reduces exactly to the current two-stream kernel.
     */
    template <std::size_t N>
    [[nodiscard]] inline_always terminal_values<N> evaluate_fold_values_generic(
        const river_terminal_cache& cache,
        const std::array<river_reach_index, N>& reach,
        const terminal_context<N>& context,
        const folded_mask<N>& folded
    ) noexcept {
        terminal_values<N> values{};
        
        /** For each active non-folded player. */
        for (std::size_t active_seat = 0; active_seat < N; ++active_seat) {
            if (folded[active_seat]) {
                /** Folded players get zero values; skip initialization because it is already zero. */
                continue;
            }
            
            /** Active player receives payoff from all remaining active opponents. */
            for (uint16_t combo_offset = 0; combo_offset < reach[active_seat].active_count; ++combo_offset) {
                const auto combo = reach[active_seat].active_indices[combo_offset];
                accumulator total_compatible = 0.0;
                
                /** Accumulate compatible mass from each active opponent. */
                for (std::size_t opponent_seat = 0; opponent_seat < N; ++opponent_seat) {
                    if (opponent_seat != active_seat && !folded[opponent_seat]) {
                        total_compatible += compatible_mass(cache, reach[opponent_seat], combo);
                    }
                }
                
                /**
                 * Store total compatible mass; payoff multiplication is handled
                 * by specialized paths where full pot accounting is available.
                 */
                values[active_seat][combo] = static_cast<terminal_value>(total_compatible);
            }
        }
        
        return values;
    }

    /** Heads-up (2-player) fold kernel: compatible opponent mass * constant payoff. */
    [[nodiscard]] inline_always terminal_values<2> evaluate_fold_values_heads_up(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        const terminal_context<2>& context,
        const heads_up_player folded
    ) noexcept {
        assert(cache.board_hash == oop_index.board_hash);
        assert(cache.board_hash == ip_index.board_hash);

        terminal_values<2> values{};
        const auto payoff = payoff_for_fold(heads_up_pot(context), folded);

        for (uint16_t offset = 0; offset < oop_index.active_count; ++offset) {
            const auto combo = oop_index.active_indices[offset];
            const auto compatible = compatible_mass(cache, ip_index, combo);
            values[heads_up_player::oop][combo] = static_cast<terminal_value>(compatible * payoff.oop);
        }

        for (uint16_t offset = 0; offset < ip_index.active_count; ++offset) {
            const auto combo = ip_index.active_indices[offset];
            const auto compatible = compatible_mass(cache, oop_index, combo);
            values[heads_up_player::ip][combo] = static_cast<terminal_value>(compatible * payoff.ip);
        }

        return values;
    }

    /**
     * Generic fold entry point with bitset support for N-way folded masks.
     * The generic kernel uses folded_mask<N> where bit i == true means seat i is folded.
     * Heads-up uses the specialized folded_mask<2> with direct boolean fields for performance.
     */
    template <std::size_t N>
    [[nodiscard]] terminal_values<N> evaluate_fold_values(
        const river_terminal_cache& cache,
        const std::array<river_reach_index, N>& reach,
        const terminal_context<N>& context,
        const folded_mask<N>& folded
    ) noexcept {
        if constexpr (N == 2) {
            /** Heads-up fast path: extract from the specialized folded_mask<2> struct. */
            const auto folded_player = folded.oop_folded ? heads_up_player::oop : heads_up_player::ip;
            return evaluate_fold_values_heads_up(cache, reach[0], reach[1], context, folded_player);
        } else {
            return evaluate_fold_values_generic(cache, reach, context, folded);
        }
    }

    /**
     * Heads-up convenience overload retained for compatibility.
     * Converts heads_up_player to folded_mask<2> using the specialized factory.
     */
    template <std::size_t N>
    [[nodiscard]] terminal_values<N> evaluate_fold_values(
        const river_terminal_cache& cache,
        const std::array<river_reach_index, N>& reach,
        const terminal_context<N>& context,
        const heads_up_player folded_player
    ) noexcept {
        static_assert(N == 2, "heads_up_player parameter only valid for N == 2");
        if constexpr (N == 2) {
            const auto folded = folded_mask<2>::from_folded_player(folded_player);
            return evaluate_fold_values(cache, reach, context, folded);
        }
    }

    [[nodiscard]] inline_always terminal_values<2> evaluate_fold_values(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        const terminal_context<2>& context,
        const heads_up_player folded
    ) noexcept {
        return evaluate_fold_values_heads_up(cache, oop_index, ip_index, context, folded);
    }

    [[nodiscard]] inline_always terminal_values<2> evaluate_fold_values(
        const river_terminal_cache& cache,
        const reach_vector& oop_reach,
        const reach_vector& ip_reach,
        const terminal_context<2>& context,
        const heads_up_player folded
    ) noexcept {
        const auto oop_index = make_river_reach_index(cache, oop_reach);
        const auto ip_index = make_river_reach_index(cache, ip_reach);
        return evaluate_fold_values(cache, oop_index, ip_index, context, folded);
    }

    /** Workspace-based fold API: caller provides ranges, workspace owns reach indices. */
    template <std::size_t N>
    [[nodiscard]] terminal_values<N> evaluate_fold_values(
        terminal_workspace<N>& workspace,
        const river_terminal_cache& cache,
        const std::array<reach_vector, N>& ranges,
        const terminal_context<N>& context,
        const folded_mask<N>& folded
    ) noexcept {
        /** Materialize ranges into workspace reach indices. */
        workspace.materialize(cache, ranges);
        
        /** Evaluate using the materialized indices. */
        return evaluate_fold_values(cache, workspace.reach, context, folded);
    }

    /** Heads-up workspace specialization. */
    [[nodiscard]] inline_always terminal_values<2> evaluate_fold_values(
        terminal_workspace<2>& workspace,
        const river_terminal_cache& cache,
        const reach_vector& oop_reach,
        const reach_vector& ip_reach,
        const terminal_context<2>& context,
        const heads_up_player folded
    ) noexcept {
        return evaluate_fold_values(workspace, cache, std::array<reach_vector, 2>{oop_reach, ip_reach}, context, folded_mask<2>::from_folded_player(folded));
    }

}
