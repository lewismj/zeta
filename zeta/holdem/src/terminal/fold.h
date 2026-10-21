#pragma once

#include "terminal/showdown.h"

namespace zeta::holdem {
    namespace detail {

        template <std::size_t N>
        [[nodiscard]] std::array<utility, N> fold_payoff(const terminal_state<N>& state) noexcept {
            std::array<utility, N> payoff{};
            for (std::size_t seat = 0; seat < N; ++seat) {
                payoff[seat] = -state.context.contribution[seat];
            }

            for (std::size_t layer_index = 0; layer_index < state.pot_layers.size(); ++layer_index) {
                const auto& layer = state.pot_layers[layer_index];
                uint16_t winner_count = 0;
                for (std::size_t seat = 0; seat < N; ++seat) {
                    if (!state.folded[seat] && layer.eligible_mask[seat]) {
                        ++winner_count;
                    }
                }
                if (winner_count == 0) {
                    continue;
                }

                const auto award = rake_adjusted_layer_amount(state.context, state.pot_layers, layer_index)
                    / static_cast<utility>(winner_count);
                for (std::size_t seat = 0; seat < N; ++seat) {
                    if (!state.folded[seat] && layer.eligible_mask[seat]) {
                        payoff[seat] += award;
                    }
                }
            }
            return payoff;
        }

        template <std::size_t N>
        void accumulate_fold_exact_for_hero(
            terminal_values<N>& values,
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& reach,
            const std::array<utility, N>& payoff,
            const std::size_t hero_seat,
            const combination_index hero_combo,
            const std::size_t seat,
            const card_mask used_cards,
            const accumulator opponent_weight
        ) noexcept {
            if (seat == N) {
                values[hero_seat][hero_combo] += static_cast<terminal_value>(opponent_weight * payoff[hero_seat]);
                return;
            }
            if (seat == hero_seat) {
                accumulate_fold_exact_for_hero(
                    values,
                    cache,
                    reach,
                    payoff,
                    hero_seat,
                    hero_combo,
                    seat + 1,
                    used_cards,
                    opponent_weight
                );
                return;
            }

            const auto& index = reach[seat];
            for (uint16_t offset = 0; offset < index.active_count; ++offset) {
                const auto combo = index.active_indices[offset];
                if ((cache.masks[combo] & used_cards) != 0) {
                    continue;
                }
                accumulate_fold_exact_for_hero(
                    values,
                    cache,
                    reach,
                    payoff,
                    hero_seat,
                    hero_combo,
                    seat + 1,
                    used_cards | cache.masks[combo],
                    opponent_weight * index.weights[combo]
                );
            }
        }
    }

    template <std::size_t N>
    [[nodiscard]] terminal_values<N> evaluate_fold_values_exact(
        const river_terminal_cache& cache,
        const std::array<river_reach_index, N>& reach,
        const terminal_state<N>& state
    ) noexcept {
        terminal_values<N> values{};
        if (state.pot_layers.empty()) {
            return values;
        }

        for (std::size_t seat = 0; seat < N; ++seat) {
            assert(cache.board_hash == reach[seat].board_hash);
        }

        const auto payoff = detail::fold_payoff(state);
        for (std::size_t hero_seat = 0; hero_seat < N; ++hero_seat) {
            const auto& hero_index = reach[hero_seat];
            for (uint16_t offset = 0; offset < hero_index.active_count; ++offset) {
                const auto hero_combo = hero_index.active_indices[offset];
                detail::accumulate_fold_exact_for_hero(
                    values,
                    cache,
                    reach,
                    payoff,
                    hero_seat,
                    hero_combo,
                    0,
                    cache.masks[hero_combo],
                    1.0
                );
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
            auto state = make_fold_terminal_state(context, folded);
            return evaluate_fold_values_exact(cache, reach, state);
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

}
