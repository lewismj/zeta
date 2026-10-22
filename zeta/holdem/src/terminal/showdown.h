#pragma once

#include "terminal/reach_index.h"

namespace zeta::holdem {
    inline_always void accumulate_showdown_bucket_values(
        terminal_result<2>& result,
        const river_terminal_cache& cache,
        const heads_up_player hero_player,
        const river_reach_index& hero_index,
        const river_reach_index& opponent_index,
        const river_rank_bucket& hero_bucket,
        const accumulator opponent_lower_total,
        const std::array<accumulator, 52>& opponent_lower_by_card,
        const river_rank_bucket* const opponent_equal_bucket,
        const utility win_component,
        const utility tie_component,
        const utility loss_component,
        const bool record_matchups
    ) noexcept {
        const accumulator opponent_equal_total = opponent_equal_bucket == nullptr ? 0.0 : opponent_equal_bucket->total_mass;
        const accumulator opponent_higher_total =
            static_cast<accumulator>(opponent_index.total_live_mass) - opponent_lower_total - opponent_equal_total;

        for (uint16_t offset = hero_bucket.begin; offset < hero_bucket.end; ++offset) {
            const auto hero_combo = hero_index.active_indices[offset];
            const auto [first, second] = cache.cards[hero_combo];

            const accumulator equal_first = opponent_equal_bucket == nullptr
                ? 0.0
                : bucket_card_mass(opponent_index, *opponent_equal_bucket, first);
            const accumulator equal_second = opponent_equal_bucket == nullptr
                ? 0.0
                : bucket_card_mass(opponent_index, *opponent_equal_bucket, second);

            const auto lower = compatible_mass_from_bucket(
                opponent_lower_total,
                opponent_lower_by_card[first],
                opponent_lower_by_card[second],
                0.0
            );
            const auto equal = compatible_mass_from_bucket(
                opponent_equal_total,
                equal_first,
                equal_second,
                opponent_index.weights[hero_combo]
            );
            const auto higher = compatible_mass_from_bucket(
                opponent_higher_total,
                static_cast<accumulator>(opponent_index.mass_by_card[first]) - opponent_lower_by_card[first] - equal_first,
                static_cast<accumulator>(opponent_index.mass_by_card[second]) - opponent_lower_by_card[second] - equal_second,
                0.0
            );

            const accumulator value = lower * win_component + equal * tie_component + higher * loss_component;
            result.values[hero_player][hero_combo] = static_cast<terminal_value>(value);

            const accumulator hero_weight = hero_index.weights[hero_combo];
            if (record_matchups) {
                result.summary.oop_wins += hero_weight * lower;
                result.summary.ties += hero_weight * equal;
                result.summary.ip_wins += hero_weight * higher;
                result.summary.matchup_weight += hero_weight * (lower + equal + higher);
                result.summary.oop_ev += hero_weight * value;
            } else {
                result.summary.ip_ev += hero_weight * value;
            }
        }
    }

    /**
     * Heads-up (2-player) exact showdown kernel: a two-stream rank-bucket merge.
     * This is the hand-tuned fast path; the generic evaluate_showdown<N> dispatches
     * here for N == 2. reach[0] == oop, reach[1] == ip.
     */
    [[nodiscard]] inline_always terminal_result<2> evaluate_showdown_heads_up(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        const terminal_context<2>& context
    ) noexcept {
        assert(cache.board_hash == oop_index.board_hash);
        assert(cache.board_hash == ip_index.board_hash);

        terminal_result<2> result{};
        const auto pot = heads_up_pot(context);
        const auto oop_win = payoff_for_oop_win(pot);
        const auto ip_win = payoff_for_ip_win(pot);
        const auto tie = payoff_for_tie(pot);

        std::size_t oop_bucket_idx = 0;
        std::size_t ip_bucket_idx = 0;
        accumulator oop_lower_total = 0.0;
        accumulator ip_lower_total = 0.0;
        std::array<accumulator, 52> oop_lower_by_card{};
        std::array<accumulator, 52> ip_lower_by_card{};

        while (oop_bucket_idx < oop_index.unique_rank_count || ip_bucket_idx < ip_index.unique_rank_count) {
            const rank_key oop_rank = oop_bucket_idx < oop_index.unique_rank_count
                ? oop_index.rank_buckets[oop_bucket_idx].rank
                : static_cast<rank_key>(UINT16_MAX);
            const rank_key ip_rank = ip_bucket_idx < ip_index.unique_rank_count
                ? ip_index.rank_buckets[ip_bucket_idx].rank
                : static_cast<rank_key>(UINT16_MAX);
            const rank_key rank = std::min(oop_rank, ip_rank);

            const river_rank_bucket* const oop_equal = oop_rank == rank ? &oop_index.rank_buckets[oop_bucket_idx] : nullptr;
            const river_rank_bucket* const ip_equal = ip_rank == rank ? &ip_index.rank_buckets[ip_bucket_idx] : nullptr;

            if (oop_equal != nullptr) {
                accumulate_showdown_bucket_values(
                    result,
                    cache,
                    heads_up_player::oop,
                    oop_index,
                    ip_index,
                    *oop_equal,
                    ip_lower_total,
                    ip_lower_by_card,
                    ip_equal,
                    oop_win.oop,
                    tie.oop,
                    ip_win.oop,
                    true
                );
            }

            if (ip_equal != nullptr) {
                accumulate_showdown_bucket_values(
                    result,
                    cache,
                    heads_up_player::ip,
                    ip_index,
                    oop_index,
                    *ip_equal,
                    oop_lower_total,
                    oop_lower_by_card,
                    oop_equal,
                    ip_win.ip,
                    tie.ip,
                    oop_win.ip,
                    false
                );
            }

            if (oop_equal != nullptr) {
                oop_lower_total += oop_equal->total_mass;
                add_bucket_cards(oop_lower_by_card, oop_index, *oop_equal);
                ++oop_bucket_idx;
            }
            if (ip_equal != nullptr) {
                ip_lower_total += ip_equal->total_mass;
                add_bucket_cards(ip_lower_by_card, ip_index, *ip_equal);
                ++ip_bucket_idx;
            }
        }

        return result;
    }

    template <std::size_t N>
    [[nodiscard]] terminal_values<N> evaluate_showdown_values_exact(
        const river_terminal_cache& cache,
        const std::array<river_reach_index, N>& reach,
        const terminal_state<N>& state
    ) noexcept;

    /**
     * Generic entry point: player count is a compile-time constant.
     */
    template <std::size_t N>
    [[nodiscard]] terminal_result<N> evaluate_showdown(
        const river_terminal_cache& cache,
        const std::array<river_reach_index, N>& reach,
        const terminal_context<N>& context
    ) noexcept {
        if constexpr (N == 2) {
            return evaluate_showdown_heads_up(cache, reach[0], reach[1], context);
        } else {
            terminal_result<N> result{};
            const auto state = make_showdown_terminal_state(context);
            result.values = evaluate_showdown_values_exact(cache, reach, state);
            for (std::size_t seat = 0; seat < N; ++seat) {
                accumulator seat_ev = 0.0;
                const auto& index = reach[seat];
                for (uint16_t offset = 0; offset < index.active_count; ++offset) {
                    const auto combo = index.active_indices[offset];
                    seat_ev += static_cast<accumulator>(index.weights[combo])
                        * static_cast<accumulator>(result.values[seat][combo]);
                }
                result.summary.seat_ev[seat] = seat_ev;
            }
            return result;
        }
    }

    namespace detail {

        template <std::size_t N>
        [[nodiscard]] std::array<utility, N> showdown_payoff_for_sample(
            const std::array<rank_key, N>& ranks,
            const terminal_state<N>& state
        ) noexcept {
            std::array<utility, N> payoff{};
            for (std::size_t seat = 0; seat < N; ++seat) {
                payoff[seat] = -state.context.contribution[seat];
            }

            for (std::size_t layer_index = 0; layer_index < state.pot_layers.size(); ++layer_index) {
                const auto& layer = state.pot_layers[layer_index];
                rank_key best_rank = 0;
                for (std::size_t seat = 0; seat < N; ++seat) {
                    if (!state.folded[seat] && layer.eligible_mask[seat] && ranks[seat] > best_rank) {
                        best_rank = ranks[seat];
                    }
                }
                if (best_rank == 0) {
                    continue;
                }

                uint16_t winner_count = 0;
                for (std::size_t seat = 0; seat < N; ++seat) {
                    if (!state.folded[seat] && layer.eligible_mask[seat] && ranks[seat] == best_rank) {
                        ++winner_count;
                    }
                }
                assert(winner_count > 0);
                const auto award = rake_adjusted_layer_amount(state.context, state.pot_layers, layer_index)
                    / static_cast<utility>(winner_count);
                for (std::size_t seat = 0; seat < N; ++seat) {
                    if (!state.folded[seat] && layer.eligible_mask[seat] && ranks[seat] == best_rank) {
                        payoff[seat] += award;
                    }
                }
            }
            return payoff;
        }

        template <std::size_t N>
        void accumulate_showdown_exact_for_hero(
            terminal_values<N>& values,
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& reach,
            const terminal_state<N>& state,
            std::array<combination_index, N>& sampled_combos,
            std::array<rank_key, N>& sampled_ranks,
            const std::size_t hero_seat,
            const combination_index hero_combo,
            const std::size_t seat,
            const card_mask used_cards,
            const accumulator opponent_weight
        ) noexcept {
            if (seat == N) {
                const auto payoff = showdown_payoff_for_sample(sampled_ranks, state);
                values[hero_seat][hero_combo] += static_cast<terminal_value>(opponent_weight * payoff[hero_seat]);
                return;
            }
            if (seat == hero_seat) {
                accumulate_showdown_exact_for_hero(
                    values,
                    cache,
                    reach,
                    state,
                    sampled_combos,
                    sampled_ranks,
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
                sampled_combos[seat] = combo;
                sampled_ranks[seat] = state.folded[seat] ? 0 : cache.rank_keys[combo];
                accumulate_showdown_exact_for_hero(
                    values,
                    cache,
                    reach,
                    state,
                    sampled_combos,
                    sampled_ranks,
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
    [[nodiscard]] terminal_values<N> evaluate_showdown_values_exact(
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

        for (std::size_t hero_seat = 0; hero_seat < N; ++hero_seat) {
            const auto& hero_index = reach[hero_seat];
            for (uint16_t offset = 0; offset < hero_index.active_count; ++offset) {
                const auto hero_combo = hero_index.active_indices[offset];
                std::array<combination_index, N> sampled_combos{};
                std::array<rank_key, N> sampled_ranks{};
                sampled_combos[hero_seat] = hero_combo;
                sampled_ranks[hero_seat] = state.folded[hero_seat] ? 0 : cache.rank_keys[hero_combo];
                detail::accumulate_showdown_exact_for_hero(
                    values,
                    cache,
                    reach,
                    state,
                    sampled_combos,
                    sampled_ranks,
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

    /**
     * Heads-up convenience overload: forwards a pair of reach indices to the
     * two-stream kernel and keeps existing call sites working.
     */
    [[nodiscard]] inline_always terminal_result<2> evaluate_showdown(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        const terminal_context<2>& context
    ) noexcept {
        return evaluate_showdown_heads_up(cache, oop_index, ip_index, context);
    }

    [[nodiscard]] inline_always terminal_result<2> evaluate_showdown(
        const river_terminal_cache& cache,
        const reach_vector& oop_reach,
        const reach_vector& ip_reach,
        const terminal_context<2>& context
    ) noexcept {
        const auto oop_index = make_river_reach_index(cache, oop_reach);
        const auto ip_index = make_river_reach_index(cache, ip_reach);
        return evaluate_showdown(cache, oop_index, ip_index, context);
    }

    [[nodiscard]] inline_always terminal_values<2> evaluate_showdown_values(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        const terminal_context<2>& context
    ) noexcept {
        return evaluate_showdown(cache, oop_index, ip_index, context).values;
    }

    [[nodiscard]] inline_always terminal_values<2> evaluate_showdown_values(
        const river_terminal_cache& cache,
        const reach_vector& oop_reach,
        const reach_vector& ip_reach,
        const terminal_context<2>& context
    ) noexcept {
        return evaluate_showdown(cache, oop_reach, ip_reach, context).values;
    }

    template <std::size_t N>
    [[nodiscard]] terminal_values<N> evaluate_showdown_values(
        const river_terminal_cache& cache,
        const std::array<river_reach_index, N>& reach,
        const terminal_context<N>& context
    ) noexcept {
        if constexpr (N == 2) {
            return evaluate_showdown(cache, reach, context).values;
        } else {
            const auto state = make_showdown_terminal_state(context);
            return evaluate_showdown_values_exact(cache, reach, state);
        }
    }

}
