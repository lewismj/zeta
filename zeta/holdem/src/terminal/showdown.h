#pragma once

#include "terminal/workspace.h"

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

    // Heads-up (2-player) exact showdown kernel: a two-stream rank-bucket merge.
    // This is the hand-tuned fast path; the generic evaluate_showdown<N> dispatches
    // here for N == 2. reach[0] == oop, reach[1] == ip.
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

    // Generic entry point: player count is a compile-time constant. Primary
    // template fails to compile for N != 2 until multiplayer kernels exist, so an
    // accidental N-way call is a hard error rather than a silent slow path.
    template <std::size_t N>
    [[nodiscard]] terminal_result<N> evaluate_showdown(
        const river_terminal_cache& cache,
        const std::array<river_reach_index, N>& reach,
        const terminal_context<N>& context
    ) noexcept {
        static_assert(N == 2, "N-way showdown evaluator not implemented");
        if constexpr (N == 2) {
            return evaluate_showdown_heads_up(cache, reach[0], reach[1], context);
        }
    }

    template <std::size_t N>
    [[nodiscard]] terminal_values<N> evaluate_showdown_values_multiplayer_sampled(
        const river_terminal_cache& cache,
        const std::array<river_reach_index, N>& reach,
        const terminal_context<N>& context,
        const uint16_t samples_per_combo
    ) noexcept;

    // Heads-up convenience overload: forwards a pair of reach indices to the
    // two-stream kernel (keeps existing call sites working).
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
        const terminal_context<N>& context,
        const uint16_t samples_per_combo = 64
    ) noexcept {
        if constexpr (N == 2) {
            (void)samples_per_combo;
            return evaluate_showdown(cache, reach, context).values;
        } else {
            return evaluate_showdown_values_multiplayer_sampled(cache, reach, context, samples_per_combo);
        }
    }

    [[nodiscard]] inline_always terminal_summary<2> summarize_showdown(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        const terminal_context<2>& context
    ) noexcept {
        return evaluate_showdown(cache, oop_index, ip_index, context).summary;
    }

    [[nodiscard]] inline_always terminal_summary<2> summarize_showdown(
        const river_terminal_cache& cache,
        const reach_vector& oop_reach,
        const reach_vector& ip_reach,
        const terminal_context<2>& context
    ) noexcept {
        return evaluate_showdown(cache, oop_reach, ip_reach, context).summary;
    }

    // Workspace-based API (preferred for CFR): caller provides ranges, workspace owns reach indices.
    // The workspace materializes ranges on first call, then reuses for subsequent evaluations.
    template <std::size_t N>
    [[nodiscard]] terminal_result<N> evaluate_showdown(
       terminal_workspace<N>& workspace,
       const river_terminal_cache& cache,
       const std::array<reach_vector, N>& ranges,
       const terminal_context<N>& context
    ) noexcept {
       // Materialize ranges into workspace reach indices
       workspace.materialize(cache, ranges);
        
       // Evaluate using the materialized indices
       return evaluate_showdown(cache, workspace.reach, context);
    }

    // Workspace-based showdown values (convenience wrapper)
    template <std::size_t N>
    [[nodiscard]] terminal_values<N> evaluate_showdown_values(
       terminal_workspace<N>& workspace,
       const river_terminal_cache& cache,
       const std::array<reach_vector, N>& ranges,
       const terminal_context<N>& context
    ) noexcept {
       if constexpr (N == 2) {
           return evaluate_showdown(workspace, cache, ranges, context).values;
       } else {
           workspace.materialize(cache, ranges);
           return evaluate_showdown_values_multiplayer_sampled(cache, workspace.reach, context, 64);
       }
    }

    // Workspace-based showdown summary (convenience wrapper)
    template <std::size_t N>
    [[nodiscard]] terminal_summary<N> summarize_showdown(
       terminal_workspace<N>& workspace,
       const river_terminal_cache& cache,
       const std::array<reach_vector, N>& ranges,
       const terminal_context<N>& context
    ) noexcept {
       return evaluate_showdown(workspace, cache, ranges, context).summary;
    }

    // Heads-up workspace specialization (overload for convenience)
    [[nodiscard]] inline_always terminal_result<2> evaluate_showdown(
       terminal_workspace<2>& workspace,
       const river_terminal_cache& cache,
       const reach_vector& oop_reach,
       const reach_vector& ip_reach,
       const terminal_context<2>& context
    ) noexcept {
       return evaluate_showdown(workspace, cache, std::array<reach_vector, 2>{oop_reach, ip_reach}, context);
    }

    [[nodiscard]] inline_always uint64_t next_sample_state(uint64_t& state) noexcept {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        return state;
    }

    [[nodiscard]] inline_always accumulator sample_unit_interval(uint64_t& state) noexcept {
        constexpr accumulator inv = 1.0 / static_cast<accumulator>(UINT64_MAX);
        return static_cast<accumulator>(next_sample_state(state)) * inv;
    }

    // Step 11: initial multiplayer kernel (N > 2) using stratified sampling with
    // importance weighting. This is the first practical multiplayer implementation
    // under the multiplayer kernel family; future kernels can replace it without
    // changing the caller-facing dispatch interface.
    template <std::size_t N>
    [[nodiscard]] terminal_values<N> evaluate_showdown_values_multiplayer_sampled(
        const river_terminal_cache& cache,
        const std::array<river_reach_index, N>& reach,
        const terminal_context<N>& context,
        const uint16_t samples_per_combo
    ) noexcept {
        static_assert(N > 2, "multiplayer sampled kernel requires N > 2");

        terminal_values<N> values{};
        if (samples_per_combo == 0) {
            return values;
        }

        constexpr uint8_t strata_count = 3;
        const utility distributed = context.gross_pot - context.rake;

        for (std::size_t hero_seat = 0; hero_seat < N; ++hero_seat) {
            const auto& hero_index = reach[hero_seat];
            for (uint16_t hero_offset = 0; hero_offset < hero_index.active_count; ++hero_offset) {
                const auto hero_combo = hero_index.active_indices[hero_offset];

                accumulator weighted_value_sum = 0.0;
                uint16_t valid_samples = 0;
                uint64_t sample_state = cache.board_hash ^ (static_cast<uint64_t>(hero_seat) << 40) ^ (static_cast<uint64_t>(hero_combo) << 16);

                for (uint16_t sample_idx = 0; sample_idx < samples_per_combo; ++sample_idx) {
                    card_mask used_cards = cache.masks[hero_combo];
                    std::array<combination_index, N> sampled_combos{};
                    std::array<rank_key, N> sampled_ranks{};
                    sampled_combos[hero_seat] = hero_combo;
                    sampled_ranks[hero_seat] = cache.rank_keys[hero_combo];
                    accumulator importance_weight = 1.0;
                    bool valid = true;

                    for (std::size_t seat = 0; seat < N; ++seat) {
                        if (seat == hero_seat) {
                            continue;
                        }
                        const auto& opponent = reach[seat];
                        if (opponent.active_count == 0) {
                            valid = false;
                            break;
                        }

                        const std::array<uint16_t, 4> bounds{
                            0,
                            static_cast<uint16_t>(opponent.active_count / 3),
                            static_cast<uint16_t>((opponent.active_count * 2) / 3),
                            opponent.active_count
                        };

                        std::array<accumulator, strata_count> stratum_mass{};
                        std::array<uint8_t, strata_count> available{};
                        uint8_t available_count = 0;
                        accumulator total_mass = 0.0;

                        for (uint8_t stratum = 0; stratum < strata_count; ++stratum) {
                            accumulator mass = 0.0;
                            for (uint16_t i = bounds[stratum]; i < bounds[stratum + 1]; ++i) {
                                const auto combo = opponent.active_indices[i];
                                if ((cache.masks[combo] & used_cards) != 0) {
                                    continue;
                                }
                                mass += opponent.weights[combo];
                            }
                            stratum_mass[stratum] = mass;
                            if (mass > 0.0) {
                                available[available_count++] = stratum;
                                total_mass += mass;
                            }
                        }

                        if (available_count == 0 || total_mass <= 0.0) {
                            valid = false;
                            break;
                        }

                        const auto selected_stratum = available[(sample_idx + static_cast<uint16_t>(seat)) % available_count];
                        const auto selected_stratum_mass = stratum_mass[selected_stratum];
                        assert(selected_stratum_mass > 0.0);
                        importance_weight *= static_cast<accumulator>(available_count) * (selected_stratum_mass / total_mass);

                        const auto pick = sample_unit_interval(sample_state) * selected_stratum_mass;
                        accumulator running = 0.0;
                        combination_index selected_combo = 0;
                        combination_index fallback_combo = 0;
                        bool have_fallback = false;
                        bool selected = false;

                        for (uint16_t i = bounds[selected_stratum]; i < bounds[selected_stratum + 1]; ++i) {
                            const auto combo = opponent.active_indices[i];
                            if ((cache.masks[combo] & used_cards) != 0) {
                                continue;
                            }
                            const auto weight = opponent.weights[combo];
                            if (weight <= 0.0f) {
                                continue;
                            }
                            fallback_combo = combo;
                            have_fallback = true;
                            running += weight;
                            if (!selected && running >= pick) {
                                selected_combo = combo;
                                selected = true;
                            }
                        }

                        if (!selected) {
                            if (!have_fallback) {
                                valid = false;
                                break;
                            }
                            selected_combo = fallback_combo;
                        }

                        sampled_combos[seat] = selected_combo;
                        sampled_ranks[seat] = cache.rank_keys[selected_combo];
                        used_cards |= cache.masks[selected_combo];
                    }

                    if (!valid) {
                        continue;
                    }

                    rank_key best_rank = sampled_ranks[0];
                    for (std::size_t seat = 1; seat < N; ++seat) {
                        if (sampled_ranks[seat] > best_rank) {
                            best_rank = sampled_ranks[seat];
                        }
                    }

                    uint16_t winner_count = 0;
                    for (std::size_t seat = 0; seat < N; ++seat) {
                        if (sampled_ranks[seat] == best_rank) {
                            ++winner_count;
                        }
                    }

                    utility hero_value = -context.contribution[hero_seat];
                    if (sampled_ranks[hero_seat] == best_rank) {
                        hero_value = (distributed / static_cast<utility>(winner_count)) - context.contribution[hero_seat];
                    }

                    weighted_value_sum += importance_weight * hero_value;
                    ++valid_samples;
                }

                if (valid_samples > 0) {
                    values[hero_seat][hero_combo] = static_cast<terminal_value>(weighted_value_sum / static_cast<accumulator>(valid_samples));
                }
            }
        }

        return values;
    }
}
