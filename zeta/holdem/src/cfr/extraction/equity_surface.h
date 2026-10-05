#pragma once

#include "eval/evaluator.h"
#include "range.h"
#include "terminal/reach_index.h"

#include <span>

namespace zeta::holdem::cfr::extraction {

    [[nodiscard]] inline double river_showdown_pot_share_equity(
        const river_terminal_cache& cache,
        const reach_vector& opponent_range,
        const float opponent_reach_probability,
        const combination_index hero_combo) noexcept
    {
        if (!is_live_combo(hero_combo, cache.river_board.mask)) {
            return 0.0;
        }

        double compatible_mass = 0.0;
        double pot_share_mass = 0.0;
        const auto hero_mask = combination_mask(hero_combo);
        const auto hero_rank = cache.rank_keys[hero_combo];

        for (const auto opponent_combo : cache.rank_order) {
            const auto opponent_weight = static_cast<double>(opponent_range[opponent_combo])
                * static_cast<double>(opponent_reach_probability);
            if (opponent_weight <= 0.0) {
                continue;
            }
            if ((hero_mask & combination_mask(opponent_combo)) != 0) {
                continue;
            }

            compatible_mass += opponent_weight;
            const auto opponent_rank = cache.rank_keys[opponent_combo];
            if (hero_rank > opponent_rank) {
                pot_share_mass += opponent_weight;
            } else if (hero_rank == opponent_rank) {
                pot_share_mass += 0.5 * opponent_weight;
            }
        }

        return compatible_mass > 0.0 ? pot_share_mass / compatible_mass : 0.0;
    }

    inline void write_river_showdown_equity_surface(
        std::span<float> output,
        const river_terminal_cache& cache,
        const reach_vector& opponent_range,
        const float opponent_reach_probability,
        const std::span<const combination_index> combo_indices)
    {
        for (std::size_t combo = 0; combo < combo_indices.size(); ++combo) {
            output[combo] = static_cast<float>(river_showdown_pot_share_equity(
                cache,
                opponent_range,
                opponent_reach_probability,
                combo_indices[combo]));
        }
    }

    [[nodiscard]] inline double turn_showdown_pot_share_equity(
        const board turn_board,
        const reach_vector& opponent_range,
        const float opponent_reach_probability,
        const combination_index hero_combo) noexcept
    {
        const auto hero_mask = combination_mask(hero_combo);
        const auto dead = turn_board.mask | hero_mask;
        if ((turn_board.mask & hero_mask) != 0u) {
            return 0.0;
        }

        double sum = 0.0;
        double count = 0.0;
        for (uint8_t river = 0; river < zeta::num_cards<zeta::default_deck>; ++river) {
            const auto river_bit = card_mask{1} << river;
            if ((dead & river_bit) != 0u) {
                continue;
            }
            const auto cache = make_river_terminal_cache(board{turn_board.mask | river_bit});
            sum += river_showdown_pot_share_equity(
                cache,
                opponent_range,
                opponent_reach_probability,
                hero_combo);
            count += 1.0;
        }
        return count > 0.0 ? sum / count : 0.0;
    }

    [[nodiscard]] inline double flop_showdown_pot_share_equity(
        const board flop_board,
        const reach_vector& opponent_range,
        const float opponent_reach_probability,
        const combination_index hero_combo) noexcept
    {
        const auto hero_mask = combination_mask(hero_combo);
        const auto dead = flop_board.mask | hero_mask;
        if ((flop_board.mask & hero_mask) != 0u) {
            return 0.0;
        }

        double sum = 0.0;
        double count = 0.0;
        for (uint8_t turn = 0; turn < zeta::num_cards<zeta::default_deck>; ++turn) {
            const auto turn_bit = card_mask{1} << turn;
            if ((dead & turn_bit) != 0u) {
                continue;
            }
            for (uint8_t river = static_cast<uint8_t>(turn + 1u); river < zeta::num_cards<zeta::default_deck>; ++river) {
                const auto river_bit = card_mask{1} << river;
                if ((dead & river_bit) != 0u) {
                    continue;
                }
                const auto cache = make_river_terminal_cache(board{flop_board.mask | turn_bit | river_bit});
                sum += river_showdown_pot_share_equity(
                    cache,
                    opponent_range,
                    opponent_reach_probability,
                    hero_combo);
                count += 1.0;
            }
        }
        return count > 0.0 ? sum / count : 0.0;
    }

}
