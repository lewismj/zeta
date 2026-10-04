#pragma once

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

}
