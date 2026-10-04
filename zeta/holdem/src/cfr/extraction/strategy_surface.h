#pragma once

#include "cfr/extraction/result_store.h"
#include "cfr/tables/strategy_table.h"

#include <algorithm>
#include <span>
#include <vector>

namespace zeta::holdem::cfr::extraction {

    [[nodiscard]] inline std::vector<float> normalized_average_strategy(
        const cfr::strategy_sum_table& strategy_sums,
        const uint32_t infoset_id)
    {
        const auto sums = strategy_sums.infoset_sums(infoset_id);
        std::vector<float> strategy(sums.size(), 0.0f);
        if (sums.empty()) {
            return strategy;
        }

        double total = 0.0;
        for (const float value : sums) {
            total += static_cast<double>(std::max(value, 0.0f));
        }

        if (total <= 0.0) {
            const auto uniform = 1.0f / static_cast<float>(sums.size());
            std::fill(strategy.begin(), strategy.end(), uniform);
            return strategy;
        }

        for (std::size_t action = 0; action < sums.size(); ++action) {
            strategy[action] = static_cast<float>(static_cast<double>(std::max(sums[action], 0.0f)) / total);
        }
        return strategy;
    }

    inline void write_strategy_surface(
        std::span<strategy_surface_entry> output,
        const std::span<const float> average_strategy,
        const uint32_t combo_count,
        const uint16_t action_count)
    {
        for (uint32_t combo = 0; combo < combo_count; ++combo) {
            for (uint16_t action = 0; action < action_count; ++action) {
                output[combo * action_count + action].average_strategy = average_strategy[action];
            }
        }
    }

}
