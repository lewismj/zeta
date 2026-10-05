#pragma once

#include "cfr/extraction/result_store.h"
#include "terminal/reach_index.h"

#include <span>
#include <string>
#include <vector>

namespace zeta::holdem::cfr::extraction {

    enum class range_interaction_strength : uint8_t {
        none,
        weak,
        moderate,
        strong
    };

    struct range_interaction_classification {
        double blocked_opponent_mass_fraction = 0.0;
        double blocked_strong_opponent_mass_fraction = 0.0;
        range_interaction_strength strength = range_interaction_strength::none;
    };

    struct category_summary_action_frequency {
        uint16_t action_index = 0;
        double frequency = 0.0;
    };

    struct category_summary_item {
        std::string category_name;
        double frequency = 0.0;
        double range_weight = 0.0;
        double average_ev = 0.0;
        double average_equity = 0.0;
        std::vector<category_summary_action_frequency> action_frequencies;
        range_interaction_classification range_interaction{};
    };

    [[nodiscard]] const char* to_string(range_interaction_strength strength) noexcept;

    [[nodiscard]] range_interaction_classification classify_range_interaction(
        combination_index hero_combo,
        const river_terminal_cache& cache,
        const river_reach_index& opponent_reach) noexcept;

    [[nodiscard]] std::vector<category_summary_item> compute_category_summaries(
        category_view categories,
        equity_view equity);

    [[nodiscard]] std::vector<category_summary_item> compute_category_summaries(
        category_view categories,
        equity_view equity,
        std::span<const combination_index> combo_indices,
        const river_terminal_cache& cache,
        const river_reach_index& opponent_reach);

}

