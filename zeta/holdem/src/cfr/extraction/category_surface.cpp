#include "cfr/extraction/category_surface.h"

#include <algorithm>
#include <array>
#include <sstream>

namespace zeta::holdem::cfr::extraction {

    namespace {
        [[nodiscard]] const char* to_string_pair_source(const pair_source source) noexcept
        {
            switch (source) {
                case pair_source::none: return "none";
                case pair_source::hole_pair: return "hole_pair";
                case pair_source::hole_board_pair: return "hole_board_pair";
                case pair_source::board_only_pair: return "board_only_pair";
            }
            return "unknown";
        }

        [[nodiscard]] const char* to_string_pair_position(const pair_position position) noexcept
        {
            switch (position) {
                case pair_position::none: return "none";
                case pair_position::top_pair: return "top_pair";
                case pair_position::middle_pair: return "middle_pair";
                case pair_position::bottom_pair: return "bottom_pair";
                case pair_position::pocket_pair_below_board: return "pocket_pair_below_board";
                case pair_position::overpair: return "overpair";
                case pair_position::underpair: return "underpair";
            }
            return "unknown";
        }

        [[nodiscard]] const char* to_string_kicker_quality(const kicker_quality kicker) noexcept
        {
            switch (kicker) {
                case kicker_quality::none: return "none";
                case kicker_quality::top: return "top";
                case kicker_quality::strong: return "strong";
                case kicker_quality::medium: return "medium";
                case kicker_quality::weak: return "weak";
            }
            return "unknown";
        }

        [[nodiscard]] std::string to_string_draw_flags(const draw_flags flags)
        {
            std::string text;
            const auto mask = static_cast<uint16_t>(flags);
            if (mask == 0u) {
                return "none";
            }
            if ((mask & static_cast<uint16_t>(draw_flags::open_ended_straight_draw)) != 0u) {
                text += (text.empty() ? "" : "+");
                text += "open_ended_straight_draw";
            }
            if ((mask & static_cast<uint16_t>(draw_flags::gutshot_straight_draw)) != 0u) {
                text += (text.empty() ? "" : "+");
                text += "gutshot_straight_draw";
            }
            if ((mask & static_cast<uint16_t>(draw_flags::backdoor_straight_draw)) != 0u) {
                text += (text.empty() ? "" : "+");
                text += "backdoor_straight_draw";
            }
            if ((mask & static_cast<uint16_t>(draw_flags::flush_draw)) != 0u) {
                text += (text.empty() ? "" : "+");
                text += "flush_draw";
            }
            return text;
        }

        [[nodiscard]] std::string to_string_blocker_flags(const blocker_flags flags)
        {
            std::string text;
            const auto mask = static_cast<uint16_t>(flags);
            if (mask == 0u) {
                return "none";
            }
            if ((mask & static_cast<uint16_t>(blocker_flags::nut_flush_blocker)) != 0u) {
                text += (text.empty() ? "" : "+");
                text += "nut_flush_blocker";
            }
            if ((mask & static_cast<uint16_t>(blocker_flags::second_nut_blocker)) != 0u) {
                text += (text.empty() ? "" : "+");
                text += "second_nut_blocker";
            }
            return text;
        }

        [[nodiscard]] bool same_classification(
            const hand_category_classification& lhs,
            const hand_category_classification& rhs) noexcept
        {
            return lhs.made_hand_tier == rhs.made_hand_tier
                && lhs.source == rhs.source
                && lhs.pair_pos == rhs.pair_pos
                && lhs.kicker == rhs.kicker
                && lhs.draws == rhs.draws
                && lhs.blockers == rhs.blockers;
        }

        [[nodiscard]] std::string classification_name(const hand_category_classification& classification)
        {
            std::ostringstream out;
            out << to_string(classification.made_hand_tier)
                << "|source=" << to_string_pair_source(classification.source)
                << "|pair_pos=" << to_string_pair_position(classification.pair_pos)
                << "|kicker=" << to_string_kicker_quality(classification.kicker)
                << "|draws=" << to_string_draw_flags(classification.draws)
                << "|blockers=" << to_string_blocker_flags(classification.blockers);
            return out.str();
        }

        [[nodiscard]] range_interaction_strength classify_range_interaction_strength(
            const double blocked_opponent_mass_fraction,
            const double blocked_strong_opponent_mass_fraction) noexcept
        {
            if (blocked_strong_opponent_mass_fraction >= 0.35 || blocked_opponent_mass_fraction >= 0.50) {
                return range_interaction_strength::strong;
            }
            if (blocked_strong_opponent_mass_fraction >= 0.20 || blocked_opponent_mass_fraction >= 0.30) {
                return range_interaction_strength::moderate;
            }
            if (blocked_opponent_mass_fraction > 0.0 || blocked_strong_opponent_mass_fraction > 0.0) {
                return range_interaction_strength::weak;
            }
            return range_interaction_strength::none;
        }

        struct category_accumulator {
            hand_category_classification classification{};
            double mass = 0.0;
            double weighted_ev = 0.0;
            double weighted_equity = 0.0;
            std::vector<double> weighted_action_mass;
            double weighted_blocked_mass = 0.0;
            double weighted_blocked_strong_mass = 0.0;
        };

        [[nodiscard]] std::vector<category_summary_item> compute_category_summaries_impl(
            category_view categories,
            equity_view equity,
            const std::span<const combination_index> combo_indices,
            const river_terminal_cache* cache,
            const river_reach_index* opponent_reach)
        {
            const auto combo_count = categories.combo_count();
            if (combo_count == 0u || combo_count != equity.combo_count()) {
                return {};
            }
            if (cache != nullptr && opponent_reach != nullptr && combo_indices.size() != combo_count) {
                return {};
            }

            std::vector<category_accumulator> accumulators;
            double total_mass = 0.0;

            for (uint32_t local = 0; local < combo_count; ++local) {
                const auto row_mass = categories.range_reach_weight(local);
                if (row_mass <= 0.0) {
                    continue;
                }
                total_mass += row_mass;

                const auto classification = categories.classification(local);
                auto it = std::find_if(
                    accumulators.begin(),
                    accumulators.end(),
                    [&](const category_accumulator& candidate) {
                        return same_classification(candidate.classification, classification);
                    });
                if (it == accumulators.end()) {
                    accumulators.push_back(category_accumulator{
                        .classification = classification,
                        .mass = 0.0,
                        .weighted_ev = 0.0,
                        .weighted_equity = 0.0,
                        .weighted_action_mass = std::vector<double>(categories.action_count(), 0.0),
                        .weighted_blocked_mass = 0.0,
                        .weighted_blocked_strong_mass = 0.0
                    });
                    it = std::prev(accumulators.end());
                }

                it->mass += row_mass;
                it->weighted_ev += row_mass * categories.combo_ev(local);
                it->weighted_equity += row_mass * equity.showdown_equity(local);
                for (uint16_t action_index = 0; action_index < categories.action_count(); ++action_index) {
                    it->weighted_action_mass[action_index] +=
                        row_mass * static_cast<double>(categories.frequency(local, action_index));
                }
                if (cache != nullptr && opponent_reach != nullptr) {
                    const auto interaction = classify_range_interaction(combo_indices[local], *cache, *opponent_reach);
                    it->weighted_blocked_mass += row_mass * interaction.blocked_opponent_mass_fraction;
                    it->weighted_blocked_strong_mass += row_mass * interaction.blocked_strong_opponent_mass_fraction;
                }
            }

            std::vector<category_summary_item> out;
            out.reserve(accumulators.size());
            for (const auto& accumulator : accumulators) {
                std::vector<category_summary_action_frequency> action_frequencies;
                action_frequencies.reserve(accumulator.weighted_action_mass.size());
                for (std::size_t action_index = 0; action_index < accumulator.weighted_action_mass.size(); ++action_index) {
                    action_frequencies.push_back(category_summary_action_frequency{
                        .action_index = static_cast<uint16_t>(action_index),
                        .frequency = accumulator.mass > 0.0
                            ? accumulator.weighted_action_mass[action_index] / accumulator.mass
                            : 0.0
                    });
                }
                const auto blocked_mass_fraction = accumulator.mass > 0.0
                    ? accumulator.weighted_blocked_mass / accumulator.mass
                    : 0.0;
                const auto blocked_strong_mass_fraction = accumulator.mass > 0.0
                    ? accumulator.weighted_blocked_strong_mass / accumulator.mass
                    : 0.0;
                out.push_back(category_summary_item{
                    .category_name = classification_name(accumulator.classification),
                    .frequency = total_mass > 0.0 ? accumulator.mass / total_mass : 0.0,
                    .range_weight = accumulator.mass,
                    .average_ev = accumulator.mass > 0.0 ? accumulator.weighted_ev / accumulator.mass : 0.0,
                    .average_equity = accumulator.mass > 0.0 ? accumulator.weighted_equity / accumulator.mass : 0.0,
                    .action_frequencies = std::move(action_frequencies),
                    .range_interaction = range_interaction_classification{
                        .blocked_opponent_mass_fraction = blocked_mass_fraction,
                        .blocked_strong_opponent_mass_fraction = blocked_strong_mass_fraction,
                        .strength = classify_range_interaction_strength(
                            blocked_mass_fraction,
                            blocked_strong_mass_fraction)
                    }
                });
            }

            return out;
        }
    }

    [[nodiscard]] const char* to_string(const range_interaction_strength strength) noexcept
    {
        switch (strength) {
            case range_interaction_strength::none: return "none";
            case range_interaction_strength::weak: return "weak";
            case range_interaction_strength::moderate: return "moderate";
            case range_interaction_strength::strong: return "strong";
        }
        return "unknown";
    }

    [[nodiscard]] range_interaction_classification classify_range_interaction(
        const combination_index hero_combo,
        const river_terminal_cache& cache,
        const river_reach_index& opponent_reach) noexcept
    {
        if (hero_combo >= combination_count || cache.board_hash != opponent_reach.board_hash) {
            return {};
        }

        struct weighted_entry {
            rank_key rank = 0;
            double weight = 0.0;
            bool compatible = false;
        };

        std::vector<weighted_entry> weighted_rank_entries;
        weighted_rank_entries.reserve(opponent_reach.active_count);
        const auto hero_mask = cache.masks[hero_combo];
        double total_mass = 0.0;
        double compatible_mass = 0.0;

        for (uint16_t offset = 0; offset < opponent_reach.active_count; ++offset) {
            const auto opponent_combo = opponent_reach.active_indices[offset];
            const auto opponent_weight = static_cast<double>(opponent_reach.weights[opponent_combo]);
            if (opponent_weight <= 0.0) {
                continue;
            }
            const auto compatible = (cache.masks[opponent_combo] & hero_mask) == 0u;
            total_mass += opponent_weight;
            if (compatible) {
                compatible_mass += opponent_weight;
            }
            weighted_rank_entries.push_back(weighted_entry{
                .rank = cache.rank_keys[opponent_combo],
                .weight = opponent_weight,
                .compatible = compatible
            });
        }

        if (total_mass <= 0.0 || weighted_rank_entries.empty()) {
            return {};
        }

        std::sort(weighted_rank_entries.begin(), weighted_rank_entries.end(), [](const weighted_entry& lhs, const weighted_entry& rhs) {
            return lhs.rank > rhs.rank;
        });

        const double strong_target_mass = total_mass * 0.25;
        double strong_mass = 0.0;
        double strong_compatible_mass = 0.0;
        for (const auto& entry : weighted_rank_entries) {
            if (strong_mass >= strong_target_mass) {
                break;
            }
            strong_mass += entry.weight;
            if (entry.compatible) {
                strong_compatible_mass += entry.weight;
            }
        }

        const auto blocked_mass_fraction = std::clamp((total_mass - compatible_mass) / total_mass, 0.0, 1.0);
        const auto blocked_strong_mass_fraction = strong_mass > 0.0
            ? std::clamp((strong_mass - strong_compatible_mass) / strong_mass, 0.0, 1.0)
            : blocked_mass_fraction;

        return range_interaction_classification{
            .blocked_opponent_mass_fraction = blocked_mass_fraction,
            .blocked_strong_opponent_mass_fraction = blocked_strong_mass_fraction,
            .strength = classify_range_interaction_strength(blocked_mass_fraction, blocked_strong_mass_fraction)
        };
    }

    [[nodiscard]] std::vector<category_summary_item> compute_category_summaries(
        const category_view categories,
        const equity_view equity)
    {
        return compute_category_summaries_impl(categories, equity, {}, nullptr, nullptr);
    }

    [[nodiscard]] std::vector<category_summary_item> compute_category_summaries(
        const category_view categories,
        const equity_view equity,
        const std::span<const combination_index> combo_indices,
        const river_terminal_cache& cache,
        const river_reach_index& opponent_reach)
    {
        return compute_category_summaries_impl(categories, equity, combo_indices, &cache, &opponent_reach);
    }

}

