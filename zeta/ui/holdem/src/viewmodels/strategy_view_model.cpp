#include "viewmodels/strategy_view_model.h"

#include "cli/solve_cli.h"
#include "viewmodels/range_view_model.h"

#include <algorithm>
#include <cmath>
#include <iomanip>
#include <map>
#include <sstream>
#include <string_view>
#include <unordered_map>

namespace zeta::holdem::ui::viewmodels {

    namespace {

        constexpr double frequency_epsilon = 0.000001;

        struct range_combo_lookup {
            std::string hand_class;
            float weight = 1.0f;
            bool live = true;
            std::vector<std::string> blocked_by;
        };

        struct hand_class_accumulator {
            std::vector<strategy_hand_row> rows;
            std::map<std::string, double> action_totals;
            double weighted_ev = 0.0;
            double total_weight = 0.0;
        };

        [[nodiscard]] std::string join_text(const std::vector<std::string>& values, const std::string_view separator)
        {
            std::ostringstream out;
            for (std::size_t i = 0; i < values.size(); ++i) {
                if (i != 0u) {
                    out << separator;
                }
                out << values[i];
            }
            return out.str();
        }

        [[nodiscard]] std::string seat_label(const spot& source, const std::size_t seat)
        {
            if (seat < source.players.size() && !source.players[seat].empty()) {
                return source.players[seat];
            }
            return "Seat " + std::to_string(seat + 1u);
        }

        [[nodiscard]] std::string canonical_hand_text(const std::string_view hand)
        {
            if (hand.size() != 4u) {
                return std::string{hand};
            }

            const auto first = cli::detail::parse_card_text(hand.substr(0u, 2u));
            const auto second = cli::detail::parse_card_text(hand.substr(2u, 2u));
            if (!first || !second || *first == *second) {
                return std::string{hand};
            }

            const auto mask = (card_mask{1} << *first) | (card_mask{1} << *second);
            for (combination_index combo = 0; combo < combination_count; ++combo) {
                if (combination_masks[combo] == mask) {
                    return cli::detail::hand_text_from_combo(combo);
                }
            }
            return std::string{hand};
        }

        [[nodiscard]] std::string hand_class_from_hand(const std::string_view hand)
        {
            const auto canonical = canonical_hand_text(hand);
            if (canonical.size() != 4u) {
                return canonical;
            }

            const auto first = cli::detail::parse_card_text(std::string_view{canonical}.substr(0u, 2u));
            const auto second = cli::detail::parse_card_text(std::string_view{canonical}.substr(2u, 2u));
            if (!first || !second) {
                return canonical;
            }

            const auto first_rank = static_cast<uint8_t>(*first % 13u);
            const auto second_rank = static_cast<uint8_t>(*second % 13u);
            const auto first_suit = static_cast<uint8_t>(*first / 13u);
            const auto second_suit = static_cast<uint8_t>(*second / 13u);
            static constexpr std::array ranks{'2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'};

            if (first_rank == second_rank) {
                return std::string{ranks[first_rank], ranks[first_rank]};
            }

            const auto high_rank = std::max(first_rank, second_rank);
            const auto low_rank = std::min(first_rank, second_rank);
            const char suited = first_suit == second_suit ? 's' : 'o';
            return std::string{ranks[high_rank], ranks[low_rank], suited};
        }

        [[nodiscard]] std::size_t matrix_index_for_class(const std::string& hand_class)
        {
            const auto labels = hand_class_labels();
            const auto found = std::ranges::find(labels, hand_class);
            if (found == labels.end()) {
                return 0u;
            }
            return static_cast<std::size_t>(std::distance(labels.begin(), found));
        }

        [[nodiscard]] std::vector<strategy_action_frequency> normalize_actions(
            const std::map<std::string, double>& totals,
            const double total_weight)
        {
            std::vector<strategy_action_frequency> actions;
            actions.reserve(totals.size());
            if (total_weight <= frequency_epsilon) {
                return actions;
            }

            for (const auto& [action, total] : totals) {
                actions.push_back(strategy_action_frequency{
                    .action = action,
                    .frequency = total / total_weight
                });
            }
            std::ranges::sort(actions, [](const auto& lhs, const auto& rhs) {
                if (std::fabs(lhs.frequency - rhs.frequency) > frequency_epsilon) {
                    return lhs.frequency > rhs.frequency;
                }
                return lhs.action < rhs.action;
            });
            return actions;
        }

        [[nodiscard]] std::string best_action_text(const std::vector<strategy_action_frequency>& actions)
        {
            if (actions.empty()) {
                return "-";
            }
            const auto best = std::ranges::max_element(actions, {}, &strategy_action_frequency::frequency);
            return best == actions.end() ? "-" : best->action;
        }

        [[nodiscard]] bool action_matches_filter(const std::string& action, const strategy_action_filter filter) noexcept
        {
            switch (filter) {
                case strategy_action_filter::all:
                    return true;
                case strategy_action_filter::fold:
                    return action == "fold";
                case strategy_action_filter::check_call:
                    return action == "check" || action == "call";
                case strategy_action_filter::bet_raise:
                    return action.starts_with("bet") || action.starts_with("raise");
                case strategy_action_filter::all_in:
                    return action == "all_in" || action == "all-in" || action == "allin";
            }
            return true;
        }

        [[nodiscard]] std::unordered_map<std::string, range_combo_lookup> make_range_lookup(
            const spot& source,
            const solve_artifact& artifact)
        {
            std::unordered_map<std::string, range_combo_lookup> lookup;
            const auto hero = static_cast<std::size_t>(artifact.hero_seat);
            if (hero >= source.ranges.size()) {
                return lookup;
            }

            const auto analysis = analyze_range(source.ranges[hero], source.board);
            if (analysis.parse_issue) {
                return lookup;
            }

            lookup.reserve(analysis.exact_combos.size());
            for (const auto& combo : analysis.exact_combos) {
                lookup.emplace(canonical_hand_text(combo.hand), range_combo_lookup{
                    .hand_class = combo.hand_class,
                    .weight = combo.weight,
                    .live = combo.live,
                    .blocked_by = combo.blocked_by
                });
            }
            return lookup;
        }

        [[nodiscard]] double entropy_mix_indicator(const std::vector<strategy_action_card>& cards)
        {
            if (cards.size() <= 1u) {
                return 0.0;
            }

            double total = 0.0;
            for (const auto& card : cards) {
                total += std::max(0.0, card.frequency);
            }
            if (total <= frequency_epsilon) {
                return 0.0;
            }

            double entropy = 0.0;
            for (const auto& card : cards) {
                const auto p = std::max(0.0, card.frequency) / total;
                if (p > frequency_epsilon) {
                    entropy -= p * std::log(p);
                }
            }
            return entropy / std::log(static_cast<double>(cards.size()));
        }

    }

    strategy_view_model make_strategy_view_model(const spot& source, const solve_artifact& artifact)
    {
        strategy_view_model model;
        const auto labels = hand_class_labels();
        for (std::size_t index = 0; index < labels.size(); ++index) {
            model.matrix[index].hand_class = labels[index];
        }

        model.metadata.algorithm = artifact.solver.algorithm;
        model.metadata.iterations = artifact.solver.iterations;
        model.metadata.timestamp = artifact.solver.timestamp;
        model.metadata.git_revision = artifact.solver.git_revision;
        model.metadata.player_count = artifact.players.size();
        model.metadata.hero_label = seat_label(source, artifact.hero_seat);
        model.metadata.root_actor_label = seat_label(source, source.root_actor);
        model.metadata.street = artifact.street;
        model.metadata.board = join_text(artifact.board, " ");
        model.metadata.seat_ranges.reserve(source.players.size());
        for (std::size_t seat = 0; seat < source.players.size(); ++seat) {
            const auto range = seat < source.ranges.size() ? source.ranges[seat] : std::string{};
            model.metadata.seat_ranges.push_back(seat_label(source, seat) + ": " + range);
        }

        const auto range_lookup = make_range_lookup(source, artifact);
        std::map<std::string, hand_class_accumulator> class_totals;
        std::map<std::string, double> action_totals;
        std::map<std::string, double> action_weighted_evs;
        std::map<std::string, double> action_weight_totals;
        double total_weight = 0.0;
        double weighted_ev = 0.0;

        model.hands.reserve(artifact.strategy.size());
        for (const auto& artifact_row : artifact.strategy) {
            const auto hand = canonical_hand_text(artifact_row.hand);
            const auto found_range = range_lookup.find(hand);
            const auto hand_class = found_range == range_lookup.end()
                ? hand_class_from_hand(hand)
                : found_range->second.hand_class;
            const auto range_weight = found_range == range_lookup.end()
                ? 1.0
                : static_cast<double>(found_range->second.weight);
            const auto live = found_range == range_lookup.end() || found_range->second.live;
            const auto blocked_by = found_range == range_lookup.end()
                ? std::vector<std::string>{}
                : found_range->second.blocked_by;

            const auto& effective_strategy = artifact_row.strategy.empty() ? artifact.root_strategy : artifact_row.strategy;
            std::vector<strategy_action_frequency> row_actions;
            row_actions.reserve(effective_strategy.size());
            for (const auto& action : effective_strategy) {
                row_actions.push_back(strategy_action_frequency{
                    .action = action.action,
                    .frequency = action.frequency
                });
            }
            std::ranges::sort(row_actions, [](const auto& lhs, const auto& rhs) {
                if (std::fabs(lhs.frequency - rhs.frequency) > frequency_epsilon) {
                    return lhs.frequency > rhs.frequency;
                }
                return lhs.action < rhs.action;
            });

            strategy_hand_row row{
                .hand = hand,
                .hand_class = hand_class,
                .best_action = best_action_text(row_actions),
                .actions = std::move(row_actions),
                .ev = artifact_row.ev,
                .range_weight = range_weight,
                .live = live,
                .blocked_by = blocked_by
            };

            const auto positive_weight = std::max(0.0, range_weight);
            auto& class_total = class_totals[hand_class];
            class_total.weighted_ev += artifact_row.ev * positive_weight;
            class_total.total_weight += positive_weight;
            for (const auto& action : row.actions) {
                const auto contribution = positive_weight * action.frequency;
                class_total.action_totals[action.action] += contribution;
                action_totals[action.action] += contribution;
                action_weighted_evs[action.action] += contribution * artifact_row.ev;
                action_weight_totals[action.action] += contribution;
            }
            class_total.rows.push_back(row);
            weighted_ev += artifact_row.ev * positive_weight;
            total_weight += positive_weight;
            model.hands.push_back(std::move(row));
        }

        model.average_ev = total_weight > frequency_epsilon ? weighted_ev / total_weight : 0.0;

        for (auto& [hand_class, total] : class_totals) {
            const auto matrix_index = matrix_index_for_class(hand_class);
            auto& cell = model.matrix[matrix_index];
            cell.available = true;
            cell.exact_combos = std::move(total.rows);
            cell.actions = normalize_actions(total.action_totals, total.total_weight);
            cell.best_action = best_action_text(cell.actions);
            cell.ev = total.total_weight > frequency_epsilon ? total.weighted_ev / total.total_weight : 0.0;
            cell.range_weight = total.total_weight;
        }

        model.action_cards.reserve(action_totals.size());
        for (const auto& [action, total] : action_totals) {
            const auto frequency = total_weight > frequency_epsilon ? total / total_weight : 0.0;
            const auto average_ev = action_weight_totals[action] > frequency_epsilon
                ? action_weighted_evs[action] / action_weight_totals[action]
                : 0.0;
            model.action_cards.push_back(strategy_action_card{
                .action = action,
                .frequency = frequency,
                .average_ev = average_ev
            });
        }
        std::ranges::sort(model.action_cards, [](const auto& lhs, const auto& rhs) {
            if (std::fabs(lhs.frequency - rhs.frequency) > frequency_epsilon) {
                return lhs.frequency > rhs.frequency;
            }
            return lhs.action < rhs.action;
        });
        model.mix_indicator = entropy_mix_indicator(model.action_cards);

        std::ranges::sort(model.hands, [](const auto& lhs, const auto& rhs) {
            if (std::fabs(lhs.ev - rhs.ev) > frequency_epsilon) {
                return lhs.ev > rhs.ev;
            }
            return lhs.hand < rhs.hand;
        });
        return model;
    }

    std::vector<strategy_filter_option> strategy_filter_options()
    {
        return {
            strategy_filter_option{.filter = strategy_action_filter::all, .label = "All actions"},
            strategy_filter_option{.filter = strategy_action_filter::fold, .label = "Fold"},
            strategy_filter_option{.filter = strategy_action_filter::check_call, .label = "Check/Call"},
            strategy_filter_option{.filter = strategy_action_filter::bet_raise, .label = "Bet/Raise"},
            strategy_filter_option{.filter = strategy_action_filter::all_in, .label = "All-in"}
        };
    }

    bool strategy_row_matches_filter(const strategy_hand_row& row, const strategy_action_filter filter) noexcept
    {
        if (filter == strategy_action_filter::all) {
            return true;
        }
        return std::ranges::any_of(row.actions, [filter](const auto& action) {
            return action.frequency > frequency_epsilon && action_matches_filter(action.action, filter);
        });
    }

    bool strategy_cell_matches_filter(const strategy_matrix_cell& cell, const strategy_action_filter filter) noexcept
    {
        if (!cell.available) {
            return false;
        }
        if (filter == strategy_action_filter::all) {
            return true;
        }
        return std::ranges::any_of(cell.exact_combos, [filter](const auto& row) {
            return strategy_row_matches_filter(row, filter);
        });
    }

    std::vector<strategy_hand_row> filtered_strategy_hands(
        const strategy_view_model& model,
        const strategy_action_filter filter)
    {
        std::vector<strategy_hand_row> rows;
        rows.reserve(model.hands.size());
        for (const auto& row : model.hands) {
            if (strategy_row_matches_filter(row, filter)) {
                rows.push_back(row);
            }
        }
        return rows;
    }

    std::string format_strategy_percent(const double frequency)
    {
        std::ostringstream out;
        out << std::fixed << std::setprecision(1) << (frequency * 100.0) << '%';
        return out.str();
    }

    std::string format_strategy_ev(const double ev)
    {
        std::ostringstream out;
        if (ev > 0.000001) {
            out << '+';
        }
        out << std::fixed << std::setprecision(2) << ev;
        return out.str();
    }

    std::string format_strategy_actions(const std::vector<strategy_action_frequency>& actions)
    {
        std::vector<std::string> parts;
        parts.reserve(actions.size());
        for (const auto& action : actions) {
            parts.push_back(action.action + ' ' + format_strategy_percent(action.frequency));
        }
        return join_text(parts, ", ");
    }

}
