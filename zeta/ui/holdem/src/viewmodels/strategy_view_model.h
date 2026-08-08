#pragma once

#include "../spot_document.h"

#include <array>
#include <cstddef>
#include <string>
#include <vector>

namespace zeta::holdem::ui::viewmodels {

    enum class strategy_action_filter {
        all,
        fold,
        check_call,
        bet_raise,
        all_in
    };

    struct strategy_action_frequency {
        std::string action;
        double frequency = 0.0;
    };

    struct strategy_hand_row {
        std::string hand;
        std::string hand_class;
        std::string best_action;
        std::vector<strategy_action_frequency> actions;
        double ev = 0.0;
        double range_weight = 0.0;
        bool live = true;
        std::vector<std::string> blocked_by;
    };

    struct strategy_matrix_cell {
        std::string hand_class;
        std::string best_action;
        std::vector<strategy_action_frequency> actions;
        std::vector<strategy_hand_row> exact_combos;
        double ev = 0.0;
        double range_weight = 0.0;
        bool available = false;
    };

    struct strategy_action_card {
        std::string action;
        double frequency = 0.0;
        double average_ev = 0.0;
    };

    struct strategy_filter_option {
        strategy_action_filter filter = strategy_action_filter::all;
        std::string label;
    };

    struct strategy_metadata_summary {
        std::string algorithm;
        uint64_t iterations = 0;
        std::string timestamp;
        std::string git_revision;
        std::size_t player_count = 0;
        std::string hero_label;
        std::string root_actor_label;
        std::string street;
        std::string board;
        std::vector<std::string> seat_ranges;
    };

    struct strategy_view_model {
        std::array<strategy_matrix_cell, 169> matrix{};
        std::vector<strategy_hand_row> hands;
        std::vector<strategy_action_card> action_cards;
        strategy_metadata_summary metadata;
        double average_ev = 0.0;
        double mix_indicator = 0.0;
    };

    /**
     * Aggregates a root solve artifact into strategy explorer data.
     */
    [[nodiscard]] strategy_view_model make_strategy_view_model(const spot& source, const solve_artifact& artifact);

    /**
     * Returns the action groups exposed by Strategy Explorer V1.
     */
    [[nodiscard]] std::vector<strategy_filter_option> strategy_filter_options();

    /**
     * Returns true when a row has positive frequency in the requested action group.
     */
    [[nodiscard]] bool strategy_row_matches_filter(const strategy_hand_row& row, strategy_action_filter filter) noexcept;

    /**
     * Returns true when a matrix cell has any combo in the requested action group.
     */
    [[nodiscard]] bool strategy_cell_matches_filter(const strategy_matrix_cell& cell, strategy_action_filter filter) noexcept;

    /**
     * Returns exact-combo rows visible under the requested action group.
     */
    [[nodiscard]] std::vector<strategy_hand_row> filtered_strategy_hands(
        const strategy_view_model& model,
        strategy_action_filter filter);

    /**
     * Formats an action frequency as a percentage with one decimal place.
     */
    [[nodiscard]] std::string format_strategy_percent(double frequency);

    /**
     * Formats an EV with a stable sign and two decimal places.
     */
    [[nodiscard]] std::string format_strategy_ev(double ev);

    /**
     * Formats all action frequencies in a compact table-friendly string.
     */
    [[nodiscard]] std::string format_strategy_actions(const std::vector<strategy_action_frequency>& actions);

}
