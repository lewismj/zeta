#pragma once

#include "../spot_document.h"

#include "range_parser.h"

#include <array>
#include <cstddef>
#include <optional>
#include <string>
#include <utility>
#include <vector>

namespace zeta::holdem::ui::viewmodels {

    struct range_parse_issue {
        std::size_t position = 0;
        std::string message;
    };

    struct range_combo_view {
        combination_index combo = 0;
        std::string hand;
        std::string hand_class;
        combo_weight weight = 0.0f;
        bool live = false;
        std::vector<std::string> blocked_by;
    };

    struct range_matrix_cell {
        std::string hand_class;
        combo_weight max_weight = 0.0f;
        double live_weight = 0.0;
        std::size_t class_combos = 0;
        std::size_t combos = 0;
        std::size_t live_combos = 0;
        bool selected = false;
        bool blocked = false;
    };

    struct range_metrics {
        std::size_t combos_before_blockers = 0;
        std::size_t live_combos = 0;
        double percent_total_hands = 0.0;
        std::vector<std::pair<std::string, std::size_t>> blocked_combos_by_card;
    };

    struct range_analysis {
        std::string source_text;
        std::optional<range_parse_issue> parse_issue;
        std::array<range_matrix_cell, 169> matrix{};
        std::vector<range_combo_view> exact_combos;
        range_metrics metrics;

        [[nodiscard]] bool valid_for_solve() const noexcept;
    };

    /**
     * Returns all 13x13 matrix labels in display order.
     */
    [[nodiscard]] std::array<std::string, 169> hand_class_labels();

    /**
     * Describes a range parser error with a user-facing message.
     */
    [[nodiscard]] std::string range_parse_error_message(range_parse_error_code code);

    /**
     * Expands a text range into matrix, combo, and board-blocker view data.
     */
    [[nodiscard]] range_analysis analyze_range(std::string_view text, const std::vector<std::string>& board);

    /**
     * Returns a compact exact-combo serialization preserving combo weights.
     */
    [[nodiscard]] std::string normalized_exact_range_text(const range_analysis& analysis);

    /**
     * Adds or replaces a matrix class in an existing text range.
     */
    [[nodiscard]] std::string set_hand_class_enabled(std::string_view source, std::string_view hand_class, bool enabled);

    /**
     * Adds a reusable named class selection to an existing text range.
     */
    [[nodiscard]] std::string add_named_class_selection(std::string_view source, std::string_view class_name);

}
