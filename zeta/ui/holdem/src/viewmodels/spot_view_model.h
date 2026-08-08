#pragma once

#include "../spot_document.h"

#include <cstddef>
#include <string>
#include <string_view>
#include <vector>

namespace zeta::holdem::ui::viewmodels {

    enum class spot_template_kind {
        heads_up_river,
        three_way_flop,
        four_way_turn
    };

    struct spot_validation_issue {
        std::string field;
        std::string message;
    };

    /**
     * Returns the number of public board cards required by a supported street.
     */
    [[nodiscard]] std::size_t board_card_count_for_street(std::string_view street) noexcept;

    /**
     * Returns all card labels accepted by the Hold'em spot parser.
     */
    [[nodiscard]] std::vector<std::string> deck_card_labels();

    /**
     * Resizes all per-player arrays together and clamps actor indices.
     */
    [[nodiscard]] spot resize_player_count(spot source, std::size_t player_count);

    /**
     * Builds one of the structured spot editor templates.
     */
    [[nodiscard]] spot make_template_spot(spot_template_kind kind);

    /**
     * Validates editor-level invariants and returns field-addressed issues.
     */
    [[nodiscard]] std::vector<spot_validation_issue> validate_structured_spot(const spot& source);

    /**
     * Creates a compact header summary for the current spot.
     */
    [[nodiscard]] std::string spot_summary_text(const spot& source, bool has_artifact);

}
