#pragma once

#include <array>
#include <cstdint>
#include <string>

namespace zeta::holdem::ui::theme {

    enum class theme_id : uint8_t {
        dark_pro,
        light_pro,
        high_contrast
    };

    enum class density_mode : uint8_t {
        compact,
        comfortable
    };

    struct theme_tokens {
        std::string background_base;
        std::string background_raised;
        std::string background_sunken;
        std::string background_input;
        std::string border_subtle;
        std::string border_strong;
        std::string text_primary;
        std::string text_secondary;
        std::string text_muted;
        std::string accent_primary;
        std::string accent_secondary;
        std::string action_primary;
        std::string action_primary_hover;
        std::string action_positive;
        std::string action_negative;
        std::string ev_positive;
        std::string ev_negative;
        std::string ev_neutral;
        std::array<std::string, 4> range_heat;
        std::string warning;
        std::string error;
        std::string success;
        std::string selection;
        std::string document_selection;
        std::string active_surface;
        std::string button_text;
        std::string destructive_text;
    };

    struct registered_theme {
        theme_id id = theme_id::dark_pro;
        std::string key;
        std::string display_name;
        theme_tokens tokens;
    };

    struct density_metrics {
        int shell_margin = 6;
        int panel_margin = 8;
        int panel_spacing = 6;
        int toolbar_spacing = 6;
        int range_cell_min_width = 48;
        int range_cell_min_height = 44;
        int action_button_height = 92;
        int console_height = 56;
    };

}
