#include "theme/theme_registry.h"

#include <array>

namespace zeta::holdem::ui::theme {

    namespace {

        [[nodiscard]] const std::array<registered_theme, 3>& themes() noexcept
        {
            static const std::array<registered_theme, 3> registry{{
                registered_theme{
                    .id = theme_id::dark_pro,
                    .key = "dark-pro",
                    .display_name = "Dark Pro",
                    .tokens = theme_tokens{
                        .background_base = "#1d2128",
                        .background_raised = "#20252e",
                        .background_sunken = "#171b21",
                        .background_input = "#20252e",
                        .border_subtle = "#3a4048",
                        .border_strong = "#8ea8c0",
                        .text_primary = "#e7e2dc",
                        .text_secondary = "#c7ced8",
                        .text_muted = "#8a9099",
                        .accent_primary = "#8ea8c0",
                        .accent_secondary = "#fb8aa7",
                        .action_primary = "#8ea8c0",
                        .action_primary_hover = "#a5bdd2",
                        .action_positive = "#a8c8a8",
                        .action_negative = "#cf7c92",
                        .ev_positive = "#a8c8a8",
                        .ev_negative = "#ff6b6b",
                        .ev_neutral = "#9aa1ab",
                        .range_heat = {"#20252e", "#2b3442", "#566f84", "#8ea8c0"},
                        .warning = "#9f8668",
                        .error = "#ff6b6b",
                        .success = "#a8c8a8",
                        .selection = "#8ea8c026"
                    }
                },
                registered_theme{
                    .id = theme_id::light_pro,
                    .key = "light-pro",
                    .display_name = "Light Pro",
                    .tokens = theme_tokens{
                        .background_base = "#f5f5f4",
                        .background_raised = "#ffffff",
                        .background_sunken = "#ebe9e4",
                        .background_input = "#ffffff",
                        .border_subtle = "#d8d6d1",
                        .border_strong = "#95a8b8",
                        .text_primary = "#000000",
                        .text_secondary = "#5b5b5b",
                        .text_muted = "#707070",
                        .accent_primary = "#95a8b8",
                        .accent_secondary = "#b76b84",
                        .action_primary = "#95a8b8",
                        .action_primary_hover = "#b0c4d0",
                        .action_positive = "#3f7d3f",
                        .action_negative = "#b76b84",
                        .ev_positive = "#3f7d3f",
                        .ev_negative = "#c0392b",
                        .ev_neutral = "#707070",
                        .range_heat = {"#ffffff", "#e2e0db", "#c6d2db", "#95a8b8"},
                        .warning = "#b89500",
                        .error = "#c0392b",
                        .success = "#3f7d3f",
                        .selection = "#fb8aa755"
                    }
                },
                registered_theme{
                    .id = theme_id::high_contrast,
                    .key = "high-contrast",
                    .display_name = "High Contrast",
                    .tokens = theme_tokens{
                        .background_base = "#000000",
                        .background_raised = "#101010",
                        .background_sunken = "#050505",
                        .background_input = "#000000",
                        .border_subtle = "#ffffff",
                        .border_strong = "#ffff00",
                        .text_primary = "#ffffff",
                        .text_secondary = "#f2f2f2",
                        .text_muted = "#d0d0d0",
                        .accent_primary = "#00e5ff",
                        .accent_secondary = "#ffff00",
                        .action_primary = "#00e5ff",
                        .action_primary_hover = "#7af3ff",
                        .action_positive = "#00ff66",
                        .action_negative = "#ff5c8a",
                        .ev_positive = "#00ff66",
                        .ev_negative = "#ff3333",
                        .ev_neutral = "#ffffff",
                        .range_heat = {"#000000", "#003b45", "#008ea3", "#00e5ff"},
                        .warning = "#ffff00",
                        .error = "#ff3333",
                        .success = "#00ff66",
                        .selection = "#00e5ff55"
                    }
                }
            }};
            return registry;
        }

    }

    std::span<const registered_theme> registered_themes() noexcept
    {
        const auto& registry = themes();
        return {registry.data(), registry.size()};
    }

    const registered_theme& default_theme() noexcept
    {
        return themes().front();
    }

    const registered_theme& find_theme(const theme_id id) noexcept
    {
        for (const auto& theme : themes()) {
            if (theme.id == id) {
                return theme;
            }
        }
        return default_theme();
    }

    std::optional<theme_id> theme_id_from_key(const std::string_view key) noexcept
    {
        for (const auto& theme : themes()) {
            if (theme.key == key) {
                return theme.id;
            }
        }
        return std::nullopt;
    }

    std::string_view theme_key(const theme_id id) noexcept
    {
        return find_theme(id).key;
    }

    std::optional<density_mode> density_mode_from_key(const std::string_view key) noexcept
    {
        if (key == "compact") {
            return density_mode::compact;
        }
        if (key == "comfortable") {
            return density_mode::comfortable;
        }
        return std::nullopt;
    }

    std::string_view density_mode_key(const density_mode mode) noexcept
    {
        switch (mode) {
            case density_mode::compact:
                return "compact";
            case density_mode::comfortable:
                return "comfortable";
        }
        return "comfortable";
    }

    std::string_view density_mode_label(const density_mode mode) noexcept
    {
        switch (mode) {
            case density_mode::compact:
                return "Compact";
            case density_mode::comfortable:
                return "Comfortable";
        }
        return "Comfortable";
    }

    density_metrics metrics_for_density(const density_mode mode) noexcept
    {
        switch (mode) {
            case density_mode::compact:
                return density_metrics{
                    .shell_margin = 4,
                    .panel_margin = 6,
                    .panel_spacing = 4,
                    .toolbar_spacing = 4,
                    .range_cell_min_width = 42,
                    .range_cell_min_height = 32,
                    .action_button_height = 76,
                    .console_height = 78
                };
            case density_mode::comfortable:
                return density_metrics{};
        }
        return density_metrics{};
    }

}
