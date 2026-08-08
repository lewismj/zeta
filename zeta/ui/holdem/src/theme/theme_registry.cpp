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
                        .border_strong = "#4e5a66",
                        .text_primary = "#e7e2dc",
                        .text_secondary = "#c7ced8",
                        .text_muted = "#8a9099",
                        .accent_primary = "#fb8aa7",
                        .accent_secondary = "#8ea8c0",
                        .action_primary = "#8ea8c0",
                        .action_primary_hover = "#a5bdd2",
                        .action_positive = "#fb8aa7",
                        .action_negative = "#ff6b6b",
                        .ev_positive = "#a8c8a8",
                        .ev_negative = "#ff6b6b",
                        .ev_neutral = "#9aa1ab",
                        .range_heat = {"#252a33", "#403842", "#765365", "#fb8aa7"},
                        .warning = "#9f8668",
                        .error = "#ff6b6b",
                        .success = "#a8c8a8",
                        .selection = "#2b3541",
                        .document_selection = "rgba(251, 138, 167, 34)",
                        .active_surface = "rgba(251, 138, 167, 24)",
                        .button_text = "#1d2128",
                        .destructive_text = "#1d2128"
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
                        .border_strong = "#c8c5bf",
                        .text_primary = "#000000",
                        .text_secondary = "#3f3f3f",
                        .text_muted = "#707070",
                        .accent_primary = "#b76b84",
                        .accent_secondary = "#95a8b8",
                        .action_primary = "#95a8b8",
                        .action_primary_hover = "#b0c4d0",
                        .action_positive = "#fb8aa7",
                        .action_negative = "#c0392b",
                        .ev_positive = "#3f7d3f",
                        .ev_negative = "#c0392b",
                        .ev_neutral = "#707070",
                        .range_heat = {"#f0efec", "#d8c7ce", "#c9859a", "#fb8aa7"},
                        .warning = "#b89500",
                        .error = "#c0392b",
                        .success = "#3f7d3f",
                        .selection = "#fbd1db",
                        .document_selection = "rgba(251, 138, 167, 85)",
                        .active_surface = "rgba(251, 138, 167, 51)",
                        .button_text = "#000000",
                        .destructive_text = "#ffffff"
                    }
                },
                registered_theme{
                    .id = theme_id::high_contrast,
                    .key = "high-contrast",
                    .display_name = "High Contrast",
                    .tokens = theme_tokens{
                        .background_base = "#0b0d10",
                        .background_raised = "#11151a",
                        .background_sunken = "#171b21",
                        .background_input = "#11151a",
                        .border_subtle = "#59636e",
                        .border_strong = "#7f8b96",
                        .text_primary = "#ffffff",
                        .text_secondary = "#e7e2dc",
                        .text_muted = "#b8bec6",
                        .accent_primary = "#ff9ab3",
                        .accent_secondary = "#b5c8d8",
                        .action_primary = "#b5c8d8",
                        .action_primary_hover = "#d0dde7",
                        .action_positive = "#ff9ab3",
                        .action_negative = "#ff7373",
                        .ev_positive = "#9fd09f",
                        .ev_negative = "#ff7373",
                        .ev_neutral = "#e7e2dc",
                        .range_heat = {"#1b2026", "#343b44", "#765365", "#ff9ab3"},
                        .warning = "#e6c15a",
                        .error = "#ff7373",
                        .success = "#9fd09f",
                        .selection = "#252d35",
                        .document_selection = "#343038",
                        .active_surface = "#2d252c",
                        .button_text = "#0b0d10",
                        .destructive_text = "#0b0d10"
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
                    .range_cell_min_height = 40,
                    .action_button_height = 76,
                    .console_height = 44
                };
            case density_mode::comfortable:
                return density_metrics{};
        }
        return density_metrics{};
    }

}
