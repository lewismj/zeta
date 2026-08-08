#pragma once

#include "theme/theme.h"

#include <optional>
#include <span>
#include <string_view>

namespace zeta::holdem::ui::theme {

    [[nodiscard]] std::span<const registered_theme> registered_themes() noexcept;
    [[nodiscard]] const registered_theme& default_theme() noexcept;
    [[nodiscard]] const registered_theme& find_theme(theme_id id) noexcept;
    [[nodiscard]] std::optional<theme_id> theme_id_from_key(std::string_view key) noexcept;
    [[nodiscard]] std::string_view theme_key(theme_id id) noexcept;

    [[nodiscard]] std::optional<density_mode> density_mode_from_key(std::string_view key) noexcept;
    [[nodiscard]] std::string_view density_mode_key(density_mode mode) noexcept;
    [[nodiscard]] std::string_view density_mode_label(density_mode mode) noexcept;
    [[nodiscard]] density_metrics metrics_for_density(density_mode mode) noexcept;

}
