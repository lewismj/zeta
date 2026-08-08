#pragma once

#include "theme/theme.h"

#include <QString>

class QWidget;

namespace zeta::holdem::ui::theme {

    [[nodiscard]] QString style_sheet(const registered_theme& theme, density_mode density);
    void apply_native_title_bar(QWidget* window, const registered_theme& theme);

}
