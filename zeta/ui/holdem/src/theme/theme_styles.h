#pragma once

#include "theme/theme.h"

#include <QString>

namespace zeta::holdem::ui::theme {

    [[nodiscard]] QString style_sheet(const registered_theme& theme, density_mode density);

}
