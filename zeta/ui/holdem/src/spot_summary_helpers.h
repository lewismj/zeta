#pragma once

#include "spot_document.h"

#include <QString>

namespace zeta::holdem::ui {

    [[nodiscard]] QString range_summary_title(const spot& source);
    [[nodiscard]] QString range_summary_value(const spot& source);
    [[nodiscard]] QString bet_summary_value(const spot& source);
    [[nodiscard]] std::size_t editable_range_index(const spot& source);

}
