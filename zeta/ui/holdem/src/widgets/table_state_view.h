#pragma once

#include "../spot_document.h"
#include "../theme/theme.h"

#include <QFrame>

class QGridLayout;

namespace zeta::holdem::ui::widgets {

    /**
     * Renders a compact table-state view for 2 to 6 player Hold'em spots.
     */
    class table_state_view final : public QFrame {
    public:
        table_state_view(const spot& source, theme::density_metrics metrics, QWidget* parent = nullptr);

        void set_spot(const spot& source);

    private:
        void rebuild();

        spot spot_;
        theme::density_metrics metrics_;
        QGridLayout* layout_ = nullptr;
    };

}
