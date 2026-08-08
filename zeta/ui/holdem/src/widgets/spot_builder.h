#pragma once

#include "../spot_document.h"
#include "../theme/theme.h"
#include "../viewmodels/spot_view_model.h"

#include <QWidget>

#include <functional>
#include <vector>

class QComboBox;
class QDoubleSpinBox;
class QGridLayout;
class QLabel;
class QPushButton;
class QSpinBox;
class QTableWidget;

namespace zeta::holdem::ui::widgets {

    /**
     * Structured editor for solver spot inputs.
     */
    class spot_builder final : public QWidget {
    public:
        using spot_changed_callback = std::function<void(spot)>;
        using duplicate_callback = std::function<void(const spot&)>;

        spot_builder(
            const spot& source,
            theme::density_metrics metrics,
            spot_changed_callback on_spot_changed,
            duplicate_callback on_duplicate,
            QWidget* parent = nullptr);

        void set_spot(const spot& source);

    private:
        void create_layout();
        void refresh_from_spot();
        void refresh_board_controls();
        void refresh_actor_selectors();
        void refresh_validation();
        void emit_spot_changed();
        void apply_template(viewmodels::spot_template_kind kind);
        [[nodiscard]] spot spot_from_controls() const;

        spot spot_;
        theme::density_metrics metrics_;
        spot_changed_callback on_spot_changed_;
        duplicate_callback on_duplicate_;
        bool updating_ = false;

        QComboBox* street_selector_ = nullptr;
        QSpinBox* player_count_ = nullptr;
        std::vector<QComboBox*> board_cards_;
        QComboBox* root_actor_ = nullptr;
        QComboBox* hero_seat_ = nullptr;
        QDoubleSpinBox* gross_pot_ = nullptr;
        QDoubleSpinBox* rake_ = nullptr;
        QDoubleSpinBox* bet_fraction_ = nullptr;
        QSpinBox* max_history_ = nullptr;
        QSpinBox* public_state_id_ = nullptr;
        QSpinBox* samples_per_combo_ = nullptr;
        QTableWidget* seat_table_ = nullptr;
        QLabel* board_error_ = nullptr;
        QLabel* players_error_ = nullptr;
        QLabel* actor_error_ = nullptr;
    };

}
