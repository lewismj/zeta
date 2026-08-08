#pragma once

#include "../spot_document.h"
#include "../theme/theme.h"
#include "../viewmodels/strategy_view_model.h"

#include <QWidget>

#include <vector>

class QComboBox;
class QGridLayout;
class QLabel;
class QPushButton;
class QTableWidget;

namespace zeta::holdem::ui::widgets {

    /**
     * Root-artifact strategy explorer with matrix, filters, hand table, and inspector.
     */
    class strategy_explorer final : public QWidget {
    public:
        strategy_explorer(
            const spot& source,
            const solve_artifact& artifact,
            theme::density_metrics metrics,
            QWidget* parent = nullptr);

    private:
        void create_layout();
        void refresh_filter();
        void refresh_matrix();
        void refresh_hand_table();
        void refresh_detail(const QString& hand_class);
        [[nodiscard]] viewmodels::strategy_action_filter active_filter() const;

        viewmodels::strategy_view_model model_;
        theme::density_metrics metrics_;
        QString selected_hand_class_;
        QComboBox* filter_selector_ = nullptr;
        QGridLayout* matrix_layout_ = nullptr;
        std::vector<QPushButton*> matrix_cells_;
        QTableWidget* hand_table_ = nullptr;
        QLabel* detail_title_ = nullptr;
        QTableWidget* detail_table_ = nullptr;
    };

}
