#pragma once

#include "theme/theme.h"

#include <QDialog>

class QCheckBox;
class QComboBox;
class QDoubleSpinBox;
class QSpinBox;

namespace zeta::holdem::ui {

    struct configuration_dialog_result {
        theme::theme_id theme = theme::theme_id::dark_pro;
        theme::density_mode density = theme::density_mode::comfortable;
        int solver_iterations = 100;
        int progress_batch_iterations = 1;
        int worker_threads = 1;
        bool card_isomorphism = false;
        bool allow_lossy_isomorphism = false;
        bool dynamic_pruning = false;
        double pruning_threshold = 0.01;
        int pruning_minimum_active_actions = 1;
        int pruning_reconsider_interval = 64;
    };

    class configuration_dialog final : public QDialog {
    public:
        configuration_dialog(
            const configuration_dialog_result& initial,
            QWidget* parent = nullptr);

        [[nodiscard]] configuration_dialog_result result() const;

    private:
        QComboBox* theme_combo_ = nullptr;
        QComboBox* density_combo_ = nullptr;
        QSpinBox* iterations_ = nullptr;
        QSpinBox* progress_batch_ = nullptr;
        QSpinBox* threads_ = nullptr;
        QCheckBox* card_isomorphism_ = nullptr;
        QCheckBox* allow_lossy_ = nullptr;
        QCheckBox* dynamic_pruning_ = nullptr;
        QDoubleSpinBox* pruning_threshold_ = nullptr;
        QSpinBox* pruning_minimum_ = nullptr;
        QSpinBox* pruning_interval_ = nullptr;
    };

}
