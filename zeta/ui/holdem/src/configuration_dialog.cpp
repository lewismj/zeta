#include "configuration_dialog.h"

#include "main_window_constants.h"
#include "theme/theme_registry.h"

#include <QCheckBox>
#include <QComboBox>
#include <QDoubleSpinBox>
#include <QFormLayout>
#include <QFrame>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QSpinBox>
#include <QTabWidget>
#include <QVBoxLayout>

#include <algorithm>

namespace zeta::holdem::ui {

    namespace {

        [[nodiscard]] QFrame* make_panel()
        {
            auto* panel = new QFrame;
            panel->setFrameShape(QFrame::StyledPanel);
            panel->setObjectName("solverPanel");
            return panel;
        }

        [[nodiscard]] QLabel* make_panel_title(const QString& text)
        {
            auto* label = new QLabel{text};
            label->setObjectName("panelTitle");
            return label;
        }

    }

    configuration_dialog::configuration_dialog(
        const configuration_dialog_result& initial,
        QWidget* parent)
        : QDialog(parent)
    {
        setWindowTitle(tr("Configuration Settings"));
        setObjectName("configurationDialog");
        setMinimumWidth(460);

        auto* root = new QVBoxLayout{this};
        root->setContentsMargins(18, 16, 18, 16);
        root->setSpacing(12);
        root->addWidget(make_panel_title(tr("Configuration")));

        auto* tabs = new QTabWidget{this};

        auto* ui_panel = make_panel();
        auto* ui_layout = new QFormLayout{ui_panel};
        ui_layout->setContentsMargins(14, 12, 14, 12);
        ui_layout->setSpacing(10);

        theme_combo_ = new QComboBox{ui_panel};
        for (const auto& registered_theme : theme::registered_themes()) {
            theme_combo_->addItem(
                QString::fromStdString(registered_theme.display_name),
                static_cast<int>(registered_theme.id));
        }
        theme_combo_->setCurrentIndex(theme_combo_->findData(static_cast<int>(initial.theme)));
        ui_layout->addRow(tr("Theme"), theme_combo_);

        density_combo_ = new QComboBox{ui_panel};
        density_combo_->addItem(tr("Comfortable"), static_cast<int>(theme::density_mode::comfortable));
        density_combo_->addItem(tr("Compact"), static_cast<int>(theme::density_mode::compact));
        density_combo_->setCurrentIndex(density_combo_->findData(static_cast<int>(initial.density)));
        ui_layout->addRow(tr("Density"), density_combo_);
        tabs->addTab(ui_panel, tr("UI"));

        auto* solver_panel = make_panel();
        auto* solver_layout = new QFormLayout{solver_panel};
        solver_layout->setContentsMargins(14, 12, 14, 12);
        solver_layout->setSpacing(10);

        iterations_ = new QSpinBox{solver_panel};
        iterations_->setRange(min_solver_iterations, max_solver_iterations);
        iterations_->setSingleStep(50);
        iterations_->setValue(initial.solver_iterations);
        iterations_->setToolTip(tr("CFR iterations for the next solve."));
        solver_layout->addRow(tr("Iterations"), iterations_);

        progress_batch_ = new QSpinBox{solver_panel};
        progress_batch_->setRange(min_solver_iterations, max_solver_iterations);
        progress_batch_->setSingleStep(10);
        progress_batch_->setValue(initial.progress_batch_iterations);
        progress_batch_->setToolTip(tr("Number of CFR iterations between progress updates."));
        solver_layout->addRow(tr("Progress batch iterations"), progress_batch_);

        threads_ = new QSpinBox{solver_panel};
        threads_->setObjectName("workerThreadsSpinBox");
        threads_->setRange(min_worker_threads, available_worker_threads());
        threads_->setValue(std::clamp(initial.worker_threads, min_worker_threads, available_worker_threads()));
        threads_->setToolTip(tr("CFR worker threads for the next solve."));
        solver_layout->addRow(tr("Worker threads"), threads_);

        card_isomorphism_ = new QCheckBox{solver_panel};
        card_isomorphism_->setObjectName("cardIsomorphismCheckBox");
        card_isomorphism_->setChecked(initial.card_isomorphism);
        card_isomorphism_->setToolTip(tr("Collapse suit-isomorphic turn/river runouts to shrink the game."));
        solver_layout->addRow(tr("Card isomorphism"), card_isomorphism_);

        allow_lossy_ = new QCheckBox{solver_panel};
        allow_lossy_->setObjectName("allowLossyIsomorphismCheckBox");
        allow_lossy_->setChecked(initial.allow_lossy_isomorphism);
        allow_lossy_->setToolTip(tr("Permit isomorphism even when ranges are not suit-symmetric (approximate)."));
        allow_lossy_->setEnabled(initial.card_isomorphism);
        solver_layout->addRow(tr("Allow lossy isomorphism"), allow_lossy_);
        connect(card_isomorphism_, &QCheckBox::toggled, allow_lossy_, &QWidget::setEnabled);

        dynamic_pruning_ = new QCheckBox{solver_panel};
        dynamic_pruning_->setObjectName("dynamicPruningCheckBox");
        dynamic_pruning_->setChecked(initial.dynamic_pruning);
        dynamic_pruning_->setToolTip(tr("Opt-in approximate solve: freeze low-regret actions and skip their subtrees."));
        solver_layout->addRow(tr("Dynamic action pruning"), dynamic_pruning_);

        pruning_threshold_ = new QDoubleSpinBox{solver_panel};
        pruning_threshold_->setObjectName("pruningThresholdSpinBox");
        pruning_threshold_->setDecimals(4);
        pruning_threshold_->setRange(0.0001, 0.9999);
        pruning_threshold_->setSingleStep(0.005);
        pruning_threshold_->setValue(initial.pruning_threshold);
        pruning_threshold_->setToolTip(tr("Reach-weighted positive-regret share below which an action is pruned."));
        pruning_threshold_->setEnabled(initial.dynamic_pruning);
        solver_layout->addRow(tr("Pruning threshold"), pruning_threshold_);

        pruning_minimum_ = new QSpinBox{solver_panel};
        pruning_minimum_->setObjectName("pruningMinimumActionsSpinBox");
        pruning_minimum_->setRange(1, 64);
        pruning_minimum_->setValue(initial.pruning_minimum_active_actions);
        pruning_minimum_->setToolTip(tr("Never prune below this many active actions per infoset."));
        pruning_minimum_->setEnabled(initial.dynamic_pruning);
        solver_layout->addRow(tr("Minimum active actions"), pruning_minimum_);

        pruning_interval_ = new QSpinBox{solver_panel};
        pruning_interval_->setObjectName("pruningReconsiderIntervalSpinBox");
        pruning_interval_->setRange(1, 1'000'000);
        pruning_interval_->setValue(initial.pruning_reconsider_interval);
        pruning_interval_->setToolTip(tr("Iterations between prune/reactivate reconsideration passes."));
        pruning_interval_->setEnabled(initial.dynamic_pruning);
        solver_layout->addRow(tr("Reconsider interval"), pruning_interval_);

        connect(dynamic_pruning_, &QCheckBox::toggled, pruning_threshold_, &QWidget::setEnabled);
        connect(dynamic_pruning_, &QCheckBox::toggled, pruning_minimum_, &QWidget::setEnabled);
        connect(dynamic_pruning_, &QCheckBox::toggled, pruning_interval_, &QWidget::setEnabled);
        tabs->addTab(solver_panel, tr("Solver"));

        root->addWidget(tabs);

        auto* buttons = new QHBoxLayout;
        buttons->addStretch(1);
        auto* ok = new QPushButton{tr("OK"), this};
        auto* cancel = new QPushButton{tr("Cancel"), this};
        ok->setDefault(true);
        buttons->addWidget(ok);
        buttons->addWidget(cancel);
        root->addLayout(buttons);
        connect(ok, &QPushButton::clicked, this, &QDialog::accept);
        connect(cancel, &QPushButton::clicked, this, &QDialog::reject);
    }

    configuration_dialog_result configuration_dialog::result() const
    {
        return configuration_dialog_result{
            .theme = static_cast<theme::theme_id>(theme_combo_->currentData().toInt()),
            .density = static_cast<theme::density_mode>(density_combo_->currentData().toInt()),
            .solver_iterations = iterations_->value(),
            .progress_batch_iterations = progress_batch_->value(),
            .worker_threads = threads_->value(),
            .card_isomorphism = card_isomorphism_->isChecked(),
            .allow_lossy_isomorphism = allow_lossy_->isChecked(),
            .dynamic_pruning = dynamic_pruning_->isChecked(),
            .pruning_threshold = pruning_threshold_->value(),
            .pruning_minimum_active_actions = pruning_minimum_->value(),
            .pruning_reconsider_interval = pruning_interval_->value()
        };
    }

}
