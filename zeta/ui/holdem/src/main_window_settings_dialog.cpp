#include "main_window.h"

#include "configuration_dialog.h"

#include <QDialog>

namespace zeta::holdem::ui {

    void main_window::show_configuration_settings()
    {
        configuration_dialog dialog{
            configuration_dialog_result{
                .theme = active_theme_,
                .density = density_mode_,
                .solver_iterations = solver_iterations_,
                .progress_batch_iterations = progress_batch_iterations_,
                .worker_threads = worker_threads_,
                .card_isomorphism = card_isomorphism_,
                .allow_lossy_isomorphism = allow_lossy_isomorphism_,
                .dynamic_pruning = dynamic_pruning_,
                .pruning_threshold = pruning_threshold_,
                .pruning_minimum_active_actions = pruning_minimum_active_actions_,
                .pruning_reconsider_interval = pruning_reconsider_interval_
            },
            this};
        dialog.setStyleSheet(styleSheet());
        (void) dialog.winId();
        apply_native_title_bar_theme(&dialog);

        if (dialog.exec() != QDialog::Accepted) {
            return;
        }

        const auto next = dialog.result();
        set_active_theme(next.theme);
        set_density_mode(next.density);

        solver_iterations_ = next.solver_iterations;
        progress_batch_iterations_ = next.progress_batch_iterations;
        worker_threads_ = next.worker_threads;
        card_isomorphism_ = next.card_isomorphism;
        allow_lossy_isomorphism_ = next.allow_lossy_isomorphism;
        dynamic_pruning_ = next.dynamic_pruning;
        pruning_threshold_ = next.pruning_threshold;
        pruning_minimum_active_actions_ = next.pruning_minimum_active_actions;
        pruning_reconsider_interval_ = next.pruning_reconsider_interval;
        settings_.set_solver_iterations(solver_iterations_);
        settings_.set_solver_progress_batch_iterations(progress_batch_iterations_);
        settings_.set_solver_worker_threads(worker_threads_);
        settings_.set_solver_card_isomorphism(card_isomorphism_);
        settings_.set_solver_allow_lossy_isomorphism(allow_lossy_isomorphism_);
        settings_.set_solver_dynamic_pruning(dynamic_pruning_);
        settings_.set_solver_pruning_threshold(pruning_threshold_);
        settings_.set_solver_pruning_minimum_active_actions(pruning_minimum_active_actions_);
        settings_.set_solver_pruning_reconsider_interval(pruning_reconsider_interval_);
        settings_.sync();
    }

}
