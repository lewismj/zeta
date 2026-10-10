#include "main_window.h"

#include "main_window_dialogs.h"
#include "main_window_visuals.h"
#include "widgets/spot_json_editor.h"

#include <QAction>
#include <QLabel>
#include <QTabWidget>
#include <QTimer>

#include <chrono>
#include <cstdlib>

namespace zeta::holdem::ui {

    void main_window::solve_active_document()
    {
        if (has_active_solve()) {
            show_themed_message(this, dialog_kind::warning, tr("Solve in progress"), tr("Wait for the active solve to finish before starting another one."));
            return;
        }
        auto* entry = active_entry();
        if (entry == nullptr || !parse_editor_into_document(*entry, true)) {
            return;
        }
        const int document_index = tabs_->currentIndex();
        if (auto transition = solver_state_.transition_to(solver_state::starting); !transition) {
            show_themed_message(this, dialog_kind::warning, tr("Invalid solver state"), QString::fromStdString(transition.error()));
            return;
        }

        solver::solver_session_request request{
            .spot_snapshot = entry->document.current_spot(),
            .iterations = static_cast<uint64_t>(solver_iterations_)
        };
        request.runtime.progress_batch_iterations = static_cast<uint64_t>(progress_batch_iterations_);
        request.runtime.worker_threads = static_cast<uint32_t>(worker_threads_);
        request.runtime.enable_card_isomorphism = card_isomorphism_;
        request.runtime.allow_lossy_card_isomorphism = allow_lossy_isomorphism_;
        request.runtime.pruning.enabled = dynamic_pruning_;
        request.runtime.pruning.prune_threshold = pruning_threshold_;
        request.runtime.pruning.minimum_active_actions = static_cast<uint32_t>(pruning_minimum_active_actions_);
        request.runtime.pruning.reconsider_interval = static_cast<uint32_t>(pruning_reconsider_interval_);
        if (const char* revision = std::getenv("ZETA_GIT_REVISION")) {
            request.runtime.git_revision = revision;
        }

        active_session_ = std::make_shared<solver::solver_session>(std::move(request));
        active_solver_document_index_ = document_index;
        const auto& session_request = active_session_->request();
        set_solve_console(*entry, tr("Started %1\nIterations: %2\nProgress batch: %3\nWorker threads: %4\nOutput: store artifact in active document\nPlayers: %5\nStatus: running")
            .arg(QString::fromStdString(cli::detail::now_utc_iso8601()))
            .arg(static_cast<qulonglong>(session_request.iterations))
            .arg(static_cast<qulonglong>(session_request.runtime.progress_batch_iterations))
            .arg(static_cast<qulonglong>(session_request.runtime.worker_threads))
            .arg(static_cast<qulonglong>(session_request.spot_snapshot.players.size())));
        if (entry->editor != nullptr) {
            entry->editor->setReadOnly(true);
        }
        status_label_->setText(tr("Solving %1 with %2 iterations.")
            .arg(display_name(*entry))
            .arg(static_cast<qulonglong>(session_request.iterations)));

        active_solver_ = std::async(std::launch::async, [session = active_session_] {
            return session->run();
        });
        (void) solver_state_.transition_to(solver_state::running);
        solver_poll_timer_->start();
        update_solver_controls();
    }

    void main_window::cancel_solver()
    {
        if (!has_active_solve()) {
            return;
        }
        if (active_session_) {
            active_session_->cancel_before_start();
        }
        if (solver_state_.state() == solver_state::running || solver_state_.state() == solver_state::starting) {
            (void) solver_state_.transition_to(solver_state::cancelling);
            if (active_solver_document_index_ >= 0 && active_solver_document_index_ < static_cast<int>(documents_.size())) {
                append_solve_console(documents_[active_solver_document_index_], tr("Cancellation requested. The run stops only if solver work has not started."));
            }
            status_label_->setText(tr("Cancellation requested."));
        }
        update_solver_controls();
    }

    void main_window::finish_solver_if_ready()
    {
        if (!active_solver_.valid()) {
            return;
        }
        if (active_solver_.wait_for(std::chrono::milliseconds{0}) != std::future_status::ready) {
            return;
        }
        auto result = active_solver_.get();
        finish_solver_session(std::move(result));
    }

    void main_window::finish_solver_session(solver::solver_session_result result)
    {
        solver_poll_timer_->stop();
        const int document_index = active_solver_document_index_;
        active_solver_document_index_ = -1;
        active_session_.reset();

        if (document_index < 0 || document_index >= static_cast<int>(documents_.size())) {
            (void) solver_state_.transition_to(result.terminal_state == solver::solver_session_terminal_state::failed
                ? solver_state::failed
                : solver_state::completed);
            update_solver_controls();
            return;
        }

        auto& entry = documents_[document_index];
        if (entry.editor != nullptr) {
            entry.editor->setReadOnly(false);
        }

        QString summary;
        switch (result.terminal_state) {
            case solver::solver_session_terminal_state::completed:
                if (result.artifact) {
                    auto solution = solver::make_action_tree_solution_store(result.spot_snapshot, *result.artifact);
                    entry.document.replace_artifact(std::move(result.artifact));
                    entry.document.replace_solution(std::move(solution));
                    entry.document.set_solved_spot(result.spot_snapshot);
                }
                summary = tr("completed");
                append_solve_console(entry, tr("Graph build: %1ms\nCFR: %2ms\nExtraction: %3ms\nFinished %4\nStatus: completed")
                    .arg(result.timing.graph_build_ms, 0, 'f', 3)
                    .arg(result.timing.cfr_iterations_ms, 0, 'f', 3)
                    .arg(result.timing.extraction_ms, 0, 'f', 3)
                    .arg(QString::fromStdString(result.metadata.finished_utc)));
                (void) solver_state_.transition_to(solver_state::completed);
                status_label_->setText(tr("Solve completed."));
                break;
            case solver::solver_session_terminal_state::failed:
                summary = tr("failed: %1").arg(QString::fromStdString(result.error_message));
                append_solve_console(entry, tr("Finished %1\nStatus: failed\nError: %2")
                    .arg(QString::fromStdString(result.metadata.finished_utc))
                    .arg(QString::fromStdString(result.error_message)));
                (void) solver_state_.transition_to(solver_state::failed);
                status_label_->setText(tr("Solve failed."));
                break;
            case solver::solver_session_terminal_state::cancelled_before_start:
                summary = tr("cancelled-before-start");
                append_solve_console(entry, tr("Finished %1\nStatus: cancelled before start")
                    .arg(QString::fromStdString(result.metadata.finished_utc)));
                if (solver_state_.state() == solver_state::running || solver_state_.state() == solver_state::starting) {
                    (void) solver_state_.transition_to(solver_state::cancelling);
                }
                (void) solver_state_.transition_to(solver_state::idle);
                status_label_->setText(tr("Solve cancelled before start."));
                break;
        }

        auto metadata = entry.document.metadata();
        metadata.last_solve_summary = summary.toStdString();
        entry.document.update_metadata(std::move(metadata));
        entry.document.add_history(solve_history_entry{
            .timestamp_utc = result.metadata.finished_utc,
            .iterations = result.iterations,
            .outcome = summary.toStdString()
        });
        refresh_document_tab(document_index);
        update_solver_controls();
    }

    void main_window::update_solver_controls()
    {
        const auto controls = solver_state_.controls();
        validate_action_->setEnabled(controls.validate_enabled && active_entry() != nullptr);
        solve_action_->setEnabled(controls.solve_enabled && active_entry() != nullptr);
        cancel_action_->setEnabled(controls.cancel_enabled);
        if (configuration_action_ != nullptr) {
            configuration_action_->setEnabled(!has_active_solve());
        }
        const bool active = has_active_solve();
        state_label_->setProperty("solverActive", active);
        status_label_->setProperty("solverActive", active);
        polish_widget(state_label_);
        polish_widget(status_label_);
        state_label_->setText(tr("State: %1").arg(QString::fromLatin1(to_string(solver_state_.state()))));
    }

    bool main_window::has_active_solve() const
    {
        return active_solver_.valid();
    }

}
