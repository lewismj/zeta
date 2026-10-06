#include "main_window.h"

#include "main_window_constants.h"
#include "main_window_dialogs.h"
#include "main_window_visuals.h"

#include <QCloseEvent>
#include <QIcon>
#include <QPlainTextEdit>
#include <QSplitter>
#include <QTextCursor>

#include <algorithm>

namespace zeta::holdem::ui {

    main_window::main_window(QWidget* parent)
        : QMainWindow(parent)
    {
        setWindowIcon(QIcon{zeta_logo_pixmap(QSize{24, 24})});
        active_theme_ = settings_.active_theme();
        density_mode_ = settings_.density();
        solver_iterations_ = settings_.solver_iterations();
        progress_batch_iterations_ = settings_.solver_progress_batch_iterations();
        worker_threads_ = std::clamp(settings_.solver_worker_threads(), min_worker_threads, available_worker_threads());
        card_isomorphism_ = settings_.solver_card_isomorphism();
        allow_lossy_isomorphism_ = settings_.solver_allow_lossy_isomorphism();
        dynamic_pruning_ = settings_.solver_dynamic_pruning();
        pruning_threshold_ = settings_.solver_pruning_threshold();
        pruning_minimum_active_actions_ = settings_.solver_pruning_minimum_active_actions();
        pruning_reconsider_interval_ = settings_.solver_pruning_reconsider_interval();
        workspace_splitter_sizes_ = settings_.workspace_splitter_sizes();
        create_actions();
        create_layout();
        new_document();
        update_solver_controls();
    }

    void main_window::closeEvent(QCloseEvent* event)
    {
        finish_solver_if_ready();
        if (has_active_solve()) {
            show_themed_message(
                this,
                dialog_kind::info,
                tr("Solve in progress"),
                tr("A solve is still running for %1. Close the window after the solve finishes.")
                    .arg(active_solver_document_index_ >= 0 && active_solver_document_index_ < static_cast<int>(documents_.size())
                        ? display_name(documents_[active_solver_document_index_])
                        : tr("the active document")));
            event->ignore();
            return;
        }

        for (int i = static_cast<int>(documents_.size()) - 1; i >= 0; --i) {
            if (!maybe_close_document(i)) {
                event->ignore();
                return;
            }
        }
        save_window_settings();
        event->accept();
    }

    void main_window::append_solve_console(document_entry& entry, const QString& text)
    {
        QString console = QString::fromStdString(entry.solve_console_text);
        if (!console.isEmpty()) {
            console += QStringLiteral("\n");
        }
        console += text;
        set_solve_console(entry, console);
    }

    void main_window::set_solve_console(document_entry& entry, const QString& text)
    {
        entry.solve_console_text = text.toStdString();
        if (entry.solve_console != nullptr) {
            entry.solve_console->setPlainText(text);
            entry.solve_console->moveCursor(QTextCursor::End);
        }
    }

    main_window::document_entry* main_window::active_entry()
    {
        const int index = tabs_ == nullptr ? -1 : tabs_->currentIndex();
        if (index < 0 || index >= static_cast<int>(documents_.size())) {
            return nullptr;
        }
        return &documents_[index];
    }

    QString main_window::display_name(const document_entry& entry) const
    {
        if (!entry.document.file_path().empty()) {
            return QString::fromStdWString(entry.document.file_path().filename().wstring());
        }
        return tr("Untitled");
    }

}
