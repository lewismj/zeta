#include "main_window.h"

#include "document_workspace_widget.h"
#include "main_window_dialogs.h"
#include "viewmodels/spot_view_model.h"
#include "widgets/spot_json_editor.h"

#include <QDialog>
#include <QFileDialog>
#include <QLabel>
#include <QPlainTextEdit>
#include <QSignalBlocker>
#include <QSplitter>
#include <QTabWidget>
#include <QTimer>

#include <filesystem>

namespace zeta::holdem::ui {

    void main_window::new_document()
    {
        add_document_tab(spot_document::create_new());
    }

    void main_window::open_document()
    {
        QFileDialog dialog{this, tr("Open Hold'em spot")};
        dialog.setAcceptMode(QFileDialog::AcceptOpen);
        dialog.setNameFilters({tr("JSON documents (*.json)"), tr("All files (*)")});
        dialog.setOption(QFileDialog::DontUseNativeDialog);
        dialog.setStyleSheet(styleSheet());
        (void) dialog.winId();
        apply_native_title_bar_theme(&dialog);
        const auto path = selected_file(dialog);
        if (path.isEmpty()) {
            return;
        }
        open_document_path(std::filesystem::path{path.toStdWString()});
    }

    void main_window::open_document_path(const std::filesystem::path& path)
    {
        auto document = spot_document::load(path);
        if (!document) {
            show_themed_message(this, dialog_kind::error, tr("Open failed"), error_text(document.error()));
            return;
        }
        add_document_tab(std::move(*document));
        add_recent_file(path);
    }

    bool main_window::save_active_document()
    {
        auto* entry = active_entry();
        if (entry == nullptr) {
            return false;
        }
        if (entry->document.file_path().empty()) {
            return save_active_document_as();
        }
        if (!parse_editor_into_document(*entry, true)) {
            return false;
        }
        auto result = entry->document.save();
        if (!result) {
            show_themed_message(this, dialog_kind::error, tr("Save failed"), error_text(result.error()));
            return false;
        }
        entry->document.clear_dirty();
        add_recent_file(entry->document.file_path());
        update_tab_title(tabs_->currentIndex());
        update_window_title();
        return true;
    }

    bool main_window::save_active_document_as()
    {
        auto* entry = active_entry();
        if (entry == nullptr) {
            return false;
        }
        QFileDialog dialog{this, tr("Save Hold'em spot")};
        dialog.setAcceptMode(QFileDialog::AcceptSave);
        dialog.setNameFilters({tr("JSON documents (*.json)"), tr("All files (*)")});
        dialog.setDefaultSuffix(QStringLiteral("json"));
        dialog.setOption(QFileDialog::DontUseNativeDialog);
        dialog.setStyleSheet(styleSheet());
        (void) dialog.winId();
        apply_native_title_bar_theme(&dialog);
        const auto path = selected_file(dialog);
        if (path.isEmpty()) {
            return false;
        }
        if (!parse_editor_into_document(*entry, true)) {
            return false;
        }
        auto result = entry->document.save_as(std::filesystem::path{path.toStdWString()});
        if (!result) {
            show_themed_message(this, dialog_kind::error, tr("Save failed"), error_text(result.error()));
            return false;
        }
        add_recent_file(entry->document.file_path());
        update_tab_title(tabs_->currentIndex());
        update_window_title();
        return true;
    }

    void main_window::validate_active_document()
    {
        auto* entry = active_entry();
        if (entry == nullptr) {
            return;
        }
        if (auto transition = solver_state_.transition_to(solver_state::validating); !transition) {
            show_themed_message(this, dialog_kind::warning, tr("Invalid solver state"), QString::fromStdString(transition.error()));
            return;
        }
        update_solver_controls();
        const bool ok = parse_editor_into_document(*entry, true);
        if (ok) {
            refresh_document_tab(tabs_->currentIndex());
        }
        (void) solver_state_.transition_to(ok ? solver_state::idle : solver_state::failed);
        status_label_->setText(ok ? tr("Spot is valid.") : tr("Spot validation failed."));
        update_solver_controls();
    }

    bool main_window::maybe_close_document(const int index)
    {
        if (index < 0 || index >= static_cast<int>(documents_.size())) {
            return true;
        }
        finish_solver_if_ready();
        if (!maybe_close_active_solve(index)) {
            return false;
        }
        tabs_->setCurrentIndex(index);
        auto& entry = documents_[index];
        if (!entry.document.is_dirty()) {
            return true;
        }
        const int choice = show_themed_dialog(
            this,
            dialog_kind::warning,
            tr("Unsaved changes"),
            tr("Save changes to %1?").arg(display_name(entry)),
            {{1, tr("Save")}, {2, tr("Discard")}, {0, tr("Cancel")}},
            1);
        if (choice == 0) {
            return false;
        }
        if (choice == 2) {
            return true;
        }
        return save_active_document();
    }

    bool main_window::maybe_close_active_solve(const int index)
    {
        if (!has_active_solve() || index != active_solver_document_index_) {
            return true;
        }
        show_themed_message(
            this,
            dialog_kind::info,
            tr("Solve in progress"),
            tr("A solve is still running for %1. Close this document after the solve finishes.")
                .arg(display_name(documents_[index])));
        return false;
    }

    bool main_window::parse_editor_into_document(document_entry& entry, const bool show_error)
    {
        if (entry.editor == nullptr) {
            return false;
        }
        auto parsed = cli::parse_spot_json(entry.editor->toPlainText().toStdString());
        if (!parsed) {
            if (show_error) {
                show_themed_message(this, dialog_kind::error, tr("Invalid spot"), QString::fromStdString(parsed.error().message));
            }
            return false;
        }
        const auto issues = viewmodels::validate_structured_spot(*parsed);
        if (!issues.empty()) {
            if (show_error) {
                show_themed_message(this, dialog_kind::error, tr("Invalid spot"), validation_text(issues));
            }
            return false;
        }
        const bool was_dirty = entry.document.is_dirty();
        entry.document.replace_spot(std::move(*parsed));
        if (!was_dirty) {
            entry.document.clear_dirty();
        }
        update_tab_title(tabs_->currentIndex());
        return true;
    }

    void main_window::add_document_tab(spot_document document)
    {
        documents_.push_back(document_entry{
            .document = std::move(document),
            .editor = nullptr,
            .solve_console = nullptr,
            .workspace_splitter = nullptr,
            .solve_console_text = "Ready.\nValidate the spot, then solve to stream progress here.",
            .updating_editor = false
        });
        const int index = static_cast<int>(documents_.size()) - 1;
        auto* root = create_document_widget(index);
        tabs_->addTab(root, display_name(documents_.back()));
        tabs_->setCurrentIndex(index);
        update_document_rail();
        update_tab_title(index);
        update_window_title();
    }

    QWidget* main_window::create_document_widget(const int index)
    {
        auto& entry = documents_[index];
        const auto metrics = theme::metrics_for_density(density_mode_);
        auto* workspace = new document_workspace_widget{
            entry.document,
            active_theme_,
            metrics,
            index == active_solver_document_index_ && has_active_solve(),
            workspace_splitter_sizes_,
            QString::fromStdString(entry.solve_console_text),
            document_workspace_callbacks{
                .on_spot_changed = [this, index](spot next_spot) {
                    if (index < 0 || index >= static_cast<int>(documents_.size())) {
                        return;
                    }
                    documents_[index].document.replace_spot(std::move(next_spot));
                    refresh_workspace_from_document(index);
                },
                .on_duplicate_requested = [this](const spot& source) {
                    auto document = spot_document::create_new();
                    document.replace_spot(source);
                    add_document_tab(std::move(document));
                },
                .on_raw_editor_dirty = [this, index] {
                    if (index < 0 || index >= static_cast<int>(documents_.size())) {
                        return;
                    }
                    auto& dirty_entry = documents_[index];
                    if (dirty_entry.updating_editor) {
                        return;
                    }
                    dirty_entry.document.mark_dirty();
                    update_tab_title(index);
                    update_window_title();
                },
                .on_leaving_raw_editor = [this, index](const QString& target_tab) {
                    if (index < 0 || index >= static_cast<int>(documents_.size())) {
                        return;
                    }
                    auto& leaving_entry = documents_[index];
                    if (!parse_editor_into_document(leaving_entry, true)) {
                        if (auto* widget = dynamic_cast<document_workspace_widget*>(tabs_->widget(index))) {
                            widget->set_current_sub_tab_by_text(tr("Spot JSON"));
                        }
                        return;
                    }
                    leaving_entry.updating_editor = true;
                    if (auto* widget = dynamic_cast<document_workspace_widget*>(tabs_->widget(index))) {
                        widget->set_updating_editor(true);
                        widget->format_editor_if_valid();
                        widget->set_updating_editor(false);
                    }
                    leaving_entry.updating_editor = false;
                    QTimer::singleShot(0, this, [this, index, target_tab] {
                        if (index < 0 || index >= static_cast<int>(documents_.size())) {
                            return;
                        }
                        refresh_document_tab(index);
                        if (auto* refreshed = dynamic_cast<document_workspace_widget*>(tabs_->widget(index))) {
                            refreshed->set_current_sub_tab_by_text(target_tab);
                        }
                    });
                }
            },
            this};

        entry.editor = workspace->editor();
        entry.solve_console = workspace->solve_console();
        entry.workspace_splitter = workspace->workspace_splitter();
        return workspace;
    }

    void main_window::refresh_document_tab(const int index)
    {
        if (index < 0 || index >= static_cast<int>(documents_.size())) {
            return;
        }
        if (documents_[index].workspace_splitter != nullptr) {
            workspace_splitter_sizes_ = documents_[index].workspace_splitter->sizes();
        }
        auto* old_widget = tabs_->widget(index);
        auto* next_widget = create_document_widget(index);
        tabs_->removeTab(index);
        tabs_->insertTab(index, next_widget, display_name(documents_[index]));
        tabs_->setCurrentIndex(index);
        delete old_widget;
        update_tab_title(index);
        update_document_rail();
        update_window_title();
    }

    void main_window::refresh_workspace_from_document(const int index)
    {
        if (index < 0 || index >= static_cast<int>(documents_.size())) {
            return;
        }
        auto* workspace = dynamic_cast<document_workspace_widget*>(tabs_->widget(index));
        if (workspace == nullptr) {
            return;
        }
        auto& entry = documents_[index];
        entry.updating_editor = true;
        workspace->set_updating_editor(true);
        workspace->set_editor_json_text(QString::fromStdString(cli::serialize_spot_json(entry.document.current_spot())));
        workspace->set_updating_editor(false);
        entry.updating_editor = false;
        workspace->refresh_inspector(entry.document);
        update_tab_title(index);
        update_window_title();
    }

}
