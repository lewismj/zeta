#pragma once

#include "solver_state.h"
#include "spot_document.h"

#include <QMainWindow>

#include <vector>

class QAction;
class QLabel;
class QPlainTextEdit;
class QTabWidget;

namespace zeta::holdem::ui {

    /**
     * Hosts multiple spot documents and exposes file actions with dirty-state prompts.
     */
    class main_window final : public QMainWindow {
    public:
        explicit main_window(QWidget* parent = nullptr);

    protected:
        void closeEvent(QCloseEvent* event) override;

    private:
        struct document_entry {
            spot_document document;
            QPlainTextEdit* editor = nullptr;
            bool updating_editor = false;
        };

        void create_actions();
        void create_layout();
        void new_document();
        void open_document();
        bool save_active_document();
        bool save_active_document_as();
        void validate_active_document();
        void solve_active_document();
        void cancel_solver();
        bool maybe_close_document(int index);
        bool parse_editor_into_document(document_entry& entry, bool show_error);
        void add_document_tab(spot_document document);
        void update_tab_title(int index);
        void update_window_title();
        void update_solver_controls();
        [[nodiscard]] document_entry* active_entry();
        [[nodiscard]] QString display_name(const document_entry& entry) const;

        QTabWidget* tabs_ = nullptr;
        QLabel* state_label_ = nullptr;
        QLabel* status_label_ = nullptr;
        QAction* new_action_ = nullptr;
        QAction* open_action_ = nullptr;
        QAction* save_action_ = nullptr;
        QAction* save_as_action_ = nullptr;
        QAction* validate_action_ = nullptr;
        QAction* solve_action_ = nullptr;
        QAction* cancel_action_ = nullptr;
        std::vector<document_entry> documents_;
        solver_state_machine solver_state_{};
    };

}
