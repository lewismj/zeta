#pragma once

#include "app/app_settings.h"
#include "solver/solver_session.h"
#include "solver_state.h"
#include "spot_document.h"
#include "theme/theme.h"

#include <QMainWindow>

#include <future>
#include <memory>
#include <optional>
#include <string>
#include <vector>

class QAction;
class QActionGroup;
class QLabel;
class QListWidget;
class QMenu;
class QPlainTextEdit;
class QSpinBox;
class QSplitter;
class QTabWidget;
class QTimer;
class QWidget;

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
            QPlainTextEdit* solve_console = nullptr;
            QSplitter* workspace_splitter = nullptr;
            std::string solve_console_text;
            bool updating_editor = false;
        };

        void create_actions();
        void create_layout();
        void new_document();
        void open_document();
        void open_document_path(const std::filesystem::path& path);
        bool save_active_document();
        bool save_active_document_as();
        void validate_active_document();
        void solve_active_document();
        void cancel_solver();
        void finish_solver_if_ready();
        void finish_solver_session(solver::solver_session_result result);
        bool maybe_close_document(int index);
        bool maybe_close_active_solve(int index);
        bool parse_editor_into_document(document_entry& entry, bool show_error);
        void add_document_tab(spot_document document);
        [[nodiscard]] QWidget* create_document_widget(int index);
        void refresh_document_tab(int index);
        void append_solve_console(document_entry& entry, const QString& text);
        void set_solve_console(document_entry& entry, const QString& text);
        void apply_active_theme();
        void set_active_theme(theme::theme_id theme);
        void set_density_mode(theme::density_mode density);
        void refresh_all_document_tabs();
        void update_document_rail();
        void update_recent_files_menu();
        void add_recent_file(const std::filesystem::path& path);
        void restore_window_settings();
        void save_window_settings();
        void update_tab_title(int index);
        void update_window_title();
        void update_solver_controls();
        [[nodiscard]] bool has_active_solve() const;
        [[nodiscard]] document_entry* active_entry();
        [[nodiscard]] QString display_name(const document_entry& entry) const;

        app::app_settings settings_;
        QTabWidget* tabs_ = nullptr;
        QSplitter* shell_splitter_ = nullptr;
        QListWidget* document_rail_ = nullptr;
        QLabel* state_label_ = nullptr;
        QLabel* status_label_ = nullptr;
        QMenu* recent_files_menu_ = nullptr;
        QActionGroup* theme_actions_ = nullptr;
        QActionGroup* density_actions_ = nullptr;
        QAction* new_action_ = nullptr;
        QAction* open_action_ = nullptr;
        QAction* save_action_ = nullptr;
        QAction* save_as_action_ = nullptr;
        QAction* validate_action_ = nullptr;
        QAction* solve_action_ = nullptr;
        QAction* cancel_action_ = nullptr;
        QSpinBox* iterations_spin_ = nullptr;
        QTimer* solver_poll_timer_ = nullptr;
        std::vector<document_entry> documents_;
        solver_state_machine solver_state_{};
        std::shared_ptr<solver::solver_session> active_session_;
        std::future<solver::solver_session_result> active_solver_;
        theme::theme_id active_theme_ = theme::theme_id::dark_pro;
        theme::density_mode density_mode_ = theme::density_mode::comfortable;
        QList<int> workspace_splitter_sizes_;
        int active_solver_document_index_ = -1;
    };

}
