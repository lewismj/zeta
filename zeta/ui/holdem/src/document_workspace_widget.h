#pragma once

#include "spot_document.h"
#include "theme/theme.h"

#include <QList>
#include <QWidget>

#include <functional>

class QPlainTextEdit;
class QSplitter;
class QTabWidget;
class QLabel;

namespace zeta::holdem::ui {

    namespace widgets {
        class spot_json_editor;
        class table_state_view;
    }

    struct document_workspace_callbacks {
        std::function<void(spot)> on_spot_changed;
        std::function<void(const spot&)> on_duplicate_requested;
        std::function<void()> on_raw_editor_dirty;
        std::function<void(const QString&)> on_leaving_raw_editor;
    };

    class document_workspace_widget final : public QWidget {
    public:
        document_workspace_widget(
            const spot_document& document,
            theme::theme_id active_theme,
            theme::density_metrics metrics,
            bool editor_read_only,
            const QList<int>& workspace_splitter_sizes,
            const QString& solve_console_text,
            document_workspace_callbacks callbacks,
            QWidget* parent = nullptr);

        [[nodiscard]] widgets::spot_json_editor* editor() const;
        [[nodiscard]] QPlainTextEdit* solve_console() const;
        [[nodiscard]] QSplitter* workspace_splitter() const;
        [[nodiscard]] QString current_sub_tab_text() const;
        void set_current_sub_tab_by_text(const QString& tab_text);

        void set_solve_console_text(const QString& text);
        void set_editor_read_only(bool read_only);
        void set_editor_json_text(const QString& text);
        void format_editor_if_valid();
        void set_updating_editor(bool updating);

        [[nodiscard]] QString editor_text() const;
        [[nodiscard]] bool is_updating_editor() const;
        void refresh_inspector(const spot_document& document);

    private:
        QWidget* create_actions_panel(const spot_document& document);
        QWidget* create_hands_panel(const spot_document& document);
        void update_inspector_summary(const spot_document& document);

        theme::theme_id active_theme_ = theme::theme_id::dark_pro;
        theme::density_metrics metrics_{};
        document_workspace_callbacks callbacks_;
        bool updating_editor_ = false;

        QTabWidget* left_tabs_ = nullptr;
        widgets::spot_json_editor* raw_editor_ = nullptr;
        QWidget* right_column_ = nullptr;
        widgets::table_state_view* table_view_ = nullptr;
        QPlainTextEdit* solve_console_ = nullptr;
        QSplitter* workspace_splitter_ = nullptr;
        QLabel* summary_header_ = nullptr;
    };

}
