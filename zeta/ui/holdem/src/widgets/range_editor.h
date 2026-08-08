#pragma once

#include "../spot_document.h"
#include "../theme/theme.h"

#include <QWidget>

#include <functional>
#include <optional>
#include <vector>

class QComboBox;
class QGridLayout;
class QLabel;
class QPlainTextEdit;
class QPushButton;
class QTableWidget;

namespace zeta::holdem::ui::widgets {

    /**
     * Per-seat range authoring surface with parser feedback and combo inspection.
     */
    class range_editor final : public QWidget {
    public:
        using spot_changed_callback = std::function<void(spot)>;

        range_editor(
            const spot& source,
            theme::density_metrics metrics,
            spot_changed_callback on_spot_changed,
            QWidget* parent = nullptr,
            theme::theme_id active_theme = theme::theme_id::dark_pro);

        void set_spot(const spot& source);

    protected:
        bool eventFilter(QObject* watched, QEvent* event) override;

    private:
        void create_layout();
        void refresh_from_spot();
        void refresh_seat_selector();
        void refresh_analysis();
        void set_current_range_text(const QString& text);
        void set_hand_class(const QString& hand_class, bool enabled);
        void add_named_selection(const char* class_name);
        void import_range_text();
        void export_range_text();
        void emit_spot_changed();
        [[nodiscard]] QString current_range_text() const;
        [[nodiscard]] std::size_t active_seat() const;

        spot spot_;
        theme::density_metrics metrics_;
        theme::theme_id active_theme_;
        spot_changed_callback on_spot_changed_;
        bool updating_ = false;
        bool drag_active_ = false;
        std::optional<bool> drag_enabled_;

        QComboBox* seat_selector_ = nullptr;
        QPlainTextEdit* range_text_ = nullptr;
        QLabel* parse_error_ = nullptr;
        QLabel* metrics_label_ = nullptr;
        QGridLayout* matrix_layout_ = nullptr;
        std::vector<QPushButton*> matrix_cells_;
        QTableWidget* combo_table_ = nullptr;
    };

}
