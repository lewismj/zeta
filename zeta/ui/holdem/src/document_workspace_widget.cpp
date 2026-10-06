#include "document_workspace_widget.h"

#include "spot_summary_helpers.h"
#include "viewmodels/spot_view_model.h"
#include "widgets/range_editor.h"
#include "widgets/spot_builder.h"
#include "widgets/spot_json_editor.h"
#include "widgets/strategy_explorer.h"
#include "widgets/table_state_view.h"

#include <QAbstractItemView>
#include <QFrame>
#include <QHeaderView>
#include <QHBoxLayout>
#include <QLabel>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QSignalBlocker>
#include <QSizePolicy>
#include <QSplitter>
#include <QTabWidget>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QTextCursor>
#include <QVBoxLayout>

#include <utility>

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

        [[nodiscard]] QPushButton* make_action_button(
            const QString& action,
            const QString& percent,
            const QString& object_name,
            const theme::density_metrics& metrics)
        {
            auto* button = new QPushButton{action + QStringLiteral("\n") + percent};
            button->setObjectName(object_name);
            button->setMinimumHeight(metrics.action_button_height);
            button->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
            return button;
        }

    }

    document_workspace_widget::document_workspace_widget(
        const spot_document& document,
        const theme::theme_id active_theme,
        const theme::density_metrics metrics,
        const bool editor_read_only,
        const QList<int>& workspace_splitter_sizes,
        const QString& solve_console_text,
        document_workspace_callbacks callbacks,
        QWidget* parent)
        : QWidget(parent)
        , active_theme_(active_theme)
        , metrics_(metrics)
        , callbacks_(std::move(callbacks))
    {
        setObjectName("documentRoot");
        auto* root_layout = new QVBoxLayout{this};
        root_layout->setContentsMargins(metrics_.shell_margin, metrics_.shell_margin, metrics_.shell_margin, metrics_.shell_margin);
        root_layout->setSpacing(metrics_.panel_spacing);

        summary_header_ = make_panel_title(QString::fromStdString(viewmodels::spot_summary_text(document.current_spot(), document.artifact().has_value())));
        summary_header_->setObjectName("spotSummaryHeader");
        root_layout->addWidget(summary_header_);

        workspace_splitter_ = new QSplitter{Qt::Horizontal, this};
        left_tabs_ = new QTabWidget{workspace_splitter_};
        left_tabs_->setObjectName("solverSubTabs");

        raw_editor_ = new widgets::spot_json_editor{left_tabs_};
        raw_editor_->set_json_text(QString::fromStdString(cli::serialize_spot_json(document.current_spot())));
        raw_editor_->setReadOnly(editor_read_only);

        right_column_ = new QWidget{workspace_splitter_};
        right_column_->setObjectName("inspectorPanel");
        auto* right_layout = new QVBoxLayout{right_column_};
        right_layout->setContentsMargins(metrics_.shell_margin, metrics_.shell_margin, metrics_.shell_margin, metrics_.shell_margin);
        right_layout->setSpacing(metrics_.panel_spacing);
        table_view_ = new widgets::table_state_view{document.current_spot(), metrics_, right_column_};
        right_layout->addWidget(table_view_);

        auto* builder = new widgets::spot_builder{
            document.current_spot(),
            metrics_,
            [this](spot next_spot) {
                if (callbacks_.on_spot_changed) {
                    callbacks_.on_spot_changed(std::move(next_spot));
                }
            },
            [this](const spot& source) {
                if (callbacks_.on_duplicate_requested) {
                    callbacks_.on_duplicate_requested(source);
                }
            },
            left_tabs_};
        left_tabs_->addTab(builder, tr("Spot Builder"));
        if (document.artifact()) {
            left_tabs_->addTab(new widgets::strategy_explorer{
                document.current_spot(),
                *document.artifact(),
                document.solution(),
                metrics_,
                left_tabs_}, tr("Strategy Explorer"));
        } else {
            auto* range_editor = new widgets::range_editor{
                document.current_spot(),
                metrics_,
                [this](spot next_spot) {
                    if (callbacks_.on_spot_changed) {
                        callbacks_.on_spot_changed(std::move(next_spot));
                    }
                },
                left_tabs_,
                active_theme_};
            left_tabs_->addTab(range_editor, tr("Ranges"));
        }
        left_tabs_->addTab(raw_editor_, tr("Spot JSON"));

        if (!document.artifact()) {
            right_layout->addWidget(create_actions_panel(document));
            right_layout->addWidget(create_hands_panel(document), 1);
        } else {
            right_layout->addStretch(1);
        }

        workspace_splitter_->addWidget(left_tabs_);
        workspace_splitter_->addWidget(right_column_);
        workspace_splitter_->setStretchFactor(0, 3);
        workspace_splitter_->setStretchFactor(1, 2);
        if (workspace_splitter_sizes.size() == 2) {
            workspace_splitter_->setSizes(workspace_splitter_sizes);
        }
        root_layout->addWidget(workspace_splitter_, 1);

        solve_console_ = new QPlainTextEdit{this};
        solve_console_->setObjectName("solveConsole");
        solve_console_->setReadOnly(true);
        solve_console_->setMaximumHeight(metrics_.console_height);
        solve_console_->setPlainText(solve_console_text);
        root_layout->addWidget(solve_console_);

        connect(raw_editor_, &QPlainTextEdit::textChanged, this, [this] {
            if (!updating_editor_ && callbacks_.on_raw_editor_dirty) {
                callbacks_.on_raw_editor_dirty();
            }
        });

        int previous_sub_tab = left_tabs_->currentIndex();
        connect(left_tabs_, &QTabWidget::currentChanged, this, [this, previous_sub_tab](const int current) mutable {
            const auto previous_widget = left_tabs_->widget(previous_sub_tab);
            const auto current_widget = left_tabs_->widget(current);
            const bool leaving_raw_editor = previous_widget == raw_editor_ && current_widget != raw_editor_;
            previous_sub_tab = current;
            if (!leaving_raw_editor || !callbacks_.on_leaving_raw_editor) {
                return;
            }
            callbacks_.on_leaving_raw_editor(left_tabs_->tabText(current));
        });
    }

    widgets::spot_json_editor* document_workspace_widget::editor() const
    {
        return raw_editor_;
    }

    QPlainTextEdit* document_workspace_widget::solve_console() const
    {
        return solve_console_;
    }

    QSplitter* document_workspace_widget::workspace_splitter() const
    {
        return workspace_splitter_;
    }

    QString document_workspace_widget::current_sub_tab_text() const
    {
        if (left_tabs_ == nullptr || left_tabs_->currentIndex() < 0) {
            return {};
        }
        return left_tabs_->tabText(left_tabs_->currentIndex());
    }

    void document_workspace_widget::set_current_sub_tab_by_text(const QString& tab_text)
    {
        if (left_tabs_ == nullptr || tab_text.isEmpty()) {
            return;
        }
        QSignalBlocker blocker{left_tabs_};
        for (int i = 0; i < left_tabs_->count(); ++i) {
            if (left_tabs_->tabText(i) == tab_text) {
                left_tabs_->setCurrentIndex(i);
                return;
            }
        }
    }

    void document_workspace_widget::set_solve_console_text(const QString& text)
    {
        if (solve_console_ != nullptr) {
            solve_console_->setPlainText(text);
            solve_console_->moveCursor(QTextCursor::End);
        }
    }

    void document_workspace_widget::set_editor_read_only(const bool read_only)
    {
        if (raw_editor_ != nullptr) {
            raw_editor_->setReadOnly(read_only);
        }
    }

    void document_workspace_widget::set_editor_json_text(const QString& text)
    {
        if (raw_editor_ != nullptr) {
            raw_editor_->set_json_text(text);
        }
    }

    void document_workspace_widget::format_editor_if_valid()
    {
        if (raw_editor_ != nullptr) {
            raw_editor_->format_document_if_valid();
        }
    }

    void document_workspace_widget::set_updating_editor(const bool updating)
    {
        updating_editor_ = updating;
    }

    QString document_workspace_widget::editor_text() const
    {
        return raw_editor_ == nullptr ? QString{} : raw_editor_->toPlainText();
    }

    bool document_workspace_widget::is_updating_editor() const
    {
        return updating_editor_;
    }

    QWidget* document_workspace_widget::create_actions_panel(const spot_document& document)
    {
        auto* panel = make_panel();
        auto* layout = new QHBoxLayout{panel};
        layout->setContentsMargins(metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin);
        layout->setSpacing(metrics_.panel_spacing);

        const auto& current_spot = document.current_spot();
        layout->addWidget(make_action_button(
            range_summary_title(current_spot),
            range_summary_value(current_spot),
            QStringLiteral("callButton"),
            metrics_));
        layout->addWidget(make_action_button(
            QStringLiteral("Bet size"),
            bet_summary_value(current_spot),
            QStringLiteral("foldButton"),
            metrics_));
        return panel;
    }

    QWidget* document_workspace_widget::create_hands_panel(const spot_document& document)
    {
        auto* table = new QTableWidget{1, 2};
        table->setObjectName("handsTable");
        table->setHorizontalHeaderLabels({QStringLiteral("Input"), QStringLiteral("Value")});
        table->verticalHeader()->setVisible(false);
        table->horizontalHeader()->setStretchLastSection(true);
        table->setEditTriggers(QAbstractItemView::NoEditTriggers);
        table->setSelectionMode(QAbstractItemView::NoSelection);

        const auto& current_spot = document.current_spot();
        const auto range_index = editable_range_index(current_spot);
        table->setItem(0, 0, new QTableWidgetItem{range_summary_title(current_spot)});
        table->setItem(0, 1, new QTableWidgetItem{
            range_index < current_spot.ranges.size() ? QString::fromStdString(current_spot.ranges[range_index]) : QString{}});
        return table;
    }

    void document_workspace_widget::update_inspector_summary(const spot_document& document)
    {
        if (right_column_ == nullptr) {
            return;
        }
        const auto& current_spot = document.current_spot();
        if (auto* range_button = right_column_->findChild<QPushButton*>(QStringLiteral("callButton"))) {
            range_button->setText(range_summary_title(current_spot) + QStringLiteral("\n") + range_summary_value(current_spot));
        }
        if (auto* bet_button = right_column_->findChild<QPushButton*>(QStringLiteral("foldButton"))) {
            bet_button->setText(QStringLiteral("Bet size\n") + bet_summary_value(current_spot));
        }
        if (auto* hands_table = right_column_->findChild<QTableWidget*>(QStringLiteral("handsTable"))) {
            const auto range_index = editable_range_index(current_spot);
            hands_table->setItem(0, 0, new QTableWidgetItem{range_summary_title(current_spot)});
            hands_table->setItem(0, 1, new QTableWidgetItem{
                range_index < current_spot.ranges.size() ? QString::fromStdString(current_spot.ranges[range_index]) : QString{}});
        }
    }

    void document_workspace_widget::refresh_inspector(const spot_document& document)
    {
        if (summary_header_ != nullptr) {
            summary_header_->setText(QString::fromStdString(viewmodels::spot_summary_text(document.current_spot(), document.artifact().has_value())));
        }
        if (table_view_ != nullptr) {
            table_view_->set_spot(document.current_spot());
        }
        update_inspector_summary(document);
    }

}
