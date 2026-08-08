#include "widgets/range_editor.h"

#include "viewmodels/range_view_model.h"

#include <QApplication>
#include <QAbstractItemView>
#include <QClipboard>
#include <QComboBox>
#include <QEvent>
#include <QFile>
#include <QFileDialog>
#include <QFrame>
#include <QGridLayout>
#include <QHeaderView>
#include <QHBoxLayout>
#include <QLabel>
#include <QMouseEvent>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QSizePolicy>
#include <QSignalBlocker>
#include <QStringList>
#include <QStyle>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QVBoxLayout>

#include <algorithm>
#include <cmath>
#include <utility>

namespace zeta::holdem::ui::widgets {

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

        [[nodiscard]] QLabel* make_error_label()
        {
            auto* label = new QLabel;
            label->setObjectName("errorLabel");
            label->setWordWrap(true);
            label->setVisible(false);
            return label;
        }

        [[nodiscard]] QString seat_text(const spot& source, const std::size_t seat)
        {
            const auto label = seat < source.players.size() && !source.players[seat].empty()
                ? source.players[seat]
                : "Seat " + std::to_string(seat + 1u);
            return QStringLiteral("%1: %2").arg(static_cast<qulonglong>(seat)).arg(QString::fromStdString(label));
        }

        [[nodiscard]] QString blocker_text(const std::vector<std::pair<std::string, std::size_t>>& blockers)
        {
            QStringList parts;
            for (const auto& [card, count] : blockers) {
                parts.push_back(QStringLiteral("%1 %2").arg(QString::fromStdString(card)).arg(static_cast<qulonglong>(count)));
            }
            return parts.join(QStringLiteral(", "));
        }

        [[nodiscard]] QString weight_text(const combo_weight weight)
        {
            if (std::fabs(weight - 1.0f) <= 0.0001f) {
                return {};
            }
            return QStringLiteral("\nx%1").arg(static_cast<double>(weight), 0, 'f', 2);
        }

        void polish(QWidget* widget)
        {
            widget->style()->unpolish(widget);
            widget->style()->polish(widget);
        }

    }

    range_editor::range_editor(
        const spot& source,
        const theme::density_metrics metrics,
        spot_changed_callback on_spot_changed,
        QWidget* parent)
        : QWidget(parent)
        , spot_(source)
        , metrics_(metrics)
        , on_spot_changed_(std::move(on_spot_changed))
    {
        setObjectName("rangeEditor");
        create_layout();
        refresh_from_spot();
    }

    void range_editor::set_spot(const spot& source)
    {
        spot_ = source;
        refresh_from_spot();
    }

    bool range_editor::eventFilter(QObject* watched, QEvent* event)
    {
        auto* button = qobject_cast<QPushButton*>(watched);
        if (button == nullptr || !button->property("handClass").isValid()) {
            return QWidget::eventFilter(watched, event);
        }

        if (event->type() == QEvent::MouseButtonPress) {
            auto* mouse = static_cast<QMouseEvent*>(event);
            if (mouse->button() == Qt::LeftButton) {
                drag_active_ = true;
                drag_enabled_ = !button->isChecked();
                set_hand_class(button->property("handClass").toString(), *drag_enabled_);
                return true;
            }
        }
        if (event->type() == QEvent::Enter && drag_active_ && drag_enabled_.has_value()) {
            set_hand_class(button->property("handClass").toString(), *drag_enabled_);
            return true;
        }
        if (event->type() == QEvent::MouseButtonRelease) {
            drag_active_ = false;
            drag_enabled_.reset();
        }

        return QWidget::eventFilter(watched, event);
    }

    void range_editor::create_layout()
    {
        auto* root = new QVBoxLayout{this};
        root->setContentsMargins(0, 0, 0, 0);
        root->setSpacing(metrics_.panel_spacing);

        auto* author_panel = make_panel();
        auto* author_layout = new QVBoxLayout{author_panel};
        author_layout->setContentsMargins(metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin);
        author_layout->setSpacing(metrics_.panel_spacing);

        auto* header = new QHBoxLayout;
        header->setSpacing(metrics_.panel_spacing);
        header->addWidget(make_panel_title(tr("Range")));
        seat_selector_ = new QComboBox{author_panel};
        seat_selector_->setObjectName("rangeSeatSelector");
        header->addWidget(seat_selector_);
        header->addStretch(1);

        auto* pairs = new QPushButton{tr("Pairs"), author_panel};
        auto* suited = new QPushButton{tr("Suited"), author_panel};
        auto* offsuit = new QPushButton{tr("Offsuit"), author_panel};
        auto* broadways = new QPushButton{tr("Broadways"), author_panel};
        auto* clear = new QPushButton{tr("Clear"), author_panel};
        auto* copy = new QPushButton{tr("Copy"), author_panel};
        auto* paste = new QPushButton{tr("Paste"), author_panel};
        auto* normalize = new QPushButton{tr("Normalize"), author_panel};
        auto* import_text = new QPushButton{tr("Import"), author_panel};
        auto* export_text = new QPushButton{tr("Export"), author_panel};
        header->addWidget(pairs);
        header->addWidget(suited);
        header->addWidget(offsuit);
        header->addWidget(broadways);
        header->addWidget(clear);
        header->addWidget(copy);
        header->addWidget(paste);
        header->addWidget(normalize);
        header->addWidget(import_text);
        header->addWidget(export_text);
        author_layout->addLayout(header);

        range_text_ = new QPlainTextEdit{author_panel};
        range_text_->setObjectName("rangeTextEditor");
        range_text_->setMaximumHeight(metrics_.console_height);
        range_text_->setLineWrapMode(QPlainTextEdit::WidgetWidth);
        author_layout->addWidget(range_text_);

        parse_error_ = make_error_label();
        parse_error_->setObjectName("rangeParseError");
        metrics_label_ = new QLabel{author_panel};
        metrics_label_->setObjectName("mutedLabel");
        metrics_label_->setWordWrap(true);
        author_layout->addWidget(parse_error_);
        author_layout->addWidget(metrics_label_);
        root->addWidget(author_panel);

        auto* matrix_panel = make_panel();
        matrix_layout_ = new QGridLayout{matrix_panel};
        matrix_layout_->setContentsMargins(metrics_.panel_spacing, metrics_.panel_spacing, metrics_.panel_spacing, metrics_.panel_spacing);
        matrix_layout_->setSpacing(metrics_.panel_spacing / 2);
        const auto labels = viewmodels::hand_class_labels();
        matrix_cells_.reserve(labels.size());
        for (std::size_t row = 0; row < 13u; ++row) {
            for (std::size_t column = 0; column < 13u; ++column) {
                const auto index = row * 13u + column;
                auto* cell = new QPushButton{QString::fromStdString(labels[index]), matrix_panel};
                cell->setObjectName("rangeCellMuted");
                cell->setCheckable(true);
                cell->setFlat(true);
                cell->setProperty("handClass", QString::fromStdString(labels[index]));
                cell->setMinimumSize(metrics_.range_cell_min_width, metrics_.range_cell_min_height);
                cell->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
                cell->installEventFilter(this);
                matrix_layout_->addWidget(cell, static_cast<int>(row), static_cast<int>(column));
                matrix_cells_.push_back(cell);
            }
        }
        root->addWidget(matrix_panel, 2);

        combo_table_ = new QTableWidget{this};
        combo_table_->setObjectName("exactComboTable");
        combo_table_->setColumnCount(5);
        combo_table_->setHorizontalHeaderLabels({tr("Combo"), tr("Class"), tr("Weight"), tr("Live"), tr("Blocked by")});
        combo_table_->verticalHeader()->setVisible(false);
        combo_table_->horizontalHeader()->setStretchLastSection(true);
        combo_table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
        combo_table_->setSelectionBehavior(QAbstractItemView::SelectRows);
        root->addWidget(combo_table_, 1);

        connect(seat_selector_, &QComboBox::currentIndexChanged, this, [this](const int index) {
            if (!updating_ && index >= 0) {
                refresh_from_spot();
            }
        });
        connect(range_text_, &QPlainTextEdit::textChanged, this, [this] {
            if (updating_) {
                return;
            }
            const auto seat = active_seat();
            if (spot_.ranges.size() < spot_.players.size()) {
                spot_.ranges.resize(spot_.players.size(), "AA");
            }
            if (seat < spot_.ranges.size()) {
                spot_.ranges[seat] = current_range_text().toStdString();
                refresh_analysis();
                emit_spot_changed();
            }
        });
        connect(pairs, &QPushButton::clicked, this, [this] { add_named_selection("pairs"); });
        connect(suited, &QPushButton::clicked, this, [this] { add_named_selection("suited"); });
        connect(offsuit, &QPushButton::clicked, this, [this] { add_named_selection("offsuit"); });
        connect(broadways, &QPushButton::clicked, this, [this] { add_named_selection("broadways"); });
        connect(clear, &QPushButton::clicked, this, [this] { set_current_range_text({}); });
        connect(copy, &QPushButton::clicked, this, [this] {
            QApplication::clipboard()->setText(current_range_text());
        });
        connect(paste, &QPushButton::clicked, this, [this] {
            set_current_range_text(QApplication::clipboard()->text());
        });
        connect(normalize, &QPushButton::clicked, this, [this] {
            const auto analysis = viewmodels::analyze_range(current_range_text().toStdString(), spot_.board);
            if (!analysis.parse_issue) {
                set_current_range_text(QString::fromStdString(viewmodels::normalized_exact_range_text(analysis)));
            }
        });
        connect(import_text, &QPushButton::clicked, this, [this] { import_range_text(); });
        connect(export_text, &QPushButton::clicked, this, [this] { export_range_text(); });
    }

    void range_editor::refresh_from_spot()
    {
        updating_ = true;
        refresh_seat_selector();
        const auto seat = active_seat();
        const auto text = seat < spot_.ranges.size() ? QString::fromStdString(spot_.ranges[seat]) : QString{};
        range_text_->setPlainText(text);
        updating_ = false;
        refresh_analysis();
    }

    void range_editor::refresh_seat_selector()
    {
        const QSignalBlocker blocker{seat_selector_};
        const int current = std::clamp(seat_selector_->currentIndex(), 0, std::max(0, static_cast<int>(spot_.players.size()) - 1));
        seat_selector_->clear();
        for (std::size_t seat = 0; seat < spot_.players.size(); ++seat) {
            seat_selector_->addItem(seat_text(spot_, seat), static_cast<int>(seat));
        }
        if (seat_selector_->count() > 0) {
            seat_selector_->setCurrentIndex(std::min(current, seat_selector_->count() - 1));
        }
    }

    void range_editor::refresh_analysis()
    {
        const auto analysis = viewmodels::analyze_range(current_range_text().toStdString(), spot_.board);
        if (analysis.parse_issue) {
            parse_error_->setText(tr("Position %1: %2")
                .arg(static_cast<qulonglong>(analysis.parse_issue->position))
                .arg(QString::fromStdString(analysis.parse_issue->message)));
            parse_error_->setVisible(true);
        } else if (analysis.metrics.live_combos == 0u) {
            parse_error_->setText(tr("Range has no live combos after board blockers."));
            parse_error_->setVisible(true);
        } else {
            parse_error_->clear();
            parse_error_->setVisible(false);
        }

        metrics_label_->setText(tr("%1 combos, %2 live, %3% of all hands. Blocked: %4")
            .arg(static_cast<qulonglong>(analysis.metrics.combos_before_blockers))
            .arg(static_cast<qulonglong>(analysis.metrics.live_combos))
            .arg(analysis.metrics.percent_total_hands, 0, 'f', 2)
            .arg(blocker_text(analysis.metrics.blocked_combos_by_card)));

        const QSignalBlocker table_blocker{combo_table_};
        combo_table_->setRowCount(static_cast<int>(analysis.exact_combos.size()));
        for (int row = 0; row < combo_table_->rowCount(); ++row) {
            const auto& combo = analysis.exact_combos[static_cast<std::size_t>(row)];
            QStringList blocked;
            for (const auto& card : combo.blocked_by) {
                blocked.push_back(QString::fromStdString(card));
            }
            combo_table_->setItem(row, 0, new QTableWidgetItem{QString::fromStdString(combo.hand)});
            combo_table_->setItem(row, 1, new QTableWidgetItem{QString::fromStdString(combo.hand_class)});
            combo_table_->setItem(row, 2, new QTableWidgetItem{QString::number(combo.weight, 'f', 3)});
            combo_table_->setItem(row, 3, new QTableWidgetItem{combo.live ? tr("Yes") : tr("No")});
            combo_table_->setItem(row, 4, new QTableWidgetItem{blocked.join(QStringLiteral(", "))});
        }

        for (std::size_t index = 0; index < matrix_cells_.size(); ++index) {
            auto* cell = matrix_cells_[index];
            const auto& model = analysis.matrix[index];
            const QSignalBlocker cell_blocker{cell};
            cell->setChecked(model.selected);
            cell->setText(QString::fromStdString(model.hand_class)
                + QStringLiteral("\n%1/%2").arg(static_cast<qulonglong>(model.live_combos)).arg(static_cast<qulonglong>(model.combos))
                + weight_text(model.max_weight));
            cell->setObjectName(model.selected && model.live_combos > 0u ? "rangeCellSelected" : "rangeCellMuted");
            cell->setToolTip(model.selected && model.live_combos == 0u ? tr("All combos blocked by board cards") : QString{});
            polish(cell);
        }
    }

    void range_editor::set_current_range_text(const QString& text)
    {
        const QSignalBlocker blocker{range_text_};
        range_text_->setPlainText(text);
        const auto seat = active_seat();
        if (spot_.ranges.size() < spot_.players.size()) {
            spot_.ranges.resize(spot_.players.size(), "AA");
        }
        if (seat < spot_.ranges.size()) {
            spot_.ranges[seat] = text.toStdString();
        }
        refresh_analysis();
        emit_spot_changed();
    }

    void range_editor::set_hand_class(const QString& hand_class, const bool enabled)
    {
        set_current_range_text(QString::fromStdString(viewmodels::set_hand_class_enabled(
            current_range_text().toStdString(),
            hand_class.toStdString(),
            enabled)));
    }

    void range_editor::add_named_selection(const char* class_name)
    {
        set_current_range_text(QString::fromStdString(viewmodels::add_named_class_selection(
            current_range_text().toStdString(),
            class_name)));
    }

    void range_editor::import_range_text()
    {
        const auto path = QFileDialog::getOpenFileName(this, tr("Import range"), {}, tr("Text files (*.txt);;All files (*)"));
        if (path.isEmpty()) {
            return;
        }
        QFile file{path};
        if (file.open(QIODevice::ReadOnly | QIODevice::Text)) {
            set_current_range_text(QString::fromUtf8(file.readAll()).trimmed());
        }
    }

    void range_editor::export_range_text()
    {
        const auto path = QFileDialog::getSaveFileName(this, tr("Export range"), {}, tr("Text files (*.txt);;All files (*)"));
        if (path.isEmpty()) {
            return;
        }
        QFile file{path};
        if (file.open(QIODevice::WriteOnly | QIODevice::Text | QIODevice::Truncate)) {
            file.write(current_range_text().toUtf8());
        }
    }

    void range_editor::emit_spot_changed()
    {
        if (on_spot_changed_) {
            on_spot_changed_(spot_);
        }
    }

    QString range_editor::current_range_text() const
    {
        return range_text_->toPlainText().trimmed();
    }

    std::size_t range_editor::active_seat() const
    {
        return static_cast<std::size_t>(std::max(0, seat_selector_->currentIndex()));
    }

}
