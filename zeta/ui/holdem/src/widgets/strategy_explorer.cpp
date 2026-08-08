#include "widgets/strategy_explorer.h"

#include <QAbstractItemView>
#include <QComboBox>
#include <QFrame>
#include <QGridLayout>
#include <QHeaderView>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QSizePolicy>
#include <QSignalBlocker>
#include <QStringList>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QTreeWidget>
#include <QTreeWidgetItem>
#include <QVBoxLayout>

#include <algorithm>
#include <cmath>
#include <utility>

namespace zeta::holdem::ui::widgets {

    namespace {

        class numeric_table_item final : public QTableWidgetItem {
        public:
            numeric_table_item(const QString& text, const double value)
                : QTableWidgetItem{text}
            {
                setData(Qt::UserRole, value);
            }

            [[nodiscard]] bool operator<(const QTableWidgetItem& other) const override
            {
                return data(Qt::UserRole).toDouble() < other.data(Qt::UserRole).toDouble();
            }
        };

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

        [[nodiscard]] QLabel* make_muted_label(const QString& text)
        {
            auto* label = new QLabel{text};
            label->setObjectName("mutedLabel");
            label->setWordWrap(true);
            return label;
        }

        [[nodiscard]] QString q(const std::string& value)
        {
            return QString::fromStdString(value);
        }

        [[nodiscard]] QString action_text(const std::vector<viewmodels::strategy_action_frequency>& actions)
        {
            return QString::fromStdString(viewmodels::format_strategy_actions(actions));
        }

        [[nodiscard]] QString ev_text(const double ev)
        {
            return QString::fromStdString(viewmodels::format_strategy_ev(ev));
        }

        [[nodiscard]] QString percent_text(const double frequency)
        {
            return QString::fromStdString(viewmodels::format_strategy_percent(frequency));
        }

        [[nodiscard]] QString seat_text(const solver::solution_store& solution, const uint8_t seat)
        {
            if (seat == solver::invalid_solution_seat) {
                return QStringLiteral("Terminal");
            }
            if (seat < solution.source.players.size()) {
                return QString::fromStdString(solution.source.players[seat]);
            }
            return QStringLiteral("Seat %1").arg(static_cast<unsigned>(seat) + 1u);
        }

        [[nodiscard]] QString blocked_text(const std::vector<std::string>& cards)
        {
            QStringList parts;
            for (const auto& card : cards) {
                parts.push_back(q(card));
            }
            return parts.join(QStringLiteral(", "));
        }

        void polish(QWidget* widget)
        {
            widget->style()->unpolish(widget);
            widget->style()->polish(widget);
        }

    }

    strategy_explorer::strategy_explorer(
        const spot& source,
        const solve_artifact& artifact,
        std::optional<solver::solution_store> solution,
        const theme::density_metrics metrics,
        QWidget* parent)
        : QWidget(parent)
        , model_(viewmodels::make_strategy_view_model(source, artifact))
        , solution_(std::move(solution).value_or(solver::make_root_only_solution_store(source, artifact)))
        , metrics_(metrics)
        , active_node_id_(q(solution_.root_node_id))
    {
        setObjectName("strategyExplorer");
        create_layout();
        refresh_filter();
    }

    strategy_explorer::strategy_explorer(
        const spot& source,
        const solve_artifact& artifact,
        const theme::density_metrics metrics,
        QWidget* parent)
        : strategy_explorer(
            source,
            artifact,
            solver::make_root_only_solution_store(source, artifact),
            metrics,
            parent)
    {
    }

    void strategy_explorer::create_layout()
    {
        auto* root = new QVBoxLayout{this};
        root->setContentsMargins(0, 0, 0, 0);
        root->setSpacing(metrics_.panel_spacing);

        auto* tree_panel = make_panel();
        auto* tree_layout = new QVBoxLayout{tree_panel};
        tree_layout->setContentsMargins(metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin);
        tree_layout->setSpacing(metrics_.panel_spacing);
        tree_layout->addWidget(make_panel_title(tr("Action Tree")));
        node_breadcrumb_ = make_muted_label(tr("Root"));
        node_breadcrumb_->setObjectName("solutionNodeBreadcrumb");
        tree_layout->addWidget(node_breadcrumb_);
        node_state_ = make_muted_label({});
        node_state_->setObjectName("solutionNodeState");
        tree_layout->addWidget(node_state_);

        auto* tree_body = new QHBoxLayout;
        tree_body->setSpacing(metrics_.panel_spacing);
        node_tree_ = new QTreeWidget{tree_panel};
        node_tree_->setObjectName("solutionActionTree");
        node_tree_->setColumnCount(3);
        node_tree_->setHeaderLabels({tr("Node"), tr("Actor"), tr("Actions")});
        node_tree_->setSelectionMode(QAbstractItemView::SingleSelection);
        node_tree_->header()->setStretchLastSection(true);
        if (const auto* root_node = solver::root_solution_node(solution_); root_node != nullptr) {
            auto* item = new QTreeWidgetItem{
                QStringList{
                    tr("Root"),
                    seat_text(solution_, root_node->acting_seat),
                    QString::number(root_node->legal_actions.size())
                }};
            item->setData(0, Qt::UserRole, q(root_node->node_id));
            item->setData(0, Qt::UserRole + 1, false);
            node_tree_->addTopLevelItem(item);
            populate_tree_item(item);
            item->setExpanded(true);
            node_tree_->setCurrentItem(item);
        }
        tree_body->addWidget(node_tree_, 3);

        node_action_table_ = new QTableWidget{tree_panel};
        node_action_table_->setObjectName("solutionNodeActionTable");
        node_action_table_->setColumnCount(3);
        node_action_table_->setHorizontalHeaderLabels({tr("Action"), tr("Frequency"), tr("EV")});
        node_action_table_->verticalHeader()->setVisible(false);
        node_action_table_->horizontalHeader()->setStretchLastSection(true);
        node_action_table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
        node_action_table_->setSelectionMode(QAbstractItemView::NoSelection);
        tree_body->addWidget(node_action_table_, 2);
        tree_layout->addLayout(tree_body);
        if (!solution_.diagnostics.empty()) {
            QStringList diagnostics;
            for (const auto& diagnostic : solution_.diagnostics) {
                diagnostics.push_back(q(diagnostic));
            }
            tree_layout->addWidget(make_muted_label(diagnostics.join(QStringLiteral(" "))));
        }
        root->addWidget(tree_panel, 1);

        auto* summary_panel = make_panel();
        auto* summary_layout = new QVBoxLayout{summary_panel};
        summary_layout->setContentsMargins(metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin);
        summary_layout->setSpacing(metrics_.panel_spacing);
        summary_layout->addWidget(make_panel_title(tr("Artifact")));

        const auto metadata = tr("%1 iterations %2 | %3 | players %4 | hero %5 | actor %6 | %7 %8 | git %9")
            .arg(q(model_.metadata.algorithm))
            .arg(static_cast<qulonglong>(model_.metadata.iterations))
            .arg(q(model_.metadata.timestamp))
            .arg(static_cast<qulonglong>(model_.metadata.player_count))
            .arg(q(model_.metadata.hero_label))
            .arg(q(model_.metadata.root_actor_label))
            .arg(q(model_.metadata.street))
            .arg(q(model_.metadata.board.empty() ? "-" : model_.metadata.board))
            .arg(q(model_.metadata.git_revision));
        auto* metadata_label = make_muted_label(metadata);
        metadata_label->setObjectName("artifactMetadataSummary");
        summary_layout->addWidget(metadata_label);

        QStringList ranges;
        for (const auto& range : model_.metadata.seat_ranges) {
            ranges.push_back(q(range));
        }
        summary_layout->addWidget(make_muted_label(tr("Ranges: %1").arg(ranges.join(QStringLiteral(" | ")))));

        auto* aggregate = new QWidget{summary_panel};
        aggregate->setObjectName("aggregateActionCards");
        auto* aggregate_layout = new QHBoxLayout{aggregate};
        aggregate_layout->setContentsMargins(0, 0, 0, 0);
        aggregate_layout->setSpacing(metrics_.panel_spacing);
        for (const auto& card : model_.action_cards) {
            auto* button = new QPushButton{
                q(card.action) + QStringLiteral("\n") + percent_text(card.frequency)
                    + QStringLiteral(" | EV ") + ev_text(card.average_ev),
                aggregate};
            button->setObjectName(card.action == "fold" ? "foldButton" : "callButton");
            button->setMinimumHeight(metrics_.action_button_height);
            button->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
            aggregate_layout->addWidget(button);
        }
        if (model_.action_cards.empty()) {
            aggregate_layout->addWidget(make_muted_label(tr("No action strategy is available.")));
        }
        summary_layout->addWidget(aggregate);
        summary_layout->addWidget(make_muted_label(tr("Average EV %1 | Mix %2%")
            .arg(ev_text(model_.average_ev))
            .arg(model_.mix_indicator * 100.0, 0, 'f', 1)));
        root->addWidget(summary_panel);

        auto* filter_panel = make_panel();
        auto* filter_layout = new QHBoxLayout{filter_panel};
        filter_layout->setContentsMargins(metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin);
        filter_layout->setSpacing(metrics_.panel_spacing);
        filter_layout->addWidget(make_panel_title(tr("Strategy")));
        filter_selector_ = new QComboBox{filter_panel};
        filter_selector_->setObjectName("strategyActionFilter");
        filter_layout->addWidget(filter_selector_);
        filter_layout->addStretch(1);
        root->addWidget(filter_panel);

        auto* body = new QHBoxLayout;
        body->setSpacing(metrics_.panel_spacing);

        auto* matrix_panel = make_panel();
        matrix_layout_ = new QGridLayout{matrix_panel};
        matrix_layout_->setContentsMargins(metrics_.panel_spacing, metrics_.panel_spacing, metrics_.panel_spacing, metrics_.panel_spacing);
        matrix_layout_->setSpacing(metrics_.panel_spacing / 2);
        matrix_cells_.reserve(model_.matrix.size());
        for (std::size_t row = 0; row < 13u; ++row) {
            for (std::size_t column = 0; column < 13u; ++column) {
                const auto index = row * 13u + column;
                auto* cell = new QPushButton{matrix_panel};
                cell->setObjectName("rangeCellMuted");
                cell->setFlat(true);
                cell->setCheckable(false);
                cell->setProperty("handClass", q(model_.matrix[index].hand_class));
                cell->setMinimumSize(metrics_.range_cell_min_width, metrics_.range_cell_min_height);
                cell->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
                connect(cell, &QPushButton::clicked, this, [this, cell] {
                    refresh_detail(cell->property("handClass").toString());
                });
                matrix_layout_->addWidget(cell, static_cast<int>(row), static_cast<int>(column));
                matrix_cells_.push_back(cell);
            }
        }
        body->addWidget(matrix_panel, 3);

        auto* detail_panel = make_panel();
        detail_panel->setObjectName("strategyDetailInspector");
        auto* detail_layout = new QVBoxLayout{detail_panel};
        detail_layout->setContentsMargins(metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin);
        detail_layout->setSpacing(metrics_.panel_spacing);
        detail_title_ = make_panel_title(tr("Hand Detail"));
        detail_layout->addWidget(detail_title_);
        detail_table_ = new QTableWidget{detail_panel};
        detail_table_->setObjectName("strategyDetailTable");
        detail_table_->setColumnCount(5);
        detail_table_->setHorizontalHeaderLabels({tr("Combo"), tr("Actions"), tr("EV"), tr("Weight"), tr("Blockers")});
        detail_table_->verticalHeader()->setVisible(false);
        detail_table_->horizontalHeader()->setStretchLastSection(true);
        detail_table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
        detail_table_->setSelectionMode(QAbstractItemView::NoSelection);
        detail_layout->addWidget(detail_table_);
        body->addWidget(detail_panel, 2);
        root->addLayout(body, 3);

        hand_table_ = new QTableWidget{this};
        hand_table_->setObjectName("strategyHandTable");
        hand_table_->setColumnCount(5);
        hand_table_->setHorizontalHeaderLabels({tr("Hand"), tr("Best action"), tr("Action frequencies"), tr("EV"), tr("Range weight")});
        hand_table_->verticalHeader()->setVisible(false);
        hand_table_->horizontalHeader()->setStretchLastSection(true);
        hand_table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
        hand_table_->setSelectionBehavior(QAbstractItemView::SelectRows);
        hand_table_->setSortingEnabled(true);
        root->addWidget(hand_table_, 2);

        connect(filter_selector_, &QComboBox::currentIndexChanged, this, [this](const int) {
            refresh_matrix();
            refresh_hand_table();
            if (!selected_hand_class_.isEmpty()) {
                refresh_detail(selected_hand_class_);
            }
        });
        connect(node_tree_, &QTreeWidget::itemExpanded, this, [this](QTreeWidgetItem* item) {
            populate_tree_item(item);
        });
        connect(node_tree_, &QTreeWidget::currentItemChanged, this, [this](QTreeWidgetItem* current, QTreeWidgetItem*) {
            if (current == nullptr) {
                return;
            }
            active_node_id_ = current->data(0, Qt::UserRole).toString();
            refresh_node_context();
        });
        connect(hand_table_, &QTableWidget::cellClicked, this, [this](const int row, const int) {
            auto* item = hand_table_->item(row, 0);
            if (item != nullptr) {
                refresh_detail(item->data(Qt::UserRole).toString());
            }
        });
    }

    void strategy_explorer::refresh_filter()
    {
        const QSignalBlocker blocker{filter_selector_};
        filter_selector_->clear();
        for (const auto& option : viewmodels::strategy_filter_options()) {
            filter_selector_->addItem(q(option.label), static_cast<int>(option.filter));
        }
        filter_selector_->setCurrentIndex(0);
        refresh_matrix();
        refresh_hand_table();
        refresh_node_context();

        const auto first = std::ranges::find_if(model_.matrix, [](const auto& cell) {
            return cell.available;
        });
        if (first != model_.matrix.end()) {
            refresh_detail(q(first->hand_class));
        }
    }

    void strategy_explorer::refresh_node_context()
    {
        const auto* node = solver::find_solution_node(solution_, active_node_id_.toStdString());
        if (node == nullptr) {
            node_breadcrumb_->setText(tr("Node unavailable"));
            node_state_->setText({});
            node_action_table_->setRowCount(0);
            return;
        }

        QStringList path;
        path.push_back(tr("Root"));
        for (const auto& action : node->path) {
            path.push_back(q(action));
        }
        node_breadcrumb_->setText(path.join(QStringLiteral(" / ")));
        node_state_->setText(tr("Actor %1 | pot %2 | commitments %3 | stacks %4")
            .arg(seat_text(solution_, node->acting_seat))
            .arg(node->table_state.pot, 0, 'f', 2)
            .arg(node->table_state.commitments.size())
            .arg(node->table_state.stacks.size()));

        node_action_table_->setRowCount(static_cast<int>(node->legal_actions.size()));
        for (int row = 0; row < node_action_table_->rowCount(); ++row) {
            const auto& action = node->legal_actions[static_cast<std::size_t>(row)];
            const auto found = std::ranges::find_if(node->average_strategy, [&action](const auto& summary) {
                return summary.action == action;
            });
            node_action_table_->setItem(row, 0, new QTableWidgetItem{q(action)});
            node_action_table_->setItem(row, 1, new QTableWidgetItem{
                found == node->average_strategy.end() ? QStringLiteral("-") : percent_text(found->frequency)});
            node_action_table_->setItem(row, 2, new QTableWidgetItem{
                found == node->average_strategy.end() ? QStringLiteral("-") : ev_text(found->average_ev)});
        }

        refresh_matrix();
        refresh_hand_table();
        if (!selected_hand_class_.isEmpty()) {
            refresh_detail(selected_hand_class_);
        }
    }

    void strategy_explorer::refresh_matrix()
    {
        const auto filter = active_filter();
        const auto show_combo_strategy = active_node_has_combo_strategy();
        for (std::size_t index = 0; index < matrix_cells_.size(); ++index) {
            auto* cell = matrix_cells_[index];
            const auto& model = model_.matrix[index];
            const auto visible = show_combo_strategy && viewmodels::strategy_cell_matches_filter(model, filter);
            QString text = q(model.hand_class);
            if (show_combo_strategy && model.available) {
                text += QStringLiteral("\n%1\nEV %2")
                    .arg(action_text(model.actions))
                    .arg(ev_text(model.ev));
            } else if (!show_combo_strategy) {
                text += QStringLiteral("\nNo node strategy");
            } else {
                text += QStringLiteral("\nUnavailable");
            }
            cell->setText(text);
            cell->setEnabled(show_combo_strategy && model.available);
            cell->setObjectName(visible ? "rangeCellPrimary" : "rangeCellMuted");
            cell->setToolTip(show_combo_strategy && model.available
                ? tr("%1 combos | weight %2").arg(model.exact_combos.size()).arg(model.range_weight, 0, 'f', 3)
                : tr("No combo strategy is available for this node"));
            polish(cell);
        }
    }

    void strategy_explorer::refresh_hand_table()
    {
        if (!active_node_has_combo_strategy()) {
            hand_table_->setRowCount(0);
            return;
        }
        const auto rows = viewmodels::filtered_strategy_hands(model_, active_filter());
        hand_table_->setSortingEnabled(false);
        hand_table_->setRowCount(static_cast<int>(rows.size()));
        for (int row = 0; row < hand_table_->rowCount(); ++row) {
            const auto& hand = rows[static_cast<std::size_t>(row)];
            auto* hand_item = new QTableWidgetItem{q(hand.hand)};
            hand_item->setData(Qt::UserRole, q(hand.hand_class));
            hand_table_->setItem(row, 0, hand_item);
            hand_table_->setItem(row, 1, new QTableWidgetItem{q(hand.best_action)});
            hand_table_->setItem(row, 2, new QTableWidgetItem{action_text(hand.actions)});
            hand_table_->setItem(row, 3, new numeric_table_item{ev_text(hand.ev), hand.ev});
            hand_table_->setItem(row, 4, new numeric_table_item{QString::number(hand.range_weight, 'f', 3), hand.range_weight});
        }
        hand_table_->setSortingEnabled(true);
    }

    void strategy_explorer::refresh_detail(const QString& hand_class)
    {
        selected_hand_class_ = hand_class;
        if (!active_node_has_combo_strategy()) {
            detail_title_->setText(tr("Hand Detail"));
            detail_table_->setRowCount(0);
            return;
        }
        const auto found = std::ranges::find_if(model_.matrix, [&hand_class](const auto& cell) {
            return q(cell.hand_class) == hand_class;
        });
        if (found == model_.matrix.end() || !found->available) {
            detail_title_->setText(tr("Hand Detail"));
            detail_table_->setRowCount(0);
            return;
        }

        detail_title_->setText(tr("%1 | %2 | EV %3 | weight %4")
            .arg(hand_class)
            .arg(q(found->best_action))
            .arg(ev_text(found->ev))
            .arg(found->range_weight, 0, 'f', 3));

        std::vector<viewmodels::strategy_hand_row> rows;
        rows.reserve(found->exact_combos.size());
        for (const auto& row : found->exact_combos) {
            if (viewmodels::strategy_row_matches_filter(row, active_filter())) {
                rows.push_back(row);
            }
        }
        std::ranges::sort(rows, [](const auto& lhs, const auto& rhs) {
            if (std::fabs(lhs.ev - rhs.ev) > 0.000001) {
                return lhs.ev > rhs.ev;
            }
            return lhs.hand < rhs.hand;
        });

        detail_table_->setRowCount(static_cast<int>(rows.size()));
        for (int row = 0; row < detail_table_->rowCount(); ++row) {
            const auto& combo = rows[static_cast<std::size_t>(row)];
            detail_table_->setItem(row, 0, new QTableWidgetItem{q(combo.hand)});
            detail_table_->setItem(row, 1, new QTableWidgetItem{action_text(combo.actions)});
            detail_table_->setItem(row, 2, new QTableWidgetItem{ev_text(combo.ev)});
            detail_table_->setItem(row, 3, new QTableWidgetItem{QString::number(combo.range_weight, 'f', 3)});
            detail_table_->setItem(row, 4, new QTableWidgetItem{blocked_text(combo.blocked_by)});
        }
    }

    void strategy_explorer::populate_tree_item(QTreeWidgetItem* item)
    {
        if (item == nullptr || item->data(0, Qt::UserRole + 1).toBool()) {
            return;
        }
        const auto* node = solver::find_solution_node(solution_, item->data(0, Qt::UserRole).toString().toStdString());
        if (node == nullptr) {
            return;
        }
        for (const auto& child_id : node->children) {
            const auto* child = solver::find_solution_node(solution_, child_id);
            if (child == nullptr) {
                continue;
            }
            const auto label = child->path.empty() ? tr("Root") : q(child->path.back());
            auto* child_item = new QTreeWidgetItem{
                QStringList{
                    label,
                    seat_text(solution_, child->acting_seat),
                    QString::number(child->legal_actions.size())
                }};
            child_item->setData(0, Qt::UserRole, q(child->node_id));
            child_item->setData(0, Qt::UserRole + 1, false);
            item->addChild(child_item);
        }
        item->setData(0, Qt::UserRole + 1, true);
    }

    viewmodels::strategy_action_filter strategy_explorer::active_filter() const
    {
        if (filter_selector_ == nullptr || filter_selector_->currentIndex() < 0) {
            return viewmodels::strategy_action_filter::all;
        }
        return static_cast<viewmodels::strategy_action_filter>(filter_selector_->currentData().toInt());
    }

    bool strategy_explorer::active_node_has_combo_strategy() const
    {
        return active_node_id_.toStdString() == solution_.root_node_id;
    }

}
