#include "widgets/strategy_explorer.h"

#include "viewmodels/lock_view_model.h"
#include "widgets/node_lock_dialog.h"

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
#include <string_view>
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

        [[nodiscard]] QString display_action_text(const std::string_view action)
        {
            if (action.empty()) {
                return QStringLiteral("-");
            }
            if (action == "all_in" || action == "all-in" || action == "allin") {
                return QStringLiteral("All In");
            }
            if (action == "fold") {
                return QStringLiteral("Fold");
            }
            if (action == "check") {
                return QStringLiteral("Check");
            }
            if (action == "call") {
                return QStringLiteral("Call");
            }
            if (action.starts_with("bet_")) {
                return QStringLiteral("Bet %1").arg(QString::fromStdString(std::string{action.substr(4u)}));
            }
            if (action.starts_with("raise_")) {
                return QStringLiteral("Raise %1").arg(QString::fromStdString(std::string{action.substr(6u)}));
            }
            return QString::fromStdString(std::string{action});
        }

        [[nodiscard]] QString action_text(const std::vector<viewmodels::strategy_action_frequency>& actions)
        {
            if (actions.empty()) {
                return QStringLiteral("-");
            }
            QStringList parts;
            parts.reserve(static_cast<int>(actions.size()));
            for (const auto& action : actions) {
                parts.push_back(QStringLiteral("%1 %2")
                    .arg(display_action_text(action.action))
                    .arg(QString::fromStdString(viewmodels::format_strategy_percent(action.frequency))));
            }
            return parts.join(QStringLiteral(", "));
        }

        [[nodiscard]] QString ev_text(const double ev)
        {
            return QString::fromStdString(viewmodels::format_strategy_ev(ev));
        }

        [[nodiscard]] QString ev_cell_text(const bool reachable, const double ev)
        {
            return reachable ? ev_text(ev) : QStringLiteral("\u2014");
        }

        [[nodiscard]] QString unreachable_tooltip()
        {
            return QStringLiteral("No unblocked opponent combos \u2014 this matchup can't occur, so EV is undefined.");
        }

        [[nodiscard]] QString hand_ev_tooltip(const viewmodels::strategy_hand_row& hand)
        {
            if (!hand.reachable) {
                return unreachable_tooltip();
            }
            return QStringLiteral("Counterfactual value %1 over opponent reach mass %2")
                .arg(hand.counterfactual_value, 0, 'f', 3)
                .arg(hand.opponent_reach_mass, 0, 'f', 3);
        }

        [[nodiscard]] QString hand_ev_header_tooltip()
        {
            return QStringLiteral(
                "EV in chips for this exact hand at the root under the solved strategy: net chips won or lost over "
                "the whole hand, including its contribution already in the pot, averaged over the opponent hands it "
                "does not block. Hover a value for the counterfactual value and opponent reach mass it comes from. "
                "An em dash means no unblocked opponent combos exist, so EV is undefined.");
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
        , solution_(std::move(solution).value_or(solver::make_action_tree_solution_store(source, artifact)))
        , metrics_(metrics)
        , active_node_id_(q(solution_.root_node_id))
        , current_locks_(source.locks)
        , solved_locks_(artifact.solver.locks)
    {
        setObjectName("strategyExplorer");
        for (const auto& node : artifact.solved_nodes) {
            if (!node.lock_indices.empty()) {
                solved_lock_indices_.emplace(node.node_id, node.lock_indices);
            }
            for (const auto& action : node.actions) {
                if (action.child_node_id == cfr::game_graph::INVALID_NODE) {
                    continue;
                }
                std::string step;
                if (node.kind == "chance") {
                    for (const auto& card : action.dealt_cards) {
                        step += card;
                    }
                } else {
                    step = action.action;
                }
                lock_steps_.emplace(action.child_node_id, std::pair{node.node_id, std::move(step)});
            }
        }
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
            solver::make_action_tree_solution_store(source, artifact),
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
                    seat_text(solution_, root_node->acting_seat) + node_lock_suffix(root_node->graph_node_id),
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

        auto* action_column = new QVBoxLayout;
        action_column->setSpacing(metrics_.panel_spacing);
        node_action_table_ = new QTableWidget{tree_panel};
        node_action_table_->setObjectName("solutionNodeActionTable");
        node_action_table_->setColumnCount(4);
        node_action_table_->setHorizontalHeaderLabels({tr("Action"), tr("Frequency"), tr("Action EV"), tr("Lock")});
        node_action_table_->horizontalHeaderItem(2)->setToolTip(
            tr("The acting seat's range EV in chips after taking this action at the selected node, averaged over "
               "every hand matchup the two ranges allow. An em dash means the action is never taken."));
        node_action_table_->horizontalHeaderItem(3)->setToolTip(
            tr("The frequency a whole-range lock fixes for this action at the selected node in the current inputs. "
               "Blank actions are free for the solver to choose."));
        node_action_table_->verticalHeader()->setVisible(false);
        node_action_table_->horizontalHeader()->setStretchLastSection(true);
        node_action_table_->setEditTriggers(QAbstractItemView::NoEditTriggers);
        node_action_table_->setSelectionMode(QAbstractItemView::NoSelection);
        action_column->addWidget(node_action_table_, 1);

        lock_status_ = make_muted_label({});
        lock_status_->setObjectName("nodeLockStatus");
        action_column->addWidget(lock_status_);
        auto* lock_buttons = new QHBoxLayout;
        lock_buttons->setSpacing(metrics_.panel_spacing);
        lock_button_ = new QPushButton{tr("Lock node..."), tree_panel};
        lock_button_->setObjectName("lockNodeButton");
        lock_button_->setToolTip(tr("Fix some of the acting player's action frequencies at this node, then re-solve "
                                    "to see how the other player exploits them."));
        unlock_button_ = new QPushButton{tr("Remove locks"), tree_panel};
        unlock_button_->setObjectName("unlockNodeButton");
        unlock_button_->setToolTip(tr("Remove the locks written for this node. Locks that reach it through any "
                                      "dealt card ('Any card') are kept; remove them from the Spot Builder."));
        lock_buttons->addWidget(lock_button_);
        lock_buttons->addWidget(unlock_button_);
        lock_buttons->addStretch(1);
        action_column->addLayout(lock_buttons);
        tree_body->addLayout(action_column, 2);
        tree_layout->addLayout(tree_body);
        connect(lock_button_, &QPushButton::clicked, this, [this] {
            open_lock_dialog();
        });
        connect(unlock_button_, &QPushButton::clicked, this, [this] {
            remove_node_locks();
        });
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
                display_action_text(card.action) + QStringLiteral("\n") + percent_text(card.frequency)
                    + tr(" | EV ") + (card.ev_available ? ev_text(card.average_ev) : QStringLiteral("\u2014")),
                aggregate};
            button->setToolTip(tr("The root actor's range EV in chips after taking this action: net chips won or "
                                 "lost over the whole hand, averaged over every hand matchup the ranges allow. "
                                 "An em dash means the action is never taken."));
            button->setObjectName(card.action == "fold" ? "foldButton" : "callButton");
            button->setMinimumHeight(metrics_.action_button_height);
            button->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
            aggregate_layout->addWidget(button);
        }
        if (model_.action_cards.empty()) {
            aggregate_layout->addWidget(make_muted_label(tr("No action strategy is available.")));
        }
        summary_layout->addWidget(aggregate);
        auto* average_ev_label = make_muted_label(tr("Range EV %1 | Mix %2%")
            .arg(ev_text(model_.average_ev))
            .arg(model_.mix_indicator * 100.0, 0, 'f', 1));
        average_ev_label->setToolTip(tr("Hero's range EV in chips at the root under the solved strategy: each hand's "
                                       "EV weighted by its range weight and by the opponent reach it can meet, so "
                                       "every compatible matchup counts once. Fully blocked hands are excluded."));
        summary_layout->addWidget(average_ev_label);
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
        detail_table_->setHorizontalHeaderLabels({tr("Combo"), tr("Actions"), tr("EV (chips)"), tr("Weight"), tr("Blockers")});
        detail_table_->horizontalHeaderItem(2)->setToolTip(hand_ev_header_tooltip());
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
        hand_table_->setHorizontalHeaderLabels({tr("Hand"), tr("Best action"), tr("Action frequencies"), tr("EV (chips)"), tr("Range weight")});
        hand_table_->horizontalHeaderItem(3)->setToolTip(detail_table_->horizontalHeaderItem(2)->toolTip());
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
            node_breadcrumb_->setText(QStringLiteral("-"));
            node_state_->setText({});
            node_action_table_->setRowCount(0);
            return;
        }

        QStringList path;
        path.push_back(tr("Root"));
        for (const auto& action : node->path) {
            path.push_back(display_action_text(action));
        }
        node_breadcrumb_->setText(path.join(QStringLiteral(" / ")));
        const auto board_text = node->board.empty() ? QStringLiteral("-") : blocked_text(node->board);
        node_state_->setText(tr("Type %1 | Actor %2 | board %3 | pot %4 | commitments %5 | stacks %6")
            .arg(q(node->kind))
            .arg(seat_text(solution_, node->acting_seat))
            .arg(board_text)
            .arg(node->table_state.pot, 0, 'f', 2)
            .arg(node->table_state.commitments.size())
            .arg(node->table_state.stacks.size()));

        node_action_table_->setRowCount(static_cast<int>(node->legal_actions.size()));
        for (int row = 0; row < node_action_table_->rowCount(); ++row) {
            const auto& action = node->legal_actions[static_cast<std::size_t>(row)];
            const auto found = std::ranges::find_if(node->average_strategy, [&action](const auto& summary) {
                return summary.action == action;
            });
            node_action_table_->setItem(row, 0, new QTableWidgetItem{display_action_text(action)});
            node_action_table_->setItem(row, 1, new QTableWidgetItem{
                found == node->average_strategy.end() ? QStringLiteral("-") : percent_text(found->frequency)});
            node_action_table_->setItem(row, 2, new QTableWidgetItem{
                found == node->average_strategy.end() || !found->ev_available
                    ? QStringLiteral("\u2014")
                    : ev_text(found->average_ev)});
            node_action_table_->setItem(row, 3, new QTableWidgetItem{});
        }
        refresh_lock_controls();

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
                text += tr("\n%1\nEV %2")
                    .arg(action_text(model.actions))
                    .arg(ev_cell_text(model.reachable, model.ev));
            } else if (!show_combo_strategy) {
                text += QStringLiteral("\nNo node strategy");
            } else {
                text += QStringLiteral("\n-");
            }
            cell->setText(text);
            cell->setEnabled(show_combo_strategy && model.available);
            cell->setObjectName(visible ? "rangeCellPrimary" : "rangeCellMuted");
            cell->setToolTip(show_combo_strategy && model.available
                ? (model.reachable
                    ? tr("%1 combos | weight %2\nEV in chips for this hand class at the root, weighted by range "
                         "weight and the opponent reach each combo can meet. Unreachable combos are excluded.")
                        .arg(model.exact_combos.size()).arg(model.range_weight, 0, 'f', 3)
                    : unreachable_tooltip())
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
            hand_table_->setItem(row, 1, new QTableWidgetItem{display_action_text(hand.best_action)});
            hand_table_->setItem(row, 2, new QTableWidgetItem{action_text(hand.actions)});
            auto* ev_item = new numeric_table_item{ev_cell_text(hand.reachable, hand.ev), hand.ev};
            ev_item->setToolTip(hand_ev_tooltip(hand));
            hand_table_->setItem(row, 3, ev_item);
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
            .arg(display_action_text(found->best_action))
            .arg(ev_cell_text(found->reachable, found->ev))
            .arg(found->range_weight, 0, 'f', 3));
        detail_title_->setToolTip(found->reachable
            ? tr("EV in chips for this hand class at the root, weighted by range weight and the opponent reach "
                 "each combo can meet. Unreachable combos are excluded.")
            : unreachable_tooltip());

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
            auto* combo_ev_item = new QTableWidgetItem{ev_cell_text(combo.reachable, combo.ev)};
            combo_ev_item->setToolTip(hand_ev_tooltip(combo));
            detail_table_->setItem(row, 2, combo_ev_item);
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
            // Dealt cards name chance children; the artifact labels those edges "deal".
            const auto step = lock_steps_.find(child->graph_node_id);
            const auto label = step != lock_steps_.end()
                ? display_action_text(step->second.second)
                : (child->path.empty() ? tr("Root") : q(child->path.back()));
            auto* child_item = new QTreeWidgetItem{
                QStringList{
                    label,
                    seat_text(solution_, child->acting_seat) + node_lock_suffix(child->graph_node_id),
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
        const auto* node = solver::find_solution_node(solution_, active_node_id_.toStdString());
        return node != nullptr && node->kind == "player" && active_node_id_.toStdString() == solution_.root_node_id;
    }

    void strategy_explorer::set_locks_changed_callback(locks_changed_callback callback)
    {
        on_locks_changed_ = std::move(callback);
        refresh_lock_controls();
    }

    void strategy_explorer::set_current_locks(std::vector<cli::node_lock> locks)
    {
        current_locks_ = std::move(locks);
        refresh_lock_controls();
    }

    std::optional<std::vector<std::string>> strategy_explorer::active_lock_path() const
    {
        const auto* node = solver::find_solution_node(solution_, active_node_id_.toStdString());
        if (node == nullptr || node->kind != "player" || node->terminal) {
            return std::nullopt;
        }
        std::vector<std::string> reversed;
        auto current = node->graph_node_id;
        for (auto step = lock_steps_.find(current); step != lock_steps_.end(); step = lock_steps_.find(current)) {
            reversed.push_back(step->second.second);
            current = step->second.first;
        }
        std::ranges::reverse(reversed);
        return reversed;
    }

    void strategy_explorer::apply_node_lock(cli::node_lock lock)
    {
        const auto path = active_lock_path();
        if (!path) {
            return;
        }
        lock.path = *path;
        current_locks_ = viewmodels::with_lock(std::move(current_locks_), std::move(lock));
        refresh_lock_controls();
        if (on_locks_changed_) {
            on_locks_changed_(current_locks_);
        }
    }

    void strategy_explorer::remove_node_locks()
    {
        const auto path = active_lock_path();
        if (!path) {
            return;
        }
        current_locks_ = viewmodels::without_locks_at(std::move(current_locks_), *path);
        refresh_lock_controls();
        if (on_locks_changed_) {
            on_locks_changed_(current_locks_);
        }
    }

    void strategy_explorer::open_lock_dialog()
    {
        const auto path = active_lock_path();
        const auto* node = solver::find_solution_node(solution_, active_node_id_.toStdString());
        if (!path || node == nullptr) {
            return;
        }
        std::vector<double> solved;
        solved.reserve(node->legal_actions.size());
        for (const auto& action : node->legal_actions) {
            const auto found = std::ranges::find(node->average_strategy, action, &solver::solution_action_summary::action);
            solved.push_back(found == node->average_strategy.end() ? 0.0 : found->frequency);
        }
        // Start from this node's whole-range lock, or its only lock, when it has one.
        std::optional<cli::node_lock> existing;
        for (const auto& lock : current_locks_) {
            if (lock.path == *path && (lock.hands.empty() || !existing)) {
                existing = lock;
            }
        }
        node_lock_dialog dialog{*path, seat_text(solution_, node->acting_seat), node->legal_actions, solved, existing, this};
        if (dialog.exec() == QDialog::Accepted && dialog.lock()) {
            apply_node_lock(*dialog.lock());
        }
    }

    void strategy_explorer::refresh_lock_controls()
    {
        if (lock_status_ == nullptr) {
            return;
        }
        const auto path = active_lock_path();
        const auto* node = solver::find_solution_node(solution_, active_node_id_.toStdString());
        const bool editable = static_cast<bool>(on_locks_changed_);
        lock_button_->setEnabled(editable && path.has_value());
        lock_button_->setVisible(editable);
        unlock_button_->setVisible(editable);
        if (!path || node == nullptr) {
            unlock_button_->setEnabled(false);
            lock_status_->setText({});
            return;
        }

        const auto matching = viewmodels::locks_at_node(current_locks_, *path);
        unlock_button_->setEnabled(editable && std::ranges::any_of(current_locks_, [&path](const cli::node_lock& lock) {
            return lock.path == *path;
        }));

        const cli::node_lock* range_lock = nullptr;
        for (const auto index : matching) {
            if (current_locks_[index].hands.empty()) {
                range_lock = &current_locks_[index];
            }
        }
        for (int row = 0; row < node_action_table_->rowCount(); ++row) {
            const auto& action = node->legal_actions[static_cast<std::size_t>(row)];
            QString text;
            if (range_lock != nullptr) {
                const auto found = std::ranges::find(range_lock->actions, action, &cli::node_lock_action::action);
                if (found != range_lock->actions.end()) {
                    text = percent_text(found->frequency);
                }
            }
            if (auto* item = node_action_table_->item(row, 3); item != nullptr) {
                item->setText(text);
            }
        }

        const auto same_lock = [](const cli::node_lock& lhs, const cli::node_lock& rhs) {
            return lhs.path == rhs.path && lhs.hands == rhs.hands
                && std::ranges::equal(lhs.actions, rhs.actions, [](const auto& a, const auto& b) {
                    return a.action == b.action && a.frequency == b.frequency;
                });
        };
        const auto solved = solved_lock_indices_.find(node->graph_node_id);
        const std::size_t solved_count = solved == solved_lock_indices_.end() ? 0u : solved->second.size();
        if (matching.empty()) {
            lock_status_->setText(solved_count > 0
                ? tr("Locked in this solve, but the lock has since been removed. Re-run the solve to unlock it.")
                : tr("No locks. Every action is free for the solver."));
            return;
        }
        QStringList lines;
        bool applied = solved_count == matching.size();
        for (const auto index : matching) {
            const auto& lock = current_locks_[index];
            lines.push_back(tr("%1: %2")
                .arg(q(viewmodels::format_lock_hands(lock)))
                .arg(q(viewmodels::format_lock_actions(lock))));
            applied = applied && std::ranges::any_of(solved_locks_, [&](const cli::node_lock& candidate) {
                return same_lock(candidate, lock);
            });
        }
        lines.push_back(applied
            ? tr("Applied in this solve.")
            : tr("Not solved yet: re-run the solve to apply."));
        lock_status_->setText(lines.join(QStringLiteral("\n")));
    }

    QString strategy_explorer::node_lock_suffix(const uint32_t graph_node_id) const
    {
        return solved_lock_indices_.contains(graph_node_id) ? tr(" · locked") : QString{};
    }

}
