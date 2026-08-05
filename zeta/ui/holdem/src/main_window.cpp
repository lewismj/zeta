#include "main_window.h"

#include <QAbstractItemView>
#include <QAction>
#include <QCloseEvent>
#include <QFileDialog>
#include <QFrame>
#include <QGridLayout>
#include <QHeaderView>
#include <QHBoxLayout>
#include <QLabel>
#include <QMenuBar>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QSizePolicy>
#include <QSplitter>
#include <QStatusBar>
#include <QStringList>
#include <QStyle>
#include <QTabWidget>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QToolBar>
#include <QVBoxLayout>

#include <algorithm>
#include <array>
#include <cctype>
#include <functional>
#include <filesystem>
#include <sstream>
#include <utility>
#include <unordered_map>
#include <vector>

namespace zeta::holdem::ui {

    namespace {

        [[nodiscard]] QString error_text(const document_error& error)
        {
            return QString::fromStdString(error.message);
        }

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
            return label;
        }

        [[nodiscard]] QString money_text(const double value)
        {
            return QString::number(value, 'f', value == static_cast<int>(value) ? 0 : 2);
        }

        [[nodiscard]] QString actor_label(const spot& spot)
        {
            if (spot.root_actor < spot.players.size()) {
                return QString::fromStdString(spot.players[spot.root_actor]);
            }
            return QStringLiteral("Actor %1").arg(static_cast<unsigned>(spot.root_actor));
        }

        [[nodiscard]] QString hero_label(const spot& spot)
        {
            if (spot.hero_seat < spot.players.size()) {
                return QString::fromStdString(spot.players[spot.hero_seat]);
            }
            return QStringLiteral("Hero %1").arg(static_cast<unsigned>(spot.hero_seat));
        }

        [[nodiscard]] std::size_t editable_range_index(const spot& spot)
        {
            if (spot.root_actor < spot.ranges.size()) {
                return spot.root_actor;
            }
            if (spot.hero_seat < spot.ranges.size()) {
                return spot.hero_seat;
            }
            return 0;
        }

        [[nodiscard]] QFrame* make_position_card(const QString& position, const QString& action, const QString& stack, const bool active)
        {
            auto* card = make_panel();
            card->setObjectName(active ? "activePositionCard" : "positionCard");
            card->setMinimumWidth(92);
            auto* layout = new QVBoxLayout{card};
            layout->setContentsMargins(8, 6, 8, 6);
            layout->setSpacing(2);

            auto* name = new QLabel{position};
            name->setObjectName("positionName");
            auto* action_label = new QLabel{action};
            action_label->setObjectName(active ? "activeActionText" : "actionText");
            auto* stack_label = make_muted_label(stack);
            layout->addWidget(name);
            layout->addWidget(action_label);
            layout->addWidget(stack_label);
            return card;
        }

        [[nodiscard]] QPushButton* make_action_button(const QString& action, const QString& percent, const QString& object_name)
        {
            auto* button = new QPushButton{action + QStringLiteral("\n") + percent};
            button->setObjectName(object_name);
            button->setMinimumHeight(92);
            button->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
            return button;
        }

        [[nodiscard]] std::vector<std::pair<QString, double>> aggregate_action_frequencies(const solve_artifact& artifact)
        {
            std::unordered_map<std::string, double> totals;
            for (const auto& hand : artifact.strategy) {
                for (const auto& action : hand.strategy) {
                    totals[action.action] += action.frequency;
                }
            }

            std::vector<std::pair<QString, double>> actions;
            actions.reserve(totals.size());
            const auto divisor = artifact.strategy.empty() ? 1.0 : static_cast<double>(artifact.strategy.size());
            for (const auto& [action, total] : totals) {
                actions.emplace_back(QString::fromStdString(action), total / divisor);
            }
            std::ranges::sort(actions, std::greater{}, &std::pair<QString, double>::second);
            return actions;
        }

        [[nodiscard]] QString hand_class_from_cards(const std::string& hand)
        {
            if (hand.size() < 4) {
                return QString::fromStdString(hand);
            }

            const auto rank_index = [](const char rank) {
                constexpr std::string_view ranks = "AKQJT98765432";
                const auto pos = ranks.find(static_cast<char>(std::toupper(static_cast<unsigned char>(rank))));
                return pos == std::string_view::npos ? ranks.size() : pos;
            };

            const char rank_a = hand[0];
            const char suit_a = hand[1];
            const char rank_b = hand[2];
            const char suit_b = hand[3];
            if (rank_a == rank_b) {
                return QStringLiteral("%1%1").arg(QChar{rank_a});
            }

            const bool first_high = rank_index(rank_a) < rank_index(rank_b);
            const char high = first_high ? rank_a : rank_b;
            const char low = first_high ? rank_b : rank_a;
            const char suited = suit_a == suit_b ? 's' : 'o';
            return QStringLiteral("%1%2%3").arg(QChar{high}).arg(QChar{low}).arg(QChar{suited});
        }

        [[nodiscard]] std::vector<std::string> range_tokens(const std::string& range)
        {
            std::vector<std::string> tokens;
            std::string token;
            std::istringstream input{range};
            while (std::getline(input, token, ',')) {
                const auto first = token.find_first_not_of(" \t\r\n");
                const auto last = token.find_last_not_of(" \t\r\n");
                if (first != std::string::npos && last != std::string::npos) {
                    tokens.push_back(token.substr(first, last - first + 1));
                }
            }
            return tokens;
        }

        [[nodiscard]] bool range_contains_exact_hand(const std::string& range, const QString& hand)
        {
            const auto hand_text = hand.toStdString();
            for (const auto& token : range_tokens(range)) {
                if (token == hand_text) {
                    return true;
                }
            }
            return false;
        }

        void set_range_contains_exact_hand(spot& spot, const QString& hand, const bool enabled)
        {
            if (spot.ranges.empty()) {
                spot.ranges.resize(spot.players.size(), "");
            }
            const auto index = editable_range_index(spot);
            if (index >= spot.ranges.size()) {
                spot.ranges.resize(index + 1u);
            }

            const auto hand_text = hand.toStdString();
            auto tokens = range_tokens(spot.ranges[index]);
            const auto existing = std::ranges::find(tokens, hand_text);
            if (enabled && existing == tokens.end()) {
                tokens.push_back(hand_text);
            } else if (!enabled && existing != tokens.end()) {
                tokens.erase(existing);
            }

            std::ostringstream out;
            for (std::size_t i = 0; i < tokens.size(); ++i) {
                if (i != 0u) {
                    out << ", ";
                }
                out << tokens[i];
            }
            spot.ranges[index] = out.str();
        }

        [[nodiscard]] std::unordered_map<std::string, const cli::hand_strategy*> strategy_by_hand_class(const solve_artifact& artifact)
        {
            std::unordered_map<std::string, const cli::hand_strategy*> rows;
            for (const auto& row : artifact.strategy) {
                rows.emplace(hand_class_from_cards(row.hand).toStdString(), &row);
            }
            return rows;
        }

        [[nodiscard]] QString primary_action_text(const cli::hand_strategy& row)
        {
            if (row.strategy.empty()) {
                return QStringLiteral("EV %1").arg(row.ev, 0, 'f', 2);
            }

            const auto best = std::ranges::max_element(row.strategy, {}, &cli::action_strategy::frequency);
            return QStringLiteral("%1 %2%\nEV %3")
                .arg(QString::fromStdString(best->action))
                .arg(best->frequency * 100.0, 0, 'f', 1)
                .arg(row.ev, 0, 'f', 2);
        }

        [[nodiscard]] QWidget* create_strategy_grid(
            const spot_document& document,
            const std::function<void(const QString&, bool)>& range_toggled)
        {
            constexpr std::array ranks{"A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"};
            auto* container = make_panel();
            auto* layout = new QGridLayout{container};
            layout->setContentsMargins(6, 6, 6, 6);
            layout->setSpacing(2);
            const auto& spot = document.current_spot();
            const auto* artifact = document.artifact() ? &*document.artifact() : nullptr;
            const auto artifact_rows = artifact == nullptr ? std::unordered_map<std::string, const cli::hand_strategy*>{} : strategy_by_hand_class(*artifact);
            const auto range_index = editable_range_index(spot);
            const std::string range = range_index < spot.ranges.size() ? spot.ranges[range_index] : std::string{};

            for (int row = 0; row < static_cast<int>(ranks.size()); ++row) {
                for (int column = 0; column < static_cast<int>(ranks.size()); ++column) {
                    QString hand;
                    if (row == column) {
                        hand = QStringLiteral("%1%1").arg(ranks[row]);
                    } else if (row < column) {
                        hand = QStringLiteral("%1%2s").arg(ranks[row], ranks[column]);
                    } else {
                        hand = QStringLiteral("%1%2o").arg(ranks[column], ranks[row]);
                    }

                    QString text = hand;
                    QString object_name = QStringLiteral("rangeCellMuted");
                    bool checked = false;
                    if (artifact != nullptr) {
                        if (const auto found = artifact_rows.find(hand.toStdString()); found != artifact_rows.end()) {
                            text += QStringLiteral("\n") + primary_action_text(*found->second);
                            object_name = QStringLiteral("rangeCellPrimary");
                        }
                    } else {
                        checked = range_contains_exact_hand(range, hand);
                        text += checked ? QStringLiteral("\nIn range") : QStringLiteral("\nClick to add");
                        object_name = checked ? QStringLiteral("rangeCellSelected") : QStringLiteral("rangeCellMuted");
                    }

                    auto* cell = new QPushButton{text};
                    cell->setCheckable(artifact == nullptr);
                    cell->setChecked(checked);
                    cell->setEnabled(artifact == nullptr);
                    cell->setFlat(true);
                    cell->setObjectName(object_name);
                    cell->setProperty("handClass", hand);
                    cell->setProperty("selected", checked);
                    cell->setSizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
                    if (artifact == nullptr) {
                        QObject::connect(cell, &QPushButton::toggled, cell, [cell, range_toggled](const bool enabled) {
                            cell->setText(cell->property("handClass").toString() + (enabled ? QStringLiteral("\nIn range") : QStringLiteral("\nClick to add")));
                            cell->setObjectName(enabled ? "rangeCellSelected" : "rangeCellMuted");
                            cell->style()->unpolish(cell);
                            cell->style()->polish(cell);
                            range_toggled(cell->property("handClass").toString(), enabled);
                        });
                    }
                    cell->setMinimumSize(48, 38);
                    layout->addWidget(cell, row, column);
                }
            }
            return container;
        }

        [[nodiscard]] QWidget* create_table_overview(const spot_document& document)
        {
            const auto& spot = document.current_spot();
            auto* panel = make_panel();
            auto* layout = new QVBoxLayout{panel};
            layout->setContentsMargins(10, 8, 10, 8);
            layout->setSpacing(8);
            layout->addWidget(make_panel_title(QStringLiteral("Overview")));

            QString table_text;
            for (std::size_t i = 0; i < spot.players.size(); ++i) {
                if (!table_text.isEmpty()) {
                    table_text += QStringLiteral("\n");
                }
                QString markers;
                if (i == spot.root_actor) {
                    markers += QStringLiteral(" to act");
                }
                if (i == spot.hero_seat) {
                    markers += QStringLiteral(" hero");
                }
                const auto stack = i < spot.stacks.size() ? spot.stacks[i] : 0.0;
                const auto contribution = i < spot.contributions.size() ? spot.contributions[i] : 0.0;
                table_text += QStringLiteral("%1%2\n  stack %3 | committed %4")
                    .arg(QString::fromStdString(spot.players[i]))
                    .arg(markers)
                    .arg(money_text(stack))
                    .arg(money_text(contribution));
            }
            auto* table = new QLabel{table_text};
            table->setAlignment(Qt::AlignCenter);
            table->setObjectName("tableFelt");
            table->setMinimumHeight(145);
            layout->addWidget(table);

            QString board;
            for (const auto& card : spot.board) {
                if (!board.isEmpty()) {
                    board += QStringLiteral(" ");
                }
                board += QString::fromStdString(card);
            }
            auto* details = make_muted_label(QStringLiteral("Street: %1\nBoard: %2\nPot: %3\nHero: %4  |  Actor: %5")
                .arg(QString::fromStdString(spot.street))
                .arg(board.isEmpty() ? QStringLiteral("-") : board)
                .arg(money_text(spot.gross_pot))
                .arg(hero_label(spot))
                .arg(actor_label(spot)));
            layout->addWidget(details);
            return panel;
        }

        [[nodiscard]] QWidget* create_actions_panel(const spot_document& document)
        {
            auto* panel = make_panel();
            auto* layout = new QHBoxLayout{panel};
            layout->setContentsMargins(8, 8, 8, 8);
            layout->setSpacing(8);

            if (document.artifact()) {
                const auto actions = aggregate_action_frequencies(*document.artifact());
                for (std::size_t i = 0; i < std::min<std::size_t>(actions.size(), 2u); ++i) {
                    layout->addWidget(make_action_button(
                        actions[i].first,
                        QStringLiteral("%1%").arg(actions[i].second * 100.0, 0, 'f', 1),
                        i == 0u ? QStringLiteral("callButton") : QStringLiteral("foldButton")));
                }
                if (actions.empty()) {
                    layout->addWidget(make_action_button(QStringLiteral("No strategy"), QStringLiteral("0.0%"), QStringLiteral("foldButton")));
                }
                return panel;
            }

            const auto& spot = document.current_spot();
            layout->addWidget(make_action_button(
                QStringLiteral("%1 range").arg(actor_label(spot)),
                QStringLiteral("%1 hands").arg(range_tokens(editable_range_index(spot) < spot.ranges.size() ? spot.ranges[editable_range_index(spot)] : std::string{}).size()),
                QStringLiteral("callButton")));
            layout->addWidget(make_action_button(
                QStringLiteral("Bet size"),
                QStringLiteral("%1% pot").arg(spot.bet_fraction * 100.0, 0, 'f', 1),
                QStringLiteral("foldButton")));
            return panel;
        }

        [[nodiscard]] QWidget* create_hands_panel(const spot_document& document)
        {
            const auto* artifact = document.artifact() ? &*document.artifact() : nullptr;
            const int rows = artifact == nullptr ? 1 : static_cast<int>(artifact->strategy.size());
            auto* table = new QTableWidget{rows, artifact == nullptr ? 2 : 3};
            table->setObjectName("handsTable");
            table->setHorizontalHeaderLabels(artifact == nullptr
                ? QStringList{QStringLiteral("Input"), QStringLiteral("Value")}
                : QStringList{QStringLiteral("Hand"), QStringLiteral("Best action"), QStringLiteral("EV")});
            table->verticalHeader()->setVisible(false);
            table->horizontalHeader()->setStretchLastSection(true);
            table->setEditTriggers(QAbstractItemView::NoEditTriggers);
            table->setSelectionMode(QAbstractItemView::NoSelection);

            if (artifact == nullptr) {
                const auto& spot = document.current_spot();
                const auto range_index = editable_range_index(spot);
                table->setItem(0, 0, new QTableWidgetItem{QStringLiteral("%1 range").arg(actor_label(spot))});
                table->setItem(0, 1, new QTableWidgetItem{range_index < spot.ranges.size() ? QString::fromStdString(spot.ranges[range_index]) : QString{}});
            } else {
                for (int row = 0; row < rows; ++row) {
                    const auto& strategy = artifact->strategy[static_cast<std::size_t>(row)];
                    table->setItem(row, 0, new QTableWidgetItem{QString::fromStdString(strategy.hand)});
                    table->setItem(row, 1, new QTableWidgetItem{strategy.strategy.empty() ? QStringLiteral("-") : primary_action_text(strategy).section('\n', 0, 0)});
                    table->setItem(row, 2, new QTableWidgetItem{QString::number(strategy.ev, 'f', 4)});
                }
            }
            return table;
        }

        void apply_dark_solver_style(QWidget& widget)
        {
            widget.setStyleSheet(QStringLiteral(R"(
                QMainWindow, QMenuBar, QToolBar, QStatusBar, QTabWidget::pane {
                    background: #111315;
                    color: #d7dde3;
                }
                QMenuBar::item:selected, QToolBar {
                    background: #1b2025;
                }
                QTabBar::tab {
                    background: #1a1f24;
                    color: #aeb7c0;
                    padding: 5px 10px;
                    border-right: 1px solid #303840;
                }
                QTabBar::tab:selected {
                    background: #242b31;
                    color: #f3f7fa;
                }
                QFrame#solverPanel, QFrame#positionCard, QFrame#activePositionCard {
                    background: #1a1f24;
                    border: 1px solid #303840;
                    border-radius: 4px;
                }
                QFrame#activePositionCard {
                    border: 1px solid #17b68f;
                }
                QLabel#panelTitle, QLabel#positionName {
                    color: #f2f6fa;
                    font-weight: 600;
                }
                QLabel#mutedLabel, QLabel#actionText {
                    color: #89949d;
                }
                QLabel#activeActionText {
                    color: #47d7b0;
                    font-weight: 600;
                }
                QPushButton#rangeCellPrimary, QPushButton#rangeCellSelected {
                    background: #347fb8;
                    color: #f5fbff;
                    border: 1px solid #4aa0dc;
                    border-radius: 2px;
                    padding: 3px;
                    text-align: left top;
                }
                QPushButton#rangeCellSelected {
                    background: #4fba68;
                    border: 1px solid #69d781;
                }
                QPushButton#rangeCellMuted {
                    background: #20252a;
                    color: #56616a;
                    border: 1px solid #252c32;
                    border-radius: 2px;
                    padding: 3px;
                    text-align: left top;
                }
                QLabel#tableFelt {
                    background: #121619;
                    border: 1px solid #303840;
                    border-radius: 70px;
                    color: #d7dde3;
                }
                QPushButton#callButton {
                    background: #58bd68;
                    color: white;
                    border: 0;
                    border-radius: 3px;
                    font-size: 22px;
                    text-align: left;
                    padding: 10px;
                }
                QPushButton#foldButton {
                    background: #3f86bd;
                    color: white;
                    border: 0;
                    border-radius: 3px;
                    font-size: 22px;
                    text-align: left;
                    padding: 10px;
                }
                QPlainTextEdit, QTableWidget {
                    background: #15191d;
                    color: #d7dde3;
                    border: 1px solid #303840;
                    selection-background-color: #2d6f9f;
                }
                QHeaderView::section {
                    background: #242b31;
                    color: #d7dde3;
                    border: 0;
                    padding: 4px;
                }
            )"));
        }

    }

    main_window::main_window(QWidget* parent)
        : QMainWindow(parent)
    {
        create_actions();
        create_layout();
        new_document();
        update_solver_controls();
    }

    void main_window::closeEvent(QCloseEvent* event)
    {
        for (int i = static_cast<int>(documents_.size()) - 1; i >= 0; --i) {
            if (!maybe_close_document(i)) {
                event->ignore();
                return;
            }
        }
        event->accept();
    }

    void main_window::create_actions()
    {
        new_action_ = new QAction{tr("&New"), this};
        open_action_ = new QAction{tr("&Open..."), this};
        save_action_ = new QAction{tr("&Save"), this};
        save_as_action_ = new QAction{tr("Save &As..."), this};
        validate_action_ = new QAction{tr("&Validate"), this};
        solve_action_ = new QAction{tr("S&olve"), this};
        cancel_action_ = new QAction{tr("&Cancel"), this};

        connect(new_action_, &QAction::triggered, this, [this] { new_document(); });
        connect(open_action_, &QAction::triggered, this, [this] { open_document(); });
        connect(save_action_, &QAction::triggered, this, [this] { save_active_document(); });
        connect(save_as_action_, &QAction::triggered, this, [this] { save_active_document_as(); });
        connect(validate_action_, &QAction::triggered, this, [this] { validate_active_document(); });
        connect(solve_action_, &QAction::triggered, this, [this] { solve_active_document(); });
        connect(cancel_action_, &QAction::triggered, this, [this] { cancel_solver(); });
    }

    void main_window::create_layout()
    {
        apply_dark_solver_style(*this);

        auto* file_menu = menuBar()->addMenu(tr("&File"));
        file_menu->addAction(new_action_);
        file_menu->addAction(open_action_);
        file_menu->addAction(save_action_);
        file_menu->addAction(save_as_action_);

        auto* solve_menu = menuBar()->addMenu(tr("&Solve"));
        solve_menu->addAction(validate_action_);
        solve_menu->addAction(solve_action_);
        solve_menu->addAction(cancel_action_);

        auto* toolbar = addToolBar(tr("Hold'em Solver"));
        toolbar->addAction(new_action_);
        toolbar->addAction(open_action_);
        toolbar->addAction(save_action_);
        toolbar->addSeparator();
        toolbar->addAction(validate_action_);
        toolbar->addAction(solve_action_);
        toolbar->addAction(cancel_action_);

        tabs_ = new QTabWidget{this};
        tabs_->setTabsClosable(true);
        setCentralWidget(tabs_);
        connect(tabs_, &QTabWidget::currentChanged, this, [this] {
            update_window_title();
            update_solver_controls();
        });
        connect(tabs_, &QTabWidget::tabCloseRequested, this, [this](const int index) {
            if (maybe_close_document(index)) {
                documents_.erase(documents_.begin() + index);
                delete tabs_->widget(index);
                update_window_title();
            }
        });

        state_label_ = new QLabel{this};
        status_label_ = new QLabel{this};
        statusBar()->addPermanentWidget(state_label_);
        statusBar()->addWidget(status_label_, 1);
        resize(1100, 720);
    }

    void main_window::new_document()
    {
        add_document_tab(spot_document::create_new());
    }

    void main_window::open_document()
    {
        const auto path = QFileDialog::getOpenFileName(this, tr("Open Hold'em spot"), {}, tr("JSON documents (*.json);;All files (*)"));
        if (path.isEmpty()) {
            return;
        }
        auto document = spot_document::load(std::filesystem::path{path.toStdWString()});
        if (!document) {
            QMessageBox::critical(this, tr("Open failed"), error_text(document.error()));
            return;
        }
        add_document_tab(std::move(*document));
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
            QMessageBox::critical(this, tr("Save failed"), error_text(result.error()));
            return false;
        }
        entry->document.clear_dirty();
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
        const auto path = QFileDialog::getSaveFileName(this, tr("Save Hold'em spot"), {}, tr("JSON documents (*.json);;All files (*)"));
        if (path.isEmpty()) {
            return false;
        }
        if (!parse_editor_into_document(*entry, true)) {
            return false;
        }
        auto result = entry->document.save_as(std::filesystem::path{path.toStdWString()});
        if (!result) {
            QMessageBox::critical(this, tr("Save failed"), error_text(result.error()));
            return false;
        }
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
            QMessageBox::warning(this, tr("Invalid solver state"), QString::fromStdString(transition.error()));
            return;
        }
        update_solver_controls();
        const bool ok = parse_editor_into_document(*entry, true);
        (void) solver_state_.transition_to(ok ? solver_state::idle : solver_state::failed);
        status_label_->setText(ok ? tr("Spot is valid.") : tr("Spot validation failed."));
        update_solver_controls();
    }

    void main_window::solve_active_document()
    {
        auto* entry = active_entry();
        if (entry == nullptr || !parse_editor_into_document(*entry, true)) {
            return;
        }
        if (auto transition = solver_state_.transition_to(solver_state::starting); !transition) {
            QMessageBox::warning(this, tr("Invalid solver state"), QString::fromStdString(transition.error()));
            return;
        }
        (void) solver_state_.transition_to(solver_state::completed);
        status_label_->setText(tr("Spot is ready for solver execution."));
        update_solver_controls();
    }

    void main_window::cancel_solver()
    {
        if (solver_state_.state() == solver_state::running || solver_state_.state() == solver_state::starting) {
            (void) solver_state_.transition_to(solver_state::cancelling);
            (void) solver_state_.transition_to(solver_state::idle);
        }
        update_solver_controls();
    }

    bool main_window::maybe_close_document(const int index)
    {
        if (index < 0 || index >= static_cast<int>(documents_.size())) {
            return true;
        }
        tabs_->setCurrentIndex(index);
        auto& entry = documents_[index];
        if (!entry.document.is_dirty()) {
            return true;
        }
        const auto choice = QMessageBox::warning(
            this,
            tr("Unsaved changes"),
            tr("Save changes to %1?").arg(display_name(entry)),
            QMessageBox::Save | QMessageBox::Discard | QMessageBox::Cancel,
            QMessageBox::Save);
        if (choice == QMessageBox::Cancel) {
            return false;
        }
        if (choice == QMessageBox::Discard) {
            return true;
        }
        return save_active_document();
    }

    bool main_window::parse_editor_into_document(document_entry& entry, const bool show_error)
    {
        auto parsed = cli::parse_spot_json(entry.editor->toPlainText().toStdString());
        if (!parsed) {
            if (show_error) {
                QMessageBox::critical(this, tr("Invalid spot"), QString::fromStdString(parsed.error().message));
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
            .updating_editor = false
        });
        const int index = static_cast<int>(documents_.size()) - 1;
        auto& entry = documents_.back();

        auto* root = new QWidget{this};
        auto* root_layout = new QVBoxLayout{root};
        root_layout->setContentsMargins(6, 6, 6, 6);
        root_layout->setSpacing(6);

        const auto& spot = entry.document.current_spot();
        auto* position_strip = new QHBoxLayout;
        position_strip->setSpacing(6);
        for (std::size_t player_index = 0; player_index < spot.players.size(); ++player_index) {
            const auto stack = player_index < spot.stacks.size() ? spot.stacks[player_index] : 0.0;
            const auto contribution = player_index < spot.contributions.size() ? spot.contributions[player_index] : 0.0;
            position_strip->addWidget(make_position_card(
                QString::fromStdString(spot.players[player_index]),
                player_index == spot.root_actor ? tr("To act") : tr("Committed %1").arg(money_text(contribution)),
                tr("Stack %1").arg(money_text(stack)),
                player_index == spot.root_actor));
        }
        position_strip->addStretch(1);
        root_layout->addLayout(position_strip);

        auto* workspace = new QSplitter{Qt::Horizontal, root};
        auto* left_tabs = new QTabWidget{workspace};
        left_tabs->setObjectName("solverSubTabs");

        auto* raw_editor = new QPlainTextEdit{left_tabs};
        raw_editor->setPlainText(QString::fromStdString(cli::serialize_spot_json(entry.document.current_spot())));
        raw_editor->setLineWrapMode(QPlainTextEdit::NoWrap);
        entry.editor = raw_editor;

        auto refresh_raw_editor = [this, raw_editor, index] {
            if (index < 0 || index >= static_cast<int>(documents_.size())) {
                return;
            }
            auto& entry = documents_[index];
            entry.updating_editor = true;
            raw_editor->setPlainText(QString::fromStdString(cli::serialize_spot_json(entry.document.current_spot())));
            entry.updating_editor = false;
            update_tab_title(index);
            update_window_title();
        };

        left_tabs->addTab(create_strategy_grid(entry.document, [this, index, refresh_raw_editor](const QString& hand, const bool enabled) {
            if (index < 0 || index >= static_cast<int>(documents_.size())) {
                return;
            }
            auto& entry = documents_[index];
            auto updated_spot = entry.document.current_spot();
            set_range_contains_exact_hand(updated_spot, hand, enabled);
            entry.document.replace_spot(std::move(updated_spot));
            refresh_raw_editor();
        }), entry.document.artifact() ? tr("Strategy + EV") : tr("Range input"));
        left_tabs->addTab(raw_editor, tr("Spot JSON"));

        auto* right_column = new QWidget{workspace};
        auto* right_layout = new QVBoxLayout{right_column};
        right_layout->setContentsMargins(0, 0, 0, 0);
        right_layout->setSpacing(6);
        right_layout->addWidget(create_table_overview(entry.document));

        right_layout->addWidget(create_actions_panel(entry.document));
        right_layout->addWidget(create_hands_panel(entry.document), 1);

        workspace->addWidget(left_tabs);
        workspace->addWidget(right_column);
        workspace->setStretchFactor(0, 3);
        workspace->setStretchFactor(1, 2);
        root_layout->addWidget(workspace, 1);

        auto* log = new QPlainTextEdit{root};
        log->setReadOnly(true);
        log->setMaximumHeight(82);
        log->setPlainText(tr("Ready.\nValidate the spot, then solve to stream progress here."));
        root_layout->addWidget(log);

        connect(raw_editor, &QPlainTextEdit::textChanged, this, [this, raw_editor] {
            for (int i = 0; i < static_cast<int>(documents_.size()); ++i) {
                auto& entry = documents_[i];
                if (entry.editor == raw_editor && !entry.updating_editor) {
                    entry.document.mark_dirty();
                    update_tab_title(i);
                    update_window_title();
                    break;
                }
            }
        });

        tabs_->addTab(root, display_name(documents_.back()));
        tabs_->setCurrentIndex(index);
        update_tab_title(index);
        update_window_title();
    }

    void main_window::update_tab_title(const int index)
    {
        if (index < 0 || index >= static_cast<int>(documents_.size())) {
            return;
        }
        QString title = display_name(documents_[index]);
        if (documents_[index].document.is_dirty()) {
            title += "*";
        }
        tabs_->setTabText(index, title);
    }

    void main_window::update_window_title()
    {
        auto* entry = active_entry();
        if (entry == nullptr) {
            setWindowTitle(tr("Zeta Hold'em Solver"));
            return;
        }
        QString title = display_name(*entry);
        if (entry->document.is_dirty()) {
            title += "*";
        }
        setWindowTitle(tr("%1 - Zeta Hold'em Solver").arg(title));
    }

    void main_window::update_solver_controls()
    {
        const auto controls = solver_state_.controls();
        validate_action_->setEnabled(controls.validate_enabled && active_entry() != nullptr);
        solve_action_->setEnabled(controls.solve_enabled && active_entry() != nullptr);
        cancel_action_->setEnabled(controls.cancel_enabled);
        state_label_->setText(tr("State: %1").arg(QString::fromLatin1(to_string(solver_state_.state()))));
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
