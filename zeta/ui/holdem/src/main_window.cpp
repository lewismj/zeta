#include "main_window.h"

#include <QAbstractItemView>
#include <QAction>
#include <QActionGroup>
#include <QCloseEvent>
#include <QFileDialog>
#include <QFileInfo>
#include <QFrame>
#include <QGridLayout>
#include <QHeaderView>
#include <QHBoxLayout>
#include <QLabel>
#include <QListWidget>
#include <QMenuBar>
#include <QMessageBox>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QSignalBlocker>
#include <QSizePolicy>
#include <QSplitter>
#include <QSpinBox>
#include <QStatusBar>
#include <QStringList>
#include <QStyle>
#include <QTabWidget>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QTextCursor>
#include <QTimer>
#include <QToolBar>
#include <QToolButton>
#include <QVBoxLayout>

#include "theme/theme_registry.h"
#include "theme/theme_styles.h"
#include "viewmodels/spot_view_model.h"
#include "widgets/spot_builder.h"
#include "widgets/table_state_view.h"

#include <algorithm>
#include <array>
#include <cctype>
#include <chrono>
#include <cstdlib>
#include <future>
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
            const std::function<void(const QString&, bool)>& range_toggled,
            const theme::density_metrics& metrics)
        {
            constexpr std::array ranks{"A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"};
            auto* container = make_panel();
            auto* layout = new QGridLayout{container};
            layout->setContentsMargins(metrics.panel_spacing, metrics.panel_spacing, metrics.panel_spacing, metrics.panel_spacing);
            layout->setSpacing(metrics.panel_spacing / 2);
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
                    cell->setMinimumSize(metrics.range_cell_min_width, metrics.range_cell_min_height);
                    layout->addWidget(cell, row, column);
                }
            }
            return container;
        }

        [[nodiscard]] QWidget* create_actions_panel(const spot_document& document, const theme::density_metrics& metrics)
        {
            auto* panel = make_panel();
            auto* layout = new QHBoxLayout{panel};
            layout->setContentsMargins(metrics.panel_margin, metrics.panel_margin, metrics.panel_margin, metrics.panel_margin);
            layout->setSpacing(metrics.panel_spacing);

            if (document.artifact()) {
                const auto actions = aggregate_action_frequencies(*document.artifact());
                for (std::size_t i = 0; i < std::min<std::size_t>(actions.size(), 2u); ++i) {
                    layout->addWidget(make_action_button(
                        actions[i].first,
                        QStringLiteral("%1%").arg(actions[i].second * 100.0, 0, 'f', 1),
                        i == 0u ? QStringLiteral("callButton") : QStringLiteral("foldButton"),
                        metrics));
                }
                if (actions.empty()) {
                    layout->addWidget(make_action_button(QStringLiteral("No strategy"), QStringLiteral("0.0%"), QStringLiteral("foldButton"), metrics));
                }
                return panel;
            }

            const auto& spot = document.current_spot();
            layout->addWidget(make_action_button(
                QStringLiteral("%1 range").arg(actor_label(spot)),
                QStringLiteral("%1 hands").arg(range_tokens(editable_range_index(spot) < spot.ranges.size() ? spot.ranges[editable_range_index(spot)] : std::string{}).size()),
                QStringLiteral("callButton"),
                metrics));
            layout->addWidget(make_action_button(
                QStringLiteral("Bet size"),
                QStringLiteral("%1% pot").arg(spot.bet_fraction * 100.0, 0, 'f', 1),
                QStringLiteral("foldButton"),
                metrics));
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

    }

    main_window::main_window(QWidget* parent)
        : QMainWindow(parent)
    {
        active_theme_ = settings_.active_theme();
        density_mode_ = settings_.density();
        workspace_splitter_sizes_ = settings_.workspace_splitter_sizes();
        create_actions();
        create_layout();
        new_document();
        update_solver_controls();
    }

    void main_window::closeEvent(QCloseEvent* event)
    {
        finish_solver_if_ready();
        if (has_active_solve()) {
            QMessageBox::information(
                this,
                tr("Solve in progress"),
                tr("A solve is still running for %1. Close the window after the solve finishes.")
                    .arg(active_solver_document_index_ >= 0 && active_solver_document_index_ < static_cast<int>(documents_.size())
                        ? display_name(documents_[active_solver_document_index_])
                        : tr("the active document")));
            event->ignore();
            return;
        }

        for (int i = static_cast<int>(documents_.size()) - 1; i >= 0; --i) {
            if (!maybe_close_document(i)) {
                event->ignore();
                return;
            }
        }
        save_window_settings();
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

        new_action_->setIcon(style()->standardIcon(QStyle::SP_FileIcon));
        open_action_->setIcon(style()->standardIcon(QStyle::SP_DialogOpenButton));
        save_action_->setIcon(style()->standardIcon(QStyle::SP_DialogSaveButton));
        validate_action_->setIcon(style()->standardIcon(QStyle::SP_DialogApplyButton));
        solve_action_->setIcon(style()->standardIcon(QStyle::SP_MediaPlay));
        cancel_action_->setIcon(style()->standardIcon(QStyle::SP_DialogCancelButton));

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
        apply_active_theme();

        auto* file_menu = menuBar()->addMenu(tr("&File"));
        file_menu->addAction(new_action_);
        file_menu->addAction(open_action_);
        recent_files_menu_ = file_menu->addMenu(tr("Open &Recent"));
        file_menu->addAction(save_action_);
        file_menu->addAction(save_as_action_);

        auto* solve_menu = menuBar()->addMenu(tr("&Solve"));
        solve_menu->addAction(validate_action_);
        solve_menu->addAction(solve_action_);
        solve_menu->addAction(cancel_action_);

        auto* view_menu = menuBar()->addMenu(tr("&View"));
        auto* theme_menu = view_menu->addMenu(tr("&Theme"));
        theme_actions_ = new QActionGroup{this};
        for (const auto& theme : theme::registered_themes()) {
            auto* action = theme_menu->addAction(QString::fromStdString(theme.display_name));
            action->setCheckable(true);
            action->setChecked(theme.id == active_theme_);
            action->setData(static_cast<int>(theme.id));
            theme_actions_->addAction(action);
            connect(action, &QAction::triggered, this, [this, id = theme.id] {
                set_active_theme(id);
            });
        }

        auto* density_menu = view_menu->addMenu(tr("&Density"));
        density_actions_ = new QActionGroup{this};
        for (const auto density : {theme::density_mode::compact, theme::density_mode::comfortable}) {
            auto* action = density_menu->addAction(QString::fromStdString(std::string{theme::density_mode_label(density)}));
            action->setCheckable(true);
            action->setChecked(density == density_mode_);
            action->setData(static_cast<int>(density));
            density_actions_->addAction(action);
            connect(action, &QAction::triggered, this, [this, density] {
                set_density_mode(density);
            });
        }

        auto* toolbar = addToolBar(tr("Hold'em Solver"));
        toolbar->setObjectName("commandBar");
        toolbar->setMovable(false);
        toolbar->addAction(new_action_);
        toolbar->addAction(open_action_);
        toolbar->addAction(save_action_);
        toolbar->addSeparator();
        toolbar->addAction(validate_action_);
        toolbar->addAction(solve_action_);
        toolbar->addAction(cancel_action_);
        toolbar->addSeparator();
        auto* iterations_label = new QLabel{tr("Iterations"), toolbar};
        toolbar->addWidget(iterations_label);
        iterations_spin_ = new QSpinBox{toolbar};
        iterations_spin_->setRange(1, 1'000'000);
        iterations_spin_->setValue(100);
        iterations_spin_->setSingleStep(50);
        iterations_spin_->setMaximumWidth(96);
        iterations_spin_->setToolTip(tr("CFR iterations for the next solve."));
        toolbar->addWidget(iterations_spin_);
        auto* output_label = new QLabel{tr(" Output: document artifact"), toolbar};
        output_label->setObjectName("mutedLabel");
        toolbar->addWidget(output_label);
        toolbar->addSeparator();
        auto* theme_button = new QToolButton{toolbar};
        theme_button->setText(tr("Theme"));
        theme_button->setToolTip(tr("Change the active application theme."));
        theme_button->setPopupMode(QToolButton::InstantPopup);
        theme_button->setMenu(theme_menu);
        toolbar->addWidget(theme_button);

        shell_splitter_ = new QSplitter{Qt::Horizontal, this};
        shell_splitter_->setObjectName("appShellSplitter");

        auto* rail = new QWidget{shell_splitter_};
        rail->setObjectName("documentRail");
        auto* rail_layout = new QVBoxLayout{rail};
        rail_layout->setContentsMargins(8, 8, 8, 8);
        rail_layout->setSpacing(6);
        auto* rail_title = new QLabel{tr("Documents"), rail};
        rail_title->setObjectName("railTitle");
        rail_layout->addWidget(rail_title);
        document_rail_ = new QListWidget{rail};
        document_rail_->setObjectName("documentRailList");
        document_rail_->setSelectionMode(QAbstractItemView::SingleSelection);
        rail_layout->addWidget(document_rail_, 1);

        tabs_ = new QTabWidget{shell_splitter_};
        tabs_->setTabsClosable(true);
        shell_splitter_->addWidget(rail);
        shell_splitter_->addWidget(tabs_);
        shell_splitter_->setStretchFactor(0, 0);
        shell_splitter_->setStretchFactor(1, 1);
        setCentralWidget(shell_splitter_);

        connect(document_rail_, &QListWidget::currentRowChanged, this, [this](const int row) {
            if (row >= 0 && row < tabs_->count() && tabs_->currentIndex() != row) {
                tabs_->setCurrentIndex(row);
            }
        });
        connect(tabs_, &QTabWidget::currentChanged, this, [this] {
            if (document_rail_ != nullptr) {
                QSignalBlocker blocker{document_rail_};
                document_rail_->setCurrentRow(tabs_->currentIndex());
            }
            update_window_title();
            update_solver_controls();
        });
        connect(tabs_, &QTabWidget::tabCloseRequested, this, [this](const int index) {
            if (maybe_close_document(index)) {
                documents_.erase(documents_.begin() + index);
                if (active_solver_document_index_ > index) {
                    --active_solver_document_index_;
                }
                auto* widget = tabs_->widget(index);
                tabs_->removeTab(index);
                delete widget;
                update_document_rail();
                update_window_title();
            }
        });

        solver_poll_timer_ = new QTimer{this};
        solver_poll_timer_->setInterval(100);
        connect(solver_poll_timer_, &QTimer::timeout, this, [this] {
            finish_solver_if_ready();
        });

        state_label_ = new QLabel{this};
        status_label_ = new QLabel{this};
        statusBar()->addPermanentWidget(state_label_);
        statusBar()->addWidget(status_label_, 1);
        resize(1180, 760);
        update_recent_files_menu();
        restore_window_settings();
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
        open_document_path(std::filesystem::path{path.toStdWString()});
    }

    void main_window::open_document_path(const std::filesystem::path& path)
    {
        auto document = spot_document::load(path);
        if (!document) {
            QMessageBox::critical(this, tr("Open failed"), error_text(document.error()));
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
            QMessageBox::critical(this, tr("Save failed"), error_text(result.error()));
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
            QMessageBox::warning(this, tr("Invalid solver state"), QString::fromStdString(transition.error()));
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

    void main_window::solve_active_document()
    {
        if (has_active_solve()) {
            QMessageBox::warning(this, tr("Solve in progress"), tr("Wait for the active solve to finish before starting another one."));
            return;
        }
        auto* entry = active_entry();
        if (entry == nullptr || !parse_editor_into_document(*entry, true)) {
            return;
        }
        const int document_index = tabs_->currentIndex();
        if (auto transition = solver_state_.transition_to(solver_state::starting); !transition) {
            QMessageBox::warning(this, tr("Invalid solver state"), QString::fromStdString(transition.error()));
            return;
        }

        solver::solver_session_request request{
            .spot_snapshot = entry->document.current_spot(),
            .iterations = static_cast<uint64_t>(iterations_spin_->value())
        };
        if (const char* revision = std::getenv("ZETA_GIT_REVISION")) {
            request.runtime.git_revision = revision;
        }

        active_session_ = std::make_shared<solver::solver_session>(std::move(request));
        active_solver_document_index_ = document_index;
        const auto& session_request = active_session_->request();
        set_solve_console(*entry, tr("Started %1\nIterations: %2\nOutput: store artifact in active document\nPlayers: %3\nStatus: running")
            .arg(QString::fromStdString(cli::detail::now_utc_iso8601()))
            .arg(static_cast<qulonglong>(session_request.iterations))
            .arg(static_cast<qulonglong>(session_request.spot_snapshot.players.size())));
        entry->editor->setReadOnly(true);
        status_label_->setText(tr("Solving %1 with %2 iterations.")
            .arg(display_name(*entry))
            .arg(static_cast<qulonglong>(session_request.iterations)));

        active_solver_ = std::async(std::launch::async, [session = active_session_] {
            return session->run();
        });
        (void) solver_state_.transition_to(solver_state::running);
        solver_poll_timer_->start();
        update_solver_controls();
    }

    void main_window::cancel_solver()
    {
        if (!has_active_solve()) {
            return;
        }
        if (active_session_) {
            active_session_->cancel_before_start();
        }
        if (solver_state_.state() == solver_state::running || solver_state_.state() == solver_state::starting) {
            (void) solver_state_.transition_to(solver_state::cancelling);
            if (active_solver_document_index_ >= 0 && active_solver_document_index_ < static_cast<int>(documents_.size())) {
                append_solve_console(documents_[active_solver_document_index_], tr("Cancellation requested. The run stops only if solver work has not started."));
            }
            status_label_->setText(tr("Cancellation requested."));
        }
        update_solver_controls();
    }

    void main_window::finish_solver_if_ready()
    {
        if (!active_solver_.valid()) {
            return;
        }
        if (active_solver_.wait_for(std::chrono::milliseconds{0}) != std::future_status::ready) {
            return;
        }
        auto result = active_solver_.get();
        finish_solver_session(std::move(result));
    }

    void main_window::finish_solver_session(solver::solver_session_result result)
    {
        solver_poll_timer_->stop();
        const int document_index = active_solver_document_index_;
        active_solver_document_index_ = -1;
        active_session_.reset();

        if (document_index < 0 || document_index >= static_cast<int>(documents_.size())) {
            (void) solver_state_.transition_to(result.terminal_state == solver::solver_session_terminal_state::failed
                ? solver_state::failed
                : solver_state::completed);
            update_solver_controls();
            return;
        }

        auto& entry = documents_[document_index];
        if (entry.editor != nullptr) {
            entry.editor->setReadOnly(false);
        }

        QString summary;
        switch (result.terminal_state) {
            case solver::solver_session_terminal_state::completed:
                if (result.artifact) {
                    entry.document.replace_artifact(std::move(result.artifact));
                }
                summary = tr("completed");
                append_solve_console(entry, tr("Graph build: %1ms\nCFR: %2ms\nExtraction: %3ms\nFinished %4\nStatus: completed")
                    .arg(result.timing.graph_build_ms, 0, 'f', 3)
                    .arg(result.timing.cfr_iterations_ms, 0, 'f', 3)
                    .arg(result.timing.extraction_ms, 0, 'f', 3)
                    .arg(QString::fromStdString(result.metadata.finished_utc)));
                (void) solver_state_.transition_to(solver_state::completed);
                status_label_->setText(tr("Solve completed."));
                break;
            case solver::solver_session_terminal_state::failed:
                summary = tr("failed: %1").arg(QString::fromStdString(result.error_message));
                append_solve_console(entry, tr("Finished %1\nStatus: failed\nError: %2")
                    .arg(QString::fromStdString(result.metadata.finished_utc))
                    .arg(QString::fromStdString(result.error_message)));
                (void) solver_state_.transition_to(solver_state::failed);
                status_label_->setText(tr("Solve failed."));
                break;
            case solver::solver_session_terminal_state::cancelled_before_start:
                summary = tr("cancelled-before-start");
                append_solve_console(entry, tr("Finished %1\nStatus: cancelled before start")
                    .arg(QString::fromStdString(result.metadata.finished_utc)));
                if (solver_state_.state() == solver_state::running || solver_state_.state() == solver_state::starting) {
                    (void) solver_state_.transition_to(solver_state::cancelling);
                }
                (void) solver_state_.transition_to(solver_state::idle);
                status_label_->setText(tr("Solve cancelled before start."));
                break;
        }

        auto metadata = entry.document.metadata();
        metadata.last_solve_summary = summary.toStdString();
        entry.document.update_metadata(std::move(metadata));
        entry.document.add_history(solve_history_entry{
            .timestamp_utc = result.metadata.finished_utc,
            .iterations = result.iterations,
            .outcome = summary.toStdString()
        });
        refresh_document_tab(document_index);
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

    bool main_window::maybe_close_active_solve(const int index)
    {
        if (!has_active_solve() || index != active_solver_document_index_) {
            return true;
        }
        QMessageBox::information(
            this,
            tr("Solve in progress"),
            tr("A solve is still running for %1. Close this document after the solve finishes.")
                .arg(display_name(documents_[index])));
        return false;
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
            .solve_console = nullptr,
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
        auto* root = new QWidget{this};
        root->setObjectName("documentRoot");
        auto* root_layout = new QVBoxLayout{root};
        root_layout->setContentsMargins(metrics.shell_margin, metrics.shell_margin, metrics.shell_margin, metrics.shell_margin);
        root_layout->setSpacing(metrics.panel_spacing);

        auto* summary_header = make_panel_title(QString::fromStdString(viewmodels::spot_summary_text(entry.document.current_spot(), entry.document.artifact().has_value())));
        summary_header->setObjectName("spotSummaryHeader");
        root_layout->addWidget(summary_header);

        auto* workspace = new QSplitter{Qt::Horizontal, root};
        entry.workspace_splitter = workspace;
        auto* left_tabs = new QTabWidget{workspace};
        left_tabs->setObjectName("solverSubTabs");

        auto* raw_editor = new QPlainTextEdit{left_tabs};
        raw_editor->setPlainText(QString::fromStdString(cli::serialize_spot_json(entry.document.current_spot())));
        raw_editor->setLineWrapMode(QPlainTextEdit::NoWrap);
        raw_editor->setReadOnly(index == active_solver_document_index_ && has_active_solve());
        entry.editor = raw_editor;

        auto* right_column = new QWidget{workspace};
        right_column->setObjectName("inspectorPanel");
        auto* right_layout = new QVBoxLayout{right_column};
        right_layout->setContentsMargins(metrics.shell_margin, metrics.shell_margin, metrics.shell_margin, metrics.shell_margin);
        right_layout->setSpacing(metrics.panel_spacing);
        auto* table_view = new widgets::table_state_view{entry.document.current_spot(), metrics, right_column};
        right_layout->addWidget(table_view);

        auto refresh_raw_editor = [this, raw_editor, summary_header, table_view, index] {
            if (index < 0 || index >= static_cast<int>(documents_.size())) {
                return;
            }
            auto& entry = documents_[index];
            entry.updating_editor = true;
            raw_editor->setPlainText(QString::fromStdString(cli::serialize_spot_json(entry.document.current_spot())));
            entry.updating_editor = false;
            summary_header->setText(QString::fromStdString(viewmodels::spot_summary_text(entry.document.current_spot(), entry.document.artifact().has_value())));
            table_view->set_spot(entry.document.current_spot());
            update_tab_title(index);
            update_window_title();
        };

        auto* builder = new widgets::spot_builder{
            entry.document.current_spot(),
            metrics,
            [this, index, refresh_raw_editor](spot next_spot) {
                if (index < 0 || index >= static_cast<int>(documents_.size())) {
                    return;
                }
                documents_[index].document.replace_spot(std::move(next_spot));
                refresh_raw_editor();
            },
            [this](const spot& source) {
                auto document = spot_document::create_new();
                document.replace_spot(source);
                add_document_tab(std::move(document));
            },
            left_tabs};
        left_tabs->addTab(builder, tr("Spot Builder"));
        left_tabs->addTab(create_strategy_grid(entry.document, [this, index, refresh_raw_editor](const QString& hand, const bool enabled) {
            if (index < 0 || index >= static_cast<int>(documents_.size())) {
                return;
            }
            auto& entry = documents_[index];
            auto updated_spot = entry.document.current_spot();
            set_range_contains_exact_hand(updated_spot, hand, enabled);
            entry.document.replace_spot(std::move(updated_spot));
            refresh_raw_editor();
        }, metrics), entry.document.artifact() ? tr("Strategy + EV") : tr("Range input"));
        left_tabs->addTab(raw_editor, tr("Spot JSON"));

        right_layout->addWidget(create_actions_panel(entry.document, metrics));
        right_layout->addWidget(create_hands_panel(entry.document), 1);

        workspace->addWidget(left_tabs);
        workspace->addWidget(right_column);
        workspace->setStretchFactor(0, 3);
        workspace->setStretchFactor(1, 2);
        if (workspace_splitter_sizes_.size() == 2) {
            workspace->setSizes(workspace_splitter_sizes_);
        }
        root_layout->addWidget(workspace, 1);

        auto* log = new QPlainTextEdit{root};
        log->setObjectName("solveConsole");
        log->setReadOnly(true);
        log->setMaximumHeight(metrics.console_height);
        log->setPlainText(QString::fromStdString(entry.solve_console_text));
        entry.solve_console = log;
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

        return root;
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

    void main_window::append_solve_console(document_entry& entry, const QString& text)
    {
        QString console = QString::fromStdString(entry.solve_console_text);
        if (!console.isEmpty()) {
            console += QStringLiteral("\n");
        }
        console += text;
        set_solve_console(entry, console);
    }

    void main_window::set_solve_console(document_entry& entry, const QString& text)
    {
        entry.solve_console_text = text.toStdString();
        if (entry.solve_console != nullptr) {
            entry.solve_console->setPlainText(text);
            entry.solve_console->moveCursor(QTextCursor::End);
        }
    }

    void main_window::apply_active_theme()
    {
        setStyleSheet(theme::style_sheet(theme::find_theme(active_theme_), density_mode_));
    }

    void main_window::set_active_theme(const theme::theme_id theme)
    {
        if (active_theme_ == theme) {
            return;
        }
        active_theme_ = theme;
        settings_.set_active_theme(active_theme_);
        settings_.sync();
        apply_active_theme();
        if (theme_actions_ != nullptr) {
            for (auto* action : theme_actions_->actions()) {
                action->setChecked(action->data().toInt() == static_cast<int>(active_theme_));
            }
        }
    }

    void main_window::set_density_mode(const theme::density_mode density)
    {
        if (density_mode_ == density) {
            return;
        }
        if (auto* entry = active_entry(); entry != nullptr && entry->workspace_splitter != nullptr) {
            workspace_splitter_sizes_ = entry->workspace_splitter->sizes();
        }
        density_mode_ = density;
        settings_.set_density(density_mode_);
        settings_.sync();
        apply_active_theme();
        if (density_actions_ != nullptr) {
            for (auto* action : density_actions_->actions()) {
                action->setChecked(action->data().toInt() == static_cast<int>(density_mode_));
            }
        }
        refresh_all_document_tabs();
    }

    void main_window::refresh_all_document_tabs()
    {
        const int current = tabs_ == nullptr ? -1 : tabs_->currentIndex();
        for (int index = 0; index < static_cast<int>(documents_.size()); ++index) {
            auto* old_widget = tabs_->widget(index);
            auto* next_widget = create_document_widget(index);
            tabs_->removeTab(index);
            tabs_->insertTab(index, next_widget, display_name(documents_[index]));
            delete old_widget;
            update_tab_title(index);
        }
        if (current >= 0 && current < tabs_->count()) {
            tabs_->setCurrentIndex(current);
        }
        update_document_rail();
    }

    void main_window::update_document_rail()
    {
        if (document_rail_ == nullptr || tabs_ == nullptr) {
            return;
        }
        QSignalBlocker blocker{document_rail_};
        document_rail_->clear();
        for (int index = 0; index < static_cast<int>(documents_.size()); ++index) {
            auto title = display_name(documents_[index]);
            if (documents_[index].document.is_dirty()) {
                title += QStringLiteral("*");
            }
            auto* item = new QListWidgetItem{title};
            item->setToolTip(documents_[index].document.file_path().empty()
                ? tr("Unsaved Hold'em spot")
                : QString::fromStdWString(documents_[index].document.file_path().wstring()));
            document_rail_->addItem(item);
        }
        if (tabs_->currentIndex() >= 0 && tabs_->currentIndex() < document_rail_->count()) {
            document_rail_->setCurrentRow(tabs_->currentIndex());
        }
    }

    void main_window::update_recent_files_menu()
    {
        if (recent_files_menu_ == nullptr) {
            return;
        }
        recent_files_menu_->clear();
        const auto files = settings_.recent_files();
        if (files.isEmpty()) {
            auto* empty_action = recent_files_menu_->addAction(tr("No recent files"));
            empty_action->setEnabled(false);
            return;
        }
        for (const auto& file : files) {
            auto* action = recent_files_menu_->addAction(QFileInfo{file}.fileName());
            action->setToolTip(file);
            connect(action, &QAction::triggered, this, [this, file] {
                open_document_path(std::filesystem::path{file.toStdWString()});
            });
        }
    }

    void main_window::add_recent_file(const std::filesystem::path& path)
    {
        if (path.empty()) {
            return;
        }
        settings_.add_recent_file(QString::fromStdWString(path.wstring()));
        settings_.sync();
        update_recent_files_menu();
    }

    void main_window::restore_window_settings()
    {
        if (const auto geometry = settings_.window_geometry(); !geometry.isEmpty()) {
            restoreGeometry(geometry);
        }
        if (shell_splitter_ != nullptr) {
            const auto sizes = settings_.shell_splitter_sizes();
            if (sizes.size() == 2) {
                shell_splitter_->setSizes(sizes);
            } else {
                shell_splitter_->setSizes(QList<int>{190, 990});
            }
        }
    }

    void main_window::save_window_settings()
    {
        settings_.set_window_geometry(saveGeometry());
        if (shell_splitter_ != nullptr) {
            settings_.set_shell_splitter_sizes(shell_splitter_->sizes());
        }
        if (auto* entry = active_entry(); entry != nullptr && entry->workspace_splitter != nullptr) {
            workspace_splitter_sizes_ = entry->workspace_splitter->sizes();
        }
        if (workspace_splitter_sizes_.size() == 2) {
            settings_.set_workspace_splitter_sizes(workspace_splitter_sizes_);
        }
        settings_.sync();
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
        update_document_rail();
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
        if (iterations_spin_ != nullptr) {
            iterations_spin_->setEnabled(!has_active_solve());
        }
        state_label_->setText(tr("State: %1").arg(QString::fromLatin1(to_string(solver_state_.state()))));
    }

    bool main_window::has_active_solve() const
    {
        return active_solver_.valid();
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
