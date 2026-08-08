#include "main_window.h"

#include <QAbstractItemView>
#include <QAction>
#include <QActionGroup>
#include <QCloseEvent>
#include <QComboBox>
#include <QDialog>
#include <QFileDialog>
#include <QFileInfo>
#include <QFormLayout>
#include <QFrame>
#include <QHeaderView>
#include <QHBoxLayout>
#include <QIcon>
#include <QLabel>
#include <QListWidget>
#include <QMenuBar>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QSignalBlocker>
#include <QSizePolicy>
#include <QSplitter>
#include <QSpinBox>
#include <QStatusBar>
#include <QStyle>
#include <QStringList>
#include <QTabWidget>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QTextCursor>
#include <QTimer>
#include <QToolBar>
#include <QVariant>
#include <QVBoxLayout>

#include "theme/theme_registry.h"
#include "theme/theme_styles.h"
#include "viewmodels/spot_view_model.h"
#include "widgets/range_editor.h"
#include "widgets/spot_builder.h"
#include "widgets/strategy_explorer.h"
#include "widgets/table_state_view.h"

#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <future>
#include <filesystem>
#include <thread>
#include <utility>
#include <vector>

namespace zeta::holdem::ui {

    namespace {

        constexpr int min_solver_iterations = 1;
        constexpr int max_solver_iterations = 1'000'000;
        constexpr int min_worker_threads = 1;
        constexpr int max_worker_threads = 64;

        [[nodiscard]] int available_worker_threads() noexcept
        {
            const auto hardware_threads = std::thread::hardware_concurrency();
            if (hardware_threads == 0) {
                return max_worker_threads;
            }
            return std::clamp(static_cast<int>(hardware_threads), min_worker_threads, max_worker_threads);
        }

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

        [[nodiscard]] QString validation_text(const std::vector<viewmodels::spot_validation_issue>& issues)
        {
            QStringList lines;
            for (const auto& issue : issues) {
                lines.push_back(QString::fromStdString(issue.message));
            }
            return lines.join(QStringLiteral("\n"));
        }

        enum class dialog_kind {
            info,
            warning,
            error
        };

        [[nodiscard]] QString dialog_icon_path(const dialog_kind kind)
        {
            switch (kind) {
                case dialog_kind::info:
                    return QStringLiteral(":/icons/info.svg");
                case dialog_kind::warning:
                    return QStringLiteral(":/icons/triangle-alert.svg");
                case dialog_kind::error:
                    return QStringLiteral(":/icons/circle-x.svg");
            }
            return QStringLiteral(":/icons/info.svg");
        }

        [[nodiscard]] const theme::registered_theme& theme_for_widget(QWidget* widget)
        {
            if (widget == nullptr || widget->window() == nullptr) {
                return theme::default_theme();
            }
            const auto theme_value = widget->window()->property("zetaThemeId");
            if (!theme_value.isValid()) {
                return theme::default_theme();
            }
            switch (theme_value.toInt()) {
                case static_cast<int>(theme::theme_id::light_pro):
                    return theme::find_theme(theme::theme_id::light_pro);
                case static_cast<int>(theme::theme_id::high_contrast):
                    return theme::find_theme(theme::theme_id::high_contrast);
                case static_cast<int>(theme::theme_id::dark_pro):
                default:
                    return theme::default_theme();
            }
        }

        [[nodiscard]] int show_themed_dialog(
            QWidget* parent,
            const dialog_kind kind,
            const QString& title,
            const QString& message,
            const std::vector<std::pair<int, QString>>& buttons,
            const int default_result)
        {
            QDialog dialog{parent};
            dialog.setWindowTitle(title);
            dialog.setModal(true);
            if (parent != nullptr && parent->window() != nullptr) {
                dialog.setStyleSheet(parent->window()->styleSheet());
            }
            (void) dialog.winId();
            theme::apply_native_title_bar(&dialog, theme_for_widget(parent));

            auto* root = new QVBoxLayout{&dialog};
            root->setContentsMargins(18, 16, 18, 16);
            root->setSpacing(14);

            auto* content = new QHBoxLayout;
            content->setSpacing(12);
            auto* icon = new QLabel{&dialog};
            icon->setObjectName("dialogIcon");
            icon->setPixmap(QIcon{dialog_icon_path(kind)}.pixmap(QSize{28, 28}));
            icon->setFixedSize(32, 32);
            icon->setAlignment(Qt::AlignTop | Qt::AlignHCenter);
            content->addWidget(icon);

            auto* label = new QLabel{message, &dialog};
            label->setObjectName("dialogMessage");
            label->setWordWrap(true);
            label->setMinimumWidth(360);
            content->addWidget(label, 1);
            root->addLayout(content);

            auto* button_row = new QHBoxLayout;
            button_row->addStretch(1);
            for (const auto& [result, text] : buttons) {
                auto* button = new QPushButton{text, &dialog};
                button->setDefault(result == default_result);
                QObject::connect(button, &QPushButton::clicked, &dialog, [&dialog, result] {
                    dialog.done(result);
                });
                button_row->addWidget(button);
            }
            root->addLayout(button_row);

            return dialog.exec();
        }

        void show_themed_message(QWidget* parent, const dialog_kind kind, const QString& title, const QString& message)
        {
            (void) show_themed_dialog(parent, kind, title, message, {{QDialog::Accepted, QObject::tr("OK")}}, QDialog::Accepted);
        }

        [[nodiscard]] QString selected_file(QFileDialog& dialog)
        {
            if (dialog.exec() != QDialog::Accepted) {
                return {};
            }
            const auto files = dialog.selectedFiles();
            return files.isEmpty() ? QString{} : files.front();
        }

        void polish(QWidget* widget)
        {
            widget->style()->unpolish(widget);
            widget->style()->polish(widget);
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

        [[nodiscard]] QString range_summary_title(const spot& spot)
        {
            return QStringLiteral("%1 range").arg(actor_label(spot));
        }

        [[nodiscard]] QString range_summary_value(const spot& spot)
        {
            const auto range_index = editable_range_index(spot);
            const auto range = range_index < spot.ranges.size() ? spot.ranges[range_index] : std::string{};
            return QStringLiteral("%1 hands").arg(range_tokens(range).size());
        }

        [[nodiscard]] QString bet_summary_value(const spot& spot)
        {
            return QStringLiteral("%1% pot").arg(spot.bet_fraction * 100.0, 0, 'f', 1);
        }

        void update_inspector_summary(QWidget* root, const spot_document& document)
        {
            if (root == nullptr) {
                return;
            }

            const auto& spot = document.current_spot();
            if (auto* range_button = root->findChild<QPushButton*>(QStringLiteral("callButton"))) {
                range_button->setText(range_summary_title(spot) + QStringLiteral("\n") + range_summary_value(spot));
            }
            if (auto* bet_button = root->findChild<QPushButton*>(QStringLiteral("foldButton"))) {
                bet_button->setText(QStringLiteral("Bet size\n") + bet_summary_value(spot));
            }
            if (auto* hands_table = root->findChild<QTableWidget*>(QStringLiteral("handsTable"))) {
                const auto range_index = editable_range_index(spot);
                hands_table->setItem(0, 0, new QTableWidgetItem{range_summary_title(spot)});
                hands_table->setItem(0, 1, new QTableWidgetItem{range_index < spot.ranges.size() ? QString::fromStdString(spot.ranges[range_index]) : QString{}});
            }
        }

        [[nodiscard]] QWidget* create_actions_panel(const spot_document& document, const theme::density_metrics& metrics)
        {
            auto* panel = make_panel();
            auto* layout = new QHBoxLayout{panel};
            layout->setContentsMargins(metrics.panel_margin, metrics.panel_margin, metrics.panel_margin, metrics.panel_margin);
            layout->setSpacing(metrics.panel_spacing);

            const auto& spot = document.current_spot();
            layout->addWidget(make_action_button(
                range_summary_title(spot),
                range_summary_value(spot),
                QStringLiteral("callButton"),
                metrics));
            layout->addWidget(make_action_button(
                QStringLiteral("Bet size"),
                bet_summary_value(spot),
                QStringLiteral("foldButton"),
                metrics));
            return panel;
        }

        [[nodiscard]] QWidget* create_hands_panel(const spot_document& document)
        {
            auto* table = new QTableWidget{1, 2};
            table->setObjectName("handsTable");
            table->setHorizontalHeaderLabels({QStringLiteral("Input"), QStringLiteral("Value")});
            table->verticalHeader()->setVisible(false);
            table->horizontalHeader()->setStretchLastSection(true);
            table->setEditTriggers(QAbstractItemView::NoEditTriggers);
            table->setSelectionMode(QAbstractItemView::NoSelection);

            const auto& spot = document.current_spot();
            const auto range_index = editable_range_index(spot);
            table->setItem(0, 0, new QTableWidgetItem{range_summary_title(spot)});
            table->setItem(0, 1, new QTableWidgetItem{range_index < spot.ranges.size() ? QString::fromStdString(spot.ranges[range_index]) : QString{}});
            return table;
        }

    }

    main_window::main_window(QWidget* parent)
        : QMainWindow(parent)
    {
        active_theme_ = settings_.active_theme();
        density_mode_ = settings_.density();
        solver_iterations_ = settings_.solver_iterations();
        progress_batch_iterations_ = settings_.solver_progress_batch_iterations();
        worker_threads_ = std::clamp(settings_.solver_worker_threads(), min_worker_threads, available_worker_threads());
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
            show_themed_message(
                this,
                dialog_kind::info,
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
        configuration_action_ = new QAction{tr("&Configuration"), this};

        new_action_->setIcon(QIcon{QStringLiteral(":/icons/file-plus.svg")});
        open_action_->setIcon(QIcon{QStringLiteral(":/icons/folder-open.svg")});
        save_action_->setIcon(QIcon{QStringLiteral(":/icons/save.svg")});
        validate_action_->setIcon(QIcon{QStringLiteral(":/icons/check-circle.svg")});
        solve_action_->setIcon(QIcon{QStringLiteral(":/icons/play.svg")});
        cancel_action_->setIcon(QIcon{QStringLiteral(":/icons/square.svg")});
        configuration_action_->setIcon(QIcon{QStringLiteral(":/icons/settings.svg")});

        connect(new_action_, &QAction::triggered, this, [this] { new_document(); });
        connect(open_action_, &QAction::triggered, this, [this] { open_document(); });
        connect(save_action_, &QAction::triggered, this, [this] { save_active_document(); });
        connect(save_as_action_, &QAction::triggered, this, [this] { save_active_document_as(); });
        connect(validate_action_, &QAction::triggered, this, [this] { validate_active_document(); });
        connect(solve_action_, &QAction::triggered, this, [this] { solve_active_document(); });
        connect(cancel_action_, &QAction::triggered, this, [this] { cancel_solver(); });
        connect(configuration_action_, &QAction::triggered, this, [this] { show_configuration_settings(); });
    }

    void main_window::create_layout()
    {
        apply_active_theme();

        menuBar()->hide();

        auto* toolbar = addToolBar(tr("Hold'em Solver"));
        toolbar->setObjectName("commandBar");
        toolbar->setMovable(false);
        toolbar->setIconSize(QSize{22, 22});
        toolbar->setToolButtonStyle(Qt::ToolButtonTextBesideIcon);
        toolbar->addAction(new_action_);
        toolbar->addAction(open_action_);
        toolbar->addAction(save_action_);
        toolbar->addSeparator();
        toolbar->addAction(validate_action_);
        toolbar->addAction(solve_action_);
        toolbar->addAction(cancel_action_);
        toolbar->addSeparator();
        toolbar->addAction(configuration_action_);

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
        state_label_->setObjectName("solverStateLabel");
        status_label_ = new QLabel{this};
        status_label_->setObjectName("solverStatusLabel");
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
        QFileDialog dialog{this, tr("Open Hold'em spot")};
        dialog.setAcceptMode(QFileDialog::AcceptOpen);
        dialog.setNameFilters({tr("JSON documents (*.json)"), tr("All files (*)")});
        dialog.setOption(QFileDialog::DontUseNativeDialog);
        dialog.setStyleSheet(styleSheet());
        (void) dialog.winId();
        apply_native_title_bar_theme(&dialog);
        const auto path = selected_file(dialog);
        if (path.isEmpty()) {
            return;
        }
        open_document_path(std::filesystem::path{path.toStdWString()});
    }

    void main_window::open_document_path(const std::filesystem::path& path)
    {
        auto document = spot_document::load(path);
        if (!document) {
            show_themed_message(this, dialog_kind::error, tr("Open failed"), error_text(document.error()));
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
            show_themed_message(this, dialog_kind::error, tr("Save failed"), error_text(result.error()));
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
        QFileDialog dialog{this, tr("Save Hold'em spot")};
        dialog.setAcceptMode(QFileDialog::AcceptSave);
        dialog.setNameFilters({tr("JSON documents (*.json)"), tr("All files (*)")});
        dialog.setDefaultSuffix(QStringLiteral("json"));
        dialog.setOption(QFileDialog::DontUseNativeDialog);
        dialog.setStyleSheet(styleSheet());
        (void) dialog.winId();
        apply_native_title_bar_theme(&dialog);
        const auto path = selected_file(dialog);
        if (path.isEmpty()) {
            return false;
        }
        if (!parse_editor_into_document(*entry, true)) {
            return false;
        }
        auto result = entry->document.save_as(std::filesystem::path{path.toStdWString()});
        if (!result) {
            show_themed_message(this, dialog_kind::error, tr("Save failed"), error_text(result.error()));
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
            show_themed_message(this, dialog_kind::warning, tr("Invalid solver state"), QString::fromStdString(transition.error()));
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
            show_themed_message(this, dialog_kind::warning, tr("Solve in progress"), tr("Wait for the active solve to finish before starting another one."));
            return;
        }
        auto* entry = active_entry();
        if (entry == nullptr || !parse_editor_into_document(*entry, true)) {
            return;
        }
        const int document_index = tabs_->currentIndex();
        if (auto transition = solver_state_.transition_to(solver_state::starting); !transition) {
            show_themed_message(this, dialog_kind::warning, tr("Invalid solver state"), QString::fromStdString(transition.error()));
            return;
        }

        solver::solver_session_request request{
            .spot_snapshot = entry->document.current_spot(),
            .iterations = static_cast<uint64_t>(solver_iterations_)
        };
        request.runtime.progress_batch_iterations = static_cast<uint64_t>(progress_batch_iterations_);
        request.runtime.worker_threads = static_cast<uint32_t>(worker_threads_);
        if (const char* revision = std::getenv("ZETA_GIT_REVISION")) {
            request.runtime.git_revision = revision;
        }

        active_session_ = std::make_shared<solver::solver_session>(std::move(request));
        active_solver_document_index_ = document_index;
        const auto& session_request = active_session_->request();
        set_solve_console(*entry, tr("Started %1\nIterations: %2\nProgress batch: %3\nWorker threads: %4\nOutput: store artifact in active document\nPlayers: %5\nStatus: running")
            .arg(QString::fromStdString(cli::detail::now_utc_iso8601()))
            .arg(static_cast<qulonglong>(session_request.iterations))
            .arg(static_cast<qulonglong>(session_request.runtime.progress_batch_iterations))
            .arg(static_cast<qulonglong>(session_request.runtime.worker_threads))
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

    void main_window::show_configuration_settings()
    {
        QDialog dialog{this};
        dialog.setWindowTitle(tr("Configuration Settings"));
        dialog.setObjectName("configurationDialog");
        dialog.setStyleSheet(styleSheet());
        dialog.setMinimumWidth(460);
        (void) dialog.winId();
        apply_native_title_bar_theme(&dialog);

        auto* root = new QVBoxLayout{&dialog};
        root->setContentsMargins(18, 16, 18, 16);
        root->setSpacing(12);

        auto* title = make_panel_title(tr("Configuration"));
        root->addWidget(title);

        auto* tabs = new QTabWidget{&dialog};

        auto* ui_panel = make_panel();
        auto* ui_layout = new QFormLayout{ui_panel};
        ui_layout->setContentsMargins(14, 12, 14, 12);
        ui_layout->setSpacing(10);

        auto* theme_combo = new QComboBox{ui_panel};
        for (const auto& registered_theme : theme::registered_themes()) {
            theme_combo->addItem(
                QString::fromStdString(registered_theme.display_name),
                static_cast<int>(registered_theme.id));
        }
        theme_combo->setCurrentIndex(theme_combo->findData(static_cast<int>(active_theme_)));
        ui_layout->addRow(tr("Theme"), theme_combo);

        auto* density_combo = new QComboBox{ui_panel};
        density_combo->addItem(tr("Comfortable"), static_cast<int>(theme::density_mode::comfortable));
        density_combo->addItem(tr("Compact"), static_cast<int>(theme::density_mode::compact));
        density_combo->setCurrentIndex(density_combo->findData(static_cast<int>(density_mode_)));
        ui_layout->addRow(tr("Density"), density_combo);

        tabs->addTab(ui_panel, tr("UI"));

        auto* solver_panel = make_panel();
        auto* solver_layout = new QFormLayout{solver_panel};
        solver_layout->setContentsMargins(14, 12, 14, 12);
        solver_layout->setSpacing(10);

        auto* iterations = new QSpinBox{solver_panel};
        iterations->setRange(min_solver_iterations, max_solver_iterations);
        iterations->setSingleStep(50);
        iterations->setValue(solver_iterations_);
        iterations->setToolTip(tr("CFR iterations for the next solve."));
        solver_layout->addRow(tr("Iterations"), iterations);

        auto* progress_batch = new QSpinBox{solver_panel};
        progress_batch->setRange(min_solver_iterations, max_solver_iterations);
        progress_batch->setSingleStep(10);
        progress_batch->setValue(progress_batch_iterations_);
        progress_batch->setToolTip(tr("Number of CFR iterations between progress updates."));
        solver_layout->addRow(tr("Progress batch iterations"), progress_batch);

        auto* threads = new QSpinBox{solver_panel};
        threads->setObjectName("workerThreadsSpinBox");
        threads->setRange(min_worker_threads, available_worker_threads());
        threads->setValue(std::clamp(worker_threads_, min_worker_threads, available_worker_threads()));
        threads->setToolTip(tr("CFR worker threads for the next solve."));
        solver_layout->addRow(tr("Worker threads"), threads);

        tabs->addTab(solver_panel, tr("Solver"));

        root->addWidget(tabs);

        auto* buttons = new QHBoxLayout;
        buttons->addStretch(1);
        auto* ok = new QPushButton{tr("OK"), &dialog};
        auto* cancel = new QPushButton{tr("Cancel"), &dialog};
        ok->setDefault(true);
        buttons->addWidget(ok);
        buttons->addWidget(cancel);
        root->addLayout(buttons);
        connect(ok, &QPushButton::clicked, &dialog, &QDialog::accept);
        connect(cancel, &QPushButton::clicked, &dialog, &QDialog::reject);

        if (dialog.exec() != QDialog::Accepted) {
            return;
        }

        const auto selected_theme = static_cast<theme::theme_id>(theme_combo->currentData().toInt());
        const auto selected_density = static_cast<theme::density_mode>(density_combo->currentData().toInt());
        set_active_theme(selected_theme);
        set_density_mode(selected_density);

        solver_iterations_ = iterations->value();
        progress_batch_iterations_ = progress_batch->value();
        worker_threads_ = threads->value();
        settings_.set_solver_iterations(solver_iterations_);
        settings_.set_solver_progress_batch_iterations(progress_batch_iterations_);
        settings_.set_solver_worker_threads(worker_threads_);
        settings_.sync();
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
                    auto solution = solver::make_action_tree_solution_store(result.spot_snapshot, *result.artifact);
                    entry.document.replace_artifact(std::move(result.artifact));
                    entry.document.replace_solution(std::move(solution));
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
        const int choice = show_themed_dialog(
            this,
            dialog_kind::warning,
            tr("Unsaved changes"),
            tr("Save changes to %1?").arg(display_name(entry)),
            {{1, tr("Save")}, {2, tr("Discard")}, {0, tr("Cancel")}},
            1);
        if (choice == 0) {
            return false;
        }
        if (choice == 2) {
            return true;
        }
        return save_active_document();
    }

    bool main_window::maybe_close_active_solve(const int index)
    {
        if (!has_active_solve() || index != active_solver_document_index_) {
            return true;
        }
        show_themed_message(
            this,
            dialog_kind::info,
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
                show_themed_message(this, dialog_kind::error, tr("Invalid spot"), QString::fromStdString(parsed.error().message));
            }
            return false;
        }
        const auto issues = viewmodels::validate_structured_spot(*parsed);
        if (!issues.empty()) {
            if (show_error) {
                show_themed_message(this, dialog_kind::error, tr("Invalid spot"), validation_text(issues));
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

        auto refresh_raw_editor = [this, raw_editor, summary_header, table_view, right_column, index] {
            if (index < 0 || index >= static_cast<int>(documents_.size())) {
                return;
            }
            auto& entry = documents_[index];
            entry.updating_editor = true;
            raw_editor->setPlainText(QString::fromStdString(cli::serialize_spot_json(entry.document.current_spot())));
            entry.updating_editor = false;
            summary_header->setText(QString::fromStdString(viewmodels::spot_summary_text(entry.document.current_spot(), entry.document.artifact().has_value())));
            table_view->set_spot(entry.document.current_spot());
            update_inspector_summary(right_column, entry.document);
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
        if (entry.document.artifact()) {
            left_tabs->addTab(new widgets::strategy_explorer{
                entry.document.current_spot(),
                *entry.document.artifact(),
                entry.document.solution(),
                metrics,
                left_tabs}, tr("Strategy Explorer"));
        } else {
            auto* range_editor = new widgets::range_editor{
                entry.document.current_spot(),
                metrics,
                [this, index, refresh_raw_editor](spot next_spot) {
                    if (index < 0 || index >= static_cast<int>(documents_.size())) {
                        return;
                    }
                    documents_[index].document.replace_spot(std::move(next_spot));
                    refresh_raw_editor();
                },
                left_tabs,
                active_theme_};
            left_tabs->addTab(range_editor, tr("Ranges"));
        }
        left_tabs->addTab(raw_editor, tr("Spot JSON"));

        if (!entry.document.artifact()) {
            right_layout->addWidget(create_actions_panel(entry.document, metrics));
            right_layout->addWidget(create_hands_panel(entry.document), 1);
        } else {
            right_layout->addStretch(1);
        }

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
        setProperty("zetaThemeId", static_cast<int>(active_theme_));
        apply_native_title_bar_theme(this);
    }

    void main_window::apply_native_title_bar_theme(QWidget* window)
    {
        theme::apply_native_title_bar(window, theme::find_theme(active_theme_));
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
        setWindowTitle(tr("%1 - Zeta Hold'em Solver").arg(title));
    }

    void main_window::update_solver_controls()
    {
        const auto controls = solver_state_.controls();
        validate_action_->setEnabled(controls.validate_enabled && active_entry() != nullptr);
        solve_action_->setEnabled(controls.solve_enabled && active_entry() != nullptr);
        cancel_action_->setEnabled(controls.cancel_enabled);
        if (configuration_action_ != nullptr) {
            configuration_action_->setEnabled(!has_active_solve());
        }
        const bool active = has_active_solve();
        state_label_->setProperty("solverActive", active);
        status_label_->setProperty("solverActive", active);
        polish(state_label_);
        polish(status_label_);
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
