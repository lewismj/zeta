#include "main_window.h"

#include "document_workspace_widget.h"
#include "main_window_visuals.h"
#include "theme/theme_registry.h"
#include "theme/theme_styles.h"

#include <QAbstractItemView>
#include <QAction>
#include <QActionGroup>
#include <QFileInfo>
#include <QIcon>
#include <QKeySequence>
#include <QLabel>
#include <QListWidget>
#include <QMenu>
#include <QMenuBar>
#include <QSignalBlocker>
#include <QSplitter>
#include <QStatusBar>
#include <QTabWidget>
#include <QTimer>
#include <QToolBar>
#include <QVariant>
#include <QVBoxLayout>

#include <filesystem>

namespace zeta::holdem::ui {

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
        save_action_->setShortcut(QKeySequence::Save);
        save_as_action_->setShortcut(QKeySequence::SaveAs);

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
        auto* logo = new QLabel{toolbar};
        logo->setObjectName("appLogo");
        logo->setPixmap(zeta_logo_pixmap(QSize{24, 24}));
        logo->setFixedSize(28, 28);
        logo->setAlignment(Qt::AlignCenter);
        toolbar->addWidget(logo);
        toolbar->addSeparator();
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

    void main_window::set_active_theme(const theme::theme_id theme_id)
    {
        if (active_theme_ == theme_id) {
            return;
        }
        active_theme_ = theme_id;
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

}
