#pragma once

#include "theme/theme.h"

#include <QByteArray>
#include <QList>
#include <QSettings>
#include <QString>
#include <QStringList>

namespace zeta::holdem::ui::app {

    /**
     * Persists user preferences and shell state for the Hold'em workbench.
     */
    class app_settings {
    public:
        app_settings();
        explicit app_settings(const QString& ini_path);

        [[nodiscard]] theme::theme_id active_theme() const;
        void set_active_theme(theme::theme_id theme);

        [[nodiscard]] theme::density_mode density() const;
        void set_density(theme::density_mode density);

        [[nodiscard]] int solver_iterations() const;
        void set_solver_iterations(int iterations);

        [[nodiscard]] int solver_progress_batch_iterations() const;
        void set_solver_progress_batch_iterations(int iterations);

        [[nodiscard]] int solver_worker_threads() const;
        void set_solver_worker_threads(int threads);

        [[nodiscard]] bool solver_card_isomorphism() const;
        void set_solver_card_isomorphism(bool enabled);

        [[nodiscard]] bool solver_allow_lossy_isomorphism() const;
        void set_solver_allow_lossy_isomorphism(bool allowed);

        [[nodiscard]] bool solver_dynamic_pruning() const;
        void set_solver_dynamic_pruning(bool enabled);

        [[nodiscard]] double solver_pruning_threshold() const;
        void set_solver_pruning_threshold(double threshold);

        [[nodiscard]] int solver_pruning_minimum_active_actions() const;
        void set_solver_pruning_minimum_active_actions(int minimum);

        [[nodiscard]] int solver_pruning_reconsider_interval() const;
        void set_solver_pruning_reconsider_interval(int interval);

        [[nodiscard]] QByteArray window_geometry() const;
        void set_window_geometry(const QByteArray& geometry);

        [[nodiscard]] QList<int> shell_splitter_sizes() const;
        void set_shell_splitter_sizes(const QList<int>& sizes);

        [[nodiscard]] QList<int> workspace_splitter_sizes() const;
        void set_workspace_splitter_sizes(const QList<int>& sizes);

        [[nodiscard]] QStringList recent_files() const;
        void set_recent_files(const QStringList& files);
        void add_recent_file(const QString& file_path);

        [[nodiscard]] QStringList pinned_files() const;
        void set_pinned_files(const QStringList& files);
        void set_file_pinned(const QString& file_path, bool pinned);

        void sync();

    private:
        [[nodiscard]] QList<int> read_int_list(const QString& key) const;
        void write_int_list(const QString& key, const QList<int>& values);

        QSettings settings_;
    };

}
