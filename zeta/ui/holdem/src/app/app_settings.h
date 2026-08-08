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

        [[nodiscard]] QByteArray window_geometry() const;
        void set_window_geometry(const QByteArray& geometry);

        [[nodiscard]] QList<int> shell_splitter_sizes() const;
        void set_shell_splitter_sizes(const QList<int>& sizes);

        [[nodiscard]] QList<int> workspace_splitter_sizes() const;
        void set_workspace_splitter_sizes(const QList<int>& sizes);

        [[nodiscard]] QStringList recent_files() const;
        void set_recent_files(const QStringList& files);
        void add_recent_file(const QString& file_path);

        void sync();

    private:
        [[nodiscard]] QList<int> read_int_list(const QString& key) const;
        void write_int_list(const QString& key, const QList<int>& values);

        QSettings settings_;
    };

}
