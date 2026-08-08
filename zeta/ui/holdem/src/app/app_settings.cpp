#include "app/app_settings.h"

#include "theme/theme_registry.h"

#include <QVariant>
#include <QVariantList>

namespace zeta::holdem::ui::app {

    namespace {

        constexpr int max_recent_files = 10;

    }

    app_settings::app_settings()
        : settings_(QSettings::IniFormat, QSettings::UserScope, QStringLiteral("Zeta"), QStringLiteral("HoldemSolver"))
    {
    }

    app_settings::app_settings(const QString& ini_path)
        : settings_(ini_path, QSettings::IniFormat)
    {
    }

    theme::theme_id app_settings::active_theme() const
    {
        const auto default_key = QString::fromStdString(std::string{theme::theme_key(theme::default_theme().id)});
        const auto key = settings_.value(QStringLiteral("appearance/theme"), default_key).toString();
        const auto parsed = theme::theme_id_from_key(key.toStdString());
        return parsed.value_or(theme::default_theme().id);
    }

    void app_settings::set_active_theme(const theme::theme_id theme)
    {
        settings_.setValue(QStringLiteral("appearance/theme"), QString::fromStdString(std::string{theme::theme_key(theme)}));
    }

    theme::density_mode app_settings::density() const
    {
        const auto key = settings_.value(QStringLiteral("appearance/density"), QStringLiteral("comfortable")).toString();
        const auto parsed = theme::density_mode_from_key(key.toStdString());
        return parsed.value_or(theme::density_mode::comfortable);
    }

    void app_settings::set_density(const theme::density_mode density)
    {
        settings_.setValue(QStringLiteral("appearance/density"), QString::fromStdString(std::string{theme::density_mode_key(density)}));
    }

    QByteArray app_settings::window_geometry() const
    {
        return settings_.value(QStringLiteral("window/geometry")).toByteArray();
    }

    void app_settings::set_window_geometry(const QByteArray& geometry)
    {
        settings_.setValue(QStringLiteral("window/geometry"), geometry);
    }

    QList<int> app_settings::shell_splitter_sizes() const
    {
        return read_int_list(QStringLiteral("window/shell_splitter"));
    }

    void app_settings::set_shell_splitter_sizes(const QList<int>& sizes)
    {
        write_int_list(QStringLiteral("window/shell_splitter"), sizes);
    }

    QList<int> app_settings::workspace_splitter_sizes() const
    {
        return read_int_list(QStringLiteral("window/workspace_splitter"));
    }

    void app_settings::set_workspace_splitter_sizes(const QList<int>& sizes)
    {
        write_int_list(QStringLiteral("window/workspace_splitter"), sizes);
    }

    QStringList app_settings::recent_files() const
    {
        return settings_.value(QStringLiteral("files/recent")).toStringList();
    }

    void app_settings::set_recent_files(const QStringList& files)
    {
        QStringList normalized;
        for (const auto& file : files) {
            if (!file.isEmpty() && !normalized.contains(file)) {
                normalized.push_back(file);
            }
            if (normalized.size() >= max_recent_files) {
                break;
            }
        }
        settings_.setValue(QStringLiteral("files/recent"), normalized);
    }

    void app_settings::add_recent_file(const QString& file_path)
    {
        if (file_path.isEmpty()) {
            return;
        }
        auto files = recent_files();
        files.removeAll(file_path);
        files.push_front(file_path);
        while (files.size() > max_recent_files) {
            files.removeLast();
        }
        settings_.setValue(QStringLiteral("files/recent"), files);
    }

    void app_settings::sync()
    {
        settings_.sync();
    }

    QList<int> app_settings::read_int_list(const QString& key) const
    {
        QList<int> values;
        for (const auto& value : settings_.value(key).toList()) {
            bool ok = false;
            const int parsed = value.toInt(&ok);
            if (ok && parsed > 0) {
                values.push_back(parsed);
            }
        }
        return values;
    }

    void app_settings::write_int_list(const QString& key, const QList<int>& values)
    {
        QVariantList stored;
        for (const int value : values) {
            if (value > 0) {
                stored.push_back(value);
            }
        }
        settings_.setValue(key, stored);
    }

}
