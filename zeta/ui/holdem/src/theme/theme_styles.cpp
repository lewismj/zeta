#include "theme/theme_styles.h"

#include "theme/theme_registry.h"

#if defined(Q_OS_WIN)
#include <dwmapi.h>
#include <windows.h>
#endif

#include <QCoreApplication>
#include <QDebug>
#include <QDir>
#include <QFile>
#include <QWidget>

#include <cstdlib>

namespace zeta::holdem::ui::theme {

    namespace {

        [[nodiscard]] QString q(const std::string& value)
        {
            return QString::fromStdString(value);
        }

        [[nodiscard]] QString read_text_file(const QString& path)
        {
            QFile file{path};
            if (!file.open(QIODevice::ReadOnly | QIODevice::Text)) {
                qWarning() << "Failed to load stylesheet:" << path << file.errorString();
                return {};
            }
            return QString::fromUtf8(file.readAll());
        }

        [[nodiscard]] QString style_sheet_template()
        {
            const auto external_path = QDir{QCoreApplication::applicationDirPath()}
                                           .filePath(QStringLiteral("styles/holdem.qss"));
            if (QFile::exists(external_path)) {
                auto sheet = read_text_file(external_path);
                if (!sheet.isEmpty()) {
                    return sheet;
                }
            }

            auto sheet = read_text_file(QStringLiteral(":/styles/holdem.qss"));
            if (sheet.isEmpty()) {
                qWarning() << "Using empty stylesheet because no stylesheet template could be loaded.";
            }
            return sheet;
        }

#if defined(Q_OS_WIN)
        [[nodiscard]] COLORREF color_ref_from_hex(const std::string& hex)
        {
            if (hex.size() != 7 || hex[0] != '#') {
                return RGB(0, 0, 0);
            }
            const auto component = [&hex](const std::size_t offset) {
                return static_cast<BYTE>(std::strtoul(hex.substr(offset, 2).c_str(), nullptr, 16));
            };
            return RGB(component(1), component(3), component(5));
        }
#endif

    }

    QString style_sheet(const registered_theme& theme, const density_mode density)
    {
        const auto& t = theme.tokens;
        const auto metrics = metrics_for_density(density);
        auto sheet = style_sheet_template();

        const auto replace_placeholder = [&sheet](const int index, const QString& value) {
            sheet.replace(QStringLiteral("%") + QString::number(index), value);
        };

        replace_placeholder(33, QString::number(density == density_mode::compact ? 3 : 4));
        replace_placeholder(37, q(t.destructive_text));
        replace_placeholder(36, q(t.button_text));
        replace_placeholder(35, q(t.active_surface));
        replace_placeholder(34, q(t.document_selection));
        replace_placeholder(32, QString::number(density == density_mode::compact ? 32 : 36));
        replace_placeholder(31, q(t.range_heat[1]));
        replace_placeholder(30, QString::number(density == density_mode::compact ? 12 : 14));
        replace_placeholder(29, QString::number(density == density_mode::compact ? 15 : 17));
        replace_placeholder(28, QString::number(density == density_mode::compact ? 12 : 14));
        replace_placeholder(27, q(t.error));
        replace_placeholder(26, QString::number(density == density_mode::compact ? 10 : 12));
        replace_placeholder(25, QString::number(density == density_mode::compact ? 22 : 26));
        replace_placeholder(24, QString::number(density == density_mode::compact ? 10 : 12));
        replace_placeholder(23, QString::number(density == density_mode::compact ? 6 : 8));
        replace_placeholder(22, QString::number(metrics.toolbar_spacing));
        replace_placeholder(21, q(t.action_positive));
        replace_placeholder(20, q(t.warning));
        replace_placeholder(19, q(t.range_heat[3]));
        replace_placeholder(18, q(t.range_heat[2]));
        replace_placeholder(17, q(t.range_heat[0]));
        replace_placeholder(16, q(t.selection));
        replace_placeholder(15, q(t.ev_positive));
        replace_placeholder(14, q(t.action_negative));
        replace_placeholder(13, q(t.action_primary_hover));
        replace_placeholder(12, q(t.action_primary));
        replace_placeholder(11, q(t.accent_secondary));
        replace_placeholder(10, q(t.accent_primary));
        replace_placeholder(9, q(t.text_muted));
        replace_placeholder(8, q(t.text_secondary));
        replace_placeholder(7, q(t.text_primary));
        replace_placeholder(6, q(t.border_strong));
        replace_placeholder(5, q(t.border_subtle));
        replace_placeholder(4, q(t.background_input));
        replace_placeholder(3, q(t.background_sunken));
        replace_placeholder(2, q(t.background_raised));
        replace_placeholder(1, q(t.background_base));
        return sheet;
    }

    void apply_native_title_bar(QWidget* window, const registered_theme& theme)
    {
#if defined(Q_OS_WIN)
        if (window == nullptr) {
            return;
        }

        const BOOL dark_title_bar = theme.id != theme_id::light_pro;
        auto hwnd = reinterpret_cast<HWND>(window->winId());

        constexpr DWORD dwmwa_use_immersive_dark_mode = 20;
        constexpr DWORD dwmwa_caption_color = 35;
        constexpr DWORD dwmwa_text_color = 36;
        (void) DwmSetWindowAttribute(hwnd, dwmwa_use_immersive_dark_mode, &dark_title_bar, sizeof(dark_title_bar));

        const COLORREF caption_color = color_ref_from_hex(theme.tokens.background_sunken);
        const COLORREF text_color = color_ref_from_hex(theme.tokens.text_primary);
        (void) DwmSetWindowAttribute(hwnd, dwmwa_caption_color, &caption_color, sizeof(caption_color));
        (void) DwmSetWindowAttribute(hwnd, dwmwa_text_color, &text_color, sizeof(text_color));
#else
        (void) window;
        (void) theme;
#endif
    }

}
