#include "main_window.h"

#include <QApplication>
#include <QDebug>
#include <QFont>
#include <QFontDatabase>

namespace {

    void load_application_fonts(QApplication& app)
    {
        const QStringList font_resources{
            QStringLiteral(":/fonts/D-DIN.ttf"),
            QStringLiteral(":/fonts/D-DIN-Bold.ttf"),
            QStringLiteral(":/fonts/D-DIN-Italic.ttf"),
            QStringLiteral(":/fonts/D-DINCondensed.ttf"),
            QStringLiteral(":/fonts/D-DINCondensed-Bold.ttf"),
            QStringLiteral(":/fonts/D-DINExp.ttf"),
            QStringLiteral(":/fonts/D-DINExp-Bold.ttf"),
            QStringLiteral(":/fonts/D-DINExp-Italic.ttf")
        };

        QString preferred_family;
        for (const auto& resource : font_resources) {
            const int font_id = QFontDatabase::addApplicationFont(resource);
            if (font_id < 0) {
                qWarning("Failed to load application font resource: %s", qPrintable(resource));
                continue;
            }
            const auto families = QFontDatabase::applicationFontFamilies(font_id);
            if (preferred_family.isEmpty() && families.contains(QStringLiteral("D-DIN"))) {
                preferred_family = QStringLiteral("D-DIN");
            }
        }

        if (!preferred_family.isEmpty()) {
            app.setFont(QFont{preferred_family, app.font().pointSize()});
        }
    }

}

int main(int argc, char** argv)
{
    QApplication app{argc, argv};
    load_application_fonts(app);
    zeta::holdem::ui::main_window window;
    window.show();
    return QApplication::exec();
}
