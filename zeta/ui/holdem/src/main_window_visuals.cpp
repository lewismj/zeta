#include "main_window_visuals.h"

#include <QFont>
#include <QImage>
#include <QPainter>
#include <QStyle>

#include <algorithm>

namespace zeta::holdem::ui {

    namespace {

        [[nodiscard]] bool has_light_foreground_pixels(const QPixmap& pixmap)
        {
            if (pixmap.isNull()) {
                return false;
            }
            const QImage image = pixmap.toImage().convertToFormat(QImage::Format_ARGB32);
            for (int y = 0; y < image.height(); ++y) {
                const auto* row = reinterpret_cast<const QRgb*>(image.constScanLine(y));
                for (int x = 0; x < image.width(); ++x) {
                    const QRgb px = row[x];
                    if (qAlpha(px) < 32) {
                        continue;
                    }
                    if (qRed(px) > 200 && qGreen(px) > 200 && qBlue(px) > 200) {
                        return true;
                    }
                }
            }
            return false;
        }

    }

    QPixmap zeta_logo_pixmap(const QSize& size)
    {
        const QPixmap source{QStringLiteral(":/icons/zeta-logo.svg")};
        QPixmap logo = source.scaled(size, Qt::KeepAspectRatio, Qt::SmoothTransformation);
        if (has_light_foreground_pixels(logo)) {
            return logo;
        }

        // Use the shipped zeta-logo.svg as the base image and recover only
        // the missing glyph in environments where Qt drops SVG text rendering.
        QPainter painter{&logo};
        painter.setRenderHint(QPainter::Antialiasing, true);
        QFont symbol_font{QStringLiteral("Segoe UI Symbol")};
        symbol_font.setPixelSize(std::max(10, (size.height() * 15) / 24));
        symbol_font.setBold(true);
        painter.setFont(symbol_font);
        painter.setPen(Qt::white);
        painter.drawText(logo.rect(), Qt::AlignCenter, QStringLiteral("ζ"));
        return logo;
    }

    void polish_widget(QWidget* widget)
    {
        if (widget == nullptr) {
            return;
        }
        widget->style()->unpolish(widget);
        widget->style()->polish(widget);
    }

}
