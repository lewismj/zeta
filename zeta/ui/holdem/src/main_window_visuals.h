#pragma once

#include <QPixmap>
#include <QSize>
#include <QWidget>

namespace zeta::holdem::ui {

    [[nodiscard]] QPixmap zeta_logo_pixmap(const QSize& size);
    void polish_widget(QWidget* widget);

}
