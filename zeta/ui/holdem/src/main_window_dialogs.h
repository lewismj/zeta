#pragma once

#include "theme/theme_registry.h"
#include "viewmodels/spot_view_model.h"

#include <QFileDialog>
#include <QString>
#include <QWidget>

#include <utility>
#include <vector>

namespace zeta::holdem::ui {

    enum class dialog_kind {
        info,
        warning,
        error
    };

    [[nodiscard]] QString error_text(const document_error& error);
    [[nodiscard]] QString validation_text(const std::vector<viewmodels::spot_validation_issue>& issues);

    [[nodiscard]] int show_themed_dialog(
        QWidget* parent,
        dialog_kind kind,
        const QString& title,
        const QString& message,
        const std::vector<std::pair<int, QString>>& buttons,
        int default_result);

    void show_themed_message(QWidget* parent, dialog_kind kind, const QString& title, const QString& message);
    [[nodiscard]] QString selected_file(QFileDialog& dialog);

}
