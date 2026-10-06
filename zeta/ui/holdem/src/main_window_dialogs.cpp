#include "main_window_dialogs.h"

#include "theme/theme_styles.h"

#include <QDialog>
#include <QHBoxLayout>
#include <QIcon>
#include <QLabel>
#include <QPushButton>
#include <QStringList>
#include <QVBoxLayout>

namespace zeta::holdem::ui {

    namespace {

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

    }

    QString error_text(const document_error& error)
    {
        return QString::fromStdString(error.message);
    }

    QString validation_text(const std::vector<viewmodels::spot_validation_issue>& issues)
    {
        QStringList lines;
        for (const auto& issue : issues) {
            lines.push_back(QString::fromStdString(issue.message));
        }
        return lines.join(QStringLiteral("\n"));
    }

    int show_themed_dialog(
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

    QString selected_file(QFileDialog& dialog)
    {
        if (dialog.exec() != QDialog::Accepted) {
            return {};
        }
        const auto files = dialog.selectedFiles();
        return files.isEmpty() ? QString{} : files.front();
    }

}
