#pragma once

#include <QPlainTextEdit>

#include <memory>
#include <optional>

class QSyntaxHighlighter;
class QMimeData;

namespace zeta::holdem::ui::widgets {

    /**
     * Raw JSON editor with syntax highlighting and undo-safe formatting.
     */
    class spot_json_editor final : public QPlainTextEdit {
    public:
        explicit spot_json_editor(QWidget* parent = nullptr);

        [[nodiscard]] static std::optional<QString> format_json_text(const QString& text);

        void set_json_text(const QString& text);
        void format_document_if_valid();

    protected:
        void insertFromMimeData(const QMimeData* source) override;

    private:
        void replace_document_text(const QString& text, bool preserve_undo_stack);
        bool formatting_ = false;
        std::unique_ptr<QSyntaxHighlighter> highlighter_;
    };

}
