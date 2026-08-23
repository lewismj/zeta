#include "widgets/spot_json_editor.h"

#include <QColor>
#include <QJsonDocument>
#include <QMimeData>
#include <QRegularExpression>
#include <QSyntaxHighlighter>
#include <QTextCharFormat>
#include <QTextCursor>

#include <algorithm>

namespace zeta::holdem::ui::widgets {

    namespace {

        class json_syntax_highlighter final : public QSyntaxHighlighter {
        public:
            explicit json_syntax_highlighter(QTextDocument* parent)
                : QSyntaxHighlighter(parent)
            {
                string_format_.setForeground(QColor{0x7A, 0xC7, 0xFF});
                key_format_.setForeground(QColor{0xE5, 0xC0, 0x7B});
                number_format_.setForeground(QColor{0x98, 0xC3, 0x79});
                keyword_format_.setForeground(QColor{0xC6, 0x78, 0xDD});
                punctuation_format_.setForeground(QColor{0x61, 0xAF, 0xEF});
            }

        protected:
            void highlightBlock(const QString& text) override
            {
                highlight_matches(text, QRegularExpression{R"("(?:\\.|[^"\\])*")"}, string_format_);
                highlight_matches(text, QRegularExpression{R"("(?:\\.|[^"\\])*"(?=\s*:))"}, key_format_);
                highlight_matches(text, QRegularExpression{R"(\b-?(?:0|[1-9]\d*)(?:\.\d+)?(?:[eE][+\-]?\d+)?\b)"}, number_format_);
                highlight_matches(text, QRegularExpression{R"(\b(?:true|false|null)\b)"}, keyword_format_);
                highlight_matches(text, QRegularExpression{R"([{}\[\]:,])"}, punctuation_format_);
            }

        private:
            void highlight_matches(const QString& text, const QRegularExpression& pattern, const QTextCharFormat& format)
            {
                auto iterator = pattern.globalMatch(text);
                while (iterator.hasNext()) {
                    const auto match = iterator.next();
                    setFormat(match.capturedStart(), match.capturedLength(), format);
                }
            }

            QTextCharFormat string_format_{};
            QTextCharFormat key_format_{};
            QTextCharFormat number_format_{};
            QTextCharFormat keyword_format_{};
            QTextCharFormat punctuation_format_{};
        };

    }

    spot_json_editor::spot_json_editor(QWidget* parent)
        : QPlainTextEdit(parent)
        , highlighter_(std::make_unique<json_syntax_highlighter>(document()))
    {
        setLineWrapMode(QPlainTextEdit::NoWrap);
    }

    std::optional<QString> spot_json_editor::format_json_text(const QString& text)
    {
        QJsonParseError error{};
        const auto document = QJsonDocument::fromJson(text.toUtf8(), &error);
        if (error.error != QJsonParseError::NoError || document.isNull()) {
            return std::nullopt;
        }
        return QString::fromUtf8(document.toJson(QJsonDocument::Indented)).trimmed();
    }

    void spot_json_editor::set_json_text(const QString& text)
    {
        auto formatted = format_json_text(text);
        setPlainText(formatted.value_or(text));
    }

    void spot_json_editor::format_document_if_valid()
    {
        if (formatting_) {
            return;
        }
        auto formatted = format_json_text(toPlainText());
        if (!formatted.has_value() || *formatted == toPlainText()) {
            return;
        }
        replace_document_text(*formatted, true);
    }

    void spot_json_editor::insertFromMimeData(const QMimeData* source)
    {
        if (source == nullptr || !source->hasText() || formatting_) {
            QPlainTextEdit::insertFromMimeData(source);
            return;
        }

        const auto pasted = source->text();
        const int pasted_cursor_position = textCursor().position() + pasted.size();
        QTextCursor cursor = textCursor();
        cursor.beginEditBlock();
        cursor.insertText(pasted);
        setTextCursor(cursor);

        auto formatted = format_json_text(toPlainText());
        if (formatted.has_value() && *formatted != toPlainText()) {
            formatting_ = true;
            QTextCursor replace_cursor{document()};
            replace_cursor.select(QTextCursor::Document);
            replace_cursor.insertText(*formatted);
            QTextCursor restored_cursor = textCursor();
            restored_cursor.setPosition(std::clamp(pasted_cursor_position, 0, static_cast<int>(formatted->size())));
            setTextCursor(restored_cursor);
            formatting_ = false;
        }
        cursor.endEditBlock();
    }

    void spot_json_editor::replace_document_text(const QString& text, const bool preserve_undo_stack)
    {
        const int cursor_position = textCursor().position();
        formatting_ = true;
        QTextCursor cursor{document()};
        if (preserve_undo_stack) {
            cursor.beginEditBlock();
        }
        cursor.select(QTextCursor::Document);
        cursor.insertText(text);
        if (preserve_undo_stack) {
            cursor.endEditBlock();
        }
        QTextCursor restored_cursor = textCursor();
        restored_cursor.setPosition(std::clamp(cursor_position, 0, static_cast<int>(text.size())));
        setTextCursor(restored_cursor);
        formatting_ = false;
    }

}
