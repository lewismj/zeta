#pragma once

#include "../spot_document.h"

#include <QDialog>

#include <optional>
#include <string>
#include <vector>

class QCheckBox;
class QDoubleSpinBox;
class QLabel;
class QLineEdit;

namespace zeta::holdem::ui::widgets {

    /**
     * Edits one node lock: a lock checkbox and frequency per action, plus optional hands.
     * Rows start from `existing` when the node already has a lock for those hands,
     * otherwise from the node's solved frequencies with nothing locked.
     */
    class node_lock_dialog final : public QDialog {
    public:
        node_lock_dialog(
            std::vector<std::string> path,
            const QString& actor,
            std::vector<std::string> actions,
            const std::vector<double>& solved_frequencies,
            const std::optional<cli::node_lock>& existing,
            QWidget* parent = nullptr);

        /** The lock built when the dialog was accepted. */
        [[nodiscard]] const std::optional<cli::node_lock>& lock() const noexcept;

        /**
         * Validates the rows and, when they form a valid lock, stores it and accepts.
         * Otherwise shows the reason and stays open. Returns whether it accepted.
         */
        bool try_accept();

    private:
        std::vector<std::string> path_;
        std::vector<std::string> actions_;
        std::vector<QCheckBox*> locked_;
        std::vector<QDoubleSpinBox*> percents_;
        QLineEdit* hands_ = nullptr;
        QLabel* error_ = nullptr;
        std::optional<cli::node_lock> lock_;
    };

}
