#include "widgets/node_lock_dialog.h"

#include "viewmodels/lock_view_model.h"

#include <QCheckBox>
#include <QDialogButtonBox>
#include <QDoubleSpinBox>
#include <QGridLayout>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QVBoxLayout>

#include <algorithm>
#include <utility>

namespace zeta::holdem::ui::widgets {

    namespace {

        [[nodiscard]] QString q(const std::string& value)
        {
            return QString::fromStdString(value);
        }

    }

    node_lock_dialog::node_lock_dialog(
        std::vector<std::string> path,
        const QString& actor,
        std::vector<std::string> actions,
        const std::vector<double>& solved_frequencies,
        const std::optional<cli::node_lock>& existing,
        QWidget* parent)
        : QDialog(parent)
        , path_(std::move(path))
        , actions_(std::move(actions))
    {
        setObjectName("nodeLockDialog");
        setWindowTitle(tr("Lock node"));
        auto* root = new QVBoxLayout{this};

        auto* heading = new QLabel{tr("%1 acts at %2").arg(actor, q(viewmodels::format_lock_path(path_))), this};
        heading->setObjectName("panelTitle");
        heading->setWordWrap(true);
        root->addWidget(heading);
        auto* hint = new QLabel{
            tr("Tick the actions to fix and set their frequencies. Unticked actions stay free: the solver shares "
               "what is left between them. Re-run the solve to see how the other player responds."),
            this};
        hint->setObjectName("mutedLabel");
        hint->setWordWrap(true);
        root->addWidget(hint);

        auto* grid = new QGridLayout;
        grid->addWidget(new QLabel{tr("Lock"), this}, 0, 0);
        grid->addWidget(new QLabel{tr("Frequency"), this}, 0, 1);
        grid->addWidget(new QLabel{tr("Solved"), this}, 0, 2);
        for (std::size_t index = 0; index < actions_.size(); ++index) {
            const auto row = static_cast<int>(index) + 1;
            const auto solved = index < solved_frequencies.size() ? solved_frequencies[index] : 0.0;
            auto* check = new QCheckBox{q(viewmodels::display_lock_step(actions_[index])), this};
            check->setObjectName(QStringLiteral("lockActionCheck_%1").arg(q(actions_[index])));
            auto* percent = new QDoubleSpinBox{this};
            percent->setObjectName(QStringLiteral("lockActionPercent_%1").arg(q(actions_[index])));
            percent->setRange(0.0, 100.0);
            percent->setDecimals(1);
            percent->setSingleStep(5.0);
            percent->setSuffix(QStringLiteral("%"));
            percent->setValue(solved * 100.0);
            if (existing) {
                const auto found = std::ranges::find(existing->actions, actions_[index], &cli::node_lock_action::action);
                if (found != existing->actions.end()) {
                    check->setChecked(true);
                    percent->setValue(found->frequency * 100.0);
                }
            }
            percent->setEnabled(check->isChecked());
            connect(check, &QCheckBox::toggled, percent, &QDoubleSpinBox::setEnabled);
            grid->addWidget(check, row, 0);
            grid->addWidget(percent, row, 1);
            grid->addWidget(new QLabel{QStringLiteral("%1%").arg(solved * 100.0, 0, 'f', 1), this}, row, 2);
            locked_.push_back(check);
            percents_.push_back(percent);
        }
        root->addLayout(grid);

        auto* hands_row = new QGridLayout;
        hands_row->addWidget(new QLabel{tr("Hands"), this}, 0, 0);
        hands_ = new QLineEdit{this};
        hands_->setObjectName("lockHandsField");
        hands_->setPlaceholderText(tr("All hands, or e.g. AA,KK,AKs"));
        hands_->setToolTip(tr("Range text naming the hands this lock applies to. Leave empty to lock the whole range. "
                              "A lock on some hands overrides a whole-range lock at the same node for those hands."));
        if (existing) {
            hands_->setText(q(existing->hands));
        }
        hands_row->addWidget(hands_, 0, 1);
        root->addLayout(hands_row);

        error_ = new QLabel{this};
        error_->setObjectName("errorLabel");
        error_->setWordWrap(true);
        error_->setVisible(false);
        root->addWidget(error_);

        auto* buttons = new QDialogButtonBox{QDialogButtonBox::Ok | QDialogButtonBox::Cancel, this};
        buttons->button(QDialogButtonBox::Ok)->setText(tr("Apply lock"));
        connect(buttons, &QDialogButtonBox::accepted, this, [this] {
            (void) try_accept();
        });
        connect(buttons, &QDialogButtonBox::rejected, this, &QDialog::reject);
        root->addWidget(buttons);
    }

    const std::optional<cli::node_lock>& node_lock_dialog::lock() const noexcept
    {
        return lock_;
    }

    bool node_lock_dialog::try_accept()
    {
        std::vector<viewmodels::lock_action_entry> entries;
        entries.reserve(actions_.size());
        for (std::size_t index = 0; index < actions_.size(); ++index) {
            entries.push_back(viewmodels::lock_action_entry{
                .action = actions_[index],
                .locked = locked_[index]->isChecked(),
                .percent = percents_[index]->value()
            });
        }
        auto built = viewmodels::make_node_lock(path_, hands_->text().toStdString(), entries);
        if (!built) {
            error_->setText(q(built.error()));
            error_->setVisible(true);
            return false;
        }
        lock_ = std::move(*built);
        accept();
        return true;
    }

}
