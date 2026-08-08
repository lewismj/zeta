#include "widgets/table_state_view.h"

#include <QGridLayout>
#include <QLabel>
#include <QLayout>
#include <QPair>
#include <QVBoxLayout>

#include <algorithm>
#include <string>
#include <vector>

namespace zeta::holdem::ui::widgets {

    namespace {

        [[nodiscard]] QString money_text(const double value)
        {
            return QString::number(value, 'f', value == static_cast<int>(value) ? 0 : 2);
        }

        [[nodiscard]] QString board_text(const spot& source)
        {
            QString out;
            for (const auto& card : source.board) {
                if (!out.isEmpty()) {
                    out += QStringLiteral(" ");
                }
                out += QString::fromStdString(card);
            }
            return out.isEmpty() ? QStringLiteral("-") : out;
        }

        [[nodiscard]] QString seat_card_text(const spot& source, const std::size_t seat)
        {
            const auto stack = seat < source.stacks.size() ? source.stacks[seat] : 0.0;
            const auto contribution = seat < source.contributions.size() ? source.contributions[seat] : 0.0;
            QString marker;
            if (seat == source.root_actor) {
                marker += QStringLiteral("  ACT");
            }
            if (seat == source.hero_seat) {
                marker += QStringLiteral("  HERO");
            }
            return QStringLiteral("%1%2\nStack %3\nCommitted %4")
                .arg(QString::fromStdString(seat < source.players.size() ? source.players[seat] : std::string{"Seat"}))
                .arg(marker)
                .arg(money_text(stack))
                .arg(money_text(contribution));
        }

        [[nodiscard]] std::vector<QPair<int, int>> seat_positions(const std::size_t count)
        {
            switch (count) {
                case 2: return {{1, 0}, {1, 2}};
                case 3: return {{0, 1}, {1, 2}, {1, 0}};
                case 4: return {{0, 1}, {1, 2}, {2, 1}, {1, 0}};
                case 5: return {{0, 0}, {0, 2}, {1, 2}, {2, 1}, {1, 0}};
                case 6: return {{0, 0}, {0, 1}, {0, 2}, {2, 2}, {2, 1}, {2, 0}};
                default: return {};
            }
        }

        void clear_layout(QLayout* layout)
        {
            while (auto* item = layout->takeAt(0)) {
                if (auto* widget = item->widget(); widget != nullptr) {
                    widget->deleteLater();
                }
                delete item;
            }
        }

    }

    table_state_view::table_state_view(const spot& source, const theme::density_metrics metrics, QWidget* parent)
        : QFrame(parent)
        , spot_(source)
        , metrics_(metrics)
    {
        setObjectName("tableStatePanel");
        layout_ = new QGridLayout{this};
        layout_->setContentsMargins(metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin);
        layout_->setSpacing(metrics_.panel_spacing);
        rebuild();
    }

    void table_state_view::set_spot(const spot& source)
    {
        spot_ = source;
        rebuild();
    }

    void table_state_view::rebuild()
    {
        clear_layout(layout_);
        layout_->setColumnStretch(0, 1);
        layout_->setColumnStretch(1, 1);
        layout_->setColumnStretch(2, 1);
        layout_->setRowStretch(0, 1);
        layout_->setRowStretch(1, 1);
        layout_->setRowStretch(2, 1);

        auto* center = new QLabel{
            QStringLiteral("%1\nBoard %2\nPot %3 | Rake %4")
                .arg(QString::fromStdString(spot_.street))
                .arg(board_text(spot_))
                .arg(money_text(spot_.gross_pot))
                .arg(money_text(spot_.rake)),
            this};
        center->setObjectName("tableFelt");
        center->setAlignment(Qt::AlignCenter);
        center->setMinimumHeight(120);
        layout_->addWidget(center, 1, 1);

        const auto positions = seat_positions(spot_.players.size());
        for (std::size_t seat = 0; seat < std::min(spot_.players.size(), positions.size()); ++seat) {
            auto* card = new QLabel{seat_card_text(spot_, seat), this};
            const bool active = seat == spot_.root_actor;
            const bool hero = seat == spot_.hero_seat;
            card->setObjectName(active && hero ? "activeHeroSeatCard" : active ? "activeSeatCard" : hero ? "heroSeatCard" : "seatCard");
            card->setAlignment(Qt::AlignCenter);
            card->setMinimumHeight(64);
            card->setWordWrap(true);
            layout_->addWidget(card, positions[seat].first, positions[seat].second);
        }
    }

}
