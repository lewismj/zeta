#include "widgets/spot_builder.h"

#include <QComboBox>
#include <QDoubleSpinBox>
#include <QFrame>
#include <QGridLayout>
#include <QHeaderView>
#include <QHBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QSignalBlocker>
#include <QSpinBox>
#include <QTableWidget>
#include <QTableWidgetItem>
#include <QVBoxLayout>

#include <algorithm>
#include <array>
#include <cstdint>
#include <utility>

namespace zeta::holdem::ui::widgets {

    namespace {

        [[nodiscard]] QFrame* make_panel()
        {
            auto* panel = new QFrame;
            panel->setFrameShape(QFrame::StyledPanel);
            panel->setObjectName("solverPanel");
            return panel;
        }

        [[nodiscard]] QLabel* make_panel_title(const QString& text)
        {
            auto* label = new QLabel{text};
            label->setObjectName("panelTitle");
            return label;
        }

        [[nodiscard]] QLabel* make_error_label()
        {
            auto* label = new QLabel;
            label->setObjectName("errorLabel");
            label->setWordWrap(true);
            label->setVisible(false);
            return label;
        }

        [[nodiscard]] QDoubleSpinBox* make_money_spin(QWidget* parent)
        {
            auto* spin = new QDoubleSpinBox{parent};
            spin->setRange(0.0, 1'000'000.0);
            spin->setDecimals(2);
            spin->setSingleStep(1.0);
            return spin;
        }

        [[nodiscard]] QDoubleSpinBox* make_fraction_spin(QWidget* parent)
        {
            auto* spin = new QDoubleSpinBox{parent};
            spin->setRange(0.01, 100.0);
            spin->setDecimals(3);
            spin->setSingleStep(0.05);
            return spin;
        }

        [[nodiscard]] QString seat_selector_text(const spot& source, const std::size_t seat)
        {
            const auto label = seat < source.players.size() && !source.players[seat].empty()
                ? source.players[seat]
                : "Seat " + std::to_string(seat + 1u);
            return QStringLiteral("%1: %2").arg(static_cast<qulonglong>(seat)).arg(QString::fromStdString(label));
        }

        [[nodiscard]] double table_number(const QTableWidget* table, const int row, const int column, const double fallback)
        {
            const auto* item = table->item(row, column);
            if (item == nullptr) {
                return fallback;
            }
            bool ok = false;
            const auto value = item->text().toDouble(&ok);
            return ok ? value : fallback;
        }

        [[nodiscard]] std::string table_text(const QTableWidget* table, const int row, const int column, const std::string& fallback)
        {
            const auto* item = table->item(row, column);
            if (item == nullptr) {
                return fallback;
            }
            return item->text().toStdString();
        }

        void set_first_issue(QLabel* label, const std::vector<viewmodels::spot_validation_issue>& issues, const std::initializer_list<const char*> fields)
        {
            for (const auto& issue : issues) {
                for (const auto* field : fields) {
                    if (issue.field == field) {
                        label->setText(QString::fromStdString(issue.message));
                        label->setVisible(true);
                        return;
                    }
                }
            }
            label->clear();
            label->setVisible(false);
        }

    }

    spot_builder::spot_builder(
        const spot& source,
        const theme::density_metrics metrics,
        spot_changed_callback on_spot_changed,
        duplicate_callback on_duplicate,
        QWidget* parent)
        : QWidget(parent)
        , spot_(source)
        , metrics_(metrics)
        , on_spot_changed_(std::move(on_spot_changed))
        , on_duplicate_(std::move(on_duplicate))
    {
        setObjectName("spotBuilder");
        create_layout();
        refresh_from_spot();
    }

    void spot_builder::set_spot(const spot& source)
    {
        spot_ = source;
        refresh_from_spot();
    }

    void spot_builder::create_layout()
    {
        auto* root = new QVBoxLayout{this};
        root->setContentsMargins(0, 0, 0, 0);
        root->setSpacing(metrics_.panel_spacing);

        auto* template_panel = make_panel();
        auto* template_layout = new QHBoxLayout{template_panel};
        template_layout->setContentsMargins(metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin);
        template_layout->setSpacing(metrics_.panel_spacing);
        template_layout->addWidget(make_panel_title(tr("Templates")));

        auto* heads_up = new QPushButton{tr("New heads-up river"), template_panel};
        auto* three_way = new QPushButton{tr("New 3-way flop"), template_panel};
        auto* four_way = new QPushButton{tr("New 4-way turn"), template_panel};
        auto* duplicate = new QPushButton{tr("Duplicate current spot"), template_panel};
        template_layout->addWidget(heads_up);
        template_layout->addWidget(three_way);
        template_layout->addWidget(four_way);
        template_layout->addWidget(duplicate);
        template_layout->addStretch(1);
        connect(heads_up, &QPushButton::clicked, this, [this] { apply_template(viewmodels::spot_template_kind::heads_up_river); });
        connect(three_way, &QPushButton::clicked, this, [this] { apply_template(viewmodels::spot_template_kind::three_way_flop); });
        connect(four_way, &QPushButton::clicked, this, [this] { apply_template(viewmodels::spot_template_kind::four_way_turn); });
        connect(duplicate, &QPushButton::clicked, this, [this] {
            if (on_duplicate_) {
                on_duplicate_(spot_from_controls());
            }
        });
        root->addWidget(template_panel);

        auto* spot_panel = make_panel();
        auto* grid = new QGridLayout{spot_panel};
        grid->setContentsMargins(metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin);
        grid->setSpacing(metrics_.panel_spacing);
        grid->addWidget(make_panel_title(tr("Spot")), 0, 0, 1, 4);

        street_selector_ = new QComboBox{spot_panel};
        street_selector_->setObjectName("streetSelector");
        street_selector_->addItems({tr("flop"), tr("turn"), tr("river")});
        player_count_ = new QSpinBox{spot_panel};
        player_count_->setObjectName("playerCountSelector");
        player_count_->setRange(static_cast<int>(cli::cli_min_players), static_cast<int>(cli::cli_max_players));
        grid->addWidget(new QLabel{tr("Street"), spot_panel}, 1, 0);
        grid->addWidget(street_selector_, 1, 1);
        grid->addWidget(new QLabel{tr("Players"), spot_panel}, 1, 2);
        grid->addWidget(player_count_, 1, 3);

        for (int index = 0; index < 5; ++index) {
            auto* card = new QComboBox{spot_panel};
            card->setObjectName(QStringLiteral("boardCard%1").arg(index));
            card->addItem(QStringLiteral("-"));
            for (const auto& label : viewmodels::deck_card_labels()) {
                card->addItem(QString::fromStdString(label));
            }
            board_cards_.push_back(card);
            grid->addWidget(card, 2, index);
            connect(card, &QComboBox::currentTextChanged, this, [this] {
                if (!updating_) {
                    spot_ = spot_from_controls();
                    refresh_validation();
                    emit_spot_changed();
                }
            });
        }
        board_error_ = make_error_label();
        grid->addWidget(board_error_, 3, 0, 1, 5);

        root_actor_ = new QComboBox{spot_panel};
        root_actor_->setObjectName("rootActorSelector");
        hero_seat_ = new QComboBox{spot_panel};
        hero_seat_->setObjectName("heroSeatSelector");
        gross_pot_ = make_money_spin(spot_panel);
        gross_pot_->setObjectName("grossPotField");
        rake_ = make_money_spin(spot_panel);
        rake_->setObjectName("rakeField");
        bet_fraction_ = make_fraction_spin(spot_panel);
        bet_fraction_->setObjectName("betFractionField");
        max_history_ = new QSpinBox{spot_panel};
        max_history_->setObjectName("maxHistoryField");
        max_history_->setRange(0, 10'000);
        public_state_id_ = new QSpinBox{spot_panel};
        public_state_id_->setObjectName("publicStateIdField");
        public_state_id_->setRange(0, 1'000'000);
        samples_per_combo_ = new QSpinBox{spot_panel};
        samples_per_combo_->setObjectName("samplesPerComboField");
        samples_per_combo_->setRange(1, 10'000);

        grid->addWidget(new QLabel{tr("Root actor"), spot_panel}, 4, 0);
        grid->addWidget(root_actor_, 4, 1);
        grid->addWidget(new QLabel{tr("Hero"), spot_panel}, 4, 2);
        grid->addWidget(hero_seat_, 4, 3);
        actor_error_ = make_error_label();
        grid->addWidget(actor_error_, 5, 0, 1, 4);

        grid->addWidget(new QLabel{tr("Gross pot"), spot_panel}, 6, 0);
        grid->addWidget(gross_pot_, 6, 1);
        grid->addWidget(new QLabel{tr("Rake"), spot_panel}, 6, 2);
        grid->addWidget(rake_, 6, 3);
        grid->addWidget(new QLabel{tr("Bet fraction"), spot_panel}, 7, 0);
        grid->addWidget(bet_fraction_, 7, 1);
        grid->addWidget(new QLabel{tr("Max history"), spot_panel}, 7, 2);
        grid->addWidget(max_history_, 7, 3);
        grid->addWidget(new QLabel{tr("Public state"), spot_panel}, 8, 0);
        grid->addWidget(public_state_id_, 8, 1);
        grid->addWidget(new QLabel{tr("Samples/combo"), spot_panel}, 8, 2);
        grid->addWidget(samples_per_combo_, 8, 3);
        root->addWidget(spot_panel);

        auto* seats_panel = make_panel();
        auto* seats_layout = new QVBoxLayout{seats_panel};
        seats_layout->setContentsMargins(metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin, metrics_.panel_margin);
        seats_layout->setSpacing(metrics_.panel_spacing);
        seats_layout->addWidget(make_panel_title(tr("Seats")));
        seat_table_ = new QTableWidget{seats_panel};
        seat_table_->setObjectName("seatTableEditor");
        seat_table_->setColumnCount(3);
        seat_table_->setHorizontalHeaderLabels({tr("Label"), tr("Stack"), tr("Committed")});
        seat_table_->verticalHeader()->setVisible(false);
        seat_table_->horizontalHeader()->setStretchLastSection(true);
        seats_layout->addWidget(seat_table_);
        players_error_ = make_error_label();
        seats_layout->addWidget(players_error_);
        root->addWidget(seats_panel, 1);

        connect(street_selector_, &QComboBox::currentTextChanged, this, [this](const QString& street) {
            if (updating_) {
                return;
            }
            spot_ = spot_from_controls();
            spot_.street = street.toStdString();
            const auto expected = viewmodels::board_card_count_for_street(spot_.street);
            if (spot_.board.size() > expected) {
                spot_.board.resize(expected);
            }
            refresh_board_controls();
            refresh_validation();
            emit_spot_changed();
        });
        connect(player_count_, &QSpinBox::valueChanged, this, [this](const int value) {
            if (updating_) {
                return;
            }
            spot_ = viewmodels::resize_player_count(spot_from_controls(), static_cast<std::size_t>(value));
            refresh_from_spot();
            emit_spot_changed();
        });
        connect(root_actor_, &QComboBox::currentIndexChanged, this, [this](const int index) {
            if (!updating_ && index >= 0) {
                spot_ = spot_from_controls();
                refresh_validation();
                emit_spot_changed();
            }
        });
        connect(hero_seat_, &QComboBox::currentIndexChanged, this, [this](const int index) {
            if (!updating_ && index >= 0) {
                spot_ = spot_from_controls();
                refresh_validation();
                emit_spot_changed();
            }
        });
        const auto numeric_changed = [this] {
            if (!updating_) {
                spot_ = spot_from_controls();
                refresh_validation();
                emit_spot_changed();
            }
        };
        connect(gross_pot_, &QDoubleSpinBox::valueChanged, this, numeric_changed);
        connect(rake_, &QDoubleSpinBox::valueChanged, this, numeric_changed);
        connect(bet_fraction_, &QDoubleSpinBox::valueChanged, this, numeric_changed);
        connect(max_history_, &QSpinBox::valueChanged, this, numeric_changed);
        connect(public_state_id_, &QSpinBox::valueChanged, this, numeric_changed);
        connect(samples_per_combo_, &QSpinBox::valueChanged, this, numeric_changed);
        connect(seat_table_, &QTableWidget::itemChanged, this, [this] {
            if (!updating_) {
                spot_ = spot_from_controls();
                refresh_actor_selectors();
                refresh_validation();
                emit_spot_changed();
            }
        });
    }

    void spot_builder::refresh_from_spot()
    {
        updating_ = true;
        street_selector_->setCurrentText(QString::fromStdString(spot_.street));
        player_count_->setValue(static_cast<int>(spot_.players.size()));

        gross_pot_->setValue(spot_.gross_pot);
        rake_->setValue(spot_.rake);
        bet_fraction_->setValue(spot_.bet_fraction);
        max_history_->setValue(spot_.max_history);
        public_state_id_->setValue(static_cast<int>(spot_.public_state_id));
        samples_per_combo_->setValue(spot_.samples_per_combo);

        seat_table_->setRowCount(static_cast<int>(spot_.players.size()));
        for (int row = 0; row < seat_table_->rowCount(); ++row) {
            const auto index = static_cast<std::size_t>(row);
            seat_table_->setItem(row, 0, new QTableWidgetItem{QString::fromStdString(index < spot_.players.size() ? spot_.players[index] : std::string{})});
            seat_table_->setItem(row, 1, new QTableWidgetItem{QString::number(index < spot_.stacks.size() ? spot_.stacks[index] : 100.0, 'f', 2)});
            seat_table_->setItem(row, 2, new QTableWidgetItem{QString::number(index < spot_.contributions.size() ? spot_.contributions[index] : 0.0, 'f', 2)});
        }
        refresh_board_controls();
        refresh_actor_selectors();
        updating_ = false;
        refresh_validation();
    }

    void spot_builder::refresh_board_controls()
    {
        const auto expected = viewmodels::board_card_count_for_street(spot_.street);
        for (std::size_t index = 0; index < board_cards_.size(); ++index) {
            auto* combo = board_cards_[index];
            combo->setVisible(index < expected);
            const auto text = index < spot_.board.size() ? QString::fromStdString(spot_.board[index]) : QStringLiteral("-");
            const int item = combo->findText(text);
            combo->setCurrentIndex(item >= 0 ? item : 0);
        }
    }

    void spot_builder::refresh_actor_selectors()
    {
        const QSignalBlocker root_blocker{root_actor_};
        const QSignalBlocker hero_blocker{hero_seat_};
        root_actor_->clear();
        hero_seat_->clear();
        for (std::size_t seat = 0; seat < spot_.players.size(); ++seat) {
            root_actor_->addItem(seat_selector_text(spot_, seat), static_cast<int>(seat));
            hero_seat_->addItem(seat_selector_text(spot_, seat), static_cast<int>(seat));
        }
        root_actor_->setCurrentIndex(std::min<std::size_t>(spot_.root_actor, spot_.players.empty() ? 0u : spot_.players.size() - 1u));
        hero_seat_->setCurrentIndex(std::min<std::size_t>(spot_.hero_seat, spot_.players.empty() ? 0u : spot_.players.size() - 1u));
    }

    void spot_builder::refresh_validation()
    {
        const auto issues = viewmodels::validate_structured_spot(spot_from_controls());
        set_first_issue(board_error_, issues, {"street", "board"});
        set_first_issue(players_error_, issues, {"players", "ranges", "stacks", "contributions"});
        set_first_issue(actor_error_, issues, {"root_actor", "hero_seat", "gross_pot", "rake", "bet_fraction", "samples_per_combo"});
    }

    void spot_builder::emit_spot_changed()
    {
        if (on_spot_changed_) {
            on_spot_changed_(spot_);
        }
    }

    void spot_builder::apply_template(const viewmodels::spot_template_kind kind)
    {
        spot_ = viewmodels::make_template_spot(kind);
        refresh_from_spot();
        emit_spot_changed();
    }

    spot spot_builder::spot_from_controls() const
    {
        auto out = spot_;
        out.street = street_selector_->currentText().toStdString();
        out.board.clear();
        const auto expected = viewmodels::board_card_count_for_street(out.street);
        for (std::size_t index = 0; index < std::min(expected, board_cards_.size()); ++index) {
            const auto text = board_cards_[index]->currentText();
            if (text != QStringLiteral("-")) {
                out.board.push_back(text.toStdString());
            }
        }

        out = viewmodels::resize_player_count(std::move(out), static_cast<std::size_t>(player_count_->value()));
        for (int row = 0; row < seat_table_->rowCount() && row < static_cast<int>(out.players.size()); ++row) {
            const auto index = static_cast<std::size_t>(row);
            out.players[index] = table_text(seat_table_, row, 0, out.players[index]);
            out.stacks[index] = table_number(seat_table_, row, 1, out.stacks[index]);
            out.contributions[index] = table_number(seat_table_, row, 2, out.contributions[index]);
        }
        out.root_actor = static_cast<uint8_t>(std::max(0, root_actor_->currentIndex()));
        out.hero_seat = static_cast<uint8_t>(std::max(0, hero_seat_->currentIndex()));
        out.gross_pot = gross_pot_->value();
        out.rake = rake_->value();
        out.bet_fraction = bet_fraction_->value();
        out.max_history = static_cast<uint16_t>(max_history_->value());
        out.public_state_id = static_cast<uint32_t>(public_state_id_->value());
        out.samples_per_combo = static_cast<uint16_t>(samples_per_combo_->value());
        return out;
    }

}
