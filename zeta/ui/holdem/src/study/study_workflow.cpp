#include "study/study_workflow.h"

#include <QPainter>

#include <algorithm>
#include <cctype>
#include <iomanip>
#include <map>
#include <set>
#include <sstream>

namespace zeta::holdem::ui::study {

    namespace {

        [[nodiscard]] std::string lower_ascii(std::string text)
        {
            std::ranges::transform(text, text.begin(), [](const unsigned char c) {
                return static_cast<char>(std::tolower(c));
            });
            return text;
        }

        [[nodiscard]] bool contains_case_insensitive(const std::string& text, const std::string_view query)
        {
            if (query.empty()) {
                return true;
            }
            return lower_ascii(text).find(lower_ascii(std::string{query})) != std::string::npos;
        }

        [[nodiscard]] bool has_tag(const study_record& record, const std::string_view tag)
        {
            if (tag.empty()) {
                return true;
            }
            return std::ranges::any_of(record.tags, [tag](const auto& value) {
                return lower_ascii(value) == lower_ascii(std::string{tag});
            });
        }

        [[nodiscard]] std::string csv_escape(const std::string_view text)
        {
            const bool quote = text.find_first_of(",\"\r\n") != std::string_view::npos;
            if (!quote) {
                return std::string{text};
            }
            std::string out{"\""};
            for (const char c : text) {
                if (c == '"') {
                    out += "\"\"";
                } else {
                    out.push_back(c);
                }
            }
            out.push_back('"');
            return out;
        }

        [[nodiscard]] std::string fixed_number(const double value, const int precision = 6)
        {
            std::ostringstream out;
            out << std::fixed << std::setprecision(precision) << value;
            return out.str();
        }

        [[nodiscard]] std::map<std::string, double> aggregate_actions(const solve_artifact& artifact)
        {
            std::map<std::string, double> totals;
            if (artifact.strategy.empty()) {
                return totals;
            }
            for (const auto& row : artifact.strategy) {
                for (const auto& action : row.strategy) {
                    totals[action.action] += action.frequency;
                }
            }
            const auto divisor = static_cast<double>(artifact.strategy.size());
            for (auto& [_, frequency] : totals) {
                frequency /= divisor;
            }
            return totals;
        }

        [[nodiscard]] std::map<std::string, std::pair<std::string, double>> hand_summary(const solve_artifact& artifact)
        {
            std::map<std::string, std::pair<std::string, double>> out;
            for (const auto& row : artifact.strategy) {
                std::string best_action;
                double best_frequency = -1.0;
                for (const auto& action : row.strategy) {
                    if (action.frequency > best_frequency) {
                        best_action = action.action;
                        best_frequency = action.frequency;
                    }
                }
                out[row.hand] = {best_action, row.ev};
            }
            return out;
        }

        [[nodiscard]] std::string join_text(const std::vector<std::string>& values, const std::string_view separator)
        {
            std::ostringstream out;
            for (std::size_t i = 0; i < values.size(); ++i) {
                if (i != 0u) {
                    out << separator;
                }
                out << values[i];
            }
            return out.str();
        }

        [[nodiscard]] bool same_spot_identity(const spot& lhs, const spot& rhs)
        {
            return lhs.players == rhs.players
                && lhs.street == rhs.street
                && lhs.board == rhs.board
                && lhs.root_actor == rhs.root_actor
                && lhs.hero_seat == rhs.hero_seat
                && lhs.ranges == rhs.ranges;
        }

    }

    std::vector<study_record> filter_studies(
        const std::vector<study_record>& studies,
        const std::string_view query,
        const std::optional<std::string_view> required_tag)
    {
        std::vector<study_record> out;
        for (const auto& record : studies) {
            if (required_tag && !has_tag(record, *required_tag)) {
                continue;
            }
            const auto path_text = record.path.string();
            if (!contains_case_insensitive(record.title, query)
                && !contains_case_insensitive(path_text, query)
                && !std::ranges::any_of(record.tags, [query](const auto& tag) {
                    return contains_case_insensitive(tag, query);
                })) {
                continue;
            }
            out.push_back(record);
        }
        std::ranges::stable_sort(out, [](const auto& lhs, const auto& rhs) {
            if (lhs.pinned != rhs.pinned) {
                return lhs.pinned;
            }
            return lhs.updated_utc > rhs.updated_utc;
        });
        return out;
    }

    std::string export_strategy_csv(const solve_artifact& artifact)
    {
        std::set<std::string> actions;
        for (const auto& row : artifact.strategy) {
            for (const auto& action : row.strategy) {
                actions.insert(action.action);
            }
        }

        std::ostringstream out;
        out << "hand,ev";
        for (const auto& action : actions) {
            out << ',' << csv_escape(action);
        }
        out << '\n';

        for (const auto& row : artifact.strategy) {
            std::map<std::string, double> frequencies;
            for (const auto& action : row.strategy) {
                frequencies[action.action] = action.frequency;
            }
            out << csv_escape(row.hand) << ',' << fixed_number(row.ev);
            for (const auto& action : actions) {
                out << ',' << fixed_number(frequencies[action]);
            }
            out << '\n';
        }
        return out.str();
    }

    std::string export_hand_table_csv(const viewmodels::strategy_view_model& model)
    {
        std::ostringstream out;
        out << "hand,hand_class,best_action,actions,ev,range_weight,live\n";
        for (const auto& row : model.hands) {
            out << csv_escape(row.hand) << ','
                << csv_escape(row.hand_class) << ','
                << csv_escape(row.best_action) << ','
                << csv_escape(viewmodels::format_strategy_actions(row.actions)) << ','
                << fixed_number(row.ev) << ','
                << fixed_number(row.range_weight) << ','
                << (row.live ? "true" : "false") << '\n';
        }
        return out.str();
    }

    std::string make_share_summary(
        const spot& source,
        const solve_artifact& artifact,
        const viewmodels::strategy_view_model& model)
    {
        std::ostringstream out;
        out << "Players: " << join_text(source.players, ", ") << '\n';
        out << "Board: " << join_text(source.board, " ") << '\n';
        out << "Pot: " << fixed_number(source.gross_pot, 2) << '\n';
        out << "Root actor: " << (source.root_actor < source.players.size() ? source.players[source.root_actor] : std::to_string(source.root_actor)) << '\n';
        out << "Hero: " << (artifact.hero_seat < source.players.size() ? source.players[artifact.hero_seat] : std::to_string(artifact.hero_seat)) << '\n';
        out << "Iterations: " << artifact.solver.iterations << '\n';
        out << "Top actions:";
        for (const auto& card : model.action_cards) {
            out << ' ' << card.action << ' ' << viewmodels::format_strategy_percent(card.frequency);
        }
        out << '\n';
        return out.str();
    }

    std::expected<strategy_run_comparison, std::string> compare_strategy_runs(
        const spot& before_spot,
        const solve_artifact& before,
        const spot& after_spot,
        const solve_artifact& after)
    {
        if (!same_spot_identity(before_spot, after_spot)
            || before.players != after.players
            || before.board != after.board
            || before.hero_seat != after.hero_seat) {
            return std::unexpected(std::string{"Runs are not compatible for comparison."});
        }

        strategy_run_comparison comparison;
        if (before.solver.iterations != after.solver.iterations) {
            comparison.settings_differences.push_back("iterations");
        }
        if (before.solver.algorithm != after.solver.algorithm) {
            comparison.settings_differences.push_back("algorithm");
        }

        const auto before_actions = aggregate_actions(before);
        const auto after_actions = aggregate_actions(after);
        std::set<std::string> actions;
        for (const auto& [action, _] : before_actions) {
            actions.insert(action);
        }
        for (const auto& [action, _] : after_actions) {
            actions.insert(action);
        }
        for (const auto& action : actions) {
            const auto before_frequency = before_actions.contains(action) ? before_actions.at(action) : 0.0;
            const auto after_frequency = after_actions.contains(action) ? after_actions.at(action) : 0.0;
            comparison.action_deltas.push_back(action_frequency_delta{
                .action = action,
                .before = before_frequency,
                .after = after_frequency,
                .delta = after_frequency - before_frequency
            });
        }

        const auto before_hands = hand_summary(before);
        const auto after_hands = hand_summary(after);
        std::set<std::string> hands;
        for (const auto& [hand, _] : before_hands) {
            hands.insert(hand);
        }
        for (const auto& [hand, _] : after_hands) {
            hands.insert(hand);
        }
        for (const auto& hand : hands) {
            const auto before_row = before_hands.contains(hand) ? before_hands.at(hand) : std::pair<std::string, double>{};
            const auto after_row = after_hands.contains(hand) ? after_hands.at(hand) : std::pair<std::string, double>{};
            if (before_row.first != after_row.first) {
                ++comparison.changed_best_action_count;
            }
            comparison.ev_deltas.push_back(hand_ev_delta{
                .hand = hand,
                .before = before_row.second,
                .after = after_row.second,
                .delta = after_row.second - before_row.second
            });
        }

        return comparison;
    }

    QImage capture_widget_image(QWidget& widget)
    {
        const auto size = widget.size().isEmpty() ? widget.sizeHint() : widget.size();
        QImage image{size, QImage::Format_ARGB32};
        image.fill(Qt::transparent);
        QPainter painter{&image};
        widget.render(&painter);
        return image;
    }

}
