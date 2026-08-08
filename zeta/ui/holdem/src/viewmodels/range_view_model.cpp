#include "viewmodels/range_view_model.h"

#include "cli/solve_cli.h"

#include <algorithm>
#include <array>
#include <cctype>
#include <cmath>
#include <iomanip>
#include <map>
#include <sstream>
#include <string_view>

namespace zeta::holdem::ui::viewmodels {

    namespace {

        constexpr std::array ranks_desc{'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2'};
        constexpr std::array ranks_asc{'2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'};

        [[nodiscard]] std::string trim_copy(const std::string_view text)
        {
            const auto first = text.find_first_not_of(" \t\r\n");
            if (first == std::string_view::npos) {
                return {};
            }
            const auto last = text.find_last_not_of(" \t\r\n");
            return std::string{text.substr(first, last - first + 1u)};
        }

        [[nodiscard]] std::vector<std::string> split_range_terms(const std::string_view source)
        {
            std::vector<std::string> terms;
            std::size_t start = 0;
            while (start <= source.size()) {
                const auto comma = source.find(',', start);
                const auto end = comma == std::string_view::npos ? source.size() : comma;
                auto term = trim_copy(source.substr(start, end - start));
                if (!term.empty()) {
                    terms.push_back(std::move(term));
                }
                if (comma == std::string_view::npos) {
                    break;
                }
                start = comma + 1u;
            }
            return terms;
        }

        [[nodiscard]] std::string join_range_terms(const std::vector<std::string>& terms)
        {
            std::ostringstream out;
            for (std::size_t i = 0; i < terms.size(); ++i) {
                if (i != 0u) {
                    out << ", ";
                }
                out << terms[i];
            }
            return out.str();
        }

        [[nodiscard]] std::array<uint8_t, 2> cards_from_combo(const card_mask mask)
        {
            std::array<uint8_t, 2> cards{};
            std::size_t count = 0;
            for (uint8_t id = 0; id < 52; ++id) {
                if ((mask & (card_mask{1} << id)) != 0u) {
                    cards[count++] = id;
                    if (count == cards.size()) {
                        break;
                    }
                }
            }
            return cards;
        }

        [[nodiscard]] int rank_order_index(const char rank)
        {
            const auto found = std::ranges::find(ranks_desc, static_cast<char>(std::toupper(static_cast<unsigned char>(rank))));
            return found == ranks_desc.end() ? static_cast<int>(ranks_desc.size()) : static_cast<int>(std::distance(ranks_desc.begin(), found));
        }

        [[nodiscard]] std::string hand_class_from_combo(const combination_index combo)
        {
            const auto cards = cards_from_combo(combination_masks[combo]);
            const auto first_rank = static_cast<uint8_t>(cards[0] % 13u);
            const auto second_rank = static_cast<uint8_t>(cards[1] % 13u);
            const auto first_suit = static_cast<uint8_t>(cards[0] / 13u);
            const auto second_suit = static_cast<uint8_t>(cards[1] / 13u);

            if (first_rank == second_rank) {
                return std::string{ranks_asc[first_rank], ranks_asc[first_rank]};
            }

            const bool first_high = first_rank > second_rank;
            const auto high_rank = first_high ? first_rank : second_rank;
            const auto low_rank = first_high ? second_rank : first_rank;
            const char suited = first_suit == second_suit ? 's' : 'o';
            return std::string{ranks_asc[high_rank], ranks_asc[low_rank], suited};
        }

        [[nodiscard]] std::size_t matrix_index_for_class(const std::string& hand_class)
        {
            if (hand_class.size() < 2u) {
                return 0u;
            }
            const auto first = rank_order_index(hand_class[0]);
            const auto second = rank_order_index(hand_class[1]);
            if (first < 0 || second < 0 || first >= 13 || second >= 13) {
                return 0u;
            }
            if (hand_class.size() == 2u) {
                return static_cast<std::size_t>(first * 13 + first);
            }
            if (hand_class[2] == 's' || hand_class[2] == 'S') {
                return static_cast<std::size_t>(std::min(first, second) * 13 + std::max(first, second));
            }
            return static_cast<std::size_t>(std::max(first, second) * 13 + std::min(first, second));
        }

        [[nodiscard]] std::size_t hand_class_combo_count(const std::string& hand_class)
        {
            if (hand_class.size() == 2u) {
                return 6u;
            }
            if (hand_class.size() == 3u && (hand_class[2] == 's' || hand_class[2] == 'S')) {
                return 4u;
            }
            return 12u;
        }

        [[nodiscard]] std::vector<std::pair<std::string, card_mask>> board_cards(const std::vector<std::string>& board)
        {
            std::vector<std::pair<std::string, card_mask>> cards;
            cards.reserve(board.size());
            for (const auto& label : board) {
                auto parsed = cli::detail::parse_card_text(label);
                if (parsed) {
                    cards.emplace_back(label, card_mask{1} << *parsed);
                }
            }
            return cards;
        }

        [[nodiscard]] bool same_class_term(const std::string& term, const std::string_view hand_class)
        {
            const auto colon = term.find(':');
            const auto unweighted = colon == std::string::npos ? std::string_view{term} : std::string_view{term}.substr(0, colon);
            return trim_copy(unweighted) == hand_class;
        }

        [[nodiscard]] std::vector<std::string> named_class_terms(const std::string_view class_name)
        {
            const auto lower = [&class_name] {
                std::string out{class_name};
                std::ranges::transform(out, out.begin(), [](const unsigned char c) {
                    return static_cast<char>(std::tolower(c));
                });
                return out;
            }();

            std::vector<std::string> terms;
            if (lower == "pairs") {
                for (const auto rank : ranks_desc) {
                    terms.push_back(std::string{rank, rank});
                }
                return terms;
            }
            if (lower == "suited") {
                for (std::size_t hi = 0; hi < ranks_desc.size(); ++hi) {
                    for (std::size_t lo = hi + 1u; lo < ranks_desc.size(); ++lo) {
                        terms.push_back(std::string{ranks_desc[hi], ranks_desc[lo], 's'});
                    }
                }
                return terms;
            }
            if (lower == "offsuit") {
                for (std::size_t hi = 0; hi < ranks_desc.size(); ++hi) {
                    for (std::size_t lo = hi + 1u; lo < ranks_desc.size(); ++lo) {
                        terms.push_back(std::string{ranks_desc[hi], ranks_desc[lo], 'o'});
                    }
                }
                return terms;
            }
            if (lower == "broadways") {
                constexpr std::array broadway{'A', 'K', 'Q', 'J', 'T'};
                for (const auto rank : broadway) {
                    terms.push_back(std::string{rank, rank});
                }
                for (std::size_t hi = 0; hi < broadway.size(); ++hi) {
                    for (std::size_t lo = hi + 1u; lo < broadway.size(); ++lo) {
                        terms.push_back(std::string{broadway[hi], broadway[lo], 's'});
                        terms.push_back(std::string{broadway[hi], broadway[lo], 'o'});
                    }
                }
            }
            return terms;
        }

        [[nodiscard]] std::string weight_suffix(const combo_weight weight)
        {
            if (std::fabs(weight - 1.0f) <= 0.0001f) {
                return {};
            }
            std::ostringstream out;
            out << ':' << std::fixed << std::setprecision(3) << weight;
            auto text = out.str();
            while (!text.empty() && text.back() == '0') {
                text.pop_back();
            }
            if (!text.empty() && text.back() == '.') {
                text.pop_back();
            }
            return text;
        }

    }

    bool range_analysis::valid_for_solve() const noexcept
    {
        return !parse_issue.has_value() && metrics.live_combos > 0u;
    }

    std::array<std::string, 169> hand_class_labels()
    {
        std::array<std::string, 169> labels{};
        for (std::size_t row = 0; row < ranks_desc.size(); ++row) {
            for (std::size_t column = 0; column < ranks_desc.size(); ++column) {
                auto& label = labels[row * 13u + column];
                if (row == column) {
                    label = std::string{ranks_desc[row], ranks_desc[row]};
                } else if (row < column) {
                    label = std::string{ranks_desc[row], ranks_desc[column], 's'};
                } else {
                    label = std::string{ranks_desc[column], ranks_desc[row], 'o'};
                }
            }
        }
        return labels;
    }

    std::string range_parse_error_message(const range_parse_error_code code)
    {
        switch (code) {
            case range_parse_error_code::none: return "No error.";
            case range_parse_error_code::expected_rank: return "Expected a card rank.";
            case range_parse_error_code::expected_suit: return "Expected a card suit.";
            case range_parse_error_code::expected_term: return "Expected a range term.";
            case range_parse_error_code::expected_comma: return "Expected a comma between range terms.";
            case range_parse_error_code::invalid_exact_combo: return "Exact combo cannot use the same card twice.";
            case range_parse_error_code::invalid_plus: return "Plus syntax is not valid for exact combos.";
            case range_parse_error_code::invalid_range: return "Range endpoints must describe compatible hand classes.";
            case range_parse_error_code::invalid_weight: return "Weight must be a non-negative decimal number.";
        }
        return "Invalid range.";
    }

    range_analysis analyze_range(const std::string_view text, const std::vector<std::string>& board)
    {
        range_analysis analysis;
        analysis.source_text = std::string{text};
        const auto labels = hand_class_labels();
        for (std::size_t index = 0; index < labels.size(); ++index) {
            analysis.matrix[index].hand_class = labels[index];
            analysis.matrix[index].class_combos = hand_class_combo_count(labels[index]);
        }

        const auto parsed = parse_range(text);
        if (!parsed.ok()) {
            analysis.parse_issue = range_parse_issue{
                .position = parsed.error.position,
                .message = range_parse_error_message(parsed.error.code)
            };
            return analysis;
        }

        const auto blockers = board_cards(board);
        std::map<std::string, std::size_t> blocked_by_card;
        for (const auto& [label, mask] : blockers) {
            blocked_by_card[label] = 0u;
        }

        analysis.exact_combos.reserve(combination_count);
        for (combination_index combo = 0; combo < combination_count; ++combo) {
            const auto weight = parsed.range[combo];
            if (weight <= 0.0f) {
                continue;
            }

            ++analysis.metrics.combos_before_blockers;
            const auto combo_mask = combination_masks[combo];
            std::vector<std::string> combo_blockers;
            for (const auto& [label, mask] : blockers) {
                if ((combo_mask & mask) != 0u) {
                    combo_blockers.push_back(label);
                    ++blocked_by_card[label];
                }
            }

            const auto hand_class = hand_class_from_combo(combo);
            const bool live = combo_blockers.empty();
            if (live) {
                ++analysis.metrics.live_combos;
            }

            auto& cell = analysis.matrix[matrix_index_for_class(hand_class)];
            cell.combos += 1u;
            cell.live_combos += live ? 1u : 0u;
            cell.live_weight += live ? static_cast<double>(weight) : 0.0;
            cell.max_weight = std::max(cell.max_weight, weight);
            cell.selected = true;
            cell.blocked = cell.live_combos == 0u;

            analysis.exact_combos.push_back(range_combo_view{
                .combo = combo,
                .hand = cli::detail::hand_text_from_combo(combo),
                .hand_class = hand_class,
                .weight = weight,
                .live = live,
                .blocked_by = std::move(combo_blockers)
            });
        }

        analysis.metrics.percent_total_hands = static_cast<double>(analysis.metrics.live_combos) * 100.0
            / static_cast<double>(combination_count);
        for (const auto& [label, count] : blocked_by_card) {
            analysis.metrics.blocked_combos_by_card.emplace_back(label, count);
        }
        return analysis;
    }

    std::string normalized_exact_range_text(const range_analysis& analysis)
    {
        std::vector<std::string> terms;
        terms.reserve(analysis.exact_combos.size());
        for (const auto& combo : analysis.exact_combos) {
            terms.push_back(combo.hand + weight_suffix(combo.weight));
        }
        return join_range_terms(terms);
    }

    std::string set_hand_class_enabled(const std::string_view source, const std::string_view hand_class, const bool enabled)
    {
        if (!enabled) {
            const auto analysis = analyze_range(source, {});
            if (!analysis.parse_issue) {
                std::vector<std::string> terms;
                terms.reserve(analysis.exact_combos.size());
                for (const auto& combo : analysis.exact_combos) {
                    if (combo.hand_class != hand_class) {
                        terms.push_back(combo.hand + weight_suffix(combo.weight));
                    }
                }
                return join_range_terms(terms);
            }
        }

        auto terms = split_range_terms(source);
        terms.erase(std::remove_if(terms.begin(), terms.end(), [hand_class](const std::string& term) {
            return same_class_term(term, hand_class);
        }), terms.end());
        if (enabled) {
            terms.emplace_back(hand_class);
        }
        return join_range_terms(terms);
    }

    std::string add_named_class_selection(const std::string_view source, const std::string_view class_name)
    {
        auto terms = split_range_terms(source);
        for (const auto& term : named_class_terms(class_name)) {
            if (std::ranges::none_of(terms, [&term](const std::string& existing) {
                return same_class_term(existing, term);
            })) {
                terms.push_back(term);
            }
        }
        return join_range_terms(terms);
    }

}
