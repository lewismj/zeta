#include "viewmodels/spot_view_model.h"

#include <algorithm>
#include <array>
#include <set>
#include <sstream>

namespace zeta::holdem::ui::viewmodels {

    namespace {

        [[nodiscard]] std::string default_player_label(const std::size_t index)
        {
            static constexpr std::array labels{"BTN", "BB", "CO", "HJ", "SB", "UTG"};
            if (index < labels.size()) {
                return labels[index];
            }
            return "Seat " + std::to_string(index + 1u);
        }

        [[nodiscard]] bool is_valid_card_label(const std::string& label)
        {
            return cli::detail::parse_card_text(label).has_value();
        }

        [[nodiscard]] std::string board_text(const std::vector<std::string>& board)
        {
            std::ostringstream out;
            for (std::size_t i = 0; i < board.size(); ++i) {
                if (i != 0u) {
                    out << ' ';
                }
                out << board[i];
            }
            return out.str();
        }

        [[nodiscard]] std::string seat_label_or_index(const spot& source, const std::size_t seat)
        {
            if (seat < source.players.size() && !source.players[seat].empty()) {
                return source.players[seat];
            }
            return "Seat " + std::to_string(seat + 1u);
        }

        void add_template_defaults(spot& out)
        {
            out.ranges.assign(out.players.size(), "AA");
            out.stacks.assign(out.players.size(), 100.0);
            out.contributions.assign(out.players.size(), 0.0);
            out.gross_pot = 100.0;
            out.rake = 0.0;
            out.bet_fraction = 0.75;
            out.max_history = 8;
            out.public_state_id = 0;
            out.root_actor = 0;
            out.hero_seat = 0;
            out.samples_per_combo = 64;
        }

    }

    std::size_t board_card_count_for_street(const std::string_view street) noexcept
    {
        if (street == "flop") {
            return 3u;
        }
        if (street == "turn") {
            return 4u;
        }
        if (street == "river") {
            return 5u;
        }
        return 0u;
    }

    std::vector<std::string> deck_card_labels()
    {
        static constexpr std::array ranks{'A', 'K', 'Q', 'J', 'T', '9', '8', '7', '6', '5', '4', '3', '2'};
        static constexpr std::array suits{'s', 'h', 'd', 'c'};

        std::vector<std::string> cards;
        cards.reserve(ranks.size() * suits.size());
        for (const auto rank : ranks) {
            for (const auto suit : suits) {
                cards.push_back(std::string{rank, suit});
            }
        }
        return cards;
    }

    spot resize_player_count(spot source, const std::size_t player_count)
    {
        const auto clamped_count = std::clamp(player_count, cli::cli_min_players, cli::cli_max_players);
        const auto old_count = source.players.size();

        source.players.resize(clamped_count);
        for (std::size_t index = old_count; index < source.players.size(); ++index) {
            source.players[index] = default_player_label(index);
        }

        source.ranges.resize(clamped_count, "AA");
        source.stacks.resize(clamped_count, 100.0);
        source.contributions.resize(clamped_count, 0.0);
        if (source.root_actor >= clamped_count) {
            source.root_actor = static_cast<uint8_t>(clamped_count - 1u);
        }
        if (source.hero_seat >= clamped_count) {
            source.hero_seat = static_cast<uint8_t>(clamped_count - 1u);
        }
        return source;
    }

    spot make_template_spot(const spot_template_kind kind)
    {
        spot out;
        add_template_defaults(out);

        switch (kind) {
            case spot_template_kind::heads_up_river:
                out.players = {"BTN", "BB"};
                add_template_defaults(out);
                out.street = "river";
                out.board = {"As", "Kd", "7c", "4h", "2s"};
                out.contributions = {50.0, 50.0};
                out.root_actor = 0;
                out.hero_seat = 0;
                break;
            case spot_template_kind::three_way_flop:
                out.players = {"BTN", "SB", "BB"};
                add_template_defaults(out);
                out.street = "flop";
                out.board = {"As", "Kd", "7c"};
                out.contributions = {30.0, 35.0, 35.0};
                out.root_actor = 1;
                out.hero_seat = 0;
                break;
            case spot_template_kind::four_way_turn:
                out.players = {"BTN", "SB", "BB", "CO"};
                add_template_defaults(out);
                out.street = "turn";
                out.board = {"As", "Kd", "7c", "4h"};
                out.contributions = {25.0, 25.0, 25.0, 25.0};
                out.root_actor = 2;
                out.hero_seat = 0;
                break;
        }

        return out;
    }

    std::vector<spot_validation_issue> validate_structured_spot(const spot& source)
    {
        std::vector<spot_validation_issue> issues;
        const auto expected_board_size = board_card_count_for_street(source.street);
        if (expected_board_size == 0u) {
            issues.push_back({"street", "Street must be flop, turn, or river."});
        }
        if (source.board.size() != expected_board_size) {
            issues.push_back({"board", "Board card count must match the selected street."});
        }

        std::set<std::string> seen_cards;
        for (const auto& card : source.board) {
            if (!is_valid_card_label(card)) {
                issues.push_back({"board", "Board contains an invalid card label."});
                continue;
            }
            if (!seen_cards.insert(card).second) {
                issues.push_back({"board", "Board contains a duplicate card."});
            }
        }

        if (source.players.size() < cli::cli_min_players || source.players.size() > cli::cli_max_players) {
            issues.push_back({"players", "Player count must be between 2 and 6."});
        }
        if (source.ranges.size() != source.players.size()) {
            issues.push_back({"ranges", "Ranges array must match player count."});
        }
        if (source.stacks.size() != source.players.size()) {
            issues.push_back({"stacks", "Stacks array must match player count."});
        }
        if (source.contributions.size() != source.players.size()) {
            issues.push_back({"contributions", "Contributions array must match player count."});
        }
        if (source.root_actor >= source.players.size()) {
            issues.push_back({"root_actor", "Root actor must refer to an existing player."});
        }
        if (source.hero_seat >= source.players.size()) {
            issues.push_back({"hero_seat", "Hero seat must refer to an existing player."});
        }
        if (source.gross_pot <= 0.0) {
            issues.push_back({"gross_pot", "Gross pot must be positive."});
        }
        if (source.rake < 0.0 || source.rake > source.gross_pot) {
            issues.push_back({"rake", "Rake must be between 0 and gross pot."});
        }
        if (source.bet_fraction <= 0.0) {
            issues.push_back({"bet_fraction", "Bet fraction must be positive."});
        }
        if (source.samples_per_combo == 0u) {
            issues.push_back({"samples_per_combo", "Samples per combo must be positive."});
        }

        const auto per_seat_count = std::min({source.players.size(), source.stacks.size(), source.contributions.size()});
        for (std::size_t seat = 0; seat < per_seat_count; ++seat) {
            if (source.players[seat].empty()) {
                issues.push_back({"players", "Seat labels cannot be empty."});
            }
            if (source.stacks[seat] < 0.0) {
                issues.push_back({"stacks", "Stacks must be non-negative."});
            }
            if (source.contributions[seat] < 0.0) {
                issues.push_back({"contributions", "Contributions must be non-negative."});
            }
        }

        return issues;
    }

    std::string spot_summary_text(const spot& source, const bool has_artifact)
    {
        const auto root = seat_label_or_index(source, source.root_actor);
        const auto hero = seat_label_or_index(source, source.hero_seat);
        std::ostringstream out;
        out << source.players.size() << " players"
            << " | " << source.street
            << " | board " << (source.board.empty() ? "-" : board_text(source.board))
            << " | pot " << source.gross_pot
            << " | actor " << root
            << " | hero " << hero
            << " | " << (has_artifact ? "solved" : "unsolved");
        return out.str();
    }

}
