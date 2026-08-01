#pragma once

#include "board.h"
#include "cfr/betting/betting.h"
#include "cfr/solver/iteration.h"
#include "range_parser.h"
#include "terminal.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <cctype>
#include <charconv>
#include <chrono>
#include <cmath>
#include <cerrno>
#include <cstdint>
#include <cstdlib>
#include <expected>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <ios>
#include <limits>
#include <regex>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace zeta::holdem::cli {

    enum class cli_error_kind : uint8_t {
        io,
        parse,
        invalid_spot,
        invalid_artifact,
        solver
    };

    struct cli_error {
        cli_error_kind kind = cli_error_kind::parse;
        std::string message;
    };

    struct action_strategy {
        std::string action;
        double frequency = 0.0;
    };

    struct hand_strategy {
        std::string hand;
        std::vector<action_strategy> strategy;
        double ev = 0.0;
    };

    struct solver_metadata {
        std::string algorithm = "cfr+";
        uint64_t iterations = 0;
        std::string timestamp;
        std::string git_revision = "unknown";
    };

    struct solve_artifact {
        uint32_t schema_version = 1;
        std::string game = "holdem";
        std::string street = "river";
        std::array<std::string, 2> players{"BTN", "BB"};
        std::array<std::string, 5> board{};
        solver_metadata solver{};
        std::vector<hand_strategy> strategy;
    };

    struct solve_spot {
        std::array<std::string, 2> players{"BTN", "BB"};
        std::array<std::string, 5> board{};
        std::string oop_range = "AA";
        std::string ip_range = "AA";
        terminal_context<2> terminal{
            .gross_pot = 100.0,
            .rake = 0.0,
            .contribution = {50.0, 50.0}
        };
        std::array<utility, 2> stacks{100.0, 100.0};
        double bet_fraction = 0.75;
        uint16_t max_history = 8;
        uint32_t public_state_id = 0;
    };

    struct solve_runtime_options {
        std::string timestamp_utc;
        std::string git_revision = "unknown";
    };

    struct solve_timing {
        double graph_build_ms = 0.0;
        double cfr_iterations_ms = 0.0;
        double extraction_ms = 0.0;
    };

    struct solve_output {
        solve_artifact artifact;
        solve_timing timing;
    };

    namespace detail {

        [[nodiscard]] inline std::string to_string(const double value, const int precision = 6)
        {
            std::ostringstream os;
            os << std::fixed << std::setprecision(precision) << value;
            return os.str();
        }

        [[nodiscard]] inline std::string trim_trailing_zeros(const std::string_view text)
        {
            std::string out{text};
            while (!out.empty() && out.back() == '0') {
                out.pop_back();
            }
            if (!out.empty() && out.back() == '.') {
                out.pop_back();
            }
            if (out.empty()) {
                return "0";
            }
            return out;
        }

        [[nodiscard]] inline std::string json_number(const double value)
        {
            return trim_trailing_zeros(to_string(value, 8));
        }

        [[nodiscard]] inline std::string json_escape(const std::string_view input)
        {
            std::string out;
            out.reserve(input.size());
            for (const char c : input) {
                switch (c) {
                    case '\\': out += "\\\\"; break;
                    case '"':  out += "\\\""; break;
                    case '\n': out += "\\n"; break;
                    case '\r': out += "\\r"; break;
                    case '\t': out += "\\t"; break;
                    default:   out.push_back(c); break;
                }
            }
            return out;
        }

        [[nodiscard]] inline bool parse_double(const std::string_view text, double& out)
        {
            std::string owned{text};
            char* end = nullptr;
            errno = 0;
            const double value = std::strtod(owned.c_str(), &end);
            if (errno != 0 || end == owned.c_str() || static_cast<std::size_t>(end - owned.c_str()) != owned.size()) {
                return false;
            }
            if (!std::isfinite(value)) {
                return false;
            }
            out = value;
            return true;
        }

        [[nodiscard]] inline bool parse_u64(const std::string_view text, uint64_t& out)
        {
            const auto begin = text.data();
            const auto end = begin + text.size();
            uint64_t value = 0;
            const auto [ptr, ec] = std::from_chars(begin, end, value);
            if (ec != std::errc{} || ptr != end) {
                return false;
            }
            out = value;
            return true;
        }

        [[nodiscard]] inline bool parse_u32(const std::string_view text, uint32_t& out)
        {
            uint64_t value = 0;
            if (!parse_u64(text, value) || value > std::numeric_limits<uint32_t>::max()) {
                return false;
            }
            out = static_cast<uint32_t>(value);
            return true;
        }

        [[nodiscard]] inline bool parse_u16(const std::string_view text, uint16_t& out)
        {
            uint64_t value = 0;
            if (!parse_u64(text, value) || value > std::numeric_limits<uint16_t>::max()) {
                return false;
            }
            out = static_cast<uint16_t>(value);
            return true;
        }

        [[nodiscard]] inline std::string make_key_pattern(const std::string_view key, const std::string_view value_pattern)
        {
            std::string pattern;
            pattern.reserve(key.size() + value_pattern.size() + 32);
            pattern += "\"";
            pattern += key;
            pattern += "\"\\s*:\\s*";
            pattern += value_pattern;
            return pattern;
        }

        [[nodiscard]] inline bool extract_string_value(
            const std::string_view json,
            const std::string_view key,
            std::string& out)
        {
            const std::regex re{make_key_pattern(key, "\"([^\"]*)\"")};
            std::cmatch match;
            if (!std::regex_search(json.begin(), json.end(), match, re)) {
                return false;
            }
            out = match[1].str();
            return true;
        }

        [[nodiscard]] inline bool extract_number_value(
            const std::string_view json,
            const std::string_view key,
            std::string& out)
        {
            const std::regex re{make_key_pattern(key, "([-+0-9.eE]+)")};
            std::cmatch match;
            if (!std::regex_search(json.begin(), json.end(), match, re)) {
                return false;
            }
            out = match[1].str();
            return true;
        }

        [[nodiscard]] inline bool extract_array_body(
            const std::string_view json,
            const std::string_view key,
            std::string& body)
        {
            const auto key_pos = json.find(std::string{"\""} + std::string{key} + "\"");
            if (key_pos == std::string_view::npos) {
                return false;
            }
            const auto colon = json.find(':', key_pos);
            if (colon == std::string_view::npos) {
                return false;
            }
            const auto open = json.find('[', colon);
            if (open == std::string_view::npos) {
                return false;
            }
            int depth = 0;
            bool in_string = false;
            bool escaped = false;
            for (std::size_t i = open; i < json.size(); ++i) {
                const char c = json[i];
                if (in_string) {
                    if (escaped) {
                        escaped = false;
                    } else if (c == '\\') {
                        escaped = true;
                    } else if (c == '"') {
                        in_string = false;
                    }
                    continue;
                }
                if (c == '"') {
                    in_string = true;
                    continue;
                }
                if (c == '[') {
                    ++depth;
                } else if (c == ']') {
                    --depth;
                    if (depth == 0) {
                        body = std::string{json.substr(open + 1, i - open - 1)};
                        return true;
                    }
                }
            }
            return false;
        }

        [[nodiscard]] inline std::vector<std::string> parse_string_array(const std::string_view array_body)
        {
            std::vector<std::string> values;
            const std::string text{array_body};
            const std::regex token_re{"\"([^\"]*)\""};
            for (auto it = std::sregex_iterator(text.begin(), text.end(), token_re);
                 it != std::sregex_iterator{};
                 ++it) {
                values.push_back((*it)[1].str());
            }
            return values;
        }

        [[nodiscard]] inline int parse_rank_char(const char c) noexcept
        {
            const char upper = static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
            if (upper >= '2' && upper <= '9') {
                return upper - '2';
            }
            switch (upper) {
                case 'T': return 8;
                case 'J': return 9;
                case 'Q': return 10;
                case 'K': return 11;
                case 'A': return 12;
                default: return -1;
            }
        }

        [[nodiscard]] inline int parse_suit_char(const char c) noexcept
        {
            switch (static_cast<char>(std::tolower(static_cast<unsigned char>(c)))) {
                case 's': return 0;
                case 'h': return 1;
                case 'd': return 2;
                case 'c': return 3;
                default: return -1;
            }
        }

        [[nodiscard]] inline std::expected<card, cli_error> parse_card_text(const std::string_view text)
        {
            if (text.size() != 2) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Card must have length 2."});
            }
            const int rank = parse_rank_char(text[0]);
            const int suit = parse_suit_char(text[1]);
            if (rank < 0 || suit < 0) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Invalid card text: " + std::string{text}});
            }
            return static_cast<card>(suit * 13 + rank);
        }

        [[nodiscard]] inline char rank_char(const uint8_t rank)
        {
            static constexpr std::array<char, 13> chars{'2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'};
            return chars[rank];
        }

        [[nodiscard]] inline char suit_char(const uint8_t suit)
        {
            static constexpr std::array<char, 4> chars{'s', 'h', 'd', 'c'};
            return chars[suit];
        }

        [[nodiscard]] inline std::string card_text_from_id(const uint8_t id)
        {
            const auto rank = static_cast<uint8_t>(id % 13);
            const auto suit = static_cast<uint8_t>(id / 13);
            std::string out;
            out.push_back(rank_char(rank));
            out.push_back(suit_char(suit));
            return out;
        }

        [[nodiscard]] inline std::string hand_text_from_combo(const combination_index combo)
        {
            const auto mask = combination_mask(combo);
            std::array<uint8_t, 2> cards{};
            std::size_t count = 0;
            for (uint8_t id = 0; id < 52; ++id) {
                if ((mask & (card_mask{1} << id)) != 0) {
                    assert(count < cards.size());
                    cards[count++] = id;
                }
            }
            assert(count == 2u);

            auto first = cards[0];
            auto second = cards[1];
            const auto first_rank = first % 13;
            const auto second_rank = second % 13;
            if (first_rank < second_rank || (first_rank == second_rank && first > second)) {
                std::swap(first, second);
            }
            return card_text_from_id(first) + card_text_from_id(second);
        }

        [[nodiscard]] inline std::expected<board, cli_error> board_from_cards(const std::array<std::string, 5>& cards)
        {
            board river{};
            std::set<card> seen;
            for (const auto& text : cards) {
                auto parsed = parse_card_text(text);
                if (!parsed) {
                    return std::unexpected(parsed.error());
                }
                if (seen.contains(*parsed)) {
                    return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Duplicate board card: " + text});
                }
                seen.insert(*parsed);
                river.add(card_mask{1} << *parsed);
            }
            if (river.size() != 5) {
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Board must contain exactly 5 unique cards."});
            }
            return river;
        }

        [[nodiscard]] inline std::string now_utc_iso8601()
        {
            const auto now = std::chrono::system_clock::now();
            const auto time = std::chrono::system_clock::to_time_t(now);
            std::tm tm_utc{};
#if defined(_WIN32)
            gmtime_s(&tm_utc, &time);
#else
            gmtime_r(&time, &tm_utc);
#endif
            std::ostringstream os;
            os << std::put_time(&tm_utc, "%Y-%m-%dT%H:%M:%SZ");
            return os.str();
        }

        [[nodiscard]] inline std::string action_label(
            const cfr::betting_action& action,
            const utility gross_pot)
        {
            using enum cfr::betting_action_kind;
            switch (action.kind) {
                case fold: return "fold";
                case check: return "check";
                case call: return "call";
                case all_in: return "all_in";
                case bet:
                case raise: {
                    const auto safe_pot = std::max(1.0, gross_pot);
                    const auto pct = static_cast<int>(std::llround((action.amount / safe_pot) * 100.0));
                    return (action.kind == bet ? "bet_" : "raise_") + std::to_string(pct);
                }
            }
            return "action";
        }

        [[nodiscard]] inline std::expected<std::array<combination_index, 2>, cli_error> choose_combo_pair(
            const river_terminal_cache& cache,
            const river_reach_index& oop,
            const river_reach_index& ip)
        {
            for (uint16_t oop_offset = 0; oop_offset < oop.active_count; ++oop_offset) {
                const auto oop_combo = oop.active_indices[oop_offset];
                const auto oop_mask = cache.masks[oop_combo];
                for (uint16_t ip_offset = 0; ip_offset < ip.active_count; ++ip_offset) {
                    const auto ip_combo = ip.active_indices[ip_offset];
                    if ((oop_mask & cache.masks[ip_combo]) == 0) {
                        return std::array<combination_index, 2>{oop_combo, ip_combo};
                    }
                }
            }
            return std::unexpected(cli_error{cli_error_kind::solver, "No disjoint live OOP/IP combo pair found."});
        }

        [[nodiscard]] inline std::expected<void, cli_error> parse_range_checked(
            const std::string_view text,
            hand_range& out,
            const char* label)
        {
            const auto parsed = parse_range(text);
            if (!parsed.ok()) {
                return std::unexpected(cli_error{
                    cli_error_kind::invalid_spot,
                    std::string{"Invalid "} + label + " range."
                });
            }
            out = parsed.range;
            return {};
        }
    }

    [[nodiscard]] inline std::expected<std::string, cli_error> read_file_text(const std::filesystem::path& path)
    {
        std::ifstream input(path, std::ios::binary);
        if (!input) {
            return std::unexpected(cli_error{
                cli_error_kind::io,
                "Failed to open file: " + path.string()
            });
        }
        std::ostringstream buffer;
        buffer << input.rdbuf();
        if (!input.good() && !input.eof()) {
            return std::unexpected(cli_error{
                cli_error_kind::io,
                "Failed to read file: " + path.string()
            });
        }
        return buffer.str();
    }

    [[nodiscard]] inline std::expected<void, cli_error> write_file_text(
        const std::filesystem::path& path,
        const std::string_view text)
    {
        std::ofstream output(path, std::ios::binary | std::ios::trunc);
        if (!output) {
            return std::unexpected(cli_error{
                cli_error_kind::io,
                "Failed to open output file: " + path.string()
            });
        }
        output.write(text.data(), static_cast<std::streamsize>(text.size()));
        if (!output) {
            return std::unexpected(cli_error{
                cli_error_kind::io,
                "Failed to write output file: " + path.string()
            });
        }
        return {};
    }

    [[nodiscard]] inline std::expected<solve_spot, cli_error> parse_spot_json(const std::string_view json)
    {
        solve_spot spot{};

        std::string board_body;
        if (!detail::extract_array_body(json, "board", board_body)) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Spot JSON missing board array."});
        }
        const auto board_values = detail::parse_string_array(board_body);
        if (board_values.size() != 5u) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Board array must have exactly 5 cards."});
        }
        for (std::size_t i = 0; i < board_values.size(); ++i) {
            spot.board[i] = board_values[i];
        }

        std::string players_body;
        if (detail::extract_array_body(json, "players", players_body)) {
            const auto players = detail::parse_string_array(players_body);
            if (players.size() != 2u) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Players array must contain exactly 2 labels."});
            }
            spot.players[0] = players[0];
            spot.players[1] = players[1];
        }

        std::string oop_range;
        if (detail::extract_string_value(json, "oop_range", oop_range)) {
            spot.oop_range = oop_range;
        }
        std::string ip_range;
        if (detail::extract_string_value(json, "ip_range", ip_range)) {
            spot.ip_range = ip_range;
        }

        std::string number_text;
        if (detail::extract_number_value(json, "gross_pot", number_text)) {
            if (!detail::parse_double(number_text, spot.terminal.gross_pot)) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Invalid gross_pot value."});
            }
        }
        if (detail::extract_number_value(json, "rake", number_text)) {
            if (!detail::parse_double(number_text, spot.terminal.rake)) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Invalid rake value."});
            }
        }
        if (detail::extract_number_value(json, "oop_contribution", number_text)) {
            if (!detail::parse_double(number_text, spot.terminal.contribution[0])) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Invalid oop_contribution value."});
            }
        }
        if (detail::extract_number_value(json, "ip_contribution", number_text)) {
            if (!detail::parse_double(number_text, spot.terminal.contribution[1])) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Invalid ip_contribution value."});
            }
        }
        if (detail::extract_number_value(json, "oop_stack", number_text)) {
            if (!detail::parse_double(number_text, spot.stacks[0])) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Invalid oop_stack value."});
            }
        }
        if (detail::extract_number_value(json, "ip_stack", number_text)) {
            if (!detail::parse_double(number_text, spot.stacks[1])) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Invalid ip_stack value."});
            }
        }
        if (detail::extract_number_value(json, "bet_fraction", number_text)) {
            if (!detail::parse_double(number_text, spot.bet_fraction)) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Invalid bet_fraction value."});
            }
        }
        if (detail::extract_number_value(json, "max_history", number_text)) {
            if (!detail::parse_u16(number_text, spot.max_history)) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Invalid max_history value."});
            }
        }
        if (detail::extract_number_value(json, "public_state_id", number_text)) {
            if (!detail::parse_u32(number_text, spot.public_state_id)) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Invalid public_state_id value."});
            }
        }

        if (spot.terminal.gross_pot <= 0.0) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "gross_pot must be positive."});
        }
        if (spot.terminal.rake < 0.0 || spot.terminal.rake > spot.terminal.gross_pot) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "rake must be in [0, gross_pot]."});
        }
        if (spot.bet_fraction <= 0.0) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "bet_fraction must be positive."});
        }
        if (spot.stacks[0] < 0.0 || spot.stacks[1] < 0.0) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Stacks must be non-negative."});
        }
        if (spot.terminal.contribution[0] < 0.0 || spot.terminal.contribution[1] < 0.0) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Contributions must be non-negative."});
        }

        auto board_result = detail::board_from_cards(spot.board);
        if (!board_result) {
            return std::unexpected(board_result.error());
        }
        return spot;
    }

    [[nodiscard]] inline std::string serialize_artifact_json(const solve_artifact& artifact)
    {
        std::ostringstream os;
        os << "{\n";
        os << "  \"schema_version\": " << artifact.schema_version << ",\n";
        os << "  \"game\": \"" << detail::json_escape(artifact.game) << "\",\n";
        os << "  \"street\": \"" << detail::json_escape(artifact.street) << "\",\n";
        os << "  \"players\": [\"" << detail::json_escape(artifact.players[0]) << "\", \"" << detail::json_escape(artifact.players[1]) << "\"],\n";
        os << "  \"board\": [";
        for (std::size_t i = 0; i < artifact.board.size(); ++i) {
            if (i != 0u) {
                os << ", ";
            }
            os << "\"" << detail::json_escape(artifact.board[i]) << "\"";
        }
        os << "],\n";
        os << "  \"solver\": {\n";
        os << "    \"algorithm\": \"" << detail::json_escape(artifact.solver.algorithm) << "\",\n";
        os << "    \"iterations\": " << artifact.solver.iterations << ",\n";
        os << "    \"timestamp\": \"" << detail::json_escape(artifact.solver.timestamp) << "\",\n";
        os << "    \"git_revision\": \"" << detail::json_escape(artifact.solver.git_revision) << "\"\n";
        os << "  },\n";
        os << "  \"strategy\": [\n";
        for (std::size_t i = 0; i < artifact.strategy.size(); ++i) {
            const auto& row = artifact.strategy[i];
            os << "    {\n";
            os << "      \"hand\": \"" << detail::json_escape(row.hand) << "\",\n";
            os << "      \"strategy\": [\n";
            for (std::size_t j = 0; j < row.strategy.size(); ++j) {
                const auto& action = row.strategy[j];
                os << "        {\"action\": \"" << detail::json_escape(action.action)
                   << "\", \"frequency\": " << detail::json_number(action.frequency) << "}";
                if (j + 1u != row.strategy.size()) {
                    os << ",";
                }
                os << "\n";
            }
            os << "      ],\n";
            os << "      \"ev\": " << detail::json_number(row.ev) << "\n";
            os << "    }";
            if (i + 1u != artifact.strategy.size()) {
                os << ",";
            }
            os << "\n";
        }
        os << "  ]\n";
        os << "}\n";
        return os.str();
    }

    [[nodiscard]] inline std::expected<solve_artifact, cli_error> parse_artifact_json(const std::string_view json)
    {
        solve_artifact artifact{};
        std::string number_text;
        if (!detail::extract_number_value(json, "schema_version", number_text)
            || !detail::parse_u32(number_text, artifact.schema_version)) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Invalid or missing schema_version."});
        }
        if (!detail::extract_string_value(json, "game", artifact.game)) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Missing game field."});
        }
        if (!detail::extract_string_value(json, "street", artifact.street)) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Missing street field."});
        }

        std::string players_body;
        if (!detail::extract_array_body(json, "players", players_body)) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Missing players array."});
        }
        {
            const auto players = detail::parse_string_array(players_body);
            if (players.size() != 2u) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Players array must have 2 labels."});
            }
            artifact.players[0] = players[0];
            artifact.players[1] = players[1];
        }

        std::string board_body;
        if (!detail::extract_array_body(json, "board", board_body)) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Missing board array."});
        }
        {
            const auto board_cards = detail::parse_string_array(board_body);
            if (board_cards.size() != 5u) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Board must have exactly 5 cards."});
            }
            for (std::size_t i = 0; i < board_cards.size(); ++i) {
                artifact.board[i] = board_cards[i];
            }
        }

        if (!detail::extract_string_value(json, "algorithm", artifact.solver.algorithm)) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Missing solver.algorithm field."});
        }
        if (!detail::extract_number_value(json, "iterations", number_text)
            || !detail::parse_u64(number_text, artifact.solver.iterations)) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Invalid solver.iterations field."});
        }
        if (!detail::extract_string_value(json, "timestamp", artifact.solver.timestamp)) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Missing solver.timestamp field."});
        }
        if (!detail::extract_string_value(json, "git_revision", artifact.solver.git_revision)) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Missing solver.git_revision field."});
        }

        std::string strategy_array;
        if (!detail::extract_array_body(json, "strategy", strategy_array)) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Missing strategy array."});
        }
        const std::regex row_re{
            R"row(\{\s*"hand"\s*:\s*"([^"]+)"\s*,\s*"strategy"\s*:\s*\[([\s\S]*?)\]\s*,\s*"ev"\s*:\s*([-+0-9.eE]+)\s*\})row"
        };
        const std::regex action_re{
            R"act(\{\s*"action"\s*:\s*"([^"]+)"\s*,\s*"frequency"\s*:\s*([-+0-9.eE]+)\s*\})act"
        };

        for (auto row_it = std::sregex_iterator(strategy_array.begin(), strategy_array.end(), row_re);
             row_it != std::sregex_iterator{};
             ++row_it) {
            hand_strategy row;
            row.hand = (*row_it)[1].str();
            const auto action_body = (*row_it)[2].str();
            const auto ev_text = (*row_it)[3].str();
            if (!detail::parse_double(ev_text, row.ev)) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Invalid EV value in strategy row."});
            }

            for (auto action_it = std::sregex_iterator(action_body.begin(), action_body.end(), action_re);
                 action_it != std::sregex_iterator{};
                 ++action_it) {
                action_strategy action{};
                action.action = (*action_it)[1].str();
                const auto frequency_text = (*action_it)[2].str();
                if (!detail::parse_double(frequency_text, action.frequency)) {
                    return std::unexpected(cli_error{cli_error_kind::parse, "Invalid action frequency value."});
                }
                row.strategy.push_back(std::move(action));
            }

            if (row.strategy.empty()) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Strategy row has no actions."});
            }
            artifact.strategy.push_back(std::move(row));
        }

        return artifact;
    }

    [[nodiscard]] inline std::expected<void, cli_error> validate_artifact(const solve_artifact& artifact)
    {
        if (artifact.schema_version != 1u) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Unsupported schema_version."});
        }
        if (artifact.game != "holdem") {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "game must be \"holdem\"."});
        }
        if (artifact.street != "river") {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "street must be \"river\"."});
        }
        auto board_result = detail::board_from_cards(artifact.board);
        if (!board_result) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, board_result.error().message});
        }

        std::set<std::string> seen_hands;
        for (const auto& row : artifact.strategy) {
            if (row.strategy.empty()) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Strategy row must contain at least one action."});
            }
            if (!seen_hands.insert(row.hand).second) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Duplicate hand entry: " + row.hand});
            }
            const auto parsed_hand = parse_range(row.hand);
            if (!parsed_hand.ok()) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Invalid hand text: " + row.hand});
            }
            std::size_t non_zero = 0;
            for (const auto weight : parsed_hand.range.weights) {
                if (weight != 0.0f) {
                    ++non_zero;
                }
            }
            if (non_zero != 1u) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Hand must represent exactly one combo: " + row.hand});
            }

            double sum = 0.0;
            for (const auto& action : row.strategy) {
                if (!std::isfinite(action.frequency) || action.frequency < 0.0 || action.frequency > 1.0) {
                    return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Action frequency out of [0,1] for hand: " + row.hand});
                }
                sum += action.frequency;
            }
            if (std::abs(sum - 1.0) > 1.0e-3) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Action frequencies must sum to 1 for hand: " + row.hand});
            }
            if (!std::isfinite(row.ev)) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "EV must be finite for hand: " + row.hand});
            }
        }

        return {};
    }

    [[nodiscard]] inline std::expected<solve_output, cli_error> solve_spot(
        const solve_spot& spot,
        const uint64_t iterations,
        const solve_runtime_options& runtime = {})
    {
        auto river_result = detail::board_from_cards(spot.board);
        if (!river_result) {
            return std::unexpected(river_result.error());
        }
        const auto river = *river_result;

        hand_range oop_range;
        if (auto parse = detail::parse_range_checked(spot.oop_range, oop_range, "oop"); !parse) {
            return std::unexpected(parse.error());
        }
        hand_range ip_range;
        if (auto parse = detail::parse_range_checked(spot.ip_range, ip_range, "ip"); !parse) {
            return std::unexpected(parse.error());
        }
        oop_range.remove_dead(river.mask);
        ip_range.remove_dead(river.mask);

        const auto cache = make_river_terminal_cache(river);
        const auto oop_reach = make_reach_vector(oop_range);
        const auto ip_reach = make_reach_vector(ip_range);
        const std::array<river_reach_index, 2> reach_indices{
            make_river_reach_index(cache, oop_reach),
            make_river_reach_index(cache, ip_reach)
        };
        if (reach_indices[0].active_count == 0u || reach_indices[1].active_count == 0u) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Board blockers removed all combos from one or both ranges."});
        }

        solve_output output{};
        const auto graph_begin = std::chrono::steady_clock::now();

        cfr::holdem_betting_graph_config<2> config{};
        config.street = cfr::solver::holdem_street::river;
        config.initial_stacks = spot.stacks;
        config.initial_committed = spot.terminal.contribution;
        config.root_actor = 0;
        config.abstraction.fixed_pot_fractions = {spot.bet_fraction};
        config.abstraction.geometric_size_count = 1;
        config.abstraction.stack_ratio_buckets = {spot.bet_fraction};
        config.abstraction.max_raises_per_street = 1;
        config.max_history = spot.max_history;
        config.public_state_id = spot.public_state_id;

        auto lowered = cfr::lower_betting_tree_to_graph(config);
        if (!lowered) {
            return std::unexpected(cli_error{
                cli_error_kind::solver,
                "Failed to lower betting tree to CFR graph."
            });
        }
        auto layout_result = cfr::make_action_table_layout(lowered->graph);
        if (!layout_result) {
            return std::unexpected(cli_error{
                cli_error_kind::solver,
                "Failed to build CFR action layout."
            });
        }
        cfr::regret_table regrets(*layout_result);
        cfr::strategy_sum_table strategy_sums(*layout_result);
        auto context = cfr::solver::make_cfr_solver_context<2>(
            lowered->graph,
            lowered->annotations,
            *layout_result,
            regrets,
            strategy_sums);

        auto combo_pair = detail::choose_combo_pair(cache, reach_indices[0], reach_indices[1]);
        if (!combo_pair) {
            return std::unexpected(combo_pair.error());
        }
        context.terminal_provider = cfr::solver::make_terminal_state_provider<2>(
            cache,
            reach_indices,
            lowered->terminal_leaves,
            lowered->terminal_states.view(),
            *combo_pair);

        output.timing.graph_build_ms = std::chrono::duration<double, std::milli>(
            std::chrono::steady_clock::now() - graph_begin).count();

        const auto iter_begin = std::chrono::steady_clock::now();
        std::array<cfr::traversal::worker_context, 1> workers{};
        for (uint64_t i = 0; i < iterations; ++i) {
            auto oop_result = cfr::solver::run_cfr_iteration(
                context,
                cfr::solver::iteration_config{
                    .variant = cfr::solver::cfr_variant::cfr_plus,
                    .iteration = i,
                    .updating_player = 0
                },
                std::span<cfr::traversal::worker_context>{workers});
            if (!oop_result) {
                return std::unexpected(cli_error{cli_error_kind::solver, "CFR iteration failed for OOP update."});
            }

            auto ip_result = cfr::solver::run_cfr_iteration(
                context,
                cfr::solver::iteration_config{
                    .variant = cfr::solver::cfr_variant::cfr_plus,
                    .iteration = i,
                    .updating_player = 1
                },
                std::span<cfr::traversal::worker_context>{workers});
            if (!ip_result) {
                return std::unexpected(cli_error{cli_error_kind::solver, "CFR iteration failed for IP update."});
            }
        }
        output.timing.cfr_iterations_ms = std::chrono::duration<double, std::milli>(
            std::chrono::steady_clock::now() - iter_begin).count();

        const auto extraction_begin = std::chrono::steady_clock::now();
        const auto root_infoset = lowered->graph.infoset_id[lowered->graph.root_node];
        const auto root_sums = strategy_sums.infoset_sums(root_infoset);
        if (root_sums.empty()) {
            return std::unexpected(cli_error{cli_error_kind::solver, "Root infoset has no actions."});
        }

        auto initial_state = cfr::make_initial_betting_state(config);
        const auto root_actions = cfr::legal_betting_actions(initial_state, config.abstraction);
        if (root_actions.size() != root_sums.size()) {
            return std::unexpected(cli_error{cli_error_kind::solver, "Root action shape does not match strategy table shape."});
        }

        double sum = 0.0;
        for (const auto value : root_sums) {
            if (value > 0.0f) {
                sum += value;
            }
        }
        std::vector<action_strategy> root_strategy;
        root_strategy.reserve(root_sums.size());
        const auto uniform = 1.0 / static_cast<double>(root_sums.size());
        for (std::size_t action_index = 0; action_index < root_sums.size(); ++action_index) {
            const auto frequency = sum > 0.0
                ? static_cast<double>(std::max(root_sums[action_index], 0.0f)) / sum
                : uniform;
            root_strategy.push_back(action_strategy{
                .action = detail::action_label(root_actions[action_index], spot.terminal.gross_pot),
                .frequency = frequency
            });
        }

        const auto showdown = evaluate_showdown(cache, reach_indices[0], reach_indices[1], spot.terminal);
        solve_artifact artifact{};
        artifact.players = spot.players;
        artifact.board = spot.board;
        artifact.solver.algorithm = "cfr+";
        artifact.solver.iterations = iterations;
        artifact.solver.timestamp = runtime.timestamp_utc.empty() ? detail::now_utc_iso8601() : runtime.timestamp_utc;
        artifact.solver.git_revision = runtime.git_revision;

        for (combination_index combo = 0; combo < combination_count; ++combo) {
            if (oop_reach[combo] <= 0.0f) {
                continue;
            }
            artifact.strategy.push_back(hand_strategy{
                .hand = detail::hand_text_from_combo(combo),
                .strategy = root_strategy,
                .ev = showdown.values[player::oop][combo]
            });
        }
        output.artifact = std::move(artifact);

        output.timing.extraction_ms = std::chrono::duration<double, std::milli>(
            std::chrono::steady_clock::now() - extraction_begin).count();

        if (auto validation = validate_artifact(output.artifact); !validation) {
            return std::unexpected(validation.error());
        }
        return output;
    }

    [[nodiscard]] inline std::string format_dump(const solve_artifact& artifact)
    {
        std::vector<std::string> actions;
        if (!artifact.strategy.empty()) {
            actions.reserve(artifact.strategy.front().strategy.size());
            for (const auto& action : artifact.strategy.front().strategy) {
                actions.push_back(action.action);
            }
        }

        const int hand_width = 8;
        const int action_width = 10;
        const int ev_width = 10;
        std::ostringstream os;
        os << std::left << std::setw(hand_width) << "Hand";
        for (const auto& action : actions) {
            os << std::setw(action_width) << action;
        }
        os << std::setw(ev_width) << "EV" << "\n";
        os << std::string(hand_width + static_cast<int>(actions.size()) * action_width + ev_width, '-') << "\n";

        for (const auto& row : artifact.strategy) {
            os << std::left << std::setw(hand_width) << row.hand;
            for (const auto& action_name : actions) {
                double frequency = 0.0;
                for (const auto& action : row.strategy) {
                    if (action.action == action_name) {
                        frequency = action.frequency;
                        break;
                    }
                }
                const auto pct = static_cast<int>(std::llround(frequency * 100.0));
                os << std::setw(action_width) << (std::to_string(pct) + "%");
            }
            os << std::showpos << std::fixed << std::setprecision(2) << std::setw(ev_width) << row.ev << std::noshowpos << "\n";
        }
        return os.str();
    }

}
