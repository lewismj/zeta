#include "cli/solve_cli.h"

#include <boost/json.hpp>

#include <limits>

namespace zeta::holdem::cli {

    namespace {

        namespace json = boost::json;

        [[nodiscard]] std::string key_name(const std::string_view key)
        {
            return std::string{key};
        }

        [[nodiscard]] const json::value* find_value(const json::object& object, const std::string_view key)
        {
            return object.if_contains(json::string_view{key.data(), key.size()});
        }

        [[nodiscard]] std::expected<json::object, cli_error> parse_object(const std::string_view text, const char* label)
        {
            boost::system::error_code ec;
            auto value = json::parse(text, ec);
            if (ec) {
                return std::unexpected(cli_error{cli_error_kind::parse, ec.message()});
            }
            if (!value.is_object()) {
                return std::unexpected(cli_error{cli_error_kind::parse, std::string{label} + " JSON must be an object."});
            }
            return std::move(value.as_object());
        }

        [[nodiscard]] std::expected<std::string, cli_error> string_value(
            const json::value& value,
            const std::string_view key)
        {
            if (!value.is_string()) {
                return std::unexpected(cli_error{cli_error_kind::parse, key_name(key) + " must be a string."});
            }
            const auto& string = value.as_string();
            return std::string{string.data(), string.size()};
        }

        [[nodiscard]] std::expected<std::string, cli_error> required_string(
            const json::object& object,
            const std::string_view key)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Missing " + key_name(key) + " field."});
            }
            return string_value(*value, key);
        }

        [[nodiscard]] std::expected<std::string, cli_error> optional_string(
            const json::object& object,
            const std::string_view key,
            std::string fallback)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return fallback;
            }
            return string_value(*value, key);
        }

        [[nodiscard]] std::expected<double, cli_error> number_value(
            const json::value& value,
            const std::string_view key)
        {
            double out = 0.0;
            if (value.is_double()) {
                out = value.as_double();
            } else if (value.is_int64()) {
                out = static_cast<double>(value.as_int64());
            } else if (value.is_uint64()) {
                out = static_cast<double>(value.as_uint64());
            } else {
                return std::unexpected(cli_error{cli_error_kind::parse, key_name(key) + " must be a number."});
            }
            if (!std::isfinite(out)) {
                return std::unexpected(cli_error{cli_error_kind::parse, key_name(key) + " must be finite."});
            }
            return out;
        }

        [[nodiscard]] std::expected<double, cli_error> optional_double(
            const json::object& object,
            const std::string_view key,
            const double fallback)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return fallback;
            }
            return number_value(*value, key);
        }

        [[nodiscard]] std::expected<uint64_t, cli_error> uint64_value(
            const json::value& value,
            const std::string_view key)
        {
            if (value.is_uint64()) {
                return value.as_uint64();
            }
            if (value.is_int64()) {
                const auto raw = value.as_int64();
                if (raw < 0) {
                    return std::unexpected(cli_error{cli_error_kind::parse, key_name(key) + " must be non-negative."});
                }
                return static_cast<uint64_t>(raw);
            }
            if (value.is_double()) {
                const auto raw = value.as_double();
                if (!std::isfinite(raw)
                    || raw < 0.0
                    || raw > static_cast<double>(std::numeric_limits<uint64_t>::max())
                    || static_cast<double>(static_cast<uint64_t>(raw)) != raw) {
                    return std::unexpected(cli_error{cli_error_kind::parse, key_name(key) + " must be an unsigned integer."});
                }
                return static_cast<uint64_t>(raw);
            }
            return std::unexpected(cli_error{cli_error_kind::parse, key_name(key) + " must be an unsigned integer."});
        }

        template <typename T>
        [[nodiscard]] std::expected<T, cli_error> optional_uint(
            const json::object& object,
            const std::string_view key,
            const T fallback)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return fallback;
            }
            auto parsed = uint64_value(*value, key);
            if (!parsed) {
                return std::unexpected(parsed.error());
            }
            if (*parsed > static_cast<uint64_t>(std::numeric_limits<T>::max())) {
                return std::unexpected(cli_error{cli_error_kind::parse, key_name(key) + " is out of range."});
            }
            return static_cast<T>(*parsed);
        }

        template <typename T>
        [[nodiscard]] std::expected<T, cli_error> required_uint(
            const json::object& object,
            const std::string_view key)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Missing " + key_name(key) + " field."});
            }
            auto parsed = uint64_value(*value, key);
            if (!parsed) {
                return std::unexpected(parsed.error());
            }
            if (*parsed > static_cast<uint64_t>(std::numeric_limits<T>::max())) {
                return std::unexpected(cli_error{cli_error_kind::parse, key_name(key) + " is out of range."});
            }
            return static_cast<T>(*parsed);
        }

        [[nodiscard]] std::expected<std::vector<std::string>, cli_error> string_array(
            const json::value& value,
            const std::string_view key)
        {
            if (!value.is_array()) {
                return std::unexpected(cli_error{cli_error_kind::parse, key_name(key) + " must be an array."});
            }
            std::vector<std::string> out;
            out.reserve(value.as_array().size());
            for (const auto& element : value.as_array()) {
                auto parsed = string_value(element, key);
                if (!parsed) {
                    return std::unexpected(parsed.error());
                }
                out.push_back(std::move(*parsed));
            }
            return out;
        }

        [[nodiscard]] std::expected<std::vector<std::string>, cli_error> required_string_array(
            const json::object& object,
            const std::string_view key)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Missing " + key_name(key) + " array."});
            }
            return string_array(*value, key);
        }

        [[nodiscard]] std::expected<std::vector<std::string>, cli_error> optional_string_array(
            const json::object& object,
            const std::string_view key,
            std::vector<std::string> fallback)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return fallback;
            }
            return string_array(*value, key);
        }

        [[nodiscard]] std::expected<std::vector<utility>, cli_error> number_array(
            const json::value& value,
            const std::string_view key)
        {
            if (!value.is_array()) {
                return std::unexpected(cli_error{cli_error_kind::parse, key_name(key) + " must be an array."});
            }
            std::vector<utility> out;
            out.reserve(value.as_array().size());
            for (const auto& element : value.as_array()) {
                auto parsed = number_value(element, key);
                if (!parsed) {
                    return std::unexpected(parsed.error());
                }
                out.push_back(*parsed);
            }
            return out;
        }

        [[nodiscard]] std::expected<std::vector<utility>, cli_error> optional_number_array(
            const json::object& object,
            const std::string_view key,
            std::vector<utility> fallback)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return fallback;
            }
            return number_array(*value, key);
        }

        [[nodiscard]] json::array string_array_json(const std::vector<std::string>& values)
        {
            json::array out;
            out.reserve(values.size());
            for (const auto& value : values) {
                out.emplace_back(value);
            }
            return out;
        }

        [[nodiscard]] json::array number_array_json(const std::vector<utility>& values)
        {
            json::array out;
            out.reserve(values.size());
            for (const auto value : values) {
                out.emplace_back(value);
            }
            return out;
        }

        [[nodiscard]] std::expected<void, cli_error> validate_spot_fields(const struct solve_spot& spot)
        {
            auto parsed_street = detail::parse_holdem_street(spot.street);
            if (!parsed_street) {
                return std::unexpected(parsed_street.error());
            }
            if (spot.board.size() != detail::board_size_for_street(*parsed_street)) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Board card count must match street."});
            }
            if (spot.players.size() < cli_min_players || spot.players.size() > cli_max_players) {
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Player count must be between 2 and 7."});
            }
            if (spot.ranges.size() != spot.players.size()) {
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Ranges array must match player count."});
            }
            if (spot.stacks.size() != spot.players.size()) {
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Stacks array must match player count."});
            }
            if (spot.contributions.size() != spot.players.size()) {
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Contributions array must match player count."});
            }
            if (spot.root_actor >= spot.players.size()) {
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "root_actor is out of range."});
            }
            if (spot.hero_seat >= spot.players.size()) {
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "hero_seat is out of range."});
            }
            if (spot.samples_per_combo == 0) {
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "samples_per_combo must be positive."});
            }
            if (spot.gross_pot <= 0.0) {
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "gross_pot must be positive."});
            }
            if (spot.rake < 0.0 || spot.rake > spot.gross_pot) {
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "rake must be in [0, gross_pot]."});
            }
            if (spot.bet_fraction <= 0.0) {
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "bet_fraction must be positive."});
            }
            for (std::size_t seat = 0; seat < spot.players.size(); ++seat) {
                if (spot.stacks[seat] < 0.0) {
                    return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Stacks must be non-negative."});
                }
                if (spot.contributions[seat] < 0.0) {
                    return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Contributions must be non-negative."});
                }
            }

            auto board_result = detail::board_from_cards(spot.board, *parsed_street);
            if (!board_result) {
                return std::unexpected(board_result.error());
            }
            return {};
        }

        [[nodiscard]] std::expected<action_strategy, cli_error> parse_action_strategy(const json::value& value)
        {
            if (!value.is_object()) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Strategy action must be an object."});
            }
            const auto& object = value.as_object();
            action_strategy action{};
            auto action_text = required_string(object, "action");
            if (!action_text) {
                return std::unexpected(action_text.error());
            }
            action.action = std::move(*action_text);
            const auto* frequency_value = find_value(object, "frequency");
            if (frequency_value == nullptr) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Missing frequency field."});
            }
            auto frequency = number_value(*frequency_value, "frequency");
            if (!frequency) {
                return std::unexpected(frequency.error());
            }
            action.frequency = *frequency;
            return action;
        }

        [[nodiscard]] std::expected<hand_strategy, cli_error> parse_hand_strategy(const json::value& value)
        {
            if (!value.is_object()) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Strategy row must be an object."});
            }
            const auto& object = value.as_object();
            hand_strategy row{};
            auto hand = required_string(object, "hand");
            if (!hand) {
                return std::unexpected(hand.error());
            }
            row.hand = std::move(*hand);
            const auto* strategy_value = find_value(object, "strategy");
            if (strategy_value == nullptr || !strategy_value->is_array()) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Strategy row must contain a strategy array."});
            }
            for (const auto& action_value : strategy_value->as_array()) {
                auto action = parse_action_strategy(action_value);
                if (!action) {
                    return std::unexpected(action.error());
                }
                row.strategy.push_back(std::move(*action));
            }
            const auto* ev_value = find_value(object, "ev");
            if (ev_value == nullptr) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Missing ev field."});
            }
            auto ev = number_value(*ev_value, "ev");
            if (!ev) {
                return std::unexpected(ev.error());
            }
            row.ev = *ev;
            return row;
        }

        [[nodiscard]] json::object solver_json(const solver_metadata& solver)
        {
            json::object out;
            out["algorithm"] = solver.algorithm;
            out["iterations"] = solver.iterations;
            out["timestamp"] = solver.timestamp;
            out["git_revision"] = solver.git_revision;
            return out;
        }

        [[nodiscard]] json::array action_strategy_json(const std::vector<action_strategy>& strategy)
        {
            json::array actions;
            actions.reserve(strategy.size());
            for (const auto& action : strategy) {
                json::object action_object;
                action_object["action"] = action.action;
                action_object["frequency"] = action.frequency;
                actions.emplace_back(std::move(action_object));
            }
            return actions;
        }

        [[nodiscard]] json::array strategy_json(const std::vector<hand_strategy>& strategy)
        {
            json::array rows;
            rows.reserve(strategy.size());
            for (const auto& row : strategy) {
                json::object row_object;
                row_object["hand"] = row.hand;
                row_object["strategy"] = action_strategy_json(row.strategy);
                row_object["ev"] = row.ev;
                rows.emplace_back(std::move(row_object));
            }
            return rows;
        }

    }

    std::expected<struct solve_spot, cli_error> parse_spot_json(const std::string_view json_text)
    {
        auto root = parse_object(json_text, "Spot");
        if (!root) {
            return std::unexpected(root.error());
        }

        struct solve_spot spot{};
        auto street = optional_string(*root, "street", spot.street);
        if (!street) {
            return std::unexpected(street.error());
        }
        spot.street = std::move(*street);
        auto parsed_street = detail::parse_holdem_street(spot.street);
        if (!parsed_street) {
            return std::unexpected(parsed_street.error());
        }

        auto board = required_string_array(*root, "board");
        if (!board) {
            return std::unexpected(board.error());
        }
        spot.board = std::move(*board);
        if (spot.board.size() != detail::board_size_for_street(*parsed_street)) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Board card count must match street."});
        }

        if (const auto* players_value = find_value(*root, "players"); players_value != nullptr) {
            auto players = string_array(*players_value, "players");
            if (!players) {
                return std::unexpected(players.error());
            }
            spot.players = std::move(*players);
            if (spot.players.size() < cli_min_players || spot.players.size() > cli_max_players) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Players array must contain between 2 and 7 labels."});
            }
            spot.ranges.assign(spot.players.size(), "AA");
            spot.contributions.assign(spot.players.size(), 0.0);
            spot.stacks.assign(spot.players.size(), 100.0);
            spot.contributions[0] = 50.0;
            spot.contributions[1] = 50.0;
        }

        auto ranges = optional_string_array(*root, "ranges", spot.ranges);
        if (!ranges) {
            return std::unexpected(ranges.error());
        }
        spot.ranges = std::move(*ranges);

        if (const auto* oop_range_value = find_value(*root, "oop_range"); oop_range_value != nullptr) {
            auto oop_range = string_value(*oop_range_value, "oop_range");
            if (!oop_range) {
                return std::unexpected(oop_range.error());
            }
            if (spot.ranges.size() < 2u) {
                spot.ranges.assign(2u, "AA");
            }
            spot.ranges[0] = std::move(*oop_range);
        }
        if (const auto* ip_range_value = find_value(*root, "ip_range"); ip_range_value != nullptr) {
            auto ip_range = string_value(*ip_range_value, "ip_range");
            if (!ip_range) {
                return std::unexpected(ip_range.error());
            }
            if (spot.ranges.size() < 2u) {
                spot.ranges.assign(2u, "AA");
            }
            spot.ranges[1] = std::move(*ip_range);
        }

        auto gross_pot = optional_double(*root, "gross_pot", spot.gross_pot);
        auto rake = optional_double(*root, "rake", spot.rake);
        auto bet_fraction = optional_double(*root, "bet_fraction", spot.bet_fraction);
        if (!gross_pot) {
            return std::unexpected(gross_pot.error());
        }
        if (!rake) {
            return std::unexpected(rake.error());
        }
        if (!bet_fraction) {
            return std::unexpected(bet_fraction.error());
        }
        spot.gross_pot = *gross_pot;
        spot.rake = *rake;
        spot.bet_fraction = *bet_fraction;

        auto contributions = optional_number_array(*root, "contributions", spot.contributions);
        auto stacks = optional_number_array(*root, "stacks", spot.stacks);
        if (!contributions) {
            return std::unexpected(contributions.error());
        }
        if (!stacks) {
            return std::unexpected(stacks.error());
        }
        spot.contributions = std::move(*contributions);
        spot.stacks = std::move(*stacks);

        if (auto oop = optional_double(*root, "oop_contribution", std::numeric_limits<double>::quiet_NaN()); !oop) {
            return std::unexpected(oop.error());
        } else if (!std::isnan(*oop)) {
            if (spot.contributions.size() < 2u) {
                spot.contributions.assign(2u, 0.0);
            }
            spot.contributions[0] = *oop;
        }
        if (auto ip = optional_double(*root, "ip_contribution", std::numeric_limits<double>::quiet_NaN()); !ip) {
            return std::unexpected(ip.error());
        } else if (!std::isnan(*ip)) {
            if (spot.contributions.size() < 2u) {
                spot.contributions.assign(2u, 0.0);
            }
            spot.contributions[1] = *ip;
        }
        if (auto oop = optional_double(*root, "oop_stack", std::numeric_limits<double>::quiet_NaN()); !oop) {
            return std::unexpected(oop.error());
        } else if (!std::isnan(*oop)) {
            if (spot.stacks.size() < 2u) {
                spot.stacks.assign(2u, 100.0);
            }
            spot.stacks[0] = *oop;
        }
        if (auto ip = optional_double(*root, "ip_stack", std::numeric_limits<double>::quiet_NaN()); !ip) {
            return std::unexpected(ip.error());
        } else if (!std::isnan(*ip)) {
            if (spot.stacks.size() < 2u) {
                spot.stacks.assign(2u, 100.0);
            }
            spot.stacks[1] = *ip;
        }

        auto max_history = optional_uint<uint16_t>(*root, "max_history", spot.max_history);
        auto public_state_id = optional_uint<uint32_t>(*root, "public_state_id", spot.public_state_id);
        auto root_actor = optional_uint<uint8_t>(*root, "root_actor", spot.root_actor);
        auto hero_seat = optional_uint<uint8_t>(*root, "hero_seat", spot.hero_seat);
        auto samples_per_combo = optional_uint<uint16_t>(*root, "samples_per_combo", spot.samples_per_combo);
        if (!max_history) {
            return std::unexpected(max_history.error());
        }
        if (!public_state_id) {
            return std::unexpected(public_state_id.error());
        }
        if (!root_actor) {
            return std::unexpected(root_actor.error());
        }
        if (!hero_seat) {
            return std::unexpected(hero_seat.error());
        }
        if (!samples_per_combo) {
            return std::unexpected(samples_per_combo.error());
        }
        spot.max_history = *max_history;
        spot.public_state_id = *public_state_id;
        spot.root_actor = *root_actor;
        spot.hero_seat = *hero_seat;
        spot.samples_per_combo = *samples_per_combo;

        if (auto validation = validate_spot_fields(spot); !validation) {
            return std::unexpected(validation.error());
        }
        return spot;
    }

    std::string serialize_spot_json(const struct solve_spot& spot)
    {
        json::object out;
        out["street"] = spot.street;
        out["players"] = string_array_json(spot.players);
        out["board"] = string_array_json(spot.board);
        out["ranges"] = string_array_json(spot.ranges);
        out["gross_pot"] = spot.gross_pot;
        out["rake"] = spot.rake;
        out["contributions"] = number_array_json(spot.contributions);
        out["stacks"] = number_array_json(spot.stacks);
        out["bet_fraction"] = spot.bet_fraction;
        out["max_history"] = static_cast<uint64_t>(spot.max_history);
        out["public_state_id"] = static_cast<uint64_t>(spot.public_state_id);
        out["root_actor"] = static_cast<uint64_t>(spot.root_actor);
        out["hero_seat"] = static_cast<uint64_t>(spot.hero_seat);
        out["samples_per_combo"] = static_cast<uint64_t>(spot.samples_per_combo);
        return json::serialize(out);
    }

    std::expected<solve_artifact, cli_error> parse_artifact_json(const std::string_view json_text)
    {
        auto root = parse_object(json_text, "Artifact");
        if (!root) {
            return std::unexpected(root.error());
        }

        solve_artifact artifact{};
        auto schema_version = required_uint<uint32_t>(*root, "schema_version");
        auto game = required_string(*root, "game");
        auto street = required_string(*root, "street");
        if (!schema_version) {
            return std::unexpected(schema_version.error());
        }
        if (!game) {
            return std::unexpected(game.error());
        }
        if (!street) {
            return std::unexpected(street.error());
        }
        artifact.schema_version = *schema_version;
        artifact.game = std::move(*game);
        artifact.street = std::move(*street);

        auto players = required_string_array(*root, "players");
        auto board = required_string_array(*root, "board");
        if (!players) {
            return std::unexpected(players.error());
        }
        if (!board) {
            return std::unexpected(board.error());
        }
        artifact.players = std::move(*players);
        artifact.board = std::move(*board);
        const auto parsed_street = detail::parse_holdem_street(artifact.street);
        if (!parsed_street) {
            return std::unexpected(parsed_street.error());
        }
        if (artifact.board.size() != detail::board_size_for_street(*parsed_street)) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Board card count must match artifact street."});
        }
        if (artifact.players.size() < cli_min_players || artifact.players.size() > cli_max_players) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Players array must have between 2 and 7 labels."});
        }

        auto hero_seat = optional_uint<uint8_t>(*root, "hero_seat", artifact.hero_seat);
        if (!hero_seat) {
            return std::unexpected(hero_seat.error());
        }
        artifact.hero_seat = *hero_seat;

        const auto* solver_value = find_value(*root, "solver");
        if (solver_value == nullptr || !solver_value->is_object()) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Missing solver object."});
        }
        const auto& solver_object = solver_value->as_object();
        auto algorithm = required_string(solver_object, "algorithm");
        auto iterations = required_uint<uint64_t>(solver_object, "iterations");
        auto timestamp = required_string(solver_object, "timestamp");
        auto git_revision = required_string(solver_object, "git_revision");
        if (!algorithm) {
            return std::unexpected(algorithm.error());
        }
        if (!iterations) {
            return std::unexpected(iterations.error());
        }
        if (!timestamp) {
            return std::unexpected(timestamp.error());
        }
        if (!git_revision) {
            return std::unexpected(git_revision.error());
        }
        artifact.solver.algorithm = std::move(*algorithm);
        artifact.solver.iterations = *iterations;
        artifact.solver.timestamp = std::move(*timestamp);
        artifact.solver.git_revision = std::move(*git_revision);

        if (const auto* root_strategy_value = find_value(*root, "root_strategy"); root_strategy_value != nullptr) {
            if (!root_strategy_value->is_array()) {
                return std::unexpected(cli_error{cli_error_kind::parse, "root_strategy must be an array."});
            }
            artifact.root_strategy.reserve(root_strategy_value->as_array().size());
            for (const auto& action_value : root_strategy_value->as_array()) {
                auto action = parse_action_strategy(action_value);
                if (!action) {
                    return std::unexpected(action.error());
                }
                artifact.root_strategy.push_back(std::move(*action));
            }
        }

        const auto* strategy_value = find_value(*root, "strategy");
        if (strategy_value == nullptr || !strategy_value->is_array()) {
            return std::unexpected(cli_error{cli_error_kind::parse, "Missing strategy array."});
        }
        artifact.strategy.reserve(strategy_value->as_array().size());
        for (const auto& row_value : strategy_value->as_array()) {
            auto row = parse_hand_strategy(row_value);
            if (!row) {
                return std::unexpected(row.error());
            }
            artifact.strategy.push_back(std::move(*row));
        }
        return artifact;
    }

    std::string serialize_artifact_json(const solve_artifact& artifact)
    {
        json::object out;
        out["schema_version"] = static_cast<uint64_t>(artifact.schema_version);
        out["game"] = artifact.game;
        out["street"] = artifact.street;
        out["players"] = string_array_json(artifact.players);
        out["board"] = string_array_json(artifact.board);
        out["hero_seat"] = static_cast<uint64_t>(artifact.hero_seat);
        out["solver"] = solver_json(artifact.solver);
        out["root_strategy"] = action_strategy_json(artifact.root_strategy);
        out["strategy"] = strategy_json(artifact.strategy);
        return json::serialize(out);
    }

}
