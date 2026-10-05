#include "cli/solve_cli.h"

#include <boost/json.hpp>

#include <cstdint>
#include <cstdlib>
#include <iomanip>
#include <limits>
#include <sstream>

#if defined(_WIN32)
#ifndef NOMINMAX
#define NOMINMAX
#endif
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#elif defined(__linux__) || defined(__unix__) || defined(__APPLE__)
#include <unistd.h>
#endif

namespace zeta::holdem::cli {

    namespace detail {

        uint64_t detected_available_memory_bytes() noexcept
        {
#if defined(_WIN32)
            MEMORYSTATUSEX status{};
            status.dwLength = sizeof(status);
            if (GlobalMemoryStatusEx(&status)) {
                return static_cast<uint64_t>(status.ullAvailPhys);
            }
            return 0u;
#elif defined(_SC_AVPHYS_PAGES) && defined(_SC_PAGE_SIZE)
            const long pages = sysconf(_SC_AVPHYS_PAGES);
            const long page_size = sysconf(_SC_PAGE_SIZE);
            if (pages > 0 && page_size > 0) {
                return static_cast<uint64_t>(pages) * static_cast<uint64_t>(page_size);
            }
            return 0u;
#else
            return 0u;
#endif
        }

    }

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

        [[nodiscard]] std::expected<double, cli_error> required_double(
            const json::object& object,
            const std::string_view key)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Missing " + key_name(key) + " field."});
            }
            return number_value(*value, key);
        }

        [[nodiscard]] std::expected<bool, cli_error> optional_bool(
            const json::object& object,
            const std::string_view key,
            const bool fallback)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return fallback;
            }
            if (!value->is_bool()) {
                return std::unexpected(cli_error{cli_error_kind::parse, key_name(key) + " must be a boolean."});
            }
            return value->as_bool();
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

        // Accepts either a player label string ("BB") or an integer index (1) for seat fields.
        [[nodiscard]] std::expected<uint8_t, cli_error> optional_player_seat(
            const json::object& object,
            const std::string_view key,
            const std::vector<std::string>& players,
            const uint8_t fallback)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return fallback;
            }
            if (value->is_string()) {
                const auto& str = value->as_string();
                const std::string label{str.data(), str.size()};
                for (std::size_t i = 0; i < players.size(); ++i) {
                    if (players[i] == label) {
                        return static_cast<uint8_t>(i);
                    }
                }
                return std::unexpected(cli_error{cli_error_kind::parse,
                    key_name(key) + " player label '" + label + "' not found in players array."});
            }
            auto parsed = uint64_value(*value, key);
            if (!parsed) {
                return std::unexpected(parsed.error());
            }
            if (*parsed > static_cast<uint64_t>(std::numeric_limits<uint8_t>::max())) {
                return std::unexpected(cli_error{cli_error_kind::parse, key_name(key) + " is out of range."});
            }
            return static_cast<uint8_t>(*parsed);
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

        [[nodiscard]] std::expected<void, cli_error> validate_spot_fields(struct solve_spot& spot)
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
            if (spot.betting_policy.fixed_pot_fractions.empty()) {
                spot.betting_policy.fixed_pot_fractions = {spot.bet_fraction > 0.0 ? spot.bet_fraction : 0.75};
                spot.betting_policy.max_raises = 1;
            }
            if (spot.betting_policy.fixed_pot_fractions.front() <= 0.0) {
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "betting_policy fixed_pot_fractions must be positive."});
            }
            if (auto policy_validation = cfr::validate_betting_abstraction_policy(spot.betting_policy); !policy_validation) {
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Invalid betting policy: " + std::string{cfr::to_string(policy_validation.error().kind)}});
            }
            spot.bet_fraction = spot.betting_policy.fixed_pot_fractions.front();
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
            auto combo = required_uint<combination_index>(object, "combination_index");
            if (!combo) {
                return std::unexpected(combo.error());
            }
            row.combination_index = *combo;
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
            auto range_weight = required_double(object, "range_weight");
            if (!range_weight) {
                return std::unexpected(range_weight.error());
            }
            auto reach_probability = required_double(object, "reach_probability");
            if (!reach_probability) {
                return std::unexpected(reach_probability.error());
            }
            row.range_weight = *range_weight;
            row.reach_probability = *reach_probability;
            row.ev = *ev;
            return row;
        }

        [[nodiscard]] std::expected<solved_node_seat_value, cli_error> parse_solved_node_seat_value(const json::value& value)
        {
            if (!value.is_object()) {
                return std::unexpected(cli_error{cli_error_kind::parse, "solved_nodes.seat_values entries must be objects."});
            }
            const auto& object = value.as_object();
            auto seat = required_uint<uint8_t>(object, "seat");
            auto range_reach_mass = required_double(object, "range_reach_mass");
            auto reach_weighted_value = required_double(object, "reach_weighted_value");
            auto conditional_range_ev = required_double(object, "conditional_range_ev");
            auto counterfactual_value = required_double(object, "counterfactual_value");
            if (!seat) return std::unexpected(seat.error());
            if (!range_reach_mass) return std::unexpected(range_reach_mass.error());
            if (!reach_weighted_value) return std::unexpected(reach_weighted_value.error());
            if (!conditional_range_ev) return std::unexpected(conditional_range_ev.error());
            if (!counterfactual_value) return std::unexpected(counterfactual_value.error());
            return solved_node_seat_value{
                .seat = *seat,
                .range_reach_mass = *range_reach_mass,
                .reach_weighted_value = *reach_weighted_value,
                .conditional_range_ev = *conditional_range_ev,
                .counterfactual_value = *counterfactual_value
            };
        }

        [[nodiscard]] std::expected<category_summary_item, cli_error> parse_category_summary_item(const json::value& value)
        {
            if (!value.is_object()) {
                return std::unexpected(cli_error{
                    cli_error_kind::parse,
                    "solved_nodes.derived.category_summaries.items entries must be objects."
                });
            }
            const auto& object = value.as_object();
            auto category_name = required_string(object, "category_name");
            auto frequency = required_double(object, "frequency");
            auto range_weight = required_double(object, "range_weight");
            auto average_ev = required_double(object, "average_ev");
            if (!category_name) return std::unexpected(category_name.error());
            if (!frequency) return std::unexpected(frequency.error());
            if (!range_weight) return std::unexpected(range_weight.error());
            if (!average_ev) return std::unexpected(average_ev.error());

            category_summary_item item{
                .category_name = std::move(*category_name),
                .frequency = *frequency,
                .range_weight = *range_weight,
                .average_ev = *average_ev
            };
            const auto* actions_value = find_value(object, "action_frequencies");
            if (actions_value == nullptr || !actions_value->is_array()) {
                return std::unexpected(cli_error{
                    cli_error_kind::parse,
                    "solved_nodes.derived.category_summaries.items.action_frequencies must be an array."
                });
            }
            for (const auto& action_value : actions_value->as_array()) {
                if (!action_value.is_object()) {
                    return std::unexpected(cli_error{
                        cli_error_kind::parse,
                        "solved_nodes.derived.category_summaries.items.action_frequencies entries must be objects."
                    });
                }
                const auto& action_object = action_value.as_object();
                auto action_index = required_uint<uint16_t>(action_object, "action_index");
                auto action_frequency = required_double(action_object, "frequency");
                if (!action_index) return std::unexpected(action_index.error());
                if (!action_frequency) return std::unexpected(action_frequency.error());
                item.action_frequencies.push_back(category_summary_action_frequency{
                    .action_index = *action_index,
                    .frequency = *action_frequency
                });
            }
            return item;
        }

        [[nodiscard]] std::expected<uint32_t, cli_error> nullable_uint32(
            const json::object& object,
            const std::string_view key,
            const uint32_t fallback)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr || value->is_null()) {
                return fallback;
            }
            return required_uint<uint32_t>(object, key);
        }

        [[nodiscard]] std::expected<uint16_t, cli_error> nullable_uint16(
            const json::object& object,
            const std::string_view key,
            const uint16_t fallback)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr || value->is_null()) {
                return fallback;
            }
            return required_uint<uint16_t>(object, key);
        }

        [[nodiscard]] std::expected<uint8_t, cli_error> nullable_uint8(
            const json::object& object,
            const std::string_view key,
            const uint8_t fallback)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr || value->is_null()) {
                return fallback;
            }
            return required_uint<uint8_t>(object, key);
        }

        [[nodiscard]] std::expected<solved_node_action, cli_error> parse_solved_node_action(const json::value& value)
        {
            if (!value.is_object()) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Solved node action must be an object."});
            }
            const auto& object = value.as_object();
            auto action = required_string(object, "action");
            auto child_node_id = nullable_uint32(object, "child_node_id", cfr::game_graph::INVALID_NODE);
            auto action_index = nullable_uint16(object, "action_index", 0u);
            auto probability = optional_double(object, "probability", 0.0);
            auto chance_event_id = nullable_uint32(object, "chance_event_id", cfr::INVALID_CHANCE_EVENT);
            auto board_partition_id = nullable_uint32(object, "board_partition_id", cfr::INVALID_BOARD_PARTITION);
            auto chance_outcome_id = nullable_uint32(object, "chance_outcome_id", cfr::INVALID_CHANCE_OUTCOME_ID);
            auto dealt_cards = optional_string_array(object, "dealt_cards", {});
            if (!action) return std::unexpected(action.error());
            if (!child_node_id) return std::unexpected(child_node_id.error());
            if (!action_index) return std::unexpected(action_index.error());
            if (!probability) return std::unexpected(probability.error());
            if (!chance_event_id) return std::unexpected(chance_event_id.error());
            if (!board_partition_id) return std::unexpected(board_partition_id.error());
            if (!chance_outcome_id) return std::unexpected(chance_outcome_id.error());
            if (!dealt_cards) return std::unexpected(dealt_cards.error());
            return solved_node_action{
                .action = std::move(*action),
                .child_node_id = *child_node_id,
                .action_index = *action_index,
                .probability = static_cast<float>(*probability),
                .chance_event_id = *chance_event_id,
                .board_partition_id = *board_partition_id,
                .chance_outcome_id = *chance_outcome_id,
                .dealt_cards = std::move(*dealt_cards)
            };
        }

        [[nodiscard]] std::string hash_to_hex(const uint64_t value)
        {
            std::ostringstream stream;
            stream << "0x" << std::hex << std::setw(16) << std::setfill('0') << value;
            return stream.str();
        }

        [[nodiscard]] uint64_t hash_from_hex(const std::string_view text)
        {
            if (text.empty()) {
                return 0u;
            }
            return std::strtoull(std::string{text}.c_str(), nullptr, 0);
        }

        [[nodiscard]] json::object solver_json(const solver_metadata& solver)
        {
            json::object out;
            out["algorithm"] = solver.algorithm;
            out["iterations"] = solver.iterations;
            out["timestamp"] = solver.timestamp;
            out["git_revision"] = solver.git_revision;

            json::object hashes;
            hashes["tree"] = hash_to_hex(solver.hashes.tree_hash);
            hashes["range"] = hash_to_hex(solver.hashes.range_hash);
            hashes["board"] = hash_to_hex(solver.hashes.board_hash);
            hashes["betting_policy"] = hash_to_hex(solver.hashes.betting_policy_hash);
            hashes["solver_config"] = hash_to_hex(solver.hashes.solver_config_hash);
            hashes["solve"] = hash_to_hex(solver.hashes.solve_hash);
            out["hashes"] = std::move(hashes);

            const auto& convergence = solver.convergence;
            json::object convergence_object;
            convergence_object["exploitability_available"] = convergence.exploitability_available;
            convergence_object["exploitability"] = convergence.exploitability;
            convergence_object["exploitability_pot_fraction"] = convergence.exploitability_pot_fraction;
            convergence_object["nash_conv"] = convergence.nash_conv;
            convergence_object["normalized_regret"] = convergence.normalized_regret;
            convergence_object["target_exploitability"] = convergence.target_exploitability;
            convergence_object["reached_target"] = convergence.reached_target;
            json::array gaps;
            gaps.reserve(convergence.best_response_gap.size());
            for (const auto gap : convergence.best_response_gap) {
                gaps.emplace_back(gap);
            }
            convergence_object["best_response_gap"] = std::move(gaps);
            json::array curve;
            curve.reserve(convergence.curve.size());
            for (const auto& sample : convergence.curve) {
                json::object sample_object;
                sample_object["iteration"] = sample.iteration;
                sample_object["metric"] = sample.metric;
                sample_object["elapsed_ms"] = sample.elapsed_ms;
                curve.emplace_back(std::move(sample_object));
            }
            convergence_object["curve"] = std::move(curve);
            out["convergence"] = std::move(convergence_object);

            out["warnings"] = string_array_json(solver.warnings);
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

        [[nodiscard]] json::array solved_node_action_json(const std::vector<solved_node_action>& actions)
        {
            json::array out;
            out.reserve(actions.size());
            for (const auto& action : actions) {
                json::object object;
                object["action"] = action.action;
                object["child_node_id"] = action.child_node_id == cfr::game_graph::INVALID_NODE
                    ? json::value{nullptr}
                    : json::value{static_cast<uint64_t>(action.child_node_id)};
                object["action_index"] = action.action_index;
                object["probability"] = action.probability;
                object["chance_event_id"] = action.chance_event_id == cfr::INVALID_CHANCE_EVENT
                    ? json::value{nullptr}
                    : json::value{static_cast<uint64_t>(action.chance_event_id)};
                object["board_partition_id"] = action.board_partition_id == cfr::INVALID_BOARD_PARTITION
                    ? json::value{nullptr}
                    : json::value{static_cast<uint64_t>(action.board_partition_id)};
                object["chance_outcome_id"] = action.chance_outcome_id == cfr::INVALID_CHANCE_OUTCOME_ID
                    ? json::value{nullptr}
                    : json::value{static_cast<uint64_t>(action.chance_outcome_id)};
                object["dealt_cards"] = string_array_json(action.dealt_cards);
                out.emplace_back(std::move(object));
            }
            return out;
        }

        [[nodiscard]] json::array strategy_json(const std::vector<hand_strategy>& strategy)
        {
            json::array rows;
            rows.reserve(strategy.size());
            for (const auto& row : strategy) {
                json::object row_object;
                row_object["combination_index"] = static_cast<uint64_t>(row.combination_index);
                row_object["hand"] = row.hand;
                row_object["strategy"] = action_strategy_json(row.strategy);
                row_object["range_weight"] = row.range_weight;
                row_object["reach_probability"] = row.reach_probability;
                row_object["ev"] = row.ev;
                rows.emplace_back(std::move(row_object));
            }
            return rows;
        }

        [[nodiscard]] json::array solved_node_seat_values_json(const std::vector<solved_node_seat_value>& seat_values)
        {
            json::array out;
            out.reserve(seat_values.size());
            for (const auto& seat_value : seat_values) {
                json::object object;
                object["seat"] = static_cast<uint64_t>(seat_value.seat);
                object["range_reach_mass"] = seat_value.range_reach_mass;
                object["reach_weighted_value"] = seat_value.reach_weighted_value;
                object["conditional_range_ev"] = seat_value.conditional_range_ev;
                object["counterfactual_value"] = seat_value.counterfactual_value;
                out.emplace_back(std::move(object));
            }
            return out;
        }

        [[nodiscard]] json::object category_summaries_json(
            const uint32_t derivation_version,
            const std::vector<category_summary_item>& items)
        {
            json::array item_array;
            item_array.reserve(items.size());
            for (const auto& item : items) {
                json::object item_object;
                item_object["category_name"] = item.category_name;
                item_object["frequency"] = item.frequency;
                item_object["range_weight"] = item.range_weight;
                item_object["average_ev"] = item.average_ev;
                json::array action_frequencies;
                action_frequencies.reserve(item.action_frequencies.size());
                for (const auto& action : item.action_frequencies) {
                    json::object action_object;
                    action_object["action_index"] = static_cast<uint64_t>(action.action_index);
                    action_object["frequency"] = action.frequency;
                    action_frequencies.emplace_back(std::move(action_object));
                }
                item_object["action_frequencies"] = std::move(action_frequencies);
                item_array.emplace_back(std::move(item_object));
            }
            json::object category_summaries;
            category_summaries["derivation_version"] = static_cast<uint64_t>(derivation_version);
            category_summaries["items"] = std::move(item_array);
            return category_summaries;
        }

        [[nodiscard]] json::array public_states_json(const std::vector<solve_artifact_public_state>& states)
        {
            json::array out;
            out.reserve(states.size());
            for (const auto& state : states) {
                json::object object;
                object["id"] = state.id;
                object["street"] = state.street;
                object["board"] = string_array_json(state.board);
                object["parent_state_id"] = state.parent_state_id == cfr::INVALID_PUBLIC_STATE_ID
                    ? json::value{nullptr}
                    : json::value{static_cast<uint64_t>(state.parent_state_id)};
                object["chance_event_id_from_parent"] = state.chance_event_id_from_parent == cfr::INVALID_CHANCE_EVENT
                    ? json::value{nullptr}
                    : json::value{static_cast<uint64_t>(state.chance_event_id_from_parent)};
                object["chance_outcome_id_from_parent"] = state.chance_outcome_id_from_parent == cfr::INVALID_CHANCE_OUTCOME_ID
                    ? json::value{nullptr}
                    : json::value{static_cast<uint64_t>(state.chance_outcome_id_from_parent)};
                object["is_root_state"] = state.is_root_state;
                object["is_terminal_river_state"] = state.is_terminal_river_state;
                out.emplace_back(std::move(object));
            }
            return out;
        }

        [[nodiscard]] json::array chance_events_json(const std::vector<solve_artifact_chance_event>& events)
        {
            json::array out;
            out.reserve(events.size());
            for (const auto& event : events) {
                json::object object;
                object["id"] = event.id;
                object["node_id"] = event.node_id;
                object["kind"] = event.kind;
                object["board"] = string_array_json(event.board);
                object["outcomes"] = solved_node_action_json(event.outcomes);
                out.emplace_back(std::move(object));
            }
            return out;
        }

        [[nodiscard]] json::array runouts_json(const std::vector<solve_artifact_runout>& runouts)
        {
            json::array out;
            out.reserve(runouts.size());
            for (const auto& runout : runouts) {
                json::object object;
                object["id"] = runout.id;
                object["root_public_state_id"] = runout.root_public_state_id;
                object["river_public_state_id"] = runout.river_public_state_id;
                object["dealt_turn"] = string_array_json(runout.dealt_turn);
                object["dealt_river"] = string_array_json(runout.dealt_river);
                out.emplace_back(std::move(object));
            }
            return out;
        }

        [[nodiscard]] json::array solved_nodes_json(
            const std::vector<solved_node>& nodes,
            const solve_artifact_export_mode mode)
        {
            json::array out;
            out.reserve(nodes.size());
            for (const auto& node : nodes) {
                json::object object;
                object["node_id"] = node.node_id;
                object["kind"] = node.kind;
                object["public_state_id"] = node.public_state_id == cfr::INVALID_PUBLIC_STATE_ID
                    ? json::value{nullptr}
                    : json::value{static_cast<uint64_t>(node.public_state_id)};
                object["parent_node_id"] = node.parent_node_id == cfr::game_graph::INVALID_NODE
                    ? json::value{nullptr}
                    : json::value{static_cast<uint64_t>(node.parent_node_id)};
                object["acting_seat"] = node.acting_seat == cfr::solver::INVALID_PLAYER
                    ? json::value{nullptr}
                    : json::value{static_cast<uint64_t>(node.acting_seat)};
                object["terminal"] = node.terminal;
                object["board"] = string_array_json(node.board);
                object["actions"] = solved_node_action_json(node.actions);
                object["range_action_frequencies"] = action_strategy_json(node.range_action_frequencies);
                object["seat_values"] = solved_node_seat_values_json(node.seat_values);
                json::object derived;
                derived["category_summaries"] = category_summaries_json(
                    node.category_summary_derivation_version,
                    node.category_summaries);
                object["derived"] = std::move(derived);
                if (mode == solve_artifact_export_mode::full
                    || (mode == solve_artifact_export_mode::standard && node.kind == "player")) {
                    object["strategy_rows"] = strategy_json(node.strategy_rows);
                }
                out.emplace_back(std::move(object));
            }
            return out;
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
        if (find_value(*root, "oop_range") != nullptr
            || find_value(*root, "ip_range") != nullptr
            || find_value(*root, "oop_contribution") != nullptr
            || find_value(*root, "ip_contribution") != nullptr
            || find_value(*root, "oop_stack") != nullptr
            || find_value(*root, "ip_stack") != nullptr) {
            return std::unexpected(cli_error{
                cli_error_kind::parse,
                "Legacy heads-up aliases are no longer accepted. Use players/ranges/contributions/stacks arrays."
            });
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

        if (const auto* betting_policy_value = find_value(*root, "betting_policy"); betting_policy_value != nullptr) {
            if (!betting_policy_value->is_object()) {
                return std::unexpected(cli_error{cli_error_kind::parse, "betting_policy must be an object."});
            }
            const auto policy_json = json::serialize(*betting_policy_value);
            auto parsed_policy = cfr::deserialize_betting_abstraction_policy(policy_json);
            if (!parsed_policy) {
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Invalid betting_policy: " + parsed_policy.error()});
            }
            spot.betting_policy = std::move(*parsed_policy);
        } else {
            spot.betting_policy = cfr::betting_abstraction_policy{
                .fixed_pot_fractions = {spot.bet_fraction},
                .max_raises = 1
            };
        }

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

        auto max_history = optional_uint<uint16_t>(*root, "max_history", spot.max_history);
        auto public_state_id = optional_uint<uint32_t>(*root, "public_state_id", spot.public_state_id);
        auto root_actor = optional_player_seat(*root, "root_actor", spot.players, spot.root_actor);
        auto hero_seat = optional_player_seat(*root, "hero_seat", spot.players, spot.hero_seat);
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

    std::expected<solve_runtime_options, cli_error> parse_spot_runtime_options(const std::string_view json_text)
    {
        auto root = parse_object(json_text, "Spot");
        if (!root) {
            return std::unexpected(root.error());
        }

        solve_runtime_options runtime{};
        const auto* runtime_value = find_value(*root, "runtime");
        if (runtime_value == nullptr) {
            return runtime;
        }
        if (!runtime_value->is_object()) {
            return std::unexpected(cli_error{cli_error_kind::parse, "runtime must be an object."});
        }
        const auto& runtime_object = runtime_value->as_object();

        auto worker_threads = optional_uint<uint32_t>(runtime_object, "worker_threads", runtime.worker_threads);
        auto memory_budget = optional_uint<uint64_t>(runtime_object, "memory_budget_bytes", runtime.memory_budget_bytes);
        auto card_isomorphism = optional_bool(runtime_object, "card_isomorphism", runtime.enable_card_isomorphism);
        auto allow_lossy = optional_bool(
            runtime_object, "allow_lossy_card_isomorphism", runtime.allow_lossy_card_isomorphism);
        if (!worker_threads) {
            return std::unexpected(worker_threads.error());
        }
        if (!memory_budget) {
            return std::unexpected(memory_budget.error());
        }
        if (!card_isomorphism) {
            return std::unexpected(card_isomorphism.error());
        }
        if (!allow_lossy) {
            return std::unexpected(allow_lossy.error());
        }
        runtime.worker_threads = *worker_threads;
        runtime.memory_budget_bytes = *memory_budget;
        runtime.enable_card_isomorphism = *card_isomorphism;
        runtime.allow_lossy_card_isomorphism = *allow_lossy;

        if (const auto* pruning_value = find_value(runtime_object, "dynamic_pruning"); pruning_value != nullptr) {
            if (!pruning_value->is_object()) {
                return std::unexpected(cli_error{cli_error_kind::parse, "runtime.dynamic_pruning must be an object."});
            }
            const auto& pruning_object = pruning_value->as_object();
            auto enabled = optional_bool(pruning_object, "enabled", runtime.pruning.enabled);
            auto prune_threshold = optional_double(pruning_object, "prune_threshold", runtime.pruning.prune_threshold);
            auto minimum_active = optional_uint<uint32_t>(
                pruning_object, "minimum_active_actions", runtime.pruning.minimum_active_actions);
            auto reconsider_interval = optional_uint<uint32_t>(
                pruning_object, "reconsider_interval", runtime.pruning.reconsider_interval);
            if (!enabled) {
                return std::unexpected(enabled.error());
            }
            if (!prune_threshold) {
                return std::unexpected(prune_threshold.error());
            }
            if (!minimum_active) {
                return std::unexpected(minimum_active.error());
            }
            if (!reconsider_interval) {
                return std::unexpected(reconsider_interval.error());
            }
            if (*prune_threshold < 0.0) {
                return std::unexpected(cli_error{
                    cli_error_kind::invalid_spot, "runtime.dynamic_pruning.prune_threshold must be non-negative."});
            }
            if (*minimum_active < 1u) {
                return std::unexpected(cli_error{
                    cli_error_kind::invalid_spot, "runtime.dynamic_pruning.minimum_active_actions must be at least 1."});
            }
            if (*reconsider_interval < 1u) {
                return std::unexpected(cli_error{
                    cli_error_kind::invalid_spot, "runtime.dynamic_pruning.reconsider_interval must be at least 1."});
            }
            runtime.pruning.enabled = *enabled;
            runtime.pruning.prune_threshold = *prune_threshold;
            runtime.pruning.minimum_active_actions = *minimum_active;
            runtime.pruning.reconsider_interval = *reconsider_interval;
        }

        if (const auto* convergence_value = find_value(runtime_object, "convergence"); convergence_value != nullptr) {
            if (!convergence_value->is_object()) {
                return std::unexpected(cli_error{cli_error_kind::parse, "runtime.convergence must be an object."});
            }
            const auto& convergence_object = convergence_value->as_object();
            auto measure = optional_bool(
                convergence_object, "measure_exploitability", runtime.convergence.measure_exploitability);
            auto interval = optional_uint<uint64_t>(
                convergence_object, "measurement_interval", runtime.convergence.measurement_interval);
            auto target = optional_double(
                convergence_object, "target_exploitability", runtime.convergence.target_exploitability);
            auto max_samples = optional_uint<uint32_t>(
                convergence_object, "max_curve_samples", runtime.convergence.max_curve_samples);
            if (!measure) {
                return std::unexpected(measure.error());
            }
            if (!interval) {
                return std::unexpected(interval.error());
            }
            if (!target) {
                return std::unexpected(target.error());
            }
            if (!max_samples) {
                return std::unexpected(max_samples.error());
            }
            if (*target < 0.0) {
                return std::unexpected(cli_error{
                    cli_error_kind::invalid_spot, "runtime.convergence.target_exploitability must be non-negative."});
            }
            runtime.convergence.measure_exploitability = *measure;
            runtime.convergence.measurement_interval = *interval;
            runtime.convergence.target_exploitability = *target;
            runtime.convergence.max_curve_samples = *max_samples;
        }

        return runtime;
    }

    std::string serialize_spot_json(const struct solve_spot& spot)
    {
        const auto serialized_policy = cfr::serialize_betting_abstraction_policy(resolve_spot_betting_policy(spot));
        const auto policy_json = json::parse(serialized_policy);
        const auto effective_bet_fraction = resolve_spot_betting_policy(spot).fixed_pot_fractions.empty()
            ? spot.bet_fraction
            : resolve_spot_betting_policy(spot).fixed_pot_fractions.front();

        json::object out;
        out["street"] = spot.street;
        out["players"] = string_array_json(spot.players);
        out["board"] = string_array_json(spot.board);
        out["ranges"] = string_array_json(spot.ranges);
        out["gross_pot"] = spot.gross_pot;
        out["rake"] = spot.rake;
        out["contributions"] = number_array_json(spot.contributions);
        out["stacks"] = number_array_json(spot.stacks);
        out["bet_fraction"] = effective_bet_fraction;
        out["betting_policy"] = policy_json;
        out["max_history"] = static_cast<uint64_t>(spot.max_history);
        out["public_state_id"] = static_cast<uint64_t>(spot.public_state_id);
        out["root_actor"] = spot.root_actor < spot.players.size()
            ? spot.players[spot.root_actor] : std::to_string(spot.root_actor);
        out["hero_seat"] = spot.hero_seat < spot.players.size()
            ? spot.players[spot.hero_seat] : std::to_string(spot.hero_seat);
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
        auto extraction_version = required_uint<uint32_t>(*root, "extraction_version");
        auto game = required_string(*root, "game");
        auto street = required_string(*root, "street");
        if (!schema_version) {
            return std::unexpected(schema_version.error());
        }
        if (*schema_version != current_artifact_schema_version) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact,
                "Unsupported schema_version. Only version " + std::to_string(current_artifact_schema_version) + " is accepted."});
        }
        if (!extraction_version) {
            return std::unexpected(extraction_version.error());
        }
        if (*extraction_version != current_extraction_version) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact,
                "Unsupported extraction_version. Only version " + std::to_string(current_extraction_version) + " is accepted."});
        }
        if (!game) {
            return std::unexpected(game.error());
        }
        if (!street) {
            return std::unexpected(street.error());
        }
        artifact.schema_version = *schema_version;
        artifact.extraction_version = *extraction_version;
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

        if (const auto* hashes_value = find_value(solver_object, "hashes");
            hashes_value != nullptr && hashes_value->is_object()) {
            const auto& hashes_object = hashes_value->as_object();
            auto tree = optional_string(hashes_object, "tree", "");
            auto range = optional_string(hashes_object, "range", "");
            auto board = optional_string(hashes_object, "board", "");
            auto betting_policy = optional_string(hashes_object, "betting_policy", "");
            auto solver_config = optional_string(hashes_object, "solver_config", "");
            auto solve = optional_string(hashes_object, "solve", "");
            if (!tree) { return std::unexpected(tree.error()); }
            if (!range) { return std::unexpected(range.error()); }
            if (!board) { return std::unexpected(board.error()); }
            if (!betting_policy) { return std::unexpected(betting_policy.error()); }
            if (!solver_config) { return std::unexpected(solver_config.error()); }
            if (!solve) { return std::unexpected(solve.error()); }
            artifact.solver.hashes.tree_hash = hash_from_hex(*tree);
            artifact.solver.hashes.range_hash = hash_from_hex(*range);
            artifact.solver.hashes.board_hash = hash_from_hex(*board);
            artifact.solver.hashes.betting_policy_hash = hash_from_hex(*betting_policy);
            artifact.solver.hashes.solver_config_hash = hash_from_hex(*solver_config);
            artifact.solver.hashes.solve_hash = hash_from_hex(*solve);
        }

        if (const auto* convergence_value = find_value(solver_object, "convergence");
            convergence_value != nullptr && convergence_value->is_object()) {
            const auto& convergence_object = convergence_value->as_object();
            auto available = optional_bool(convergence_object, "exploitability_available", false);
            auto exploitability = optional_double(convergence_object, "exploitability", 0.0);
            auto pot_fraction = optional_double(convergence_object, "exploitability_pot_fraction", 0.0);
            auto nash_conv = optional_double(convergence_object, "nash_conv", 0.0);
            auto normalized_regret = optional_double(convergence_object, "normalized_regret", 0.0);
            auto target = optional_double(convergence_object, "target_exploitability", 0.0);
            auto reached = optional_bool(convergence_object, "reached_target", false);
            auto gaps = optional_number_array(convergence_object, "best_response_gap", {});
            if (!available) { return std::unexpected(available.error()); }
            if (!exploitability) { return std::unexpected(exploitability.error()); }
            if (!pot_fraction) { return std::unexpected(pot_fraction.error()); }
            if (!nash_conv) { return std::unexpected(nash_conv.error()); }
            if (!normalized_regret) { return std::unexpected(normalized_regret.error()); }
            if (!target) { return std::unexpected(target.error()); }
            if (!reached) { return std::unexpected(reached.error()); }
            if (!gaps) { return std::unexpected(gaps.error()); }
            auto& convergence = artifact.solver.convergence;
            convergence.exploitability_available = *available;
            convergence.exploitability = *exploitability;
            convergence.exploitability_pot_fraction = *pot_fraction;
            convergence.nash_conv = *nash_conv;
            convergence.normalized_regret = *normalized_regret;
            convergence.target_exploitability = *target;
            convergence.reached_target = *reached;
            convergence.best_response_gap.assign(gaps->begin(), gaps->end());
            if (const auto* curve_value = find_value(convergence_object, "curve");
                curve_value != nullptr) {
                if (!curve_value->is_array()) {
                    return std::unexpected(cli_error{cli_error_kind::parse, "convergence.curve must be an array."});
                }
                for (const auto& element : curve_value->as_array()) {
                    if (!element.is_object()) {
                        return std::unexpected(cli_error{
                            cli_error_kind::parse, "convergence.curve entries must be objects."});
                    }
                    const auto& sample_object = element.as_object();
                    auto iteration = optional_uint<uint64_t>(sample_object, "iteration", 0);
                    auto metric = optional_double(sample_object, "metric", 0.0);
                    auto elapsed_ms = optional_double(sample_object, "elapsed_ms", 0.0);
                    if (!iteration) { return std::unexpected(iteration.error()); }
                    if (!metric) { return std::unexpected(metric.error()); }
                    if (!elapsed_ms) { return std::unexpected(elapsed_ms.error()); }
                    convergence.curve.push_back(convergence_sample{
                        .iteration = *iteration,
                        .metric = *metric,
                        .elapsed_ms = *elapsed_ms,
                    });
                }
            }
        }

        auto warnings = optional_string_array(solver_object, "warnings", {});
        if (!warnings) {
            return std::unexpected(warnings.error());
        }
        artifact.solver.warnings = std::move(*warnings);

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

        if (const auto* public_states_value = find_value(*root, "public_states"); public_states_value != nullptr) {
            if (!public_states_value->is_array()) {
                return std::unexpected(cli_error{cli_error_kind::parse, "public_states must be an array."});
            }
            for (const auto& state_value : public_states_value->as_array()) {
                if (!state_value.is_object()) {
                    return std::unexpected(cli_error{cli_error_kind::parse, "public_states entries must be objects."});
                }
                const auto& object = state_value.as_object();
                auto id = required_uint<uint32_t>(object, "id");
                auto street = required_string(object, "street");
                auto board = required_string_array(object, "board");
                auto parent = nullable_uint32(object, "parent_state_id", cfr::INVALID_PUBLIC_STATE_ID);
                auto event = nullable_uint32(object, "chance_event_id_from_parent", cfr::INVALID_CHANCE_EVENT);
                auto outcome = nullable_uint32(object, "chance_outcome_id_from_parent", cfr::INVALID_CHANCE_OUTCOME_ID);
                if (!id) return std::unexpected(id.error());
                if (!street) return std::unexpected(street.error());
                if (!board) return std::unexpected(board.error());
                if (!parent) return std::unexpected(parent.error());
                if (!event) return std::unexpected(event.error());
                if (!outcome) return std::unexpected(outcome.error());
                artifact.public_states.push_back(solve_artifact_public_state{
                    .id = *id,
                    .street = std::move(*street),
                    .board = std::move(*board),
                    .parent_state_id = *parent,
                    .chance_event_id_from_parent = *event,
                    .chance_outcome_id_from_parent = *outcome,
                    .is_root_state = object.if_contains("is_root_state") != nullptr && object.at("is_root_state").as_bool(),
                    .is_terminal_river_state = object.if_contains("is_terminal_river_state") != nullptr && object.at("is_terminal_river_state").as_bool()
                });
            }
        }

        if (const auto* chance_events_value = find_value(*root, "chance_events"); chance_events_value != nullptr) {
            if (!chance_events_value->is_array()) {
                return std::unexpected(cli_error{cli_error_kind::parse, "chance_events must be an array."});
            }
            for (const auto& event_value : chance_events_value->as_array()) {
                if (!event_value.is_object()) {
                    return std::unexpected(cli_error{cli_error_kind::parse, "chance_events entries must be objects."});
                }
                const auto& object = event_value.as_object();
                auto id = required_uint<uint32_t>(object, "id");
                auto node_id = required_uint<uint32_t>(object, "node_id");
                auto kind = required_string(object, "kind");
                auto board = required_string_array(object, "board");
                if (!id) return std::unexpected(id.error());
                if (!node_id) return std::unexpected(node_id.error());
                if (!kind) return std::unexpected(kind.error());
                if (!board) return std::unexpected(board.error());
                solve_artifact_chance_event event_record{
                    .id = *id,
                    .node_id = *node_id,
                    .kind = std::move(*kind),
                    .board = std::move(*board)
                };
                if (const auto* outcomes = find_value(object, "outcomes"); outcomes != nullptr) {
                    if (!outcomes->is_array()) {
                        return std::unexpected(cli_error{cli_error_kind::parse, "chance_events.outcomes must be an array."});
                    }
                    for (const auto& outcome_value : outcomes->as_array()) {
                        auto parsed = parse_solved_node_action(outcome_value);
                        if (!parsed) {
                            return std::unexpected(parsed.error());
                        }
                        event_record.outcomes.push_back(std::move(*parsed));
                    }
                }
                artifact.chance_events.push_back(std::move(event_record));
            }
        }

        if (const auto* runouts_value = find_value(*root, "runouts"); runouts_value != nullptr) {
            if (!runouts_value->is_array()) {
                return std::unexpected(cli_error{cli_error_kind::parse, "runouts must be an array."});
            }
            for (const auto& runout_value : runouts_value->as_array()) {
                if (!runout_value.is_object()) {
                    return std::unexpected(cli_error{cli_error_kind::parse, "runouts entries must be objects."});
                }
                const auto& object = runout_value.as_object();
                auto id = required_uint<uint32_t>(object, "id");
                auto root_state = required_uint<uint32_t>(object, "root_public_state_id");
                auto river_state = required_uint<uint32_t>(object, "river_public_state_id");
                auto dealt_turn = required_string_array(object, "dealt_turn");
                auto dealt_river = required_string_array(object, "dealt_river");
                if (!id) return std::unexpected(id.error());
                if (!root_state) return std::unexpected(root_state.error());
                if (!river_state) return std::unexpected(river_state.error());
                if (!dealt_turn) return std::unexpected(dealt_turn.error());
                if (!dealt_river) return std::unexpected(dealt_river.error());
                artifact.runouts.push_back(solve_artifact_runout{
                    .id = *id,
                    .root_public_state_id = *root_state,
                    .river_public_state_id = *river_state,
                    .dealt_turn = std::move(*dealt_turn),
                    .dealt_river = std::move(*dealt_river)
                });
            }
        }

        if (const auto* solved_nodes_value = find_value(*root, "solved_nodes"); solved_nodes_value != nullptr) {
            if (!solved_nodes_value->is_array()) {
                return std::unexpected(cli_error{cli_error_kind::parse, "solved_nodes must be an array."});
            }
            for (const auto& node_value : solved_nodes_value->as_array()) {
                if (!node_value.is_object()) {
                    return std::unexpected(cli_error{cli_error_kind::parse, "solved_nodes entries must be objects."});
                }
                const auto& object = node_value.as_object();
                auto node_id = required_uint<uint32_t>(object, "node_id");
                auto kind = required_string(object, "kind");
                auto public_state_id = nullable_uint32(object, "public_state_id", cfr::INVALID_PUBLIC_STATE_ID);
                auto parent_node_id = nullable_uint32(object, "parent_node_id", cfr::game_graph::INVALID_NODE);
                auto acting_seat = nullable_uint8(object, "acting_seat", cfr::solver::INVALID_PLAYER);
                auto board = required_string_array(object, "board");
                if (!node_id) return std::unexpected(node_id.error());
                if (!kind) return std::unexpected(kind.error());
                if (!public_state_id) return std::unexpected(public_state_id.error());
                if (!parent_node_id) return std::unexpected(parent_node_id.error());
                if (!acting_seat) return std::unexpected(acting_seat.error());
                if (!board) return std::unexpected(board.error());
                solved_node node{
                    .node_id = *node_id,
                    .kind = std::move(*kind),
                    .public_state_id = *public_state_id,
                    .parent_node_id = *parent_node_id,
                    .acting_seat = *acting_seat,
                    .terminal = object.if_contains("terminal") != nullptr && object.at("terminal").as_bool(),
                    .board = std::move(*board)
                };
                if (const auto* actions_value = find_value(object, "actions"); actions_value != nullptr) {
                    if (!actions_value->is_array()) {
                        return std::unexpected(cli_error{cli_error_kind::parse, "solved_nodes.actions must be an array."});
                    }
                    for (const auto& action_value : actions_value->as_array()) {
                        auto parsed = parse_solved_node_action(action_value);
                        if (!parsed) {
                            return std::unexpected(parsed.error());
                        }
                        node.actions.push_back(std::move(*parsed));
                    }
                }
                if (const auto* strategy_value = find_value(object, "range_action_frequencies"); strategy_value != nullptr) {
                    if (!strategy_value->is_array()) {
                        return std::unexpected(cli_error{cli_error_kind::parse, "solved_nodes.range_action_frequencies must be an array."});
                    }
                    for (const auto& action_value : strategy_value->as_array()) {
                        auto action = parse_action_strategy(action_value);
                        if (!action) {
                            return std::unexpected(action.error());
                        }
                        node.range_action_frequencies.push_back(std::move(*action));
                    }
                }
                const auto* seat_values_value = find_value(object, "seat_values");
                if (seat_values_value == nullptr || !seat_values_value->is_array()) {
                    return std::unexpected(cli_error{cli_error_kind::parse, "solved_nodes.seat_values must be an array."});
                }
                for (const auto& seat_value : seat_values_value->as_array()) {
                    auto parsed_seat_value = parse_solved_node_seat_value(seat_value);
                    if (!parsed_seat_value) {
                        return std::unexpected(parsed_seat_value.error());
                    }
                    node.seat_values.push_back(std::move(*parsed_seat_value));
                }
                const auto* derived_value = find_value(object, "derived");
                if (derived_value == nullptr || !derived_value->is_object()) {
                    return std::unexpected(cli_error{cli_error_kind::parse, "solved_nodes.derived must be an object."});
                }
                const auto& derived_object = derived_value->as_object();
                const auto* category_summaries_value = find_value(derived_object, "category_summaries");
                if (category_summaries_value == nullptr || !category_summaries_value->is_object()) {
                    return std::unexpected(cli_error{cli_error_kind::parse, "solved_nodes.derived.category_summaries must be an object."});
                }
                const auto& category_summaries_object = category_summaries_value->as_object();
                auto derivation_version = required_uint<uint32_t>(category_summaries_object, "derivation_version");
                if (!derivation_version) {
                    return std::unexpected(derivation_version.error());
                }
                node.category_summary_derivation_version = *derivation_version;
                const auto* category_items_value = find_value(category_summaries_object, "items");
                if (category_items_value == nullptr || !category_items_value->is_array()) {
                    return std::unexpected(cli_error{cli_error_kind::parse, "solved_nodes.derived.category_summaries.items must be an array."});
                }
                for (const auto& category_item : category_items_value->as_array()) {
                    auto parsed_item = parse_category_summary_item(category_item);
                    if (!parsed_item) {
                        return std::unexpected(parsed_item.error());
                    }
                    node.category_summaries.push_back(std::move(*parsed_item));
                }
                if (const auto* rows_value = find_value(object, "strategy_rows"); rows_value != nullptr) {
                    if (!rows_value->is_array()) {
                        return std::unexpected(cli_error{cli_error_kind::parse, "solved_nodes.strategy_rows must be an array."});
                    }
                    for (const auto& row_value : rows_value->as_array()) {
                        auto row = parse_hand_strategy(row_value);
                        if (!row) {
                            return std::unexpected(row.error());
                        }
                        node.strategy_rows.push_back(std::move(*row));
                    }
                }
                artifact.solved_nodes.push_back(std::move(node));
            }
        }

        if (auto validation = validate_artifact(artifact); !validation) {
            return std::unexpected(validation.error());
        }
        return artifact;
    }

    std::string serialize_artifact_json(const solve_artifact& artifact, const solve_artifact_export_mode mode)
    {
        json::object out;
        out["schema_version"] = static_cast<uint64_t>(artifact.schema_version);
        out["extraction_version"] = static_cast<uint64_t>(artifact.extraction_version);
        out["game"] = artifact.game;
        out["street"] = artifact.street;
        out["players"] = string_array_json(artifact.players);
        out["board"] = string_array_json(artifact.board);
        out["hero_seat"] = static_cast<uint64_t>(artifact.hero_seat);
        out["solver"] = solver_json(artifact.solver);
        out["root_strategy"] = action_strategy_json(artifact.root_strategy);
        if (mode != solve_artifact_export_mode::summary) {
            out["strategy"] = strategy_json(artifact.strategy);
        } else {
            out["strategy"] = json::array{};
        }
        out["public_states"] = public_states_json(artifact.public_states);
        out["chance_events"] = chance_events_json(artifact.chance_events);
        out["runouts"] = runouts_json(artifact.runouts);
        out["solved_nodes"] = solved_nodes_json(artifact.solved_nodes, mode);
        return json::serialize(out);
    }

}
