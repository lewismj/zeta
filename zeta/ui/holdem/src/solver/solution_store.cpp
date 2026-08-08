#include "solver/solution_store.h"

#include <boost/json.hpp>

#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <map>
#include <numeric>
#include <sstream>
#include <utility>

namespace zeta::holdem::ui::solver {

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

        [[nodiscard]] std::string json_string(const json::value& value)
        {
            const auto& string = value.as_string();
            return std::string{string.data(), string.size()};
        }

        [[nodiscard]] std::expected<json::object, solution_store_error> parse_object(
            const std::string_view text,
            const char* label)
        {
            boost::system::error_code ec;
            auto value = json::parse(text, ec);
            if (ec) {
                return std::unexpected(solution_store_error{solution_store_error_kind::parse, ec.message()});
            }
            if (!value.is_object()) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    std::string{label} + " JSON must be an object."
                });
            }
            return std::move(value.as_object());
        }

        [[nodiscard]] std::expected<std::string, solution_store_error> string_value(
            const json::value& value,
            const std::string_view key)
        {
            if (!value.is_string()) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    key_name(key) + " must be a string."
                });
            }
            return json_string(value);
        }

        [[nodiscard]] std::expected<std::string, solution_store_error> required_string(
            const json::object& object,
            const std::string_view key)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    "Missing " + key_name(key) + " field."
                });
            }
            return string_value(*value, key);
        }

        [[nodiscard]] std::expected<uint64_t, solution_store_error> uint64_value(
            const json::value& value,
            const std::string_view key)
        {
            if (value.is_uint64()) {
                return value.as_uint64();
            }
            if (value.is_int64() && value.as_int64() >= 0) {
                return static_cast<uint64_t>(value.as_int64());
            }
            return std::unexpected(solution_store_error{
                solution_store_error_kind::parse,
                key_name(key) + " must be a non-negative integer."
            });
        }

        template <typename T>
        [[nodiscard]] std::expected<T, solution_store_error> required_uint(
            const json::object& object,
            const std::string_view key)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    "Missing " + key_name(key) + " field."
                });
            }
            auto parsed = uint64_value(*value, key);
            if (!parsed) {
                return std::unexpected(parsed.error());
            }
            if (*parsed > static_cast<uint64_t>(std::numeric_limits<T>::max())) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    key_name(key) + " is out of range."
                });
            }
            return static_cast<T>(*parsed);
        }

        [[nodiscard]] std::expected<double, solution_store_error> number_value(
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
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    key_name(key) + " must be a number."
                });
            }
            if (!std::isfinite(out)) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    key_name(key) + " must be finite."
                });
            }
            return out;
        }

        [[nodiscard]] std::expected<double, solution_store_error> required_number(
            const json::object& object,
            const std::string_view key)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    "Missing " + key_name(key) + " field."
                });
            }
            return number_value(*value, key);
        }

        [[nodiscard]] std::expected<std::vector<std::string>, solution_store_error> string_array(
            const json::value& value,
            const std::string_view key)
        {
            if (!value.is_array()) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    key_name(key) + " must be an array."
                });
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

        [[nodiscard]] std::expected<std::vector<std::string>, solution_store_error> required_string_array(
            const json::object& object,
            const std::string_view key)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    "Missing " + key_name(key) + " array."
                });
            }
            return string_array(*value, key);
        }

        [[nodiscard]] std::expected<std::vector<double>, solution_store_error> required_number_array(
            const json::object& object,
            const std::string_view key)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr || !value->is_array()) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    "Missing " + key_name(key) + " array."
                });
            }
            std::vector<double> out;
            out.reserve(value->as_array().size());
            for (const auto& element : value->as_array()) {
                auto parsed = number_value(element, key);
                if (!parsed) {
                    return std::unexpected(parsed.error());
                }
                out.push_back(*parsed);
            }
            return out;
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

        [[nodiscard]] json::array number_array_json(const std::vector<double>& values)
        {
            json::array out;
            out.reserve(values.size());
            for (const auto value : values) {
                out.emplace_back(value);
            }
            return out;
        }

        [[nodiscard]] std::string compatibility_text(const solution_compatibility_mode mode)
        {
            return std::string{to_string(mode)};
        }

        [[nodiscard]] std::expected<solution_compatibility_mode, solution_store_error> parse_compatibility_mode(
            const std::string& text)
        {
            if (text == "root-only-artifact") {
                return solution_compatibility_mode::root_only_artifact;
            }
            if (text == "action-tree") {
                return solution_compatibility_mode::action_tree;
            }
            return std::unexpected(solution_store_error{
                solution_store_error_kind::parse,
                "Unsupported solution compatibility mode."
            });
        }

        [[nodiscard]] solution_source_summary make_source_summary(
            const struct cli::solve_spot& spot,
            const cli::solve_artifact& artifact)
        {
            return solution_source_summary{
                .game = artifact.game,
                .street = artifact.street,
                .players = artifact.players,
                .board = artifact.board,
                .hero_seat = artifact.hero_seat,
                .root_actor = spot.root_actor,
                .solver = artifact.solver
            };
        }

        [[nodiscard]] std::vector<solution_action_summary> aggregate_root_strategy(
            const cli::solve_artifact& artifact)
        {
            std::map<std::string, double> frequency_totals;
            std::map<std::string, double> weighted_evs;
            std::map<std::string, double> ev_weights;
            for (const auto& row : artifact.strategy) {
                for (const auto& action : row.strategy) {
                    const auto frequency = std::max(0.0, action.frequency);
                    frequency_totals[action.action] += frequency;
                    weighted_evs[action.action] += frequency * row.ev;
                    ev_weights[action.action] += frequency;
                }
            }

            const auto row_count = static_cast<double>(std::max<std::size_t>(artifact.strategy.size(), 1u));
            std::vector<solution_action_summary> out;
            out.reserve(frequency_totals.size());
            for (const auto& [action, total] : frequency_totals) {
                const auto average_ev = ev_weights[action] > 0.000001 ? weighted_evs[action] / ev_weights[action] : 0.0;
                out.push_back(solution_action_summary{
                    .action = action,
                    .frequency = total / row_count,
                    .average_ev = average_ev
                });
            }
            std::ranges::sort(out, [](const auto& lhs, const auto& rhs) {
                if (std::abs(lhs.frequency - rhs.frequency) > 0.000001) {
                    return lhs.frequency > rhs.frequency;
                }
                return lhs.action < rhs.action;
            });
            return out;
        }

        [[nodiscard]] std::vector<solution_seat_ev> aggregate_root_evs(const cli::solve_artifact& artifact)
        {
            if (artifact.strategy.empty()) {
                return {};
            }
            double total = 0.0;
            for (const auto& row : artifact.strategy) {
                total += row.ev;
            }
            return {
                solution_seat_ev{
                    .seat = artifact.hero_seat,
                    .ev = total / static_cast<double>(artifact.strategy.size())
                }
            };
        }

        [[nodiscard]] std::vector<std::string> root_action_labels_from_artifact(const cli::solve_artifact& artifact)
        {
            std::vector<std::string> labels;
            if (artifact.strategy.empty()) {
                return labels;
            }
            labels.reserve(artifact.strategy.front().strategy.size());
            for (const auto& action : artifact.strategy.front().strategy) {
                labels.push_back(action.action);
            }
            return labels;
        }

        [[nodiscard]] solution_table_state root_table_state_from_spot(const struct cli::solve_spot& spot)
        {
            solution_table_state table;
            table.stacks.assign(spot.stacks.begin(), spot.stacks.end());
            table.commitments.assign(spot.contributions.begin(), spot.contributions.end());
            table.pot = std::accumulate(table.commitments.begin(), table.commitments.end(), 0.0);
            if (table.pot <= 0.0) {
                table.pot = spot.gross_pot;
            }
            return table;
        }

        [[nodiscard]] solution_node make_root_node_from_artifact(
            const struct cli::solve_spot& spot,
            const cli::solve_artifact& artifact)
        {
            return solution_node{
                .node_id = "root",
                .path = {},
                .acting_seat = spot.root_actor,
                .terminal = false,
                .truncated = false,
                .legal_actions = root_action_labels_from_artifact(artifact),
                .average_strategy = aggregate_root_strategy(artifact),
                .seat_evs = aggregate_root_evs(artifact),
                .table_state = root_table_state_from_spot(spot),
                .children = {}
            };
        }

        [[nodiscard]] std::string child_node_id(
            const std::string& parent,
            const uint16_t action_index,
            const std::string& action_label)
        {
            return parent + "/" + std::to_string(action_index) + "-" + action_label;
        }

        template <std::size_t N>
        [[nodiscard]] solution_table_state table_state_from_betting(const cfr::betting_state<N>& state)
        {
            solution_table_state table;
            table.stacks.reserve(N);
            table.commitments.reserve(N);
            for (std::size_t seat = 0; seat < N; ++seat) {
                table.stacks.push_back(state.stacks[seat]);
                table.commitments.push_back(state.committed[seat]);
                table.pot += state.committed[seat];
            }
            return table;
        }

        template <std::size_t N>
        void append_betting_tree_nodes(
            solution_store& store,
            const cfr::holdem_betting_graph_config<N>& config,
            const struct cli::solve_spot& spot,
            const cfr::betting_state<N>& state,
            const std::string& node_id,
            const std::vector<std::string>& path)
        {
            const auto at_history_limit = state.action_history.size() >= config.max_history;
            auto actions = state.terminal() || at_history_limit
                ? std::vector<cfr::betting_action>{}
                : cfr::legal_betting_actions(state, config.abstraction);

            solution_node node;
            node.node_id = node_id;
            node.path = path;
            node.acting_seat = state.terminal() ? invalid_solution_seat : state.actor;
            node.terminal = state.terminal();
            node.truncated = !state.terminal() && at_history_limit;
            node.table_state = table_state_from_betting(state);
            node.legal_actions.reserve(actions.size());
            node.children.reserve(actions.size());
            for (uint16_t action_index = 0; action_index < static_cast<uint16_t>(actions.size()); ++action_index) {
                const auto label = cli::detail::action_label(actions[action_index], spot.gross_pot);
                node.legal_actions.push_back(label);
                node.children.push_back(child_node_id(node_id, action_index, label));
            }
            store.nodes.push_back(std::move(node));

            for (uint16_t action_index = 0; action_index < static_cast<uint16_t>(actions.size()); ++action_index) {
                auto child = cfr::apply_betting_action(state, actions[action_index], config.abstraction);
                if (!child) {
                    store.diagnostics.push_back("A betting-tree child could not be applied.");
                    continue;
                }
                auto child_path = path;
                child_path.push_back(cli::detail::action_label(actions[action_index], spot.gross_pot));
                append_betting_tree_nodes(
                    store,
                    config,
                    spot,
                    *child,
                    child_node_id(node_id, action_index, child_path.back()),
                    child_path);
            }
        }

        template <std::size_t N>
        [[nodiscard]] bool populate_action_tree_nodes(solution_store& store, const struct cli::solve_spot& spot)
        {
            auto parsed_street = cli::detail::parse_holdem_street(spot.street);
            if (!parsed_street) {
                store.diagnostics.push_back(parsed_street.error().message);
                return false;
            }

            cfr::holdem_betting_graph_config<N> config{};
            config.street = *parsed_street;
            for (std::size_t seat = 0; seat < N; ++seat) {
                config.initial_stacks[seat] = spot.stacks[seat];
                config.initial_committed[seat] = spot.contributions[seat];
            }
            config.root_actor = spot.root_actor;
            config.abstraction.fixed_pot_fractions = {spot.bet_fraction};
            config.abstraction.geometric_size_count = 1;
            config.abstraction.stack_ratio_buckets = {spot.bet_fraction};
            config.abstraction.max_raises_per_street = 1;
            config.max_history = spot.max_history;
            config.public_state_id = spot.public_state_id;

            const auto initial_state = cfr::make_initial_betting_state(config);
            if (auto validation = cfr::validate_betting_state(initial_state); !validation) {
                store.diagnostics.push_back("Initial betting state is invalid.");
                return false;
            }

            append_betting_tree_nodes(store, config, spot, initial_state, store.root_node_id, {});
            return !store.nodes.empty();
        }

        [[nodiscard]] bool populate_action_tree_nodes_for_player_count(solution_store& store, const struct cli::solve_spot& spot)
        {
            switch (spot.players.size()) {
                case 2: return populate_action_tree_nodes<2>(store, spot);
                case 3: return populate_action_tree_nodes<3>(store, spot);
                case 4: return populate_action_tree_nodes<4>(store, spot);
                case 5: return populate_action_tree_nodes<5>(store, spot);
                case 6: return populate_action_tree_nodes<6>(store, spot);
                default:
                    store.diagnostics.push_back("Player count is outside the supported solution range.");
                    return false;
            }
        }

        void apply_root_artifact_data(solution_store& store, const cli::solve_artifact& artifact)
        {
            auto root = std::ranges::find_if(store.nodes, [&store](const auto& node) {
                return node.node_id == store.root_node_id;
            });
            if (root == store.nodes.end()) {
                return;
            }
            root->average_strategy = aggregate_root_strategy(artifact);
            root->seat_evs = aggregate_root_evs(artifact);
            if (root->legal_actions.empty()) {
                root->legal_actions = root_action_labels_from_artifact(artifact);
            }
        }

        [[nodiscard]] std::expected<solution_action_summary, solution_store_error> parse_action_summary(
            const json::value& value)
        {
            if (!value.is_object()) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    "Action summary must be an object."
                });
            }
            const auto& object = value.as_object();
            auto action = required_string(object, "action");
            auto frequency = required_number(object, "frequency");
            auto average_ev = required_number(object, "average_ev");
            if (!action) {
                return std::unexpected(action.error());
            }
            if (!frequency) {
                return std::unexpected(frequency.error());
            }
            if (!average_ev) {
                return std::unexpected(average_ev.error());
            }
            return solution_action_summary{
                .action = std::move(*action),
                .frequency = *frequency,
                .average_ev = *average_ev
            };
        }

        [[nodiscard]] std::expected<solution_seat_ev, solution_store_error> parse_seat_ev(const json::value& value)
        {
            if (!value.is_object()) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    "Seat EV must be an object."
                });
            }
            const auto& object = value.as_object();
            auto seat = required_uint<uint8_t>(object, "seat");
            auto ev = required_number(object, "ev");
            if (!seat) {
                return std::unexpected(seat.error());
            }
            if (!ev) {
                return std::unexpected(ev.error());
            }
            return solution_seat_ev{.seat = *seat, .ev = *ev};
        }

        [[nodiscard]] std::expected<solution_table_state, solution_store_error> parse_table_state(
            const json::value& value)
        {
            if (!value.is_object()) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    "table_state must be an object."
                });
            }
            const auto& object = value.as_object();
            auto pot = required_number(object, "pot");
            auto stacks = required_number_array(object, "stacks");
            auto commitments = required_number_array(object, "commitments");
            if (!pot) {
                return std::unexpected(pot.error());
            }
            if (!stacks) {
                return std::unexpected(stacks.error());
            }
            if (!commitments) {
                return std::unexpected(commitments.error());
            }
            return solution_table_state{
                .pot = *pot,
                .stacks = std::move(*stacks),
                .commitments = std::move(*commitments)
            };
        }

        [[nodiscard]] std::expected<solution_node, solution_store_error> parse_solution_node(const json::value& value)
        {
            if (!value.is_object()) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    "Solution node must be an object."
                });
            }
            const auto& object = value.as_object();
            auto node_id = required_string(object, "node_id");
            const auto* path_value = find_value(object, "path");
            const auto* legal_actions_value = find_value(object, "legal_actions");
            const auto* children_value = find_value(object, "children");
            const auto* table_state_value = find_value(object, "table_state");
            if (!node_id) {
                return std::unexpected(node_id.error());
            }
            if (path_value == nullptr || legal_actions_value == nullptr || children_value == nullptr || table_state_value == nullptr) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    "Solution node is missing path, legal_actions, children, or table_state."
                });
            }
            auto path = string_array(*path_value, "path");
            auto legal_actions = string_array(*legal_actions_value, "legal_actions");
            auto children = string_array(*children_value, "children");
            auto table_state = parse_table_state(*table_state_value);
            if (!path) {
                return std::unexpected(path.error());
            }
            if (!legal_actions) {
                return std::unexpected(legal_actions.error());
            }
            if (!children) {
                return std::unexpected(children.error());
            }
            if (!table_state) {
                return std::unexpected(table_state.error());
            }

            uint8_t acting_seat = invalid_solution_seat;
            if (const auto* acting = find_value(object, "acting_seat"); acting != nullptr && !acting->is_null()) {
                auto parsed = uint64_value(*acting, "acting_seat");
                if (!parsed) {
                    return std::unexpected(parsed.error());
                }
                if (*parsed > static_cast<uint64_t>(std::numeric_limits<uint8_t>::max())) {
                    return std::unexpected(solution_store_error{
                        solution_store_error_kind::parse,
                        "acting_seat is out of range."
                    });
                }
                acting_seat = static_cast<uint8_t>(*parsed);
            }

            solution_node node;
            node.node_id = std::move(*node_id);
            node.path = std::move(*path);
            node.acting_seat = acting_seat;
            if (const auto* terminal = find_value(object, "terminal"); terminal != nullptr) {
                if (!terminal->is_bool()) {
                    return std::unexpected(solution_store_error{
                        solution_store_error_kind::parse,
                        "terminal must be a boolean."
                    });
                }
                node.terminal = terminal->as_bool();
            }
            if (const auto* truncated = find_value(object, "truncated"); truncated != nullptr) {
                if (!truncated->is_bool()) {
                    return std::unexpected(solution_store_error{
                        solution_store_error_kind::parse,
                        "truncated must be a boolean."
                    });
                }
                node.truncated = truncated->as_bool();
            }
            node.legal_actions = std::move(*legal_actions);
            node.table_state = std::move(*table_state);
            node.children = std::move(*children);

            if (const auto* strategy = find_value(object, "average_strategy"); strategy != nullptr) {
                if (!strategy->is_array()) {
                    return std::unexpected(solution_store_error{
                        solution_store_error_kind::parse,
                        "average_strategy must be an array."
                    });
                }
                node.average_strategy.reserve(strategy->as_array().size());
                for (const auto& action_value : strategy->as_array()) {
                    auto parsed = parse_action_summary(action_value);
                    if (!parsed) {
                        return std::unexpected(parsed.error());
                    }
                    node.average_strategy.push_back(std::move(*parsed));
                }
            }

            if (const auto* evs = find_value(object, "seat_evs"); evs != nullptr) {
                if (!evs->is_array()) {
                    return std::unexpected(solution_store_error{
                        solution_store_error_kind::parse,
                        "seat_evs must be an array."
                    });
                }
                node.seat_evs.reserve(evs->as_array().size());
                for (const auto& ev_value : evs->as_array()) {
                    auto parsed = parse_seat_ev(ev_value);
                    if (!parsed) {
                        return std::unexpected(parsed.error());
                    }
                    node.seat_evs.push_back(*parsed);
                }
            }

            return node;
        }

        [[nodiscard]] std::expected<solution_source_summary, solution_store_error> parse_source_summary(
            const json::value& value)
        {
            if (!value.is_object()) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    "source must be an object."
                });
            }
            const auto& object = value.as_object();
            auto game = required_string(object, "game");
            auto street = required_string(object, "street");
            auto players = required_string_array(object, "players");
            auto board = required_string_array(object, "board");
            auto hero = required_uint<uint8_t>(object, "hero_seat");
            auto root_actor = required_uint<uint8_t>(object, "root_actor");
            const auto* solver_value = find_value(object, "solver");
            if (!game) {
                return std::unexpected(game.error());
            }
            if (!street) {
                return std::unexpected(street.error());
            }
            if (!players) {
                return std::unexpected(players.error());
            }
            if (!board) {
                return std::unexpected(board.error());
            }
            if (!hero) {
                return std::unexpected(hero.error());
            }
            if (!root_actor) {
                return std::unexpected(root_actor.error());
            }
            if (solver_value == nullptr || !solver_value->is_object()) {
                return std::unexpected(solution_store_error{
                    solution_store_error_kind::parse,
                    "source.solver must be an object."
                });
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

            return solution_source_summary{
                .game = std::move(*game),
                .street = std::move(*street),
                .players = std::move(*players),
                .board = std::move(*board),
                .hero_seat = *hero,
                .root_actor = *root_actor,
                .solver = cli::solver_metadata{
                    .algorithm = std::move(*algorithm),
                    .iterations = *iterations,
                    .timestamp = std::move(*timestamp),
                    .git_revision = std::move(*git_revision)
                }
            };
        }

        [[nodiscard]] json::object solver_json(const cli::solver_metadata& solver)
        {
            json::object out;
            out["algorithm"] = solver.algorithm;
            out["iterations"] = solver.iterations;
            out["timestamp"] = solver.timestamp;
            out["git_revision"] = solver.git_revision;
            return out;
        }

        [[nodiscard]] json::object source_json(const solution_source_summary& source)
        {
            json::object out;
            out["game"] = source.game;
            out["street"] = source.street;
            out["players"] = string_array_json(source.players);
            out["board"] = string_array_json(source.board);
            out["hero_seat"] = static_cast<uint64_t>(source.hero_seat);
            out["root_actor"] = static_cast<uint64_t>(source.root_actor);
            out["solver"] = solver_json(source.solver);
            return out;
        }

        [[nodiscard]] json::array action_summary_json(const std::vector<solution_action_summary>& actions)
        {
            json::array out;
            out.reserve(actions.size());
            for (const auto& action : actions) {
                json::object object;
                object["action"] = action.action;
                object["frequency"] = action.frequency;
                object["average_ev"] = action.average_ev;
                out.emplace_back(std::move(object));
            }
            return out;
        }

        [[nodiscard]] json::array seat_evs_json(const std::vector<solution_seat_ev>& evs)
        {
            json::array out;
            out.reserve(evs.size());
            for (const auto& ev : evs) {
                json::object object;
                object["seat"] = static_cast<uint64_t>(ev.seat);
                object["ev"] = ev.ev;
                out.emplace_back(std::move(object));
            }
            return out;
        }

        [[nodiscard]] json::object table_state_json(const solution_table_state& table)
        {
            json::object out;
            out["pot"] = table.pot;
            out["stacks"] = number_array_json(table.stacks);
            out["commitments"] = number_array_json(table.commitments);
            return out;
        }

        [[nodiscard]] json::array nodes_json(const std::vector<solution_node>& nodes)
        {
            json::array out;
            out.reserve(nodes.size());
            for (const auto& node : nodes) {
                json::object object;
                object["node_id"] = node.node_id;
                object["path"] = string_array_json(node.path);
                object["acting_seat"] = node.acting_seat == invalid_solution_seat
                    ? json::value{nullptr}
                    : json::value{static_cast<uint64_t>(node.acting_seat)};
                object["terminal"] = node.terminal;
                object["truncated"] = node.truncated;
                object["legal_actions"] = string_array_json(node.legal_actions);
                object["average_strategy"] = action_summary_json(node.average_strategy);
                object["seat_evs"] = seat_evs_json(node.seat_evs);
                object["table_state"] = table_state_json(node.table_state);
                object["children"] = string_array_json(node.children);
                out.emplace_back(std::move(object));
            }
            return out;
        }

    }

    solution_store make_root_only_solution_store(const struct cli::solve_spot& spot, const cli::solve_artifact& artifact)
    {
        solution_store store;
        store.compatibility_mode = solution_compatibility_mode::root_only_artifact;
        store.source = make_source_summary(spot, artifact);
        store.nodes.push_back(make_root_node_from_artifact(spot, artifact));
        store.diagnostics.push_back("Loaded root-only artifact; node descendants and non-root strategies are unavailable.");
        return store;
    }

    solution_store make_action_tree_solution_store(const struct cli::solve_spot& spot, const cli::solve_artifact& artifact)
    {
        solution_store store;
        store.compatibility_mode = solution_compatibility_mode::action_tree;
        store.source = make_source_summary(spot, artifact);
        if (!populate_action_tree_nodes_for_player_count(store, spot)) {
            auto fallback = make_root_only_solution_store(spot, artifact);
            fallback.diagnostics.push_back("Betting-tree extraction failed; saved root-only solution data.");
            return fallback;
        }
        apply_root_artifact_data(store, artifact);
        store.diagnostics.push_back("Average strategy is available for the root node in the current solver artifact.");
        return store;
    }

    std::expected<solution_store, solution_store_error> parse_solution_store_json(const std::string_view text)
    {
        auto root = parse_object(text, "Solution");
        if (!root) {
            return std::unexpected(root.error());
        }

        auto schema_version = required_uint<uint32_t>(*root, "solution_schema_version");
        auto compatibility = required_string(*root, "compatibility");
        auto root_node_id = required_string(*root, "root_node_id");
        const auto* source_value = find_value(*root, "source");
        const auto* nodes_value = find_value(*root, "nodes");
        if (!schema_version) {
            return std::unexpected(schema_version.error());
        }
        if (*schema_version != current_solution_schema_version) {
            return std::unexpected(solution_store_error{
                solution_store_error_kind::invalid_solution,
                "Unsupported solution_schema_version."
            });
        }
        if (!compatibility) {
            return std::unexpected(compatibility.error());
        }
        auto parsed_mode = parse_compatibility_mode(*compatibility);
        if (!parsed_mode) {
            return std::unexpected(parsed_mode.error());
        }
        if (!root_node_id) {
            return std::unexpected(root_node_id.error());
        }
        if (source_value == nullptr || nodes_value == nullptr || !nodes_value->is_array()) {
            return std::unexpected(solution_store_error{
                solution_store_error_kind::parse,
                "Solution requires source and nodes."
            });
        }

        auto source = parse_source_summary(*source_value);
        if (!source) {
            return std::unexpected(source.error());
        }

        solution_store store;
        store.schema_version = *schema_version;
        store.compatibility_mode = *parsed_mode;
        store.root_node_id = std::move(*root_node_id);
        store.source = std::move(*source);
        store.nodes.reserve(nodes_value->as_array().size());
        for (const auto& node_value : nodes_value->as_array()) {
            auto node = parse_solution_node(node_value);
            if (!node) {
                return std::unexpected(node.error());
            }
            store.nodes.push_back(std::move(*node));
        }
        if (find_solution_node(store, store.root_node_id) == nullptr) {
            return std::unexpected(solution_store_error{
                solution_store_error_kind::invalid_solution,
                "Solution root node is missing."
            });
        }

        if (const auto* diagnostics = find_value(*root, "diagnostics"); diagnostics != nullptr) {
            auto parsed = string_array(*diagnostics, "diagnostics");
            if (!parsed) {
                return std::unexpected(parsed.error());
            }
            store.diagnostics = std::move(*parsed);
        }
        return store;
    }

    std::string serialize_solution_store_json(const solution_store& store)
    {
        json::object root;
        root["solution_schema_version"] = static_cast<uint64_t>(current_solution_schema_version);
        root["compatibility"] = compatibility_text(store.compatibility_mode);
        root["root_node_id"] = store.root_node_id;
        root["source"] = source_json(store.source);
        root["nodes"] = nodes_json(store.nodes);
        root["diagnostics"] = string_array_json(store.diagnostics);
        return json::serialize(root);
    }

    const solution_node* find_solution_node(const solution_store& store, const std::string_view node_id) noexcept
    {
        const auto found = std::ranges::find_if(store.nodes, [node_id](const auto& node) {
            return node.node_id == node_id;
        });
        return found == store.nodes.end() ? nullptr : &*found;
    }

    const solution_node* root_solution_node(const solution_store& store) noexcept
    {
        return find_solution_node(store, store.root_node_id);
    }

    std::string_view to_string(const solution_compatibility_mode mode) noexcept
    {
        switch (mode) {
            case solution_compatibility_mode::root_only_artifact: return "root-only-artifact";
            case solution_compatibility_mode::action_tree: return "action-tree";
        }
        return "root-only-artifact";
    }

}
