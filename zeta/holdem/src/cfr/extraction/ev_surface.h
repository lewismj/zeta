#pragma once

#include "cfr/extraction/equity_surface.h"
#include "cfr/extraction/strategy_surface.h"
#include "cfr/chance/chance.h"
#include "cfr/solver/metadata.h"
#include "cfr/traversal/traversal.h"
#include "terminal/terminal.h"

#include <array>
#include <cassert>
#include <expected>
#include <functional>
#include <limits>
#include <span>
#include <string>
#include <unordered_map>

namespace zeta::holdem::cfr::extraction {

    struct river_heads_up_extraction_input {
        const cfr::game_graph* graph = nullptr;
        const cfr::solver::solver_graph_annotations* annotations = nullptr;
        const cfr::strategy_sum_table* strategy_sums = nullptr;
        const cfr::chance_event_table* chance_events = nullptr;
        const river_terminal_cache* river_cache = nullptr;
        std::span<const cfr::traversal::river_terminal_leaf> terminal_leaves{};
        std::span<const terminal_state<2>> terminal_states{};
        std::array<reach_vector, 2> ranges{};
    };

    enum class river_extraction_error_kind : uint8_t {
        missing_input,
        metadata_size_mismatch,
        invalid_chance_table,
        invalid_actor,
        invalid_infoset,
        invalid_terminal_leaf,
        unsupported_node,
        value_cycle
    };

    struct river_extraction_error {
        river_extraction_error_kind kind{};
        uint32_t node_id = cfr::game_graph::INVALID_NODE;
    };

    [[nodiscard]] constexpr const char* to_string(const river_extraction_error_kind kind) noexcept
    {
        using enum river_extraction_error_kind;
        switch (kind) {
            case missing_input:          return "river_extraction_error_kind::missing_input";
            case metadata_size_mismatch: return "river_extraction_error_kind::metadata_size_mismatch";
            case invalid_chance_table:   return "river_extraction_error_kind::invalid_chance_table";
            case invalid_actor:          return "river_extraction_error_kind::invalid_actor";
            case invalid_infoset:        return "river_extraction_error_kind::invalid_infoset";
            case invalid_terminal_leaf:  return "river_extraction_error_kind::invalid_terminal_leaf";
            case unsupported_node:       return "river_extraction_error_kind::unsupported_node";
            case value_cycle:            return "river_extraction_error_kind::value_cycle";
        }
        return "river_extraction_error_kind::unknown";
    }

    namespace detail {

        struct extracted_reach_state {
            float oop = 0.0f;
            float ip = 0.0f;
            float chance = 0.0f;
        };

        [[nodiscard]] inline std::vector<combination_index> live_river_combos(const river_terminal_cache& cache)
        {
            std::vector<combination_index> combos;
            combos.reserve(cache.rank_order_count);
            for (std::size_t i = 0; i < cache.rank_order_count; ++i) {
                combos.push_back(cache.rank_order[i]);
            }
            return combos;
        }

        [[nodiscard]] inline float chance_probability_for_edge(
            const cfr::game_graph& graph,
            const cfr::chance_event_table* chance_events,
            const uint32_t node_id,
            const cfr::edge child_edge) noexcept
        {
            if (chance_events != nullptr) {
                return chance_events->probability_for_edge(node_id, child_edge);
            }
            const auto count = graph.action_count(node_id);
            return count == 0u ? 0.0f : 1.0f / static_cast<float>(count);
        }

        [[nodiscard]] inline heads_up_player player_for_actor(const uint8_t actor) noexcept
        {
            return actor == 0u ? heads_up_player::oop : heads_up_player::ip;
        }

        [[nodiscard]] inline float player_reach(const extracted_reach_state& reach, const heads_up_player player) noexcept
        {
            return player == heads_up_player::oop ? reach.oop : reach.ip;
        }

        [[nodiscard]] inline float opponent_reach(const extracted_reach_state& reach, const heads_up_player player) noexcept
        {
            return player == heads_up_player::oop ? reach.ip : reach.oop;
        }

        [[nodiscard]] inline std::expected<void, river_extraction_error> validate_input(
            const river_heads_up_extraction_input& input)
        {
            if (input.graph == nullptr
                || input.annotations == nullptr
                || input.strategy_sums == nullptr
                || input.river_cache == nullptr
                || input.terminal_leaves.empty()
                || input.terminal_states.empty()) {
                return std::unexpected(river_extraction_error{river_extraction_error_kind::missing_input});
            }

            const auto node_count = input.graph->node_count;
            if (input.annotations->actor_by_node.size() != node_count
                || input.annotations->state_by_node.size() != node_count
                || input.terminal_leaves.size() < node_count) {
                return std::unexpected(river_extraction_error{river_extraction_error_kind::metadata_size_mismatch});
            }
            if (input.chance_events != nullptr && input.chance_events->event_id_by_node.size() != node_count) {
                return std::unexpected(river_extraction_error{river_extraction_error_kind::invalid_chance_table});
            }

            for (uint32_t node_id = 0; node_id < node_count; ++node_id) {
                if (input.graph->is_player_node(node_id)) {
                    const auto actor = input.annotations->actor_by_node[node_id];
                    if (actor > 1u) {
                        return std::unexpected(river_extraction_error{river_extraction_error_kind::invalid_actor, node_id});
                    }
                    const auto infoset_id = input.graph->infoset_id[node_id];
                    if (infoset_id == cfr::game_graph::INVALID_INFOSET
                        || infoset_id >= input.strategy_sums->infoset_count()
                        || input.strategy_sums->action_count(infoset_id) != input.graph->action_count(node_id)) {
                        return std::unexpected(river_extraction_error{river_extraction_error_kind::invalid_infoset, node_id});
                    }
                }
                if (input.graph->is_terminal(node_id)
                    && input.terminal_leaves[node_id].terminal_state_id >= input.terminal_states.size()) {
                    return std::unexpected(river_extraction_error{river_extraction_error_kind::invalid_terminal_leaf, node_id});
                }
            }

            return {};
        }

    }

    [[nodiscard]] inline std::expected<result_store, river_extraction_error> extract_river_heads_up_result_store(
        const river_heads_up_extraction_input& input)
    {
        if (auto valid = detail::validate_input(input); !valid) {
            return std::unexpected(valid.error());
        }

        const auto& graph = *input.graph;
        const auto& annotations = *input.annotations;
        const auto combo_indices = detail::live_river_combos(*input.river_cache);
        const auto combo_count = static_cast<uint32_t>(combo_indices.size());

        std::vector<detail::extracted_reach_state> reach_by_node(graph.node_count);
        reach_by_node[graph.root_node] = detail::extracted_reach_state{1.0f, 1.0f, 1.0f};

        for (uint32_t node_id = graph.root_node + 1u; node_id-- > 0u;) {
            const auto parent_reach = reach_by_node[node_id];
            if (parent_reach.chance == 0.0f) {
                continue;
            }

            const auto edges = graph.out_edges(node_id);
            if (edges.empty()) {
                continue;
            }

            if (graph.is_player_node(node_id)) {
                const auto actor = detail::player_for_actor(annotations.actor_by_node[node_id]);
                const auto strategy = normalized_average_strategy(*input.strategy_sums, graph.infoset_id[node_id]);
                for (const auto child_edge : edges) {
                    auto child_reach = parent_reach;
                    if (actor == heads_up_player::oop) {
                        child_reach.oop *= strategy[child_edge.action_index];
                    } else {
                        child_reach.ip *= strategy[child_edge.action_index];
                    }
                    reach_by_node[child_edge.child_node] = child_reach;
                }
            } else if (graph.is_chance_node(node_id)) {
                for (const auto child_edge : edges) {
                    auto child_reach = parent_reach;
                    child_reach.chance *= detail::chance_probability_for_edge(
                        graph,
                        input.chance_events,
                        node_id,
                        child_edge);
                    reach_by_node[child_edge.child_node] = child_reach;
                }
            } else if (!graph.is_terminal(node_id)) {
                return std::unexpected(river_extraction_error{river_extraction_error_kind::unsupported_node, node_id});
            }
        }

        terminal_engine<2> engine{};
        std::array<river_reach_index, 2> reach_indices{
            make_river_reach_index(*input.river_cache, input.ranges[0]),
            make_river_reach_index(*input.river_cache, input.ranges[1])
        };
        std::vector<terminal_values<2>> terminal_value_cache(input.terminal_states.size());
        for (std::size_t terminal_id = 0; terminal_id < input.terminal_states.size(); ++terminal_id) {
            terminal_value_cache[terminal_id] = engine.evaluate_terminal_values(
                *input.river_cache,
                reach_indices,
                input.terminal_states[terminal_id]);
        }

        constexpr uint8_t unvisited = 0;
        constexpr uint8_t visiting = 1;
        constexpr uint8_t visited = 2;
        std::vector<std::array<std::vector<double>, 2>> value_cache(graph.node_count);
        std::vector<std::array<uint8_t, 2>> value_state(graph.node_count);

        std::function<std::expected<std::span<const double>, river_extraction_error>(
            uint32_t,
            heads_up_player)> value_for_node;

        value_for_node = [&](const uint32_t node_id, const heads_up_player perspective)
            -> std::expected<std::span<const double>, river_extraction_error> {
            const auto player_slot = player_index(perspective);
            if (value_state[node_id][player_slot] == visited) {
                return std::span<const double>{value_cache[node_id][player_slot]};
            }
            if (value_state[node_id][player_slot] == visiting) {
                return std::unexpected(river_extraction_error{river_extraction_error_kind::value_cycle, node_id});
            }

            value_state[node_id][player_slot] = visiting;
            auto& values = value_cache[node_id][player_slot];
            values.assign(combo_count, 0.0);

            if (graph.is_terminal(node_id)) {
                const auto terminal_id = input.terminal_leaves[node_id].terminal_state_id;
                const auto& terminal_values = terminal_value_cache[terminal_id][perspective];
                for (uint32_t combo = 0; combo < combo_count; ++combo) {
                    values[combo] = terminal_values[combo_indices[combo]];
                }
            } else if (graph.is_player_node(node_id)) {
                const auto strategy = normalized_average_strategy(*input.strategy_sums, graph.infoset_id[node_id]);
                for (const auto child_edge : graph.out_edges(node_id)) {
                    auto child_values = value_for_node(child_edge.child_node, perspective);
                    if (!child_values) {
                        return std::unexpected(child_values.error());
                    }
                    for (uint32_t combo = 0; combo < combo_count; ++combo) {
                        values[combo] += static_cast<double>(strategy[child_edge.action_index]) * (*child_values)[combo];
                    }
                }
            } else if (graph.is_chance_node(node_id)) {
                for (const auto child_edge : graph.out_edges(node_id)) {
                    auto child_values = value_for_node(child_edge.child_node, perspective);
                    if (!child_values) {
                        return std::unexpected(child_values.error());
                    }
                    const auto chance_probability = static_cast<double>(detail::chance_probability_for_edge(
                        graph,
                        input.chance_events,
                        node_id,
                        child_edge));
                    for (uint32_t combo = 0; combo < combo_count; ++combo) {
                        values[combo] += chance_probability * (*child_values)[combo];
                    }
                }
            } else {
                return std::unexpected(river_extraction_error{river_extraction_error_kind::unsupported_node, node_id});
            }

            value_state[node_id][player_slot] = visited;
            return std::span<const double>{values};
        };

        std::unordered_map<uint32_t, strategy_context_id> context_by_infoset;
        std::vector<node_record> nodes;
        std::vector<strategy_surface_record> strategy_surfaces;
        std::vector<strategy_surface_entry> strategy_entries;
        std::vector<combo_reach_entry> combo_reaches;
        std::vector<combo_value_entry> combo_values;
        std::vector<action_value_entry> action_values;
        std::vector<seat_value> seat_values;
        std::vector<float> equities;
        std::vector<combination_index> store_combo_indices;
        std::vector<hand_category_classification> categories;

        for (uint32_t graph_node_id = 0; graph_node_id < graph.node_count; ++graph_node_id) {
            if (!graph.is_player_node(graph_node_id)) {
                continue;
            }

            const auto action_count = static_cast<uint16_t>(graph.action_count(graph_node_id));
            const auto infoset_id = graph.infoset_id[graph_node_id];
            const auto [context_it, inserted] = context_by_infoset.emplace(
                infoset_id,
                static_cast<strategy_context_id>(strategy_surfaces.size()));
            if (inserted) {
                const auto strategy_begin = static_cast<uint32_t>(strategy_entries.size());
                strategy_surfaces.push_back(strategy_surface_record{
                    .strategy_begin = strategy_begin,
                    .combo_count = combo_count,
                    .action_count = action_count
                });
                strategy_entries.resize(strategy_entries.size() + static_cast<std::size_t>(combo_count) * action_count);
                const auto strategy = normalized_average_strategy(*input.strategy_sums, infoset_id);
                write_strategy_surface(
                    std::span<strategy_surface_entry>{strategy_entries}.subspan(strategy_begin, static_cast<std::size_t>(combo_count) * action_count),
                    strategy,
                    combo_count,
                    action_count);
            }

            const auto actor = detail::player_for_actor(annotations.actor_by_node[graph_node_id]);
            const auto actor_slot = player_index(actor);
            const auto opponent_slot = actor_slot == 0u ? 1u : 0u;
            const auto node_value = value_for_node(graph_node_id, actor);
            if (!node_value) {
                return std::unexpected(node_value.error());
            }

            const auto node_id = static_cast<uint32_t>(nodes.size());
            const auto combo_begin = static_cast<uint32_t>(combo_reaches.size());
            const auto action_value_begin = static_cast<uint32_t>(action_values.size());
            const auto seat_value_begin = static_cast<uint32_t>(seat_values.size());
            nodes.push_back(node_record{
                .node_id = node_id,
                .strategy_context_id = context_it->second,
                .public_state_id = annotations.state_by_node[graph_node_id].public_state_id,
                .combo_begin = combo_begin,
                .combo_count = combo_count,
                .action_val_begin = action_value_begin,
                .action_count = action_count,
                .seat_value_begin = seat_value_begin,
                .seat_value_count = 1
            });

            combo_reaches.resize(combo_reaches.size() + combo_count);
            combo_values.resize(combo_values.size() + combo_count);
            action_values.resize(action_values.size() + static_cast<std::size_t>(combo_count) * action_count);
            equities.resize(equities.size() + combo_count);
            store_combo_indices.insert(store_combo_indices.end(), combo_indices.begin(), combo_indices.end());
            categories.resize(categories.size() + combo_count);

            const auto& reach = reach_by_node[graph_node_id];
            const auto actor_reach = detail::player_reach(reach, actor);
            const auto opponent_reach = detail::opponent_reach(reach, actor);
            const auto& actor_range = input.ranges[actor_slot];
            const auto& opponent_range = input.ranges[opponent_slot];
            double range_reach_mass = 0.0;
            double reach_weighted_value = 0.0;
            double counterfactual_value = 0.0;

            write_river_showdown_equity_surface(
                std::span<float>{equities}.subspan(combo_begin, combo_count),
                *input.river_cache,
                opponent_range,
                opponent_reach,
                combo_indices);

            for (uint32_t combo = 0; combo < combo_count; ++combo) {
                const auto global_combo = combo_indices[combo];
                const auto range_weight = actor_range[global_combo];
                const auto value = (*node_value)[combo];
                const auto range_reach_weight = static_cast<double>(range_weight) * static_cast<double>(actor_reach);

                combo_reaches[combo_begin + combo] = combo_reach_entry{
                    .range_weight = range_weight,
                    .reach_probability = actor_reach
                };
                combo_values[combo_begin + combo] = combo_value_entry{.combo_profile_value = value};
                categories[combo_begin + combo] = categorize_hand(combination_mask(global_combo), input.river_cache->river_board);

                range_reach_mass += range_reach_weight;
                reach_weighted_value += range_reach_weight * value;
                counterfactual_value += static_cast<double>(range_weight)
                    * static_cast<double>(opponent_reach)
                    * static_cast<double>(reach.chance)
                    * value;

                for (const auto child_edge : graph.out_edges(graph_node_id)) {
                    const auto child_values = value_for_node(child_edge.child_node, actor);
                    if (!child_values) {
                        return std::unexpected(child_values.error());
                    }
                    const auto q = (*child_values)[combo];
                    action_values[action_value_begin + combo * action_count + child_edge.action_index] =
                        action_value_entry{
                            .q_profile = q,
                            .profile_advantage = compute_profile_advantage(q, value)
                        };
                }
            }

            seat_values.push_back(seat_value{
                .range_reach_mass = range_reach_mass,
                .reach_weighted_value = reach_weighted_value,
                .conditional_range_ev = compute_conditional_range_ev(reach_weighted_value, range_reach_mass),
                .counterfactual_value = counterfactual_value
            });
        }

        return result_store{
            std::move(nodes),
            std::move(strategy_surfaces),
            std::move(strategy_entries),
            std::move(combo_reaches),
            std::move(combo_values),
            std::move(action_values),
            std::move(seat_values),
            std::move(equities),
            std::move(store_combo_indices),
            std::move(categories)
        };
    }

}
