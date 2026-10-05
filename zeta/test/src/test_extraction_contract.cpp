#include <boost/test/unit_test.hpp>

#include "cfr/extraction/ev_surface.h"
#include "cfr/extraction/contract.h"
#include "cfr/graph/builder.h"
#include "cfr/tables/strategy_table.h"
#include "terminal/terminal.h"

#include <array>
#include <algorithm>
#include <bit>
#include <chrono>
#include <cmath>
#include <functional>
#include <limits>
#include <sstream>
#include <vector>

using namespace zeta::holdem::cfr::extraction;

namespace {
    constexpr zeta::card_mask test_card(const int suit, const int rank)
    {
        return zeta::card_mask{1} << (suit * 13 + rank);
    }

    zeta::holdem::board extraction_test_river_board()
    {
        return zeta::holdem::board{
            test_card(0, 12) | test_card(1, 11) | test_card(2, 10) | test_card(3, 9) | test_card(0, 0)
        };
    }

    zeta::holdem::cfr::game_graph require_extraction_graph(
        std::expected<zeta::holdem::cfr::game_graph, zeta::holdem::cfr::graph_build_error> result)
    {
        BOOST_REQUIRE(result.has_value());
        return std::move(*result);
    }

    std::pair<zeta::holdem::combination_index, zeta::holdem::combination_index> first_extraction_compatible_live_combos(
        const zeta::holdem::river_terminal_cache& cache)
    {
        for (std::size_t lhs_order = 0; lhs_order < cache.rank_order_count; ++lhs_order) {
            const auto lhs = cache.rank_order[lhs_order];
            for (std::size_t rhs_order = lhs_order + 1; rhs_order < cache.rank_order_count; ++rhs_order) {
                const auto rhs = cache.rank_order[rhs_order];
                if ((cache.masks[lhs] & cache.masks[rhs]) == 0) {
                    return {lhs, rhs};
                }
            }
        }

        BOOST_FAIL("compatible live combos not found");
        return {0, 0};
    }

    [[nodiscard]] std::vector<float> oracle_normalized_strategy(
        const zeta::holdem::cfr::strategy_sum_table& strategy_sums,
        const uint32_t infoset_id)
    {
        const auto sums = strategy_sums.infoset_sums(infoset_id);
        std::vector<float> normalized(sums.size(), 0.0f);
        if (sums.empty()) {
            return normalized;
        }

        double total = 0.0;
        for (const float sum : sums) {
            total += static_cast<double>(std::max(sum, 0.0f));
        }

        if (total <= 0.0) {
            const auto uniform = 1.0f / static_cast<float>(sums.size());
            std::fill(normalized.begin(), normalized.end(), uniform);
            return normalized;
        }

        for (std::size_t action = 0; action < sums.size(); ++action) {
            normalized[action] = static_cast<float>(static_cast<double>(std::max(sums[action], 0.0f)) / total);
        }
        return normalized;
    }

    struct oracle_reach_state {
        float oop = 0.0f;
        float ip = 0.0f;
        float chance = 0.0f;
    };

    [[nodiscard]] std::vector<oracle_reach_state> oracle_reach_by_node(
        const zeta::holdem::cfr::game_graph& graph,
        const zeta::holdem::cfr::solver::solver_graph_annotations& annotations,
        const zeta::holdem::cfr::strategy_sum_table& strategy_sums)
    {
        std::vector<oracle_reach_state> reaches(graph.node_count);
        reaches[graph.root_node] = oracle_reach_state{1.0f, 1.0f, 1.0f};

        for (uint32_t node_id = graph.root_node + 1u; node_id-- > 0u;) {
            const auto parent = reaches[node_id];
            if (parent.chance == 0.0f) {
                continue;
            }
            const auto edges = graph.out_edges(node_id);
            if (edges.empty()) {
                continue;
            }

            if (graph.is_player_node(node_id)) {
                const auto strategy = oracle_normalized_strategy(strategy_sums, graph.infoset_id[node_id]);
                const auto actor = annotations.actor_by_node[node_id];
                for (const auto edge : edges) {
                    auto child = parent;
                    if (actor == 0u) {
                        child.oop *= strategy[edge.action_index];
                    } else {
                        child.ip *= strategy[edge.action_index];
                    }
                    reaches[edge.child_node] = child;
                }
            } else if (graph.is_chance_node(node_id)) {
                const auto chance_probability = 1.0f / static_cast<float>(edges.size());
                for (const auto edge : edges) {
                    auto child = parent;
                    child.chance *= chance_probability;
                    reaches[edge.child_node] = child;
                }
            }
        }

        return reaches;
    }

    [[nodiscard]] std::vector<zeta::holdem::combination_index> oracle_combo_domain(
        const zeta::holdem::river_terminal_cache& cache)
    {
        std::vector<zeta::holdem::combination_index> combos;
        combos.reserve(cache.rank_order_count);
        for (std::size_t i = 0; i < cache.rank_order_count; ++i) {
            combos.push_back(cache.rank_order[i]);
        }
        return combos;
    }

    [[nodiscard]] std::vector<std::array<std::vector<double>, 2>> oracle_value_cache(
        const zeta::holdem::cfr::game_graph& graph,
        const zeta::holdem::cfr::solver::solver_graph_annotations& annotations,
        const zeta::holdem::cfr::strategy_sum_table& strategy_sums,
        const zeta::holdem::river_terminal_cache& cache,
        const std::span<const zeta::holdem::cfr::traversal::river_terminal_leaf> terminal_leaves,
        const std::span<const zeta::holdem::terminal_state<2>> terminal_states,
        const std::array<zeta::holdem::reach_vector, 2>& ranges)
    {
        const auto combo_indices = oracle_combo_domain(cache);
        const auto combo_count = static_cast<uint32_t>(combo_indices.size());
        zeta::holdem::terminal_engine<2> engine{};
        const std::array<zeta::holdem::river_reach_index, 2> reach_indices{
            zeta::holdem::make_river_reach_index(cache, ranges[0]),
            zeta::holdem::make_river_reach_index(cache, ranges[1])
        };

        std::vector<zeta::holdem::terminal_values<2>> terminal_value_cache(terminal_states.size());
        for (std::size_t terminal_id = 0; terminal_id < terminal_states.size(); ++terminal_id) {
            terminal_value_cache[terminal_id] = engine.evaluate_terminal_values(cache, reach_indices, terminal_states[terminal_id]);
        }

        constexpr uint8_t unvisited = 0;
        constexpr uint8_t visiting = 1;
        constexpr uint8_t visited = 2;
        std::vector<std::array<std::vector<double>, 2>> cache_by_node(graph.node_count);
        std::vector<std::array<uint8_t, 2>> state(graph.node_count, {unvisited, unvisited});

        std::function<const std::vector<double>&(uint32_t, zeta::holdem::heads_up_player)> evaluate =
            [&](const uint32_t node_id, const zeta::holdem::heads_up_player perspective) -> const std::vector<double>& {
            const auto player_slot = zeta::holdem::player_index(perspective);
            BOOST_REQUIRE(state[node_id][player_slot] != visiting);
            if (state[node_id][player_slot] == visited) {
                return cache_by_node[node_id][player_slot];
            }

            state[node_id][player_slot] = visiting;
            auto& values = cache_by_node[node_id][player_slot];
            values.assign(combo_count, 0.0);

            if (graph.is_terminal(node_id)) {
                const auto terminal_state_id = terminal_leaves[node_id].terminal_state_id;
                const auto& terminal_values = terminal_value_cache[terminal_state_id][perspective];
                for (uint32_t combo = 0; combo < combo_count; ++combo) {
                    values[combo] = terminal_values[combo_indices[combo]];
                }
            } else if (graph.is_player_node(node_id)) {
                const auto strategy = oracle_normalized_strategy(strategy_sums, graph.infoset_id[node_id]);
                for (const auto edge : graph.out_edges(node_id)) {
                    const auto& child = evaluate(edge.child_node, perspective);
                    for (uint32_t combo = 0; combo < combo_count; ++combo) {
                        values[combo] += static_cast<double>(strategy[edge.action_index]) * child[combo];
                    }
                }
            } else if (graph.is_chance_node(node_id)) {
                const auto chance_probability = 1.0 / static_cast<double>(graph.action_count(node_id));
                for (const auto edge : graph.out_edges(node_id)) {
                    const auto& child = evaluate(edge.child_node, perspective);
                    for (uint32_t combo = 0; combo < combo_count; ++combo) {
                        values[combo] += chance_probability * child[combo];
                    }
                }
            }

            state[node_id][player_slot] = visited;
            return values;
        };

        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            if (!graph.is_player_node(node_id)) {
                continue;
            }
            const auto actor = annotations.actor_by_node[node_id] == 0u
                ? zeta::holdem::heads_up_player::oop
                : zeta::holdem::heads_up_player::ip;
            (void)evaluate(node_id, actor);
            for (const auto edge : graph.out_edges(node_id)) {
                (void)evaluate(edge.child_node, actor);
            }
        }

        return cache_by_node;
    }

    [[nodiscard]] double oracle_showdown_equity(
        const zeta::holdem::river_terminal_cache& cache,
        const zeta::holdem::reach_vector& opponent_range,
        const float opponent_reach_probability,
        const zeta::holdem::combination_index hero_combo)
    {
        if (!zeta::holdem::is_live_combo(hero_combo, cache.river_board.mask)) {
            return 0.0;
        }

        const auto hero_mask = zeta::holdem::combination_mask(hero_combo);
        const auto hero_rank = cache.rank_keys[hero_combo];
        double compatible_mass = 0.0;
        double pot_share_mass = 0.0;

        for (std::size_t i = 0; i < cache.rank_order_count; ++i) {
            const auto opponent_combo = cache.rank_order[i];
            const auto opponent_weight = static_cast<double>(opponent_range[opponent_combo])
                * static_cast<double>(opponent_reach_probability);
            if (opponent_weight <= 0.0) {
                continue;
            }
            if ((hero_mask & zeta::holdem::combination_mask(opponent_combo)) != 0) {
                continue;
            }

            compatible_mass += opponent_weight;
            const auto opponent_rank = cache.rank_keys[opponent_combo];
            if (hero_rank > opponent_rank) {
                pot_share_mass += opponent_weight;
            } else if (hero_rank == opponent_rank) {
                pot_share_mass += 0.5 * opponent_weight;
            }
        }

        return compatible_mass > 0.0 ? pot_share_mass / compatible_mass : 0.0;
    }

    [[nodiscard]] zeta::holdem::combination_index combo_from_exact_cards(
        const zeta::card_mask first,
        const zeta::card_mask second)
    {
        const auto target = first | second;
        for (zeta::holdem::combination_index combo = 0; combo < zeta::holdem::combination_count; ++combo) {
            if (zeta::holdem::combination_mask(combo) == target) {
                return combo;
            }
        }
        BOOST_FAIL("exact two-card combo not found");
        return 0;
    }

    [[nodiscard]] double manual_turn_showdown_pot_share_equity(
        const zeta::holdem::board turn_board,
        const zeta::holdem::reach_vector& opponent_range,
        const float opponent_reach_probability,
        const zeta::holdem::combination_index hero_combo,
        uint32_t& runout_count_out)
    {
        const auto hero_mask = zeta::holdem::combination_mask(hero_combo);
        if ((hero_mask & turn_board.mask) != 0u) {
            runout_count_out = 0u;
            return 0.0;
        }

        double runout_equity_sum = 0.0;
        uint32_t runout_count = 0u;
        for (uint8_t river = 0; river < zeta::num_cards<zeta::default_deck>; ++river) {
            const auto river_bit = zeta::card_mask{1} << river;
            if ((turn_board.mask & river_bit) != 0u || (hero_mask & river_bit) != 0u) {
                continue;
            }
            const auto board_mask = turn_board.mask | river_bit;
            const auto hero_rank = zeta::holdem::evaluate(hero_mask | board_mask);
            double compatible_mass = 0.0;
            double pot_share_mass = 0.0;
            for (zeta::holdem::combination_index opponent_combo = 0; opponent_combo < zeta::holdem::combination_count; ++opponent_combo) {
                const auto opponent_weight = static_cast<double>(opponent_range[opponent_combo])
                    * static_cast<double>(opponent_reach_probability);
                if (opponent_weight <= 0.0) {
                    continue;
                }
                const auto opponent_mask = zeta::holdem::combination_mask(opponent_combo);
                if ((opponent_mask & board_mask) != 0u || (opponent_mask & hero_mask) != 0u) {
                    continue;
                }
                compatible_mass += opponent_weight;
                const auto opponent_rank = zeta::holdem::evaluate(opponent_mask | board_mask);
                if (hero_rank > opponent_rank) {
                    pot_share_mass += opponent_weight;
                } else if (hero_rank == opponent_rank) {
                    pot_share_mass += 0.5 * opponent_weight;
                }
            }
            runout_equity_sum += compatible_mass > 0.0 ? pot_share_mass / compatible_mass : 0.0;
            ++runout_count;
        }
        runout_count_out = runout_count;
        return runout_count > 0u ? runout_equity_sum / static_cast<double>(runout_count) : 0.0;
    }

    [[nodiscard]] double manual_flop_showdown_pot_share_equity(
        const zeta::holdem::board flop_board,
        const zeta::holdem::reach_vector& opponent_range,
        const float opponent_reach_probability,
        const zeta::holdem::combination_index hero_combo,
        uint32_t& runout_count_out)
    {
        const auto hero_mask = zeta::holdem::combination_mask(hero_combo);
        if ((hero_mask & flop_board.mask) != 0u) {
            runout_count_out = 0u;
            return 0.0;
        }

        double runout_equity_sum = 0.0;
        uint32_t runout_count = 0u;
        for (uint8_t turn = 0; turn < zeta::num_cards<zeta::default_deck>; ++turn) {
            const auto turn_bit = zeta::card_mask{1} << turn;
            if ((flop_board.mask & turn_bit) != 0u || (hero_mask & turn_bit) != 0u) {
                continue;
            }
            for (uint8_t river = static_cast<uint8_t>(turn + 1u); river < zeta::num_cards<zeta::default_deck>; ++river) {
                const auto river_bit = zeta::card_mask{1} << river;
                if ((flop_board.mask & river_bit) != 0u || (hero_mask & river_bit) != 0u) {
                    continue;
                }
                const auto board_mask = flop_board.mask | turn_bit | river_bit;
                const auto hero_rank = zeta::holdem::evaluate(hero_mask | board_mask);
                double compatible_mass = 0.0;
                double pot_share_mass = 0.0;
                for (zeta::holdem::combination_index opponent_combo = 0; opponent_combo < zeta::holdem::combination_count; ++opponent_combo) {
                    const auto opponent_weight = static_cast<double>(opponent_range[opponent_combo])
                        * static_cast<double>(opponent_reach_probability);
                    if (opponent_weight <= 0.0) {
                        continue;
                    }
                    const auto opponent_mask = zeta::holdem::combination_mask(opponent_combo);
                    if ((opponent_mask & board_mask) != 0u || (opponent_mask & hero_mask) != 0u) {
                        continue;
                    }
                    compatible_mass += opponent_weight;
                    const auto opponent_rank = zeta::holdem::evaluate(opponent_mask | board_mask);
                    if (hero_rank > opponent_rank) {
                        pot_share_mass += opponent_weight;
                    } else if (hero_rank == opponent_rank) {
                        pot_share_mass += 0.5 * opponent_weight;
                    }
                }
                runout_equity_sum += compatible_mass > 0.0 ? pot_share_mass / compatible_mass : 0.0;
                ++runout_count;
            }
        }
        runout_count_out = runout_count;
        return runout_count > 0u ? runout_equity_sum / static_cast<double>(runout_count) : 0.0;
    }

    [[nodiscard]] uint32_t float_bits(const float value)
    {
        return std::bit_cast<uint32_t>(value);
    }

    [[nodiscard]] uint64_t double_bits(const double value)
    {
        return std::bit_cast<uint64_t>(value);
    }
}

BOOST_AUTO_TEST_SUITE(extraction_contract_suite)

BOOST_AUTO_TEST_CASE(test_quality_and_solve_enums)
{
    // solve_mode
    BOOST_CHECK_EQUAL(to_string(solve_mode::normal), "normal");
    BOOST_CHECK_EQUAL(to_string(solve_mode::preview), "preview");
    BOOST_CHECK(parse_solve_mode("normal") == solve_mode::normal);
    BOOST_CHECK(parse_solve_mode("preview") == solve_mode::preview);
    BOOST_CHECK(!parse_solve_mode("invalid").has_value());

    std::ostringstream ss_mode;
    ss_mode << solve_mode::normal << " " << solve_mode::preview;
    BOOST_CHECK_EQUAL(ss_mode.str(), "normal preview");

    // termination_reason
    BOOST_CHECK_EQUAL(to_string(termination_reason::iteration_limit), "iteration_limit");
    BOOST_CHECK_EQUAL(to_string(termination_reason::exploitability_target), "exploitability_target");
    BOOST_CHECK_EQUAL(to_string(termination_reason::user_interrupted), "user_interrupted");
    BOOST_CHECK(parse_termination_reason("iteration_limit") == termination_reason::iteration_limit);
    BOOST_CHECK(parse_termination_reason("exploitability_target") == termination_reason::exploitability_target);
    BOOST_CHECK(parse_termination_reason("user_interrupted") == termination_reason::user_interrupted);
    BOOST_CHECK(!parse_termination_reason("other").has_value());

    // convergence_status
    BOOST_CHECK_EQUAL(to_string(convergence_status::target_met), "target_met");
    BOOST_CHECK_EQUAL(to_string(convergence_status::iteration_limited), "iteration_limited");
    BOOST_CHECK_EQUAL(to_string(convergence_status::not_evaluated), "not_evaluated");
    BOOST_CHECK(parse_convergence_status("target_met") == convergence_status::target_met);
    BOOST_CHECK(parse_convergence_status("iteration_limited") == convergence_status::iteration_limited);
    BOOST_CHECK(parse_convergence_status("not_evaluated") == convergence_status::not_evaluated);
    BOOST_CHECK(!parse_convergence_status("unknown").has_value());

    // evaluation_method
    BOOST_CHECK_EQUAL(to_string(evaluation_method::exact), "exact");
    BOOST_CHECK_EQUAL(to_string(evaluation_method::sampled), "sampled");
    BOOST_CHECK(parse_evaluation_method("exact") == evaluation_method::exact);
    BOOST_CHECK(parse_evaluation_method("sampled") == evaluation_method::sampled);
    BOOST_CHECK(!parse_evaluation_method("approx").has_value());

    // abstraction_mode
    BOOST_CHECK_EQUAL(to_string(abstraction_mode::exact), "exact");
    BOOST_CHECK_EQUAL(to_string(abstraction_mode::suit_isomorphic), "suit_isomorphic");
    BOOST_CHECK_EQUAL(to_string(abstraction_mode::range_abstracted), "range_abstracted");
    BOOST_CHECK_EQUAL(to_string(abstraction_mode::mixed), "mixed");
    BOOST_CHECK(parse_abstraction_mode("exact") == abstraction_mode::exact);
    BOOST_CHECK(parse_abstraction_mode("suit_isomorphic") == abstraction_mode::suit_isomorphic);
    BOOST_CHECK(parse_abstraction_mode("range_abstracted") == abstraction_mode::range_abstracted);
    BOOST_CHECK(parse_abstraction_mode("mixed") == abstraction_mode::mixed);
    BOOST_CHECK(!parse_abstraction_mode("lossy").has_value());

    // artifact_export_mode
    BOOST_CHECK_EQUAL(to_string(artifact_export_mode::summary), "summary");
    BOOST_CHECK_EQUAL(to_string(artifact_export_mode::standard), "standard");
    BOOST_CHECK_EQUAL(to_string(artifact_export_mode::full), "full");
    BOOST_CHECK(parse_artifact_export_mode("summary") == artifact_export_mode::summary);
    BOOST_CHECK(parse_artifact_export_mode("standard") == artifact_export_mode::standard);
    BOOST_CHECK(parse_artifact_export_mode("full") == artifact_export_mode::full);
    BOOST_CHECK(!parse_artifact_export_mode("compact").has_value());

    // solve_status
    BOOST_CHECK_EQUAL(to_string(solve_status::iteration_limited), "iteration_limited");
    BOOST_CHECK_EQUAL(to_string(solve_status::converged), "converged");
    BOOST_CHECK_EQUAL(to_string(solve_status::not_evaluated), "not_evaluated");
    BOOST_CHECK(parse_solve_status("iteration_limited") == solve_status::iteration_limited);
    BOOST_CHECK(parse_solve_status("converged") == solve_status::converged);
    BOOST_CHECK(parse_solve_status("not_evaluated") == solve_status::not_evaluated);
    BOOST_CHECK(!parse_solve_status("failed").has_value());
}

BOOST_AUTO_TEST_CASE(test_version_and_sentinel_constants)
{
    BOOST_CHECK_EQUAL(CURRENT_SCHEMA_VERSION, 4u);
    BOOST_CHECK_EQUAL(CURRENT_EXTRACTION_VERSION, 1u);
    BOOST_CHECK_EQUAL(INVALID_STRATEGY_CONTEXT_ID, std::numeric_limits<strategy_context_id>::max());
    BOOST_CHECK_EQUAL(INVALID_COMBO_LOCAL_INDEX, std::numeric_limits<combo_local_index>::max());
    BOOST_CHECK_EQUAL(INVALID_ACTION_INDEX, std::numeric_limits<action_index>::max());
}

BOOST_AUTO_TEST_CASE(test_reach_conditioning_and_precision)
{
    const float w0 = 0.33333334f;
    const float reach_prob = 0.5f;

    const combo_reach reach{.range_weight = w0, .reach_probability = reach_prob};
    const double derived_weight = reach.range_reach_weight();
    const double expected_weight = static_cast<double>(w0) * static_cast<double>(reach_prob);

    BOOST_CHECK_CLOSE(derived_weight, expected_weight, 1e-6);
    BOOST_CHECK_CLOSE(compute_range_reach_weight(w0, reach_prob), expected_weight, 1e-6);

    // Joint reach mass
    const float opp_reach = 0.75f;
    const double chance_reach = 0.25;
    const double joint_mass = compute_joint_reach_mass(w0, reach_prob, opp_reach, chance_reach);
    const double expected_joint = static_cast<double>(w0) *
                                 static_cast<double>(reach_prob) *
                                 static_cast<double>(opp_reach) *
                                 chance_reach;
    BOOST_CHECK_CLOSE(joint_mass, expected_joint, 1e-6);

    // Range reach mass across multiple combos
    const std::vector<float> range_weights = {0.5f, 1.0f, 0.25f};
    const std::vector<float> reach_probs = {0.8f, 0.4f, 0.0f}; // includes a zero-reach combo
    const double total_range_reach = compute_range_reach_mass(range_weights, reach_probs);
    // 0.5*0.8 + 1.0*0.4 + 0.25*0.0 = 0.4 + 0.4 + 0.0 = 0.8
    BOOST_CHECK_CLOSE(total_range_reach, 0.8, 1e-4);
}

BOOST_AUTO_TEST_CASE(test_canonical_q_v_a_chain_and_advantage_identity)
{
    // 2-action mixed strategy: check (40%), bet (60%)
    const std::array<float, 2> strategy = {0.4f, 0.6f};
    const std::array<double, 2> q_profile = {50.0, 70.0};

    BOOST_CHECK(verify_strategy_normalized(strategy));

    // V = 0.4 * 50.0 + 0.6 * 70.0 = 20.0 + 42.0 = 62.0
    const double v_profile = compute_combo_profile_value(strategy, q_profile);
    BOOST_CHECK_CLOSE(v_profile, 62.0, 1e-4);

    // Profile advantages: A = Q - V
    const std::array<double, 2> advantages = {
        compute_profile_advantage(q_profile[0], v_profile), // 50 - 62 = -12
        compute_profile_advantage(q_profile[1], v_profile)  // 70 - 62 = +8
    };

    BOOST_CHECK_CLOSE(advantages[0], -12.0, 1e-4);
    BOOST_CHECK_CLOSE(advantages[1], 8.0, 1e-4);

    // Strategy-Weighted Advantage Identity: 0.4 * (-12) + 0.6 * (8) = -4.8 + 4.8 = 0.0
    BOOST_CHECK(verify_strategy_weighted_advantage_identity(strategy, advantages));

    // Multi-action scenario (3 actions: fold, call, raise)
    const std::array<float, 3> strategy3 = {0.1f, 0.3f, 0.6f};
    const std::array<double, 3> q3 = {-10.0, 20.0, 45.0};
    BOOST_CHECK(verify_strategy_normalized(strategy3));

    // V = 0.1*(-10) + 0.3*(20) + 0.6*(45) = -1.0 + 6.0 + 27.0 = 32.0
    const double v3 = compute_combo_profile_value(strategy3, q3);
    BOOST_CHECK_CLOSE(v3, 32.0, 1e-4);

    const std::array<double, 3> adv3 = {
        compute_profile_advantage(q3[0], v3), // -10 - 32 = -42
        compute_profile_advantage(q3[1], v3), //  20 - 32 = -12
        compute_profile_advantage(q3[2], v3)  //  45 - 32 = +13
    };

    // 0.1*(-42) + 0.3*(-12) + 0.6*(13) = -4.2 - 3.6 + 7.8 = 0.0
    BOOST_CHECK(verify_strategy_weighted_advantage_identity(strategy3, adv3));
}

BOOST_AUTO_TEST_CASE(test_reach_weighted_ev_and_counterfactual_value)
{
    const std::vector<float> range_weights = {1.0f, 1.0f, 0.5f};
    const std::vector<float> hero_reach = {0.8f, 0.2f, 0.0f}; // Combo 2 has zero reach
    const std::vector<float> opp_reach = {0.5f, 0.6f, 0.4f};
    const std::vector<double> combo_evs = {100.0, -50.0, 200.0};
    const double chance_reach = 1.0;

    // Range reach mass: 1.0*0.8 + 1.0*0.2 + 0.5*0.0 = 0.8 + 0.2 + 0.0 = 1.0
    const double range_mass = compute_range_reach_mass(range_weights, hero_reach);
    BOOST_CHECK_CLOSE(range_mass, 1.0, 1e-4);

    // Reach weighted EV: 0.8*100.0 + 0.2*(-50.0) + 0.0*200.0 = 80 - 10 = 70.0
    const double reach_weighted_ev = compute_reach_weighted_ev(range_weights, hero_reach, combo_evs);
    BOOST_CHECK_CLOSE(reach_weighted_ev, 70.0, 1e-4);

    // Conditional range EV: 70.0 / 1.0 = 70.0
    const double cond_ev = compute_conditional_range_ev(reach_weighted_ev, range_mass);
    BOOST_CHECK_CLOSE(cond_ev, 70.0, 1e-4);

    // Counterfactual value (uses opponent reach, excludes hero reach):
    // 1.0*0.5*1.0*100.0 + 1.0*0.6*1.0*(-50.0) + 0.5*0.4*1.0*200.0
    // = 50.0 - 30.0 + 40.0 = 60.0
    const double cfv = compute_counterfactual_value(range_weights, opp_reach, chance_reach, combo_evs);
    BOOST_CHECK_CLOSE(cfv, 60.0, 1e-4);
}

BOOST_AUTO_TEST_CASE(test_invariants_and_guardrails)
{
    // Heads-up zero-sum payoff check
    BOOST_CHECK(verify_heads_up_zero_sum(50.0, -50.0));
    BOOST_CHECK(verify_heads_up_zero_sum(0.0, 0.0));
    BOOST_CHECK(!verify_heads_up_zero_sum(50.0, -49.0));

    // Showdown equity bounds
    BOOST_CHECK(verify_showdown_equity_bounds(0.0));
    BOOST_CHECK(verify_showdown_equity_bounds(0.5));
    BOOST_CHECK(verify_showdown_equity_bounds(1.0));
    BOOST_CHECK(!verify_showdown_equity_bounds(-0.01));
    BOOST_CHECK(!verify_showdown_equity_bounds(1.01));

    // Heads-up equity conservation
    BOOST_CHECK(verify_heads_up_equity_conservation(0.65, 0.35));
    BOOST_CHECK(verify_heads_up_equity_conservation(0.5, 0.5));
    BOOST_CHECK(!verify_heads_up_equity_conservation(0.6, 0.3));

    // Multiway equity conservation
    const std::array<double, 3> multi_eq = {0.5, 0.3, 0.2};
    BOOST_CHECK(verify_multiway_equity_conservation(multi_eq));

    const std::array<double, 3> invalid_multi_eq = {0.5, 0.3, 0.3};
    BOOST_CHECK(!verify_multiway_equity_conservation(invalid_multi_eq));
}

BOOST_AUTO_TEST_CASE(test_derived_category_reductions)
{
    const double range_mass = 1.0;
    const double cat_weight = 0.25;
    const double cat_action_weight = 0.20; // 80% frequency on this action within category
    const double cat_reach_weighted_ev = 15.0;
    const double cat_reach_weighted_equity = 0.175;

    // Category reach frequency: 0.25 / 1.0 = 0.25 (25%)
    const double f_c = compute_category_reach_frequency(cat_weight, range_mass);
    BOOST_CHECK_CLOSE(f_c, 0.25, 1e-9);

    // Conditional action frequency: 0.20 / 0.25 = 0.80 (80%)
    const double f_a_c = compute_category_conditional_action_frequency(cat_action_weight, cat_weight);
    BOOST_CHECK_CLOSE(f_a_c, 0.80, 1e-9);

    // Average EV: 15.0 / 0.25 = 60.0
    const double avg_ev = compute_category_average_ev(cat_reach_weighted_ev, cat_weight);
    BOOST_CHECK_CLOSE(avg_ev, 60.0, 1e-9);

    // Average Showdown Equity: 0.175 / 0.25 = 0.70 (70%)
    const double avg_eq = compute_category_average_equity(cat_reach_weighted_equity, cat_weight);
    BOOST_CHECK_CLOSE(avg_eq, 0.70, 1e-9);
}

BOOST_AUTO_TEST_CASE(postflop_exact_equity_matches_manual_turn_and_flop_oracles)
{
    zeta::holdem::reach_vector opponent_range{};
    const auto opponent_a = combo_from_exact_cards(test_card(3, 0), test_card(2, 1)); // 2c3d
    const auto opponent_b = combo_from_exact_cards(test_card(3, 4), test_card(2, 5)); // 6c7d
    opponent_range[opponent_a] = 0.65f;
    opponent_range[opponent_b] = 0.35f;

    const auto hero_combo = combo_from_exact_cards(test_card(1, 8), test_card(0, 7)); // Th9s
    const zeta::holdem::board turn_board{
        test_card(1, 12) | test_card(1, 11) | test_card(1, 10) | test_card(1, 9) // AhKhQhJh
    };
    const zeta::holdem::board flop_board{
        test_card(0, 12) | test_card(2, 11) | test_card(3, 6) // AsKd8c
    };

    uint32_t turn_runouts = 0u;
    const auto turn_manual = manual_turn_showdown_pot_share_equity(
        turn_board,
        opponent_range,
        1.0f,
        hero_combo,
        turn_runouts);
    const auto turn_surface = turn_showdown_pot_share_equity(
        turn_board,
        opponent_range,
        1.0f,
        hero_combo);

    uint32_t flop_runouts = 0u;
    const auto flop_manual = manual_flop_showdown_pot_share_equity(
        flop_board,
        opponent_range,
        1.0f,
        hero_combo,
        flop_runouts);
    const auto flop_surface = flop_showdown_pot_share_equity(
        flop_board,
        opponent_range,
        1.0f,
        hero_combo);

    BOOST_CHECK_EQUAL(turn_runouts, 46u);
    BOOST_CHECK_EQUAL(flop_runouts, 1081u);
    BOOST_CHECK_CLOSE(turn_surface, turn_manual, 1e-6);
    BOOST_CHECK_CLOSE(flop_surface, flop_manual, 1e-6);
    BOOST_CHECK_CLOSE(turn_surface, 1.0, 1e-9);
    BOOST_CHECK(turn_surface >= 0.0 && turn_surface <= 1.0);
    BOOST_CHECK(flop_surface >= 0.0 && flop_surface <= 1.0);
}

BOOST_AUTO_TEST_CASE(river_hu_extraction_populates_q_v_a_and_equity_surfaces)
{
    namespace cfr = zeta::holdem::cfr;

    cfr::graph_builder builder;
    const auto root = builder.add_node(cfr::node_kind::player);
    const auto showdown_terminal = builder.add_node(cfr::node_kind::terminal);
    const auto fold_terminal = builder.add_node(cfr::node_kind::terminal);
    builder.add_edge(root, showdown_terminal, 0);
    builder.add_edge(root, fold_terminal, 1);
    builder.set_infoset_id(root, 0);
    std::vector<uint32_t> remap;
    auto graph_result = builder.build(remap);
    BOOST_REQUIRE_MESSAGE(graph_result.has_value(), zeta::holdem::cfr::to_string(graph_result.error().kind));
    auto graph = std::move(*graph_result);

    cfr::action_table_layout layout;
    layout.action_offsets = {0, 2};
    cfr::strategy_sum_table strategy_sums(layout);
    strategy_sums.value(0, 0) = 4.0f;
    strategy_sums.value(0, 1) = 6.0f;

    const auto board = extraction_test_river_board();
    const auto cache = zeta::holdem::make_river_terminal_cache(board);
    const auto [oop_combo, ip_combo] = first_extraction_compatible_live_combos(cache);

    zeta::holdem::reach_vector oop_range{};
    zeta::holdem::reach_vector ip_range{};
    oop_range[oop_combo] = 1.0f;
    ip_range[ip_combo] = 1.0f;

    const auto terminal_context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);
    zeta::holdem::terminal_state_table<2> terminal_states;
    terminal_states.states.push_back(zeta::holdem::make_showdown_terminal_state(terminal_context));
    terminal_states.states.push_back(zeta::holdem::make_fold_terminal_state(
        terminal_context,
        zeta::holdem::heads_up_player::ip));

    std::vector<cfr::traversal::river_terminal_leaf> terminal_leaves(graph.node_count);
    for (const auto edge : graph.out_edges(graph.root_node)) {
        terminal_leaves[edge.child_node] = cfr::traversal::river_terminal_leaf{edge.action_index};
    }

    cfr::solver::solver_graph_annotations annotations;
    annotations.actor_by_node.assign(graph.node_count, 0u);
    annotations.state_by_node.assign(graph.node_count, cfr::solver::solver_node_state_metadata{
        .street = cfr::solver::holdem_street::river,
        .public_state_id = 7u,
        .betting_state_id = 0u
    });

    auto extracted = extract_river_heads_up_result_store(river_heads_up_extraction_input{
        .graph = &graph,
        .annotations = &annotations,
        .strategy_sums = &strategy_sums,
        .river_cache = &cache,
        .terminal_leaves = terminal_leaves,
        .terminal_states = terminal_states.view(),
        .ranges = {oop_range, ip_range}
    });
    BOOST_REQUIRE(extracted.has_value());
    BOOST_REQUIRE_EQUAL(extracted->node_count(), 1u);

    const auto node = extracted->node(0);
    uint32_t oop_local = INVALID_COMBO_LOCAL_INDEX;
    for (uint32_t local = 0; local < node.combo_count(); ++local) {
        if (node.combo_index(local) == oop_combo) {
            oop_local = local;
            break;
        }
    }
    BOOST_REQUIRE_NE(oop_local, INVALID_COMBO_LOCAL_INDEX);

    const std::array<zeta::holdem::river_reach_index, 2> reach_indices{
        zeta::holdem::make_river_reach_index(cache, oop_range),
        zeta::holdem::make_river_reach_index(cache, ip_range)
    };
    const zeta::holdem::terminal_engine<2> engine{};
    const auto showdown_values = engine.evaluate_terminal_values(cache, reach_indices, terminal_states[0]);
    const auto fold_values = engine.evaluate_terminal_values(cache, reach_indices, terminal_states[1]);
    const auto q_showdown = static_cast<double>(showdown_values[zeta::holdem::heads_up_player::oop][oop_combo]);
    const auto q_fold = static_cast<double>(fold_values[zeta::holdem::heads_up_player::oop][oop_combo]);
    const auto expected_value = 0.4 * q_showdown + 0.6 * q_fold;

    BOOST_CHECK_CLOSE(node.strategy().frequency(oop_local, 0), 0.4f, 0.001f);
    BOOST_CHECK_CLOSE(node.strategy().frequency(oop_local, 1), 0.6f, 0.001f);
    BOOST_CHECK_CLOSE(node.values().q_value(oop_local, 0), q_showdown, 0.001);
    BOOST_CHECK_CLOSE(node.values().q_value(oop_local, 1), q_fold, 0.001);
    BOOST_CHECK_CLOSE(node.values().combo_ev(oop_local), expected_value, 0.001);
    BOOST_CHECK_CLOSE(node.values().profile_advantage(oop_local, 0), q_showdown - expected_value, 0.001);
    BOOST_CHECK_CLOSE(node.values().profile_advantage(oop_local, 1), q_fold - expected_value, 0.001);
    const std::array<float, 2> expected_strategy{0.4f, 0.6f};
    const std::array<double, 2> expected_advantages{q_showdown - expected_value, q_fold - expected_value};
    BOOST_CHECK(verify_strategy_weighted_advantage_identity(expected_strategy, expected_advantages));
    BOOST_CHECK(verify_showdown_equity_bounds(node.equity().showdown_equity(oop_local)));
    BOOST_CHECK_CLOSE(node.values().range_reach_mass(), 1.0, 0.001);
    BOOST_CHECK_CLOSE(node.values().conditional_range_ev(), expected_value, 0.001);
}

BOOST_AUTO_TEST_CASE(river_hu_extraction_uses_chance_event_probabilities)
{
    namespace cfr = zeta::holdem::cfr;

    cfr::graph_builder builder;
    const auto root = builder.add_node(cfr::node_kind::player);
    const auto chance_node = builder.add_node(cfr::node_kind::chance);
    const auto fold_win_terminal = builder.add_node(cfr::node_kind::terminal);
    const auto showdown_terminal = builder.add_node(cfr::node_kind::terminal);
    const auto fold_lose_terminal = builder.add_node(cfr::node_kind::terminal);
    builder.add_edge(root, chance_node, 0);
    builder.add_edge(root, fold_win_terminal, 1);
    builder.add_edge(chance_node, showdown_terminal, 0);
    builder.add_edge(chance_node, fold_lose_terminal, 1);
    builder.set_infoset_id(root, 0);
    std::vector<uint32_t> remap;
    auto graph_result = builder.build(remap);
    BOOST_REQUIRE_MESSAGE(graph_result.has_value(), zeta::holdem::cfr::to_string(graph_result.error().kind));
    auto graph = std::move(*graph_result);
    const auto root_id = remap[root];
    const auto chance_node_id = remap[chance_node];
    const auto fold_win_terminal_id = remap[fold_win_terminal];
    const auto showdown_terminal_id = remap[showdown_terminal];
    const auto fold_lose_terminal_id = remap[fold_lose_terminal];

    cfr::action_table_layout layout;
    layout.action_offsets = {0, 2};
    cfr::strategy_sum_table strategy_sums(layout);
    strategy_sums.value(0, 0) = 1.0f;
    strategy_sums.value(0, 1) = 0.0f;

    const auto board = extraction_test_river_board();
    const auto cache = zeta::holdem::make_river_terminal_cache(board);
    const auto [oop_combo, ip_combo] = first_extraction_compatible_live_combos(cache);
    zeta::holdem::reach_vector oop_range{};
    zeta::holdem::reach_vector ip_range{};
    oop_range[oop_combo] = 1.0f;
    ip_range[ip_combo] = 1.0f;

    const auto context = zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0);
    zeta::holdem::terminal_state_table<2> terminal_states;
    terminal_states.states.push_back(zeta::holdem::make_showdown_terminal_state(context)); // 0
    terminal_states.states.push_back(zeta::holdem::make_fold_terminal_state(context, zeta::holdem::heads_up_player::ip)); // 1
    terminal_states.states.push_back(zeta::holdem::make_fold_terminal_state(context, zeta::holdem::heads_up_player::oop)); // 2

    std::vector<cfr::traversal::river_terminal_leaf> terminal_leaves(graph.node_count);
    terminal_leaves[fold_win_terminal_id] = cfr::traversal::river_terminal_leaf{1};
    terminal_leaves[showdown_terminal_id] = cfr::traversal::river_terminal_leaf{0};
    terminal_leaves[fold_lose_terminal_id] = cfr::traversal::river_terminal_leaf{2};

    cfr::solver::solver_graph_annotations annotations;
    annotations.actor_by_node.assign(graph.node_count, cfr::solver::INVALID_PLAYER);
    annotations.actor_by_node[root_id] = 0u;
    annotations.state_by_node.assign(graph.node_count, cfr::solver::solver_node_state_metadata{
        .street = cfr::solver::holdem_street::river,
        .public_state_id = 31u,
        .betting_state_id = 0u
    });

    cfr::chance_event_table chance_events;
    chance_events.event_id_by_node.assign(graph.node_count, cfr::INVALID_CHANCE_EVENT);
    chance_events.events.push_back(cfr::chance_event{
        .node_id = chance_node_id,
        .first_outcome = 0u,
        .outcome_count = 2u
    });
    chance_events.outcomes.push_back(cfr::chance_outcome{
        .child_node = showdown_terminal_id,
        .action_index = 0u,
        .probability = 0.8f
    });
    chance_events.outcomes.push_back(cfr::chance_outcome{
        .child_node = fold_lose_terminal_id,
        .action_index = 1u,
        .probability = 0.2f
    });
    chance_events.event_id_by_node[chance_node_id] = 0u;

    auto extracted = extract_river_heads_up_result_store(river_heads_up_extraction_input{
        .graph = &graph,
        .annotations = &annotations,
        .strategy_sums = &strategy_sums,
        .chance_events = &chance_events,
        .river_cache = &cache,
        .terminal_leaves = terminal_leaves,
        .terminal_states = terminal_states.view(),
        .ranges = {oop_range, ip_range}
    });
    BOOST_REQUIRE(extracted.has_value());
    BOOST_REQUIRE_EQUAL(extracted->node_count(), 1u);

    const auto node = extracted->node(0);
    uint32_t hero_local = INVALID_COMBO_LOCAL_INDEX;
    for (uint32_t local = 0; local < node.combo_count(); ++local) {
        if (node.combo_index(local) == oop_combo) {
            hero_local = local;
            break;
        }
    }
    BOOST_REQUIRE_NE(hero_local, INVALID_COMBO_LOCAL_INDEX);

    const std::array<zeta::holdem::river_reach_index, 2> reach_indices{
        zeta::holdem::make_river_reach_index(cache, oop_range),
        zeta::holdem::make_river_reach_index(cache, ip_range)
    };
    const zeta::holdem::terminal_engine<2> engine{};
    const auto showdown_value = static_cast<double>(
        engine.evaluate_terminal_values(cache, reach_indices, terminal_states[0])[zeta::holdem::heads_up_player::oop][oop_combo]);
    const auto fold_lose_value = static_cast<double>(
        engine.evaluate_terminal_values(cache, reach_indices, terminal_states[2])[zeta::holdem::heads_up_player::oop][oop_combo]);
    const auto fold_win_value = static_cast<double>(
        engine.evaluate_terminal_values(cache, reach_indices, terminal_states[1])[zeta::holdem::heads_up_player::oop][oop_combo]);

    const auto expected_chance_q = 0.8 * showdown_value + 0.2 * fold_lose_value;
    BOOST_CHECK_CLOSE(node.values().q_value(hero_local, 0), expected_chance_q, 1e-6);
    BOOST_CHECK_CLOSE(node.values().q_value(hero_local, 1), fold_win_value, 1e-6);
    BOOST_CHECK_CLOSE(node.values().combo_ev(hero_local), expected_chance_q, 1e-6);
}

BOOST_AUTO_TEST_CASE(river_hu_extraction_aliases_shared_infoset_strategy_surfaces)
{
    namespace cfr = zeta::holdem::cfr;

    cfr::graph_builder builder;
    const auto root = builder.add_node(cfr::node_kind::chance);
    const auto lhs_player = builder.add_node(cfr::node_kind::player);
    const auto rhs_player = builder.add_node(cfr::node_kind::player);
    const auto lhs_terminal = builder.add_node(cfr::node_kind::terminal);
    const auto rhs_terminal = builder.add_node(cfr::node_kind::terminal);
    builder.add_edge(root, lhs_player, 0);
    builder.add_edge(root, rhs_player, 1);
    builder.add_edge(lhs_player, lhs_terminal, 0);
    builder.add_edge(rhs_player, rhs_terminal, 0);
    builder.set_infoset_id(lhs_player, 0);
    builder.set_infoset_id(rhs_player, 0);
    auto graph = require_extraction_graph(builder.build());

    cfr::action_table_layout layout;
    layout.action_offsets = {0, 1};
    cfr::strategy_sum_table strategy_sums(layout);
    strategy_sums.value(0, 0) = 1.0f;

    const auto board = extraction_test_river_board();
    const auto cache = zeta::holdem::make_river_terminal_cache(board);
    const auto [oop_combo, ip_combo] = first_extraction_compatible_live_combos(cache);
    zeta::holdem::reach_vector oop_range{};
    zeta::holdem::reach_vector ip_range{};
    oop_range[oop_combo] = 1.0f;
    ip_range[ip_combo] = 1.0f;

    zeta::holdem::terminal_state_table<2> terminal_states;
    terminal_states.states.push_back(zeta::holdem::make_showdown_terminal_state(
        zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0)));
    std::vector<cfr::traversal::river_terminal_leaf> terminal_leaves(graph.node_count);
    for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
        if (graph.is_terminal(node_id)) {
            terminal_leaves[node_id] = cfr::traversal::river_terminal_leaf{0};
        }
    }

    cfr::solver::solver_graph_annotations annotations;
    annotations.actor_by_node.assign(graph.node_count, 0u);
    annotations.state_by_node.assign(graph.node_count, cfr::solver::solver_node_state_metadata{
        .street = cfr::solver::holdem_street::river,
        .public_state_id = 11u,
        .betting_state_id = 0u
    });

    auto extracted = extract_river_heads_up_result_store(river_heads_up_extraction_input{
        .graph = &graph,
        .annotations = &annotations,
        .strategy_sums = &strategy_sums,
        .river_cache = &cache,
        .terminal_leaves = terminal_leaves,
        .terminal_states = terminal_states.view(),
        .ranges = {oop_range, ip_range}
    });
    BOOST_REQUIRE(extracted.has_value());
    BOOST_REQUIRE_EQUAL(extracted->node_count(), 2u);
    BOOST_REQUIRE_EQUAL(extracted->strategy_context_count(), 1u);
    BOOST_CHECK_EQUAL(extracted->node_strategy_context_id(0), extracted->node_strategy_context_id(1));
    BOOST_CHECK_EQUAL(extracted->node(0).strategy().entries().data(), extracted->node(1).strategy().entries().data());
    BOOST_CHECK_NE(extracted->node(0).values().action_values().data(), extracted->node(1).values().action_values().data());
    BOOST_CHECK_CLOSE(extracted->node(0).values().range_reach_mass(), 1.0, 0.001);
    BOOST_CHECK_CLOSE(extracted->node(1).values().range_reach_mass(), 1.0, 0.001);
}

BOOST_AUTO_TEST_CASE(river_hu_extraction_preserves_zero_reach_combos)
{
    namespace cfr = zeta::holdem::cfr;

    cfr::graph_builder builder;
    const auto root = builder.add_node(cfr::node_kind::player);
    const auto reached_node = builder.add_node(cfr::node_kind::player);
    const auto zero_reach_node = builder.add_node(cfr::node_kind::player);
    const auto reached_terminal = builder.add_node(cfr::node_kind::terminal);
    const auto zero_reach_terminal = builder.add_node(cfr::node_kind::terminal);
    builder.add_edge(root, reached_node, 0);
    builder.add_edge(root, zero_reach_node, 1);
    builder.add_edge(reached_node, reached_terminal, 0);
    builder.add_edge(zero_reach_node, zero_reach_terminal, 0);
    builder.set_infoset_id(root, 0);
    builder.set_infoset_id(reached_node, 1);
    builder.set_infoset_id(zero_reach_node, 2);
    auto graph = require_extraction_graph(builder.build());

    cfr::action_table_layout layout;
    layout.action_offsets = {0, 2, 3, 4};
    cfr::strategy_sum_table strategy_sums(layout);
    strategy_sums.value(0, 0) = 1.0f;
    strategy_sums.value(0, 1) = 0.0f;
    strategy_sums.value(1, 0) = 1.0f;
    strategy_sums.value(2, 0) = 1.0f;

    const auto board = extraction_test_river_board();
    const auto cache = zeta::holdem::make_river_terminal_cache(board);
    const auto [oop_combo, ip_combo] = first_extraction_compatible_live_combos(cache);
    zeta::holdem::reach_vector oop_range{};
    zeta::holdem::reach_vector ip_range{};
    oop_range[oop_combo] = 1.0f;
    ip_range[ip_combo] = 1.0f;

    zeta::holdem::terminal_state_table<2> terminal_states;
    terminal_states.states.push_back(zeta::holdem::make_showdown_terminal_state(
        zeta::holdem::make_heads_up_context(200.0, 0.0, 50.0, 50.0)));
    std::vector<cfr::traversal::river_terminal_leaf> terminal_leaves(graph.node_count);
    for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
        if (graph.is_terminal(node_id)) {
            terminal_leaves[node_id] = cfr::traversal::river_terminal_leaf{0};
        }
    }

    cfr::solver::solver_graph_annotations annotations;
    annotations.actor_by_node.assign(graph.node_count, cfr::solver::INVALID_PLAYER);
    for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
        if (graph.is_player_node(node_id)) {
            annotations.actor_by_node[node_id] = 0u;
        }
    }
    annotations.state_by_node.assign(graph.node_count, cfr::solver::solver_node_state_metadata{
        .street = cfr::solver::holdem_street::river,
        .public_state_id = 13u,
        .betting_state_id = 0u
    });

    auto extracted = extract_river_heads_up_result_store(river_heads_up_extraction_input{
        .graph = &graph,
        .annotations = &annotations,
        .strategy_sums = &strategy_sums,
        .river_cache = &cache,
        .terminal_leaves = terminal_leaves,
        .terminal_states = terminal_states.view(),
        .ranges = {oop_range, ip_range}
    });
    BOOST_REQUIRE(extracted.has_value());
    BOOST_REQUIRE_EQUAL(extracted->node_count(), 3u);

    const auto node = extracted->node(0);
    uint32_t combo_local = INVALID_COMBO_LOCAL_INDEX;
    for (uint32_t local = 0; local < node.combo_count(); ++local) {
        if (node.combo_index(local) != oop_combo && node.values().range_weight(local) == 0.0f) {
            combo_local = local;
            break;
        }
    }
    BOOST_REQUIRE_NE(combo_local, INVALID_COMBO_LOCAL_INDEX);
    BOOST_CHECK_SMALL(node.values().range_weight(combo_local), 1e-6f);
    BOOST_CHECK(node.values().reach_probability(combo_local) >= 0.0f);
    BOOST_CHECK(std::isfinite(node.values().combo_ev(combo_local)));
    BOOST_CHECK(std::isfinite(node.values().q_value(combo_local, 0)));
    BOOST_CHECK(std::isfinite(node.equity().showdown_equity(combo_local)));
}

BOOST_AUTO_TEST_CASE(river_hu_extraction_differential_matches_oracle_and_query_contract)
{
    namespace cfr = zeta::holdem::cfr;

    cfr::graph_builder builder;
    const auto root = builder.add_node(cfr::node_kind::player);
    const auto oop_node = builder.add_node(cfr::node_kind::player);
    const auto ip_node = builder.add_node(cfr::node_kind::player);
    const auto oop_showdown_terminal = builder.add_node(cfr::node_kind::terminal);
    const auto oop_fold_terminal = builder.add_node(cfr::node_kind::terminal);
    const auto ip_showdown_terminal = builder.add_node(cfr::node_kind::terminal);
    const auto ip_fold_terminal = builder.add_node(cfr::node_kind::terminal);
    builder.add_edge(root, oop_node, 0);
    builder.add_edge(root, ip_node, 1);
    builder.add_edge(oop_node, oop_showdown_terminal, 0);
    builder.add_edge(oop_node, oop_fold_terminal, 1);
    builder.add_edge(ip_node, ip_showdown_terminal, 0);
    builder.add_edge(ip_node, ip_fold_terminal, 1);
    builder.set_infoset_id(root, 0);
    builder.set_infoset_id(oop_node, 1);
    builder.set_infoset_id(ip_node, 2);
    std::vector<uint32_t> remap;
    auto graph_result = builder.build(remap);
    BOOST_REQUIRE_MESSAGE(graph_result.has_value(), zeta::holdem::cfr::to_string(graph_result.error().kind));
    auto graph = std::move(*graph_result);
    const auto root_node = remap[root];
    const auto oop_node_id = remap[oop_node];
    const auto ip_node_id = remap[ip_node];
    const auto oop_showdown_terminal_id = remap[oop_showdown_terminal];
    const auto oop_fold_terminal_id = remap[oop_fold_terminal];
    const auto ip_showdown_terminal_id = remap[ip_showdown_terminal];
    const auto ip_fold_terminal_id = remap[ip_fold_terminal];

    cfr::action_table_layout layout;
    layout.action_offsets = {0, 2, 4, 6};
    cfr::strategy_sum_table strategy_sums(layout);
    strategy_sums.value(0, 0) = 2.0f;
    strategy_sums.value(0, 1) = 3.0f;
    strategy_sums.value(1, 0) = 7.0f;
    strategy_sums.value(1, 1) = 5.0f;
    strategy_sums.value(2, 0) = 4.0f;
    strategy_sums.value(2, 1) = 6.0f;

    const auto board = extraction_test_river_board();
    const auto cache = zeta::holdem::make_river_terminal_cache(board);
    zeta::holdem::reach_vector oop_range{};
    zeta::holdem::reach_vector ip_range{};
    for (std::size_t i = 0; i < cache.rank_order_count && i < 64; ++i) {
        oop_range[cache.rank_order[i]] = (i % 7u == 0u) ? 0.75f : 0.25f;
    }
    for (std::size_t i = 8; i < cache.rank_order_count && i < 72; ++i) {
        ip_range[cache.rank_order[i]] = (i % 5u == 0u) ? 1.0f : 0.35f;
    }

    zeta::holdem::terminal_state_table<2> terminal_states;
    const auto context = zeta::holdem::make_heads_up_context(180.0, 0.0, 45.0, 45.0);
    terminal_states.states.push_back(zeta::holdem::make_showdown_terminal_state(context));
    terminal_states.states.push_back(zeta::holdem::make_fold_terminal_state(context, zeta::holdem::heads_up_player::oop));
    terminal_states.states.push_back(zeta::holdem::make_fold_terminal_state(context, zeta::holdem::heads_up_player::ip));

    std::vector<cfr::traversal::river_terminal_leaf> terminal_leaves(graph.node_count);
    terminal_leaves[oop_showdown_terminal_id] = cfr::traversal::river_terminal_leaf{0};
    terminal_leaves[oop_fold_terminal_id] = cfr::traversal::river_terminal_leaf{1};
    terminal_leaves[ip_showdown_terminal_id] = cfr::traversal::river_terminal_leaf{0};
    terminal_leaves[ip_fold_terminal_id] = cfr::traversal::river_terminal_leaf{2};

    cfr::solver::solver_graph_annotations annotations;
    annotations.actor_by_node.assign(graph.node_count, cfr::solver::INVALID_PLAYER);
    annotations.actor_by_node[root_node] = 1u;
    annotations.actor_by_node[oop_node_id] = 0u;
    annotations.actor_by_node[ip_node_id] = 1u;
    annotations.state_by_node.assign(graph.node_count, cfr::solver::solver_node_state_metadata{
        .street = cfr::solver::holdem_street::river,
        .public_state_id = 19u,
        .betting_state_id = 0u
    });

    const auto input = river_heads_up_extraction_input{
        .graph = &graph,
        .annotations = &annotations,
        .strategy_sums = &strategy_sums,
        .river_cache = &cache,
        .terminal_leaves = terminal_leaves,
        .terminal_states = terminal_states.view(),
        .ranges = {oop_range, ip_range}
    };
    auto extracted = extract_river_heads_up_result_store(input);
    BOOST_REQUIRE(extracted.has_value());

    const auto combo_indices = oracle_combo_domain(cache);
    const auto oracle_reaches = oracle_reach_by_node(graph, annotations, strategy_sums);
    const auto oracle_values = oracle_value_cache(
        graph,
        annotations,
        strategy_sums,
        cache,
        terminal_leaves,
        terminal_states.view(),
        {oop_range, ip_range});

    uint32_t result_node_id = 0;
    for (uint32_t graph_node = 0; graph_node < graph.node_count; ++graph_node) {
        if (!graph.is_player_node(graph_node)) {
            continue;
        }

        const auto actor = annotations.actor_by_node[graph_node] == 0u
            ? zeta::holdem::heads_up_player::oop
            : zeta::holdem::heads_up_player::ip;
        const auto actor_slot = zeta::holdem::player_index(actor);
        const auto opponent_slot = actor_slot == 0u ? 1u : 0u;
        const auto actor_reach = actor == zeta::holdem::heads_up_player::oop
            ? oracle_reaches[graph_node].oop
            : oracle_reaches[graph_node].ip;
        const auto opponent_reach = actor == zeta::holdem::heads_up_player::oop
            ? oracle_reaches[graph_node].ip
            : oracle_reaches[graph_node].oop;
        const auto& actor_range = input.ranges[actor_slot];
        const auto& opponent_range = input.ranges[opponent_slot];
        const auto strategy = oracle_normalized_strategy(strategy_sums, graph.infoset_id[graph_node]);

        const auto node = extracted->node(result_node_id++);
        BOOST_CHECK_EQUAL(node.action_count(), graph.action_count(graph_node));
        BOOST_CHECK_EQUAL(node.public_state_id(), annotations.state_by_node[graph_node].public_state_id);
        const auto context_strategy = extracted->context_strategy(node.strategy_context_id());
        BOOST_CHECK_EQUAL(context_strategy.entries().data(), node.strategy().entries().data());

        double expected_range_reach_mass = 0.0;
        double expected_reach_weighted_ev = 0.0;
        double expected_counterfactual_value = 0.0;
        for (uint32_t local = 0; local < node.combo_count(); ++local) {
            const auto combo = node.combo_index(local);
            BOOST_CHECK_EQUAL(combo, combo_indices[local]);
            const auto combo_ev = oracle_values[graph_node][actor_slot][local];
            BOOST_CHECK_EQUAL(float_bits(node.strategy().frequency(local, 0)), float_bits(strategy[0]));
            if (node.action_count() > 1u) {
                BOOST_CHECK_EQUAL(float_bits(node.strategy().frequency(local, 1)), float_bits(strategy[1]));
            }

            const auto range_weight = actor_range[combo];
            const auto range_reach_weight = static_cast<double>(range_weight) * static_cast<double>(actor_reach);
            expected_range_reach_mass += range_reach_weight;
            expected_reach_weighted_ev += range_reach_weight * combo_ev;
            expected_counterfactual_value += static_cast<double>(range_weight)
                * static_cast<double>(opponent_reach)
                * static_cast<double>(oracle_reaches[graph_node].chance)
                * combo_ev;

            BOOST_CHECK_EQUAL(float_bits(node.values().range_weight(local)), float_bits(range_weight));
            BOOST_CHECK_EQUAL(float_bits(node.values().reach_probability(local)), float_bits(actor_reach));
            BOOST_CHECK_EQUAL(double_bits(node.values().combo_ev(local)), double_bits(combo_ev));
            for (const auto child : graph.out_edges(graph_node)) {
                const auto child_q = oracle_values[child.child_node][actor_slot][local];
                BOOST_CHECK_EQUAL(double_bits(node.values().q_value(local, child.action_index)), double_bits(child_q));
                BOOST_CHECK_EQUAL(
                    double_bits(node.values().profile_advantage(local, child.action_index)),
                    double_bits(child_q - combo_ev));
            }

            const auto expected_equity = oracle_showdown_equity(cache, opponent_range, opponent_reach, combo);
            BOOST_CHECK_SMALL(std::abs(node.equity().showdown_equity(local) - expected_equity), 1e-6);
            BOOST_CHECK(node.categories().classification(local).made_hand_tier >= zeta::holdem::hand_category::high_card);
        }

        BOOST_CHECK_EQUAL(double_bits(node.values().range_reach_mass()), double_bits(expected_range_reach_mass));
        BOOST_CHECK_EQUAL(double_bits(node.values().reach_weighted_ev()), double_bits(expected_reach_weighted_ev));
        BOOST_CHECK_EQUAL(
            double_bits(node.values().conditional_range_ev()),
            double_bits(compute_conditional_range_ev(expected_reach_weighted_ev, expected_range_reach_mass)));
        BOOST_CHECK_EQUAL(double_bits(node.values().counterfactual_value()), double_bits(expected_counterfactual_value));
    }

    BOOST_CHECK_EQUAL(result_node_id, extracted->node_count());
}

BOOST_AUTO_TEST_CASE(river_hu_extraction_is_bitwise_deterministic)
{
    namespace cfr = zeta::holdem::cfr;

    cfr::graph_builder builder;
    const auto root = builder.add_node(cfr::node_kind::player);
    const auto player_next = builder.add_node(cfr::node_kind::player);
    const auto showdown_terminal = builder.add_node(cfr::node_kind::terminal);
    const auto fold_terminal_root = builder.add_node(cfr::node_kind::terminal);
    const auto fold_terminal_child = builder.add_node(cfr::node_kind::terminal);
    builder.add_edge(root, player_next, 0);
    builder.add_edge(root, fold_terminal_root, 1);
    builder.add_edge(player_next, showdown_terminal, 0);
    builder.add_edge(player_next, fold_terminal_child, 1);
    builder.set_infoset_id(root, 0);
    builder.set_infoset_id(player_next, 1);
    std::vector<uint32_t> remap;
    auto graph_result = builder.build(remap);
    BOOST_REQUIRE_MESSAGE(graph_result.has_value(), zeta::holdem::cfr::to_string(graph_result.error().kind));
    auto graph = std::move(*graph_result);
    const auto root_node = remap[root];
    const auto player_next_node = remap[player_next];
    const auto showdown_terminal_id = remap[showdown_terminal];
    const auto fold_terminal_root_id = remap[fold_terminal_root];
    const auto fold_terminal_child_id = remap[fold_terminal_child];

    cfr::action_table_layout layout;
    layout.action_offsets = {0, 2, 4};
    cfr::strategy_sum_table strategy_sums(layout);
    strategy_sums.value(0, 0) = 9.0f;
    strategy_sums.value(0, 1) = 11.0f;
    strategy_sums.value(1, 0) = 3.0f;
    strategy_sums.value(1, 1) = 7.0f;

    const auto board = extraction_test_river_board();
    const auto cache = zeta::holdem::make_river_terminal_cache(board);
    zeta::holdem::reach_vector oop_range{};
    zeta::holdem::reach_vector ip_range{};
    for (std::size_t i = 0; i < cache.rank_order_count && i < 50; ++i) {
        oop_range[cache.rank_order[i]] = 0.4f + static_cast<float>(i % 3u) * 0.2f;
        ip_range[cache.rank_order[(i + 9u) % cache.rank_order_count]] = 0.3f + static_cast<float>(i % 5u) * 0.15f;
    }

    zeta::holdem::terminal_state_table<2> terminal_states;
    const auto context = zeta::holdem::make_heads_up_context(220.0, 0.0, 60.0, 50.0);
    terminal_states.states.push_back(zeta::holdem::make_showdown_terminal_state(context));
    terminal_states.states.push_back(zeta::holdem::make_fold_terminal_state(context, zeta::holdem::heads_up_player::oop));

    std::vector<cfr::traversal::river_terminal_leaf> terminal_leaves(graph.node_count);
    terminal_leaves[showdown_terminal_id] = cfr::traversal::river_terminal_leaf{0};
    terminal_leaves[fold_terminal_root_id] = cfr::traversal::river_terminal_leaf{1};
    terminal_leaves[fold_terminal_child_id] = cfr::traversal::river_terminal_leaf{1};

    cfr::solver::solver_graph_annotations annotations;
    annotations.actor_by_node.assign(graph.node_count, cfr::solver::INVALID_PLAYER);
    annotations.actor_by_node[root_node] = 0u;
    annotations.actor_by_node[player_next_node] = 1u;
    annotations.state_by_node.assign(graph.node_count, cfr::solver::solver_node_state_metadata{
        .street = cfr::solver::holdem_street::river,
        .public_state_id = 23u,
        .betting_state_id = 0u
    });

    const auto input = river_heads_up_extraction_input{
        .graph = &graph,
        .annotations = &annotations,
        .strategy_sums = &strategy_sums,
        .river_cache = &cache,
        .terminal_leaves = terminal_leaves,
        .terminal_states = terminal_states.view(),
        .ranges = {oop_range, ip_range}
    };
    auto baseline = extract_river_heads_up_result_store(input);
    BOOST_REQUIRE(baseline.has_value());

    for (int iteration = 0; iteration < 5; ++iteration) {
        auto current = extract_river_heads_up_result_store(input);
        BOOST_REQUIRE(current.has_value());
        BOOST_REQUIRE_EQUAL(current->node_count(), baseline->node_count());
        BOOST_REQUIRE_EQUAL(current->strategy_context_count(), baseline->strategy_context_count());

        for (uint32_t node_id = 0; node_id < baseline->node_count(); ++node_id) {
            const auto lhs = baseline->node(node_id);
            const auto rhs = current->node(node_id);
            BOOST_CHECK_EQUAL(lhs.node_id(), rhs.node_id());
            BOOST_CHECK_EQUAL(lhs.strategy_context_id(), rhs.strategy_context_id());
            BOOST_CHECK_EQUAL(lhs.public_state_id(), rhs.public_state_id());
            BOOST_CHECK_EQUAL(lhs.combo_count(), rhs.combo_count());
            BOOST_CHECK_EQUAL(lhs.action_count(), rhs.action_count());
            for (uint32_t combo = 0; combo < lhs.combo_count(); ++combo) {
                BOOST_CHECK_EQUAL(lhs.combo_index(combo), rhs.combo_index(combo));
                BOOST_CHECK_EQUAL(float_bits(lhs.values().range_weight(combo)), float_bits(rhs.values().range_weight(combo)));
                BOOST_CHECK_EQUAL(float_bits(lhs.values().reach_probability(combo)), float_bits(rhs.values().reach_probability(combo)));
                BOOST_CHECK_EQUAL(double_bits(lhs.values().combo_ev(combo)), double_bits(rhs.values().combo_ev(combo)));
                BOOST_CHECK_EQUAL(double_bits(lhs.equity().showdown_equity(combo)), double_bits(rhs.equity().showdown_equity(combo)));
                const auto lhs_category = lhs.categories().classification(combo);
                const auto rhs_category = rhs.categories().classification(combo);
                BOOST_CHECK_EQUAL(static_cast<int>(lhs_category.made_hand_tier), static_cast<int>(rhs_category.made_hand_tier));
                BOOST_CHECK_EQUAL(static_cast<int>(lhs_category.source), static_cast<int>(rhs_category.source));
                BOOST_CHECK_EQUAL(static_cast<int>(lhs_category.pair_pos), static_cast<int>(rhs_category.pair_pos));
                BOOST_CHECK_EQUAL(static_cast<int>(lhs_category.kicker), static_cast<int>(rhs_category.kicker));
                BOOST_CHECK_EQUAL(static_cast<int>(lhs_category.draws), static_cast<int>(rhs_category.draws));
                BOOST_CHECK_EQUAL(static_cast<int>(lhs_category.blockers), static_cast<int>(rhs_category.blockers));
                for (uint16_t action = 0; action < lhs.action_count(); ++action) {
                    BOOST_CHECK_EQUAL(float_bits(lhs.strategy().frequency(combo, action)), float_bits(rhs.strategy().frequency(combo, action)));
                    BOOST_CHECK_EQUAL(double_bits(lhs.values().q_value(combo, action)), double_bits(rhs.values().q_value(combo, action)));
                    BOOST_CHECK_EQUAL(double_bits(lhs.values().profile_advantage(combo, action)), double_bits(rhs.values().profile_advantage(combo, action)));
                }
            }
            BOOST_CHECK_EQUAL(double_bits(lhs.values().range_reach_mass()), double_bits(rhs.values().range_reach_mass()));
            BOOST_CHECK_EQUAL(double_bits(lhs.values().reach_weighted_ev()), double_bits(rhs.values().reach_weighted_ev()));
            BOOST_CHECK_EQUAL(double_bits(lhs.values().conditional_range_ev()), double_bits(rhs.values().conditional_range_ev()));
            BOOST_CHECK_EQUAL(double_bits(lhs.values().counterfactual_value()), double_bits(rhs.values().counterfactual_value()));
        }
    }
}

BOOST_AUTO_TEST_CASE(river_hu_extraction_benchmark_and_memory_budget)
{
    namespace cfr = zeta::holdem::cfr;

    cfr::graph_builder builder;
    const auto root = builder.add_node(cfr::node_kind::player);
    const auto left = builder.add_node(cfr::node_kind::player);
    const auto right = builder.add_node(cfr::node_kind::player);
    const auto showdown_left_terminal = builder.add_node(cfr::node_kind::terminal);
    const auto fold_oop_terminal = builder.add_node(cfr::node_kind::terminal);
    const auto showdown_right_terminal = builder.add_node(cfr::node_kind::terminal);
    const auto fold_ip_terminal = builder.add_node(cfr::node_kind::terminal);
    builder.add_edge(root, left, 0);
    builder.add_edge(root, right, 1);
    builder.add_edge(left, showdown_left_terminal, 0);
    builder.add_edge(left, fold_oop_terminal, 1);
    builder.add_edge(right, showdown_right_terminal, 0);
    builder.add_edge(right, fold_ip_terminal, 1);
    builder.set_infoset_id(root, 0);
    builder.set_infoset_id(left, 1);
    builder.set_infoset_id(right, 2);
    std::vector<uint32_t> remap;
    auto graph_result = builder.build(remap);
    BOOST_REQUIRE_MESSAGE(graph_result.has_value(), zeta::holdem::cfr::to_string(graph_result.error().kind));
    auto graph = std::move(*graph_result);
    const auto root_node = remap[root];
    const auto left_node = remap[left];
    const auto right_node = remap[right];
    const auto showdown_left_terminal_id = remap[showdown_left_terminal];
    const auto fold_oop_terminal_id = remap[fold_oop_terminal];
    const auto showdown_right_terminal_id = remap[showdown_right_terminal];
    const auto fold_ip_terminal_id = remap[fold_ip_terminal];

    cfr::action_table_layout layout;
    layout.action_offsets = {0, 2, 4, 6};
    cfr::strategy_sum_table strategy_sums(layout);
    strategy_sums.value(0, 0) = 5.0f;
    strategy_sums.value(0, 1) = 5.0f;
    strategy_sums.value(1, 0) = 6.0f;
    strategy_sums.value(1, 1) = 4.0f;
    strategy_sums.value(2, 0) = 3.0f;
    strategy_sums.value(2, 1) = 7.0f;

    const auto board = extraction_test_river_board();
    const auto cache = zeta::holdem::make_river_terminal_cache(board);
    zeta::holdem::reach_vector oop_range{};
    zeta::holdem::reach_vector ip_range{};
    for (std::size_t i = 0; i < cache.rank_order_count && i < 160; ++i) {
        oop_range[cache.rank_order[i]] = 0.5f;
        ip_range[cache.rank_order[(i + 11u) % cache.rank_order_count]] = 0.5f;
    }

    zeta::holdem::terminal_state_table<2> terminal_states;
    const auto context = zeta::holdem::make_heads_up_context(300.0, 0.0, 75.0, 75.0);
    terminal_states.states.push_back(zeta::holdem::make_showdown_terminal_state(context));
    terminal_states.states.push_back(zeta::holdem::make_fold_terminal_state(context, zeta::holdem::heads_up_player::oop));
    terminal_states.states.push_back(zeta::holdem::make_fold_terminal_state(context, zeta::holdem::heads_up_player::ip));

    std::vector<cfr::traversal::river_terminal_leaf> terminal_leaves(graph.node_count);
    terminal_leaves[showdown_left_terminal_id] = cfr::traversal::river_terminal_leaf{0};
    terminal_leaves[fold_oop_terminal_id] = cfr::traversal::river_terminal_leaf{1};
    terminal_leaves[showdown_right_terminal_id] = cfr::traversal::river_terminal_leaf{0};
    terminal_leaves[fold_ip_terminal_id] = cfr::traversal::river_terminal_leaf{2};

    cfr::solver::solver_graph_annotations annotations;
    annotations.actor_by_node.assign(graph.node_count, cfr::solver::INVALID_PLAYER);
    annotations.actor_by_node[root_node] = 0u;
    annotations.actor_by_node[left_node] = 0u;
    annotations.actor_by_node[right_node] = 1u;
    annotations.state_by_node.assign(graph.node_count, cfr::solver::solver_node_state_metadata{
        .street = cfr::solver::holdem_street::river,
        .public_state_id = 29u,
        .betting_state_id = 0u
    });

    const auto input = river_heads_up_extraction_input{
        .graph = &graph,
        .annotations = &annotations,
        .strategy_sums = &strategy_sums,
        .river_cache = &cache,
        .terminal_leaves = terminal_leaves,
        .terminal_states = terminal_states.view(),
        .ranges = {oop_range, ip_range}
    };

    constexpr int benchmark_iterations = 25;
    const auto start = std::chrono::steady_clock::now();
    std::expected<result_store, river_extraction_error> last{};
    for (int i = 0; i < benchmark_iterations; ++i) {
        last = extract_river_heads_up_result_store(input);
        BOOST_REQUIRE(last.has_value());
    }
    const auto end = std::chrono::steady_clock::now();
    const auto elapsed_seconds = std::chrono::duration<double>(end - start).count();
    const auto throughput = static_cast<double>(benchmark_iterations) / elapsed_seconds;
    BOOST_TEST_MESSAGE("river extraction benchmark: " << throughput << " extracts/s over " << benchmark_iterations << " iterations");
    BOOST_CHECK_GT(throughput, 0.0);

    for (uint32_t node_id = 0; node_id < last->node_count(); ++node_id) {
        const auto node = last->node(node_id);
        const auto bytes = node_footprint_bytes(node.combo_count(), node.action_count(), 1u);
        BOOST_CHECK_LE(bytes, two_action_node_footprint_budget_bytes);
    }
}

BOOST_AUTO_TEST_SUITE_END()
