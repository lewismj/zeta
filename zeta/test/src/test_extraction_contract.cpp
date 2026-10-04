#include <boost/test/unit_test.hpp>

#include "cfr/extraction/contract.h"

#include <array>
#include <cmath>
#include <sstream>
#include <vector>

using namespace zeta::holdem::cfr::extraction;

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

BOOST_AUTO_TEST_SUITE_END()
