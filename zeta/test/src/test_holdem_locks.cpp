#include <boost/test/unit_test.hpp>

#include "cli/solve_cli.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <initializer_list>
#include <numeric>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

// Node locking: authoring format, validation, the constrained CFR+ solve, and the
// artifact fields that record it. See doc/holdem/plans/node_locking_plan.md.

namespace {

    namespace cli = zeta::holdem::cli;

    constexpr const char* river_spot = R"({
  "street": "river",
  "players": ["BTN", "BB"],
  "board": ["Ah", "Kd", "7c", "4s", "2h"],
  "ranges": ["AA,KK,77,QJs,65s", "AK,AQ,KQ,JTs"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "betting_policy": {"fixed_pot_fractions": [0.75], "max_raises": 1},
  "max_history": 6,
  "root_actor": "BTN",
  "hero_seat": "BTN"
})";

    constexpr const char* turn_spot = R"({
  "street": "turn",
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c", "4h"],
  "ranges": ["AK,77,QJs", "AQ,KQ,JTs"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "betting_policy": {"fixed_pot_fractions": [0.5], "max_raises": 1},
  "max_history": 6
})";

    struct cli::solve_spot parse(const char* json)
    {
        auto spot = cli::parse_spot_json(json);
        BOOST_REQUIRE_MESSAGE(spot.has_value(), (spot ? std::string{} : spot.error().message));
        return *spot;
    }

    cli::node_lock lock(
        std::vector<std::string> path,
        std::vector<cli::node_lock_action> actions,
        std::string hands = {})
    {
        return cli::node_lock{.path = std::move(path), .hands = std::move(hands), .actions = std::move(actions)};
    }

    cli::solve_artifact solve(const struct cli::solve_spot& spot, const uint64_t iterations, cli::solve_runtime_options runtime = {})
    {
        auto output = cli::solve_spot(spot, iterations, runtime);
        BOOST_REQUIRE_MESSAGE(output.has_value(), (output ? std::string{} : output.error().message));
        return std::move(output->artifact);
    }

    // The path is taken by value: the returned node lives in the artifact, and GCC's
    // -Wdangling-reference would otherwise suspect it of referring to the path temporary.
    const cli::solved_node& node_at(const cli::solve_artifact& artifact, const std::initializer_list<std::string_view> path)
    {
        for (const auto& node : artifact.solved_nodes) {
            if (node.kind != "player") {
                continue;
            }
            if (const auto node_path = cli::node_lock_path(artifact, node.node_id);
                node_path.has_value() && std::ranges::equal(*node_path, path)) {
                return node;
            }
        }
        BOOST_FAIL("No player node at the requested path.");
        return artifact.solved_nodes.front();
    }

    double frequency_of(const std::vector<cli::action_strategy>& strategy, const std::string& action)
    {
        for (const auto& entry : strategy) {
            if (entry.action == action) {
                return entry.frequency;
            }
        }
        BOOST_FAIL("Action " + action + " not found.");
        return 0.0;
    }

    // The root actor's range EV in chips, weighting each hand by its range weight.
    double hero_root_ev(const cli::solve_artifact& artifact)
    {
        double value = 0.0;
        double mass = 0.0;
        for (const auto& row : artifact.strategy) {
            value += row.range_weight * row.counterfactual_value;
            mass += row.range_weight * row.opponent_reach_mass;
        }
        return mass > 0.0 ? value / mass : 0.0;
    }

    std::string solve_error(const struct cli::solve_spot& spot)
    {
        auto output = cli::solve_spot(spot, 2);
        BOOST_REQUIRE(!output.has_value());
        BOOST_CHECK(output.error().kind == cli::cli_error_kind::invalid_spot);
        return output.error().message;
    }

    bool contains(const std::string& text, const std::string& needle)
    {
        return text.find(needle) != std::string::npos;
    }

}

BOOST_AUTO_TEST_SUITE(holdem_node_locks)

BOOST_AUTO_TEST_CASE(every_action_in_the_artifact_has_a_real_label) {
    const auto artifact = solve(parse(turn_spot), 4);
    std::size_t labelled = 0;
    for (const auto& node : artifact.solved_nodes) {
        if (node.kind != "player") {
            continue;
        }
        for (const auto& action : node.range_action_frequencies) {
            BOOST_CHECK_MESSAGE(!std::all_of(action.action.begin(), action.action.end(), [](const char c) {
                return std::isdigit(static_cast<unsigned char>(c)) != 0;
            }), "index label left at node " << node.node_id);
            ++labelled;
        }
        for (const auto& row : node.strategy_rows) {
            for (std::size_t index = 0; index < row.strategy.size(); ++index) {
                BOOST_CHECK_EQUAL(row.strategy[index].action, node.range_action_frequencies[index].action);
            }
        }
    }
    BOOST_CHECK_GT(labelled, 100u);
    // The root keeps its existing labels and a river decision is labelled against its own pot.
    BOOST_CHECK_EQUAL(artifact.root_strategy[1].action, "bet_50");
    const auto& river = node_at(artifact, {"check", "check", "2s"});
    BOOST_CHECK_EQUAL(river.range_action_frequencies[1].action, "bet_50");
}

BOOST_AUTO_TEST_CASE(spot_json_round_trips_locks_in_canonical_form) {
    auto spot = parse(R"({
  "street": "turn", "players": ["BTN", "BB"], "board": ["As", "Kd", "7c", "4h"],
  "ranges": ["AK", "AQ"], "gross_pot": 100.0, "contributions": [50.0, 50.0], "stacks": [100.0, 100.0],
  "locks": [
    {"path": ["check", "check", "qh", "check"], "hands": "AKs", "actions": {"check": 0.3333333333, "all_in": 0.25}},
    {"path": [], "actions": {"bet_50": 0}}
  ]
})");
    BOOST_REQUIRE_EQUAL(spot.locks.size(), 2u);
    const auto& first = spot.locks[0];
    BOOST_CHECK_EQUAL(first.path[2], "Qh");
    BOOST_CHECK_EQUAL(first.hands, "AKs");
    BOOST_REQUIRE_EQUAL(first.actions.size(), 2u);
    BOOST_CHECK_EQUAL(first.actions[0].action, "all_in");
    BOOST_CHECK_EQUAL(first.actions[1].action, "check");
    BOOST_CHECK_CLOSE(first.actions[1].frequency, 0.333333, 1e-9);
    BOOST_CHECK(spot.locks[1].path.empty());

    const auto serialized = cli::serialize_spot_json(spot);
    const auto reparsed = parse(serialized.c_str());
    BOOST_CHECK_EQUAL(cli::serialize_spot_json(reparsed), serialized);

    spot.locks.clear();
    BOOST_CHECK(!contains(cli::serialize_spot_json(spot), "locks"));
}

BOOST_AUTO_TEST_CASE(malformed_locks_are_rejected_when_parsed) {
    const auto parse_error = [](const std::string& locks) {
        const auto json = std::string{R"({"street": "river", "players": ["BTN", "BB"], "board": ["Ah", "Kd", "7c", "4s", "2h"],
            "ranges": ["AA", "KK"], "gross_pot": 100.0, "contributions": [50.0, 50.0], "stacks": [100.0, 100.0], "locks": )"}
            + locks + "}";
        auto spot = cli::parse_spot_json(json);
        BOOST_REQUIRE(!spot.has_value());
        return spot.error().message;
    };
    BOOST_CHECK(contains(parse_error(R"({"path": []})"), "locks must be an array"));
    BOOST_CHECK(contains(parse_error(R"([{"path": []}])"), "Lock 1: actions must be an object"));
    BOOST_CHECK(contains(parse_error(R"([{"path": [], "actions": {}}])"), "at least one action"));
    BOOST_CHECK(contains(parse_error(R"([{"path": [], "actions": {"check": 1.5}}])"), "between 0 and 1"));
    BOOST_CHECK(contains(parse_error(R"([{"path": [], "actions": {"check": -0.1}}])"), "between 0 and 1"));
    BOOST_CHECK(contains(parse_error(R"([{"path": [], "actions": {"check": 0.6, "bet_75": 0.6}}])"), "more than 1"));
    BOOST_CHECK(contains(parse_error(R"([{"path": [], "actions": {"check": 1}}, {"path": [""], "actions": {"check": 1}}])"),
        "Lock 2: path step 1 is empty"));
    BOOST_CHECK(contains(parse_error(R"([{"path": [], "hands": "AZ", "actions": {"check": 1}}])"), "invalid hands"));
}

BOOST_AUTO_TEST_CASE(locks_that_do_not_fit_the_tree_are_rejected_before_solving) {
    auto spot = parse(turn_spot);
    const auto error_for = [&](std::vector<cli::node_lock> locks) {
        spot.locks = std::move(locks);
        return solve_error(spot);
    };
    BOOST_CHECK(contains(error_for({lock({"bet_33"}, {{"fold", 0.0}})}), "path step 1 ('bet_33') is not an action here"));
    BOOST_CHECK(contains(error_for({lock({"check", "check"}, {{"check", 0.0}})}), "ends where a card is dealt"));
    BOOST_CHECK(contains(error_for({lock({"check", "*"}, {{"check", 0.0}})}), "'*' only matches a dealt card"));
    BOOST_CHECK(contains(error_for({lock({"check", "check", "As"}, {{"check", 0.0}})}), "not a card dealt here"));
    BOOST_CHECK(contains(error_for({lock({"all_in", "fold", "check"}, {{"check", 0.0}})}), "past the end of the hand"));
    BOOST_CHECK(contains(error_for({lock({"all_in", "fold"}, {{"check", 0.0}})}), "ends at the end of the hand"));
    BOOST_CHECK(contains(error_for({lock({}, {{"raise_99", 0.0}})}), "'raise_99' is not an action at the locked node"));
    BOOST_CHECK(contains(error_for({lock({}, {{"check", 0.5}, {"bet_50", 0.2}, {"all_in", 0.2}})}), "must add up to 1"));
    BOOST_CHECK(contains(error_for({lock({}, {{"check", 1.0}}), lock({}, {{"check", 0.0}})}),
        "Lock 2: another whole-range lock"));
    BOOST_CHECK(contains(error_for({lock({}, {{"check", 1.0}}, "AK"), lock({}, {{"check", 0.0}}, "AKo")}),
        "overlap lock 1"));
    BOOST_CHECK(contains(error_for({lock({}, {{"check", 1.0}}, "22")}), "match nothing in BTN's range"));

    spot.locks = {lock({}, {{"check", 1.0}}, "AK")};
    auto output = cli::solve_spot(spot, 2, {.enable_card_isomorphism = true, .allow_lossy_card_isomorphism = true});
    BOOST_REQUIRE(!output.has_value());
    BOOST_CHECK(contains(output.error().message, "card isomorphism"));
}

BOOST_AUTO_TEST_CASE(a_solve_without_locks_records_none) {
    const auto artifact = solve(parse(river_spot), 20);
    BOOST_CHECK(artifact.solver.locks.empty());
    BOOST_CHECK_EQUAL(artifact.solver.hashes.locks_hash, 0u);
    for (const auto& warning : artifact.solver.warnings) {
        BOOST_CHECK(!contains(warning, "node lock"));
    }
    for (const auto& node : artifact.solved_nodes) {
        BOOST_CHECK(node.lock_indices.empty());
        for (const auto& row : node.strategy_rows) {
            BOOST_CHECK(!row.locked);
        }
    }
}

BOOST_AUTO_TEST_CASE(a_full_lock_is_reproduced_exactly) {
    auto spot = parse(river_spot);
    spot.locks = {lock({"bet_75"}, {{"fold", 0.2}, {"call", 0.7}, {"all_in", 0.1}})};
    const auto artifact = solve(spot, 60);
    const auto& node = node_at(artifact, {"bet_75"});
    BOOST_REQUIRE_EQUAL(node.lock_indices.size(), 1u);
    BOOST_CHECK_EQUAL(node.lock_indices[0], 0u);
    BOOST_CHECK_CLOSE(frequency_of(node.range_action_frequencies, "fold"), 0.2, 1e-4);
    BOOST_CHECK_CLOSE(frequency_of(node.range_action_frequencies, "call"), 0.7, 1e-4);
    BOOST_CHECK_CLOSE(frequency_of(node.range_action_frequencies, "all_in"), 0.1, 1e-4);
    for (const auto& row : node.strategy_rows) {
        BOOST_CHECK(row.locked);
        BOOST_CHECK_CLOSE(frequency_of(row.strategy, "call"), 0.7, 1e-4);
    }
    BOOST_CHECK_EQUAL(artifact.solver.locks.size(), 1u);
    BOOST_CHECK(std::ranges::any_of(artifact.solver.warnings, [](const std::string& warning) {
        return contains(warning, "1 node lock");
    }));
}

BOOST_AUTO_TEST_CASE(a_partial_lock_pins_its_actions_and_solves_the_rest) {
    auto spot = parse(river_spot);
    spot.locks = {lock({"bet_75"}, {{"fold", 0.25}})};
    const auto artifact = solve(spot, 120);
    const auto& node = node_at(artifact, {"bet_75"});
    for (const auto& row : node.strategy_rows) {
        BOOST_CHECK_CLOSE(frequency_of(row.strategy, "fold"), 0.25, 1e-3);
        BOOST_CHECK_CLOSE(frequency_of(row.strategy, "call") + frequency_of(row.strategy, "all_in"), 0.75, 1e-3);
    }
    // The free actions are solved, not spread evenly (which would give 0.375 each).
    double spread = 0.0;
    for (const auto& row : node.strategy_rows) {
        spread = std::max(spread, std::fabs(frequency_of(row.strategy, "call") - frequency_of(row.strategy, "all_in")));
    }
    BOOST_CHECK_GT(spread, 0.2);
}

BOOST_AUTO_TEST_CASE(the_opponent_exploits_a_lock) {
    const auto unlocked = solve(parse(river_spot), 300);
    auto spot = parse(river_spot);
    spot.locks = {lock({"bet_75"}, {{"fold", 1.0}})};
    const auto locked = solve(spot, 300);

    // BB folding everything to a bet makes betting pure profit for BTN.
    BOOST_CHECK_GT(hero_root_ev(locked), hero_root_ev(unlocked) + 10.0);
    BOOST_CHECK_GT(frequency_of(locked.root_strategy, "bet_75"), frequency_of(unlocked.root_strategy, "bet_75") + 0.2);
}

BOOST_AUTO_TEST_CASE(a_hand_lock_overrides_the_range_lock_for_its_hands_only) {
    auto spot = parse(river_spot);
    spot.locks = {
        lock({"bet_75"}, {{"fold", 1.0}}, "AK"),
        lock({"bet_75"}, {{"fold", 0.0}})
    };
    const auto artifact = solve(spot, 80);
    const auto& node = node_at(artifact, {"bet_75"});
    BOOST_CHECK_EQUAL(node.lock_indices.size(), 2u);
    std::size_t ak_rows = 0;
    for (const auto& row : node.strategy_rows) {
        BOOST_CHECK(row.locked);
        const bool ak = (row.hand.starts_with("A") && row.hand[2] == 'K') || (row.hand.starts_with("K") && row.hand[2] == 'A');
        if (ak) {
            ++ak_rows;
            BOOST_CHECK_CLOSE(frequency_of(row.strategy, "fold"), 1.0, 1e-4);
        } else {
            BOOST_CHECK_SMALL(frequency_of(row.strategy, "fold"), 1e-6);
        }
    }
    BOOST_CHECK_GT(ak_rows, 0u);
}

BOOST_AUTO_TEST_CASE(a_wildcard_path_locks_the_node_on_every_runout) {
    auto spot = parse(turn_spot);
    spot.locks = {lock({"check", "check", "*"}, {{"bet_50", 0.0}})};
    const auto artifact = solve(spot, 30);
    std::size_t locked_nodes = 0;
    for (const auto& node : artifact.solved_nodes) {
        if (node.lock_indices.empty()) {
            continue;
        }
        ++locked_nodes;
        const auto path = cli::node_lock_path(artifact, node.node_id);
        BOOST_REQUIRE(path.has_value());
        BOOST_REQUIRE_EQUAL(path->size(), 3u);
        BOOST_CHECK_EQUAL((*path)[0], "check");
        BOOST_CHECK_EQUAL((*path)[1], "check");
        BOOST_CHECK_SMALL(frequency_of(node.range_action_frequencies, "bet_50"), 1e-6);
    }
    BOOST_CHECK_EQUAL(locked_nodes, 48u);  // one per unseen river card

    // A named card locks that runout only.
    spot.locks = {lock({"check", "check", "Qh"}, {{"bet_50", 0.0}})};
    const auto single = solve(spot, 4);
    BOOST_CHECK_EQUAL(std::ranges::count_if(single.solved_nodes, [](const cli::solved_node& node) {
        return !node.lock_indices.empty();
    }), 1);
}

BOOST_AUTO_TEST_CASE(a_node_lock_path_resolves_back_to_its_node) {
    const auto unlocked = solve(parse(turn_spot), 4);
    const auto& target = node_at(unlocked, {"check", "bet_50"});
    auto spot = parse(turn_spot);
    spot.locks = {lock(*cli::node_lock_path(unlocked, target.node_id), {{"fold", 0.0}})};
    const auto locked = solve(spot, 4);
    for (const auto& node : locked.solved_nodes) {
        BOOST_CHECK_EQUAL(node.lock_indices.empty(), node.node_id != target.node_id);
    }
}

BOOST_AUTO_TEST_CASE(exploitability_is_measured_in_the_locked_game) {
    auto spot = parse(river_spot);
    spot.locks = {lock({"bet_75"}, {{"fold", 0.5}})};
    cli::solve_runtime_options runtime{};
    runtime.convergence.measure_exploitability = true;
    const auto early = solve(spot, 20, runtime);
    const auto late = solve(spot, 400, runtime);
    BOOST_REQUIRE(late.solver.convergence.exploitability_available);
    BOOST_CHECK_GE(late.solver.convergence.exploitability, 0.0);
    BOOST_CHECK_LT(late.solver.convergence.exploitability, early.solver.convergence.exploitability);
    // Converges in the locked game: well under 1% of the pot after 400 iterations.
    BOOST_CHECK_LT(late.solver.convergence.exploitability_pot_fraction, 0.01);

    // The same check on a turn spot, whose locked nodes sit under chance nodes.
    auto turn = parse(turn_spot);
    turn.locks = {lock({"check", "check", "*"}, {{"check", 1.0}})};
    const auto turn_early = solve(turn, 10, runtime);
    const auto turn_late = solve(turn, 200, runtime);
    BOOST_CHECK_LT(turn_late.solver.convergence.exploitability, turn_early.solver.convergence.exploitability);
}

BOOST_AUTO_TEST_CASE(locked_solves_are_deterministic_across_worker_counts) {
    auto spot = parse(turn_spot);
    spot.locks = {lock({"check"}, {{"bet_50", 0.5}}), lock({"check", "check", "*"}, {{"all_in", 0.0}})};
    // A fixed timestamp, so two solves that straddle a second still serialise alike.
    const auto single = solve(spot, 12, {.timestamp_utc = "2026-01-01T00:00:00Z", .worker_threads = 1});
    const auto parallel = solve(spot, 12, {.timestamp_utc = "2026-01-01T00:00:00Z", .worker_threads = 6});
    BOOST_CHECK_EQUAL(
        cli::serialize_artifact_json(single, cli::solve_artifact_export_mode::full),
        cli::serialize_artifact_json(parallel, cli::solve_artifact_export_mode::full));
}

BOOST_AUTO_TEST_CASE(the_locks_hash_identifies_the_constraint) {
    auto spot = parse(river_spot);
    const auto base = solve(spot, 2).solver.hashes;

    spot.locks = {lock({"bet_75"}, {{"fold", 0.5}}, "AK")};
    const auto with_lock = solve(spot, 2).solver.hashes;
    BOOST_CHECK_NE(with_lock.locks_hash, 0u);
    BOOST_CHECK_NE(with_lock.solve_hash, base.solve_hash);
    BOOST_CHECK_EQUAL(with_lock.tree_hash, base.tree_hash);

    // The same hands written another way hash the same.
    spot.locks = {lock({"bet_75"}, {{"fold", 0.5}}, "AKs,AKo")};
    BOOST_CHECK_EQUAL(solve(spot, 2).solver.hashes.locks_hash, with_lock.locks_hash);

    spot.locks = {lock({"bet_75"}, {{"fold", 0.6}}, "AK")};
    BOOST_CHECK_NE(solve(spot, 2).solver.hashes.locks_hash, with_lock.locks_hash);
}

BOOST_AUTO_TEST_CASE(artifact_json_round_trips_lock_fields) {
    auto spot = parse(river_spot);
    spot.locks = {lock({"bet_75"}, {{"fold", 0.0}}), lock({"bet_75"}, {{"fold", 1.0}}, "AK")};
    const auto artifact = solve(spot, 10);
    const auto json = cli::serialize_artifact_json(artifact, cli::solve_artifact_export_mode::full);
    auto parsed = cli::parse_artifact_json(json);
    BOOST_REQUIRE_MESSAGE(parsed.has_value(), (parsed ? std::string{} : parsed.error().message));
    BOOST_CHECK_EQUAL(parsed->schema_version, cli::current_artifact_schema_version);
    BOOST_REQUIRE_EQUAL(parsed->solver.locks.size(), 2u);
    BOOST_CHECK_EQUAL(parsed->solver.locks[1].hands, "AK");
    BOOST_CHECK_EQUAL(parsed->solver.hashes.locks_hash, artifact.solver.hashes.locks_hash);
    BOOST_CHECK_EQUAL(parsed->solver.hashes.solve_hash, artifact.solver.hashes.solve_hash);
    struct cli::solve_spot from_parsed;
    struct cli::solve_spot from_solve;
    from_parsed.locks = parsed->solver.locks;
    from_solve.locks = artifact.solver.locks;
    BOOST_CHECK_EQUAL(cli::serialize_spot_json(from_parsed), cli::serialize_spot_json(from_solve));
    const auto& node = node_at(*parsed, {"bet_75"});
    BOOST_CHECK_EQUAL(node.lock_indices.size(), 2u);
    BOOST_CHECK(std::ranges::all_of(node.strategy_rows, &cli::hand_strategy::locked));
}

BOOST_AUTO_TEST_CASE(schema_6_artifacts_still_load_without_locks) {
    const auto artifact = solve(parse(river_spot), 2);
    auto json = cli::serialize_artifact_json(artifact);
    const auto version = json.find("\"schema_version\":7");
    BOOST_REQUIRE(version != std::string::npos);
    json.replace(version, std::string{"\"schema_version\":7"}.size(), "\"schema_version\":6");
    auto parsed = cli::parse_artifact_json(json);
    BOOST_REQUIRE_MESSAGE(parsed.has_value(), (parsed ? std::string{} : parsed.error().message));
    BOOST_CHECK_EQUAL(parsed->schema_version, cli::current_artifact_schema_version);
    BOOST_CHECK(parsed->solver.locks.empty());

    json.replace(json.find("\"schema_version\":6"), std::string{"\"schema_version\":6"}.size(), "\"schema_version\":5");
    BOOST_CHECK(!cli::parse_artifact_json(json).has_value());
}

BOOST_AUTO_TEST_SUITE_END()
