#include <boost/test/unit_test.hpp>

#include "cli/solve_cli.h"

#include <array>
#include <cmath>
#include <ranges>
#include <string>
#include <vector>

namespace {

    constexpr const char* sample_spot = R"({
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c", "4h", "2s"],
  "ranges": ["AA,AKs", "AA,AKs"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "bet_fraction": 0.5,
  "max_history": 8,
  "public_state_id": 7
})";

    constexpr const char* sample_spot_multiway = R"({
  "players": ["BTN", "BB", "CO"],
  "board": ["2s", "3h", "4d", "5c", "9d"],
  "ranges": ["AsKs", "QhQd", "JcTc"],
  "gross_pot": 150.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0, 50.0],
  "stacks": [200.0, 200.0, 200.0],
  "bet_fraction": 0.5,
  "max_history": 8,
  "public_state_id": 11,
  "root_actor": 0,
  "hero_seat": 0,
  "samples_per_combo": 8
})";

    constexpr const char* sample_spot_turn = R"({
  "street": "turn",
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c", "4h"],
  "ranges": ["AhKh", "QdJd"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "bet_fraction": 0.5,
  "max_history": 6,
  "public_state_id": 5,
  "samples_per_combo": 8
})";

    constexpr const char* sample_spot_flop = R"({
  "street": "flop",
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c"],
  "ranges": ["AhKh", "QdJd"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "bet_fraction": 0.5,
  "max_history": 4,
  "public_state_id": 3,
  "samples_per_combo": 4
})";

    // Hero (seat 0, "Th9s") already holds a royal flush on the Ah Kh Qh Jh turn
    // board, so it wins at showdown on every possible river regardless of the
    // dealt card. This makes the runout-averaged counterfactual value exactly
    // hand-computable and independent of the CFR machinery.
    constexpr const char* sample_spot_golden_turn = R"({
  "street": "turn",
  "players": ["BTN", "BB"],
  "board": ["Ah", "Kh", "Qh", "Jh"],
  "ranges": ["Th9s", "2c3d"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [50.0, 50.0],
  "bet_fraction": 0.5,
  "max_history": 4,
  "hero_seat": 0
})";

    constexpr const char* sample_spot_asymmetric_river = R"({
  "players": ["BTN", "BB"],
  "board": ["Ah", "Kd", "Qc", "Jh", "2s"],
  "ranges": ["AA,AKs,AQo", "AA,KK,QQ,AKo"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [200.0, 200.0],
  "bet_fraction": 0.75,
  "max_history": 8,
  "public_state_id": 9,
  "root_actor": 0,
  "hero_seat": 0,
  "samples_per_combo": 8
})";

}

BOOST_AUTO_TEST_CASE(holdem_cli_parses_spot_json) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot);

    BOOST_REQUIRE(spot.has_value());
    BOOST_CHECK_EQUAL(spot->players.size(), 2u);
    BOOST_CHECK_EQUAL(spot->players[0], "BTN");
    BOOST_CHECK_EQUAL(spot->players[1], "BB");
    BOOST_CHECK_EQUAL(spot->board[0], "As");
    BOOST_CHECK_EQUAL(spot->board[4], "2s");
    BOOST_CHECK_EQUAL(spot->bet_fraction, 0.5);
    BOOST_CHECK_EQUAL(spot->public_state_id, 7u);
}

BOOST_AUTO_TEST_CASE(holdem_cli_json_accepts_escaped_strings_and_reordered_fields) {
    constexpr const char* json = R"({
  "samples_per_combo": 8,
  "public_state_id": 3,
  "max_history": 5,
  "bet_fraction": 0.5,
  "stacks": [100, 120],
  "contributions": [40, 60],
  "rake": 0,
  "gross_pot": 100,
  "ranges": ["AhKh", "QdJd"],
  "board": ["As", "Kd", "7c", "4h", "2s"],
  "players": ["BT\"N", "B\\B"],
  "street": "river"
})";

    auto spot = zeta::holdem::cli::parse_spot_json(json);

    BOOST_REQUIRE(spot.has_value());
    BOOST_CHECK_EQUAL(spot->players[0], "BT\"N");
    BOOST_CHECK_EQUAL(spot->players[1], "B\\B");
    BOOST_CHECK_EQUAL(spot->ranges[0], "AhKh");
    BOOST_CHECK_EQUAL(spot->stacks[1], 120.0);
}

BOOST_AUTO_TEST_CASE(holdem_cli_parses_betting_policy_json) {
    constexpr const char* json = R"({
  "players": ["BTN", "BB"],
  "street": "river",
  "board": ["As", "Kd", "7c", "4h", "2s"],
  "ranges": ["AA", "AA"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "bet_fraction": 0.5,
  "betting_policy": {
    "fixed_pot_fractions": [0.5, 1.0],
    "max_raises": 2,
    "min_bet_increment": 1.0,
    "all_in_threshold": 0.95
  }
})";

    auto spot = zeta::holdem::cli::parse_spot_json(json);
    BOOST_REQUIRE(spot.has_value());
    BOOST_CHECK_EQUAL(spot->betting_policy.fixed_pot_fractions.size(), 2u);
    BOOST_CHECK_EQUAL(spot->betting_policy.fixed_pot_fractions[0], 0.5);
    BOOST_CHECK_EQUAL(spot->betting_policy.max_raises, 2u);
    BOOST_CHECK_EQUAL(spot->bet_fraction, 0.5);
}

BOOST_AUTO_TEST_CASE(holdem_cli_defaults_legacy_betting_policy_when_missing) {
    constexpr const char* json = R"({
  "players": ["BTN", "BB"],
  "street": "river",
  "board": ["As", "Kd", "7c", "4h", "2s"],
  "ranges": ["AA", "AA"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "bet_fraction": 0.75
})";

    auto spot = zeta::holdem::cli::parse_spot_json(json);
    BOOST_REQUIRE(spot.has_value());
    BOOST_CHECK_EQUAL(spot->betting_policy.fixed_pot_fractions.size(), 1u);
    BOOST_CHECK_EQUAL(spot->betting_policy.fixed_pot_fractions.front(), 0.75);
    BOOST_CHECK_EQUAL(spot->betting_policy.max_raises, 1u);
    BOOST_CHECK_EQUAL(spot->bet_fraction, 0.75);
}

BOOST_AUTO_TEST_CASE(holdem_cli_json_rejects_removed_heads_up_legacy_aliases) {
    constexpr const char* json = R"({
  "board": ["As", "Kd", "7c", "4h", "2s"],
  "oop_range": "AhKh",
  "ip_range": "QdJd",
  "oop_contribution": 35.0,
  "ip_contribution": 65.0,
  "oop_stack": 90.0,
  "ip_stack": 110.0,
  "gross_pot": 100.0
})";

    BOOST_CHECK(!zeta::holdem::cli::parse_spot_json(json).has_value());
}

BOOST_AUTO_TEST_CASE(holdem_cli_json_rejects_wrong_types) {
    constexpr const char* wrong_string = R"({
  "players": ["BTN", 7],
  "board": ["As", "Kd", "7c", "4h", "2s"],
  "ranges": ["AhKh", "QdJd"]
})";
    constexpr const char* wrong_array_value = R"({
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c", "4h", "2s"],
  "ranges": ["AhKh", "QdJd"],
  "stacks": [100.0, "deep"]
})";
    constexpr const char* out_of_range_integer = R"({
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c", "4h", "2s"],
  "ranges": ["AhKh", "QdJd"],
  "hero_seat": 300
})";
    constexpr const char* unknown_label = R"({
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c", "4h", "2s"],
  "ranges": ["AhKh", "QdJd"],
  "root_actor": "CO"
})";

    BOOST_CHECK(!zeta::holdem::cli::parse_spot_json(wrong_string).has_value());
    BOOST_CHECK(!zeta::holdem::cli::parse_spot_json(wrong_array_value).has_value());
    BOOST_CHECK(!zeta::holdem::cli::parse_spot_json(out_of_range_integer).has_value());
    BOOST_CHECK(!zeta::holdem::cli::parse_spot_json(unknown_label).has_value());
}

BOOST_AUTO_TEST_CASE(holdem_cli_json_accepts_player_labels_for_seat_fields) {
    constexpr const char* with_labels = R"({
  "players": ["BTN", "BB", "CO"],
  "board": ["2s", "3h", "4d", "5c", "9d"],
  "ranges": ["AsKs", "QhQd", "JcTc"],
  "gross_pot": 150.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0, 50.0],
  "stacks": [200.0, 200.0, 200.0],
  "bet_fraction": 0.5,
  "root_actor": "BB",
  "hero_seat": "CO"
})";

    auto spot = zeta::holdem::cli::parse_spot_json(with_labels);

    BOOST_REQUIRE(spot.has_value());
    BOOST_CHECK_EQUAL(spot->root_actor, 1u);
    BOOST_CHECK_EQUAL(spot->hero_seat, 2u);
}

BOOST_AUTO_TEST_CASE(holdem_cli_spot_json_roundtrips_serialized_spot) {
    struct zeta::holdem::cli::solve_spot spot;
    spot.players = {"BT\"N", "B\\B"};
    spot.board = {"As", "Kd", "7c", "4h", "2s"};
    spot.ranges = {"AhKh", "QdJd"};
    spot.gross_pot = 123.5;
    spot.rake = 1.25;
    spot.contributions = {45.0, 78.5};
    spot.stacks = {200.0, 180.0};
    spot.bet_fraction = 0.625;
    spot.max_history = 9;
    spot.public_state_id = 44;
    spot.root_actor = 1;
    spot.hero_seat = 1;
    spot.samples_per_combo = 12;

    const auto json = zeta::holdem::cli::serialize_spot_json(spot);
    auto parsed = zeta::holdem::cli::parse_spot_json(json);

    BOOST_REQUIRE(parsed.has_value());
    BOOST_CHECK_EQUAL(parsed->players[0], spot.players[0]);
    BOOST_CHECK_EQUAL(parsed->players[1], spot.players[1]);
    BOOST_CHECK_EQUAL(parsed->gross_pot, spot.gross_pot);
    BOOST_CHECK_EQUAL(parsed->rake, spot.rake);
    BOOST_CHECK_EQUAL(parsed->contributions[1], spot.contributions[1]);
    BOOST_CHECK_EQUAL(parsed->root_actor, spot.root_actor);
    BOOST_CHECK_EQUAL(parsed->hero_seat, spot.hero_seat);
}

BOOST_AUTO_TEST_CASE(holdem_cli_solve_produces_valid_artifact) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot);
    BOOST_REQUIRE(spot.has_value());

    auto output = zeta::holdem::cli::solve_spot(
        *spot,
        2,
        zeta::holdem::cli::solve_runtime_options{
            .timestamp_utc = "2026-08-01T19:47:11Z",
            .git_revision = "abc1234"
        });
    BOOST_REQUIRE(output.has_value());
    BOOST_CHECK_GT(output->artifact.strategy.size(), 0u);
    BOOST_CHECK_GT(output->artifact.root_strategy.size(), 0u);
    BOOST_CHECK(std::ranges::all_of(output->artifact.strategy, [](const auto& row) {
        return !row.strategy.empty();
    }));
    BOOST_CHECK_EQUAL(output->artifact.schema_version, 3u);
    BOOST_CHECK_EQUAL(output->artifact.game, "holdem");
    BOOST_CHECK_EQUAL(output->artifact.street, "river");
    BOOST_CHECK_EQUAL(output->artifact.players.size(), 2u);
    BOOST_CHECK_EQUAL(output->artifact.hero_seat, 0u);
    BOOST_CHECK_EQUAL(output->artifact.solver.algorithm, "cfr+");
    BOOST_CHECK_EQUAL(output->artifact.solver.iterations, 2u);
    BOOST_CHECK_EQUAL(output->artifact.solver.timestamp, "2026-08-01T19:47:11Z");
    BOOST_CHECK_EQUAL(output->artifact.solver.git_revision, "abc1234");

    auto validation = zeta::holdem::cli::validate_artifact(output->artifact);
    BOOST_CHECK(validation.has_value());
}

BOOST_AUTO_TEST_CASE(holdem_cli_solve_produces_combo_specific_root_strategies_for_asymmetric_river) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot_asymmetric_river);
    BOOST_REQUIRE(spot.has_value());

    auto output = zeta::holdem::cli::solve_spot(*spot, 200);
    BOOST_REQUIRE(output.has_value());
    BOOST_REQUIRE(!output->artifact.strategy.empty());
    BOOST_CHECK_GT(output->artifact.root_strategy.size(), 0u);
    BOOST_CHECK(std::ranges::all_of(output->artifact.strategy, [](const auto& row) {
        return !row.strategy.empty();
    }));

    bool found_difference = false;
    for (std::size_t lhs = 0; lhs < output->artifact.strategy.size() && !found_difference; ++lhs) {
        for (std::size_t rhs = lhs + 1; rhs < output->artifact.strategy.size() && !found_difference; ++rhs) {
            const auto& left = output->artifact.strategy[lhs].strategy;
            const auto& right = output->artifact.strategy[rhs].strategy;
            if (left.size() != right.size()) {
                found_difference = true;
                break;
            }
            for (std::size_t action = 0; action < left.size(); ++action) {
                if (std::fabs(left[action].frequency - right[action].frequency) > 1.0e-5) {
                    found_difference = true;
                    break;
                }
            }
        }
    }
    BOOST_CHECK(found_difference);
}

BOOST_AUTO_TEST_CASE(holdem_cli_validate_rejects_duplicate_board_cards) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot);
    BOOST_REQUIRE(spot.has_value());
    auto output = zeta::holdem::cli::solve_spot(*spot, 1);
    BOOST_REQUIRE(output.has_value());

    output->artifact.board[1] = output->artifact.board[0];
    auto validation = zeta::holdem::cli::validate_artifact(output->artifact);

    BOOST_REQUIRE(!validation);
    BOOST_CHECK(validation.error().kind == zeta::holdem::cli::cli_error_kind::invalid_artifact);
}

BOOST_AUTO_TEST_CASE(holdem_cli_roundtrips_artifact_json_and_dump) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot);
    BOOST_REQUIRE(spot.has_value());
    auto output = zeta::holdem::cli::solve_spot(*spot, 1);
    BOOST_REQUIRE(output.has_value());

    const auto json = zeta::holdem::cli::serialize_artifact_json(output->artifact);
    auto parsed = zeta::holdem::cli::parse_artifact_json(json);
    BOOST_REQUIRE(parsed.has_value());
    BOOST_REQUIRE(zeta::holdem::cli::validate_artifact(*parsed).has_value());

    const auto dump = zeta::holdem::cli::format_dump(*parsed);
    BOOST_CHECK(dump.find("Hand") != std::string::npos);
    BOOST_CHECK(dump.find("EV") != std::string::npos);
    BOOST_CHECK(dump.find('%') != std::string::npos);
}

BOOST_AUTO_TEST_CASE(holdem_cli_artifact_json_accepts_nested_objects_and_escaped_actions) {
    zeta::holdem::cli::solve_artifact artifact;
    artifact.players = {"BT\"N", "B\\B"};
    artifact.board = {"As", "Kd", "7c", "4h", "2s"};
    artifact.hero_seat = 0;
    artifact.solver.iterations = 12;
    artifact.solver.timestamp = "2026-08-01T19:47:11Z";
    artifact.solver.git_revision = "abc1234";
    artifact.root_strategy = {
        zeta::holdem::cli::action_strategy{.action = "bet_50", .frequency = 0.25},
        zeta::holdem::cli::action_strategy{.action = "call\\check", .frequency = 0.75}
    };
    artifact.strategy = {
        zeta::holdem::cli::hand_strategy{
            .hand = "QhJd",
            .strategy = {},
            .ev = 3.5
        }
    };

    const auto json = zeta::holdem::cli::serialize_artifact_json(artifact);
    auto parsed = zeta::holdem::cli::parse_artifact_json(json);

    BOOST_REQUIRE(parsed.has_value());
    BOOST_CHECK_EQUAL(parsed->players[0], "BT\"N");
    BOOST_REQUIRE_EQUAL(parsed->strategy.size(), 1u);
    BOOST_REQUIRE_EQUAL(parsed->root_strategy.size(), 2u);
    BOOST_CHECK_EQUAL(parsed->root_strategy[1].action, "call\\check");
    BOOST_CHECK(parsed->strategy[0].strategy.empty());
    BOOST_REQUIRE(zeta::holdem::cli::validate_artifact(*parsed).has_value());
}

BOOST_AUTO_TEST_CASE(holdem_cli_solve_multiway_produces_valid_artifact) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot_multiway);
    BOOST_REQUIRE(spot.has_value());

    auto output = zeta::holdem::cli::solve_spot(*spot, 1);
    BOOST_REQUIRE(output.has_value());
    BOOST_CHECK_EQUAL(output->artifact.players.size(), 3u);
    BOOST_CHECK_EQUAL(output->artifact.players[2], "CO");
    BOOST_CHECK_EQUAL(output->artifact.hero_seat, 0u);
    BOOST_CHECK_GT(output->artifact.strategy.size(), 0u);
    BOOST_REQUIRE(zeta::holdem::cli::validate_artifact(output->artifact).has_value());
}

BOOST_AUTO_TEST_CASE(holdem_cli_multi_street_solve_supports_turn_street) {
    auto turn_spot = zeta::holdem::cli::parse_spot_json(sample_spot_turn);
    BOOST_REQUIRE(turn_spot.has_value());
    auto turn_output = zeta::holdem::cli::solve_spot(*turn_spot, 4);
    BOOST_REQUIRE(turn_output.has_value());
    BOOST_CHECK_EQUAL(turn_output->artifact.street, "turn");
    BOOST_CHECK_EQUAL(turn_output->artifact.board.size(), 4u);
    BOOST_CHECK_EQUAL(turn_output->artifact.solver.algorithm, "cfr+");
    BOOST_CHECK(!turn_output->artifact.root_strategy.empty());
    BOOST_CHECK(std::ranges::all_of(turn_output->artifact.strategy, [](const auto& row) {
        return !row.strategy.empty();
    }));
    BOOST_REQUIRE(zeta::holdem::cli::validate_artifact(turn_output->artifact).has_value());
}

BOOST_AUTO_TEST_CASE(holdem_cli_multi_street_flop_solve_backs_up_two_chance_layers) {
    namespace cli = zeta::holdem::cli;
    namespace detail = cli::detail;
    namespace cfr = zeta::holdem::cfr;

    // A full-deck flop solve enumerates every remaining river runout and is
    // intractable at this stage (Stage 3 adds the memory/isomorphism controls
    // that make it feasible). To exercise the flop-specific two-chance-layer
    // value backup here, restrict the live deck to a single turn/river pair so
    // the interleaved turn+river tree stays tiny while still driving both chance
    // layers of the unified CFR+ solve.
    auto spot = cli::parse_spot_json(sample_spot_flop);
    BOOST_REQUIRE(spot.has_value());

    const auto street = detail::parse_holdem_street(spot->street);
    BOOST_REQUIRE(street.has_value());
    BOOST_REQUIRE(*street == cfr::solver::holdem_street::flop);

    const auto flop_board = detail::board_from_cards(spot->board, *street);
    BOOST_REQUIRE(flop_board.has_value());

    // Leave exactly two community cards (2h, 3h) live for the turn and river deals.
    const auto full_board = detail::board_from_cards(
        std::vector<std::string>{"As", "Kd", "7c", "2h", "3h"},
        cfr::solver::holdem_street::river);
    BOOST_REQUIRE(full_board.has_value());
    constexpr zeta::card_mask full_deck = (zeta::card_mask{1} << 52) - 1;
    const zeta::card_mask live_runout = full_board->mask & ~flop_board->mask;

    std::array<zeta::holdem::hand_range, 2> ranges{};
    std::array<zeta::holdem::reach_vector, 2> reach_vectors{};
    for (std::size_t seat = 0; seat < 2; ++seat) {
        BOOST_REQUIRE(detail::parse_range_checked(
            spot->ranges[seat], ranges[seat], "seat").has_value());
        ranges[seat].remove_dead(flop_board->mask);
        reach_vectors[seat] = zeta::holdem::make_reach_vector(ranges[seat]);
    }

    cfr::holdem_betting_graph_config<2> config{};
    config.street = *street;
    config.initial_stacks = {spot->stacks[0], spot->stacks[1]};
    config.initial_committed = {spot->contributions[0], spot->contributions[1]};
    config.root_actor = spot->root_actor;
    config.abstraction = cli::resolve_spot_betting_policy(*spot);
    config.max_history = spot->max_history;
    config.public_state_id = spot->public_state_id;

    cfr::holdem_public_game_config<2> public_config{};
    public_config.street = *street;
    public_config.board_cards = flop_board->mask;
    public_config.dead_cards = full_deck & ~flop_board->mask & ~live_runout;
    public_config.initial_stacks = config.initial_stacks;
    public_config.initial_committed = config.initial_committed;
    public_config.root_actor = config.root_actor;
    public_config.abstraction = config.abstraction;
    public_config.max_history = config.max_history;
    public_config.public_state_id = config.public_state_id;

    auto lowered = cfr::lower_multi_street_public_game(public_config);
    BOOST_REQUIRE(lowered.has_value());
    auto layout = cfr::make_action_table_layout(lowered->graph);
    BOOST_REQUIRE(layout.has_value());

    // The restricted deck deals exactly one (turn, river) ordering, so the tree
    // interleaves a turn chance layer and a river chance layer above the river
    // showdown terminals: both flop-root and turn public states must be present.
    bool has_turn_state = false;
    bool has_river_state = false;
    for (const auto& state : lowered->public_states.states) {
        if (state.street == cfr::solver::holdem_street::turn) {
            has_turn_state = true;
        }
        if (state.street == cfr::solver::holdem_street::river) {
            has_river_state = true;
        }
    }
    BOOST_CHECK(has_turn_state);
    BOOST_CHECK(has_river_state);

    cli::solve_output output{};
    auto solved = detail::solve_multi_street_public_game<2>(
        *spot, 8, {}, *lowered, *layout, config, reach_vectors, output);
    BOOST_REQUIRE(solved.has_value());

    BOOST_CHECK_EQUAL(output.artifact.solver.algorithm, "cfr+");
    BOOST_CHECK(!output.artifact.root_strategy.empty());
    BOOST_REQUIRE(!output.artifact.strategy.empty());
    for (const auto& row : output.artifact.strategy) {
        BOOST_CHECK(std::isfinite(row.ev));
        BOOST_CHECK(!row.strategy.empty());
    }
}

BOOST_AUTO_TEST_CASE(holdem_cli_multi_street_solve_matches_hand_computed_nuts_ev) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot_golden_turn);
    BOOST_REQUIRE(spot.has_value());

    auto output = zeta::holdem::cli::solve_spot(*spot, 600);
    BOOST_REQUIRE(output.has_value());
    BOOST_REQUIRE(zeta::holdem::cli::validate_artifact(output->artifact).has_value());
    BOOST_CHECK_EQUAL(output->artifact.solver.algorithm, "cfr+");
    BOOST_REQUIRE_EQUAL(output->artifact.strategy.size(), 1u);

    // Independent equilibrium computation. The hero ("Th9s") already holds a royal
    // flush on the Ah Kh Qh Jh turn and is unbeatable on every river, while the
    // villain ("2c3d") is drawing dead. The villain's loss-minimizing equilibrium
    // response is to commit no further chips (fold to any bet / never bet into the
    // nuts), so the hero simply collects the existing 100 pot on every runout:
    //   counterfactual value = (gross_pot - rake) - hero_contribution = 100 - 50 = 50.
    // The villain's single combo is never blocked by the hero, so the opponent reach
    // mass is exactly 1.0 and the counterfactual value is undivided.
    constexpr double expected_ev = 50.0;
    BOOST_CHECK_CLOSE(output->artifact.strategy.front().ev, expected_ev, 1.0);
}

BOOST_AUTO_TEST_CASE(holdem_cli_multi_street_solve_converges_with_iterations) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot_golden_turn);
    BOOST_REQUIRE(spot.has_value());

    auto coarse = zeta::holdem::cli::solve_spot(*spot, 1);
    auto refined = zeta::holdem::cli::solve_spot(*spot, 600);
    BOOST_REQUIRE(coarse.has_value());
    BOOST_REQUIRE(refined.has_value());
    BOOST_REQUIRE_EQUAL(coarse->artifact.solver.iterations, 1u);
    BOOST_REQUIRE_EQUAL(refined->artifact.solver.iterations, 600u);
    BOOST_REQUIRE_EQUAL(coarse->artifact.strategy.size(), 1u);
    BOOST_REQUIRE_EQUAL(refined->artifact.strategy.size(), 1u);

    // The averaged strategy after a single iteration has not yet reached the nuts
    // equilibrium, so its counterfactual value is measurably further from the
    // hand-computed target than the well-converged solve.
    constexpr double equilibrium_ev = 50.0;
    const double coarse_error = std::fabs(coarse->artifact.strategy.front().ev - equilibrium_ev);
    const double refined_error = std::fabs(refined->artifact.strategy.front().ev - equilibrium_ev);
    BOOST_CHECK_LT(refined_error, coarse_error);
}

BOOST_AUTO_TEST_CASE(holdem_cli_multi_street_solve_is_deterministic_across_worker_counts) {
    auto turn_spot = zeta::holdem::cli::parse_spot_json(sample_spot_turn);
    BOOST_REQUIRE(turn_spot.has_value());

    auto single = zeta::holdem::cli::solve_spot(*turn_spot, 32, {.worker_threads = 1});
    auto parallel = zeta::holdem::cli::solve_spot(*turn_spot, 32, {.worker_threads = 8});
    BOOST_REQUIRE(single.has_value());
    BOOST_REQUIRE(parallel.has_value());
    BOOST_REQUIRE(!single->artifact.strategy.empty());
    BOOST_REQUIRE_EQUAL(single->artifact.strategy.size(), parallel->artifact.strategy.size());

    // The unified CFR+ solve is worker-count independent: the same converged
    // strategy and per-combo EV are produced regardless of the configured threads.
    for (std::size_t i = 0; i < single->artifact.strategy.size(); ++i) {
        BOOST_CHECK_EQUAL(single->artifact.strategy[i].hand, parallel->artifact.strategy[i].hand);
        BOOST_CHECK_EQUAL(single->artifact.strategy[i].ev, parallel->artifact.strategy[i].ev);
        BOOST_REQUIRE_EQUAL(single->artifact.strategy[i].strategy.size(),
            parallel->artifact.strategy[i].strategy.size());
        for (std::size_t action = 0; action < single->artifact.strategy[i].strategy.size(); ++action) {
            BOOST_CHECK_EQUAL(single->artifact.strategy[i].strategy[action].frequency,
                parallel->artifact.strategy[i].strategy[action].frequency);
        }
    }
}

BOOST_AUTO_TEST_CASE(holdem_cli_nonriver_artifact_persists_multi_street_graph_payload) {
    auto turn_spot = zeta::holdem::cli::parse_spot_json(sample_spot_turn);
    BOOST_REQUIRE(turn_spot.has_value());

    auto turn_output = zeta::holdem::cli::solve_spot(*turn_spot, 16, {.worker_threads = 2});
    BOOST_REQUIRE(turn_output.has_value());
    BOOST_CHECK_EQUAL(turn_output->artifact.schema_version, 3u);
    BOOST_CHECK_EQUAL(turn_output->artifact.solver.algorithm, "cfr+");
    BOOST_CHECK(!turn_output->artifact.public_states.empty());
    BOOST_CHECK(!turn_output->artifact.chance_events.empty());
    BOOST_CHECK(!turn_output->artifact.runouts.empty());
    BOOST_CHECK(!turn_output->artifact.solved_nodes.empty());

    // A turn solve interleaves the turn betting round with the river deal: the
    // public-state registry must carry the turn root plus the dealt river boards,
    // and each river public state must own a complete-runout entry.
    bool has_turn_state = false;
    bool has_river_state = false;
    for (const auto& state : turn_output->artifact.public_states) {
        if (state.street == "turn") {
            has_turn_state = true;
        }
        if (state.street == "river") {
            has_river_state = true;
        }
    }
    BOOST_CHECK(has_turn_state);
    BOOST_CHECK(has_river_state);

    const auto json = zeta::holdem::cli::serialize_artifact_json(turn_output->artifact);
    auto parsed = zeta::holdem::cli::parse_artifact_json(json);
    BOOST_REQUIRE(parsed.has_value());
    BOOST_CHECK_EQUAL(parsed->public_states.size(), turn_output->artifact.public_states.size());
    BOOST_CHECK_EQUAL(parsed->chance_events.size(), turn_output->artifact.chance_events.size());
    BOOST_CHECK_EQUAL(parsed->runouts.size(), turn_output->artifact.runouts.size());
    BOOST_CHECK_EQUAL(parsed->solved_nodes.size(), turn_output->artifact.solved_nodes.size());
}

BOOST_AUTO_TEST_CASE(holdem_cli_rejects_old_artifact_schema_versions) {
    constexpr const char* json = R"({
  "schema_version": 2,
  "game": "holdem",
  "street": "river",
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c", "4h", "2s"],
  "hero_seat": 0,
  "solver": {"algorithm": "cfr+", "iterations": 1, "timestamp": "2026-08-01T19:47:11Z", "git_revision": "abc1234"},
  "root_strategy": [{"action": "check", "frequency": 1.0}],
  "strategy": [{"hand": "AhAd", "strategy": [{"action": "check", "frequency": 1.0}], "ev": 1.0}]
})";

    auto parsed = zeta::holdem::cli::parse_artifact_json(json);
    BOOST_REQUIRE(!parsed);
    BOOST_CHECK(parsed.error().kind == zeta::holdem::cli::cli_error_kind::invalid_artifact);
}
