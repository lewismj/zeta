#include <boost/test/unit_test.hpp>

#include "cli/solve_cli.h"

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

BOOST_AUTO_TEST_CASE(holdem_cli_json_accepts_legacy_heads_up_fields) {
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

    auto spot = zeta::holdem::cli::parse_spot_json(json);

    BOOST_REQUIRE(spot.has_value());
    BOOST_REQUIRE_EQUAL(spot->players.size(), 2u);
    BOOST_CHECK_EQUAL(spot->ranges[0], "AhKh");
    BOOST_CHECK_EQUAL(spot->ranges[1], "QdJd");
    BOOST_CHECK_EQUAL(spot->contributions[0], 35.0);
    BOOST_CHECK_EQUAL(spot->contributions[1], 65.0);
    BOOST_CHECK_EQUAL(spot->stacks[0], 90.0);
    BOOST_CHECK_EQUAL(spot->stacks[1], 110.0);
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

    BOOST_CHECK(!zeta::holdem::cli::parse_spot_json(wrong_string).has_value());
    BOOST_CHECK(!zeta::holdem::cli::parse_spot_json(wrong_array_value).has_value());
    BOOST_CHECK(!zeta::holdem::cli::parse_spot_json(out_of_range_integer).has_value());
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
    BOOST_CHECK_EQUAL(output->artifact.schema_version, 1u);
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
    artifact.strategy = {
        zeta::holdem::cli::hand_strategy{
            .hand = "QhJd",
            .strategy = {
                zeta::holdem::cli::action_strategy{.action = "bet_50", .frequency = 0.25},
                zeta::holdem::cli::action_strategy{.action = "call\\check", .frequency = 0.75}
            },
            .ev = 3.5
        }
    };

    const auto json = zeta::holdem::cli::serialize_artifact_json(artifact);
    auto parsed = zeta::holdem::cli::parse_artifact_json(json);

    BOOST_REQUIRE(parsed.has_value());
    BOOST_CHECK_EQUAL(parsed->players[0], "BT\"N");
    BOOST_REQUIRE_EQUAL(parsed->strategy.size(), 1u);
    BOOST_CHECK_EQUAL(parsed->strategy[0].strategy[1].action, "call\\check");
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

BOOST_AUTO_TEST_CASE(holdem_cli_solve_supports_turn_and_flop_streets) {
    auto turn_spot = zeta::holdem::cli::parse_spot_json(sample_spot_turn);
    BOOST_REQUIRE(turn_spot.has_value());
    auto turn_output = zeta::holdem::cli::solve_spot(*turn_spot, 1);
    BOOST_REQUIRE(turn_output.has_value());
    BOOST_CHECK_EQUAL(turn_output->artifact.street, "turn");
    BOOST_CHECK_EQUAL(turn_output->artifact.board.size(), 4u);
    BOOST_REQUIRE(zeta::holdem::cli::validate_artifact(turn_output->artifact).has_value());

    auto flop_spot = zeta::holdem::cli::parse_spot_json(sample_spot_flop);
    BOOST_REQUIRE(flop_spot.has_value());
    auto flop_output = zeta::holdem::cli::solve_spot(*flop_spot, 1);
    BOOST_REQUIRE(flop_output.has_value());
    BOOST_CHECK_EQUAL(flop_output->artifact.street, "flop");
    BOOST_CHECK_EQUAL(flop_output->artifact.board.size(), 3u);
    BOOST_REQUIRE(zeta::holdem::cli::validate_artifact(flop_output->artifact).has_value());
}
