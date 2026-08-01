#include <boost/test/unit_test.hpp>

#include "cli/solve_cli.h"

namespace {

    constexpr const char* sample_spot = R"({
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c", "4h", "2s"],
  "oop_range": "AA,AKs",
  "ip_range": "AA,AKs",
  "gross_pot": 100.0,
  "rake": 0.0,
  "oop_contribution": 50.0,
  "ip_contribution": 50.0,
  "oop_stack": 100.0,
  "ip_stack": 100.0,
  "bet_fraction": 0.5,
  "max_history": 8,
  "public_state_id": 7
})";

}

BOOST_AUTO_TEST_CASE(holdem_cli_parses_spot_json) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot);

    BOOST_REQUIRE(spot.has_value());
    BOOST_CHECK_EQUAL(spot->players[0], "BTN");
    BOOST_CHECK_EQUAL(spot->players[1], "BB");
    BOOST_CHECK_EQUAL(spot->board[0], "As");
    BOOST_CHECK_EQUAL(spot->board[4], "2s");
    BOOST_CHECK_EQUAL(spot->bet_fraction, 0.5);
    BOOST_CHECK_EQUAL(spot->public_state_id, 7u);
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
