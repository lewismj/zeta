#include <boost/test/unit_test.hpp>

#include "cli/solve_cli.h"
#include "solver_state.h"
#include "spot_document.h"

BOOST_AUTO_TEST_CASE(holdem_ui_spot_json_roundtrip_preserves_phase1_fields) {
    zeta::holdem::ui::spot spot;
    spot.street = "turn";
    spot.players = {"BTN", "BB", "CO"};
    spot.board = {"As", "Kd", "7c", "4h"};
    spot.ranges = {"AhKh", "QdJd", "TcTs"};
    spot.gross_pot = 150.0;
    spot.rake = 2.5;
    spot.contributions = {50.0, 50.0, 50.0};
    spot.stacks = {100.0, 120.0, 140.0};
    spot.bet_fraction = 0.5;
    spot.max_history = 6;
    spot.public_state_id = 12;
    spot.root_actor = 2;
    spot.hero_seat = 1;
    spot.samples_per_combo = 16;

    const auto json = zeta::holdem::cli::serialize_spot_json(spot);
    const auto parsed = zeta::holdem::cli::parse_spot_json(json);

    BOOST_REQUIRE(parsed.has_value());
    BOOST_CHECK_EQUAL(parsed->street, "turn");
    BOOST_CHECK_EQUAL(parsed->players.size(), 3u);
    BOOST_CHECK_EQUAL(parsed->players[2], "CO");
    BOOST_CHECK_EQUAL(parsed->board.size(), 4u);
    BOOST_CHECK_EQUAL(parsed->ranges[1], "QdJd");
    BOOST_CHECK_EQUAL(parsed->gross_pot, 150.0);
    BOOST_CHECK_EQUAL(parsed->rake, 2.5);
    BOOST_CHECK_EQUAL(parsed->stacks[2], 140.0);
    BOOST_CHECK_EQUAL(parsed->root_actor, 2u);
    BOOST_CHECK_EQUAL(parsed->hero_seat, 1u);
    BOOST_CHECK_EQUAL(parsed->samples_per_combo, 16u);
}

BOOST_AUTO_TEST_CASE(holdem_ui_spot_document_dirty_transitions_and_document_roundtrip) {
    auto document = zeta::holdem::ui::spot_document::create_new();
    BOOST_CHECK(!document.is_dirty());

    auto next_spot = document.current_spot();
    next_spot.street = "flop";
    next_spot.board = {"As", "Kd", "7c"};
    next_spot.ranges = {"AhKh", "QdJd"};
    document.replace_spot(next_spot);
    BOOST_CHECK(document.is_dirty());

    document.add_history(zeta::holdem::ui::solve_history_entry{
        .timestamp_utc = "2026-08-03T20:00:00Z",
        .iterations = 100,
        .outcome = "completed"
    });
    const auto json = document.serialize_json();
    const auto parsed = zeta::holdem::ui::spot_document::parse_json(json);

    BOOST_REQUIRE(parsed.has_value());
    BOOST_CHECK(!parsed->is_dirty());
    BOOST_CHECK_EQUAL(parsed->current_spot().street, "flop");
    BOOST_CHECK_EQUAL(parsed->current_spot().board.size(), 3u);
    BOOST_REQUIRE_EQUAL(parsed->recent_history().size(), 1u);
    BOOST_CHECK_EQUAL(parsed->recent_history()[0].iterations, 100u);
    BOOST_CHECK_EQUAL(parsed->recent_history()[0].outcome, "completed");
}

BOOST_AUTO_TEST_CASE(holdem_ui_solver_state_machine_drives_controls) {
    zeta::holdem::ui::solver_state_machine machine;
    BOOST_CHECK(machine.controls().validate_enabled);
    BOOST_CHECK(machine.controls().solve_enabled);
    BOOST_CHECK(!machine.controls().cancel_enabled);

    auto transition = machine.transition_to(zeta::holdem::ui::solver_state::starting);
    BOOST_REQUIRE(transition.has_value());
    BOOST_CHECK(!machine.controls().validate_enabled);
    BOOST_CHECK(!machine.controls().solve_enabled);
    BOOST_CHECK(machine.controls().cancel_enabled);

    transition = machine.transition_to(zeta::holdem::ui::solver_state::running);
    BOOST_REQUIRE(transition.has_value());
    BOOST_CHECK(machine.controls().cancel_enabled);

    transition = machine.transition_to(zeta::holdem::ui::solver_state::completed);
    BOOST_REQUIRE(transition.has_value());
    BOOST_CHECK(machine.controls().validate_enabled);
    BOOST_CHECK(machine.controls().solve_enabled);
    BOOST_CHECK(!machine.controls().cancel_enabled);

    transition = machine.transition_to(zeta::holdem::ui::solver_state::cancelling);
    BOOST_CHECK(!transition.has_value());
}
