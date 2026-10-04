#include <boost/test/unit_test.hpp>

#include "cli/solve_cli.h"

#include <array>
#include <cmath>
#include <ranges>
#include <string>
#include <vector>

namespace {

    zeta::holdem::combination_index combo_index_for(const std::string& hand)
    {
        const auto parsed = zeta::holdem::parse_range(hand);
        BOOST_REQUIRE(parsed.ok());
        for (zeta::holdem::combination_index combo = 0; combo < zeta::holdem::combination_count; ++combo) {
            if (parsed.range.weights[combo] != 0.0f) {
                return combo;
            }
        }
        BOOST_FAIL("Expected exact hand to contain one combo.");
        return 0;
    }

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

    // A four-to-a-royal spade turn board leaves the three non-spade suits (hearts,
    // diamonds, clubs) fully interchangeable, and the identical rank-only "AA" range
    // for both seats is invariant under every permutation of those free suits. This is
    // the canonical suit-symmetric spot: collapsing suit-isomorphic river runouts is
    // lossless, so an isomorphism-reduced solve must reproduce the exact solve.
    constexpr const char* sample_spot_suit_symmetric_turn = R"({
  "street": "turn",
  "players": ["BTN", "BB"],
  "board": ["As", "Ks", "Qs", "Js"],
  "ranges": ["AA", "AA"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "bet_fraction": 0.5,
  "max_history": 4,
  "public_state_id": 5
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
    BOOST_CHECK_EQUAL(output->artifact.schema_version, 4u);
    BOOST_CHECK_EQUAL(output->artifact.extraction_version, 1u);
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
            .combination_index = combo_index_for("QhJd"),
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

BOOST_AUTO_TEST_CASE(holdem_cli_multi_street_solve_rejects_over_budget_footprint) {
    auto turn_spot = zeta::holdem::cli::parse_spot_json(sample_spot_turn);
    BOOST_REQUIRE(turn_spot.has_value());

    // A one-byte budget is smaller than any real footprint, so the pre-build check
    // must refuse the solve before allocating the CFR tables and name the dominant
    // cost dimension in an actionable message.
    auto rejected = zeta::holdem::cli::solve_spot(
        *turn_spot, 64, {.memory_budget_bytes = 1});
    BOOST_REQUIRE(!rejected.has_value());
    BOOST_CHECK(rejected.error().kind == zeta::holdem::cli::cli_error_kind::solver);
    BOOST_CHECK(rejected.error().message.find("exceeds the budget") != std::string::npos);

    // A generous budget solves the same spot normally, confirming the rejection is the
    // budget check and not an unrelated failure.
    auto solved = zeta::holdem::cli::solve_spot(
        *turn_spot, 8, {.memory_budget_bytes = uint64_t{8} << 30});
    BOOST_REQUIRE(solved.has_value());
    BOOST_CHECK(!solved->artifact.strategy.empty());
}

BOOST_AUTO_TEST_CASE(holdem_cli_card_isomorphism_matches_exact_for_suit_symmetric_ranges) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot_suit_symmetric_turn);
    BOOST_REQUIRE(spot.has_value());

    auto exact = zeta::holdem::cli::solve_spot(*spot, 200, {});
    auto iso = zeta::holdem::cli::solve_spot(
        *spot, 200, {.enable_card_isomorphism = true});
    BOOST_REQUIRE(exact.has_value());
    BOOST_REQUIRE(iso.has_value());

    // Suit isomorphism is lossless for suit-symmetric ranges: the reduced solve must
    // return the same hands and per-hand counterfactual values as the exact solve.
    BOOST_REQUIRE_EQUAL(exact->artifact.strategy.size(), iso->artifact.strategy.size());
    BOOST_REQUIRE(!exact->artifact.strategy.empty());
    for (std::size_t i = 0; i < exact->artifact.strategy.size(); ++i) {
        BOOST_CHECK_EQUAL(exact->artifact.strategy[i].hand, iso->artifact.strategy[i].hand);
        BOOST_CHECK_SMALL(exact->artifact.strategy[i].ev - iso->artifact.strategy[i].ev, 1e-3);
    }
}

BOOST_AUTO_TEST_CASE(holdem_cli_dynamic_pruning_default_off_matches_exact_solver) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot_turn);
    BOOST_REQUIRE(spot.has_value());

    // With pruning disabled the solver must be bit-identical to the exact Step 2/3
    // solver: same hands, per-combo EV, and every action frequency.
    auto exact = zeta::holdem::cli::solve_spot(*spot, 96, {});
    auto defaulted = zeta::holdem::cli::solve_spot(
        *spot, 96, {.pruning = {.enabled = false}});
    BOOST_REQUIRE(exact.has_value());
    BOOST_REQUIRE(defaulted.has_value());
    BOOST_REQUIRE_EQUAL(exact->artifact.strategy.size(), defaulted->artifact.strategy.size());
    for (std::size_t i = 0; i < exact->artifact.strategy.size(); ++i) {
        BOOST_CHECK_EQUAL(exact->artifact.strategy[i].hand, defaulted->artifact.strategy[i].hand);
        BOOST_CHECK_EQUAL(exact->artifact.strategy[i].ev, defaulted->artifact.strategy[i].ev);
        BOOST_REQUIRE_EQUAL(exact->artifact.strategy[i].strategy.size(),
            defaulted->artifact.strategy[i].strategy.size());
        for (std::size_t action = 0; action < exact->artifact.strategy[i].strategy.size(); ++action) {
            BOOST_CHECK_EQUAL(exact->artifact.strategy[i].strategy[action].frequency,
                defaulted->artifact.strategy[i].strategy[action].frequency);
        }
    }
}

BOOST_AUTO_TEST_CASE(holdem_cli_dynamic_pruning_stays_within_tolerance_of_exact_solve) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot_turn);
    BOOST_REQUIRE(spot.has_value());

    constexpr uint64_t iterations = 256;
    auto exact = zeta::holdem::cli::solve_spot(*spot, iterations, {});
    // Opt-in approximate pruning: actions contributing under 1% of an infoset's
    // reach-weighted positive regret are frozen, reconsidered every 16 iterations,
    // and at least one action stays active. The approximate solve must track the
    // exact solve closely in both per-combo EV and root action frequencies.
    auto pruned = zeta::holdem::cli::solve_spot(
        *spot,
        iterations,
        {.pruning = {
             .enabled = true,
             .prune_threshold = 0.01,
             .minimum_active_actions = 1,
             .reconsider_interval = 16}});
    BOOST_REQUIRE(exact.has_value());
    BOOST_REQUIRE(pruned.has_value());
    BOOST_REQUIRE(zeta::holdem::cli::validate_artifact(pruned->artifact).has_value());
    BOOST_REQUIRE_EQUAL(exact->artifact.strategy.size(), pruned->artifact.strategy.size());
    BOOST_REQUIRE(!exact->artifact.strategy.empty());

    for (std::size_t i = 0; i < exact->artifact.strategy.size(); ++i) {
        BOOST_CHECK_EQUAL(exact->artifact.strategy[i].hand, pruned->artifact.strategy[i].hand);
        BOOST_CHECK_SMALL(exact->artifact.strategy[i].ev - pruned->artifact.strategy[i].ev, 1.0);
        BOOST_REQUIRE_EQUAL(exact->artifact.strategy[i].strategy.size(),
            pruned->artifact.strategy[i].strategy.size());
        for (std::size_t action = 0; action < exact->artifact.strategy[i].strategy.size(); ++action) {
            BOOST_CHECK_SMALL(
                exact->artifact.strategy[i].strategy[action].frequency
                    - pruned->artifact.strategy[i].strategy[action].frequency,
                0.05);
        }
    }
}

BOOST_AUTO_TEST_CASE(holdem_cli_parses_solver_runtime_knobs_from_spot_json) {
    // The spot JSON exposes the solver runtime knobs (memory budget, isomorphism,
    // dynamic pruning, worker threads) via an optional "runtime" object.
    constexpr const char* runtime_spot = R"({
  "street": "turn",
  "players": ["BTN", "BB"],
  "board": ["As", "Ks", "7h", "4h"],
  "ranges": ["AhKh", "QdJd"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "bet_fraction": 0.5,
  "max_history": 4,
  "public_state_id": 5,
  "runtime": {
    "worker_threads": 4,
    "memory_budget_bytes": 8589934592,
    "card_isomorphism": true,
    "allow_lossy_card_isomorphism": true,
    "dynamic_pruning": {
      "enabled": true,
      "prune_threshold": 0.02,
      "minimum_active_actions": 2,
      "reconsider_interval": 32
    }
  }
})";
    auto runtime = zeta::holdem::cli::parse_spot_runtime_options(runtime_spot);
    BOOST_REQUIRE(runtime.has_value());
    BOOST_CHECK_EQUAL(runtime->worker_threads, 4u);
    BOOST_CHECK_EQUAL(runtime->memory_budget_bytes, uint64_t{8} << 30);
    BOOST_CHECK(runtime->enable_card_isomorphism);
    BOOST_CHECK(runtime->allow_lossy_card_isomorphism);
    BOOST_CHECK(runtime->pruning.enabled);
    BOOST_CHECK(runtime->pruning.is_active());
    BOOST_CHECK_CLOSE(runtime->pruning.prune_threshold, 0.02, 1e-6);
    BOOST_CHECK_EQUAL(runtime->pruning.minimum_active_actions, 2u);
    BOOST_CHECK_EQUAL(runtime->pruning.reconsider_interval, 32u);
}

BOOST_AUTO_TEST_CASE(holdem_cli_runtime_options_default_when_block_absent) {
    // A spot with no "runtime" block yields the exact-solver defaults: pruning off,
    // isomorphism off, and an auto-derived memory budget.
    auto runtime = zeta::holdem::cli::parse_spot_runtime_options(sample_spot_turn);
    BOOST_REQUIRE(runtime.has_value());
    BOOST_CHECK(!runtime->enable_card_isomorphism);
    BOOST_CHECK(!runtime->allow_lossy_card_isomorphism);
    BOOST_CHECK(!runtime->pruning.enabled);
    BOOST_CHECK(!runtime->pruning.is_active());
    BOOST_CHECK_EQUAL(runtime->memory_budget_bytes, 0u);
}

BOOST_AUTO_TEST_CASE(holdem_cli_runtime_options_reject_invalid_minimum_active_actions) {
    // minimum_active_actions must stay >= 1; a zero value is a hard parse error, not a
    // silently clamped value.
    constexpr const char* bad_runtime_spot = R"({
  "street": "turn",
  "players": ["BTN", "BB"],
  "board": ["As", "Ks", "7h", "4h"],
  "ranges": ["AhKh", "QdJd"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "bet_fraction": 0.5,
  "max_history": 4,
  "public_state_id": 5,
  "runtime": {"dynamic_pruning": {"enabled": true, "minimum_active_actions": 0}}
})";
    auto runtime = zeta::holdem::cli::parse_spot_runtime_options(bad_runtime_spot);
    BOOST_REQUIRE(!runtime.has_value());
    BOOST_CHECK(runtime.error().message.find("minimum_active_actions") != std::string::npos);
}

BOOST_AUTO_TEST_CASE(holdem_cli_card_isomorphism_rejects_asymmetric_ranges) {
    // The turn board As Ks 7h 4h uses only spades and hearts, so diamonds and clubs are
    // interchangeable (swapping them fixes the board). Diamond-only ranges are not
    // invariant under that swap, so a lossless isomorphism request must be refused
    // unless the caller explicitly opts into the lossy approximation.
    constexpr const char* asymmetric_iso_spot = R"({
  "street": "turn",
  "players": ["BTN", "BB"],
  "board": ["As", "Ks", "7h", "4h"],
  "ranges": ["AdKd", "QdJd"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "bet_fraction": 0.5,
  "max_history": 4,
  "public_state_id": 5
})";
    auto spot = zeta::holdem::cli::parse_spot_json(asymmetric_iso_spot);
    BOOST_REQUIRE(spot.has_value());

    auto rejected = zeta::holdem::cli::solve_spot(
        *spot, 16, {.enable_card_isomorphism = true});
    BOOST_REQUIRE(!rejected.has_value());
    BOOST_CHECK(rejected.error().kind == zeta::holdem::cli::cli_error_kind::solver);
    BOOST_CHECK(rejected.error().message.find("suit-symmetric") != std::string::npos);

    auto accepted = zeta::holdem::cli::solve_spot(
        *spot, 16, {.enable_card_isomorphism = true, .allow_lossy_card_isomorphism = true});
    BOOST_REQUIRE(accepted.has_value());
    BOOST_CHECK(!accepted->artifact.strategy.empty());
}

BOOST_AUTO_TEST_CASE(holdem_cli_nonriver_artifact_persists_multi_street_graph_payload) {
    auto turn_spot = zeta::holdem::cli::parse_spot_json(sample_spot_turn);
    BOOST_REQUIRE(turn_spot.has_value());

    auto turn_output = zeta::holdem::cli::solve_spot(*turn_spot, 16, {.worker_threads = 2});
    BOOST_REQUIRE(turn_output.has_value());
    BOOST_CHECK_EQUAL(turn_output->artifact.schema_version, 4u);
    BOOST_CHECK_EQUAL(turn_output->artifact.extraction_version, 1u);
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

BOOST_AUTO_TEST_CASE(holdem_cli_populates_solve_hashes_and_warnings_by_default) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot);
    BOOST_REQUIRE(spot.has_value());

    // Reproducible input hashes and abstraction warnings are always recorded, even
    // when the (opt-in) exploitability measurement is left disabled.
    auto output = zeta::holdem::cli::solve_spot(*spot, 4);
    BOOST_REQUIRE(output.has_value());
    const auto& hashes = output->artifact.solver.hashes;
    BOOST_CHECK_NE(hashes.tree_hash, 0u);
    BOOST_CHECK_NE(hashes.range_hash, 0u);
    BOOST_CHECK_NE(hashes.board_hash, 0u);
    BOOST_CHECK_NE(hashes.betting_policy_hash, 0u);
    BOOST_CHECK_NE(hashes.solver_config_hash, 0u);
    BOOST_CHECK_NE(hashes.solve_hash, 0u);
    BOOST_CHECK(!output->artifact.solver.warnings.empty());
    // Without opt-in measurement no best-response pass runs.
    BOOST_CHECK(!output->artifact.solver.convergence.exploitability_available);
}

BOOST_AUTO_TEST_CASE(holdem_cli_solve_hashes_are_stable_and_input_sensitive) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot);
    auto other = zeta::holdem::cli::parse_spot_json(sample_spot_asymmetric_river);
    BOOST_REQUIRE(spot.has_value());
    BOOST_REQUIRE(other.has_value());

    // Two identical solves hash identically; a spot with a different board, range,
    // and betting policy hashes differently in every component and in the combined
    // digest, so hashes can gate solution reuse.
    auto first = zeta::holdem::cli::solve_spot(*spot, 4);
    auto second = zeta::holdem::cli::solve_spot(*spot, 4);
    auto different = zeta::holdem::cli::solve_spot(*other, 4);
    BOOST_REQUIRE(first.has_value());
    BOOST_REQUIRE(second.has_value());
    BOOST_REQUIRE(different.has_value());

    BOOST_CHECK_EQUAL(first->artifact.solver.hashes.solve_hash, second->artifact.solver.hashes.solve_hash);
    BOOST_CHECK_EQUAL(first->artifact.solver.hashes.board_hash, second->artifact.solver.hashes.board_hash);
    BOOST_CHECK_NE(first->artifact.solver.hashes.board_hash, different->artifact.solver.hashes.board_hash);
    BOOST_CHECK_NE(first->artifact.solver.hashes.range_hash, different->artifact.solver.hashes.range_hash);
    BOOST_CHECK_NE(first->artifact.solver.hashes.solve_hash, different->artifact.solver.hashes.solve_hash);

    // The iteration budget participates in the solver-configuration hash.
    auto more_iterations = zeta::holdem::cli::solve_spot(*spot, 8);
    BOOST_REQUIRE(more_iterations.has_value());
    BOOST_CHECK_NE(first->artifact.solver.hashes.solver_config_hash,
        more_iterations->artifact.solver.hashes.solver_config_hash);
}

BOOST_AUTO_TEST_CASE(holdem_cli_reports_heads_up_exploitability_when_enabled) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot_golden_turn);
    BOOST_REQUIRE(spot.has_value());

    // The nuts spot has an exact equilibrium (hero holds an unbeatable royal, villain
    // is drawing dead), so a well-converged solve is essentially unexploitable: both
    // per-seat best-response gaps and the aggregate exploitability collapse toward zero.
    auto output = zeta::holdem::cli::solve_spot(
        *spot, 600, {.convergence = {.measure_exploitability = true}});
    BOOST_REQUIRE(output.has_value());
    const auto& convergence = output->artifact.solver.convergence;
    BOOST_REQUIRE(convergence.exploitability_available);
    BOOST_CHECK_GE(convergence.exploitability, 0.0);
    BOOST_CHECK_LT(convergence.exploitability, 1.0);
    BOOST_CHECK_CLOSE(convergence.nash_conv, 2.0 * convergence.exploitability, 1e-6);
    BOOST_REQUIRE_EQUAL(convergence.best_response_gap.size(), 2u);
    BOOST_CHECK_GE(convergence.best_response_gap[0], 0.0);
    BOOST_CHECK_GE(convergence.best_response_gap[1], 0.0);
    BOOST_CHECK_GT(output->artifact.solver.hashes.solve_hash, 0u);
}

BOOST_AUTO_TEST_CASE(holdem_cli_exploitability_decreases_with_iterations) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot_golden_turn);
    BOOST_REQUIRE(spot.has_value());

    // A barely-iterated average strategy leaves the opponent's play exploitable; a
    // well-converged solve drives the measured exploitability down.
    auto coarse = zeta::holdem::cli::solve_spot(
        *spot, 2, {.convergence = {.measure_exploitability = true}});
    auto refined = zeta::holdem::cli::solve_spot(
        *spot, 600, {.convergence = {.measure_exploitability = true}});
    BOOST_REQUIRE(coarse.has_value());
    BOOST_REQUIRE(refined.has_value());
    BOOST_REQUIRE(coarse->artifact.solver.convergence.exploitability_available);
    BOOST_REQUIRE(refined->artifact.solver.convergence.exploitability_available);
    BOOST_CHECK_LE(refined->artifact.solver.convergence.exploitability,
        coarse->artifact.solver.convergence.exploitability);
}

BOOST_AUTO_TEST_CASE(holdem_cli_records_convergence_curve_with_interval) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot_golden_turn);
    BOOST_REQUIRE(spot.has_value());

    // A positive measurement interval samples the exploitability over the CFR loop;
    // samples are ordered by increasing iteration and carry non-negative metrics.
    auto output = zeta::holdem::cli::solve_spot(
        *spot, 200, {.convergence = {.measure_exploitability = true, .measurement_interval = 25}});
    BOOST_REQUIRE(output.has_value());
    const auto& curve = output->artifact.solver.convergence.curve;
    BOOST_REQUIRE(!curve.empty());
    for (std::size_t i = 0; i < curve.size(); ++i) {
        BOOST_CHECK_GT(curve[i].iteration, 0u);
        BOOST_CHECK_LE(curve[i].iteration, 200u);
        BOOST_CHECK_GE(curve[i].metric, 0.0);
        if (i > 0) {
            BOOST_CHECK_LT(curve[i - 1].iteration, curve[i].iteration);
        }
    }
}

BOOST_AUTO_TEST_CASE(holdem_cli_curve_sample_count_is_capped) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot_golden_turn);
    BOOST_REQUIRE(spot.has_value());

    // The retained curve honours max_curve_samples: a tiny interval over many
    // iterations still keeps at most the configured number of samples.
    auto output = zeta::holdem::cli::solve_spot(
        *spot,
        200,
        {.convergence = {
             .measure_exploitability = true,
             .measurement_interval = 1,
             .max_curve_samples = 3}});
    BOOST_REQUIRE(output.has_value());
    BOOST_CHECK_LE(output->artifact.solver.convergence.curve.size(), 3u);
}

BOOST_AUTO_TEST_CASE(holdem_cli_stops_early_on_target_exploitability) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot_golden_turn);
    BOOST_REQUIRE(spot.has_value());

    // The nuts spot converges to zero exploitability, so a generous quality target is
    // reached well before the iteration budget is exhausted: the solve stops early and
    // records the reduced iteration count.
    constexpr uint64_t budget = 2000;
    auto output = zeta::holdem::cli::solve_spot(
        *spot,
        budget,
        {.convergence = {.measure_exploitability = true, .target_exploitability = 5.0}});
    BOOST_REQUIRE(output.has_value());
    const auto& convergence = output->artifact.solver.convergence;
    BOOST_CHECK(convergence.reached_target);
    BOOST_CHECK_LT(output->artifact.solver.iterations, budget);
    BOOST_CHECK_LE(convergence.exploitability, 5.0);
    BOOST_CHECK_CLOSE(convergence.target_exploitability, 5.0, 1e-6);
}

BOOST_AUTO_TEST_CASE(holdem_cli_multiway_reports_normalized_regret_without_exploitability) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot_multiway);
    BOOST_REQUIRE(spot.has_value());

    // Multiway solves do not compute an exact best response; they report a normalized
    // average-regret metric and warn that exploitability is unavailable.
    auto output = zeta::holdem::cli::solve_spot(
        *spot, 16, {.convergence = {.measure_exploitability = true}});
    BOOST_REQUIRE(output.has_value());
    const auto& convergence = output->artifact.solver.convergence;
    BOOST_CHECK(!convergence.exploitability_available);
    BOOST_CHECK_GE(convergence.normalized_regret, 0.0);
    const bool warns_multiway = std::ranges::any_of(
        output->artifact.solver.warnings, [](const std::string& warning) {
            return warning.find("normalized average-regret") != std::string::npos;
        });
    BOOST_CHECK(warns_multiway);
}

BOOST_AUTO_TEST_CASE(holdem_cli_parses_convergence_runtime_knobs_from_spot_json) {
    constexpr const char* convergence_spot = R"({
  "street": "turn",
  "players": ["BTN", "BB"],
  "board": ["As", "Ks", "7h", "4h"],
  "ranges": ["AhKh", "QdJd"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "bet_fraction": 0.5,
  "max_history": 4,
  "public_state_id": 5,
  "runtime": {
    "convergence": {
      "measure_exploitability": true,
      "measurement_interval": 20,
      "target_exploitability": 0.5,
      "max_curve_samples": 64
    }
  }
})";
    auto runtime = zeta::holdem::cli::parse_spot_runtime_options(convergence_spot);
    BOOST_REQUIRE(runtime.has_value());
    BOOST_CHECK(runtime->convergence.measure_exploitability);
    BOOST_CHECK_EQUAL(runtime->convergence.measurement_interval, 20u);
    BOOST_CHECK_CLOSE(runtime->convergence.target_exploitability, 0.5, 1e-6);
    BOOST_CHECK_EQUAL(runtime->convergence.max_curve_samples, 64u);
}

BOOST_AUTO_TEST_CASE(holdem_cli_convergence_runtime_defaults_when_block_absent) {
    // With no runtime.convergence block the reporting knobs default to off.
    auto runtime = zeta::holdem::cli::parse_spot_runtime_options(sample_spot_turn);
    BOOST_REQUIRE(runtime.has_value());
    BOOST_CHECK(!runtime->convergence.measure_exploitability);
    BOOST_CHECK_EQUAL(runtime->convergence.measurement_interval, 0u);
    BOOST_CHECK_CLOSE(runtime->convergence.target_exploitability, 0.0, 1e-6);
}

BOOST_AUTO_TEST_CASE(holdem_cli_convergence_runtime_rejects_negative_target) {
    constexpr const char* bad_spot = R"({
  "street": "turn",
  "players": ["BTN", "BB"],
  "board": ["As", "Ks", "7h", "4h"],
  "ranges": ["AhKh", "QdJd"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "bet_fraction": 0.5,
  "max_history": 4,
  "public_state_id": 5,
  "runtime": {"convergence": {"target_exploitability": -1.0}}
})";
    auto runtime = zeta::holdem::cli::parse_spot_runtime_options(bad_spot);
    BOOST_REQUIRE(!runtime.has_value());
    BOOST_CHECK(runtime.error().message.find("target_exploitability") != std::string::npos);
}

BOOST_AUTO_TEST_CASE(holdem_cli_artifact_json_roundtrips_hashes_and_convergence) {
    auto spot = zeta::holdem::cli::parse_spot_json(sample_spot_golden_turn);
    BOOST_REQUIRE(spot.has_value());

    auto output = zeta::holdem::cli::solve_spot(
        *spot, 120, {.convergence = {.measure_exploitability = true, .measurement_interval = 20}});
    BOOST_REQUIRE(output.has_value());
    BOOST_REQUIRE(output->artifact.solver.convergence.exploitability_available);
    BOOST_REQUIRE(!output->artifact.solver.convergence.curve.empty());

    const auto json = zeta::holdem::cli::serialize_artifact_json(output->artifact);
    auto parsed = zeta::holdem::cli::parse_artifact_json(json);
    BOOST_REQUIRE(parsed.has_value());

    const auto& original = output->artifact.solver;
    const auto& restored = parsed->solver;
    BOOST_CHECK_EQUAL(restored.hashes.tree_hash, original.hashes.tree_hash);
    BOOST_CHECK_EQUAL(restored.hashes.solve_hash, original.hashes.solve_hash);
    BOOST_CHECK_EQUAL(restored.hashes.board_hash, original.hashes.board_hash);
    BOOST_CHECK_EQUAL(restored.convergence.exploitability_available,
        original.convergence.exploitability_available);
    BOOST_CHECK_CLOSE(restored.convergence.exploitability, original.convergence.exploitability, 1e-9);
    BOOST_CHECK_CLOSE(restored.convergence.nash_conv, original.convergence.nash_conv, 1e-9);
    BOOST_REQUIRE_EQUAL(restored.convergence.best_response_gap.size(),
        original.convergence.best_response_gap.size());
    BOOST_REQUIRE_EQUAL(restored.convergence.curve.size(), original.convergence.curve.size());
    BOOST_CHECK_EQUAL(restored.convergence.curve.front().iteration,
        original.convergence.curve.front().iteration);
    BOOST_REQUIRE_EQUAL(restored.warnings.size(), original.warnings.size());
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
