#include <boost/test/unit_test.hpp>

#include "cli/solve_cli.h"
#include "app/app_settings.h"
#include "document/document_json.h"
#include "solver/solver_session.h"
#include "solver_state.h"
#include "spot_document.h"
#include "theme/theme_registry.h"
#include "theme/theme_styles.h"
#include "viewmodels/range_view_model.h"
#include "viewmodels/spot_view_model.h"
#include "widgets/range_editor.h"
#include "widgets/spot_builder.h"

#include <QApplication>
#include <QComboBox>
#include <QImage>
#include <QLabel>
#include <QPainter>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QSpinBox>
#include <QTableWidget>
#include <QTemporaryDir>
#include <QVBoxLayout>

#include <algorithm>
#include <set>
#include <regex>
#include <utility>

namespace {

    [[nodiscard]] zeta::holdem::ui::spot sample_heads_up_spot()
    {
        auto spot = zeta::holdem::cli::parse_spot_json(R"({
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c", "4h", "2s"],
  "ranges": ["AA,AKs", "AA,AKs"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "bet_fraction": 0.5,
  "max_history": 8,
  "public_state_id": 7,
  "samples_per_combo": 8
})");
        BOOST_REQUIRE(spot.has_value());
        return *spot;
    }

    [[nodiscard]] QApplication& qt_app()
    {
        qputenv("QT_QPA_PLATFORM", QByteArray{"offscreen"});
        if (auto* existing = qobject_cast<QApplication*>(QApplication::instance())) {
            return *existing;
        }
        static int argc = 1;
        static char app_name[] = "zeta_tests";
        static char* argv[] = {app_name, nullptr};
        static auto* app = new QApplication{argc, argv};
        return *app;
    }

    void insert_token_colors(const zeta::holdem::ui::theme::theme_tokens& tokens, std::set<std::string>& colors)
    {
        colors.insert(tokens.background_base);
        colors.insert(tokens.background_raised);
        colors.insert(tokens.background_sunken);
        colors.insert(tokens.background_input);
        colors.insert(tokens.border_subtle);
        colors.insert(tokens.border_strong);
        colors.insert(tokens.text_primary);
        colors.insert(tokens.text_secondary);
        colors.insert(tokens.text_muted);
        colors.insert(tokens.accent_primary);
        colors.insert(tokens.accent_secondary);
        colors.insert(tokens.action_primary);
        colors.insert(tokens.action_primary_hover);
        colors.insert(tokens.action_positive);
        colors.insert(tokens.action_negative);
        colors.insert(tokens.ev_positive);
        colors.insert(tokens.ev_negative);
        colors.insert(tokens.ev_neutral);
        colors.insert(tokens.warning);
        colors.insert(tokens.error);
        colors.insert(tokens.success);
        colors.insert(tokens.selection);
        for (const auto& color : tokens.range_heat) {
            colors.insert(color);
        }
    }

    [[nodiscard]] bool has_issue(
        const std::vector<zeta::holdem::ui::viewmodels::spot_validation_issue>& issues,
        const std::string& field)
    {
        return std::ranges::any_of(issues, [&field](const auto& issue) {
            return issue.field == field;
        });
    }

}

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

BOOST_AUTO_TEST_CASE(holdem_ui_document_json_accepts_reordered_envelope_escaped_strings_and_artifact_null) {
    constexpr const char* json = R"({
  "recent_history": [
    {
      "outcome": "failed: quoted \"message\"",
      "iterations": 3,
      "timestamp_utc": "2026-08-03T20:00:00Z"
    }
  ],
  "artifact": null,
  "metadata": {
    "tags": ["river", "quote\"tag", "slash\\tag"],
    "last_solve_summary": "No artifact",
    "updated_utc": "2026-08-03T20:00:00Z",
    "created_utc": "2026-08-03T19:00:00Z"
  },
  "spot": {
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
  },
  "document_schema_version": 1
})";

    auto parsed = zeta::holdem::ui::document::parse_document_json(json);

    BOOST_REQUIRE(parsed.has_value());
    BOOST_CHECK_EQUAL(parsed->spot.players[0], "BT\"N");
    BOOST_CHECK_EQUAL(parsed->spot.players[1], "B\\B");
    BOOST_CHECK(!parsed->artifact.has_value());
    BOOST_REQUIRE(parsed->metadata.has_value());
    BOOST_REQUIRE_EQUAL(parsed->metadata->tags.size(), 3u);
    BOOST_CHECK_EQUAL(parsed->metadata->tags[1], "quote\"tag");
    BOOST_REQUIRE_EQUAL(parsed->recent_history.size(), 1u);
    BOOST_CHECK_EQUAL(parsed->recent_history[0].outcome, "failed: quoted \"message\"");
}

BOOST_AUTO_TEST_CASE(holdem_ui_document_json_rejects_missing_spot_and_invalid_history_entries) {
    constexpr const char* missing_spot = R"({
  "document_schema_version": 1,
  "metadata": {},
  "artifact": null,
  "recent_history": []
})";
    constexpr const char* invalid_history = R"({
  "document_schema_version": 1,
  "spot": {
    "players": ["BTN", "BB"],
    "board": ["As", "Kd", "7c", "4h", "2s"],
    "ranges": ["AhKh", "QdJd"]
  },
  "recent_history": [
    {"timestamp_utc": "2026-08-03T20:00:00Z", "iterations": -1, "outcome": "failed"}
  ]
})";

    BOOST_CHECK(!zeta::holdem::ui::document::parse_document_json(missing_spot).has_value());
    BOOST_CHECK(!zeta::holdem::ui::document::parse_document_json(invalid_history).has_value());
}

BOOST_AUTO_TEST_CASE(holdem_ui_document_dirty_after_artifact_replacement_and_persistence) {
    auto document = zeta::holdem::ui::spot_document::create_new();
    document.replace_spot(sample_heads_up_spot());
    document.clear_dirty();
    auto artifact = zeta::holdem::cli::solve_artifact{};
    artifact.players = {"BTN", "BB"};
    artifact.board = {"As", "Kd", "7c", "4h", "2s"};
    artifact.solver.iterations = 1;
    artifact.solver.timestamp = "2026-08-03T20:00:00Z";
    artifact.solver.git_revision = "abc1234";
    artifact.strategy = {
        zeta::holdem::cli::hand_strategy{
            .hand = "AhAd",
            .strategy = {
                zeta::holdem::cli::action_strategy{.action = "check", .frequency = 1.0}
            },
            .ev = 1.25
        }
    };

    document.replace_artifact(artifact);
    document.add_history(zeta::holdem::ui::solve_history_entry{
        .timestamp_utc = "2026-08-03T20:00:01Z",
        .iterations = 1,
        .outcome = "completed"
    });

    BOOST_CHECK(document.is_dirty());
    auto parsed = zeta::holdem::ui::spot_document::parse_json(document.serialize_json());
    BOOST_REQUIRE(parsed.has_value());
    BOOST_REQUIRE(parsed->artifact().has_value());
    BOOST_CHECK_EQUAL(parsed->artifact()->solver.iterations, 1u);
    BOOST_REQUIRE_EQUAL(parsed->recent_history().size(), 1u);
    BOOST_CHECK_EQUAL(parsed->recent_history()[0].outcome, "completed");
    BOOST_CHECK(!parsed->is_dirty());
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

BOOST_AUTO_TEST_CASE(holdem_ui_solver_session_completes_and_carries_timing_artifact_and_snapshot) {
    zeta::holdem::ui::solver::solver_session session{
        zeta::holdem::ui::solver::solver_session_request{
            .spot_snapshot = sample_heads_up_spot(),
            .iterations = 1,
            .runtime = zeta::holdem::cli::solve_runtime_options{
                .timestamp_utc = "2026-08-03T20:00:00Z",
                .git_revision = "abc1234"
            }
        }
    };

    const auto result = session.run();

    BOOST_CHECK(result.terminal_state == zeta::holdem::ui::solver::solver_session_terminal_state::completed);
    BOOST_REQUIRE(result.artifact.has_value());
    BOOST_CHECK_EQUAL(result.artifact->solver.iterations, 1u);
    BOOST_CHECK_EQUAL(result.artifact->solver.timestamp, "2026-08-03T20:00:00Z");
    BOOST_CHECK_EQUAL(result.artifact->solver.git_revision, "abc1234");
    BOOST_CHECK_EQUAL(result.spot_snapshot.players.size(), 2u);
    BOOST_CHECK_GE(result.timing.graph_build_ms, 0.0);
    BOOST_CHECK_GE(result.timing.cfr_iterations_ms, 0.0);
    BOOST_CHECK_GE(result.timing.extraction_ms, 0.0);
}

BOOST_AUTO_TEST_CASE(holdem_ui_solver_session_reports_failed_and_cancelled_before_start_states) {
    auto invalid_spot = sample_heads_up_spot();
    invalid_spot.root_actor = 9;
    zeta::holdem::ui::solver::solver_session failed_session{
        zeta::holdem::ui::solver::solver_session_request{
            .spot_snapshot = invalid_spot,
            .iterations = 1
        }
    };

    const auto failed = failed_session.run();
    BOOST_CHECK(failed.terminal_state == zeta::holdem::ui::solver::solver_session_terminal_state::failed);
    BOOST_CHECK(!failed.artifact.has_value());
    BOOST_CHECK(!failed.error_message.empty());

    zeta::holdem::ui::solver::solver_session cancelled_session{
        zeta::holdem::ui::solver::solver_session_request{
            .spot_snapshot = sample_heads_up_spot(),
            .iterations = 1
        }
    };
    cancelled_session.cancel_before_start();
    const auto cancelled = cancelled_session.run();

    BOOST_CHECK(cancelled.terminal_state == zeta::holdem::ui::solver::solver_session_terminal_state::cancelled_before_start);
    BOOST_CHECK(!cancelled.artifact.has_value());
    BOOST_CHECK(cancelled.error_message.empty());
}

BOOST_AUTO_TEST_CASE(holdem_ui_structured_spot_builder_resizes_player_arrays_safely) {
    auto spot = zeta::holdem::ui::viewmodels::make_template_spot(
        zeta::holdem::ui::viewmodels::spot_template_kind::heads_up_river);
    spot.players[0] = "Hero";
    spot.ranges[0] = "AhAd";
    spot.stacks[0] = 250.0;
    spot.contributions[0] = 75.0;
    spot.root_actor = 1;
    spot.hero_seat = 1;

    auto resized = zeta::holdem::ui::viewmodels::resize_player_count(std::move(spot), 6);

    BOOST_REQUIRE_EQUAL(resized.players.size(), 6u);
    BOOST_CHECK_EQUAL(resized.ranges.size(), 6u);
    BOOST_CHECK_EQUAL(resized.stacks.size(), 6u);
    BOOST_CHECK_EQUAL(resized.contributions.size(), 6u);
    BOOST_CHECK_EQUAL(resized.players[0], "Hero");
    BOOST_CHECK_EQUAL(resized.ranges[0], "AhAd");
    BOOST_CHECK_EQUAL(resized.stacks[0], 250.0);
    BOOST_CHECK_EQUAL(resized.contributions[0], 75.0);
    BOOST_CHECK_EQUAL(resized.ranges[5], "AA");
    BOOST_CHECK_EQUAL(resized.stacks[5], 100.0);
    BOOST_CHECK_EQUAL(resized.contributions[5], 0.0);

    resized.root_actor = 5;
    resized.hero_seat = 4;
    auto shrunk = zeta::holdem::ui::viewmodels::resize_player_count(std::move(resized), 3);

    BOOST_REQUIRE_EQUAL(shrunk.players.size(), 3u);
    BOOST_CHECK_EQUAL(shrunk.ranges.size(), 3u);
    BOOST_CHECK_EQUAL(shrunk.stacks.size(), 3u);
    BOOST_CHECK_EQUAL(shrunk.contributions.size(), 3u);
    BOOST_CHECK_EQUAL(shrunk.root_actor, 2u);
    BOOST_CHECK_EQUAL(shrunk.hero_seat, 2u);
}

BOOST_AUTO_TEST_CASE(holdem_ui_structured_spot_validation_reports_board_actor_and_array_errors) {
    auto spot = zeta::holdem::ui::viewmodels::make_template_spot(
        zeta::holdem::ui::viewmodels::spot_template_kind::three_way_flop);
    BOOST_CHECK(zeta::holdem::ui::viewmodels::validate_structured_spot(spot).empty());

    spot.board = {"As", "As", "7c"};
    auto issues = zeta::holdem::ui::viewmodels::validate_structured_spot(spot);
    BOOST_CHECK(has_issue(issues, "board"));

    spot.board = {"As", "Kd"};
    issues = zeta::holdem::ui::viewmodels::validate_structured_spot(spot);
    BOOST_CHECK(has_issue(issues, "board"));

    spot.board = {"As", "Kd", "1x"};
    issues = zeta::holdem::ui::viewmodels::validate_structured_spot(spot);
    BOOST_CHECK(has_issue(issues, "board"));

    spot.board = {"As", "Kd", "7c"};
    spot.root_actor = 7;
    spot.hero_seat = 8;
    spot.ranges.pop_back();
    spot.stacks.pop_back();
    spot.contributions.pop_back();
    issues = zeta::holdem::ui::viewmodels::validate_structured_spot(spot);
    BOOST_CHECK(has_issue(issues, "root_actor"));
    BOOST_CHECK(has_issue(issues, "hero_seat"));
    BOOST_CHECK(has_issue(issues, "ranges"));
    BOOST_CHECK(has_issue(issues, "stacks"));
    BOOST_CHECK(has_issue(issues, "contributions"));
}

BOOST_AUTO_TEST_CASE(holdem_ui_structured_spot_templates_roundtrip_to_valid_json_for_two_to_six_players) {
    for (const auto kind : {
             zeta::holdem::ui::viewmodels::spot_template_kind::heads_up_river,
             zeta::holdem::ui::viewmodels::spot_template_kind::three_way_flop,
             zeta::holdem::ui::viewmodels::spot_template_kind::four_way_turn}) {
        const auto templated = zeta::holdem::ui::viewmodels::make_template_spot(kind);
        BOOST_CHECK(zeta::holdem::ui::viewmodels::validate_structured_spot(templated).empty());
        const auto parsed = zeta::holdem::cli::parse_spot_json(zeta::holdem::cli::serialize_spot_json(templated));
        BOOST_REQUIRE(parsed.has_value());
        BOOST_CHECK_EQUAL(parsed->players.size(), templated.players.size());
        BOOST_CHECK_EQUAL(parsed->board.size(), templated.board.size());
    }

    auto resizable = zeta::holdem::ui::viewmodels::make_template_spot(
        zeta::holdem::ui::viewmodels::spot_template_kind::heads_up_river);
    for (std::size_t count = 2; count <= 6; ++count) {
        auto spot = zeta::holdem::ui::viewmodels::resize_player_count(resizable, count);
        spot.root_actor = static_cast<uint8_t>(count - 1u);
        spot.hero_seat = 0;
        BOOST_CHECK(zeta::holdem::ui::viewmodels::validate_structured_spot(spot).empty());
        const auto parsed = zeta::holdem::cli::parse_spot_json(zeta::holdem::cli::serialize_spot_json(spot));
        BOOST_REQUIRE(parsed.has_value());
        BOOST_CHECK_EQUAL(parsed->players.size(), count);
        BOOST_CHECK_EQUAL(parsed->ranges.size(), count);
        BOOST_CHECK_EQUAL(parsed->stacks.size(), count);
        BOOST_CHECK_EQUAL(parsed->contributions.size(), count);
    }
}

BOOST_AUTO_TEST_CASE(holdem_ui_spot_builder_reflects_validated_json_edits_in_structured_controls) {
    auto& app = qt_app();
    auto initial = zeta::holdem::ui::viewmodels::make_template_spot(
        zeta::holdem::ui::viewmodels::spot_template_kind::heads_up_river);
    auto observed = initial;
    zeta::holdem::ui::widgets::spot_builder builder{
        initial,
        zeta::holdem::ui::theme::metrics_for_density(zeta::holdem::ui::theme::density_mode::comfortable),
        [&observed](zeta::holdem::ui::spot next) {
            observed = std::move(next);
        },
        {},
        nullptr};

    auto edited = zeta::holdem::ui::viewmodels::make_template_spot(
        zeta::holdem::ui::viewmodels::spot_template_kind::four_way_turn);
    edited.players = {"A", "B", "C", "D"};
    const auto parsed = zeta::holdem::cli::parse_spot_json(zeta::holdem::cli::serialize_spot_json(edited));
    BOOST_REQUIRE(parsed.has_value());

    builder.set_spot(*parsed);
    app.processEvents();

    auto* street = builder.findChild<QComboBox*>("streetSelector");
    auto* player_count = builder.findChild<QSpinBox*>("playerCountSelector");
    auto* root_actor = builder.findChild<QComboBox*>("rootActorSelector");
    auto* hero_seat = builder.findChild<QComboBox*>("heroSeatSelector");

    BOOST_REQUIRE(street != nullptr);
    BOOST_REQUIRE(player_count != nullptr);
    BOOST_REQUIRE(root_actor != nullptr);
    BOOST_REQUIRE(hero_seat != nullptr);
    BOOST_CHECK_EQUAL(street->currentText().toStdString(), "turn");
    BOOST_CHECK_EQUAL(player_count->value(), 4);
    BOOST_CHECK_EQUAL(root_actor->currentIndex(), static_cast<int>(edited.root_actor));
    BOOST_CHECK_EQUAL(hero_seat->currentIndex(), static_cast<int>(edited.hero_seat));
}

BOOST_AUTO_TEST_CASE(holdem_ui_range_view_model_expands_exact_class_and_weighted_syntax) {
    {
        const auto exact = zeta::holdem::ui::viewmodels::analyze_range("AhKh", {});
        BOOST_CHECK(!exact.parse_issue.has_value());
        BOOST_REQUIRE_EQUAL(exact.exact_combos.size(), 1u);
        BOOST_CHECK_EQUAL(exact.exact_combos[0].hand, "AhKh");
        BOOST_CHECK_EQUAL(exact.exact_combos[0].hand_class, "AKs");
        BOOST_CHECK_EQUAL(exact.metrics.combos_before_blockers, 1u);
        BOOST_CHECK_EQUAL(exact.metrics.live_combos, 1u);
    }

    {
        const auto classes = zeta::holdem::ui::viewmodels::analyze_range("TT+, AQs-AJs, KQo", {});
        BOOST_CHECK(!classes.parse_issue.has_value());
        BOOST_CHECK_EQUAL(classes.metrics.combos_before_blockers, 50u);
        BOOST_CHECK_EQUAL(classes.metrics.live_combos, 50u);
    }

    {
        const auto weighted = zeta::holdem::ui::viewmodels::analyze_range("AA:0.5, AKs:0.25", {});
        BOOST_CHECK(!weighted.parse_issue.has_value());
        const auto labels = zeta::holdem::ui::viewmodels::hand_class_labels();
        const auto aa = std::distance(labels.begin(), std::ranges::find(labels, std::string{"AA"}));
        const auto aks = std::distance(labels.begin(), std::ranges::find(labels, std::string{"AKs"}));
        BOOST_REQUIRE_LT(aa, static_cast<std::ptrdiff_t>(weighted.matrix.size()));
        BOOST_REQUIRE_LT(aks, static_cast<std::ptrdiff_t>(weighted.matrix.size()));
        BOOST_CHECK_CLOSE(weighted.matrix[static_cast<std::size_t>(aa)].max_weight, 0.5f, 0.001);
        BOOST_CHECK_CLOSE(weighted.matrix[static_cast<std::size_t>(aks)].max_weight, 0.25f, 0.001);
        const auto normalized = zeta::holdem::ui::viewmodels::normalized_exact_range_text(weighted);
        BOOST_CHECK(normalized.find(":0.5") != std::string::npos);
        BOOST_CHECK(normalized.find(":0.25") != std::string::npos);
    }

    const auto invalid = zeta::holdem::ui::viewmodels::analyze_range("AA AKs", {});
    BOOST_REQUIRE(invalid.parse_issue.has_value());
    BOOST_CHECK_EQUAL(invalid.parse_issue->position, 3u);

    const auto without_aces = zeta::holdem::ui::viewmodels::analyze_range(
        zeta::holdem::ui::viewmodels::set_hand_class_enabled("TT+", "AA", false),
        {});
    BOOST_CHECK(!without_aces.parse_issue.has_value());
    BOOST_CHECK_EQUAL(without_aces.metrics.combos_before_blockers, 24u);
}

BOOST_AUTO_TEST_CASE(holdem_ui_range_view_model_reports_board_blockers_live_metrics_and_empty_ranges) {
    const auto blocked = zeta::holdem::ui::viewmodels::analyze_range("AA", {"As"});
    BOOST_CHECK(!blocked.parse_issue.has_value());
    BOOST_CHECK_EQUAL(blocked.metrics.combos_before_blockers, 6u);
    BOOST_CHECK_EQUAL(blocked.metrics.live_combos, 3u);
    BOOST_REQUIRE_EQUAL(blocked.metrics.blocked_combos_by_card.size(), 1u);
    BOOST_CHECK_EQUAL(blocked.metrics.blocked_combos_by_card[0].first, "As");
    BOOST_CHECK_EQUAL(blocked.metrics.blocked_combos_by_card[0].second, 3u);

    auto spot = zeta::holdem::ui::viewmodels::make_template_spot(
        zeta::holdem::ui::viewmodels::spot_template_kind::heads_up_river);
    spot.ranges[0] = "AsKd";
    const auto issues = zeta::holdem::ui::viewmodels::validate_structured_spot(spot);
    BOOST_CHECK(has_issue(issues, "ranges"));
}

BOOST_AUTO_TEST_CASE(holdem_ui_range_text_roundtrips_weighted_ranges_through_spot_json_per_seat) {
    auto spot = zeta::holdem::ui::viewmodels::make_template_spot(
        zeta::holdem::ui::viewmodels::spot_template_kind::three_way_flop);
    spot.ranges = {"AA:0.5, AKs:0.25", "QQ+, AJs:0.75", "AhKh:0.2"};

    const auto json = zeta::holdem::cli::serialize_spot_json(spot);
    const auto parsed = zeta::holdem::cli::parse_spot_json(json);

    BOOST_REQUIRE(parsed.has_value());
    BOOST_REQUIRE_EQUAL(parsed->ranges.size(), 3u);
    BOOST_CHECK_EQUAL(parsed->ranges[0], "AA:0.5, AKs:0.25");
    BOOST_CHECK_EQUAL(parsed->ranges[1], "QQ+, AJs:0.75");
    BOOST_CHECK_EQUAL(parsed->ranges[2], "AhKh:0.2");
}

BOOST_AUTO_TEST_CASE(holdem_ui_range_editor_authors_all_seat_ranges_without_raw_json) {
    auto& app = qt_app();
    auto initial = zeta::holdem::ui::viewmodels::resize_player_count(
        zeta::holdem::ui::viewmodels::make_template_spot(zeta::holdem::ui::viewmodels::spot_template_kind::heads_up_river),
        6);
    auto observed = initial;

    zeta::holdem::ui::widgets::range_editor editor{
        initial,
        zeta::holdem::ui::theme::metrics_for_density(zeta::holdem::ui::theme::density_mode::comfortable),
        [&observed](zeta::holdem::ui::spot next) {
            observed = std::move(next);
        },
        nullptr};

    auto* seat_selector = editor.findChild<QComboBox*>("rangeSeatSelector");
    auto* range_text = editor.findChild<QPlainTextEdit*>("rangeTextEditor");
    auto* combo_table = editor.findChild<QTableWidget*>("exactComboTable");
    BOOST_REQUIRE(seat_selector != nullptr);
    BOOST_REQUIRE(range_text != nullptr);
    BOOST_REQUIRE(combo_table != nullptr);

    const std::vector<std::string> ranges{
        "AA:0.5",
        "KK",
        "QQ",
        "JJ",
        "TT",
        "AKs:0.25"
    };
    for (std::size_t seat = 0; seat < ranges.size(); ++seat) {
        seat_selector->setCurrentIndex(static_cast<int>(seat));
        range_text->setPlainText(QString::fromStdString(ranges[seat]));
        app.processEvents();
        BOOST_CHECK_EQUAL(observed.ranges[seat], ranges[seat]);
    }

    BOOST_CHECK_EQUAL(combo_table->rowCount(), 4);
}

BOOST_AUTO_TEST_CASE(holdem_ui_theme_registry_exposes_required_stage2_themes_and_tokens) {
    const auto themes = zeta::holdem::ui::theme::registered_themes();

    BOOST_REQUIRE_EQUAL(themes.size(), 3u);
    BOOST_CHECK(zeta::holdem::ui::theme::theme_id_from_key("dark-pro").has_value());
    BOOST_CHECK(zeta::holdem::ui::theme::theme_id_from_key("light-pro").has_value());
    BOOST_CHECK(zeta::holdem::ui::theme::theme_id_from_key("high-contrast").has_value());

    for (const auto& theme : themes) {
        BOOST_CHECK(!theme.key.empty());
        BOOST_CHECK(!theme.display_name.empty());
        BOOST_CHECK(!theme.tokens.background_base.empty());
        BOOST_CHECK(!theme.tokens.background_raised.empty());
        BOOST_CHECK(!theme.tokens.border_subtle.empty());
        BOOST_CHECK(!theme.tokens.text_primary.empty());
        BOOST_CHECK(!theme.tokens.text_muted.empty());
        BOOST_CHECK(!theme.tokens.accent_primary.empty());
        BOOST_CHECK(!theme.tokens.action_primary.empty());
        BOOST_CHECK(!theme.tokens.ev_positive.empty());
        BOOST_CHECK(!theme.tokens.ev_negative.empty());
        BOOST_CHECK(!theme.tokens.ev_neutral.empty());
        BOOST_CHECK(!theme.tokens.warning.empty());
        BOOST_CHECK(!theme.tokens.error.empty());
        BOOST_CHECK(!theme.tokens.success.empty());
        BOOST_CHECK_EQUAL(theme.tokens.range_heat.size(), 4u);
    }
}

BOOST_AUTO_TEST_CASE(holdem_ui_settings_persist_theme_density_recent_files_and_splitters) {
    QTemporaryDir dir;
    BOOST_REQUIRE(dir.isValid());
    const auto settings_path = dir.filePath(QStringLiteral("holdem-ui.ini"));

    {
        zeta::holdem::ui::app::app_settings settings{settings_path};
        settings.set_active_theme(zeta::holdem::ui::theme::theme_id::high_contrast);
        settings.set_density(zeta::holdem::ui::theme::density_mode::compact);
        settings.set_shell_splitter_sizes(QList<int>{180, 820});
        settings.set_workspace_splitter_sizes(QList<int>{620, 360});
        settings.add_recent_file(QStringLiteral("C:/tmp/a.json"));
        settings.add_recent_file(QStringLiteral("C:/tmp/b.json"));
        settings.add_recent_file(QStringLiteral("C:/tmp/a.json"));
        settings.sync();
    }

    zeta::holdem::ui::app::app_settings settings{settings_path};
    BOOST_CHECK(settings.active_theme() == zeta::holdem::ui::theme::theme_id::high_contrast);
    BOOST_CHECK(settings.density() == zeta::holdem::ui::theme::density_mode::compact);
    BOOST_REQUIRE_EQUAL(settings.shell_splitter_sizes().size(), 2);
    BOOST_CHECK_EQUAL(settings.shell_splitter_sizes()[0], 180);
    BOOST_REQUIRE_EQUAL(settings.workspace_splitter_sizes().size(), 2);
    BOOST_CHECK_EQUAL(settings.workspace_splitter_sizes()[1], 360);
    BOOST_REQUIRE_EQUAL(settings.recent_files().size(), 2);
    BOOST_CHECK_EQUAL(settings.recent_files()[0].toStdString(), "C:/tmp/a.json");
    BOOST_CHECK_EQUAL(settings.recent_files()[1].toStdString(), "C:/tmp/b.json");
}

BOOST_AUTO_TEST_CASE(holdem_ui_theme_styles_use_registered_tokens_only) {
    const std::regex color_pattern{"#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?"};

    for (const auto& theme : zeta::holdem::ui::theme::registered_themes()) {
        std::set<std::string> token_colors;
        insert_token_colors(theme.tokens, token_colors);

        const auto sheet = zeta::holdem::ui::theme::style_sheet(theme, zeta::holdem::ui::theme::density_mode::comfortable).toStdString();
        BOOST_CHECK(!sheet.empty());
        for (std::sregex_iterator it{sheet.begin(), sheet.end(), color_pattern}, end; it != end; ++it) {
            BOOST_CHECK_MESSAGE(token_colors.contains(it->str()), "Theme stylesheet used non-token color " << it->str());
        }
    }
}

BOOST_AUTO_TEST_CASE(holdem_ui_widget_render_smoke_covers_every_theme) {
    auto& app = qt_app();

    for (const auto& theme : zeta::holdem::ui::theme::registered_themes()) {
        QWidget root;
        root.setStyleSheet(zeta::holdem::ui::theme::style_sheet(theme, zeta::holdem::ui::theme::density_mode::comfortable));
        auto* layout = new QVBoxLayout{&root};
        auto* title = new QLabel{QString::fromStdString(theme.display_name), &root};
        title->setObjectName("panelTitle");
        auto* button = new QPushButton{QStringLiteral("Solve"), &root};
        button->setObjectName("callButton");
        auto* console = new QPlainTextEdit{QStringLiteral("Ready"), &root};
        console->setObjectName("solveConsole");
        layout->addWidget(title);
        layout->addWidget(button);
        layout->addWidget(console);
        root.resize(260, 180);
        root.show();
        app.processEvents();

        QImage image{root.size(), QImage::Format_ARGB32};
        image.fill(Qt::transparent);
        QPainter painter{&image};
        root.render(&painter);

        std::set<QRgb> sampled_colors;
        for (int y = 0; y < image.height(); y += 12) {
            for (int x = 0; x < image.width(); x += 12) {
                sampled_colors.insert(image.pixel(x, y));
            }
        }

        BOOST_CHECK_MESSAGE(sampled_colors.size() > 1u, "Theme render produced a flat image for " << theme.display_name);
    }
}
