#include <boost/test/unit_test.hpp>

#include "cli/solve_cli.h"
#include "app/app_settings.h"
#include "document/document_json.h"
#include "main_window.h"
#include "solver/solution_store.h"
#include "solver/solver_session.h"
#include "solver_state.h"
#include "spot_document.h"
#include "study/study_workflow.h"
#include "theme/theme_registry.h"
#include "theme/theme_styles.h"
#include "viewmodels/range_view_model.h"
#include "viewmodels/spot_view_model.h"
#include "viewmodels/strategy_view_model.h"
#include "widgets/range_editor.h"
#include "widgets/spot_builder.h"
#include "widgets/strategy_explorer.h"

#include <QAction>
#include <QApplication>
#include <QComboBox>
#include <QDialog>
#include <QFile>
#include <QFontDatabase>
#include <QIcon>
#include <QImage>
#include <QImageReader>
#include <QLabel>
#include <QListWidget>
#include <QPainter>
#include <QPlainTextEdit>
#include <QPushButton>
#include <QSpinBox>
#include <QTableWidget>
#include <QTemporaryDir>
#include <QTimer>
#include <QTreeWidget>
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

    [[nodiscard]] zeta::holdem::ui::spot sample_three_way_river_spot()
    {
        zeta::holdem::ui::spot spot;
        spot.players = {"BTN", "BB", "CO"};
        spot.street = "river";
        spot.board = {"2s", "3h", "4d", "5c", "9d"};
        spot.ranges = {"AsKs", "QhQd", "JcTc"};
        spot.gross_pot = 150.0;
        spot.rake = 0.0;
        spot.contributions = {50.0, 50.0, 50.0};
        spot.stacks = {200.0, 200.0, 200.0};
        spot.bet_fraction = 0.5;
        spot.max_history = 8;
        spot.public_state_id = 11;
        spot.root_actor = 0;
        spot.hero_seat = 0;
        spot.samples_per_combo = 8;
        return spot;
    }

    [[nodiscard]] zeta::holdem::cli::solve_artifact sample_strategy_artifact()
    {
        zeta::holdem::cli::solve_artifact artifact;
        artifact.players = {"BTN", "BB"};
        artifact.street = "river";
        artifact.board = {"2s", "3d", "4c", "5h", "6s"};
        artifact.hero_seat = 0;
        artifact.solver.algorithm = "cfr+";
        artifact.solver.iterations = 25;
        artifact.solver.timestamp = "2026-08-03T20:00:00Z";
        artifact.solver.git_revision = "abc1234";
        artifact.strategy = {
            zeta::holdem::cli::hand_strategy{
                .hand = "AhAd",
                .strategy = {
                    zeta::holdem::cli::action_strategy{.action = "check", .frequency = 0.75},
                    zeta::holdem::cli::action_strategy{.action = "bet_50", .frequency = 0.25}
                },
                .ev = 2.0
            },
            zeta::holdem::cli::hand_strategy{
                .hand = "AcAs",
                .strategy = {
                    zeta::holdem::cli::action_strategy{.action = "check", .frequency = 0.25},
                    zeta::holdem::cli::action_strategy{.action = "bet_50", .frequency = 0.75}
                },
                .ev = 4.0
            },
            zeta::holdem::cli::hand_strategy{
                .hand = "KdKh",
                .strategy = {
                    zeta::holdem::cli::action_strategy{.action = "fold", .frequency = 1.0}
                },
                .ev = -1.0
            }
        };
        return artifact;
    }

    [[nodiscard]] zeta::holdem::ui::spot sample_strategy_spot()
    {
        auto spot = sample_heads_up_spot();
        spot.street = "river";
        spot.board = {"2s", "3d", "4c", "5h", "6s"};
        spot.ranges = {"AhAd:0.5, AcAs, KdKh:0.25", "QcQh"};
        spot.root_actor = 1;
        spot.hero_seat = 0;
        return spot;
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
        colors.insert(tokens.document_selection);
        colors.insert(tokens.active_surface);
        colors.insert(tokens.button_text);
        colors.insert(tokens.destructive_text);
        for (const auto& color : tokens.range_heat) {
            colors.insert(color);
        }
    }

    [[nodiscard]] bool style_rule_contains(
        const std::string& sheet,
        const std::string_view selector,
        const std::string_view declaration)
    {
        const auto selector_pos = sheet.find(std::string{selector} + " {");
        if (selector_pos == std::string::npos) {
            return false;
        }
        const auto rule_end = sheet.find("\n}", selector_pos);
        if (rule_end == std::string::npos) {
            return false;
        }
        return sheet.substr(selector_pos, rule_end - selector_pos).find(declaration) != std::string::npos;
    }

    [[nodiscard]] bool has_issue(
        const std::vector<zeta::holdem::ui::viewmodels::spot_validation_issue>& issues,
        const std::string& field)
    {
        return std::ranges::any_of(issues, [&field](const auto& issue) {
            return issue.field == field;
        });
    }

    [[nodiscard]] const zeta::holdem::ui::viewmodels::strategy_matrix_cell* strategy_cell(
        const zeta::holdem::ui::viewmodels::strategy_view_model& model,
        const std::string& hand_class)
    {
        const auto found = std::ranges::find_if(model.matrix, [&hand_class](const auto& cell) {
            return cell.hand_class == hand_class;
        });
        return found == model.matrix.end() ? nullptr : &*found;
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

BOOST_AUTO_TEST_CASE(holdem_ui_document_metadata_persists_tags_and_study_notes) {
    auto document = zeta::holdem::ui::spot_document::create_new();
    document.replace_spot(sample_heads_up_spot());
    auto metadata = document.metadata();
    metadata.study_notes = "Turn barrel candidate; compare low rake run.";
    metadata.tags = {"river", "multiway", "review"};
    document.update_metadata(metadata);

    const auto reopened = zeta::holdem::ui::spot_document::parse_json(document.serialize_json());

    BOOST_REQUIRE(reopened.has_value());
    BOOST_CHECK_EQUAL(reopened->metadata().study_notes, metadata.study_notes);
    BOOST_REQUIRE_EQUAL(reopened->metadata().tags.size(), 3u);
    BOOST_CHECK_EQUAL(reopened->metadata().tags[1], "multiway");
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

BOOST_AUTO_TEST_CASE(holdem_ui_solver_session_completes_three_way_spot) {
    zeta::holdem::ui::solver::solver_session session{
        zeta::holdem::ui::solver::solver_session_request{
            .spot_snapshot = sample_three_way_river_spot(),
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
    BOOST_CHECK_EQUAL(result.artifact->players.size(), 3u);
    BOOST_CHECK_EQUAL(result.artifact->hero_seat, 0u);
    BOOST_CHECK(!result.artifact->strategy.empty());
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

    auto resized = zeta::holdem::ui::viewmodels::resize_player_count(std::move(spot), 7);

    BOOST_REQUIRE_EQUAL(resized.players.size(), 7u);
    BOOST_CHECK_EQUAL(resized.ranges.size(), 7u);
    BOOST_CHECK_EQUAL(resized.stacks.size(), 7u);
    BOOST_CHECK_EQUAL(resized.contributions.size(), 7u);
    BOOST_CHECK_EQUAL(resized.players[0], "Hero");
    BOOST_CHECK_EQUAL(resized.ranges[0], "AhAd");
    BOOST_CHECK_EQUAL(resized.stacks[0], 250.0);
    BOOST_CHECK_EQUAL(resized.contributions[0], 75.0);
    BOOST_CHECK_EQUAL(resized.ranges[6], "AA");
    BOOST_CHECK_EQUAL(resized.stacks[6], 100.0);
    BOOST_CHECK_EQUAL(resized.contributions[6], 0.0);

    resized.root_actor = 6;
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

BOOST_AUTO_TEST_CASE(holdem_ui_structured_spot_templates_roundtrip_to_valid_json_for_two_to_seven_players) {
    for (const auto kind : {
             zeta::holdem::ui::viewmodels::spot_template_kind::heads_up_river,
             zeta::holdem::ui::viewmodels::spot_template_kind::three_way_flop,
             zeta::holdem::ui::viewmodels::spot_template_kind::four_way_turn,
             zeta::holdem::ui::viewmodels::spot_template_kind::five_way_turn,
             zeta::holdem::ui::viewmodels::spot_template_kind::six_way_turn,
             zeta::holdem::ui::viewmodels::spot_template_kind::seven_way_turn}) {
        const auto templated = zeta::holdem::ui::viewmodels::make_template_spot(kind);
        BOOST_CHECK(zeta::holdem::ui::viewmodels::validate_structured_spot(templated).empty());
        const auto parsed = zeta::holdem::cli::parse_spot_json(zeta::holdem::cli::serialize_spot_json(templated));
        BOOST_REQUIRE(parsed.has_value());
        BOOST_CHECK_EQUAL(parsed->players.size(), templated.players.size());
        BOOST_CHECK_EQUAL(parsed->board.size(), templated.board.size());
    }

    auto resizable = zeta::holdem::ui::viewmodels::make_template_spot(
        zeta::holdem::ui::viewmodels::spot_template_kind::heads_up_river);
    for (std::size_t count = 2; count <= 7; ++count) {
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

    const auto seven_way = zeta::holdem::ui::viewmodels::make_template_spot(
        zeta::holdem::ui::viewmodels::spot_template_kind::seven_way_turn);
    BOOST_REQUIRE_EQUAL(seven_way.players.size(), 7u);
    BOOST_CHECK_EQUAL(seven_way.players[3], "UTG");
    BOOST_CHECK_EQUAL(seven_way.players[4], "LJ");
    BOOST_CHECK_EQUAL(seven_way.players[5], "HJ");
    BOOST_CHECK_EQUAL(seven_way.players[6], "CO");
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
    auto* first_board_card = builder.findChild<QComboBox*>("boardCard0");
    auto* second_board_card = builder.findChild<QComboBox*>("boardCard1");

    BOOST_REQUIRE(street != nullptr);
    BOOST_REQUIRE(player_count != nullptr);
    BOOST_REQUIRE(root_actor != nullptr);
    BOOST_REQUIRE(hero_seat != nullptr);
    BOOST_REQUIRE(first_board_card != nullptr);
    BOOST_REQUIRE(second_board_card != nullptr);
    BOOST_CHECK_EQUAL(street->currentText().toStdString(), "turn");
    BOOST_CHECK_EQUAL(player_count->value(), 4);
    BOOST_CHECK_EQUAL(root_actor->currentIndex(), static_cast<int>(edited.root_actor));
    BOOST_CHECK_EQUAL(hero_seat->currentIndex(), static_cast<int>(edited.hero_seat));
    BOOST_CHECK_EQUAL(first_board_card->currentText().toStdString(), "A\u2660");
    BOOST_CHECK_EQUAL(first_board_card->currentData().toString().toStdString(), "As");
    BOOST_CHECK_EQUAL(first_board_card->property("cardSuitTone").toString().toStdString(), "amber");
    BOOST_CHECK_EQUAL(second_board_card->currentText().toStdString(), "K\u2666");
    BOOST_CHECK_EQUAL(second_board_card->currentData().toString().toStdString(), "Kd");
    BOOST_CHECK_EQUAL(second_board_card->property("cardSuitTone").toString().toStdString(), "red");
}

BOOST_AUTO_TEST_CASE(holdem_ui_spot_builder_keeps_seats_table_with_header) {
    auto& app = qt_app();
    auto initial = zeta::holdem::ui::viewmodels::make_template_spot(
        zeta::holdem::ui::viewmodels::spot_template_kind::three_way_flop);
    const auto metrics = zeta::holdem::ui::theme::metrics_for_density(zeta::holdem::ui::theme::density_mode::comfortable);
    zeta::holdem::ui::widgets::spot_builder builder{
        initial,
        metrics,
        [](zeta::holdem::ui::spot) {},
        {},
        nullptr};
    builder.resize(900, 700);
    builder.show();
    app.processEvents();

    QLabel* seats_title = nullptr;
    const auto titles = builder.findChildren<QLabel*>("panelTitle");
    for (auto* title : titles) {
        if (title->text() == QStringLiteral("Seats")) {
            seats_title = title;
            break;
        }
    }
    auto* seat_table = builder.findChild<QTableWidget*>("seatTableEditor");

    BOOST_REQUIRE(seats_title != nullptr);
    BOOST_REQUIRE(seat_table != nullptr);
    BOOST_CHECK_LE(seats_title->height(), seats_title->sizeHint().height() + 2);
    BOOST_CHECK_LE(seat_table->geometry().top(), seats_title->geometry().bottom() + metrics.panel_spacing + 2);
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

BOOST_AUTO_TEST_CASE(holdem_ui_strategy_view_model_aggregates_matrix_table_cards_and_metadata_consistently) {
    const auto model = zeta::holdem::ui::viewmodels::make_strategy_view_model(
        sample_strategy_spot(),
        sample_strategy_artifact());

    const auto* aa = strategy_cell(model, "AA");
    BOOST_REQUIRE(aa != nullptr);
    BOOST_CHECK(aa->available);
    BOOST_REQUIRE_EQUAL(aa->exact_combos.size(), 2u);
    BOOST_CHECK_EQUAL(aa->best_action, "bet_50");
    BOOST_CHECK_CLOSE(aa->ev, 3.3333333333, 0.001);
    BOOST_CHECK_CLOSE(aa->range_weight, 1.5, 0.001);
    BOOST_REQUIRE_EQUAL(aa->actions.size(), 2u);
    BOOST_CHECK_EQUAL(aa->actions[0].action, "bet_50");
    BOOST_CHECK_CLOSE(aa->actions[0].frequency, 0.5833333333, 0.001);

    const auto* kk = strategy_cell(model, "KK");
    BOOST_REQUIRE(kk != nullptr);
    BOOST_CHECK(kk->available);
    BOOST_CHECK_EQUAL(kk->best_action, "fold");
    BOOST_CHECK_CLOSE(kk->ev, -1.0, 0.001);

    BOOST_REQUIRE_EQUAL(model.hands.size(), 3u);
    BOOST_CHECK_CLOSE(model.average_ev, 2.7142857142, 0.001);
    BOOST_REQUIRE_EQUAL(model.action_cards.size(), 3u);
    BOOST_CHECK_EQUAL(model.action_cards[0].action, "bet_50");
    BOOST_CHECK_CLOSE(model.action_cards[0].frequency, 0.5, 0.001);
    BOOST_CHECK_GT(model.mix_indicator, 0.0);

    BOOST_CHECK_EQUAL(model.metadata.algorithm, "cfr+");
    BOOST_CHECK_EQUAL(model.metadata.iterations, 25u);
    BOOST_CHECK_EQUAL(model.metadata.player_count, 2u);
    BOOST_CHECK_EQUAL(model.metadata.hero_label, "BTN");
    BOOST_CHECK_EQUAL(model.metadata.root_actor_label, "BB");
    BOOST_REQUIRE_EQUAL(model.metadata.seat_ranges.size(), 2u);
    BOOST_CHECK(model.metadata.seat_ranges[0].find("AhAd:0.5") != std::string::npos);
}

BOOST_AUTO_TEST_CASE(holdem_ui_strategy_view_model_filters_hands_and_formats_ev) {
    const auto model = zeta::holdem::ui::viewmodels::make_strategy_view_model(
        sample_strategy_spot(),
        sample_strategy_artifact());

    const auto bet_rows = zeta::holdem::ui::viewmodels::filtered_strategy_hands(
        model,
        zeta::holdem::ui::viewmodels::strategy_action_filter::bet_raise);
    BOOST_REQUIRE_EQUAL(bet_rows.size(), 2u);
    BOOST_CHECK(std::ranges::all_of(bet_rows, [](const auto& row) {
        return row.hand_class == "AA";
    }));

    const auto fold_rows = zeta::holdem::ui::viewmodels::filtered_strategy_hands(
        model,
        zeta::holdem::ui::viewmodels::strategy_action_filter::fold);
    BOOST_REQUIRE_EQUAL(fold_rows.size(), 1u);
    BOOST_CHECK_EQUAL(fold_rows[0].hand_class, "KK");

    const auto* aa = strategy_cell(model, "AA");
    const auto* kk = strategy_cell(model, "KK");
    BOOST_REQUIRE(aa != nullptr);
    BOOST_REQUIRE(kk != nullptr);
    BOOST_CHECK(zeta::holdem::ui::viewmodels::strategy_cell_matches_filter(
        *aa,
        zeta::holdem::ui::viewmodels::strategy_action_filter::check_call));
    BOOST_CHECK(!zeta::holdem::ui::viewmodels::strategy_cell_matches_filter(
        *kk,
        zeta::holdem::ui::viewmodels::strategy_action_filter::bet_raise));

    BOOST_CHECK_EQUAL(zeta::holdem::ui::viewmodels::format_strategy_ev(1.25), "+1.25");
    BOOST_CHECK_EQUAL(zeta::holdem::ui::viewmodels::format_strategy_ev(-0.5), "-0.50");
    BOOST_CHECK_EQUAL(zeta::holdem::ui::viewmodels::format_strategy_ev(0.0), "0.00");
    BOOST_CHECK_EQUAL(zeta::holdem::ui::viewmodels::format_strategy_percent(0.625), "62.5%");
}

BOOST_AUTO_TEST_CASE(holdem_ui_solution_store_migrates_legacy_artifact_to_root_only_fallback) {
    const auto spot_json = zeta::holdem::cli::serialize_spot_json(sample_strategy_spot());
    const auto artifact_json = zeta::holdem::cli::serialize_artifact_json(sample_strategy_artifact());
    const auto document_json = std::string{R"({
  "document_schema_version": 1,
  "metadata": {},
  "spot": )"} + spot_json + R"(,
  "artifact": )" + artifact_json + R"(,
  "recent_history": []
})";

    const auto parsed = zeta::holdem::ui::spot_document::parse_json(document_json);

    BOOST_REQUIRE(parsed.has_value());
    BOOST_REQUIRE(parsed->artifact().has_value());
    BOOST_REQUIRE(parsed->solution().has_value());
    BOOST_CHECK(parsed->solution()->compatibility_mode == zeta::holdem::ui::solver::solution_compatibility_mode::root_only_artifact);
    const auto* root = zeta::holdem::ui::solver::root_solution_node(*parsed->solution());
    BOOST_REQUIRE(root != nullptr);
    BOOST_CHECK_EQUAL(root->node_id, "root");
    BOOST_CHECK_EQUAL(root->acting_seat, sample_strategy_spot().root_actor);
    BOOST_REQUIRE_EQUAL(root->average_strategy.size(), 3u);
    BOOST_CHECK(std::ranges::any_of(root->average_strategy, [](const auto& action) {
        return action.action == "check";
    }));
    BOOST_CHECK(!parsed->solution()->diagnostics.empty());
}

BOOST_AUTO_TEST_CASE(holdem_ui_solution_store_saves_reopens_action_tree_nodes_and_root_frequencies) {
    auto spot = sample_strategy_spot();
    spot.max_history = 2;
    const auto artifact = sample_strategy_artifact();
    auto solution = zeta::holdem::ui::solver::make_action_tree_solution_store(spot, artifact);

    BOOST_CHECK(solution.compatibility_mode == zeta::holdem::ui::solver::solution_compatibility_mode::action_tree);
    const auto* root = zeta::holdem::ui::solver::root_solution_node(solution);
    BOOST_REQUIRE(root != nullptr);
    BOOST_CHECK_EQUAL(root->acting_seat, spot.root_actor);
    BOOST_REQUIRE(!root->children.empty());
    BOOST_REQUIRE(!root->average_strategy.empty());
    BOOST_CHECK_CLOSE(root->average_strategy.front().frequency, 1.0 / 3.0, 0.001);

    auto document = zeta::holdem::ui::spot_document::create_new();
    document.replace_spot(spot);
    document.replace_artifact(artifact);
    document.replace_solution(solution);

    const auto reopened = zeta::holdem::ui::spot_document::parse_json(document.serialize_json());
    BOOST_REQUIRE(reopened.has_value());
    BOOST_REQUIRE(reopened->solution().has_value());
    BOOST_CHECK(reopened->solution()->compatibility_mode == zeta::holdem::ui::solver::solution_compatibility_mode::action_tree);
    const auto* reopened_root = zeta::holdem::ui::solver::root_solution_node(*reopened->solution());
    BOOST_REQUIRE(reopened_root != nullptr);
    BOOST_REQUIRE(!reopened_root->children.empty());
    const auto* child = zeta::holdem::ui::solver::find_solution_node(*reopened->solution(), reopened_root->children.front());
    BOOST_REQUIRE(child != nullptr);
    BOOST_REQUIRE_EQUAL(child->path.size(), 1u);
    BOOST_CHECK_EQUAL(child->table_state.commitments.size(), spot.players.size());
}

BOOST_AUTO_TEST_CASE(holdem_ui_strategy_explorer_widget_renders_artifact_and_action_filter) {
    auto& app = qt_app();

    zeta::holdem::ui::widgets::strategy_explorer explorer{
        sample_strategy_spot(),
        sample_strategy_artifact(),
        zeta::holdem::ui::theme::metrics_for_density(zeta::holdem::ui::theme::density_mode::comfortable),
        nullptr};
    explorer.resize(900, 700);
    explorer.show();
    app.processEvents();

    auto* filter = explorer.findChild<QComboBox*>("strategyActionFilter");
    auto* hand_table = explorer.findChild<QTableWidget*>("strategyHandTable");
    auto* detail_table = explorer.findChild<QTableWidget*>("strategyDetailTable");
    auto* metadata = explorer.findChild<QLabel*>("artifactMetadataSummary");

    BOOST_REQUIRE(filter != nullptr);
    BOOST_REQUIRE(hand_table != nullptr);
    BOOST_REQUIRE(detail_table != nullptr);
    BOOST_REQUIRE(metadata != nullptr);
    BOOST_CHECK(metadata->text().contains(QStringLiteral("players 2")));
    BOOST_CHECK_EQUAL(hand_table->rowCount(), 3);

    filter->setCurrentIndex(1);
    app.processEvents();

    BOOST_CHECK_EQUAL(hand_table->rowCount(), 1);
    BOOST_CHECK(detail_table->rowCount() <= 1);
}

BOOST_AUTO_TEST_CASE(holdem_ui_strategy_explorer_widget_renders_solution_action_tree_navigation) {
    auto& app = qt_app();
    auto spot = sample_strategy_spot();
    spot.max_history = 2;
    auto artifact = sample_strategy_artifact();
    auto solution = zeta::holdem::ui::solver::make_action_tree_solution_store(spot, artifact);

    zeta::holdem::ui::widgets::strategy_explorer explorer{
        spot,
        artifact,
        solution,
        zeta::holdem::ui::theme::metrics_for_density(zeta::holdem::ui::theme::density_mode::comfortable),
        nullptr};
    explorer.resize(1000, 760);
    explorer.show();
    app.processEvents();

    auto* tree = explorer.findChild<QTreeWidget*>("solutionActionTree");
    auto* node_actions = explorer.findChild<QTableWidget*>("solutionNodeActionTable");
    auto* breadcrumb = explorer.findChild<QLabel*>("solutionNodeBreadcrumb");
    auto* state = explorer.findChild<QLabel*>("solutionNodeState");
    auto* hand_table = explorer.findChild<QTableWidget*>("strategyHandTable");

    BOOST_REQUIRE(tree != nullptr);
    BOOST_REQUIRE(node_actions != nullptr);
    BOOST_REQUIRE(breadcrumb != nullptr);
    BOOST_REQUIRE(state != nullptr);
    BOOST_REQUIRE(hand_table != nullptr);
    BOOST_REQUIRE_EQUAL(tree->topLevelItemCount(), 1);
    BOOST_REQUIRE(tree->topLevelItem(0)->childCount() > 0);
    BOOST_CHECK_EQUAL(node_actions->rowCount(), 3);
    BOOST_CHECK(state->text().contains(QStringLiteral("BB")));

    tree->setCurrentItem(tree->topLevelItem(0)->child(0));
    app.processEvents();

    BOOST_CHECK(breadcrumb->text().contains(QStringLiteral("Root /")));
    BOOST_CHECK_EQUAL(hand_table->rowCount(), 0);
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

BOOST_AUTO_TEST_CASE(holdem_ui_zeta_logo_resource_loads_as_icon) {
    auto& app = qt_app();
    (void) app;

    QIcon icon{QStringLiteral(":/icons/zeta-logo.svg")};
    BOOST_CHECK(!icon.isNull());
    BOOST_CHECK(!icon.pixmap(QSize{64, 64}).isNull());

    zeta::holdem::ui::main_window window;
    auto* logo = window.findChild<QLabel*>("appLogo");

    BOOST_REQUIRE(logo != nullptr);
    BOOST_CHECK(!logo->pixmap().isNull());
    BOOST_CHECK(!window.windowIcon().isNull());
}

BOOST_AUTO_TEST_CASE(holdem_ui_settings_persist_theme_density_recent_files_and_splitters) {
    QTemporaryDir dir;
    BOOST_REQUIRE(dir.isValid());
    const auto settings_path = dir.filePath(QStringLiteral("holdem-ui.ini"));

    {
        zeta::holdem::ui::app::app_settings settings{settings_path};
        settings.set_active_theme(zeta::holdem::ui::theme::theme_id::high_contrast);
        settings.set_density(zeta::holdem::ui::theme::density_mode::compact);
        settings.set_solver_iterations(250);
        settings.set_solver_progress_batch_iterations(25);
        settings.set_solver_worker_threads(4);
        settings.set_shell_splitter_sizes(QList<int>{180, 820});
        settings.set_workspace_splitter_sizes(QList<int>{620, 360});
        settings.add_recent_file(QStringLiteral("C:/tmp/a.json"));
        settings.add_recent_file(QStringLiteral("C:/tmp/b.json"));
        settings.add_recent_file(QStringLiteral("C:/tmp/a.json"));
        settings.set_file_pinned(QStringLiteral("C:/tmp/b.json"), true);
        settings.set_file_pinned(QStringLiteral("C:/tmp/c.json"), true);
        settings.set_file_pinned(QStringLiteral("C:/tmp/b.json"), false);
        settings.sync();
    }

    zeta::holdem::ui::app::app_settings settings{settings_path};
    BOOST_CHECK(settings.active_theme() == zeta::holdem::ui::theme::theme_id::high_contrast);
    BOOST_CHECK(settings.density() == zeta::holdem::ui::theme::density_mode::compact);
    BOOST_CHECK_EQUAL(settings.solver_iterations(), 250);
    BOOST_CHECK_EQUAL(settings.solver_progress_batch_iterations(), 25);
    BOOST_CHECK_EQUAL(settings.solver_worker_threads(), 4);
    BOOST_REQUIRE_EQUAL(settings.shell_splitter_sizes().size(), 2);
    BOOST_CHECK_EQUAL(settings.shell_splitter_sizes()[0], 180);
    BOOST_REQUIRE_EQUAL(settings.workspace_splitter_sizes().size(), 2);
    BOOST_CHECK_EQUAL(settings.workspace_splitter_sizes()[1], 360);
    BOOST_REQUIRE_EQUAL(settings.recent_files().size(), 2);
    BOOST_CHECK_EQUAL(settings.recent_files()[0].toStdString(), "C:/tmp/a.json");
    BOOST_CHECK_EQUAL(settings.recent_files()[1].toStdString(), "C:/tmp/b.json");
    BOOST_REQUIRE_EQUAL(settings.pinned_files().size(), 1);
    BOOST_CHECK_EQUAL(settings.pinned_files()[0].toStdString(), "C:/tmp/c.json");
}

BOOST_AUTO_TEST_CASE(holdem_ui_theme_styles_use_registered_tokens_only) {
    const std::regex color_pattern{"#[0-9A-Fa-f]{6}([0-9A-Fa-f]{2})?"};

    for (const auto& theme : zeta::holdem::ui::theme::registered_themes()) {
        std::set<std::string> token_colors;
        insert_token_colors(theme.tokens, token_colors);

        const auto sheet = zeta::holdem::ui::theme::style_sheet(theme, zeta::holdem::ui::theme::density_mode::comfortable).toStdString();
        BOOST_CHECK(!sheet.empty());
        BOOST_CHECK_EQUAL(sheet.find('%'), std::string::npos);
        BOOST_CHECK_EQUAL(sheet.find("pxpx"), std::string::npos);
        for (std::sregex_iterator it{sheet.begin(), sheet.end(), color_pattern}, end; it != end; ++it) {
            BOOST_CHECK_MESSAGE(token_colors.contains(it->str()), "Theme stylesheet used non-token color " << it->str());
        }

        if (theme.id == zeta::holdem::ui::theme::theme_id::dark_pro) {
            BOOST_CHECK(sheet.find("font-size: 14px;") != std::string::npos);
            BOOST_CHECK(style_rule_contains(sheet, "QPushButton#callButton", "background: " + theme.tokens.action_positive));
            BOOST_CHECK(style_rule_contains(sheet, "QPushButton#foldButton", "background: " + theme.tokens.action_negative));
            BOOST_CHECK(style_rule_contains(sheet, "QPushButton#rangeCellHeat4", "background: " + theme.tokens.range_heat[3]));
            BOOST_CHECK(style_rule_contains(sheet, "QListWidget#documentRailList::item:selected", "background: " + theme.tokens.document_selection));
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

BOOST_AUTO_TEST_CASE(holdem_ui_study_filtering_prioritizes_pinned_recent_and_tags) {
    const std::vector<zeta::holdem::ui::study::study_record> studies{
        zeta::holdem::ui::study::study_record{
            .path = "C:/study/river-a.json",
            .title = "River A",
            .tags = {"river", "btn"},
            .pinned = false,
            .updated_utc = "2026-08-03T20:00:00Z"
        },
        zeta::holdem::ui::study::study_record{
            .path = "C:/study/flop-pinned.json",
            .title = "Flop pinned",
            .tags = {"flop", "multiway"},
            .pinned = true,
            .updated_utc = "2026-08-01T20:00:00Z"
        },
        zeta::holdem::ui::study::study_record{
            .path = "C:/study/flop-new.json",
            .title = "Flop new",
            .tags = {"flop"},
            .pinned = false,
            .updated_utc = "2026-08-04T20:00:00Z"
        }
    };

    const auto filtered = zeta::holdem::ui::study::filter_studies(studies, "flop", std::string_view{"flop"});

    BOOST_REQUIRE_EQUAL(filtered.size(), 2u);
    BOOST_CHECK(filtered[0].pinned);
    BOOST_CHECK_EQUAL(filtered[0].title, "Flop pinned");
    BOOST_CHECK_EQUAL(filtered[1].title, "Flop new");
}

BOOST_AUTO_TEST_CASE(holdem_ui_exports_strategy_and_hand_tables_as_stable_csv) {
    const auto artifact = sample_strategy_artifact();
    const auto model = zeta::holdem::ui::viewmodels::make_strategy_view_model(
        sample_strategy_spot(),
        artifact);

    const auto strategy_csv = zeta::holdem::ui::study::export_strategy_csv(artifact);
    const auto hand_csv = zeta::holdem::ui::study::export_hand_table_csv(model);

    BOOST_CHECK(strategy_csv.starts_with("hand,ev,bet_50,check,fold"));
    BOOST_CHECK(strategy_csv.find("AhAd,2.000000,0.250000,0.750000,0.000000") != std::string::npos);
    BOOST_CHECK(hand_csv.starts_with("hand,hand_class,best_action,actions,ev,range_weight,live"));
    BOOST_CHECK(hand_csv.find(",AA,bet_50") != std::string::npos);
}

BOOST_AUTO_TEST_CASE(holdem_ui_run_comparison_reports_deltas_and_rejects_incompatible_spots) {
    auto before = sample_strategy_artifact();
    auto after = before;
    after.solver.iterations = 50;
    after.strategy[0].strategy = {
        zeta::holdem::cli::action_strategy{.action = "check", .frequency = 0.10},
        zeta::holdem::cli::action_strategy{.action = "bet_50", .frequency = 0.90}
    };
    after.strategy[0].ev = 3.0;
    auto spot = sample_strategy_spot();

    auto comparison = zeta::holdem::ui::study::compare_strategy_runs(spot, before, spot, after);

    BOOST_REQUIRE(comparison.has_value());
    BOOST_CHECK_EQUAL(comparison->changed_best_action_count, 1u);
    BOOST_REQUIRE(!comparison->settings_differences.empty());
    BOOST_CHECK_EQUAL(comparison->settings_differences[0], "iterations");
    BOOST_CHECK(std::ranges::any_of(comparison->action_deltas, [](const auto& delta) {
        return delta.action == "bet_50" && delta.delta > 0.0;
    }));
    BOOST_CHECK(std::ranges::any_of(comparison->ev_deltas, [](const auto& delta) {
        return delta.hand == "AhAd" && delta.delta > 0.0;
    }));

    auto incompatible = spot;
    incompatible.board[0] = "Ac";
    BOOST_CHECK(!zeta::holdem::ui::study::compare_strategy_runs(spot, before, incompatible, after).has_value());
}

BOOST_AUTO_TEST_CASE(holdem_ui_share_summary_and_screenshot_capture_current_widget) {
    auto& app = qt_app();
    const auto spot = sample_strategy_spot();
    const auto artifact = sample_strategy_artifact();
    const auto model = zeta::holdem::ui::viewmodels::make_strategy_view_model(spot, artifact);

    const auto summary = zeta::holdem::ui::study::make_share_summary(spot, artifact, model);

    BOOST_CHECK(summary.find("Players: BTN, BB") != std::string::npos);
    BOOST_CHECK(summary.find("Board: 2s 3d 4c 5h 6s") != std::string::npos);
    BOOST_CHECK(summary.find("Iterations: 25") != std::string::npos);
    BOOST_CHECK(summary.find("Top actions:") != std::string::npos);

    QLabel label{QStringLiteral("Current strategy view")};
    label.resize(240, 60);
    label.show();
    app.processEvents();
    const auto image = zeta::holdem::ui::study::capture_widget_image(label);
    BOOST_CHECK_EQUAL(image.width(), 240);
    BOOST_CHECK_EQUAL(image.height(), 60);
}

BOOST_AUTO_TEST_CASE(holdem_ui_resources_expose_toolbar_icons_and_ddin_font) {
    auto& app = qt_app();
    (void) app;

    const auto image_formats = QImageReader::supportedImageFormats();
    const bool svg_supported = std::ranges::contains(image_formats, QByteArray{"svg"});

    for (const auto& icon : {
             QStringLiteral(":/icons/file-plus.svg"),
             QStringLiteral(":/icons/folder-open.svg"),
             QStringLiteral(":/icons/save.svg"),
             QStringLiteral(":/icons/check-circle.svg"),
             QStringLiteral(":/icons/play.svg"),
             QStringLiteral(":/icons/square.svg"),
             QStringLiteral(":/icons/settings.svg")}) {
        BOOST_CHECK(QFile::exists(icon));
        if (svg_supported) {
            BOOST_CHECK(!QIcon{icon}.pixmap(QSize{22, 22}).isNull());
        }
    }

    for (const auto& font : {
             QStringLiteral(":/fonts/D-DIN.ttf"),
             QStringLiteral(":/fonts/D-DIN-Bold.ttf"),
             QStringLiteral(":/fonts/D-DIN-Italic.ttf"),
             QStringLiteral(":/fonts/D-DINCondensed.ttf"),
             QStringLiteral(":/fonts/D-DINCondensed-Bold.ttf"),
             QStringLiteral(":/fonts/D-DINExp.ttf"),
             QStringLiteral(":/fonts/D-DINExp-Bold.ttf"),
             QStringLiteral(":/fonts/D-DINExp-Italic.ttf")}) {
        const int font_id = QFontDatabase::addApplicationFont(font);
        BOOST_REQUIRE(font_id >= 0);
        BOOST_CHECK(!QFontDatabase::applicationFontFamilies(font_id).empty());
    }
}

BOOST_AUTO_TEST_CASE(holdem_ui_main_window_launch_smoke_has_command_shell) {
    auto& app = qt_app();
    zeta::holdem::ui::main_window window;
    window.resize(900, 640);
    window.show();
    app.processEvents();

    BOOST_CHECK(window.findChild<QTabWidget*>() != nullptr);
    BOOST_CHECK(window.findChild<QListWidget*>("documentRailList") != nullptr);
    BOOST_CHECK(window.findChild<QPlainTextEdit*>("solveConsole") != nullptr);
}

BOOST_AUTO_TEST_CASE(holdem_ui_configuration_dialog_allows_worker_thread_edits) {
    auto& app = qt_app();
    zeta::holdem::ui::main_window window;
    window.show();
    app.processEvents();

    QAction* configuration = nullptr;
    for (auto* action : window.findChildren<QAction*>()) {
        const auto text = QString{action->text()}.remove('&');
        if (text == QStringLiteral("Configuration")) {
            configuration = action;
            break;
        }
    }
    BOOST_REQUIRE(configuration != nullptr);

    bool inspected = false;
    QTimer::singleShot(0, [&inspected] {
        auto* dialog = qobject_cast<QDialog*>(QApplication::activeModalWidget());
        if (dialog == nullptr) {
            return;
        }
        auto* threads = dialog->findChild<QSpinBox*>(QStringLiteral("workerThreadsSpinBox"));
        inspected = threads != nullptr && threads->isEnabled() && threads->maximum() >= threads->minimum();
        if (threads != nullptr) {
            threads->setValue(threads->maximum());
        }
        dialog->accept();
    });

    configuration->trigger();

    BOOST_CHECK(inspected);
}
