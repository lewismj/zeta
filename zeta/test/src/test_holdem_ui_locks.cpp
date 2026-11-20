#include <boost/test/unit_test.hpp>

#include "cli/solve_cli.h"
#include "document_workspace_widget.h"
#include "spot_document.h"
#include "theme/theme_registry.h"
#include "viewmodels/lock_view_model.h"
#include "widgets/node_lock_dialog.h"
#include "widgets/spot_builder.h"
#include "widgets/strategy_explorer.h"

#include <QApplication>
#include <QCheckBox>
#include <QDoubleSpinBox>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QTableWidget>
#include <QTreeWidget>

#include <optional>
#include <string>
#include <vector>

// Node-lock authoring in the hold'em UI: the lock view model, the lock editor, the
// Strategy Explorer's lock controls and the Spot Builder's lock list.

namespace {

    namespace cli = zeta::holdem::cli;
    namespace ui = zeta::holdem::ui;
    namespace vm = zeta::holdem::ui::viewmodels;

    QApplication& qt_app()
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
  "hero_seat": "BTN",
  "locks": [{"path": ["bet_75"], "actions": {"fold": 1.0}}]
})";

    struct solved_spot {
        ui::spot spot;
        ui::solve_artifact artifact;
    };

    solved_spot solve_locked_river()
    {
        auto spot = cli::parse_spot_json(river_spot);
        BOOST_REQUIRE(spot.has_value());
        auto output = cli::solve_spot(*spot, 20);
        BOOST_REQUIRE_MESSAGE(output.has_value(), (output ? std::string{} : output.error().message));
        return solved_spot{*spot, std::move(output->artifact)};
    }

    QTreeWidgetItem* child_named(QTreeWidgetItem* parent, const QString& text)
    {
        for (int index = 0; index < parent->childCount(); ++index) {
            if (parent->child(index)->text(0) == text) {
                return parent->child(index);
            }
        }
        return nullptr;
    }

    ui::theme::density_metrics metrics()
    {
        return ui::theme::metrics_for_density(ui::theme::density_mode::comfortable);
    }

}

BOOST_AUTO_TEST_SUITE(holdem_ui_node_locks)

BOOST_AUTO_TEST_CASE(lock_view_model_formats_and_edits_locks) {
    const cli::node_lock lock{
        .path = {"check", "bet_75", "*"},
        .hands = {},
        .actions = {{"call", 0.7}, {"fold", 0.125}}
    };
    BOOST_CHECK_EQUAL(vm::format_lock_path(lock.path), "Check / Bet 75 / Any card");
    BOOST_CHECK_EQUAL(vm::format_lock_path({}), "Root");
    BOOST_CHECK_EQUAL(vm::format_lock_hands(lock), "All hands");
    BOOST_CHECK_EQUAL(vm::format_lock_actions(lock), "Call 70%, Fold 12.5%");

    BOOST_CHECK(vm::lock_path_matches(lock.path, {"check", "bet_75", "Kh"}));
    BOOST_CHECK(!vm::lock_path_matches(lock.path, {"check", "bet_75"}));
    BOOST_CHECK(!vm::lock_path_matches(lock.path, {"check", "check", "Kh"}));

    auto hand_lock = lock;
    hand_lock.hands = "AA";
    auto locks = vm::with_lock({lock}, hand_lock);
    BOOST_CHECK_EQUAL(locks.size(), 2u);
    auto replacement = lock;
    replacement.actions = {{"call", 1.0}};
    locks = vm::with_lock(locks, replacement);
    BOOST_REQUIRE_EQUAL(locks.size(), 2u);
    BOOST_CHECK_EQUAL(locks[0].actions.size(), 1u);
    BOOST_CHECK_EQUAL(vm::locks_at_node(locks, {"check", "bet_75", "2c"}).size(), 2u);
    BOOST_CHECK(vm::without_locks_at(locks, lock.path).empty());
    BOOST_CHECK_EQUAL(vm::without_locks_at(locks, {"check", "bet_75", "2c"}).size(), 2u);
}

BOOST_AUTO_TEST_CASE(lock_view_model_validates_editor_rows) {
    const std::vector<std::string> path{"bet_75"};
    const auto build = [&](std::vector<vm::lock_action_entry> entries, const std::string& hands = {}) {
        return vm::make_node_lock(path, hands, entries);
    };
    BOOST_CHECK(!build({{"fold", false, 10.0}, {"call", false, 90.0}}).has_value());
    BOOST_CHECK(build({{"fold", true, 60.0}, {"call", true, 50.0}, {"all_in", false, 0.0}}).error().find("more than 100%")
        != std::string::npos);
    BOOST_CHECK(build({{"fold", true, 30.0}, {"call", true, 50.0}}).error().find("add up to 100%") != std::string::npos);
    BOOST_CHECK(build({{"fold", true, 30.0}, {"call", false, 0.0}}, "AZ").error().find("Invalid hands") != std::string::npos);

    auto lock = build({{"fold", true, 30.0}, {"call", false, 0.0}, {"all_in", true, 10.0}}, "  AK  ");
    BOOST_REQUIRE(lock.has_value());
    BOOST_CHECK(lock->path == path);
    BOOST_CHECK_EQUAL(lock->hands, "AK");
    BOOST_REQUIRE_EQUAL(lock->actions.size(), 2u);
    BOOST_CHECK_EQUAL(lock->actions[0].action, "all_in");  // canonical order
    BOOST_CHECK_CLOSE(lock->actions[1].frequency, 0.3, 1e-9);
}

BOOST_AUTO_TEST_CASE(lock_dialog_builds_a_lock_from_its_rows) {
    qt_app();
    ui::widgets::node_lock_dialog dialog{
        {"bet_75"}, QStringLiteral("BB"), {"fold", "call", "all_in"}, {0.4, 0.5, 0.1}, std::nullopt};
    auto* fold = dialog.findChild<QCheckBox*>("lockActionCheck_fold");
    auto* fold_percent = dialog.findChild<QDoubleSpinBox*>("lockActionPercent_fold");
    auto* error = dialog.findChild<QLabel*>("errorLabel");
    BOOST_REQUIRE(fold != nullptr && fold_percent != nullptr && error != nullptr);
    BOOST_CHECK_CLOSE(fold_percent->value(), 40.0, 1e-6);  // starts from the solved mix
    BOOST_CHECK(!fold_percent->isEnabled());

    BOOST_CHECK(!dialog.try_accept());
    BOOST_CHECK(!error->isHidden());
    BOOST_CHECK(!dialog.lock().has_value());

    fold->setChecked(true);
    fold_percent->setValue(100.0);
    BOOST_CHECK(dialog.try_accept());
    BOOST_REQUIRE(dialog.lock().has_value());
    BOOST_REQUIRE_EQUAL(dialog.lock()->actions.size(), 1u);
    BOOST_CHECK_EQUAL(dialog.lock()->actions[0].action, "fold");
    BOOST_CHECK_CLOSE(dialog.lock()->actions[0].frequency, 1.0, 1e-9);

    // An existing lock pre-fills its rows and hands.
    const cli::node_lock existing{.path = {"bet_75"}, .hands = "AK", .actions = {{"call", 0.25}}};
    ui::widgets::node_lock_dialog edit{
        {"bet_75"}, QStringLiteral("BB"), {"fold", "call", "all_in"}, {0.4, 0.5, 0.1}, existing};
    BOOST_CHECK(edit.findChild<QCheckBox*>("lockActionCheck_call")->isChecked());
    BOOST_CHECK_CLOSE(edit.findChild<QDoubleSpinBox*>("lockActionPercent_call")->value(), 25.0, 1e-6);
    BOOST_CHECK(edit.findChild<QLineEdit*>("lockHandsField")->text() == QStringLiteral("AK"));
}

BOOST_AUTO_TEST_CASE(strategy_explorer_shows_and_edits_node_locks) {
    auto& app = qt_app();
    const auto solved = solve_locked_river();
    ui::widgets::strategy_explorer explorer{solved.spot, solved.artifact, metrics()};
    std::optional<std::vector<cli::node_lock>> reported;
    explorer.set_locks_changed_callback([&reported](std::vector<cli::node_lock> locks) {
        reported = std::move(locks);
    });
    explorer.resize(1000, 760);
    explorer.show();
    app.processEvents();

    auto* tree = explorer.findChild<QTreeWidget*>("solutionActionTree");
    auto* actions = explorer.findChild<QTableWidget*>("solutionNodeActionTable");
    auto* status = explorer.findChild<QLabel*>("nodeLockStatus");
    auto* lock_button = explorer.findChild<QPushButton*>("lockNodeButton");
    auto* unlock_button = explorer.findChild<QPushButton*>("unlockNodeButton");
    BOOST_REQUIRE(tree != nullptr && actions != nullptr && status != nullptr && lock_button != nullptr && unlock_button != nullptr);

    // The root is free; the locked node is marked in the tree and reports its lock.
    BOOST_CHECK(explorer.active_lock_path().has_value() && explorer.active_lock_path()->empty());
    BOOST_CHECK(status->text().contains(QStringLiteral("No locks")));
    BOOST_CHECK(lock_button->isEnabled());
    BOOST_CHECK(!unlock_button->isEnabled());
    auto* bet = child_named(tree->topLevelItem(0), QStringLiteral("Bet 75"));
    BOOST_REQUIRE(bet != nullptr);
    BOOST_CHECK(bet->text(1).contains(QStringLiteral("locked")));
    BOOST_CHECK(!tree->topLevelItem(0)->text(1).contains(QStringLiteral("locked")));

    tree->setCurrentItem(bet);
    app.processEvents();
    BOOST_REQUIRE(explorer.active_lock_path().has_value());
    BOOST_CHECK(*explorer.active_lock_path() == std::vector<std::string>{"bet_75"});
    BOOST_CHECK(actions->item(0, 0)->text() == QStringLiteral("Fold"));
    BOOST_CHECK(!actions->item(0, 3)->text().isEmpty());
    BOOST_CHECK(actions->item(1, 3)->text().isEmpty());
    BOOST_CHECK(status->text().contains(QStringLiteral("Applied in this solve")));
    BOOST_CHECK(unlock_button->isEnabled());

    // Changing the lock reports the new list and marks it unsolved.
    explorer.apply_node_lock(cli::node_lock{.path = {}, .hands = {}, .actions = {{"fold", 0.5}}});
    BOOST_REQUIRE(reported.has_value());
    BOOST_REQUIRE_EQUAL(reported->size(), 1u);
    BOOST_CHECK((*reported)[0].path == std::vector<std::string>{"bet_75"});
    BOOST_CHECK_CLOSE((*reported)[0].actions[0].frequency, 0.5, 1e-9);
    BOOST_CHECK(status->text().contains(QStringLiteral("Not solved yet")));

    explorer.remove_node_locks();
    BOOST_REQUIRE(reported.has_value());
    BOOST_CHECK(reported->empty());
    BOOST_CHECK(status->text().contains(QStringLiteral("lock has since been removed")));
    BOOST_CHECK(!unlock_button->isEnabled());
}

BOOST_AUTO_TEST_CASE(strategy_explorer_hides_lock_controls_without_a_callback) {
    auto& app = qt_app();
    const auto solved = solve_locked_river();
    ui::widgets::strategy_explorer explorer{solved.spot, solved.artifact, metrics()};
    explorer.show();
    app.processEvents();
    BOOST_CHECK(explorer.findChild<QPushButton*>("lockNodeButton")->isHidden());
    BOOST_CHECK(explorer.findChild<QPushButton*>("unlockNodeButton")->isHidden());
}

BOOST_AUTO_TEST_CASE(spot_builder_lists_and_removes_locks) {
    qt_app();
    auto spot = cli::parse_spot_json(river_spot);
    BOOST_REQUIRE(spot.has_value());
    std::optional<ui::spot> emitted;
    ui::widgets::spot_builder builder{*spot, metrics(), [&emitted](ui::spot next) { emitted = std::move(next); }, {}};
    auto* table = builder.findChild<QTableWidget*>("nodeLockTable");
    auto* remove = builder.findChild<QPushButton*>("removeNodeLockButton");
    auto* clear = builder.findChild<QPushButton*>("clearNodeLocksButton");
    BOOST_REQUIRE(table != nullptr && remove != nullptr && clear != nullptr);
    BOOST_REQUIRE_EQUAL(table->rowCount(), 1);
    BOOST_CHECK(table->item(0, 0)->text() == QStringLiteral("Bet 75"));
    BOOST_CHECK(table->item(0, 1)->text() == QStringLiteral("All hands"));
    BOOST_CHECK(table->item(0, 2)->text() == QStringLiteral("Fold 100%"));
    BOOST_CHECK(!remove->isEnabled());

    auto locks = spot->locks;
    locks.push_back(cli::node_lock{.path = {}, .hands = "AA", .actions = {{"check", 1.0}}});
    builder.set_locks(locks);
    BOOST_CHECK_EQUAL(table->rowCount(), 2);
    BOOST_CHECK(!emitted.has_value());  // syncing locks is not an edit

    table->selectRow(0);
    BOOST_CHECK(remove->isEnabled());
    remove->click();
    BOOST_REQUIRE(emitted.has_value());
    BOOST_REQUIRE_EQUAL(emitted->locks.size(), 1u);
    BOOST_CHECK_EQUAL(emitted->locks[0].hands, "AA");

    clear->click();
    BOOST_CHECK(emitted->locks.empty());
    BOOST_CHECK_EQUAL(table->rowCount(), 0);
    BOOST_CHECK(!clear->isEnabled());
}

BOOST_AUTO_TEST_CASE(workspace_keeps_builder_and_explorer_locks_in_step) {
    auto& app = qt_app();
    const auto solved = solve_locked_river();
    auto document = ui::spot_document::create_new();
    document.replace_spot(solved.spot);
    document.replace_artifact(solved.artifact);  // clears the solved snapshot, so set it after
    document.set_solved_spot(solved.spot);

    std::optional<std::vector<cli::node_lock>> from_explorer;
    ui::document_workspace_widget workspace{
        document,
        ui::theme::theme_id::dark_pro,
        metrics(),
        false,
        {},
        {},
        ui::document_workspace_callbacks{
            .on_locks_changed = [&from_explorer](std::vector<cli::node_lock> locks) { from_explorer = std::move(locks); }
        }};
    workspace.show();
    app.processEvents();

    auto* explorer = dynamic_cast<ui::widgets::strategy_explorer*>(workspace.findChild<QWidget*>("strategyExplorer"));
    auto* builder_table = workspace.findChild<QTableWidget*>("nodeLockTable");
    BOOST_REQUIRE(explorer != nullptr && builder_table != nullptr);
    BOOST_CHECK_EQUAL(builder_table->rowCount(), 1);

    // Lock the root from the explorer, then do what the main window does with the result.
    explorer->apply_node_lock(cli::node_lock{.path = {}, .hands = {}, .actions = {{"check", 0.0}}});
    BOOST_REQUIRE(from_explorer.has_value());
    BOOST_CHECK_EQUAL(from_explorer->size(), 2u);
    auto next = document.current_spot();
    next.locks = *from_explorer;
    document.replace_spot(next);
    workspace.refresh_inspector(document);
    workspace.update_strategy_staleness(document.is_strategy_stale());
    BOOST_CHECK_EQUAL(builder_table->rowCount(), 2);
    BOOST_CHECK(document.is_strategy_stale());
}

BOOST_AUTO_TEST_SUITE_END()
