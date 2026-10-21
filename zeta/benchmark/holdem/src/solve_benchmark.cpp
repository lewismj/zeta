#include <cstdint>
#include <iostream>
#include <optional>
#include <string>

#include <benchmark/benchmark.h>

#include "cfr/betting/betting.h"
#include "cli/solve_cli.h"

namespace {

    namespace cli = zeta::holdem::cli;
    namespace cfr = zeta::holdem::cfr;

    // `cli::solve_spot` names both the parsed-spot struct and the solve function; the
    // function hides the type here, so refer to the struct through an elaborated alias.
    using parsed_spot = struct cli::solve_spot;

    // A spot solved end to end through `cli::solve_spot`, the same path the CLI and UI
    // use, for a fixed number of CFR+ iterations.
    struct solve_case {
        const char* label;
        const char* spot_json;
        std::uint64_t iterations;
    };

    constexpr solve_case river_heads_up{
        .label = "river/heads-up",
        .spot_json = R"({
  "street": "river",
  "players": ["BTN", "BB"],
  "board": ["Ah", "Kd", "7c", "4s", "2h"],
  "ranges": ["AA,KK,QQ,JJ,TT,AK,AQ,AJ,KQ,QJs,JTs,T9s", "99,88,77,66,AT,KJ,QT,J9s,98s,87s,76s,A5s"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [200.0, 200.0],
  "betting_policy": {"fixed_pot_fractions": [0.33, 0.75, 1.5], "max_raises": 2},
  "max_history": 8
})",
        .iterations = 50
    };

    constexpr solve_case river_three_way{
        .label = "river/3-way",
        .spot_json = R"({
  "street": "river",
  "players": ["BTN", "SB", "BB"],
  "board": ["Ah", "Kd", "7c", "4s", "2h"],
  "ranges": ["AA,KK,AK", "QQ,JJ,AQ", "TT,99,KQ"],
  "gross_pot": 150.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0, 50.0],
  "stacks": [100.0, 100.0, 100.0],
  "betting_policy": {"fixed_pot_fractions": [0.75], "max_raises": 1},
  "max_history": 6
})",
        .iterations = 10
    };

    constexpr solve_case turn_heads_up{
        .label = "turn/heads-up",
        .spot_json = R"({
  "street": "turn",
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c", "4h"],
  "ranges": ["AA,KK,AK,AQ,KQ,QJs", "QQ,JJ,TT,AJ,KJ,JTs"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [100.0, 100.0],
  "betting_policy": {"fixed_pot_fractions": [0.75], "max_raises": 1},
  "max_history": 6
})",
        .iterations = 10
    };

    constexpr solve_case flop_heads_up{
        .label = "flop/heads-up",
        .spot_json = R"({
  "street": "flop",
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c"],
  "ranges": ["AK,AQ,KQ", "QJs,JTs,T9s"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [60.0, 60.0],
  "betting_policy": {"fixed_pot_fractions": [0.75], "max_raises": 1},
  "max_history": 4
})",
        .iterations = 2
    };

    // Sized only: the exact public-game shape and pre-build memory estimate for a wide
    // flop abstraction, measured without allocating the graph.
    constexpr const char* flop_realistic_spot = R"({
  "street": "flop",
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c"],
  "ranges": ["AA,KK,QQ,AKs,AQs,AJs,KQs", "TT,99,88,JTs,T9s,98s,A5s"],
  "gross_pot": 60.0,
  "rake": 0.0,
  "contributions": [30.0, 30.0],
  "stacks": [200.0, 200.0],
  "betting_policy": {"fixed_pot_fractions": [0.33, 0.75, 1.5], "max_raises": 3},
  "max_history": 8,
  "public_state_id": 3
})";

    [[nodiscard]] std::int64_t terminal_leaf_count(const cli::solve_artifact& artifact)
    {
        std::int64_t count = 0;
        for (const auto& node : artifact.solved_nodes) {
            count += node.terminal ? 1 : 0;
        }
        return count;
    }

    // state.range(0) is the worker thread count.
    void benchmark_solve(benchmark::State& state, const solve_case& c)
    {
        auto spot = cli::parse_spot_json(c.spot_json);
        if (!spot) {
            state.SkipWithError("benchmark spot failed to parse");
            return;
        }
        const cli::solve_runtime_options runtime{
            .worker_threads = static_cast<std::uint32_t>(state.range(0)),
            .memory_budget_bytes = std::uint64_t{16} << 30
        };

        // A warm-up solve records the graph shape and build time, so the timed loop
        // measures steady-state solves only.
        auto probe = cli::solve_spot(*spot, c.iterations, runtime);
        if (!probe) {
            state.SkipWithError(("benchmark solve failed: " + probe.error().message).c_str());
            return;
        }
        const auto terminals = terminal_leaf_count(probe->artifact);

        for (auto _ : state) {
            auto output = cli::solve_spot(*spot, c.iterations, runtime);
            benchmark::DoNotOptimize(output);
        }

        const auto solved_iterations = static_cast<std::int64_t>(state.iterations()) * static_cast<std::int64_t>(c.iterations);
        const auto players = static_cast<std::int64_t>(spot->players.size());
        state.SetLabel(c.label);
        state.SetItemsProcessed(solved_iterations);
        state.counters["graph_nodes"] = static_cast<double>(probe->artifact.solved_nodes.size());
        state.counters["terminal_leaves"] = static_cast<double>(terminals);
        state.counters["graph_build_ms"] = probe->timing.graph_build_ms;
        state.counters["cfr_iterations_per_second"] = benchmark::Counter(
            static_cast<double>(solved_iterations), benchmark::Counter::kIsRate);
        // Each CFR iteration evaluates every terminal leaf once per updating player.
        state.counters["terminal_evals_per_second"] = benchmark::Counter(
            static_cast<double>(solved_iterations * terminals * players), benchmark::Counter::kIsRate);
    }

    [[nodiscard]] std::optional<cfr::holdem_public_game_config<2>> public_config_from_spot(const parsed_spot& spot)
    {
        auto street = cli::detail::parse_holdem_street(spot.street);
        if (!street) {
            return std::nullopt;
        }
        auto board = cli::detail::board_from_cards(spot.board, *street);
        if (!board) {
            return std::nullopt;
        }
        cfr::holdem_public_game_config<2> config{};
        config.street = *street;
        config.board_cards = board->mask;
        config.initial_stacks = {spot.stacks[0], spot.stacks[1]};
        config.initial_committed = {spot.contributions[0], spot.contributions[1]};
        config.root_actor = spot.root_actor;
        config.abstraction = cli::resolve_spot_betting_policy(spot);
        config.max_history = spot.max_history;
        config.public_state_id = spot.public_state_id;
        return config;
    }

    void BM_SolveRiverHeadsUp(benchmark::State& state) {
        benchmark_solve(state, river_heads_up);
    }

    void BM_SolveRiverThreeWay(benchmark::State& state) {
        benchmark_solve(state, river_three_way);
    }

    void BM_SolveTurnHeadsUp(benchmark::State& state) {
        benchmark_solve(state, turn_heads_up);
    }

    void BM_SolveFlopHeadsUp(benchmark::State& state) {
        benchmark_solve(state, flop_heads_up);
    }

    void BM_SizeFlopRealistic(benchmark::State& state) {
        auto spot = cli::parse_spot_json(flop_realistic_spot);
        auto config = spot ? public_config_from_spot(*spot) : std::nullopt;
        if (!config) {
            state.SkipWithError("benchmark spot failed to lower into a public-game config");
            return;
        }
        std::uint64_t estimated_bytes = 0;
        for (auto _ : state) {
            auto estimate = cfr::estimate_multi_street_public_game_memory(*config);
            benchmark::DoNotOptimize(estimate);
            if (!estimate) {
                state.SkipWithError("benchmark spot failed memory estimation");
                return;
            }
            estimated_bytes = estimate->total_bytes;
        }
        const auto shape = cfr::estimate_multi_street_public_game_shape(*config);
        if (!shape) {
            state.SkipWithError("benchmark spot failed shape estimation");
            return;
        }
        state.SetLabel("flop/realistic (sized only)");
        state.counters["graph_nodes"] = static_cast<double>(shape->node_count);
        state.counters["terminal_leaves"] = static_cast<double>(shape->terminal_node_count);
        state.counters["estimated_memory_bytes"] = static_cast<double>(estimated_bytes);
    }

}

BENCHMARK(BM_SolveRiverHeadsUp)->Arg(1)->Arg(4)->Arg(8)->Unit(benchmark::kMillisecond)->UseRealTime();
BENCHMARK(BM_SolveRiverThreeWay)->Arg(1)->Arg(4)->Arg(8)->Unit(benchmark::kMillisecond)->UseRealTime();
BENCHMARK(BM_SolveTurnHeadsUp)->Arg(1)->Arg(4)->Arg(8)->Unit(benchmark::kMillisecond)->UseRealTime();
BENCHMARK(BM_SolveFlopHeadsUp)->Arg(1)->Arg(4)->Arg(8)->Unit(benchmark::kMillisecond)->UseRealTime();
BENCHMARK(BM_SizeFlopRealistic)->Unit(benchmark::kMicrosecond);

int main(int argc, char** argv) {
    std::cout << "BM_Solve*/<n>             : full solves through cli::solve_spot with n worker threads\n";
    std::cout << "cfr_iterations_per_second : CFR+ iterations/sec (every player updated once per iteration)\n";
    std::cout << "terminal_evals_per_second : terminal-leaf evaluations/sec (the parallelised work)\n";
    std::cout << "BM_SizeFlopRealistic      : exact shape and pre-build memory estimate, no solve\n\n";

    benchmark::Initialize(&argc, argv);
    if (benchmark::ReportUnrecognizedArguments(argc, argv)) {
        return 1;
    }
    benchmark::RunSpecifiedBenchmarks();
    benchmark::Shutdown();
    return 0;
}
