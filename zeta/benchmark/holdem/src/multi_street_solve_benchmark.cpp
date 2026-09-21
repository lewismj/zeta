#include <cstdint>
#include <iostream>
#include <optional>
#include <string>
#include <string_view>

#include <benchmark/benchmark.h>

#include "cfr/betting/betting.h"
#include "cli/solve_cli.h"

namespace {

    namespace cli = zeta::holdem::cli;
    namespace cfr = zeta::holdem::cfr;

    // `cli::solve_spot` names both the parsed-spot struct and the solve function; the
    // function hides the type here, so refer to the struct through an elaborated alias.
    using parsed_spot = struct cli::solve_spot;

    // A named multi-street solve configuration. Small cases are solved end to end so
    // the harness can record build time and CFR/terminal throughput; the realistic
    // flop case is sized only (its exact footprint is measured without allocating the
    // graph) to exercise the pre-build memory budgeting on a wide betting abstraction.
    struct solve_case {
        const char* label;
        const char* spot_json;
        std::int64_t iterations;
    };

    constexpr solve_case flop_small{
        .label = "flop/small",
        .spot_json = R"({
  "street": "flop",
  "players": ["BTN", "BB"],
  "board": ["As", "Kd", "7c"],
  "ranges": ["AhKh", "QdJd"],
  "gross_pot": 100.0,
  "rake": 0.0,
  "contributions": [50.0, 50.0],
  "stacks": [60.0, 60.0],
  "betting_policy": {"fixed_pot_fractions": [0.75], "max_raises": 1},
  "max_history": 4,
  "public_state_id": 3,
  "samples_per_combo": 1
})",
        .iterations = 0
    };

    constexpr solve_case turn_small{
        .label = "turn/small",
        .spot_json = R"({
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
})",
        .iterations = 32,
    };

    constexpr solve_case flop_realistic{
        .label = "flop/realistic",
        .spot_json = R"({
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
  "public_state_id": 3,
  "samples_per_combo": 8
})",
        .iterations = 0
    };

    struct graph_shape_counts {
        std::int64_t nodes = 0;
        std::int64_t edges = 0;
        std::int64_t infosets = 0;
        std::int64_t chance_outcomes = 0;
        std::int64_t terminal_leaves = 0;
    };

    [[nodiscard]] graph_shape_counts count_artifact_shape(const cli::solve_artifact& artifact)
    {
        graph_shape_counts counts{};
        counts.nodes = static_cast<std::int64_t>(artifact.solved_nodes.size());
        for (const auto& node : artifact.solved_nodes) {
            counts.edges += static_cast<std::int64_t>(node.actions.size());
            if (node.kind == "player") {
                ++counts.infosets;
            }
            if (node.terminal) {
                ++counts.terminal_leaves;
            }
        }
        for (const auto& event : artifact.chance_events) {
            counts.chance_outcomes += static_cast<std::int64_t>(event.outcomes.size());
        }
        return counts;
    }

    [[nodiscard]] graph_shape_counts shape_from_estimate(const cfr::multi_street_public_game_shape& shape)
    {
        graph_shape_counts counts{};
        counts.nodes = static_cast<std::int64_t>(shape.node_count);
        counts.edges = static_cast<std::int64_t>(shape.edge_count);
        counts.infosets = static_cast<std::int64_t>(shape.infoset_count);
        counts.chance_outcomes = static_cast<std::int64_t>(shape.chance_outcome_count);
        counts.terminal_leaves = static_cast<std::int64_t>(shape.terminal_node_count);
        return counts;
    }

    // Rebuild the public-game config from a spot so the harness can estimate the exact
    // graph shape and CFR memory footprint without allocating the solver tables.
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
        config.dead_cards = 0u;
        config.initial_stacks = {spot.stacks[0], spot.stacks[1]};
        config.initial_committed = {spot.contributions[0], spot.contributions[1]};
        config.root_actor = spot.root_actor;
        config.abstraction = cli::resolve_spot_betting_policy(spot);
        config.max_history = spot.max_history;
        config.public_state_id = spot.public_state_id;
        return config;
    }

    void set_shape_counters(benchmark::State& state, const graph_shape_counts& counts, std::uint64_t estimated_bytes)
    {
        state.counters["graph_nodes"] = static_cast<double>(counts.nodes);
        state.counters["graph_edges"] = static_cast<double>(counts.edges);
        state.counters["infosets"] = static_cast<double>(counts.infosets);
        state.counters["chance_outcomes"] = static_cast<double>(counts.chance_outcomes);
        state.counters["terminal_leaves"] = static_cast<double>(counts.terminal_leaves);
        state.counters["estimated_memory_bytes"] = static_cast<double>(estimated_bytes);
    }

    void benchmark_solve(benchmark::State& state, const solve_case& c)
    {
        auto spot = cli::parse_spot_json(c.spot_json);
        if (!spot) {
            state.SkipWithError("benchmark spot failed to parse");
            return;
        }
        auto config = public_config_from_spot(*spot);
        if (!config) {
            state.SkipWithError("benchmark spot failed to lower into a public-game config");
            return;
        }
        auto memory = cfr::estimate_multi_street_public_game_memory(*config);
        if (!memory) {
            state.SkipWithError((std::string{"benchmark spot failed memory estimation: "}
                + cfr::to_string(memory.error().kind)).c_str());
            return;
        }

        // A single warm-up solve captures the realized graph shape and build time so the
        // per-iteration loop measures steady-state throughput only.
        const cli::solve_runtime_options runtime{.memory_budget_bytes = std::uint64_t{16} << 30};
        auto probe = cli::solve_spot(*spot, static_cast<std::uint64_t>(c.iterations), runtime);
        if (!probe) {
            state.SkipWithError("benchmark solve failed");
            return;
        }
        const auto counts = count_artifact_shape(probe->artifact);
        state.counters["graph_build_ms"] = probe->timing.graph_build_ms;

        for (auto _ : state) {
            auto output = cli::solve_spot(*spot, static_cast<std::uint64_t>(c.iterations), runtime);
            benchmark::DoNotOptimize(output);
        }

        const auto solved_iterations = state.iterations() * c.iterations;
        set_shape_counters(state, counts, memory->total_bytes);
        state.SetLabel(c.label);
        state.SetItemsProcessed(solved_iterations);
        state.counters["cfr_iterations_per_second"] = benchmark::Counter(
            static_cast<double>(solved_iterations), benchmark::Counter::kIsRate);
        // Each CFR iteration re-evaluates every terminal leaf once per updating player.
        state.counters["terminal_evals_per_second"] = benchmark::Counter(
            static_cast<double>(solved_iterations * counts.terminal_leaves * 2), benchmark::Counter::kIsRate);
    }

    void benchmark_size_only(benchmark::State& state, const solve_case& c)
    {
        auto spot = cli::parse_spot_json(c.spot_json);
        if (!spot) {
            state.SkipWithError("benchmark spot failed to parse");
            return;
        }
        auto config = public_config_from_spot(*spot);
        if (!config) {
            state.SkipWithError("benchmark spot failed to lower into a public-game config");
            return;
        }

        cfr::multi_street_public_game_shape shape{};
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
        auto measured_shape = cfr::estimate_multi_street_public_game_shape(*config);
        if (!measured_shape) {
            state.SkipWithError("benchmark spot failed shape estimation");
            return;
        }
        shape = *measured_shape;

        set_shape_counters(state, shape_from_estimate(shape), estimated_bytes);
        state.SetLabel(c.label);
        state.SetItemsProcessed(state.iterations());
    }

    void BM_MultiStreetSize_FlopSmall(benchmark::State& state) {
        benchmark_size_only(state, flop_small);
    }

    void BM_MultiStreetSolve_TurnSmall(benchmark::State& state) {
        benchmark_solve(state, turn_small);
    }

    void BM_MultiStreetSize_FlopRealistic(benchmark::State& state) {
        benchmark_size_only(state, flop_realistic);
    }

}

BENCHMARK(BM_MultiStreetSize_FlopSmall)->Unit(benchmark::kMicrosecond);
BENCHMARK(BM_MultiStreetSolve_TurnSmall)->Unit(benchmark::kMillisecond);
BENCHMARK(BM_MultiStreetSize_FlopRealistic)->Unit(benchmark::kMicrosecond);

int main(int argc, char** argv) {
    std::cout << "solver             : unified heads-up CFR+ over the exact multi-street game\n";
    std::cout << "flop/turn small    : solved end to end; reports build time and throughput\n";
    std::cout << "flop/realistic     : sized only; reports exact shape and estimated footprint\n\n";
    std::cout << "cfr_iterations_per_second : full CFR+ iterations/sec (both player updates)\n";
    std::cout << "terminal_evals_per_second : terminal-leaf evaluations/sec across iterations\n";
    std::cout << "estimated_memory_bytes    : pre-build CFR footprint the budget check consumes\n\n";

    benchmark::Initialize(&argc, argv);
    if (benchmark::ReportUnrecognizedArguments(argc, argv)) {
        return 1;
    }
    benchmark::RunSpecifiedBenchmarks();
    benchmark::Shutdown();
    return 0;
}
