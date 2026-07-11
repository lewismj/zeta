#include <cstdint>
#include <iostream>
#include <string_view>

#include <benchmark/benchmark.h>

#include "range_parser.h"

namespace {

    struct parser_case {
        const char* label;
        std::string_view range;
        std::int64_t expanded_combos;
    };

    constexpr parser_case simple_range{
        .label = "AA",
        .range = "AA",
        .expanded_combos = 6
    };

    constexpr parser_case typical_preflop_range{
        .label = "22+,A2s+,K9s+,QTs+,JTs,T9s,98s,AJo+,KQo",
        .range = "22+,A2s+,K9s+,QTs+,JTs,T9s,98s,AJo+,KQo",
        .expanded_combos = 210
    };

    constexpr parser_case wide_solver_range{
        .label = "wide solver range",
        .range = "22+,A2s+,K2s+,Q2s+,J2s+,T2s+,92s+,82s+,72s+,62s+,52s+,42s+,32s+,"
                 "A2o+,K2o+,Q2o+,J2o+",
        .expanded_combos = 738
    };

    constexpr parser_case full_range{
        .label = "full 1326-combo range",
        .range = "22+,A2s+,K2s+,Q2s+,J2s+,T2s+,92s+,82s+,72s+,62s+,52s+,42s+,32s+,"
                 "A2o+,K2o+,Q2o+,J2o+,T2o+,92o+,82o+,72o+,62o+,52o+,42o+,32o+",
        .expanded_combos = 1326
    };

    constexpr parser_case exact_combo_heavy_range{
        .label = "exact combo heavy",
        .range = "AsKh,AcKd,AhKs,AdKc,AsQh,AcQd,AhQs,AdQc,KsQh,KcQd,KhQs,KdQc,"
                 "AsJh,AcJd,AhJs,AdJc",
        .expanded_combos = 16
    };

    constexpr parser_case weighted_range{
        .label = "AA:0.25,KK:0.5,AKs:0.8,AQo:0.2",
        .range = "AA:0.25,KK:0.5,AKs:0.8,AQo:0.2",
        .expanded_combos = 28
    };

    void benchmark_parse_range(benchmark::State& state, const parser_case& c) {
        const auto validation = zeta::holdem::parse_range(c.range);
        if (!validation.ok()) {
            state.SkipWithError("benchmark range failed to parse");
            return;
        }

        std::uint64_t sink = 0;
        for (auto _ : state) {
            auto parsed = zeta::holdem::parse_range(c.range);
            benchmark::DoNotOptimize(parsed);
            sink += parsed.ok() ? 1u : 0u;
        }

        benchmark::DoNotOptimize(sink);
        state.SetLabel(c.label);
        state.SetItemsProcessed(state.iterations());
        state.SetBytesProcessed(state.iterations() * static_cast<std::int64_t>(c.range.size()));
        state.counters["combos_per_second"] = benchmark::Counter(
            static_cast<double>(state.iterations() * c.expanded_combos),
            benchmark::Counter::kIsRate
        );
        state.counters["combos_per_range"] = benchmark::Counter(
            static_cast<double>(c.expanded_combos),
            benchmark::Counter::kAvgThreads
        );
    }

    void benchmark_invalid_ranges(benchmark::State& state) {
        constexpr std::string_view invalid_ranges[] = {
            "AKx",
            "AA:",
            "AsAs",
            "AsKh+"
        };
        constexpr std::int64_t invalid_count = static_cast<std::int64_t>(sizeof(invalid_ranges) / sizeof(invalid_ranges[0]));
        constexpr std::int64_t total_bytes = 3 + 3 + 4 + 5;

        for (const auto input : invalid_ranges) {
            if (zeta::holdem::parse_range(input).ok()) {
                state.SkipWithError("invalid benchmark input parsed successfully");
                return;
            }
        }

        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto input : invalid_ranges) {
                auto parsed = zeta::holdem::parse_range(input);
                benchmark::DoNotOptimize(parsed);
                sink += parsed.ok() ? 0u : 1u;
            }
        }

        benchmark::DoNotOptimize(sink);
        state.SetLabel("AKx,AA:,AsAs,AsKh+");
        state.SetItemsProcessed(state.iterations() * invalid_count);
        state.SetBytesProcessed(state.iterations() * total_bytes);
    }

    void BM_ParseRange_SimpleAA(benchmark::State& state) {
        benchmark_parse_range(state, simple_range);
    }

    void BM_ParseRange_TypicalPreflop(benchmark::State& state) {
        benchmark_parse_range(state, typical_preflop_range);
    }

    void BM_ParseRange_WideSolverRange(benchmark::State& state) {
        benchmark_parse_range(state, wide_solver_range);
    }

    void BM_ParseRange_FullRange(benchmark::State& state) {
        benchmark_parse_range(state, full_range);
    }

    void BM_ParseRange_ExactComboHeavy(benchmark::State& state) {
        benchmark_parse_range(state, exact_combo_heavy_range);
    }

    void BM_ParseRange_Weighted(benchmark::State& state) {
        benchmark_parse_range(state, weighted_range);
    }

    void BM_ParseRange_InvalidInputs(benchmark::State& state) {
        benchmark_invalid_ranges(state);
    }

    void BM_ParseRange_ConstructParserOnly(benchmark::State& state) {
        std::uint64_t sink = 0;
        for (auto _ : state) {
            auto parser = zeta::holdem::detail::range_parser{typical_preflop_range.range};
            benchmark::DoNotOptimize(parser);
            ++sink;
        }
        benchmark::DoNotOptimize(sink);
        state.SetLabel("construct parser state only");
        state.SetItemsProcessed(state.iterations());
    }

}

BENCHMARK(BM_ParseRange_SimpleAA)->Unit(benchmark::kMicrosecond);
BENCHMARK(BM_ParseRange_TypicalPreflop)->Unit(benchmark::kMicrosecond);
BENCHMARK(BM_ParseRange_WideSolverRange)->Unit(benchmark::kMicrosecond);
BENCHMARK(BM_ParseRange_FullRange)->Unit(benchmark::kMicrosecond);
BENCHMARK(BM_ParseRange_ExactComboHeavy)->Unit(benchmark::kMicrosecond);
BENCHMARK(BM_ParseRange_Weighted)->Unit(benchmark::kMicrosecond);
BENCHMARK(BM_ParseRange_InvalidInputs)->Unit(benchmark::kMicrosecond);
BENCHMARK(BM_ParseRange_ConstructParserOnly)->Unit(benchmark::kNanosecond);

int main(int argc, char** argv) {
    std::cout << "range parser      : PokerStove preflop grammar + Zeta weights\n";
    std::cout << "storage           : direct hand_range emission\n";
    std::cout << "combo indexing    : direct O(1) combination index\n\n";
    std::cout << "items_per_second  : parsed ranges/sec, or invalid inputs/sec for invalid cases\n";
    std::cout << "combos_per_second : emitted hole-card combinations/sec for successful cases\n\n";

    benchmark::Initialize(&argc, argv);
    if (benchmark::ReportUnrecognizedArguments(argc, argv)) {
        return 1;
    }
    benchmark::RunSpecifiedBenchmarks();
    benchmark::Shutdown();
    return 0;
}
