#include <algorithm>
#include <cstdint>
#include <iostream>
#include <memory>
#include <random>
#include <vector>

#include <benchmark/benchmark.h>

#include "terminal.h"

namespace {

    constexpr std::size_t benchmark_board_count = 8;
    constexpr std::size_t river_live_combos = 1081;

    struct benchmark_case {
        zeta::holdem::river_terminal_cache cache{};
        zeta::holdem::reach_vector oop_reach{};
        zeta::holdem::reach_vector ip_reach{};
    };

    struct benchmark_data {
        std::vector<zeta::holdem::board> boards;
        std::vector<benchmark_case> dense_cases;
        std::vector<benchmark_case> sparse_50_cases;
        std::vector<benchmark_case> sparse_100_cases;
        std::vector<benchmark_case> sparse_300_cases;
        zeta::holdem::terminal_context context{
            .pot = {
                .gross_pot = 300.0,
                .rake = 15.0,
                .oop_contribution = 100.0,
                .ip_contribution = 100.0
            }
        };
    };

    [[nodiscard]] zeta::card_mask random_five_card_mask(std::mt19937_64& rng) {
        std::uniform_int_distribution<int> dist(0, 51);
        zeta::card_mask mask = 0;
        while (zeta::ops::popcount(mask) < 5) {
            mask |= (zeta::card_mask{1} << dist(rng));
        }
        return mask;
    }

    [[nodiscard]] std::vector<zeta::holdem::board> build_river_boards(const std::size_t count, const std::uint64_t seed) {
        std::mt19937_64 rng(seed);
        std::vector<zeta::holdem::board> boards;
        boards.reserve(count);
        for (std::size_t i = 0; i < count; ++i) {
            boards.push_back(zeta::holdem::board{random_five_card_mask(rng)});
        }
        return boards;
    }

    [[nodiscard]] zeta::holdem::reach_vector make_dense_reach(
        const zeta::holdem::river_terminal_cache& cache,
        const std::uint64_t seed
    ) {
        std::mt19937_64 rng(seed);
        std::uniform_int_distribution<int> dist(1, 8);
        zeta::holdem::reach_vector reach{};
        for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
            const auto combo = cache.rank_order[order];
            reach[combo] = static_cast<float>(dist(rng)) * 0.125f;
        }
        return reach;
    }

    [[nodiscard]] zeta::holdem::reach_vector make_sparse_reach(
        const zeta::holdem::river_terminal_cache& cache,
        const std::uint64_t seed,
        const std::size_t target_count
    ) {
        std::mt19937_64 rng(seed);
        std::uniform_int_distribution<int> dist(1, 8);
        zeta::holdem::reach_vector reach{};

        std::vector<zeta::holdem::combination_index> picks;
        picks.reserve(cache.rank_order_count);
        for (std::size_t i = 0; i < cache.rank_order_count; ++i) {
            picks.push_back(cache.rank_order[i]);
        }
        std::shuffle(picks.begin(), picks.end(), rng);
        const auto limit = std::min(target_count, picks.size());
        for (std::size_t i = 0; i < limit; ++i) {
            reach[picks[i]] = static_cast<float>(dist(rng)) * 0.125f;
        }
        return reach;
    }

    [[nodiscard]] benchmark_case make_case(const zeta::holdem::board board, const std::size_t sparse_count, const std::uint64_t seed) {
        benchmark_case c{};
        c.cache = zeta::holdem::make_river_terminal_cache(board);
        c.oop_reach = sparse_count == 0
            ? make_dense_reach(c.cache, 0xD065E000ULL + seed)
            : make_sparse_reach(c.cache, 0x5A125E00ULL + seed, sparse_count);
        c.ip_reach = sparse_count == 0
            ? make_dense_reach(c.cache, 0xD065E100ULL + seed)
            : make_sparse_reach(c.cache, 0x5A125E10ULL + seed, sparse_count);
        return c;
    }

    void populate_cases(
        const std::vector<zeta::holdem::board>& boards,
        std::vector<benchmark_case>& out,
        const std::size_t sparse_count
    ) {
        out.reserve(boards.size());
        for (std::size_t i = 0; i < boards.size(); ++i) {
            out.push_back(make_case(boards[i], sparse_count, static_cast<std::uint64_t>(i)));
        }
    }

    [[nodiscard]] const benchmark_data& data() {
        static const auto instance = []() {
            auto d = std::make_unique<benchmark_data>();
            d->boards = build_river_boards(benchmark_board_count, 0xA51CEULL);
            populate_cases(d->boards, d->dense_cases, 0);
            populate_cases(d->boards, d->sparse_50_cases, 50);
            populate_cases(d->boards, d->sparse_100_cases, 100);
            populate_cases(d->boards, d->sparse_300_cases, 300);
            return d;
        }();
        return *instance;
    }

    [[nodiscard]] const std::vector<benchmark_case>& sparse_cases(const std::size_t sparse_count) {
        const auto& d = data();
        if (sparse_count == 50) {
            return d.sparse_50_cases;
        }
        if (sparse_count == 300) {
            return d.sparse_300_cases;
        }
        return d.sparse_100_cases;
    }

    void BM_RiverTerminalCacheConstruction(benchmark::State& state) {
        const auto& boards = data().boards;
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto& board : boards) {
                const auto cache = zeta::holdem::make_river_terminal_cache(board);
                benchmark::DoNotOptimize(sink += cache.rank_order_count + cache.unique_rank_count);
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * boards.size()));
        state.counters["live_combos"] = benchmark::Counter(river_live_combos, benchmark::Counter::kIsIterationInvariant);
    }

    void BM_RiverReachIndexConstructionDense(benchmark::State& state) {
        const auto& cases = data().dense_cases;
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto& c : cases) {
                const auto oop = zeta::holdem::make_river_reach_index(c.cache, c.oop_reach);
                const auto ip = zeta::holdem::make_river_reach_index(c.cache, c.ip_reach);
                benchmark::DoNotOptimize(sink += oop.active_count + ip.active_count);
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * cases.size() * river_live_combos * 2));
    }

    void BM_RiverReachIndexConstructionSparse(benchmark::State& state) {
        const auto sparse_count = static_cast<std::size_t>(state.range(0));
        const auto& cases = sparse_cases(sparse_count);
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto& c : cases) {
                const auto oop = zeta::holdem::make_river_reach_index(c.cache, c.oop_reach);
                const auto ip = zeta::holdem::make_river_reach_index(c.cache, c.ip_reach);
                benchmark::DoNotOptimize(sink += oop.active_count + ip.active_count);
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * cases.size() * sparse_count * 2));
    }

    void BM_TerminalFoldValuesDense(benchmark::State& state) {
        const auto& d = data();
        const auto& cases = d.dense_cases;
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto& c : cases) {
                auto values = zeta::holdem::evaluate_fold_values(
                    c.cache,
                    c.oop_reach,
                    c.ip_reach,
                    d.context,
                    zeta::holdem::player::ip
                );
                benchmark::DoNotOptimize(values);
                sink += c.cache.rank_order_count;
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * cases.size() * river_live_combos * 2));
    }

    void BM_TerminalFoldValuesSparse(benchmark::State& state) {
        const auto sparse_count = static_cast<std::size_t>(state.range(0));
        const auto& d = data();
        const auto& cases = sparse_cases(sparse_count);
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto& c : cases) {
                auto values = zeta::holdem::evaluate_fold_values(
                    c.cache,
                    c.oop_reach,
                    c.ip_reach,
                    d.context,
                    zeta::holdem::player::ip
                );
                benchmark::DoNotOptimize(values);
                sink += sparse_count;
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * cases.size() * sparse_count * 2));
    }

    void BM_TerminalShowdownValuesDense(benchmark::State& state) {
        const auto& d = data();
        const auto& cases = d.dense_cases;
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto& c : cases) {
                auto values = zeta::holdem::evaluate_showdown_values(c.cache, c.oop_reach, c.ip_reach, d.context);
                benchmark::DoNotOptimize(values);
                sink += c.cache.rank_order_count;
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * cases.size() * river_live_combos * 2));
    }

    void BM_TerminalShowdownValuesSparse(benchmark::State& state) {
        const auto sparse_count = static_cast<std::size_t>(state.range(0));
        const auto& d = data();
        const auto& cases = sparse_cases(sparse_count);
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto& c : cases) {
                auto values = zeta::holdem::evaluate_showdown_values(c.cache, c.oop_reach, c.ip_reach, d.context);
                benchmark::DoNotOptimize(values);
                sink += sparse_count;
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * cases.size() * sparse_count * 2));
    }

    template<typename T>
    void BM_MemoryLayoutBytes(benchmark::State& state, const char* label) {
        std::uint64_t sink = 0;
        for (auto _ : state) {
            benchmark::DoNotOptimize(sink += sizeof(T));
        }
        benchmark::DoNotOptimize(sink);
        state.SetLabel(label);
        state.SetItemsProcessed(state.iterations());
        state.counters["bytes"] = benchmark::Counter(sizeof(T), benchmark::Counter::kIsIterationInvariant);
    }

    void BM_MemoryLayoutTerminalValues(benchmark::State& state) {
        BM_MemoryLayoutBytes<zeta::holdem::terminal_values>(state, "terminal_values");
    }

    void BM_MemoryLayoutRiverTerminalCache(benchmark::State& state) {
        BM_MemoryLayoutBytes<zeta::holdem::river_terminal_cache>(state, "river_terminal_cache");
    }

    void BM_MemoryLayoutRiverReachIndex(benchmark::State& state) {
        BM_MemoryLayoutBytes<zeta::holdem::river_reach_index>(state, "river_reach_index");
    }

}

BENCHMARK(BM_RiverTerminalCacheConstruction)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_RiverReachIndexConstructionDense)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_RiverReachIndexConstructionSparse)->Arg(50)->Arg(100)->Arg(300)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalFoldValuesDense)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalFoldValuesSparse)->Arg(50)->Arg(100)->Arg(300)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalShowdownValuesDense)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalShowdownValuesSparse)->Arg(50)->Arg(100)->Arg(300)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_MemoryLayoutTerminalValues)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_MemoryLayoutRiverTerminalCache)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_MemoryLayoutRiverReachIndex)->Unit(benchmark::kNanosecond);

int main(int argc, char** argv) {
    std::cout << "terminal evaluator : river cache + reach index + rank-sweep showdown/fold\n";
    std::cout << "boards per sample  : " << benchmark_board_count << "\n";
    std::cout << "memory bytes       : terminal_values=" << sizeof(zeta::holdem::terminal_values)
              << " cache=" << sizeof(zeta::holdem::river_terminal_cache)
              << " reach_index=" << sizeof(zeta::holdem::river_reach_index) << "\n\n";

    benchmark::Initialize(&argc, argv);
    if (benchmark::ReportUnrecognizedArguments(argc, argv)) {
        return 1;
    }
    benchmark::RunSpecifiedBenchmarks();
    benchmark::Shutdown();
    return 0;
}
