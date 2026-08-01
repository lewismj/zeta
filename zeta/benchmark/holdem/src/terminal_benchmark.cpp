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
        zeta::holdem::reach_vector p2_reach{};
        zeta::holdem::reach_vector p3_reach{};
        zeta::holdem::reach_vector p4_reach{};
        zeta::holdem::reach_vector p5_reach{};
        std::unique_ptr<zeta::holdem::river_reach_index> oop_index{};
        std::unique_ptr<zeta::holdem::river_reach_index> ip_index{};
        std::unique_ptr<zeta::holdem::river_reach_index> p2_index{};
        std::unique_ptr<zeta::holdem::river_reach_index> p3_index{};
        std::unique_ptr<zeta::holdem::river_reach_index> p4_index{};
        std::unique_ptr<zeta::holdem::river_reach_index> p5_index{};
        std::uint16_t oop_active = 0;
        std::uint16_t ip_active = 0;
        std::uint16_t p2_active = 0;
        std::uint16_t p3_active = 0;
        std::uint16_t p4_active = 0;
        std::uint16_t p5_active = 0;
        std::uint16_t oop_buckets = 0;
        std::uint16_t ip_buckets = 0;
        std::uint16_t p2_buckets = 0;
        std::uint16_t p3_buckets = 0;
        std::uint16_t p4_buckets = 0;
        std::uint16_t p5_buckets = 0;
        std::uint64_t compatible_matchups = 0;
    };

    struct benchmark_data {
        std::vector<zeta::holdem::board> boards;
        std::vector<benchmark_case> dense_cases;
        std::vector<benchmark_case> sparse_50_cases;
        std::vector<benchmark_case> sparse_100_cases;
        std::vector<benchmark_case> sparse_300_cases;
        zeta::holdem::terminal_context<2> context{
            .gross_pot = 300.0,
            .rake = 15.0,
            .contribution = {100.0, 100.0}
        };
        zeta::holdem::terminal_context<3> context3{
            .gross_pot = 450.0,
            .rake = 15.0,
            .contribution = {150.0, 150.0, 150.0}
        };
        zeta::holdem::terminal_context<4> context4{
            .gross_pot = 600.0,
            .rake = 20.0,
            .contribution = {150.0, 150.0, 150.0, 150.0}
        };
        zeta::holdem::terminal_context<6> context6{
            .gross_pot = 900.0,
            .rake = 30.0,
            .contribution = {150.0, 150.0, 150.0, 150.0, 150.0, 150.0}
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
        c.p2_reach = sparse_count == 0
            ? make_dense_reach(c.cache, 0xD065E200ULL + seed)
            : make_sparse_reach(c.cache, 0x5A125E20ULL + seed, sparse_count);
        c.p3_reach = sparse_count == 0
            ? make_dense_reach(c.cache, 0xD065E300ULL + seed)
            : make_sparse_reach(c.cache, 0x5A125E30ULL + seed, sparse_count);
        c.p4_reach = sparse_count == 0
            ? make_dense_reach(c.cache, 0xD065E400ULL + seed)
            : make_sparse_reach(c.cache, 0x5A125E40ULL + seed, sparse_count);
        c.p5_reach = sparse_count == 0
            ? make_dense_reach(c.cache, 0xD065E500ULL + seed)
            : make_sparse_reach(c.cache, 0x5A125E50ULL + seed, sparse_count);
        c.oop_index = std::make_unique<zeta::holdem::river_reach_index>(zeta::holdem::make_river_reach_index(c.cache, c.oop_reach));
        c.ip_index = std::make_unique<zeta::holdem::river_reach_index>(zeta::holdem::make_river_reach_index(c.cache, c.ip_reach));
        c.p2_index = std::make_unique<zeta::holdem::river_reach_index>(zeta::holdem::make_river_reach_index(c.cache, c.p2_reach));
        c.p3_index = std::make_unique<zeta::holdem::river_reach_index>(zeta::holdem::make_river_reach_index(c.cache, c.p3_reach));
        c.p4_index = std::make_unique<zeta::holdem::river_reach_index>(zeta::holdem::make_river_reach_index(c.cache, c.p4_reach));
        c.p5_index = std::make_unique<zeta::holdem::river_reach_index>(zeta::holdem::make_river_reach_index(c.cache, c.p5_reach));
        c.oop_active = c.oop_index->active_count;
        c.ip_active = c.ip_index->active_count;
        c.p2_active = c.p2_index->active_count;
        c.p3_active = c.p3_index->active_count;
        c.p4_active = c.p4_index->active_count;
        c.p5_active = c.p5_index->active_count;
        c.oop_buckets = c.oop_index->unique_rank_count;
        c.ip_buckets = c.ip_index->unique_rank_count;
        c.p2_buckets = c.p2_index->unique_rank_count;
        c.p3_buckets = c.p3_index->unique_rank_count;
        c.p4_buckets = c.p4_index->unique_rank_count;
        c.p5_buckets = c.p5_index->unique_rank_count;
        for (std::uint16_t oi = 0; oi < c.oop_index->active_count; ++oi) {
            const auto oop_combo = c.oop_index->active_indices[oi];
            const auto oop_mask = c.cache.masks[oop_combo];
            for (std::uint16_t ii = 0; ii < c.ip_index->active_count; ++ii) {
                const auto ip_combo = c.ip_index->active_indices[ii];
                if ((oop_mask & c.cache.masks[ip_combo]) == 0) {
                    ++c.compatible_matchups;
                }
            }
        }
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
        std::uint64_t active_total = 0;
        std::uint64_t bucket_total = 0;
        for (const auto& c : cases) {
            active_total += c.oop_active + c.ip_active;
            bucket_total += c.oop_buckets + c.ip_buckets;
        }
        for (auto _ : state) {
            for (const auto& c : cases) {
                const auto oop = zeta::holdem::make_river_reach_index(c.cache, c.oop_reach);
                const auto ip = zeta::holdem::make_river_reach_index(c.cache, c.ip_reach);
                benchmark::DoNotOptimize(sink += oop.active_count + ip.active_count);
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * active_total));
        state.counters["rank_buckets"] = benchmark::Counter(static_cast<double>(bucket_total), benchmark::Counter::kIsIterationInvariant);
    }

    void BM_RiverReachIndexConstructionSparse(benchmark::State& state) {
        const auto sparse_count = static_cast<std::size_t>(state.range(0));
        const auto& cases = sparse_cases(sparse_count);
        std::uint64_t sink = 0;
        std::uint64_t active_total = 0;
        std::uint64_t bucket_total = 0;
        for (const auto& c : cases) {
            active_total += c.oop_active + c.ip_active;
            bucket_total += c.oop_buckets + c.ip_buckets;
        }
        for (auto _ : state) {
            for (const auto& c : cases) {
                const auto oop = zeta::holdem::make_river_reach_index(c.cache, c.oop_reach);
                const auto ip = zeta::holdem::make_river_reach_index(c.cache, c.ip_reach);
                benchmark::DoNotOptimize(sink += oop.active_count + ip.active_count);
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * active_total));
        state.counters["rank_buckets"] = benchmark::Counter(static_cast<double>(bucket_total), benchmark::Counter::kIsIterationInvariant);
    }

    void BM_TerminalFoldValuesDense(benchmark::State& state) {
        const auto& d = data();
        const auto& cases = d.dense_cases;
        std::uint64_t sink = 0;
        std::uint64_t hero_combo_total = 0;
        std::uint64_t matchup_total = 0;
        for (const auto& c : cases) {
            hero_combo_total += c.oop_active + c.ip_active;
            matchup_total += c.compatible_matchups;
        }
        for (auto _ : state) {
            for (const auto& c : cases) {
                auto values = zeta::holdem::evaluate_fold_values(
                    c.cache,
                    *c.oop_index,
                    *c.ip_index,
                    d.context,
                    zeta::holdem::player::ip
                );
                benchmark::DoNotOptimize(values);
                sink += c.oop_active;
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * hero_combo_total));
        state.counters["compatible_matchups"] = benchmark::Counter(
            static_cast<double>(matchup_total),
            benchmark::Counter::kIsIterationInvariant
        );
    }

    void BM_TerminalFoldValuesSparse(benchmark::State& state) {
        const auto sparse_count = static_cast<std::size_t>(state.range(0));
        const auto& d = data();
        const auto& cases = sparse_cases(sparse_count);
        std::uint64_t sink = 0;
        std::uint64_t hero_combo_total = 0;
        std::uint64_t matchup_total = 0;
        for (const auto& c : cases) {
            hero_combo_total += c.oop_active + c.ip_active;
            matchup_total += c.compatible_matchups;
        }
        for (auto _ : state) {
            for (const auto& c : cases) {
                auto values = zeta::holdem::evaluate_fold_values(
                    c.cache,
                    *c.oop_index,
                    *c.ip_index,
                    d.context,
                    zeta::holdem::player::ip
                );
                benchmark::DoNotOptimize(values);
                sink += c.ip_active;
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * hero_combo_total));
        state.counters["compatible_matchups"] = benchmark::Counter(
            static_cast<double>(matchup_total),
            benchmark::Counter::kIsIterationInvariant
        );
    }

    void BM_TerminalShowdownValuesDense(benchmark::State& state) {
        const auto& d = data();
        const auto& cases = d.dense_cases;
        std::uint64_t sink = 0;
        std::uint64_t hero_combo_total = 0;
        std::uint64_t matchup_total = 0;
        for (const auto& c : cases) {
            hero_combo_total += c.oop_active + c.ip_active;
            matchup_total += c.compatible_matchups;
        }
        for (auto _ : state) {
            for (const auto& c : cases) {
                auto values = zeta::holdem::evaluate_showdown_values(c.cache, *c.oop_index, *c.ip_index, d.context);
                benchmark::DoNotOptimize(values);
                sink += c.oop_buckets;
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * hero_combo_total));
        state.counters["compatible_matchups"] = benchmark::Counter(
            static_cast<double>(matchup_total),
            benchmark::Counter::kIsIterationInvariant
        );
    }

    void BM_TerminalShowdownValuesSparse(benchmark::State& state) {
        const auto sparse_count = static_cast<std::size_t>(state.range(0));
        const auto& d = data();
        const auto& cases = sparse_cases(sparse_count);
        std::uint64_t sink = 0;
        std::uint64_t hero_combo_total = 0;
        std::uint64_t matchup_total = 0;
        for (const auto& c : cases) {
            hero_combo_total += c.oop_active + c.ip_active;
            matchup_total += c.compatible_matchups;
        }
        for (auto _ : state) {
            for (const auto& c : cases) {
                auto values = zeta::holdem::evaluate_showdown_values(c.cache, *c.oop_index, *c.ip_index, d.context);
                benchmark::DoNotOptimize(values);
                sink += c.ip_buckets;
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * hero_combo_total));
        state.counters["compatible_matchups"] = benchmark::Counter(
            static_cast<double>(matchup_total),
            benchmark::Counter::kIsIterationInvariant
        );
    }

    void BM_TerminalEngineShowdownDense(benchmark::State& state) {
        const auto& d = data();
        const auto& cases = d.dense_cases;
        zeta::holdem::terminal_engine<2> engine{};
        zeta::holdem::terminal_workspace<2> workspace{};
        std::uint64_t sink = 0;
        std::uint64_t hero_combo_total = 0;
        std::uint64_t matchup_total = 0;
        for (const auto& c : cases) {
            hero_combo_total += c.oop_active + c.ip_active;
            matchup_total += c.compatible_matchups;
        }
        for (auto _ : state) {
            for (const auto& c : cases) {
                auto values = engine.evaluate_showdown_values(
                    workspace,
                    c.cache,
                    c.oop_reach,
                    c.ip_reach,
                    d.context
                );
                benchmark::DoNotOptimize(values);
                sink += c.oop_buckets;
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * hero_combo_total));
        state.counters["compatible_matchups"] = benchmark::Counter(
            static_cast<double>(matchup_total),
            benchmark::Counter::kIsIterationInvariant
        );
        state.counters["kernel_family"] = benchmark::Counter(
            static_cast<double>(static_cast<std::uint8_t>(zeta::holdem::terminal_engine<2>::kernel_family())),
            benchmark::Counter::kIsIterationInvariant
        );
    }

    /**
     * End-to-end engine path (range->index materialization + showdown) is benchmarked above.
     * This isolates engine wrapper/dispatch over pre-indexed reaches to compare with raw kernel.
     */
    void BM_TerminalEngineShowdownDensePreindexed(benchmark::State& state) {
        const auto& d = data();
        const auto& cases = d.dense_cases;
        zeta::holdem::terminal_engine<2> engine{};
        std::uint64_t sink = 0;
        std::uint64_t hero_combo_total = 0;
        std::uint64_t matchup_total = 0;
        for (const auto& c : cases) {
            hero_combo_total += c.oop_active + c.ip_active;
            matchup_total += c.compatible_matchups;
        }
        for (auto _ : state) {
            for (const auto& c : cases) {
                auto values = engine.evaluate_showdown_values(
                    c.cache,
                    *c.oop_index,
                    *c.ip_index,
                    d.context
                );
                benchmark::DoNotOptimize(values);
                sink += c.ip_buckets;
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * hero_combo_total));
        state.counters["compatible_matchups"] = benchmark::Counter(
            static_cast<double>(matchup_total),
            benchmark::Counter::kIsIterationInvariant
        );
        state.counters["kernel_family"] = benchmark::Counter(
            static_cast<double>(static_cast<std::uint8_t>(zeta::holdem::terminal_engine<2>::kernel_family())),
            benchmark::Counter::kIsIterationInvariant
        );
    }

    /** Solver-like hot path: cache + reach indexes are already materialized and reused. */
    void BM_TerminalEngineShowdownDenseCached(benchmark::State& state) {
        const auto& d = data();
        const auto& cases = d.dense_cases;
        zeta::holdem::terminal_engine<2> engine{};
        std::array<const benchmark_case*, benchmark_board_count> cached_cases{};
        for (std::size_t i = 0; i < benchmark_board_count; ++i) {
            cached_cases[i] = &cases[i];
        }

        std::uint64_t sink = 0;
        std::uint64_t hero_combo_total = 0;
        std::uint64_t matchup_total = 0;
        for (const auto* c : cached_cases) {
            hero_combo_total += c->oop_active + c->ip_active;
            matchup_total += c->compatible_matchups;
        }

        for (auto _ : state) {
            for (const auto* c : cached_cases) {
                benchmark::DoNotOptimize(engine.evaluate_showdown_values(
                    c->cache,
                    *c->oop_index,
                    *c->ip_index,
                    d.context
                ));
                sink += c->oop_buckets;
            }
        }

        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * hero_combo_total));
        state.counters["compatible_matchups"] = benchmark::Counter(
            static_cast<double>(matchup_total),
            benchmark::Counter::kIsIterationInvariant
        );
        state.counters["kernel_family"] = benchmark::Counter(
            static_cast<double>(static_cast<std::uint8_t>(zeta::holdem::terminal_engine<2>::kernel_family())),
            benchmark::Counter::kIsIterationInvariant
        );
    }

    void BM_TerminalEngineThreeWaySidePotSparse(benchmark::State& state) {
        const auto& cases = sparse_cases(50);
        zeta::holdem::terminal_engine<3> engine{};
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto& c : cases) {
                std::array<zeta::holdem::river_reach_index, 3> reach{
                    *c.oop_index,
                    *c.ip_index,
                    *c.p2_index
                };
                std::vector<zeta::holdem::pot_layer<3>> layers(2);
                layers[0].amount = 300.0;
                layers[1].amount = 100.0;
                for (std::size_t seat = 0; seat < 3; ++seat) {
                    layers[0].contributors_mask.set(seat);
                    layers[0].eligible_mask.set(seat);
                }
                layers[1].contributors_mask.set(0);
                layers[1].contributors_mask.set(1);
                layers[1].eligible_mask.set(0);
                layers[1].eligible_mask.set(1);
                auto terminal = zeta::holdem::make_terminal_state(
                    zeta::holdem::terminal_state_kind::showdown,
                    zeta::holdem::terminal_context<3>{
                        .gross_pot = 400.0,
                        .rake = 15.0,
                        .contribution = {150.0, 150.0, 100.0}
                    },
                    layers
                );
                auto values = engine.evaluate_terminal_values(c.cache, reach, terminal);
                benchmark::DoNotOptimize(values);
                sink += c.oop_active + c.ip_active + c.p2_active;
            }
        }
        benchmark::DoNotOptimize(sink);
        state.counters["kernel_family"] = benchmark::Counter(
            static_cast<double>(static_cast<std::uint8_t>(zeta::holdem::terminal_engine<3>::kernel_family())),
            benchmark::Counter::kIsIterationInvariant
        );
    }

    void BM_RiverCFRIterationDense(benchmark::State& state) {
        const auto& d = data();
        const auto& cases = d.dense_cases;
        zeta::holdem::terminal_engine<2> engine{};
        std::array<const benchmark_case*, benchmark_board_count> cached_cases{};
        for (std::size_t i = 0; i < benchmark_board_count; ++i) {
            cached_cases[i] = &cases[i];
        }

        std::array<float, zeta::holdem::combination_count> oop_regret{};
        std::array<float, zeta::holdem::combination_count> ip_regret{};
        std::array<float, zeta::holdem::combination_count> oop_strategy_sum{};
        std::array<float, zeta::holdem::combination_count> ip_strategy_sum{};

        std::uint64_t sink = 0;
        std::uint64_t regret_updates = 0;
        for (const auto* c : cached_cases) {
            regret_updates += c->oop_active + c->ip_active;
        }

        for (auto _ : state) {
            for (const auto* c : cached_cases) {
                auto values = engine.evaluate_showdown_values(
                    c->cache,
                    *c->oop_index,
                    *c->ip_index,
                    d.context
                );
                benchmark::DoNotOptimize(values);

                for (std::uint16_t oi = 0; oi < c->oop_index->active_count; ++oi) {
                    const auto combo = c->oop_index->active_indices[oi];
                    const auto value = values[zeta::holdem::player::oop][combo];
                    oop_regret[combo] += value;
                    oop_strategy_sum[combo] += std::max(oop_regret[combo], 0.0f);
                    sink += static_cast<std::uint64_t>(oop_strategy_sum[combo] > 0.0f);
                }

                for (std::uint16_t ii = 0; ii < c->ip_index->active_count; ++ii) {
                    const auto combo = c->ip_index->active_indices[ii];
                    const auto value = values[zeta::holdem::player::ip][combo];
                    ip_regret[combo] += value;
                    ip_strategy_sum[combo] += std::max(ip_regret[combo], 0.0f);
                    sink += static_cast<std::uint64_t>(ip_strategy_sum[combo] > 0.0f);
                }
            }
        }

        benchmark::DoNotOptimize(oop_regret);
        benchmark::DoNotOptimize(ip_regret);
        benchmark::DoNotOptimize(oop_strategy_sum);
        benchmark::DoNotOptimize(ip_strategy_sum);
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * regret_updates));
        state.counters["terminal_calls"] = benchmark::Counter(
            static_cast<double>(benchmark_board_count),
            benchmark::Counter::kIsIterationInvariantRate
        );
        state.counters["regret_updates"] = benchmark::Counter(
            static_cast<double>(regret_updates),
            benchmark::Counter::kIsIterationInvariantRate
        );
        state.counters["strategy_updates"] = benchmark::Counter(
            static_cast<double>(regret_updates),
            benchmark::Counter::kIsIterationInvariantRate
        );
    }

    void BM_RiverCFRIterationDenseParallel(benchmark::State& state) {
        const auto& d = data();
        const auto& cases = d.dense_cases;
        zeta::holdem::terminal_engine<2> engine{};
        std::array<const benchmark_case*, benchmark_board_count> cached_cases{};
        for (std::size_t i = 0; i < benchmark_board_count; ++i) {
            cached_cases[i] = &cases[i];
        }

        std::array<float, zeta::holdem::combination_count> oop_regret{};
        std::array<float, zeta::holdem::combination_count> ip_regret{};
        std::array<float, zeta::holdem::combination_count> oop_strategy_sum{};
        std::array<float, zeta::holdem::combination_count> ip_strategy_sum{};

        std::uint64_t sink = 0;
        std::uint64_t regret_updates = 0;
        for (const auto* c : cached_cases) {
            regret_updates += c->oop_active + c->ip_active;
        }

        for (auto _ : state) {
            for (const auto* c : cached_cases) {
                auto values = engine.evaluate_showdown_values(
                    c->cache,
                    *c->oop_index,
                    *c->ip_index,
                    d.context
                );
                benchmark::DoNotOptimize(values);

                for (std::uint16_t oi = 0; oi < c->oop_index->active_count; ++oi) {
                    const auto combo = c->oop_index->active_indices[oi];
                    const auto value = values[zeta::holdem::player::oop][combo];
                    oop_regret[combo] += value;
                    oop_strategy_sum[combo] += std::max(oop_regret[combo], 0.0f);
                    sink += static_cast<std::uint64_t>(oop_strategy_sum[combo] > 0.0f);
                }

                for (std::uint16_t ii = 0; ii < c->ip_index->active_count; ++ii) {
                    const auto combo = c->ip_index->active_indices[ii];
                    const auto value = values[zeta::holdem::player::ip][combo];
                    ip_regret[combo] += value;
                    ip_strategy_sum[combo] += std::max(ip_regret[combo], 0.0f);
                    sink += static_cast<std::uint64_t>(ip_strategy_sum[combo] > 0.0f);
                }
            }
        }

        benchmark::DoNotOptimize(oop_regret);
        benchmark::DoNotOptimize(ip_regret);
        benchmark::DoNotOptimize(oop_strategy_sum);
        benchmark::DoNotOptimize(ip_strategy_sum);
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * regret_updates));
        state.counters["threads"] = benchmark::Counter(static_cast<double>(state.threads()), benchmark::Counter::kIsIterationInvariant);
        state.counters["terminal_calls"] = benchmark::Counter(
            static_cast<double>(benchmark_board_count),
            benchmark::Counter::kIsIterationInvariantRate
        );
        state.counters["regret_updates"] = benchmark::Counter(
            static_cast<double>(regret_updates),
            benchmark::Counter::kIsIterationInvariantRate
        );
        state.counters["strategy_updates"] = benchmark::Counter(
            static_cast<double>(regret_updates),
            benchmark::Counter::kIsIterationInvariantRate
        );
    }

    void BM_TerminalEngineMultiplayerShowdownDense(benchmark::State& state) {
        const auto& d = data();
        const auto& cases = d.dense_cases;
        zeta::holdem::terminal_engine<3> engine{};
        zeta::holdem::terminal_workspace<3> workspace{};
        std::uint64_t sink = 0;
        std::uint64_t hero_combo_total = 0;
        for (const auto& c : cases) {
            hero_combo_total += c.oop_active + c.ip_active + c.p2_active;
        }
        for (auto _ : state) {
            for (const auto& c : cases) {
                auto values = engine.evaluate_showdown_values(
                    workspace,
                    c.cache,
                    std::array<zeta::holdem::reach_vector, 3>{c.oop_reach, c.ip_reach, c.p2_reach},
                    d.context3
                );
                benchmark::DoNotOptimize(values);
                sink += c.p2_buckets;
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * hero_combo_total));
        state.counters["kernel_family"] = benchmark::Counter(
            static_cast<double>(static_cast<std::uint8_t>(zeta::holdem::terminal_engine<3>::kernel_family())),
            benchmark::Counter::kIsIterationInvariant
        );
        state.counters["players"] = benchmark::Counter(3.0, benchmark::Counter::kIsIterationInvariant);
    }

    void BM_TerminalEngineMultiplayerShowdownDense4(benchmark::State& state) {
        const auto& d = data();
        const auto& cases = d.dense_cases;
        zeta::holdem::terminal_engine<4> engine{};
        zeta::holdem::terminal_workspace<4> workspace{};
        std::uint64_t sink = 0;
        std::uint64_t hero_combo_total = 0;
        for (const auto& c : cases) {
            hero_combo_total += c.oop_active + c.ip_active + c.p2_active + c.p3_active;
        }
        for (auto _ : state) {
            for (const auto& c : cases) {
                auto values = engine.evaluate_showdown_values(
                    workspace,
                    c.cache,
                    std::array<zeta::holdem::reach_vector, 4>{c.oop_reach, c.ip_reach, c.p2_reach, c.p3_reach},
                    d.context4
                );
                benchmark::DoNotOptimize(values);
                sink += c.p3_buckets;
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * hero_combo_total));
        state.counters["kernel_family"] = benchmark::Counter(
            static_cast<double>(static_cast<std::uint8_t>(zeta::holdem::terminal_engine<4>::kernel_family())),
            benchmark::Counter::kIsIterationInvariant
        );
        state.counters["players"] = benchmark::Counter(4.0, benchmark::Counter::kIsIterationInvariant);
    }

    void BM_TerminalEngineMultiplayerShowdownDense6(benchmark::State& state) {
        const auto& d = data();
        const auto& cases = d.dense_cases;
        zeta::holdem::terminal_engine<6> engine{};
        zeta::holdem::terminal_workspace<6> workspace{};
        std::uint64_t sink = 0;
        std::uint64_t hero_combo_total = 0;
        for (const auto& c : cases) {
            hero_combo_total += c.oop_active + c.ip_active + c.p2_active + c.p3_active + c.p4_active + c.p5_active;
        }
        for (auto _ : state) {
            for (const auto& c : cases) {
                auto values = engine.evaluate_showdown_values(
                    workspace,
                    c.cache,
                    std::array<zeta::holdem::reach_vector, 6>{c.oop_reach, c.ip_reach, c.p2_reach, c.p3_reach, c.p4_reach, c.p5_reach},
                    d.context6
                );
                benchmark::DoNotOptimize(values);
                sink += c.p5_buckets;
            }
        }
        benchmark::DoNotOptimize(sink);
        state.SetItemsProcessed(static_cast<std::int64_t>(state.iterations() * hero_combo_total));
        state.counters["kernel_family"] = benchmark::Counter(
            static_cast<double>(static_cast<std::uint8_t>(zeta::holdem::terminal_engine<6>::kernel_family())),
            benchmark::Counter::kIsIterationInvariant
        );
        state.counters["players"] = benchmark::Counter(6.0, benchmark::Counter::kIsIterationInvariant);
    }

}

BENCHMARK(BM_RiverTerminalCacheConstruction)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_RiverReachIndexConstructionDense)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_RiverReachIndexConstructionSparse)->Arg(50)->Arg(100)->Arg(300)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalFoldValuesDense)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalFoldValuesSparse)->Arg(50)->Arg(100)->Arg(300)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalShowdownValuesDense)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalShowdownValuesSparse)->Arg(50)->Arg(100)->Arg(300)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalEngineShowdownDense)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalEngineShowdownDensePreindexed)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalEngineShowdownDenseCached)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalEngineThreeWaySidePotSparse)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_RiverCFRIterationDense)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_RiverCFRIterationDenseParallel)
    ->Threads(1)
    ->Threads(2)
    ->Threads(4)
    ->Threads(8)
    ->Threads(12)
    ->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalEngineMultiplayerShowdownDense)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalEngineMultiplayerShowdownDense4)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_TerminalEngineMultiplayerShowdownDense6)->Unit(benchmark::kNanosecond);

int main(int argc, char** argv) {
    std::cout << "terminal evaluator : river cache + reach index + rank-sweep showdown/fold\n";
    std::cout << "boards per sample  : " << benchmark_board_count << "\n";
    std::cout << "memory bytes       : terminal_values=" << sizeof(zeta::holdem::terminal_values<2>)
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
