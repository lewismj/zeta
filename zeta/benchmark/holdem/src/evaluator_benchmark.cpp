#include <algorithm>
#include <array>
#include <cstdint>
#include <initializer_list>
#include <iostream>
#include <numeric>
#include <random>
#include <utility>
#include <vector>

#include <benchmark/benchmark.h>

#include "evaluator.h"
#include "tables.h"

namespace {
    constexpr std::size_t evaluator_sample_hands = 200000;
    constexpr std::size_t corpus_permutations = 8;
    constexpr std::int64_t all_seven_card_hands = 133784560;

    [[nodiscard]] std::vector<zeta::card_mask> build_random_hands(const std::size_t hand_count, const uint64_t seed) {
        std::mt19937_64 rng(seed);
        std::uniform_int_distribution<int> dist(0, 51);

        std::vector<zeta::card_mask> hands;
        hands.reserve(hand_count);
        for (std::size_t h = 0; h < hand_count; ++h) {
            std::array<uint8_t, 52> used{};
            zeta::card_mask hand = 0;
            std::size_t selected = 0;
            while (selected < 7) {
                const int card = dist(rng);
                if (used[card] != 0) {
                    continue;
                }
                used[card] = 1;
                hand |= (zeta::card_mask{1} << card);
                ++selected;
            }
            hands.push_back(hand);
        }
        return hands;
    }

    [[nodiscard]] constexpr zeta::card_mask card(const int suit, const int rank) {
        return zeta::card_mask{1} << (suit * 13 + rank);
    }

    [[nodiscard]] constexpr zeta::card_mask hand7(std::initializer_list<std::pair<int, int>> cards) {
        zeta::card_mask m = 0;
        for (const auto [s, r] : cards) {
            m |= card(s, r);
        }
        return m;
    }

    [[nodiscard]] std::vector<zeta::card_mask> build_adversarial_hands() {
        return {
            hand7({{0, 12}, {1, 12}, {2, 12}, {3, 12}, {0, 11}, {1, 10}, {2, 9}}),
            hand7({{0, 12}, {1, 12}, {2, 11}, {3, 11}, {0, 10}, {1, 10}, {2, 9}}),
            hand7({{0, 8}, {0, 7}, {0, 6}, {0, 5}, {0, 4}, {1, 12}, {2, 11}}),
            hand7({{0, 12}, {1, 11}, {2, 10}, {3, 9}, {0, 8}, {1, 7}, {2, 6}}),
            hand7({{0, 0}, {1, 0}, {2, 0}, {3, 5}, {0, 5}, {1, 5}, {2, 12}}),
            hand7({{0, 2}, {1, 4}, {2, 6}, {3, 8}, {0, 10}, {1, 12}, {2, 1}}),
            hand7({{0, 3}, {1, 3}, {2, 3}, {3, 3}, {0, 2}, {1, 2}, {2, 2}}),
            hand7({{1, 8}, {1, 7}, {1, 6}, {1, 5}, {1, 3}, {0, 12}, {2, 0}})
        };
    }

    [[nodiscard]] std::vector<zeta::card_mask> repeat_to_size(
        const std::vector<zeta::card_mask>& base,
        const std::size_t target_size
    ) {
        std::vector<zeta::card_mask> out;
        out.reserve(target_size);
        while (out.size() < target_size) {
            out.insert(out.end(), base.begin(), base.end());
        }
        out.resize(target_size);
        return out;
    }

    template<typename T>
    [[nodiscard]] std::vector<std::vector<T>> build_shuffled_corpora(
        const std::vector<T>& base,
        const std::size_t corpus_count,
        const uint64_t seed
    ) {
        std::vector<std::vector<T>> corpora;
        corpora.reserve(corpus_count);
        corpora.push_back(base);
        std::mt19937_64 rng(seed);
        for (std::size_t i = 1; i < corpus_count; ++i) {
            auto copy = base;
            std::shuffle(copy.begin(), copy.end(), rng);
            corpora.push_back(std::move(copy));
        }
        return corpora;
    }

    struct quinary_layers {
        uint16_t ones{};
        uint16_t twos{};
        uint16_t threes{};
        uint16_t fours{};
    };

    [[nodiscard]] quinary_layers layers_from_masks(const zeta::holdem::hand_masks& masks) {
        const auto key = zeta::holdem::non_flush_key(masks);
        return quinary_layers{
            .ones = static_cast<uint16_t>(key),
            .twos = static_cast<uint16_t>(key >> 13),
            .threes = static_cast<uint16_t>(key >> 26),
            .fours = static_cast<uint16_t>(key >> 39)
        };
    }

    struct holdem_benchmark_data {
        const std::array<zeta::holdem::hand_rank, zeta::holdem::lookup::non_flush_quinary_table_size>& runtime_table =
            zeta::holdem::lookup::non_flush_table;
        std::vector<std::size_t> lookup_indices;
        std::vector<std::vector<std::size_t>> lookup_index_corpora;
        std::vector<zeta::card_mask> random_hands;
        std::vector<zeta::holdem::hand_masks> random_masks;
        std::vector<std::vector<zeta::card_mask>> random_hand_corpora;
        std::vector<zeta::card_mask> adversarial_hands;
        std::vector<std::vector<zeta::card_mask>> adversarial_corpora;
        std::vector<zeta::holdem::hand_masks> non_flush_masks;
        std::vector<std::size_t> non_flush_indices;
        std::vector<quinary_layers> non_flush_layers;
        std::vector<std::vector<std::size_t>> non_flush_index_corpora;
        std::size_t zero_rank_count{};

        holdem_benchmark_data() {
            for (const auto rank : runtime_table) {
                if (rank.value == 0) {
                    ++zero_rank_count;
                }
            }

            lookup_indices.resize(runtime_table.size());
            std::iota(lookup_indices.begin(), lookup_indices.end(), std::size_t{0});
            lookup_index_corpora = build_shuffled_corpora(lookup_indices, corpus_permutations, 0xA11CEULL);

            random_hands = build_random_hands(evaluator_sample_hands, 0xC0FFEE1234ULL);
            random_hand_corpora = build_shuffled_corpora(random_hands, 4, 0x51F7EDULL);
            random_masks.reserve(random_hands.size());
            for (const auto hand : random_hands) {
                random_masks.push_back(zeta::holdem::suit_rank_masks(hand));
            }

            adversarial_hands = repeat_to_size(build_adversarial_hands(), evaluator_sample_hands);
            adversarial_corpora = build_shuffled_corpora(adversarial_hands, 4, 0xBAD5EEDULL);

            non_flush_masks.reserve(random_hands.size());
            for (const auto hand : random_hands) {
                const auto masks = zeta::holdem::suit_rank_masks(hand);
                zeta::suit flush = zeta::suit::spades;
                if (!zeta::holdem::find_flush_suit(masks, flush)) {
                    non_flush_masks.push_back(masks);
                }
            }

            non_flush_indices.reserve(non_flush_masks.size());
            non_flush_layers.reserve(non_flush_masks.size());
            for (const auto& masks : non_flush_masks) {
                non_flush_indices.push_back(zeta::holdem::non_flush_quinary_index(masks));
                non_flush_layers.push_back(layers_from_masks(masks));
            }
            non_flush_index_corpora = build_shuffled_corpora(non_flush_indices, 4, 0xD15EA5EULL);
        }
    };

    [[nodiscard]] const holdem_benchmark_data& data() {
        static const holdem_benchmark_data instance;
        return instance;
    }

    void add_common_counters(benchmark::State& state, const std::uint64_t sink) {
        state.counters["sink"] = static_cast<double>(sink);
    }

    void BM_DenseTableLookup(benchmark::State& state) {
        const auto& d = data();
        std::uint64_t sink = 0;
        std::size_t corpus = 0;
        for (auto _ : state) {
            const auto& indices = d.lookup_index_corpora[corpus++ % d.lookup_index_corpora.size()];
            for (const auto index : indices) {
                benchmark::DoNotOptimize(sink += d.runtime_table[index].value);
            }
        }
        state.SetItemsProcessed(static_cast<int64_t>(state.iterations() * d.lookup_indices.size()));
        add_common_counters(state, sink);
    }

    void BM_FullEvaluateRandom(benchmark::State& state) {
        const auto& d = data();
        std::uint64_t sink = 0;
        std::size_t corpus = 0;
        for (auto _ : state) {
            const auto& hands = d.random_hand_corpora[corpus++ % d.random_hand_corpora.size()];
            for (const auto hand : hands) {
                benchmark::DoNotOptimize(sink += zeta::holdem::evaluate(hand).value);
            }
        }
        state.SetItemsProcessed(static_cast<int64_t>(state.iterations() * d.random_hands.size()));
        add_common_counters(state, sink);
    }

    void BM_FullEvaluateAdversarial(benchmark::State& state) {
        const auto& d = data();
        std::uint64_t sink = 0;
        std::size_t corpus = 0;
        for (auto _ : state) {
            const auto& hands = d.adversarial_corpora[corpus++ % d.adversarial_corpora.size()];
            for (const auto hand : hands) {
                benchmark::DoNotOptimize(sink += zeta::holdem::evaluate(hand).value);
            }
        }
        state.SetItemsProcessed(static_cast<int64_t>(state.iterations() * d.adversarial_hands.size()));
        add_common_counters(state, sink);
    }

    void BM_EvaluateFromMasksRandom(benchmark::State& state) {
        const auto& d = data();
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto& masks : d.random_masks) {
                benchmark::DoNotOptimize(sink += zeta::holdem::evaluate(masks).value);
            }
        }
        state.SetItemsProcessed(static_cast<int64_t>(state.iterations() * d.random_masks.size()));
        add_common_counters(state, sink);
    }

    void BM_IsolatedQuinaryIndex(benchmark::State& state) {
        const auto& d = data();
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto& masks : d.non_flush_masks) {
                benchmark::DoNotOptimize(sink += zeta::holdem::non_flush_quinary_index(masks));
            }
        }
        state.SetItemsProcessed(static_cast<int64_t>(state.iterations() * d.non_flush_masks.size()));
        add_common_counters(state, sink);
    }

    void BM_QuinaryIndexFromLayers(benchmark::State& state) {
        const auto& d = data();
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto& layers : d.non_flush_layers) {
                benchmark::DoNotOptimize(sink += zeta::holdem::lookup::quinary_index_from_layers(
                    layers.ones,
                    layers.twos,
                    layers.threes,
                    layers.fours
                ));
            }
        }
        state.SetItemsProcessed(static_cast<int64_t>(state.iterations() * d.non_flush_layers.size()));
        add_common_counters(state, sink);
    }

    void BM_MasksOnly(benchmark::State& state) {
        const auto& d = data();
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto hand : d.random_hands) {
                const auto masks = zeta::holdem::suit_rank_masks(hand);
                benchmark::DoNotOptimize(sink += static_cast<std::uint64_t>(masks.spades ^ masks.hearts ^ masks.diamonds ^ masks.clubs));
            }
        }
        state.SetItemsProcessed(static_cast<int64_t>(state.iterations() * d.random_hands.size()));
        add_common_counters(state, sink);
    }

    void BM_MasksFlushCheck(benchmark::State& state) {
        const auto& d = data();
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto hand : d.random_hands) {
                const auto masks = zeta::holdem::suit_rank_masks(hand);
                zeta::suit flush = zeta::suit::spades;
                if (zeta::holdem::find_flush_suit(masks, flush)) {
                    benchmark::DoNotOptimize(sink += zeta::holdem::flush_index(masks, flush));
                } else {
                    benchmark::DoNotOptimize(sink += static_cast<std::uint64_t>(masks.spades ^ masks.hearts ^ masks.diamonds ^ masks.clubs));
                }
            }
        }
        state.SetItemsProcessed(static_cast<int64_t>(state.iterations() * d.random_hands.size()));
        add_common_counters(state, sink);
    }

    void BM_MasksFlushIndex(benchmark::State& state) {
        const auto& d = data();
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (const auto hand : d.random_hands) {
                const auto masks = zeta::holdem::suit_rank_masks(hand);
                zeta::suit flush = zeta::suit::spades;
                if (zeta::holdem::find_flush_suit(masks, flush)) {
                    benchmark::DoNotOptimize(sink += zeta::holdem::flush_index(masks, flush));
                } else {
                    benchmark::DoNotOptimize(sink += zeta::holdem::non_flush_quinary_index(masks));
                }
            }
        }
        state.SetItemsProcessed(static_cast<int64_t>(state.iterations() * d.random_hands.size()));
        add_common_counters(state, sink);
    }

    void BM_DenseLookupOnly(benchmark::State& state) {
        const auto& d = data();
        std::uint64_t sink = 0;
        std::size_t corpus = 0;
        for (auto _ : state) {
            const auto& indices = d.non_flush_index_corpora[corpus++ % d.non_flush_index_corpora.size()];
            for (const auto index : indices) {
                benchmark::DoNotOptimize(sink += d.runtime_table[index].value);
            }
        }
        state.SetItemsProcessed(static_cast<int64_t>(state.iterations() * d.non_flush_indices.size()));
        add_common_counters(state, sink);
    }

    void BM_EvaluateAllSevenCards(benchmark::State& state) {
        std::uint64_t sink = 0;
        for (auto _ : state) {
            for (int c0 = 0; c0 < 46; ++c0) {
                const auto b0 = zeta::card_mask{1} << c0;
                for (int c1 = c0 + 1; c1 < 47; ++c1) {
                    const auto b1 = b0 | (zeta::card_mask{1} << c1);
                    for (int c2 = c1 + 1; c2 < 48; ++c2) {
                        const auto b2 = b1 | (zeta::card_mask{1} << c2);
                        for (int c3 = c2 + 1; c3 < 49; ++c3) {
                            const auto b3 = b2 | (zeta::card_mask{1} << c3);
                            for (int c4 = c3 + 1; c4 < 50; ++c4) {
                                const auto b4 = b3 | (zeta::card_mask{1} << c4);
                                for (int c5 = c4 + 1; c5 < 51; ++c5) {
                                    const auto b5 = b4 | (zeta::card_mask{1} << c5);
                                    for (int c6 = c5 + 1; c6 < 52; ++c6) {
                                        const auto hand = b5 | (zeta::card_mask{1} << c6);
                                        benchmark::DoNotOptimize(sink += zeta::holdem::evaluate(hand).value);
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        state.SetItemsProcessed(state.iterations() * all_seven_card_hands);
        add_common_counters(state, sink);
    }
}

BENCHMARK(BM_DenseTableLookup)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_FullEvaluateRandom)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_FullEvaluateAdversarial)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_EvaluateFromMasksRandom)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_IsolatedQuinaryIndex)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_QuinaryIndexFromLayers)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_MasksOnly)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_MasksFlushCheck)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_MasksFlushIndex)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_DenseLookupOnly)->Unit(benchmark::kNanosecond);
BENCHMARK(BM_EvaluateAllSevenCards)->Unit(benchmark::kNanosecond);

int main(int argc, char** argv) {
    const auto& d = data();
    constexpr auto rank_table_bytes = zeta::holdem::lookup::non_flush_quinary_table_size * sizeof(d.runtime_table[0]);
    constexpr auto index_table_bytes = sizeof(zeta::holdem::lookup::quinary_chunk0)
        + sizeof(zeta::holdem::lookup::quinary_chunk1)
        + sizeof(zeta::holdem::lookup::quinary_chunk2);

    std::cout << "non-flush indexing : restricted quinary perfect index\n";
    std::cout << "non-flush slots    : " << d.runtime_table.size() << "\n";
    std::cout << "zero-rank slots    : " << d.zero_rank_count << "\n";
    std::cout << "rank table bytes   : " << rank_table_bytes << "\n";
    std::cout << "index table bytes  : " << index_table_bytes << "\n";
    std::cout << "total table bytes  : " << (rank_table_bytes + index_table_bytes) << "\n";
    std::cout << "sample hands       : " << d.random_hands.size() << "\n";
    std::cout << "non-flush masks    : " << d.non_flush_masks.size() << "\n";
    std::cout << "adversarial patterns: " << build_adversarial_hands().size() << "\n\n";

    benchmark::Initialize(&argc, argv);
    if (benchmark::ReportUnrecognizedArguments(argc, argv)) {
        return 1;
    }
    benchmark::RunSpecifiedBenchmarks();
    benchmark::Shutdown();
    return 0;
}