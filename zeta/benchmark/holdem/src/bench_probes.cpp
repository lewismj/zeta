#include <algorithm>
#include <array>
#include <atomic>
#include <cassert>
#include <chrono>
#include <cmath>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <initializer_list>
#include <iostream>
#include <numeric>
#include <random>
#include <utility>
#include <vector>

#include "evaluator.h"
#include "tables.h"

namespace {
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


    struct benchmark_options {
        std::chrono::milliseconds min_benchmark_time{2000};
        std::chrono::milliseconds stage_time{500};
        std::size_t benchmark_samples = 7;
        std::size_t evaluator_sample_hands = 200000;
        double max_random_ns = 0.0;
        double max_adversarial_ns = 0.0;
        double max_index_ns = 0.0;
    };

    [[nodiscard]] bool parse_double_arg(
        const int argc,
        char** argv,
        int& index,
        const char* option,
        double& out
    ) {
        if (index + 1 >= argc) {
            std::cerr << "Missing value for " << option << "\n";
            return false;
        }
        out = std::stod(argv[++index]);
        return true;
    }

    [[nodiscard]] benchmark_options parse_options(const int argc, char** argv) {
        benchmark_options options{};
        for (int i = 1; i < argc; ++i) {
            const std::string arg = argv[i];
            if (arg == "--quick") {
                options.min_benchmark_time = std::chrono::milliseconds{100};
                options.stage_time = std::chrono::milliseconds{50};
                options.benchmark_samples = 3;
                options.evaluator_sample_hands = 20000;
            } else if (arg == "--max-random-ns") {
                if (!parse_double_arg(argc, argv, i, "--max-random-ns", options.max_random_ns)) {
                    std::exit(2);
                }
            } else if (arg == "--max-adversarial-ns") {
                if (!parse_double_arg(argc, argv, i, "--max-adversarial-ns", options.max_adversarial_ns)) {
                    std::exit(2);
                }
            } else if (arg == "--max-index-ns") {
                if (!parse_double_arg(argc, argv, i, "--max-index-ns", options.max_index_ns)) {
                    std::exit(2);
                }
            } else {
                std::cerr << "Unknown option: " << arg << "\n";
                std::exit(2);
            }
        }
        return options;
    }

    [[nodiscard]] bool threshold_failed(const char* name, const double measured, const double threshold) {
        if (threshold <= 0.0 || measured <= threshold) {
            return false;
        }
        std::cerr << "Benchmark regression: " << name << " measured " << measured
                  << " ns, threshold " << threshold << " ns\n";
        return true;
    }
    struct summary_stats {
        double min{};
        double median{};
        double mean{};
        double stddev{};
    };

    [[nodiscard]] summary_stats summarize_samples(std::vector<double> samples) {
        assert(!samples.empty());
        std::sort(samples.begin(), samples.end());
        const std::size_t n = samples.size();
        const double median = (n % 2 == 0)
            ? 0.5 * (samples[(n / 2) - 1] + samples[n / 2])
            : samples[n / 2];
        const double mean = std::accumulate(samples.begin(), samples.end(), 0.0) / static_cast<double>(n);
        double variance = 0.0;
        for (const double value : samples) {
            const double delta = value - mean;
            variance += delta * delta;
        }
        variance /= static_cast<double>(n);
        return summary_stats{
            .min = samples.front(),
            .median = median,
            .mean = mean,
            .stddev = std::sqrt(variance)
        };
    }

    struct benchmark_sample {
        double ns_per_op{};
        double ops_per_sec{};
        std::size_t rounds{};
        std::uint64_t sink{};
    };

    template<typename RoundFn>
    [[nodiscard]] benchmark_sample run_benchmark_sample(
        const std::size_t ops_per_round,
        const std::chrono::nanoseconds min_duration,
        RoundFn&& run_round
    ) {
        assert(ops_per_round > 0);
        std::uint64_t sink = 0;

        run_round(sink); // warmup
        std::atomic_signal_fence(std::memory_order_seq_cst);

        std::size_t rounds = 0;
        const auto started = std::chrono::steady_clock::now();
        auto elapsed = std::chrono::nanoseconds{0};
        do {
            run_round(sink);
            ++rounds;
            elapsed = std::chrono::duration_cast<std::chrono::nanoseconds>(std::chrono::steady_clock::now() - started);
        } while (elapsed < min_duration);
        std::atomic_signal_fence(std::memory_order_seq_cst);

        const double total_ops = static_cast<double>(rounds) * static_cast<double>(ops_per_round);
        const double total_ns = static_cast<double>(elapsed.count());
        const double ns_per_op = total_ns / total_ops;
        const double ops_per_sec = (total_ops * 1'000'000'000.0) / total_ns;
        return benchmark_sample{
            .ns_per_op = ns_per_op,
            .ops_per_sec = ops_per_sec,
            .rounds = rounds,
            .sink = sink
        };
    }

    struct benchmark_series {
        summary_stats ns_stats{};
        summary_stats throughput_stats{};
        std::size_t rounds_total{};
        std::uint64_t sink{};
    };

    template<typename RoundFn>
    [[nodiscard]] benchmark_series run_benchmark_series(
        const std::size_t ops_per_round,
        const std::chrono::nanoseconds min_duration,
        const std::size_t samples,
        RoundFn&& run_round
    ) {
        assert(samples > 0);
        std::vector<double> ns_per_op_samples;
        std::vector<double> throughput_samples;
        ns_per_op_samples.reserve(samples);
        throughput_samples.reserve(samples);

        std::size_t rounds_total = 0;
        std::uint64_t sink = 0;
        for (std::size_t sample = 0; sample < samples; ++sample) {
            const auto measured = run_benchmark_sample(ops_per_round, min_duration, run_round);
            ns_per_op_samples.push_back(measured.ns_per_op);
            throughput_samples.push_back(measured.ops_per_sec);
            rounds_total += measured.rounds;
            sink ^= measured.sink;
        }

        return benchmark_series{
            .ns_stats = summarize_samples(std::move(ns_per_op_samples)),
            .throughput_stats = summarize_samples(std::move(throughput_samples)),
            .rounds_total = rounds_total,
            .sink = sink
        };
    }
}

int main(const int argc, char** argv) {
    const auto options = parse_options(argc, argv);
    const auto& runtime_table = zeta::holdem::lookup::non_flush_table;

    const auto min_benchmark_time = options.min_benchmark_time;
    const auto benchmark_samples = options.benchmark_samples;
    constexpr std::size_t corpus_permutations = 8;

    std::size_t zero_rank_count = 0;
    for (const auto rank : runtime_table) {
        if (rank.value == 0) {
            ++zero_rank_count;
        }
    }
    std::cout << "non-flush indexing : restricted quinary perfect index\n";
    std::cout << "non-flush slots    : " << runtime_table.size() << "\n";
    std::cout << "zero-rank slots    : " << zero_rank_count << "\n";
    constexpr auto rank_table_bytes = zeta::holdem::lookup::non_flush_quinary_table_size * sizeof(runtime_table[0]);
    constexpr auto index_table_bytes = sizeof(zeta::holdem::lookup::quinary_chunk0)
        + sizeof(zeta::holdem::lookup::quinary_chunk1)
        + sizeof(zeta::holdem::lookup::quinary_chunk2);
    std::cout << "rank table bytes   : " << rank_table_bytes << "\n";
    std::cout << "index table bytes  : " << index_table_bytes << "\n";
    std::cout << "total table bytes  : " << (rank_table_bytes + index_table_bytes) << "\n";

    std::vector<std::size_t> lookup_indices(runtime_table.size());
    std::iota(lookup_indices.begin(), lookup_indices.end(), std::size_t{0});
    auto lookup_index_corpora = build_shuffled_corpora(lookup_indices, corpus_permutations, 0xA11CEULL);
    std::size_t lookup_corpus_index = 0;
    auto lookup_round = [&](std::uint64_t& sink) {
        const auto& indices = lookup_index_corpora[lookup_corpus_index % lookup_index_corpora.size()];
        ++lookup_corpus_index;
        for (const auto index : indices) {
            sink += runtime_table[index].value;
        }
    };
    const auto lookup_series = run_benchmark_series(
        lookup_indices.size(),
        min_benchmark_time,
        benchmark_samples,
        lookup_round
    );

    std::cout << "lookup benchmark (dense table, shuffled index corpus):\n";
    std::cout << "samples               : " << benchmark_samples << "\n";
    std::cout << "min time/sample (ms)  : " << min_benchmark_time.count() << "\n";
    std::cout << "ns per lookup (median): " << lookup_series.ns_stats.median << "\n";
    std::cout << "ns per lookup (min)   : " << lookup_series.ns_stats.min << "\n";
    std::cout << "ns per lookup (stddev): " << lookup_series.ns_stats.stddev << "\n";
    std::cout << "lookups/sec (median)  : " << lookup_series.throughput_stats.median << "\n";
    std::cout << "lookups/sec (stddev)  : " << lookup_series.throughput_stats.stddev << "\n";
    std::cout << "sink                  : " << lookup_series.sink << "\n";
    std::cout << "\n";

    const auto evaluator_sample_hands = options.evaluator_sample_hands;
    const auto random_hands = build_random_hands(evaluator_sample_hands, 0xC0FFEE1234ULL);
    const auto random_hand_corpora = build_shuffled_corpora(random_hands, 4, 0x51F7EDULL);
    std::size_t random_corpus_index = 0;
    auto random_eval_round = [&](std::uint64_t& sink) {
        const auto& hands = random_hand_corpora[random_corpus_index % random_hand_corpora.size()];
        ++random_corpus_index;
        for (const auto hand : hands) {
            sink += zeta::holdem::evaluate(hand).value;
        }
    };
    const auto random_eval_series = run_benchmark_series(
        random_hands.size(),
        min_benchmark_time,
        benchmark_samples,
        random_eval_round
    );

    const auto adversarial_base = build_adversarial_hands();
    const auto adversarial_hands = repeat_to_size(adversarial_base, evaluator_sample_hands);
    const auto adversarial_corpora = build_shuffled_corpora(adversarial_hands, 4, 0xBAD5EEDULL);
    std::size_t adversarial_corpus_index = 0;
    auto adversarial_eval_round = [&](std::uint64_t& sink) {
        const auto& hands = adversarial_corpora[adversarial_corpus_index % adversarial_corpora.size()];
        ++adversarial_corpus_index;
        for (const auto hand : hands) {
            sink += zeta::holdem::evaluate(hand).value;
        }
    };
    const auto adversarial_eval_series = run_benchmark_series(
        adversarial_hands.size(),
        min_benchmark_time,
        benchmark_samples,
        adversarial_eval_round
    );

    std::cout << "full evaluator benchmark (random corpus):\n";
    std::cout << "sample hands            : " << random_hands.size() << "\n";
    std::cout << "samples                 : " << benchmark_samples << "\n";
    std::cout << "min time/sample (ms)    : " << min_benchmark_time.count() << "\n";
    std::cout << "ns per evaluate (median): " << random_eval_series.ns_stats.median << "\n";
    std::cout << "ns per evaluate (min)   : " << random_eval_series.ns_stats.min << "\n";
    std::cout << "ns per evaluate (stddev): " << random_eval_series.ns_stats.stddev << "\n";
    std::cout << "evals/sec (median)      : " << random_eval_series.throughput_stats.median << "\n";
    std::cout << "evals/sec (stddev)      : " << random_eval_series.throughput_stats.stddev << "\n";
    std::cout << "sink                    : " << random_eval_series.sink << "\n";
    std::cout << "\n";

    std::cout << "full evaluator benchmark (adversarial corpus):\n";
    std::cout << "sample hands            : " << adversarial_hands.size() << "\n";
    std::cout << "patterns                : " << adversarial_base.size() << "\n";
    std::cout << "samples                 : " << benchmark_samples << "\n";
    std::cout << "min time/sample (ms)    : " << min_benchmark_time.count() << "\n";
    std::cout << "ns per evaluate (median): " << adversarial_eval_series.ns_stats.median << "\n";
    std::cout << "ns per evaluate (min)   : " << adversarial_eval_series.ns_stats.min << "\n";
    std::cout << "ns per evaluate (stddev): " << adversarial_eval_series.ns_stats.stddev << "\n";
    std::cout << "evals/sec (median)      : " << adversarial_eval_series.throughput_stats.median << "\n";
    std::cout << "evals/sec (stddev)      : " << adversarial_eval_series.throughput_stats.stddev << "\n";
    std::cout << "sink                    : " << adversarial_eval_series.sink << "\n";
    std::cout << "\n";

    // Diagnostic stage decomposition for the evaluator hot path.
    std::vector<zeta::holdem::hand_masks> non_flush_masks;
    non_flush_masks.reserve(random_hands.size());
    for (const auto hand : random_hands) {
        const auto masks = zeta::holdem::suit_rank_masks(hand);
        zeta::suit flush = zeta::suit::spades;
        if (!zeta::holdem::find_flush_suit(masks, flush)) {
            non_flush_masks.push_back(masks);
        }
    }
    assert(!non_flush_masks.empty());

    std::vector<std::size_t> non_flush_indices;
    non_flush_indices.reserve(non_flush_masks.size());
    for (const auto& masks : non_flush_masks) {
        non_flush_indices.push_back(zeta::holdem::non_flush_quinary_index(masks));
    }
    auto non_flush_index_corpora = build_shuffled_corpora(non_flush_indices, 4, 0xD15EA5EULL);
    std::size_t stage_index_corpus_index = 0;

    const auto stage_time = options.stage_time;

    auto index_only_round = [&](std::uint64_t& sink) {
        for (const auto& masks : non_flush_masks) {
            sink += zeta::holdem::non_flush_quinary_index(masks);
        }
    };
    const auto index_only_sample = run_benchmark_sample(non_flush_masks.size(), stage_time, index_only_round);

    auto stage_masks_round = [&](std::uint64_t& sink) {
        for (const auto hand : random_hands) {
            const auto masks = zeta::holdem::suit_rank_masks(hand);
            sink += static_cast<std::uint64_t>(masks.spades ^ masks.hearts ^ masks.diamonds ^ masks.clubs);
        }
    };
    const auto stage_masks_sample = run_benchmark_sample(random_hands.size(), stage_time, stage_masks_round);

    auto stage_flush_round = [&](std::uint64_t& sink) {
        for (const auto hand : random_hands) {
            const auto masks = zeta::holdem::suit_rank_masks(hand);
            zeta::suit flush = zeta::suit::spades;
            if (zeta::holdem::find_flush_suit(masks, flush)) {
                sink += zeta::holdem::flush_index(masks, flush);
            } else {
                sink += static_cast<std::uint64_t>(masks.spades ^ masks.hearts ^ masks.diamonds ^ masks.clubs);
            }
        }
    };
    const auto stage_flush_sample = run_benchmark_sample(random_hands.size(), stage_time, stage_flush_round);

    auto stage_index_round = [&](std::uint64_t& sink) {
        for (const auto hand : random_hands) {
            const auto masks = zeta::holdem::suit_rank_masks(hand);
            zeta::suit flush = zeta::suit::spades;
            if (zeta::holdem::find_flush_suit(masks, flush)) {
                sink += zeta::holdem::flush_index(masks, flush);
            } else {
                sink += zeta::holdem::non_flush_quinary_index(masks);
            }
        }
    };
    const auto stage_index_sample = run_benchmark_sample(random_hands.size(), stage_time, stage_index_round);


    auto stage_dense_lookup_round = [&](std::uint64_t& sink) {
        const auto& indices = non_flush_index_corpora[stage_index_corpus_index % non_flush_index_corpora.size()];
        ++stage_index_corpus_index;
        for (const auto index : indices) {
            sink += runtime_table[index].value;
        }
    };
    const auto stage_dense_lookup_sample = run_benchmark_sample(non_flush_indices.size(), stage_time, stage_dense_lookup_round);

    std::cout << "staged evaluator profile (same corpus):\n";
    std::cout << "sample hands           : " << random_hands.size() << "\n";
    std::cout << "non-flush masks        : " << non_flush_masks.size() << "\n";
    std::cout << "min stage time (ms)    : " << stage_time.count() << "\n";
    std::cout << "isolated quinary index : " << index_only_sample.ns_per_op << " ns (sink " << index_only_sample.sink << ")\n";
    std::cout << "masks-only ns          : " << stage_masks_sample.ns_per_op << " (sink " << stage_masks_sample.sink << ")\n";
    std::cout << "masks+flush-check ns   : " << stage_flush_sample.ns_per_op << " (sink " << stage_flush_sample.sink << ")\n";
    std::cout << "masks+flush+index ns   : " << stage_index_sample.ns_per_op << " (sink " << stage_index_sample.sink << ")\n";
    std::cout << "dense-lookup-only ns   : " << stage_dense_lookup_sample.ns_per_op << " (sink " << stage_dense_lookup_sample.sink << ")\n";
    std::cout << "full evaluate ns (med) : " << random_eval_series.ns_stats.median << " (sink " << random_eval_series.sink << ")\n";
    std::cout << "delta flush-check      : " << (stage_flush_sample.ns_per_op - stage_masks_sample.ns_per_op) << "\n";
    std::cout << "delta index            : " << (stage_index_sample.ns_per_op - stage_flush_sample.ns_per_op) << "\n";
    std::cout << "delta remainder        : " << (random_eval_series.ns_stats.median - stage_index_sample.ns_per_op) << "\n";

    const bool failed = threshold_failed("random full evaluate", random_eval_series.ns_stats.median, options.max_random_ns)
        || threshold_failed("adversarial full evaluate", adversarial_eval_series.ns_stats.median, options.max_adversarial_ns)
        || threshold_failed("isolated quinary index", index_only_sample.ns_per_op, options.max_index_ns);

    return failed ? 1 : 0;
}
