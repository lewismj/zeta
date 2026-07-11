#include "tables.h"

#include <algorithm>
#include <array>
#include <cstdlib>
#include <cstdint>
#include <vector>

namespace zeta::holdem::lookup {
    namespace {

        constexpr uint32_t category_shift = 24;
        constexpr uint32_t rank_nibble = 4;

        [[nodiscard]] constexpr hand_rank make_rank(const hand_category category, const uint32_t tie_break) noexcept {
            return hand_rank{(static_cast<uint32_t>(category) << category_shift) | tie_break};
        }

        [[nodiscard]] constexpr uint32_t pack5(const int a, const int b = 0, const int c = 0, const int d = 0, const int e = 0) noexcept {
            return (static_cast<uint32_t>(a) << (rank_nibble * 4))
                 | (static_cast<uint32_t>(b) << (rank_nibble * 3))
                 | (static_cast<uint32_t>(c) << (rank_nibble * 2))
                 | (static_cast<uint32_t>(d) << (rank_nibble * 1))
                 | (static_cast<uint32_t>(e));
        }

        [[nodiscard]] int straight_high(const uint16_t mask) noexcept {
            for (int hi = 12; hi >= 4; --hi) {
                const uint16_t run = static_cast<uint16_t>(0x1Fu << (hi - 4));
                if ((mask & run) == run) {
                    return hi;
                }
            }

            const uint16_t wheel = static_cast<uint16_t>((1u << 12) | 0x0Fu);
            if ((mask & wheel) == wheel) {
                return 3;
            }

            return -1;
        }

        [[nodiscard]] std::array<int, 5> top_ranks_from_mask(const uint16_t mask) noexcept {
            std::array<int, 5> out{};
            int idx = 0;
            for (int r = 12; r >= 0 && idx < 5; --r) {
                if (mask & (static_cast<uint16_t>(1u) << r)) {
                    out[idx++] = r;
                }
            }
            return out;
        }

        [[nodiscard]] hand_rank evaluate_flush_mask(const uint16_t rank_mask) noexcept {
            if (rank_mask == 0) {
                return {};
            }

            const int hi = straight_high(rank_mask);
            if (hi >= 0) {
                return make_rank(hand_category::straight_flush, static_cast<uint32_t>(hi));
            }

            const auto top = top_ranks_from_mask(rank_mask);
            return make_rank(hand_category::flush, pack5(top[0], top[1], top[2], top[3], top[4]));
        }

        [[nodiscard]] hand_rank evaluate_non_flush_counts(const std::array<uint8_t, 13>& counts) noexcept {
            uint16_t presence = 0;
            std::vector<int> quads;
            std::vector<int> trips;
            std::vector<int> pairs;
            std::vector<int> singles;

            quads.reserve(2);
            trips.reserve(3);
            pairs.reserve(6);
            singles.reserve(13);

            for (int r = 12; r >= 0; --r) {
                const uint8_t c = counts[r];
                if (c == 0) {
                    continue;
                }

                presence = static_cast<uint16_t>(presence | (static_cast<uint16_t>(1u) << r));

                if (c == 4) {
                    quads.push_back(r);
                }
                if (c >= 3) {
                    trips.push_back(r);
                }
                if (c >= 2) {
                    pairs.push_back(r);
                }
                if (c >= 1) {
                    singles.push_back(r);
                }
            }

            if (!quads.empty()) {
                int kicker = 0;
                for (int r = 12; r >= 0; --r) {
                    if (r != quads[0] && counts[r] > 0) {
                        kicker = r;
                        break;
                    }
                }
                return make_rank(hand_category::quads, pack5(quads[0], kicker));
            }

            if (!trips.empty()) {
                int pair_rank = -1;
                for (const int r : pairs) {
                    if (r != trips[0]) {
                        pair_rank = r;
                        break;
                    }
                }
                if (pair_rank >= 0) {
                    return make_rank(hand_category::full_house, pack5(trips[0], pair_rank));
                }
            }

            const int straight = straight_high(presence);
            if (straight >= 0) {
                return make_rank(hand_category::straight, static_cast<uint32_t>(straight));
            }

            if (!trips.empty()) {
                std::array<int, 2> kickers{};
                int k = 0;
                for (int r = 12; r >= 0 && k < 2; --r) {
                    if (r != trips[0] && counts[r] > 0) {
                        kickers[k++] = r;
                    }
                }
                return make_rank(hand_category::trips, pack5(trips[0], kickers[0], kickers[1]));
            }

            if (pairs.size() >= 2) {
                int kicker = 0;
                for (int r = 12; r >= 0; --r) {
                    if (r != pairs[0] && r != pairs[1] && counts[r] > 0) {
                        kicker = r;
                        break;
                    }
                }
                return make_rank(hand_category::two_pair, pack5(pairs[0], pairs[1], kicker));
            }

            if (pairs.size() == 1) {
                std::array<int, 3> kickers{};
                int k = 0;
                for (int r = 12; r >= 0 && k < 3; --r) {
                    if (r != pairs[0] && counts[r] > 0) {
                        kickers[k++] = r;
                    }
                }
                return make_rank(hand_category::pair, pack5(pairs[0], kickers[0], kickers[1], kickers[2]));
            }

            const auto top = top_ranks_from_mask(presence);
            return make_rank(hand_category::high_card, pack5(top[0], top[1], top[2], top[3], top[4]));
        }

        [[nodiscard]] uint64_t state_key_from_counts(const std::array<uint8_t, 13>& counts) noexcept {
            uint16_t ones = 0;
            uint16_t twos = 0;
            uint16_t threes = 0;
            uint16_t fours = 0;

            for (int r = 0; r < 13; ++r) {
                const uint16_t bit = static_cast<uint16_t>(1u << r);
                const uint8_t c = counts[r];
                if (c >= 1) ones = static_cast<uint16_t>(ones | bit);
                if (c >= 2) twos = static_cast<uint16_t>(twos | bit);
                if (c >= 3) threes = static_cast<uint16_t>(threes | bit);
                if (c >= 4) fours = static_cast<uint16_t>(fours | bit);
            }

            return static_cast<uint64_t>(ones)
                | (static_cast<uint64_t>(twos) << 13)
                | (static_cast<uint64_t>(threes) << 26)
                | (static_cast<uint64_t>(fours) << 39);
        }

        [[nodiscard]] std::array<hand_rank, (1u << 13)> build_flush_table() {
            std::array<hand_rank, (1u << 13)> table{};
            for (uint32_t mask = 0; mask < table.size(); ++mask) {
                table[mask] = evaluate_flush_mask(static_cast<uint16_t>(mask));
            }
            return table;
        }

        void gen_counts(const int rank, int remaining, std::array<uint8_t, 13>& counts, std::vector<non_flush_entry>& out) {
            if (rank == 13) {
                if (remaining == 0) {
                    out.push_back(non_flush_entry{state_key_from_counts(counts), evaluate_non_flush_counts(counts)});
                }
                return;
            }

            const int max_count = std::min(4, remaining);
            for (int c = 0; c <= max_count; ++c) {
                counts[rank] = static_cast<uint8_t>(c);
                gen_counts(rank + 1, remaining - c, counts, out);
            }
            counts[rank] = 0;
        }

        [[nodiscard]] std::vector<non_flush_entry> build_rank_table() {
            std::vector<non_flush_entry> table;
            table.reserve(50000);

            std::array<uint8_t, 13> counts{};
            gen_counts(0, 7, counts, table);

            std::sort(table.begin(), table.end(), [](const non_flush_entry& a, const non_flush_entry& b) {
                return a.key < b.key;
            });

            for (std::size_t i = 1; i < table.size(); ++i) {
                const auto& lhs = table[i - 1];
                const auto& rhs = table[i];
                if (lhs.key == rhs.key && lhs.rank.value != rhs.rank.value) {
                    std::abort();
                }
            }

            table.erase(std::unique(table.begin(), table.end(), [](const non_flush_entry& a, const non_flush_entry& b) {
                return a.key == b.key;
            }), table.end());

            return table;
        }

        [[nodiscard]] std::size_t quinary_index_from_key(const uint64_t key) noexcept {
            std::array<uint8_t, 13> counts{};
            const auto ones = static_cast<uint16_t>(key);
            const auto twos = static_cast<uint16_t>(key >> 13);
            const auto threes = static_cast<uint16_t>(key >> 26);
            const auto fours = static_cast<uint16_t>(key >> 39);
            for (std::size_t rank = 0; rank < counts.size(); ++rank) {
                const auto bit = static_cast<uint16_t>(1u << rank);
                counts[rank] = static_cast<uint8_t>(((ones & bit) != 0)
                    + ((twos & bit) != 0)
                    + ((threes & bit) != 0)
                    + ((fours & bit) != 0));
            }
            return quinary_index_from_counts(counts);
        }

    }

    const std::array<hand_rank, (1u << 13)> flush_table = build_flush_table();
    const quinary_chunk4_table quinary_chunk0 = build_quinary_chunk_table<0, 4>();
    const quinary_chunk4_table quinary_chunk1 = build_quinary_chunk_table<4, 4>();
    const quinary_chunk5_table quinary_chunk2 = build_quinary_chunk_table<8, 5>();

    const std::vector<non_flush_entry>& rank_table() noexcept {
        static const std::vector<non_flush_entry> table = build_rank_table();
        return table;
    }

    const std::array<hand_rank, non_flush_quinary_table_size> non_flush_table = [] {
        static std::array<hand_rank, non_flush_quinary_table_size> out{};
        for (const auto& entry : rank_table()) {
            out[quinary_index_from_key(entry.key)] = entry.rank;
        }

        return out;
    }();
}
