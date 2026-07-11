#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#ifdef ZETA_HOLDEM_ENABLE_RANK_TABLE
#include <vector>
#endif

#include "eval.h"

namespace zeta::holdem::lookup {

    inline constexpr std::size_t non_flush_quinary_table_size = 49205;
    inline constexpr uint32_t quinary_chunk_index_mask = 0x00ff'ffffu;

    struct non_flush_entry {
        uint64_t key{};
        hand_rank rank;
    };

    using quinary_dp_table = std::array<std::array<std::array<uint32_t, 8>, 14>, 5>;

    [[nodiscard]] constexpr quinary_dp_table build_quinary_dp() noexcept {
        std::array<std::array<uint32_t, 8>, 14> ways{};
        ways[0][0] = 1;
        for (std::size_t len = 1; len < ways.size(); ++len) {
            for (std::size_t sum = 0; sum < ways[len].size(); ++sum) {
                for (std::size_t digit = 0; digit <= 4 && digit <= sum; ++digit) {
                    ways[len][sum] += ways[len - 1][sum - digit];
                }
            }
        }

        quinary_dp_table dp{};
        for (std::size_t digit = 0; digit < dp.size(); ++digit) {
            for (std::size_t len = 0; len < ways.size(); ++len) {
                for (std::size_t sum = 0; sum < ways[len].size(); ++sum) {
                    for (std::size_t smaller = 0; smaller < digit && smaller <= sum; ++smaller) {
                        dp[digit][len][sum] += ways[len][sum - smaller];
                    }
                }
            }
        }
        return dp;
    }

    inline constexpr quinary_dp_table quinary_dp = build_quinary_dp();

    [[nodiscard]] constexpr std::array<uint16_t, 16> build_quinary_weights4() noexcept {
        std::array<uint16_t, 16> out{};
        for (std::size_t mask = 0; mask < out.size(); ++mask) {
            uint16_t value = 0;
            uint16_t weight = 1;
            for (std::size_t bit = 0; bit < 4; ++bit) {
                if ((mask & (std::size_t{1} << bit)) != 0) {
                    value = static_cast<uint16_t>(value + weight);
                }
                weight = static_cast<uint16_t>(weight * 5);
            }
            out[mask] = value;
        }
        return out;
    }

    [[nodiscard]] constexpr std::array<uint16_t, 32> build_quinary_weights5() noexcept {
        std::array<uint16_t, 32> out{};
        for (std::size_t mask = 0; mask < out.size(); ++mask) {
            uint16_t value = 0;
            uint16_t weight = 1;
            for (std::size_t bit = 0; bit < 5; ++bit) {
                if ((mask & (std::size_t{1} << bit)) != 0) {
                    value = static_cast<uint16_t>(value + weight);
                }
                weight = static_cast<uint16_t>(weight * 5);
            }
            out[mask] = value;
        }
        return out;
    }

    inline constexpr auto quinary_weights4 = build_quinary_weights4();
    inline constexpr auto quinary_weights5 = build_quinary_weights5();

    [[nodiscard]] constexpr uint32_t pack_quinary_chunk(const uint32_t index, const uint32_t used) noexcept {
        return index | (used << 24);
    }

    [[nodiscard]] constexpr uint32_t quinary_chunk_index(const uint32_t packed) noexcept {
        return packed & quinary_chunk_index_mask;
    }

    [[nodiscard]] constexpr uint32_t quinary_chunk_used(const uint32_t packed) noexcept {
        return packed >> 24;
    }

    using quinary_chunk4_table = std::array<std::array<uint32_t, 625>, 8>;
    using quinary_chunk5_table = std::array<std::array<uint32_t, 3125>, 8>;

    template<std::size_t StartRank, std::size_t Len>
    [[nodiscard]] constexpr std::array<std::array<uint32_t, (Len == 4 ? 625 : 3125)>, 8> build_quinary_chunk_table() noexcept {
        std::array<std::array<uint32_t, (Len == 4 ? 625 : 3125)>, 8> out{};
        for (std::size_t remaining = 0; remaining < out.size(); ++remaining) {
            for (std::size_t code = 0; code < out[remaining].size(); ++code) {
                std::size_t tmp = code;
                std::size_t current_remaining = remaining;
                uint32_t index = 0;
                uint32_t used = 0;
                bool valid = true;
                for (std::size_t offset = 0; offset < Len; ++offset) {
                    const std::size_t count = tmp % 5;
                    tmp /= 5;
                    used += static_cast<uint32_t>(count);
                    if (count > current_remaining) {
                        valid = false;
                    } else if (valid) {
                        index += quinary_dp[count][12 - StartRank - offset][current_remaining];
                        current_remaining -= count;
                    }
                }
                out[remaining][code] = pack_quinary_chunk(valid ? index : 0, used);
            }
        }
        return out;
    }

    [[nodiscard]] constexpr std::size_t quinary_index_from_counts(const std::array<uint8_t, 13>& counts) noexcept {
        std::size_t index = 0;
        std::size_t remaining = 7;
        for (std::size_t rank = 0; rank < counts.size(); ++rank) {
            const auto count = static_cast<std::size_t>(counts[rank]);
            index += quinary_dp[count][counts.size() - rank - 1][remaining];
            remaining -= count;
        }
        return index;
    }

    extern const std::array<hand_rank, (1u << 13)> flush_table;
    extern const quinary_chunk4_table quinary_chunk0;
    extern const quinary_chunk4_table quinary_chunk1;
    extern const quinary_chunk5_table quinary_chunk2;
#ifdef ZETA_HOLDEM_ENABLE_RANK_TABLE
    const std::vector<non_flush_entry>& rank_table() noexcept;
#endif
    extern const std::array<hand_rank, non_flush_quinary_table_size> non_flush_table;

}
