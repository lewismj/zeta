#pragma once

#include <cassert>
#include <cstddef>
#include <cstdint>
#include <utility>

#include <core.h>
#include "eval.h"
#include "tables.h"


namespace zeta::holdem {

    struct hand_masks {
        uint16_t spades{};
        uint16_t hearts{};
        uint16_t diamonds{};
        uint16_t clubs{};
    };

    [[nodiscard]] inline_always hand_masks suit_rank_masks(const card_mask seven) noexcept {
        return hand_masks{
            .spades = ops::suit_ranks<default_deck>(seven, suit::spades),
            .hearts = ops::suit_ranks<default_deck>(seven, suit::hearts),
            .diamonds = ops::suit_ranks<default_deck>(seven, suit::diamonds),
            .clubs = ops::suit_ranks<default_deck>(seven, suit::clubs)
        };
    }

    [[nodiscard]] inline_always bool find_flush_suit(const hand_masks& masks, suit& suit_out) noexcept {
        if (ops::popcount(masks.spades) >= 5) {
            suit_out = suit::spades;
            return true;
        }
        if (ops::popcount(masks.hearts) >= 5) {
            suit_out = suit::hearts;
            return true;
        }
        if (ops::popcount(masks.diamonds) >= 5) {
            suit_out = suit::diamonds;
            return true;
        }
        if (ops::popcount(masks.clubs) >= 5) {
            suit_out = suit::clubs;
            return true;
        }
        return false;
    }

    [[nodiscard]] inline_always uint32_t flush_index(const hand_masks& masks, const suit flush) noexcept {
        switch (flush) {
            case suit::spades: return masks.spades;
            case suit::hearts: return masks.hearts;
            case suit::diamonds: return masks.diamonds;
            case suit::clubs: return masks.clubs;
        }
        std::unreachable();
    }

    [[nodiscard]] inline_always uint64_t non_flush_key(const hand_masks& masks) noexcept {
        const auto s0 = masks.spades;
        const auto s1 = masks.hearts;
        const auto s2 = masks.diamonds;
        const auto s3 = masks.clubs;
        const auto ones = static_cast<uint16_t>(s0 | s1 | s2 | s3);
        const auto twos = static_cast<uint16_t>((s0 & s1) | (s0 & s2) | (s0 & s3)
                                                   | (s1 & s2) | (s1 & s3) | (s2 & s3));
        const auto threes = static_cast<uint16_t>((s0 & s1 & s2) | (s0 & s1 & s3)
                                                      | (s0 & s2 & s3) | (s1 & s2 & s3));
        const auto fours = static_cast<uint16_t>(s0 & s1 & s2 & s3);

        return static_cast<uint64_t>(ones)
            | (static_cast<uint64_t>(twos) << 13)
            | (static_cast<uint64_t>(threes) << 26)
            | (static_cast<uint64_t>(fours) << 39);
    }

    [[nodiscard]] inline_always std::size_t non_flush_quinary_index(const hand_masks& masks) noexcept {
        const auto s0 = masks.spades;
        const auto s1 = masks.hearts;
        const auto s2 = masks.diamonds;
        const auto s3 = masks.clubs;

        const auto ones = static_cast<uint16_t>(s0 | s1 | s2 | s3);
        const auto twos = static_cast<uint16_t>((s0 & s1) | (s0 & s2) | (s0 & s3)
                                                   | (s1 & s2) | (s1 & s3) | (s2 & s3));
        const auto threes = static_cast<uint16_t>((s0 & s1 & s2) | (s0 & s1 & s3)
                                                      | (s0 & s2 & s3) | (s1 & s2 & s3));
        const auto fours = static_cast<uint16_t>(s0 & s1 & s2 & s3);

        const auto code0 = static_cast<std::size_t>(
            lookup::quinary_weights4[ones & 0x0f]
            + lookup::quinary_weights4[twos & 0x0f]
            + lookup::quinary_weights4[threes & 0x0f]
            + lookup::quinary_weights4[fours & 0x0f]
        );
        const auto chunk0 = lookup::quinary_chunk0[7][code0];
        const auto remaining1 = 7 - lookup::quinary_chunk_used(chunk0);

        const auto code1 = static_cast<std::size_t>(
            lookup::quinary_weights4[(ones >> 4) & 0x0f]
            + lookup::quinary_weights4[(twos >> 4) & 0x0f]
            + lookup::quinary_weights4[(threes >> 4) & 0x0f]
            + lookup::quinary_weights4[(fours >> 4) & 0x0f]
        );
        const auto chunk1 = lookup::quinary_chunk1[remaining1][code1];
        const auto remaining2 = remaining1 - lookup::quinary_chunk_used(chunk1);

        const auto code2 = static_cast<std::size_t>(
            lookup::quinary_weights5[(ones >> 8) & 0x1f]
            + lookup::quinary_weights5[(twos >> 8) & 0x1f]
            + lookup::quinary_weights5[(threes >> 8) & 0x1f]
            + lookup::quinary_weights5[(fours >> 8) & 0x1f]
        );
        const auto chunk2 = lookup::quinary_chunk2[remaining2][code2];

        return lookup::quinary_chunk_index(chunk0)
            + lookup::quinary_chunk_index(chunk1)
            + lookup::quinary_chunk_index(chunk2);
    }


    [[nodiscard]] inline_always hand_rank evaluate(const hand_masks& masks) noexcept {
        const auto spades = masks.spades;
        const auto hearts = masks.hearts;
        const auto diamonds = masks.diamonds;
        const auto clubs = masks.clubs;

        const int sp_count = ops::popcount(spades);
        const int he_count = ops::popcount(hearts);
        const int di_count = ops::popcount(diamonds);
        const int cl_count = ops::popcount(clubs);
        const auto& flush = lookup::flush_table;
        if (sp_count >= 5) [[unlikely]] {
            return flush[spades];
        }
        if (he_count >= 5) [[unlikely]] {
            return flush[hearts];
        }
        if (di_count >= 5) [[unlikely]] {
            return flush[diamonds];
        }
        if (cl_count >= 5) [[unlikely]] {
            return flush[clubs];
        }

        return lookup::non_flush_table[non_flush_quinary_index(masks)];
    }



    [[nodiscard]] inline_always hand_rank evaluate(const card_mask seven) noexcept {
        assert(ops::popcount(seven) == 7);
        return evaluate(suit_rank_masks(seven));
    }

}
