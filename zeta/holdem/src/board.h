#pragma once

#include <array>
#include <cstddef>
#include <utility>
#include <core.h>


namespace zeta::holdem {

    enum class street : uint8_t {
        preflop,
        flop,
        turn,
        river
    };


    struct board {
        card_mask mask = 0;             /**< 3, 4 or 5 bits set. */

        constexpr int size() const noexcept
        {
            return ops::popcount(mask);
        }

        constexpr bool contains(card c) const noexcept
        {
            return mask & (card_mask{1} << c);
        }

        constexpr bool empty() const noexcept
        {
            return mask == 0;
        }

        [[nodiscard]] constexpr street board_street() const noexcept {
            const auto n = size();
            assert(n == 0 || n == 3 || n == 4 || n == 5);
            switch (n) {
                case 0: return street::preflop;
                case 3: return street::flop;
                case 4: return street::turn;
                case 5: return street::river;
                default:
                    std::unreachable();
            }
        }

        constexpr void add(const card_mask m) {
            assert(ops::popcount(m) >= 1);          /**< At least one card. */
            assert((mask & m) == 0);                /**< No duplicates. */
            assert(ops::popcount(mask | m) <= 5);   /**< Max 5 board cards. */
            mask |= m;
        }

        constexpr void remove(const card_mask m) {
            assert((mask & m) == m);  /**< Cards must be present. */
            mask &= ~m;
        }

    };

    struct combination {
        card_mask mask;
    };

    /**
     * Hole card combinations:
     *   0	    As Ah
     *   1	    As Ad
     *   2	    As  Ac
     *           ...	...
     *   1325	2d  2c
     */
    using combination_index = uint16_t;
    inline constexpr std::size_t combination_count = 1326;

    [[nodiscard]] constexpr std::array<card_mask, combination_count> make_combination_masks() noexcept {
        std::array<card_mask, combination_count> masks{};
        std::size_t idx = 0;

        for (int first_rank = 12; first_rank >= 0; --first_rank) {
            for (int first_suit = 0; first_suit < 4; ++first_suit) {
                const auto first_card = static_cast<card>(first_suit * 13 + first_rank);
                for (int second_rank = first_rank; second_rank >= 0; --second_rank) {
                    const int second_suit_start = (second_rank == first_rank) ? first_suit + 1 : 0;
                    for (int second_suit = second_suit_start; second_suit < 4; ++second_suit) {
                        const auto second_card = static_cast<card>(second_suit * 13 + second_rank);
                        masks[idx++] = (card_mask{1} << first_card) | (card_mask{1} << second_card);
                    }
                }
            }
        }

        assert(idx == masks.size());
        return masks;
    }

    inline constexpr std::array<card_mask, combination_count> combination_masks = make_combination_masks();


}
