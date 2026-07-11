#pragma once

#include <array>
#include <cstddef>

#include "board.h"

namespace zeta::holdem {

    using combo_weight = float;

    struct cache_align hand_range final {
        std::array<combo_weight, combination_count> weights{};

        constexpr hand_range() noexcept = default;

        explicit constexpr hand_range(const combo_weight weight) noexcept {
            weights.fill(weight);
        }

        [[nodiscard]] inline_always combo_weight operator[](const combination_index idx) const noexcept {
            return weights[idx];
        }

        [[nodiscard]] inline_always combo_weight& operator[](const combination_index idx) noexcept {
            return weights[idx];
        }

        constexpr void clear() noexcept {
            weights.fill(0.0f);
        }

        constexpr void fill(const combo_weight weight) noexcept {
            weights.fill(weight);
        }

        [[nodiscard]] inline_hint bool empty() const noexcept {
            for (const auto weight : weights) {
                if (weight != 0.0f) {
                    return false;
                }
            }
            return true;
        }

        [[nodiscard]] inline_hint combo_weight total_weight() const noexcept {
            combo_weight total = 0.0f;
            for (const auto weight : weights) {
                total += weight;
            }
            return total;
        }

        inline_hint void normalize() noexcept {
            const auto total = total_weight();
            if (total <= 0.0f) {
                return;
            }

            const auto inverse = 1.0f / total;
            for (auto& weight : weights) {
                weight *= inverse;
            }
        }

        inline_hint void remove_dead(const card_mask dead) noexcept {
            const auto* masks = combination_masks.data();
            for (combination_index i = 0; i < combination_count; ++i) {
                if (masks[i] & dead) {
                    weights[i] = 0.0f;
                }
            }
        }

        inline_hint void scale(const combo_weight factor) noexcept {
            for (auto& weight : weights) {
                weight *= factor;
            }
        }

        [[nodiscard]] constexpr auto begin() noexcept {
            return weights.begin();
        }

        [[nodiscard]] constexpr auto end() noexcept {
            return weights.end();
        }

        [[nodiscard]] constexpr auto begin() const noexcept {
            return weights.begin();
        }

        [[nodiscard]] constexpr auto end() const noexcept {
            return weights.end();
        }

        [[nodiscard]] constexpr auto data() noexcept {
            return weights.data();
        }

        [[nodiscard]] constexpr auto data() const noexcept {
            return weights.data();
        }
    };

    [[nodiscard]] inline_always hand_range full_range(const combo_weight weight = 1.0f) noexcept {
        return hand_range{weight};
    }

    [[nodiscard]] inline_always card_mask combination_mask(const combination_index idx) noexcept {
        return combination_masks[idx];
    }

    [[nodiscard]] inline_always bool is_live_combo(const combination_index idx, const card_mask dead) noexcept {
        return (combination_mask(idx) & dead) == 0;
    }

}
