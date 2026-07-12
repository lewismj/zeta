#pragma once

#include "terminal/terminal_types.h"

namespace zeta::holdem {
    [[nodiscard]] inline_always river_terminal_cache make_river_terminal_cache(const board river) noexcept {
        assert(river.board_street() == street::river);
        assert(ops::popcount(river.mask) == 5);

        river_terminal_cache cache{};
        cache.board_hash = static_cast<uint64_t>(river.mask);
        cache.river_board = river;

        std::array<hand_rank, combination_count> evaluated_ranks{};
        const auto board_masks = suit_rank_masks(river.mask);

        for (combination_index i = 0; i < combination_count; ++i) {
            const auto combo = combination_mask(i);
            cache.masks[i] = combo;
            cache.cards[i] = extract_combo_cards(combo);

            if ((combo & river.mask) != 0) {
                continue;
            }

            const auto combo_masks = suit_rank_masks(combo);
            const hand_masks masks{
                .spades = static_cast<uint16_t>(board_masks.spades | combo_masks.spades),
                .hearts = static_cast<uint16_t>(board_masks.hearts | combo_masks.hearts),
                .diamonds = static_cast<uint16_t>(board_masks.diamonds | combo_masks.diamonds),
                .clubs = static_cast<uint16_t>(board_masks.clubs | combo_masks.clubs)
            };
            evaluated_ranks[i] = evaluate(masks);
            set_combo_live(cache.live, i);
            cache.rank_order[cache.rank_order_count++] = i;
        }

        auto begin = cache.rank_order.begin();
        auto end = begin + static_cast<std::ptrdiff_t>(cache.rank_order_count);
        std::sort(begin, end, [&](const combination_index lhs, const combination_index rhs) {
            const auto lhs_rank = evaluated_ranks[lhs];
            const auto rhs_rank = evaluated_ranks[rhs];
            if (lhs_rank == rhs_rank) {
                return lhs < rhs;
            }
            return lhs_rank < rhs_rank;
        });

        hand_rank previous{};
        bool have_previous = false;
        rank_key current_key = 0;

        for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
            const auto idx = cache.rank_order[order];
            const auto rank = evaluated_ranks[idx];
            if (!have_previous || rank != previous) {
                ++current_key;
                cache.unique_ranks[current_key] = rank;
                previous = rank;
                have_previous = true;
            }
            cache.rank_keys[idx] = current_key;
        }

        cache.unique_rank_count = current_key;
        return cache;
    }

    [[nodiscard]] inline_always accumulator clamp_compatible_mass(const accumulator mass) noexcept {
        if (mass < 0.0 && mass > -1.0e-3) {
            return 0.0;
        }
        assert(mass >= -1.0e-3);
        return mass;
    }

    [[nodiscard]] inline_always accumulator compatible_mass_from_bucket(
        const accumulator total,
        const accumulator first_card_mass,
        const accumulator second_card_mass,
        const accumulator exact_same_combo_weight
    ) noexcept {
        return clamp_compatible_mass(total - first_card_mass - second_card_mass + exact_same_combo_weight);
    }

    [[nodiscard]] inline_always accumulator bucket_card_mass(
        const river_reach_index& index,
        const river_rank_bucket& bucket,
        const uint8_t card
    ) noexcept {
        const auto offset = bucket.card_mass_lookup[card];
        if (offset == missing_bucket_card_mass) {
            return 0.0;
        }
        const auto entry_index = static_cast<uint16_t>(bucket.card_mass_begin + offset);
        assert(entry_index < bucket.card_mass_end);
        return index.bucket_card_masses[entry_index].mass;
    }

    inline_always void add_bucket_cards(
        std::array<accumulator, 52>& out,
        const river_reach_index& index,
        const river_rank_bucket& bucket
    ) noexcept {
        for (uint16_t i = bucket.card_mass_begin; i < bucket.card_mass_end; ++i) {
            const auto entry = index.bucket_card_masses[i];
            out[entry.card] += entry.mass;
        }
    }

    [[nodiscard]] inline_always accumulator compatible_reach_mass(
        const river_terminal_cache& cache,
        const river_reach_index& opponent,
        const combination_index hero_combo
    ) noexcept {
        assert(cache.board_hash == opponent.board_hash);
        const auto [first, second] = cache.cards[hero_combo];
        return compatible_mass_from_bucket(
            opponent.total_live_mass,
            opponent.mass_by_card[first],
            opponent.mass_by_card[second],
            opponent.weights[hero_combo]
        );
    }

    [[nodiscard]] inline_always accumulator compatible_mass(
        const river_terminal_cache& cache,
        const river_reach_index& opponent,
        const combination_index hero_combo
    ) noexcept {
        return compatible_reach_mass(cache, opponent, hero_combo);
    }

    [[nodiscard]] inline_always river_reach_index make_river_reach_index(
        const river_terminal_cache& cache,
        const reach_vector& reach
    ) noexcept {
        river_reach_index index{};
        index.board_hash = cache.board_hash;

        std::array<accumulator, 52> bucket_card_accumulator{};
        std::array<uint8_t, 52> bucket_touched_cards{};
        uint8_t bucket_touched_count = 0;
        bool have_bucket = false;
        uint16_t current_bucket = 0;

        const auto flush_bucket = [&]() noexcept {
            if (!have_bucket) {
                return;
            }

            auto& bucket = index.rank_buckets[current_bucket];
            bucket.end = index.active_count;
            bucket.card_mass_begin = index.bucket_card_mass_count;
            for (uint8_t touched_idx = 0; touched_idx < bucket_touched_count; ++touched_idx) {
                const auto card = bucket_touched_cards[touched_idx];
                const auto mass = bucket_card_accumulator[card];
                if (mass <= 0.0) {
                    bucket_card_accumulator[card] = 0.0;
                    continue;
                }
                assert(index.bucket_card_mass_count < index.bucket_card_masses.size());
                const auto relative_offset = static_cast<uint8_t>(index.bucket_card_mass_count - bucket.card_mass_begin);
                bucket.card_mass_lookup[card] = relative_offset;
                index.bucket_card_masses[index.bucket_card_mass_count++] = river_bucket_card_mass{
                    .card = card,
                    .mass = static_cast<combo_weight>(mass)
                };
                bucket_card_accumulator[card] = 0.0;
            }
            bucket.card_mass_end = index.bucket_card_mass_count;
            bucket_touched_count = 0;
        };

        for (std::size_t order = 0; order < cache.rank_order_count; ++order) {
            const auto combo_idx = cache.rank_order[order];
            const auto weight = reach[combo_idx];
            if (weight <= 0.0f) {
                continue;
            }

            const auto rank = cache.rank_keys[combo_idx];
            assert(rank != 0);
            if (!have_bucket || index.rank_buckets[current_bucket].rank != rank) {
                flush_bucket();
                current_bucket = index.unique_rank_count++;
                auto& bucket = index.rank_buckets[current_bucket];
                bucket.rank = rank;
                bucket.begin = index.active_count;
                bucket.total_mass = 0.0;
                bucket.card_mass_lookup.fill(missing_bucket_card_mass);
                have_bucket = true;
            }

            index.weights[combo_idx] = weight;
            index.active_indices[index.active_count++] = combo_idx;
            index.total_live_mass += weight;

            auto& bucket = index.rank_buckets[current_bucket];
            bucket.total_mass += weight;

            const auto [first, second] = cache.cards[combo_idx];
            index.mass_by_card[first] += weight;
            index.mass_by_card[second] += weight;

            if (bucket_card_accumulator[first] == 0.0) {
                bucket_touched_cards[bucket_touched_count++] = first;
            }
            if (bucket_card_accumulator[second] == 0.0) {
                bucket_touched_cards[bucket_touched_count++] = second;
            }
            bucket_card_accumulator[first] += weight;
            bucket_card_accumulator[second] += weight;
        }

        flush_bucket();
        assert(index.bucket_card_mass_count <= index.active_count * 2u);
        return index;
    }

    // Terminal evaluation workspace: owns the materialized reach indices for all active players.
    // This is a reusable scratch object for thread-local use in CFR traversal.
    //
    // Architecture:
    // - workspace owns the reach_index array (large: ~129 KB per player)
    // - caller provides ranges (immutable input)
    // - cache remains immutable (shared read-only)
    // - workspace materializes ranges -> reach_index on evaluation
    // - kernel evaluates using workspace's reach indices
    // - workspace is reused across many node evaluations (thread-local)
    //
    // This avoids per-node allocations and makes lifetime/ownership explicit.
}
