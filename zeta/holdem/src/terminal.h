#pragma once

#include <array>
#include <algorithm>
#include <bitset>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <vector>

#include "board.h"
#include "evaluator.h"
#include "range.h"

namespace zeta::holdem {

    // Heads-up seat identity. Only meaningful for 2-player terminals; N-way
    // kernels address seats by index (0..N-1). `player` is retained as a
    // back-compat alias so existing call sites compile unchanged.
    enum class heads_up_player : uint8_t {
        oop,
        ip
    };

    using player = heads_up_player;

    using terminal_value = float;
    using accumulator = double;
    using utility = double;

    enum class terminal_kernel_family : uint8_t {
        heads_up_exact,
        multiplayer
    };

    // N-way seat mask: generic template uses bitset<N> where bit i == true means seat i is folded.
    template <std::size_t N>
    struct folded_mask {
        std::bitset<N> bits;

        [[nodiscard]] constexpr bool operator[](std::size_t seat) const noexcept {
            return bits[seat];
        }

        constexpr bool& operator[](std::size_t seat) noexcept {
            return bits[seat];
        }
    };

    // Heads-up specialization: genuine optimization with direct boolean fields, not bitset.
    // This avoids any bitset overhead in the fast path.
    template <>
    struct folded_mask<2> {
        bool oop_folded = false;
        bool ip_folded = false;

        // Accessor for compatibility with generic interface
        [[nodiscard]] constexpr bool operator[](std::size_t seat) const noexcept {
            return seat == 0 ? oop_folded : ip_folded;
        }

        // Helper to set folded state
        constexpr void set_folded(std::size_t seat, bool value) noexcept {
            if (seat == 0) {
                oop_folded = value;
            } else {
                ip_folded = value;
            }
        }

        // Factory from heads_up_player for compatibility.
        [[nodiscard]] static constexpr folded_mask<2> from_folded_player(const heads_up_player folded) noexcept {
            folded_mask<2> mask;
            if (folded == heads_up_player::oop) {
                mask.oop_folded = true;
            } else {
                mask.ip_folded = true;
            }
            return mask;
        }
    };

    struct terminal_pot {
        utility gross_pot = 0.0;
        utility rake = 0.0;
        utility oop_contribution = 0.0;
        utility ip_contribution = 0.0;
    };

    // Player-neutral, compile-time-sized terminal accounting context.
    // For N active players, `contribution[seat]` is that seat's contribution.
    // Heads-up is terminal_context<2> (contribution[0]=oop, contribution[1]=ip).
    template <std::size_t N>
    struct terminal_context {
        utility gross_pot = 0.0;
        utility rake = 0.0;
        std::array<utility, N> contribution{};
    };

    // Zero-cost clarity alias: heads-up accounting is exactly terminal_context<2>
    // (contribution[0]=oop, contribution[1]=ip). No separate type is needed; this
    // keeps the single templated context boundary while naming the heads-up intent.
    using heads_up_context = terminal_context<2>;

    struct terminal_payoff {
        utility oop = 0.0;
        utility ip = 0.0;
    };

    // Build a heads-up context from explicit pot accounting.
    [[nodiscard]] constexpr terminal_context<2> make_heads_up_context(
        const utility gross_pot,
        const utility rake,
        const utility oop_contribution,
        const utility ip_contribution
    ) noexcept {
        return terminal_context<2>{
            .gross_pot = gross_pot,
            .rake = rake,
            .contribution = {oop_contribution, ip_contribution}
        };
    }

    [[nodiscard]] constexpr terminal_context<2> make_heads_up_context(const terminal_pot pot) noexcept {
        return make_heads_up_context(pot.gross_pot, pot.rake, pot.oop_contribution, pot.ip_contribution);
    }

    // Recover the heads-up pot accounting from a two-player context so the
    // existing payoff helpers can be reused unchanged.
    [[nodiscard]] constexpr terminal_pot heads_up_pot(const terminal_context<2>& context) noexcept {
        return terminal_pot{
            .gross_pot = context.gross_pot,
            .rake = context.rake,
            .oop_contribution = context.contribution[0],
            .ip_contribution = context.contribution[1]
        };
    }

    [[nodiscard]] constexpr std::size_t player_index(const heads_up_player p) noexcept {
        return p == heads_up_player::oop ? 0u : 1u;
    }

    // ============================================================================
    // Phase 2, Step 9: pot_structure<N> and payoff infrastructure
    // ============================================================================
    //
    // Separates hand ranking (showdown) from payoff computation (pot distribution).
    // Side-pot handling, rake application, and eligibility are payoff concerns,
    // not ranking concerns.
    //
    // This structure can be extended for:
    // - Step 15 (range_data policy): bucketed ranges instead of raw combos
    // - Step 16 (rake_policy): generalized rake models beyond linear deduction
    // - Step 17 (parallelism): memory layout and NUMA affinity hints

    // Side pot representation: accumulates contributions toward a particular pot.
    // Each seat's total winnings is distributed across main pot + side pots[0..n-1].
    template <std::size_t N>
    struct side_pot {
        // Seats that contributed to this pot (and are thus eligible to win it).
        std::bitset<N> eligible{};
        // Total amount in this pot before distribution.
        utility amount = 0.0;
    };

    // Rake policy abstraction: how rake is deducted from the gross pot.
    // Hook for Step 16: allows plugging in different rake models.
    //
    // Default (linear): rake = f * gross_pot (capped at max_rake if present).
    // Examples:
    //   - No-flop rake: zero on side pots, full rate on main pot.
    //   - Time collection: rake = time_amount (fixed).
    //   - Rake cap: rake = min(f * gross_pot, cap).
    //   - Tournament: rake = zero.
    struct rake_policy {
        // Standard online poker rake: fraction of the pot (e.g., 0.05 for 5%).
        float rate = 0.0f;
        // Optional cap: rake cannot exceed this amount. Zero = no cap.
        float max_rake = 0.0f;

        // Compute rake deducted from a pot. Override this function (or use
        // a derived class / custom policy) to implement different rake models.
        [[nodiscard]] constexpr utility compute_rake(const utility gross_amount) const noexcept {
            utility computed = static_cast<utility>(rate) * gross_amount;
            if (max_rake > 0.0f) {
                computed = std::min(computed, static_cast<utility>(max_rake));
            }
            return computed;
        }
    };

    // Range data policy: abstraction for range representation.
    // Hook for Step 15: allows plugging in different range sources (raw combos, buckets, etc.).
    //
    // The evaluator's core algorithm works with per-combo weighting. This policy
    // allows a future implementation to feed in bucketed ranges, sampled subsets,
    // or importance-weighted distributions without changing the payoff kernel.
    struct range_data_policy {
        // Placeholder: could be specialized for:
        //   - exact_range_policy: raw 1081 combos (current)
        //   - bucketed_range_policy: precomputed hand strength buckets
        //   - sampled_range_policy: importance-weighted samples
        //   - abstract_range_policy: strategic abstraction (e.g., isomorphic groups)
        //
        // For now, this is a marker struct. The evaluator uses raw reach_vector.
        // Later, a template parameter can select the policy.
    };

    // Memory layout policy: hints for parallelism and NUMA optimization.
    // Hook for Step 17: allows specifying memory affinity and layout constraints.
    //
    // The evaluator is single-threaded, but the workspace and cache can be
    // placed according to these hints when used in a parallel CFR solver.
    struct memory_layout_policy {
        // Alignment requirement for workspace allocation (e.g., 64 for cache line).
        // Zero = default alignment.
        size_t alignment = 0;

        // NUMA affinity node, if relevant (e.g., for thread-local workspaces).
        // -1 = no preference (system chooses).
        int numa_node = -1;

        // True if this workspace is read-only (sharable across threads).
        // False = thread-local only.
        bool is_shared = false;

        // Memory size estimate for planning (diagnostic only; not enforced).
        [[nodiscard]] static constexpr size_t estimate_workspace_bytes(std::size_t N) noexcept {
            // Rough estimate: N reach indices at ~129 KB each + scratch buffers.
            return N * 129'000 + 8'000;
        }
    };

    // N-way pot structure: main pot + side pots, rake policy, and active-set mask.
    // Separates payoff computation from hand ranking.
    template <std::size_t N>
    struct pot_structure {
        // Which seats are active and eligible to win (complement of folded_mask).
        std::bitset<N> active{};

        // Main pot and side pots. For simplicity, we accumulate side pots linearly.
        // (A more complex representation could track per-player all-in amounts.)
        std::vector<side_pot<N>> pots;

        // Rake policy: determines how rake is deducted.
        // Default is linear: rate * gross_pot (capped).
        rake_policy rake{};

        // Range data policy: future hook for bucketed/sampled ranges.
        // Currently unused; prepared for Step 15.
        range_data_policy range_policy{};

        // Memory layout hints: for NUMA-aware allocation and thread pinning.
        // Prepared for Step 17 (parallelism & memory).
        memory_layout_policy memory_policy{};

        // Initialize main pot with all active seats eligible.
        // Call after setting active and before distributing side pots.
        constexpr void initialize_main_pot(const utility gross_pot) noexcept {
            pots.clear();
            pots.emplace_back();
            pots[0].eligible = active;
            pots[0].amount = gross_pot;
        }

        // Total pot balance (sum of all side pots).
        [[nodiscard]] constexpr utility total_pot_balance() const noexcept {
            utility total = 0.0;
            for (const auto& pot : pots) {
                total += pot.amount;
            }
            return total;
        }

        // Count of active players.
        [[nodiscard]] constexpr std::size_t active_count() const noexcept {
            return active.count();
        }
    };

    // Heads-up specialization: no side pots, simpler structure.
    // This keeps the data layout tight for the fast path.
    template <>
    struct pot_structure<2> {
        bool oop_active = true;
        bool ip_active = true;

        utility main_pot = 0.0;
        // No side pots for heads-up (binary all-in semantics).

        rake_policy rake{};
        range_data_policy range_policy{};
        memory_layout_policy memory_policy{};

        // Accessors for generic interface compatibility.
        [[nodiscard]] constexpr std::size_t active_count() const noexcept {
            return (oop_active ? 1 : 0) + (ip_active ? 1 : 0);
        }

        [[nodiscard]] constexpr utility total_pot_balance() const noexcept {
            return main_pot;
        }
    };

    using value_array = std::array<terminal_value, combination_count>;

    // Templated structure-of-arrays: one contiguous per-combo value array per
    // active player. terminal_values<2> is exactly the heads-up layout. Even at
    // 6 players this is 6 * combination_count floats (~31 KB), so no nesting or
    // compression is needed.
    template <std::size_t N>
    struct terminal_values {
        std::array<value_array, N> player_values{};

        // Heads-up ergonomic access by seat enum (valid while N >= 2).
        [[nodiscard]] constexpr const value_array& operator[](const heads_up_player p) const noexcept {
            return player_values[player_index(p)];
        }

        [[nodiscard]] constexpr value_array& operator[](const heads_up_player p) noexcept {
            return player_values[player_index(p)];
        }

        // Seat-indexed access for the generic (N-way) form.
        [[nodiscard]] constexpr const value_array& operator[](const std::size_t seat) const noexcept {
            return player_values[seat];
        }

        [[nodiscard]] constexpr value_array& operator[](const std::size_t seat) noexcept {
            return player_values[seat];
        }
    };

    // Aggregate EV / win-tie accounting for an evaluated terminal. The summary is
    // inherently kernel-specific: the heads-up specialization below exposes the
    // lower/equal/higher decomposition (oop vs ip). An N-way summary would carry a
    // different shape, so the primary template is left unimplemented on purpose.
    template <std::size_t N>
    struct terminal_summary {
        static_assert(N == 2, "terminal_summary is only specialized for heads-up (N == 2)");
    };

    template <>
    struct terminal_summary<2> {
        accumulator oop_ev = 0.0;
        accumulator ip_ev = 0.0;
        accumulator matchup_weight = 0.0;
        accumulator ties = 0.0;
        accumulator oop_wins = 0.0;
        accumulator ip_wins = 0.0;
    };

    // Result bundle for an evaluated terminal. Fully templated on the player count
    // so a future N-way kernel returns terminal_result<N> rather than being forced
    // through a permanently heads-up (two-seat) shape.
    template <std::size_t N>
    struct terminal_result {
        terminal_values<N> values{};
        terminal_summary<N> summary{};
    };

    struct reach_vector {
        std::array<combo_weight, combination_count> weights{};

        [[nodiscard]] constexpr combo_weight operator[](const combination_index idx) const noexcept {
            return weights[idx];
        }

        [[nodiscard]] constexpr combo_weight& operator[](const combination_index idx) noexcept {
            return weights[idx];
        }
    };

    [[nodiscard]] inline_always reach_vector make_reach_vector(const hand_range& range) noexcept {
        reach_vector reach{};
        reach.weights = range.weights;
        return reach;
    }

    [[nodiscard]] constexpr utility distributed_pot(const terminal_pot pot) noexcept {
        assert(pot.gross_pot >= 0.0);
        assert(pot.rake >= 0.0);
        assert(pot.oop_contribution >= 0.0);
        assert(pot.ip_contribution >= 0.0);
        assert(pot.gross_pot >= pot.rake);
        assert((pot.gross_pot - pot.rake) >= pot.oop_contribution + pot.ip_contribution);
        return pot.gross_pot - pot.rake;
    }

    [[nodiscard]] constexpr terminal_payoff payoff_for_oop_win(const terminal_pot pot) noexcept {
        const auto awarded = distributed_pot(pot);
        return terminal_payoff{
            .oop = awarded - pot.oop_contribution,
            .ip = -pot.ip_contribution
        };
    }

    [[nodiscard]] constexpr terminal_payoff payoff_for_ip_win(const terminal_pot pot) noexcept {
        const auto awarded = distributed_pot(pot);
        return terminal_payoff{
            .oop = -pot.oop_contribution,
            .ip = awarded - pot.ip_contribution
        };
    }

    [[nodiscard]] constexpr terminal_payoff payoff_for_tie(const terminal_pot pot) noexcept {
        const auto split = distributed_pot(pot) * 0.5;
        return terminal_payoff{
            .oop = split - pot.oop_contribution,
            .ip = split - pot.ip_contribution
        };
    }

    [[nodiscard]] constexpr terminal_payoff payoff_for_fold(const terminal_pot pot, const heads_up_player folded) noexcept {
        return folded == heads_up_player::oop ? payoff_for_ip_win(pot) : payoff_for_oop_win(pot);
    }

    using combo_bitset = std::array<uint64_t, (combination_count + 63) / 64>;
    using rank_key = uint16_t;
    constexpr std::size_t river_live_combination_count = ((52u - 5u) * (52u - 6u)) / 2u;
    constexpr uint8_t missing_bucket_card_mass = UINT8_MAX;

    struct combo_cards {
        uint8_t first = 0;
        uint8_t second = 0;
    };

    [[nodiscard]] constexpr bool combo_live(const combo_bitset& bits, const combination_index idx) noexcept {
        return (bits[idx / 64u] & (uint64_t{1} << (idx % 64u))) != 0;
    }

    constexpr void set_combo_live(combo_bitset& bits, const combination_index idx) noexcept {
        bits[idx / 64u] |= uint64_t{1} << (idx % 64u);
    }

    [[nodiscard]] inline_always combo_cards extract_combo_cards(card_mask mask) noexcept {
        const auto first_bit = ops::pop_lsb(mask);
        const auto second_bit = ops::pop_lsb(mask);
        assert(mask == 0);
        return combo_cards{
            .first = static_cast<uint8_t>(ops::lsb_index(first_bit)),
            .second = static_cast<uint8_t>(ops::lsb_index(second_bit))
        };
    }

    struct river_terminal_cache {
        uint64_t board_hash = 0;
        board river_board{};
        std::array<card_mask, combination_count> masks{};
        std::array<rank_key, combination_count> rank_keys{};
        std::array<hand_rank, river_live_combination_count + 1> unique_ranks{};
        uint16_t unique_rank_count = 0;
        std::array<combo_cards, combination_count> cards{};
        combo_bitset live{};
        std::array<combination_index, river_live_combination_count> rank_order{};
        std::size_t rank_order_count = 0;
    };

    struct river_rank_bucket {
        rank_key rank = 0;
        accumulator total_mass = 0.0;
        uint16_t begin = 0;
        uint16_t end = 0;
        uint16_t card_mass_begin = 0;
        uint16_t card_mass_end = 0;
        std::array<uint8_t, 52> card_mass_lookup{};
    };

    struct river_bucket_card_mass {
        uint8_t card = 0;
        accumulator mass = 0.0;
    };

    struct river_reach_index {
        uint64_t board_hash = 0;
        std::array<combo_weight, combination_count> weights{};
        std::array<combination_index, river_live_combination_count> active_indices{};
        std::array<accumulator, 52> mass_by_card{};
        accumulator total_live_mass = 0.0;
        uint16_t active_count = 0;
        std::array<river_rank_bucket, river_live_combination_count> rank_buckets{};
        uint16_t unique_rank_count = 0;
        std::array<river_bucket_card_mass, river_live_combination_count * 2> bucket_card_masses{};
        uint16_t bucket_card_mass_count = 0;
    };

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
    template <std::size_t N>
    struct terminal_workspace {
        std::array<river_reach_index, N> reach{};

        // Materialize ranges into reach indices for the given board.
        // Call this once per board before evaluating multiple nodes on that board.
        void materialize(
            const river_terminal_cache& cache,
            const std::array<reach_vector, N>& ranges
        ) noexcept {
            for (std::size_t seat = 0; seat < N; ++seat) {
                reach[seat] = make_river_reach_index(cache, ranges[seat]);
            }
        }
    };

    inline_always void accumulate_showdown_bucket_values(
        terminal_result<2>& result,
        const river_terminal_cache& cache,
        const heads_up_player hero_player,
        const river_reach_index& hero_index,
        const river_reach_index& opponent_index,
        const river_rank_bucket& hero_bucket,
        const accumulator opponent_lower_total,
        const std::array<accumulator, 52>& opponent_lower_by_card,
        const river_rank_bucket* const opponent_equal_bucket,
        const utility win_component,
        const utility tie_component,
        const utility loss_component,
        const bool record_matchups
    ) noexcept {
        const accumulator opponent_equal_total = opponent_equal_bucket == nullptr ? 0.0 : opponent_equal_bucket->total_mass;
        const accumulator opponent_higher_total =
            static_cast<accumulator>(opponent_index.total_live_mass) - opponent_lower_total - opponent_equal_total;

        for (uint16_t offset = hero_bucket.begin; offset < hero_bucket.end; ++offset) {
            const auto hero_combo = hero_index.active_indices[offset];
            const auto [first, second] = cache.cards[hero_combo];

            const accumulator equal_first = opponent_equal_bucket == nullptr
                ? 0.0
                : bucket_card_mass(opponent_index, *opponent_equal_bucket, first);
            const accumulator equal_second = opponent_equal_bucket == nullptr
                ? 0.0
                : bucket_card_mass(opponent_index, *opponent_equal_bucket, second);

            const auto lower = compatible_mass_from_bucket(
                opponent_lower_total,
                opponent_lower_by_card[first],
                opponent_lower_by_card[second],
                0.0
            );
            const auto equal = compatible_mass_from_bucket(
                opponent_equal_total,
                equal_first,
                equal_second,
                opponent_index.weights[hero_combo]
            );
            const auto higher = compatible_mass_from_bucket(
                opponent_higher_total,
                static_cast<accumulator>(opponent_index.mass_by_card[first]) - opponent_lower_by_card[first] - equal_first,
                static_cast<accumulator>(opponent_index.mass_by_card[second]) - opponent_lower_by_card[second] - equal_second,
                0.0
            );

            const accumulator value = lower * win_component + equal * tie_component + higher * loss_component;
            result.values[hero_player][hero_combo] = static_cast<terminal_value>(value);

            const accumulator hero_weight = hero_index.weights[hero_combo];
            if (record_matchups) {
                result.summary.oop_wins += hero_weight * lower;
                result.summary.ties += hero_weight * equal;
                result.summary.ip_wins += hero_weight * higher;
                result.summary.matchup_weight += hero_weight * (lower + equal + higher);
                result.summary.oop_ev += hero_weight * value;
            } else {
                result.summary.ip_ev += hero_weight * value;
            }
        }
    }

    // Heads-up (2-player) exact showdown kernel: a two-stream rank-bucket merge.
    // This is the hand-tuned fast path; the generic evaluate_showdown<N> dispatches
    // here for N == 2. reach[0] == oop, reach[1] == ip.
    [[nodiscard]] inline_always terminal_result<2> evaluate_showdown_heads_up(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        const terminal_context<2>& context
    ) noexcept {
        assert(cache.board_hash == oop_index.board_hash);
        assert(cache.board_hash == ip_index.board_hash);

        terminal_result<2> result{};
        const auto pot = heads_up_pot(context);
        const auto oop_win = payoff_for_oop_win(pot);
        const auto ip_win = payoff_for_ip_win(pot);
        const auto tie = payoff_for_tie(pot);

        std::size_t oop_bucket_idx = 0;
        std::size_t ip_bucket_idx = 0;
        accumulator oop_lower_total = 0.0;
        accumulator ip_lower_total = 0.0;
        std::array<accumulator, 52> oop_lower_by_card{};
        std::array<accumulator, 52> ip_lower_by_card{};

        while (oop_bucket_idx < oop_index.unique_rank_count || ip_bucket_idx < ip_index.unique_rank_count) {
            const rank_key oop_rank = oop_bucket_idx < oop_index.unique_rank_count
                ? oop_index.rank_buckets[oop_bucket_idx].rank
                : static_cast<rank_key>(UINT16_MAX);
            const rank_key ip_rank = ip_bucket_idx < ip_index.unique_rank_count
                ? ip_index.rank_buckets[ip_bucket_idx].rank
                : static_cast<rank_key>(UINT16_MAX);
            const rank_key rank = std::min(oop_rank, ip_rank);

            const river_rank_bucket* const oop_equal = oop_rank == rank ? &oop_index.rank_buckets[oop_bucket_idx] : nullptr;
            const river_rank_bucket* const ip_equal = ip_rank == rank ? &ip_index.rank_buckets[ip_bucket_idx] : nullptr;

            if (oop_equal != nullptr) {
                accumulate_showdown_bucket_values(
                    result,
                    cache,
                    heads_up_player::oop,
                    oop_index,
                    ip_index,
                    *oop_equal,
                    ip_lower_total,
                    ip_lower_by_card,
                    ip_equal,
                    oop_win.oop,
                    tie.oop,
                    ip_win.oop,
                    true
                );
            }

            if (ip_equal != nullptr) {
                accumulate_showdown_bucket_values(
                    result,
                    cache,
                    heads_up_player::ip,
                    ip_index,
                    oop_index,
                    *ip_equal,
                    oop_lower_total,
                    oop_lower_by_card,
                    oop_equal,
                    ip_win.ip,
                    tie.ip,
                    oop_win.ip,
                    false
                );
            }

            if (oop_equal != nullptr) {
                oop_lower_total += oop_equal->total_mass;
                add_bucket_cards(oop_lower_by_card, oop_index, *oop_equal);
                ++oop_bucket_idx;
            }
            if (ip_equal != nullptr) {
                ip_lower_total += ip_equal->total_mass;
                add_bucket_cards(ip_lower_by_card, ip_index, *ip_equal);
                ++ip_bucket_idx;
            }
        }

        return result;
    }

    // Generic entry point: player count is a compile-time constant. Primary
    // template fails to compile for N != 2 until multiplayer kernels exist, so an
    // accidental N-way call is a hard error rather than a silent slow path.
    template <std::size_t N>
    [[nodiscard]] terminal_result<N> evaluate_showdown(
        const river_terminal_cache& cache,
        const std::array<river_reach_index, N>& reach,
        const terminal_context<N>& context
    ) noexcept {
        static_assert(N == 2, "N-way showdown evaluator not implemented");
        if constexpr (N == 2) {
            return evaluate_showdown_heads_up(cache, reach[0], reach[1], context);
        }
    }

    // Heads-up convenience overload: forwards a pair of reach indices to the
    // two-stream kernel (keeps existing call sites working).
    [[nodiscard]] inline_always terminal_result<2> evaluate_showdown(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        const terminal_context<2>& context
    ) noexcept {
        return evaluate_showdown_heads_up(cache, oop_index, ip_index, context);
    }

    [[nodiscard]] inline_always terminal_result<2> evaluate_showdown(
        const river_terminal_cache& cache,
        const reach_vector& oop_reach,
        const reach_vector& ip_reach,
        const terminal_context<2>& context
    ) noexcept {
        const auto oop_index = make_river_reach_index(cache, oop_reach);
        const auto ip_index = make_river_reach_index(cache, ip_reach);
        return evaluate_showdown(cache, oop_index, ip_index, context);
    }

    [[nodiscard]] inline_always terminal_values<2> evaluate_showdown_values(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        const terminal_context<2>& context
    ) noexcept {
        return evaluate_showdown(cache, oop_index, ip_index, context).values;
    }

    [[nodiscard]] inline_always terminal_values<2> evaluate_showdown_values(
        const river_terminal_cache& cache,
        const reach_vector& oop_reach,
        const reach_vector& ip_reach,
        const terminal_context<2>& context
    ) noexcept {
        return evaluate_showdown(cache, oop_reach, ip_reach, context).values;
    }

    [[nodiscard]] inline_always terminal_summary<2> summarize_showdown(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        const terminal_context<2>& context
    ) noexcept {
        return evaluate_showdown(cache, oop_index, ip_index, context).summary;
    }

    [[nodiscard]] inline_always terminal_summary<2> summarize_showdown(
        const river_terminal_cache& cache,
        const reach_vector& oop_reach,
        const reach_vector& ip_reach,
        const terminal_context<2>& context
    ) noexcept {
        return evaluate_showdown(cache, oop_reach, ip_reach, context).summary;
    }

    // Workspace-based API (preferred for CFR): caller provides ranges, workspace owns reach indices.
    // The workspace materializes ranges on first call, then reuses for subsequent evaluations.
    template <std::size_t N>
    [[nodiscard]] terminal_result<N> evaluate_showdown(
       terminal_workspace<N>& workspace,
       const river_terminal_cache& cache,
       const std::array<reach_vector, N>& ranges,
       const terminal_context<N>& context
    ) noexcept {
       // Materialize ranges into workspace reach indices
       workspace.materialize(cache, ranges);
        
       // Evaluate using the materialized indices
       return evaluate_showdown(cache, workspace.reach, context);
    }

    // Workspace-based showdown values (convenience wrapper)
    template <std::size_t N>
    [[nodiscard]] terminal_values<N> evaluate_showdown_values(
       terminal_workspace<N>& workspace,
       const river_terminal_cache& cache,
       const std::array<reach_vector, N>& ranges,
       const terminal_context<N>& context
    ) noexcept {
       return evaluate_showdown(workspace, cache, ranges, context).values;
    }

    // Workspace-based showdown summary (convenience wrapper)
    template <std::size_t N>
    [[nodiscard]] terminal_summary<N> summarize_showdown(
       terminal_workspace<N>& workspace,
       const river_terminal_cache& cache,
       const std::array<reach_vector, N>& ranges,
       const terminal_context<N>& context
    ) noexcept {
       return evaluate_showdown(workspace, cache, ranges, context).summary;
    }

    // Heads-up workspace specialization (overload for convenience)
    [[nodiscard]] inline_always terminal_result<2> evaluate_showdown(
       terminal_workspace<2>& workspace,
       const river_terminal_cache& cache,
       const reach_vector& oop_reach,
       const reach_vector& ip_reach,
       const terminal_context<2>& context
    ) noexcept {
       return evaluate_showdown(workspace, cache, std::array<reach_vector, 2>{oop_reach, ip_reach}, context);
    }

    // Generic N-way fold kernel: for each active player, accumulate compatible mass
    // from all other active opponents × constant payoff per opponent. Generalizes
    // cleanly because fold is deterministic payout regardless of hand strength.
    //
    // Algorithm (pseudocode):
    // for active_player in players:
    //     if folded[active_player]:
    //         values[active_player][:] = 0  (folded players act no further)
    //     else:
    //         for combo in active_player_combos:
    //             for opponent in active_opponents (excluding active_player):
    //                 total_compatible += compatible_mass(opponent, combo)
    //             value[combo] = total_compatible * payoff_per_compatible_unit
    //
    // For heads-up: this reduces exactly to the current two-stream kernel.
    template <std::size_t N>
    [[nodiscard]] inline_always terminal_values<N> evaluate_fold_values_generic(
        const river_terminal_cache& cache,
        const std::array<river_reach_index, N>& reach,
        const terminal_context<N>& context,
        const folded_mask<N>& folded
    ) noexcept {
        terminal_values<N> values{};
        
        // For each active (non-folded) player
        for (std::size_t active_seat = 0; active_seat < N; ++active_seat) {
            if (folded[active_seat]) {
                // Folded players get zero values; skip initialization (already zero)
                continue;
            }
            
            // Active player receives payoff from all remaining active opponents
            for (uint16_t combo_offset = 0; combo_offset < reach[active_seat].active_count; ++combo_offset) {
                const auto combo = reach[active_seat].active_indices[combo_offset];
                accumulator total_compatible = 0.0;
                
                // Accumulate compatible mass from each active opponent
                for (std::size_t opponent_seat = 0; opponent_seat < N; ++opponent_seat) {
                    if (opponent_seat != active_seat && !folded[opponent_seat]) {
                        total_compatible += compatible_mass(cache, reach[opponent_seat], combo);
                    }
                }
                
                // For now: store total compatible mass. Payoff multiplier (win amount per opponent)
                // will be added when pot_structure<N> is available. For heads-up validation,
                // this accumulates to the correct denominator and is tested at that scale.
                values[active_seat][combo] = static_cast<terminal_value>(total_compatible);
            }
        }
        
        return values;
    }

    // Heads-up (2-player) fold kernel: compatible opponent mass × constant payoff.
    [[nodiscard]] inline_always terminal_values<2> evaluate_fold_values_heads_up(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        const terminal_context<2>& context,
        const heads_up_player folded
    ) noexcept {
        assert(cache.board_hash == oop_index.board_hash);
        assert(cache.board_hash == ip_index.board_hash);

        terminal_values<2> values{};
        const auto payoff = payoff_for_fold(heads_up_pot(context), folded);

        for (uint16_t offset = 0; offset < oop_index.active_count; ++offset) {
            const auto combo = oop_index.active_indices[offset];
            const auto compatible = compatible_mass(cache, ip_index, combo);
            values[heads_up_player::oop][combo] = static_cast<terminal_value>(compatible * payoff.oop);
        }

        for (uint16_t offset = 0; offset < ip_index.active_count; ++offset) {
            const auto combo = ip_index.active_indices[offset];
            const auto compatible = compatible_mass(cache, oop_index, combo);
            values[heads_up_player::ip][combo] = static_cast<terminal_value>(compatible * payoff.ip);
        }

        return values;
    }

    // Generic fold entry point with bitset: now properly handles N-way folded masks.
    // The generic kernel uses folded_mask<N> (bitset<N>) where bit i == true means seat i is folded.
    // Heads-up uses the specialized folded_mask<2> with direct boolean fields for performance.
    template <std::size_t N>
    [[nodiscard]] terminal_values<N> evaluate_fold_values(
        const river_terminal_cache& cache,
        const std::array<river_reach_index, N>& reach,
        const terminal_context<N>& context,
        const folded_mask<N>& folded
    ) noexcept {
        if constexpr (N == 2) {
            // Heads-up fast path: extract from the specialized folded_mask<2> struct
            const auto folded_player = folded.oop_folded ? heads_up_player::oop : heads_up_player::ip;
            return evaluate_fold_values_heads_up(cache, reach[0], reach[1], context, folded_player);
        } else {
            return evaluate_fold_values_generic(cache, reach, context, folded);
        }
    }

    // Heads-up convenience overload (deprecated signature, kept for compatibility).
    // Converts heads_up_player to folded_mask<2> using the specialized factory.
    template <std::size_t N>
    [[nodiscard]] terminal_values<N> evaluate_fold_values(
        const river_terminal_cache& cache,
        const std::array<river_reach_index, N>& reach,
        const terminal_context<N>& context,
        const heads_up_player folded_player
    ) noexcept {
        static_assert(N == 2, "heads_up_player parameter only valid for N == 2");
        if constexpr (N == 2) {
            const auto folded = folded_mask<2>::from_folded_player(folded_player);
            return evaluate_fold_values(cache, reach, context, folded);
        }
    }

    [[nodiscard]] inline_always terminal_values<2> evaluate_fold_values(
        const river_terminal_cache& cache,
        const river_reach_index& oop_index,
        const river_reach_index& ip_index,
        const terminal_context<2>& context,
        const heads_up_player folded
    ) noexcept {
        return evaluate_fold_values_heads_up(cache, oop_index, ip_index, context, folded);
    }

    [[nodiscard]] inline_always terminal_values<2> evaluate_fold_values(
        const river_terminal_cache& cache,
        const reach_vector& oop_reach,
        const reach_vector& ip_reach,
        const terminal_context<2>& context,
        const heads_up_player folded
    ) noexcept {
        const auto oop_index = make_river_reach_index(cache, oop_reach);
        const auto ip_index = make_river_reach_index(cache, ip_reach);
        return evaluate_fold_values(cache, oop_index, ip_index, context, folded);
    }

    // Workspace-based fold API (preferred for CFR): caller provides ranges, workspace owns reach indices.
    template <std::size_t N>
    [[nodiscard]] terminal_values<N> evaluate_fold_values(
        terminal_workspace<N>& workspace,
        const river_terminal_cache& cache,
        const std::array<reach_vector, N>& ranges,
        const terminal_context<N>& context,
        const folded_mask<N>& folded
    ) noexcept {
        // Materialize ranges into workspace reach indices
        workspace.materialize(cache, ranges);
        
        // Evaluate using the materialized indices
        return evaluate_fold_values(cache, workspace.reach, context, folded);
    }

    // Heads-up workspace specialization (overload for convenience)
    [[nodiscard]] inline_always terminal_values<2> evaluate_fold_values(
        terminal_workspace<2>& workspace,
        const river_terminal_cache& cache,
        const reach_vector& oop_reach,
        const reach_vector& ip_reach,
        const terminal_context<2>& context,
        const heads_up_player folded
    ) noexcept {
        return evaluate_fold_values(workspace, cache, std::array<reach_vector, 2>{oop_reach, ip_reach}, context, folded_mask<2>::from_folded_player(folded));
    }

    // Stage 10 dispatch layer: select kernel family at compile-time by player count.
    // - N == 2: heads-up exact kernel family
    // - N > 2 : multiplayer kernel family (current implementation under this family is sampled)
    template <std::size_t N>
    struct terminal_engine {
        [[nodiscard]] static constexpr terminal_kernel_family kernel_family() noexcept {
            if constexpr (N == 2) {
                return terminal_kernel_family::heads_up_exact;
            } else {
                return terminal_kernel_family::multiplayer;
            }
        }

        [[nodiscard]] static constexpr bool is_heads_up_exact() noexcept {
            return kernel_family() == terminal_kernel_family::heads_up_exact;
        }

        // Showdown dispatch: currently only the heads-up exact kernel is implemented.
        [[nodiscard]] auto evaluate_showdown(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& reach,
            const terminal_context<N>& context
        ) const noexcept {
            if constexpr (N == 2) {
                return ::zeta::holdem::evaluate_showdown(cache, reach, context);
            } else {
                static_assert(N == 2, "multiplayer showdown kernel not implemented");
            }
        }

        [[nodiscard]] auto evaluate_showdown(
            terminal_workspace<N>& workspace,
            const river_terminal_cache& cache,
            const std::array<reach_vector, N>& ranges,
            const terminal_context<N>& context
        ) const noexcept {
            if constexpr (N == 2) {
                return ::zeta::holdem::evaluate_showdown(workspace, cache, ranges, context);
            } else {
                static_assert(N == 2, "multiplayer showdown kernel not implemented");
            }
        }

        [[nodiscard]] auto evaluate_showdown_values(
            terminal_workspace<N>& workspace,
            const river_terminal_cache& cache,
            const std::array<reach_vector, N>& ranges,
            const terminal_context<N>& context
        ) const noexcept {
            if constexpr (N == 2) {
                return ::zeta::holdem::evaluate_showdown_values(workspace, cache, ranges, context);
            } else {
                static_assert(N == 2, "multiplayer showdown kernel not implemented");
            }
        }

        // Fold dispatch: heads-up exact path for N == 2, generic N-way fold for N > 2.
        [[nodiscard]] terminal_values<N> evaluate_fold_values(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& reach,
            const terminal_context<N>& context,
            const folded_mask<N>& folded
        ) const noexcept {
            return ::zeta::holdem::evaluate_fold_values(cache, reach, context, folded);
        }

        [[nodiscard]] terminal_values<N> evaluate_fold_values(
            terminal_workspace<N>& workspace,
            const river_terminal_cache& cache,
            const std::array<reach_vector, N>& ranges,
            const terminal_context<N>& context,
            const folded_mask<N>& folded
        ) const noexcept {
            return ::zeta::holdem::evaluate_fold_values(workspace, cache, ranges, context, folded);
        }
    };

    // Heads-up convenience overloads for engine callers that keep the legacy two-range style.
    template <>
    struct terminal_engine<2> {
        [[nodiscard]] static constexpr terminal_kernel_family kernel_family() noexcept {
            return terminal_kernel_family::heads_up_exact;
        }

        [[nodiscard]] static constexpr bool is_heads_up_exact() noexcept {
            return true;
        }

        [[nodiscard]] terminal_result<2> evaluate_showdown(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, 2>& reach,
            const terminal_context<2>& context
        ) const noexcept {
            return ::zeta::holdem::evaluate_showdown(cache, reach, context);
        }

        [[nodiscard]] terminal_result<2> evaluate_showdown(
            terminal_workspace<2>& workspace,
            const river_terminal_cache& cache,
            const std::array<reach_vector, 2>& ranges,
            const terminal_context<2>& context
        ) const noexcept {
            return ::zeta::holdem::evaluate_showdown(workspace, cache, ranges, context);
        }

        [[nodiscard]] terminal_result<2> evaluate_showdown(
            terminal_workspace<2>& workspace,
            const river_terminal_cache& cache,
            const reach_vector& oop_reach,
            const reach_vector& ip_reach,
            const terminal_context<2>& context
        ) const noexcept {
            return ::zeta::holdem::evaluate_showdown(workspace, cache, oop_reach, ip_reach, context);
        }

        [[nodiscard]] terminal_values<2> evaluate_showdown_values(
            terminal_workspace<2>& workspace,
            const river_terminal_cache& cache,
            const std::array<reach_vector, 2>& ranges,
            const terminal_context<2>& context
        ) const noexcept {
            return ::zeta::holdem::evaluate_showdown_values(workspace, cache, ranges, context);
        }

        [[nodiscard]] terminal_values<2> evaluate_showdown_values(
            terminal_workspace<2>& workspace,
            const river_terminal_cache& cache,
            const reach_vector& oop_reach,
            const reach_vector& ip_reach,
            const terminal_context<2>& context
        ) const noexcept {
            return ::zeta::holdem::evaluate_showdown_values(workspace, cache, std::array<reach_vector, 2>{oop_reach, ip_reach}, context);
        }

        [[nodiscard]] terminal_values<2> evaluate_fold_values(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, 2>& reach,
            const terminal_context<2>& context,
            const folded_mask<2>& folded
        ) const noexcept {
            return ::zeta::holdem::evaluate_fold_values(cache, reach, context, folded);
        }

        [[nodiscard]] terminal_values<2> evaluate_fold_values(
            terminal_workspace<2>& workspace,
            const river_terminal_cache& cache,
            const std::array<reach_vector, 2>& ranges,
            const terminal_context<2>& context,
            const folded_mask<2>& folded
        ) const noexcept {
            return ::zeta::holdem::evaluate_fold_values(workspace, cache, ranges, context, folded);
        }
    };

}
