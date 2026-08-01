#pragma once

#include <algorithm>
#include <array>
#include <bitset>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <span>
#include <vector>

#include "board.h"
#include "eval/evaluator.h"
#include "range.h"

namespace zeta::holdem {

    /**
     * Heads-up seat identity. Only meaningful for 2-player terminals; N-way
     * kernels address seats by index (0..N-1). `player` is retained as a
     * back-compat alias so existing call sites compile unchanged.
     */
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

    /**
     * Player-indexed mask for terminal and betting state.
     */
    template <std::size_t N>
    struct player_mask {
        std::bitset<N> bits;

        [[nodiscard]] constexpr bool operator[](const std::size_t seat) const noexcept {
            return bits[seat];
        }

        constexpr void set(const std::size_t seat, const bool value = true) noexcept {
            bits.set(seat, value);
        }

        [[nodiscard]] constexpr std::size_t count() const noexcept {
            return bits.count();
        }
    };

    /**
     * N-way folded mask. Bit i is true when seat i has folded.
     */
    template <std::size_t N>
    struct folded_mask {
        player_mask<N> players;

        [[nodiscard]] constexpr bool operator[](std::size_t seat) const noexcept {
            return players[seat];
        }

        constexpr void set_folded(const std::size_t seat, const bool value) noexcept {
            players.set(seat, value);
        }
    };

    /**
     * Heads-up specialization: genuine optimization with direct boolean fields,
     * not bitset. This avoids any bitset overhead in the fast path.
     */
    template <>
    struct folded_mask<2> {
        bool oop_folded = false;
        bool ip_folded = false;

        /** Accessor for compatibility with generic interface. */
        [[nodiscard]] constexpr bool operator[](const std::size_t seat) const noexcept {
            return seat == 0 ? oop_folded : ip_folded;
        }

        /** Helper to set folded state. */
        constexpr void set_folded(const std::size_t seat, const bool value) noexcept {
            if (seat == 0) {
                oop_folded = value;
            } else {
                ip_folded = value;
            }
        }

        /** Factory from heads_up_player for compatibility. */
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

    /**
     * Player-neutral, compile-time-sized terminal accounting context.
     * For N active players, `contribution[seat]` is that seat's contribution.
     * Heads-up is terminal_context<2> (contribution[0]=oop, contribution[1]=ip).
     */
    template <std::size_t N>
    struct terminal_context {
        utility gross_pot = 0.0;
        utility rake = 0.0;
        std::array<utility, N> contribution{};
    };

    enum class terminal_state_kind : uint8_t {
        none = 0,
        showdown = 1,
        fold = 2,
        timeout = 3,
        rake_adjusted = 4,
        variant_specific = 5
    };

    struct terminal_rake_adjustment_payload {
        utility rake = 0.0;
    };

    struct terminal_variant_payload {
        uint32_t payload_id = 0;
    };

    /**
     * Auditable pot layer used by terminal records.
     */
    template <std::size_t N>
    struct pot_layer {
        utility amount = 0.0;
        player_mask<N> eligible_mask{};
        player_mask<N> contributors_mask{};
    };

    /**
     * Terminal accounting record selected by terminal leaves.
     */
    template <std::size_t N>
    struct terminal_state {
        terminal_state_kind kind = terminal_state_kind::none;
        terminal_context<N> context{};
        std::vector<pot_layer<N>> pot_layers{};
        folded_mask<N> folded{};
        player_mask<N> all_in_eligible_mask{};
        player_mask<N> active_eligible_mask{};
        uint32_t variant_payload_id = 0;
    };

    /**
     * Owning table for terminal states referenced by graph terminal leaves.
     */
    template <std::size_t N>
    struct terminal_state_table {
        std::vector<terminal_state<N>> states;

        [[nodiscard]] std::span<const terminal_state<N>> view() const noexcept {
            return states;
        }

        [[nodiscard]] std::size_t size() const noexcept {
            return states.size();
        }

        [[nodiscard]] bool contains(const uint32_t state_id) const noexcept {
            return state_id < states.size();
        }

        [[nodiscard]] const terminal_state<N>& operator[](const uint32_t state_id) const noexcept {
            assert(contains(state_id));
            return states[state_id];
        }
    };

    template <std::size_t N>
    [[nodiscard]] terminal_state<N> make_terminal_state(
        const terminal_state_kind kind,
        const terminal_context<N>& context,
        const std::vector<pot_layer<N>>& pot_layers,
        const folded_mask<N>& folded = {},
        const player_mask<N>& all_in_eligible_mask = {},
        const player_mask<N>& active_eligible_mask = {},
        const uint32_t variant_payload_id = 0
    ) {
        return terminal_state<N>{
            .kind = kind,
            .context = context,
            .pot_layers = pot_layers,
            .folded = folded,
            .all_in_eligible_mask = all_in_eligible_mask,
            .active_eligible_mask = active_eligible_mask,
            .variant_payload_id = variant_payload_id
        };
    }

    /**
     * Zero-cost clarity alias: heads-up accounting is exactly terminal_context<2>
     * (contribution[0]=oop, contribution[1]=ip). No separate type is needed; this
     * keeps the single templated context boundary while naming the heads-up intent.
     */
    using heads_up_context = terminal_context<2>;

    struct terminal_payoff {
        utility oop = 0.0;
        utility ip = 0.0;
    };

    /** Build a heads-up context from explicit pot accounting. */
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

    template <std::size_t N>
    [[nodiscard]] constexpr utility total_pot_amount(const std::vector<pot_layer<N>>& pot_layers) noexcept {
        utility total = 0.0;
        for (const auto& layer : pot_layers) {
            total += layer.amount;
        }
        return total;
    }

    template <std::size_t N>
    [[nodiscard]] constexpr utility rake_adjusted_layer_amount(
        const terminal_context<N>& context,
        const std::vector<pot_layer<N>>& pot_layers,
        const std::size_t layer_index
    ) noexcept {
        assert(layer_index < pot_layers.size());
        const auto gross = total_pot_amount(pot_layers);
        if (gross <= 0.0 || context.rake <= 0.0) {
            return pot_layers[layer_index].amount;
        }
        const auto rake = std::min(context.rake, gross);
        return pot_layers[layer_index].amount - (rake * (pot_layers[layer_index].amount / gross));
    }

    template <std::size_t N>
    [[nodiscard]] pot_layer<N> make_main_pot_layer(const terminal_context<N>& context) noexcept {
        pot_layer<N> layer{};
        layer.amount = context.gross_pot;
        for (std::size_t seat = 0; seat < N; ++seat) {
            if (context.contribution[seat] > 0.0) {
                layer.contributors_mask.set(seat);
            }
            layer.eligible_mask.set(seat);
        }
        return layer;
    }

    template <std::size_t N>
    [[nodiscard]] std::vector<pot_layer<N>> make_default_pot_layers(const terminal_context<N>& context) {
        return {make_main_pot_layer(context)};
    }

    template <std::size_t N>
    [[nodiscard]] terminal_state<N> make_rake_adjusted_terminal_state(
        const terminal_context<N>& context,
        const std::vector<pot_layer<N>>& pot_layers,
        const terminal_rake_adjustment_payload payload
    ) {
        auto state = make_terminal_state(terminal_state_kind::rake_adjusted, context, pot_layers);
        state.context.rake = payload.rake;
        return state;
    }

    template <std::size_t N>
    [[nodiscard]] terminal_state<N> make_variant_terminal_state(
        const terminal_context<N>& context,
        const std::vector<pot_layer<N>>& pot_layers,
        const terminal_variant_payload payload
    ) {
        return make_terminal_state(
            terminal_state_kind::variant_specific,
            context,
            pot_layers,
            {},
            {},
            {},
            payload.payload_id
        );
    }

    template <std::size_t N>
    [[nodiscard]] terminal_state<N> make_showdown_terminal_state(const terminal_context<N>& context) {
        terminal_state<N> state{};
        state.kind = terminal_state_kind::showdown;
        state.context = context;
        state.pot_layers.push_back(make_main_pot_layer(context));
        for (std::size_t seat = 0; seat < N; ++seat) {
            state.active_eligible_mask.set(seat);
        }
        return state;
    }

    template <std::size_t N>
    [[nodiscard]] terminal_state<N> make_fold_terminal_state(
        const terminal_context<N>& context,
        const folded_mask<N>& folded
    ) {
        terminal_state<N> state{};
        state.kind = terminal_state_kind::fold;
        state.context = context;
        state.folded = folded;
        state.pot_layers.push_back(make_main_pot_layer(context));
        for (std::size_t seat = 0; seat < N; ++seat) {
            if (!folded[seat]) {
                state.active_eligible_mask.set(seat);
            }
        }
        return state;
    }

    [[nodiscard]] inline terminal_state<2> make_fold_terminal_state(
        const terminal_context<2>& context,
        const heads_up_player folded
    ) {
        return make_fold_terminal_state(context, folded_mask<2>::from_folded_player(folded));
    }

    /**
     * Recover the heads-up pot accounting from a two-player context so the
     * existing payoff helpers can be reused unchanged.
     */
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

    /**
     * Payoff infrastructure for pot distribution.
     *
     * Separates hand ranking (showdown) from payoff computation. Side-pot
     * handling, rake application, and eligibility are payoff concerns, not
     * ranking concerns.
     *
     * The policy hooks below support bucketed ranges, generalized rake models,
     * and memory-layout hints without changing terminal evaluation APIs.
     */

    /**
     * Side pot representation: accumulates contributions toward a particular pot.
     * Each seat's total winnings is distributed across main pot + side pots[0..n-1].
     */
    template <std::size_t N>
    struct side_pot {
        /** Seats that contributed to this pot and are thus eligible to win it. */
        player_mask<N> eligible{};
        /** Total amount in this pot before distribution. */
        utility amount = 0.0;
    };

    /**
     * Rake policy abstraction: how rake is deducted from the gross pot.
     *
     * Default (linear): rake = f * gross_pot (capped at max_rake if present).
     * Examples:
     *   - No-flop rake: zero on side pots, full rate on main pot.
     *   - Time collection: rake = time_amount (fixed).
     *   - Rake cap: rake = min(f * gross_pot, cap).
     *   - Tournament: rake = zero.
     */
    struct rake_policy {
        /** Standard online poker rake: fraction of the pot (e.g., 0.05 for 5%). */
        float rate = 0.0f;
        /** Optional cap: rake cannot exceed this amount. Zero = no cap. */
        float max_rake = 0.0f;

        /**
         * Compute rake deducted from a pot. Override this function (or use
         * a derived class / custom policy) to implement different rake models.
         */
        [[nodiscard]] constexpr utility compute_rake(const utility gross_amount) const noexcept {
            utility computed = static_cast<utility>(rate) * gross_amount;
            if (max_rake > 0.0f) {
                computed = std::min(computed, static_cast<utility>(max_rake));
            }
            return computed;
        }
    };

    /**
     * Range data policy: abstraction for range representation.
     *
     * The evaluator's core algorithm works with per-combo weighting. This policy
     * allows a future implementation to feed in bucketed ranges, sampled subsets,
     * or importance-weighted distributions without changing the payoff kernel.
     */
    struct range_data_policy {
        /**
         * Placeholder: could be specialized for exact_range_policy,
         * bucketed_range_policy, sampled_range_policy, or abstract_range_policy.
         * For now, this is a marker struct. The evaluator uses raw reach_vector.
         */
    };

    /**
     * Memory layout policy: hints for parallelism and NUMA optimization.
     *
     * The evaluator is single-threaded, but the workspace and cache can be
     * placed according to these hints when used in a parallel CFR solver.
     */
    struct memory_layout_policy {
        /**
         * Alignment requirement for workspace allocation (e.g., 64 for cache line).
         * Zero = default alignment.
         */
        size_t alignment = 0;

        /**
         * NUMA affinity node, if relevant (e.g., for thread-local workspaces).
         * -1 = no preference (system chooses).
         */
        int numa_node = -1;

        /**
         * True if this workspace is read-only (sharable across threads).
         * False = thread-local only.
         */
        bool is_shared = false;

        /** Memory size estimate for planning (diagnostic only; not enforced). */
        [[nodiscard]] static constexpr size_t estimate_workspace_bytes(std::size_t N) noexcept {
            /** Rough estimate: N reach indices at ~129 KB each + scratch buffers. */
            return N * 129'000 + 8'000;
        }
    };

    /**
     * N-way pot structure: main pot + side pots, rake policy, and active-set mask.
     * Separates payoff computation from hand ranking.
     */
    template <std::size_t N>
    struct pot_structure {
        /** Which seats are active and eligible to win (complement of folded_mask). */
        player_mask<N> active{};

        /**
         * Main pot and side pots. For simplicity, we accumulate side pots linearly.
         * A more complex representation could track per-player all-in amounts.
         */
        std::vector<side_pot<N>> pots;

        /**
         * Rake policy: determines how rake is deducted.
         * Default is linear: rate * gross_pot (capped).
         */
        rake_policy rake{};

        /** Range data policy: future hook for bucketed/sampled ranges. */
        range_data_policy range_policy{};

        /** Memory layout hints for NUMA-aware allocation and thread pinning. */
        memory_layout_policy memory_policy{};

        /**
         * Initialize main pot with all active seats eligible.
         * Call after setting active and before distributing side pots.
         */
        constexpr void initialize_main_pot(const utility gross_pot) noexcept {
            pots.clear();
            pots.emplace_back();
            pots[0].eligible = active;
            pots[0].amount = gross_pot;
        }

        /** Total pot balance (sum of all side pots). */
        [[nodiscard]] constexpr utility total_pot_balance() const noexcept {
            utility total = 0.0;
            for (const auto& pot : pots) {
                total += pot.amount;
            }
            return total;
        }

        /** Count of active players. */
        [[nodiscard]] constexpr std::size_t active_count() const noexcept {
            return active.count();
        }
    };

    /**
     * Heads-up specialization: no side pots, simpler structure.
     * This keeps the data layout tight for the fast path.
     */
    template <>
    struct pot_structure<2> {
        bool oop_active = true;
        bool ip_active = true;

        utility main_pot = 0.0;
        /** No side pots for heads-up (binary all-in semantics). */

        rake_policy rake{};
        range_data_policy range_policy{};
        memory_layout_policy memory_policy{};

        /** Accessors for generic interface compatibility. */
        [[nodiscard]] constexpr std::size_t active_count() const noexcept {
            return (oop_active ? 1 : 0) + (ip_active ? 1 : 0);
        }

        [[nodiscard]] constexpr utility total_pot_balance() const noexcept {
            return main_pot;
        }
    };

    using value_array = std::array<terminal_value, combination_count>;

    /**
     * Templated structure-of-arrays: one contiguous per-combo value array per
     * active player. terminal_values<2> is exactly the heads-up layout. Even at
     * 6 players this is 6 * combination_count floats (~31 KB), so no nesting or
     * compression is needed.
     */
    template <std::size_t N>
    struct terminal_values {
        std::array<value_array, N> player_values{};

        /** Heads-up ergonomic access by seat enum (valid while N >= 2). */
        [[nodiscard]] constexpr const value_array& operator[](const heads_up_player p) const noexcept {
            return player_values[player_index(p)];
        }

        [[nodiscard]] constexpr value_array& operator[](const heads_up_player p) noexcept {
            return player_values[player_index(p)];
        }

        /** Seat-indexed access for the generic (N-way) form. */
        [[nodiscard]] constexpr const value_array& operator[](const std::size_t seat) const noexcept {
            return player_values[seat];
        }

        [[nodiscard]] constexpr value_array& operator[](const std::size_t seat) noexcept {
            return player_values[seat];
        }
    };

    /**
     * Aggregate EV / win-tie accounting for an evaluated terminal. The summary is
     * inherently kernel-specific: the heads-up specialization below exposes the
     * lower/equal/higher decomposition (oop vs ip). An N-way summary would carry a
     * different shape, so the primary template is left unimplemented on purpose.
     */
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

    /**
     * Result bundle for an evaluated terminal. Fully templated on the player count
     * so a future N-way kernel returns terminal_result<N> rather than being forced
     * through a permanently heads-up (two-seat) shape.
     */
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

}
