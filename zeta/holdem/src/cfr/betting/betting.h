#pragma once

#include "cfr/graph/builder.h"
#include "cfr/graph/validation.h"
#include "cfr/solver/infoset_planning.h"
#include "cfr/solver/iteration.h"
#include "terminal/terminal_types.h"

#include <algorithm>
#include <array>
#include <bit>
#include <cstdint>
#include <expected>
#include <limits>
#include <ostream>
#include <span>
#include <vector>

namespace zeta::holdem::cfr {

    enum class betting_action_kind : uint8_t {
        fold = 0,
        check = 1,
        call = 2,
        bet = 3,
        raise = 4,
        all_in = 5
    };

    struct betting_action {
        betting_action_kind kind = betting_action_kind::check;
        utility amount = 0.0;      /**< Chips added by the acting player. */
        utility target_bet = 0.0;  /**< Actor's committed amount after the action. */
    };

    struct betting_action_record {
        uint8_t actor = solver::INVALID_PLAYER;
        betting_action action{};
    };

    enum class betting_validation_error_kind : uint8_t {
        invalid_actor,
        invalid_stack,
        invalid_commitment,
        invalid_current_bet,
        invalid_terminal_state,
        illegal_action,
        memory_plan_failed,
        graph_build_failed
    };

    struct betting_validation_error {
        betting_validation_error_kind kind{};
        uint32_t state_id = 0;
        uint32_t node_id = game_graph::INVALID_NODE;
        solver::cfr_memory_plan_error memory_plan_error{};
        graph_build_error graph_error{};
    };

    [[nodiscard]] constexpr const char* to_string(const betting_validation_error_kind kind) noexcept
    {
        using enum betting_validation_error_kind;
        switch (kind) {
            case invalid_actor:         return "betting_validation_error_kind::invalid_actor";
            case invalid_stack:         return "betting_validation_error_kind::invalid_stack";
            case invalid_commitment:    return "betting_validation_error_kind::invalid_commitment";
            case invalid_current_bet:   return "betting_validation_error_kind::invalid_current_bet";
            case invalid_terminal_state:return "betting_validation_error_kind::invalid_terminal_state";
            case illegal_action:        return "betting_validation_error_kind::illegal_action";
            case memory_plan_failed:    return "betting_validation_error_kind::memory_plan_failed";
            case graph_build_failed:    return "betting_validation_error_kind::graph_build_failed";
        }
        return "betting_validation_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const betting_validation_error_kind kind)
    {
        return os << to_string(kind);
    }

    template <std::size_t N>
    struct betting_state {
        solver::holdem_street street = solver::holdem_street::river;
        uint8_t actor = 0;
        std::array<utility, N> stacks{};
        std::array<utility, N> committed{};
        folded_mask<N> folded{};
        player_mask<N> all_in{};
        utility current_bet = 0.0;
        uint16_t raise_count = 0;
        std::vector<betting_action_record> action_history{};
        std::vector<pot_layer<N>> pot_layers{};
        terminal_state_kind terminal_kind = terminal_state_kind::none;
        std::array<bool, N> acted_since_aggression{};

        [[nodiscard]] bool terminal() const noexcept
        {
            return terminal_kind != terminal_state_kind::none;
        }
    };

    struct betting_abstraction_policy {
        std::array<std::vector<double>, 5> street_pot_fractions{};
        std::vector<double> fixed_pot_fractions{0.5, 1.0};
        std::vector<double> stack_ratio_buckets{};
        uint16_t geometric_size_count = 0;
        double forced_all_in_threshold = 0.95;
        uint16_t max_raises_per_street = 2;
        utility min_raise = 1.0;

        [[nodiscard]] std::span<const double> fractions_for_street(const solver::holdem_street street) const noexcept
        {
            const auto index = static_cast<std::size_t>(street);
            if (index < street_pot_fractions.size() && !street_pot_fractions[index].empty()) {
                return street_pot_fractions[index];
            }
            return fixed_pot_fractions;
        }
    };

    template <std::size_t N>
    struct holdem_betting_graph_config {
        solver::holdem_street street = solver::holdem_street::river;
        std::array<utility, N> initial_stacks{};
        std::array<utility, N> initial_committed{};
        uint8_t root_actor = 0;
        betting_abstraction_policy abstraction{};
        uint16_t max_history = 16;
        uint32_t public_state_id = 0;
        solver::cfr_memory_plan_options memory_plan_options{};
        solver::cfr_memory_plan_limits memory_plan_limits{};
    };

    template <std::size_t N>
    struct holdem_betting_graph {
        game_graph graph{};
        solver::solver_graph_annotations annotations{};
        terminal_state_table<N> terminal_states{};
        std::vector<solver::cfr_terminal_leaf> terminal_leaves{};
        std::vector<solver::solver_node_state_metadata> rich_state_metadata{};
        uint64_t deterministic_hash = 0;
    };

    namespace detail {

        [[nodiscard]] inline uint64_t hash_combine(const uint64_t value, const uint64_t input) noexcept
        {
            uint64_t hash = value;
            for (uint32_t shift = 0; shift < 64; shift += 8) {
                hash ^= (input >> shift) & 0xffu;
                hash *= solver::compatibility_hasher::PRIME;
            }
            return hash;
        }

        [[nodiscard]] inline uint64_t hash_utility(uint64_t hash, const utility value) noexcept
        {
            return hash_combine(hash, std::bit_cast<uint64_t>(value));
        }

        template <std::size_t N>
        [[nodiscard]] uint8_t next_actor(const betting_state<N>& state) noexcept
        {
            for (std::size_t offset = 1; offset <= N; ++offset) {
                const auto candidate = static_cast<uint8_t>((state.actor + offset) % N);
                if (!state.folded[candidate] && !state.all_in[candidate]) {
                    return candidate;
                }
            }
            return solver::INVALID_PLAYER;
        }

        template <std::size_t N>
        [[nodiscard]] uint32_t active_count(const betting_state<N>& state) noexcept
        {
            uint32_t count = 0;
            for (std::size_t seat = 0; seat < N; ++seat) {
                if (!state.folded[seat]) {
                    ++count;
                }
            }
            return count;
        }

        template <std::size_t N>
        [[nodiscard]] bool betting_round_complete(const betting_state<N>& state) noexcept
        {
            for (std::size_t seat = 0; seat < N; ++seat) {
                if (state.folded[seat] || state.all_in[seat]) {
                    continue;
                }
                if (!state.acted_since_aggression[seat] || state.committed[seat] < state.current_bet) {
                    return false;
                }
            }
            return true;
        }

        template <std::size_t N>
        [[nodiscard]] std::vector<pot_layer<N>> make_pot_layers(const betting_state<N>& state)
        {
            std::vector<utility> levels;
            levels.reserve(N);
            for (const auto contribution : state.committed) {
                if (contribution > 0.0) {
                    levels.push_back(contribution);
                }
            }
            std::sort(levels.begin(), levels.end());
            levels.erase(std::unique(levels.begin(), levels.end()), levels.end());

            std::vector<pot_layer<N>> layers;
            utility previous = 0.0;
            for (const auto level : levels) {
                pot_layer<N> layer{};
                const auto slice = level - previous;
                if (slice <= 0.0) {
                    continue;
                }
                uint32_t contributors = 0;
                for (std::size_t seat = 0; seat < N; ++seat) {
                    if (state.committed[seat] >= level) {
                        layer.contributors_mask.set(seat);
                        ++contributors;
                        if (!state.folded[seat]) {
                            layer.eligible_mask.set(seat);
                        }
                    }
                }
                layer.amount = slice * static_cast<utility>(contributors);
                layers.push_back(layer);
                previous = level;
            }
            return layers;
        }

        template <std::size_t N>
        [[nodiscard]] terminal_state<N> make_terminal_state_from_betting(const betting_state<N>& state)
        {
            terminal_context<N> context{};
            for (std::size_t seat = 0; seat < N; ++seat) {
                context.contribution[seat] = state.committed[seat];
                context.gross_pot += state.committed[seat];
            }

            terminal_state<N> terminal{};
            terminal.kind = state.terminal_kind;
            terminal.context = context;
            terminal.folded = state.folded;
            terminal.all_in_eligible_mask = state.all_in;
            terminal.pot_layers = make_pot_layers(state);
            for (std::size_t seat = 0; seat < N; ++seat) {
                if (!state.folded[seat]) {
                    terminal.active_eligible_mask.set(seat);
                }
            }
            return terminal;
        }

        template <std::size_t N>
        void settle_if_terminal(betting_state<N>& state) noexcept
        {
            if (active_count(state) <= 1u) {
                state.terminal_kind = terminal_state_kind::fold;
                state.actor = solver::INVALID_PLAYER;
                return;
            }
            if (betting_round_complete(state) || next_actor(state) == solver::INVALID_PLAYER) {
                state.terminal_kind = terminal_state_kind::showdown;
                state.actor = solver::INVALID_PLAYER;
            }
        }

        [[nodiscard]] inline bool contains_action_kind(
            const std::span<const betting_action> actions,
            const betting_action_kind kind,
            const utility target_bet) noexcept
        {
            for (const auto& action : actions) {
                if (action.kind == kind && action.target_bet == target_bet) {
                    return true;
                }
            }
            return false;
        }
    }

    template <std::size_t N>
    [[nodiscard]] std::expected<void, betting_validation_error> validate_betting_state(
        const betting_state<N>& state,
        const uint32_t state_id = 0) noexcept
    {
        if (state.terminal()) {
            if (state.actor != solver::INVALID_PLAYER) {
                return std::unexpected(betting_validation_error{betting_validation_error_kind::invalid_terminal_state, state_id});
            }
        } else if (state.actor >= N || state.folded[state.actor] || state.all_in[state.actor]) {
            return std::unexpected(betting_validation_error{betting_validation_error_kind::invalid_actor, state_id});
        }

        utility max_committed = 0.0;
        for (std::size_t seat = 0; seat < N; ++seat) {
            if (state.stacks[seat] < 0.0) {
                return std::unexpected(betting_validation_error{betting_validation_error_kind::invalid_stack, state_id});
            }
            if (state.committed[seat] < 0.0) {
                return std::unexpected(betting_validation_error{betting_validation_error_kind::invalid_commitment, state_id});
            }
            max_committed = std::max(max_committed, state.committed[seat]);
        }
        if (state.current_bet < 0.0 || state.current_bet < max_committed) {
            return std::unexpected(betting_validation_error{betting_validation_error_kind::invalid_current_bet, state_id});
        }
        return {};
    }

    template <std::size_t N>
    [[nodiscard]] std::vector<betting_action> legal_betting_actions(
        const betting_state<N>& state,
        const betting_abstraction_policy& policy)
    {
        std::vector<betting_action> actions;
        if (state.terminal() || state.actor >= N || state.folded[state.actor] || state.all_in[state.actor]) {
            return actions;
        }

        const auto actor = state.actor;
        const auto to_call = std::max<utility>(0.0, state.current_bet - state.committed[actor]);
        const auto stack = state.stacks[actor];
        if (to_call > 0.0) {
            actions.push_back(betting_action{
                .kind = betting_action_kind::fold,
                .amount = 0.0,
                .target_bet = state.committed[actor]
            });
            actions.push_back(betting_action{
                .kind = betting_action_kind::call,
                .amount = std::min(stack, to_call),
                .target_bet = state.committed[actor] + std::min(stack, to_call)
            });
        } else {
            actions.push_back(betting_action{
                .kind = betting_action_kind::check,
                .amount = 0.0,
                .target_bet = state.committed[actor]
            });
        }

        const auto can_aggress = stack > to_call && state.raise_count < policy.max_raises_per_street;
        if (can_aggress) {
            utility pot = 0.0;
            for (const auto contribution : state.committed) {
                pot += contribution;
            }
            pot = std::max<utility>(policy.min_raise, pot);
            std::vector<utility> target_bets;
            for (const auto fraction : policy.fractions_for_street(state.street)) {
                if (fraction <= 0.0) {
                    continue;
                }
                const auto increment = std::max(policy.min_raise, pot * static_cast<utility>(fraction));
                target_bets.push_back(state.current_bet + increment);
            }
            if (policy.geometric_size_count > 0u) {
                const auto all_in_target = state.committed[actor] + stack;
                for (uint16_t i = 1; i <= policy.geometric_size_count; ++i) {
                    const auto t = static_cast<utility>(i) / static_cast<utility>(policy.geometric_size_count + 1u);
                    target_bets.push_back(state.current_bet + (all_in_target - state.current_bet) * t);
                }
            }
            for (const auto ratio : policy.stack_ratio_buckets) {
                if (ratio > 0.0) {
                    target_bets.push_back(state.committed[actor] + stack * static_cast<utility>(ratio));
                }
            }

            std::sort(target_bets.begin(), target_bets.end());
            target_bets.erase(std::unique(target_bets.begin(), target_bets.end()), target_bets.end());
            const auto all_in_target = state.committed[actor] + stack;
            for (auto target : target_bets) {
                target = std::min(target, all_in_target);
                if (target <= state.current_bet || target <= state.committed[actor]) {
                    continue;
                }
                const auto commits_all_in = target >= all_in_target * policy.forced_all_in_threshold;
                const auto kind = state.current_bet == 0.0 ? betting_action_kind::bet : betting_action_kind::raise;
                if (!commits_all_in && !detail::contains_action_kind(actions, kind, target)) {
                    actions.push_back(betting_action{
                        .kind = kind,
                        .amount = target - state.committed[actor],
                        .target_bet = target
                    });
                }
            }
        }

        if (stack > 0.0) {
            const auto target = state.committed[actor] + stack;
            if (!detail::contains_action_kind(actions, betting_action_kind::all_in, target)) {
                actions.push_back(betting_action{
                    .kind = betting_action_kind::all_in,
                    .amount = stack,
                    .target_bet = target
                });
            }
        }

        return actions;
    }

    template <std::size_t N>
    [[nodiscard]] std::expected<betting_state<N>, betting_validation_error> apply_betting_action(
        const betting_state<N>& state,
        const betting_action& action,
        const betting_abstraction_policy& policy,
        const uint32_t state_id = 0)
    {
        if (auto result = validate_betting_state(state, state_id); !result) {
            return std::unexpected(result.error());
        }

        const auto legal = legal_betting_actions(state, policy);
        bool matched = false;
        for (const auto& candidate : legal) {
            if (candidate.kind == action.kind && candidate.target_bet == action.target_bet) {
                matched = true;
                break;
            }
        }
        if (!matched) {
            return std::unexpected(betting_validation_error{betting_validation_error_kind::illegal_action, state_id});
        }

        auto next = state;
        const auto actor = static_cast<std::size_t>(state.actor);
        next.action_history.push_back(betting_action_record{state.actor, action});

        if (action.kind == betting_action_kind::fold) {
            next.folded.set_folded(actor, true);
            next.acted_since_aggression[actor] = true;
        } else {
            const auto chips = std::min(next.stacks[actor], std::max<utility>(0.0, action.target_bet - next.committed[actor]));
            next.stacks[actor] -= chips;
            next.committed[actor] += chips;
            if (next.stacks[actor] == 0.0 || action.kind == betting_action_kind::all_in) {
                next.all_in.set(actor);
            }
            next.acted_since_aggression[actor] = true;
            if (action.kind == betting_action_kind::bet
                || action.kind == betting_action_kind::raise
                || (action.kind == betting_action_kind::all_in && next.committed[actor] > next.current_bet)) {
                next.current_bet = next.committed[actor];
                ++next.raise_count;
                next.acted_since_aggression.fill(false);
                next.acted_since_aggression[actor] = true;
            }
        }

        detail::settle_if_terminal(next);
        if (!next.terminal()) {
            next.actor = detail::next_actor(next);
        }
        next.pot_layers = detail::make_pot_layers(next);
        return next;
    }

    template <std::size_t N>
    [[nodiscard]] betting_state<N> make_initial_betting_state(const holdem_betting_graph_config<N>& config)
    {
        betting_state<N> state{};
        state.street = config.street;
        state.actor = config.root_actor;
        state.stacks = config.initial_stacks;
        state.committed = config.initial_committed;
        state.current_bet = *std::max_element(state.committed.begin(), state.committed.end());
        state.pot_layers = detail::make_pot_layers(state);
        return state;
    }

    template <std::size_t N>
    [[nodiscard]] std::expected<solver::cfr_memory_estimate, solver::cfr_memory_plan_error> estimate_betting_graph_memory(
        const holdem_betting_graph_config<N>& config) noexcept
    {
        const auto street_fractions = config.abstraction.fractions_for_street(config.street);
        uint64_t max_actions_per_state = 3u;
        if (!solver::checked_add(max_actions_per_state, street_fractions.size())
            || !solver::checked_add(max_actions_per_state, config.abstraction.geometric_size_count)
            || !solver::checked_add(max_actions_per_state, config.abstraction.stack_ratio_buckets.size())) {
            return std::unexpected(solver::cfr_memory_plan_error{solver::cfr_memory_plan_error_kind::estimate_overflow});
        }
        max_actions_per_state = std::max<uint64_t>(max_actions_per_state, 1u);

        uint64_t node_count = 1u;
        uint64_t nodes_at_depth = 1u;
        for (uint16_t depth = 0; depth < config.max_history; ++depth) {
            if (!solver::checked_mul(nodes_at_depth, max_actions_per_state, nodes_at_depth)
                || !solver::checked_add(node_count, nodes_at_depth)) {
                return std::unexpected(solver::cfr_memory_plan_error{solver::cfr_memory_plan_error_kind::estimate_overflow});
            }
        }

        const auto edge_count = node_count == 0u ? 0u : node_count - 1u;
        return solver::estimate_cfr_memory(
            solver::cfr_memory_shape{
                .node_count = node_count,
                .edge_count = edge_count,
                .infoset_count = node_count,
                .action_value_count = edge_count,
                .max_depth = config.max_history
            },
            config.memory_plan_options,
            config.memory_plan_limits);
    }

    template <std::size_t N>
    [[nodiscard]] uint64_t hash_betting_graph(const holdem_betting_graph<N>& lowered) noexcept
    {
        uint64_t hash = solver::compatibility_hasher::OFFSET;
        const auto& graph = lowered.graph;
        hash = detail::hash_combine(hash, N);
        hash = detail::hash_combine(hash, graph.node_count);
        hash = detail::hash_combine(hash, graph.root_node);
        hash = detail::hash_combine(hash, graph.edges.size());
        hash = detail::hash_combine(hash, graph.infoset_count);
        hash = detail::hash_combine(hash, graph.terminal_count);
        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            hash = detail::hash_combine(hash, static_cast<uint64_t>(graph.node_types[node_id]));
            hash = detail::hash_combine(hash, graph.infoset_id[node_id]);
            hash = detail::hash_combine(hash, lowered.annotations.actor_by_node[node_id]);
            hash = detail::hash_combine(hash, lowered.annotations.terminal_leaf_id_by_node[node_id]);
            hash = detail::hash_combine(hash, static_cast<uint64_t>(lowered.annotations.state_by_node[node_id].street));
            hash = detail::hash_combine(hash, lowered.annotations.state_by_node[node_id].public_state_id);
            hash = detail::hash_combine(hash, lowered.annotations.state_by_node[node_id].betting_state_id);
        }
        for (const auto& edge : graph.edges) {
            hash = detail::hash_combine(hash, edge.child_node);
            hash = detail::hash_combine(hash, edge.action_index);
        }
        for (const auto& state : lowered.terminal_states.states) {
            hash = detail::hash_combine(hash, static_cast<uint64_t>(state.kind));
            for (const auto contribution : state.context.contribution) {
                hash = detail::hash_utility(hash, contribution);
            }
            for (const auto& layer : state.pot_layers) {
                hash = detail::hash_utility(hash, layer.amount);
            }
        }
        return hash;
    }

    template <std::size_t N>
    [[nodiscard]] std::expected<holdem_betting_graph<N>, betting_validation_error> lower_betting_tree_to_graph(
        const holdem_betting_graph_config<N>& config)
    {
        auto root_state = make_initial_betting_state(config);
        if (auto result = validate_betting_state(root_state); !result) {
            return std::unexpected(result.error());
        }

        if (auto memory_result = estimate_betting_graph_memory(config); !memory_result) {
            return std::unexpected(betting_validation_error{
                .kind = betting_validation_error_kind::memory_plan_failed,
                .memory_plan_error = memory_result.error()
            });
        }

        graph_builder builder;
        std::vector<uint8_t> actor_by_infoset;
        std::vector<terminal_state<N>> terminal_states_in_leaf_order;
        uint32_t next_infoset_id = 0;

        auto add_state = [&](const betting_state<N>& state) {
            const auto old_node = builder.add_node(state.terminal() ? node_kind::terminal : node_kind::player);
            if (!state.terminal()) {
                builder.set_infoset_id(old_node, next_infoset_id);
                actor_by_infoset.push_back(state.actor);
                ++next_infoset_id;
            } else {
                terminal_states_in_leaf_order.push_back(detail::make_terminal_state_from_betting(state));
            }
            return old_node;
        };

        auto root = add_state(root_state);
        builder.set_root(root);

        auto expand = [&](auto&& self, const uint32_t parent_node, const betting_state<N>& state) -> std::expected<void, betting_validation_error> {
            if (state.terminal()) {
                return {};
            }
            if (state.action_history.size() >= config.max_history) {
                return std::unexpected(betting_validation_error{betting_validation_error_kind::invalid_terminal_state});
            }
            const auto actions = legal_betting_actions(state, config.abstraction);
            for (uint16_t action_index = 0; action_index < static_cast<uint16_t>(actions.size()); ++action_index) {
                auto child = apply_betting_action(state, actions[action_index], config.abstraction);
                if (!child) {
                    return std::unexpected(child.error());
                }
                const auto child_node = add_state(*child);
                builder.add_edge(parent_node, child_node, action_index);
                if (auto result = self(self, child_node, *child); !result) {
                    return result;
                }
            }
            return {};
        };

        if (auto result = expand(expand, root, root_state); !result) {
            return std::unexpected(result.error());
        }

        auto graph_result = builder.build();
        if (!graph_result) {
            return std::unexpected(betting_validation_error{
                .kind = betting_validation_error_kind::graph_build_failed,
                .graph_error = graph_result.error()
            });
        }

        holdem_betting_graph<N> lowered;
        lowered.graph = std::move(*graph_result);
        lowered.annotations.actor_by_node.assign(lowered.graph.node_count, solver::INVALID_PLAYER);
        lowered.annotations.chance_event_id_by_node.assign(lowered.graph.node_count, solver::INVALID_METADATA_ID);
        lowered.annotations.terminal_leaf_id_by_node.assign(lowered.graph.node_count, solver::INVALID_METADATA_ID);
        lowered.annotations.state_by_node.assign(lowered.graph.node_count, {});
        lowered.terminal_leaves.assign(lowered.graph.node_count, solver::cfr_terminal_leaf{});
        lowered.rich_state_metadata.assign(lowered.graph.node_count, {});

        uint32_t terminal_state_id = 0;
        for (uint32_t node_id = 0; node_id < lowered.graph.node_count; ++node_id) {
            lowered.annotations.state_by_node[node_id] = solver::solver_node_state_metadata{
                .street = config.street,
                .public_state_id = config.public_state_id,
                .betting_state_id = node_id
            };
            lowered.rich_state_metadata[node_id] = lowered.annotations.state_by_node[node_id];
            if (lowered.graph.is_player_node(node_id)) {
                lowered.annotations.actor_by_node[node_id] = actor_by_infoset[lowered.graph.infoset_id[node_id]];
            }
            if (lowered.graph.is_terminal(node_id)) {
                lowered.annotations.terminal_leaf_id_by_node[node_id] = terminal_state_id;
                lowered.terminal_leaves[node_id].terminal_state_id = terminal_state_id;
                lowered.terminal_states.states.push_back(terminal_states_in_leaf_order[terminal_state_id]);
                ++terminal_state_id;
            }
        }

        if (auto result = graph_validation::validate_all(lowered.graph); !result) {
            return std::unexpected(betting_validation_error{
                .kind = betting_validation_error_kind::graph_build_failed,
                .node_id = result.error().node_id,
                .graph_error = result.error()
            });
        }
        if (auto result = validate_solver_graph_view(make_solver_graph_view<N>(lowered.graph, lowered.annotations)); !result) {
            return std::unexpected(betting_validation_error{
                .kind = betting_validation_error_kind::invalid_terminal_state,
                .node_id = result.error().node_id
            });
        }

        lowered.deterministic_hash = hash_betting_graph(lowered);
        return lowered;
    }

}
