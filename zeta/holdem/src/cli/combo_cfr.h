#pragma once

// Combo-vectorised CFR+ solver behind cli::solve_spot: strategy tables, node locks,
// counterfactual-value evaluation, exploitability, hashing and artifact extraction.
// Kept out of solve_cli.h so code that only needs the spot and artifact types does not
// compile the solver. Include it only to call the solver internals directly (tests).

#include "cli/solve_cli.h"
#include "cfr/scheduler/scheduler.h"
#include "cfr/tables/table_layout.h"
#include "terminal.h"

#include <algorithm>
#include <array>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <expected>
#include <limits>
#include <memory>
#include <numeric>
#include <optional>
#include <span>
#include <string>
#include <vector>

namespace zeta::holdem::cli {

    namespace detail {

        // A pool stopped by cancellation may have skipped terminal leaves, so its last
        // batch is incomplete; checking it as well as the request keeps the stop sticky
        // even if the callback's answer changes.
        [[nodiscard]] inline bool solve_stopped(
            const solve_runtime_options& runtime,
            const cfr::scheduler::worker_pool& pool)
        {
            return pool.stopped() || solve_cancel_requested(runtime);
        }

        // Lets a long batch of terminal leaves stop between chunks once cancelled.
        inline void stop_pool_on_cancel(cfr::scheduler::worker_pool& pool, const solve_runtime_options& runtime)
        {
            if (runtime.cancellation_requested) {
                pool.set_stop_condition([&runtime] { return solve_cancel_requested(runtime); });
            }
        }

        struct combo_action_table {
            std::vector<float> values;
            std::vector<uint32_t> action_offsets;
            uint32_t combo_stride = 0;

            combo_action_table() = default;

            explicit combo_action_table(const cfr::action_table_layout& layout) :
                values(static_cast<std::size_t>(combination_count) * layout.value_count(), 0.0f),
                action_offsets(layout.action_offsets),
                combo_stride(layout.value_count())
            {
            }

            [[nodiscard]] uint32_t action_count(const uint32_t infoset_id) const noexcept
            {
                return cfr::table_action_count(action_offsets, infoset_id);
            }

            [[nodiscard]] uint32_t offset(const uint32_t infoset_id, const uint32_t action_index) const noexcept
            {
                return cfr::table_value_offset(action_offsets, infoset_id, action_index);
            }

            [[nodiscard]] std::span<float> combo_infoset(
                const combination_index combo,
                const uint32_t infoset_id) noexcept
            {
                const auto begin = static_cast<std::size_t>(combo) * combo_stride + action_offsets[infoset_id];
                const auto count = action_count(infoset_id);
                return count == 0u ? std::span<float>{} : std::span<float>{values.data() + begin, count};
            }

            [[nodiscard]] std::span<const float> combo_infoset(
                const combination_index combo,
                const uint32_t infoset_id) const noexcept
            {
                const auto begin = static_cast<std::size_t>(combo) * combo_stride + action_offsets[infoset_id];
                const auto count = action_count(infoset_id);
                return count == 0u ? std::span<const float>{} : std::span<const float>{values.data() + begin, count};
            }

            [[nodiscard]] float& value(
                const combination_index combo,
                const uint32_t infoset_id,
                const uint32_t action_index) noexcept
            {
                return values[static_cast<std::size_t>(combo) * combo_stride + offset(infoset_id, action_index)];
            }

            [[nodiscard]] const float& value(
                const combination_index combo,
                const uint32_t infoset_id,
                const uint32_t action_index) const noexcept
            {
                return values[static_cast<std::size_t>(combo) * combo_stride + offset(infoset_id, action_index)];
            }
        };

        template <std::size_t N>
        [[nodiscard]] inline std::array<std::vector<combination_index>, N> active_combos_by_player(
            const std::array<reach_vector, N>& reach_vectors)
        {
            std::array<std::vector<combination_index>, N> out{};
            for (std::size_t seat = 0; seat < N; ++seat) {
                for (combination_index combo = 0; combo < combination_count; ++combo) {
                    if (reach_vectors[seat][combo] > 0.0f) {
                        out[seat].push_back(combo);
                    }
                }
            }
            return out;
        }

        // ---------------------------------------------------------------------
        // Action labels.
        //
        // A player node's edges follow legal_betting_actions for its betting state, so
        // replaying the betting from the root recovers every action. A bet or raise is
        // labelled by the chips it adds as a percentage of the gross pot before it: the
        // spot's gross_pot plus every chip committed since the root, which is the root's
        // existing convention carried down the tree. Labels repeated at one node get a
        // ".2", ".3", ... suffix so each is unique. Chance edges are labelled by the dealt
        // card(s).
        // ---------------------------------------------------------------------

        inline void make_labels_unique(std::vector<std::string>& labels)
        {
            for (std::size_t index = 1; index < labels.size(); ++index) {
                uint32_t occurrence = 1;
                for (std::size_t earlier = 0; earlier < index; ++earlier) {
                    if (labels[earlier] == labels[index]
                        || labels[earlier].starts_with(labels[index] + ".")) {
                        ++occurrence;
                    }
                }
                if (occurrence > 1) {
                    labels[index] += "." + std::to_string(occurrence);
                }
            }
        }

        [[nodiscard]] inline std::string dealt_cards_label(const card_mask cards)
        {
            std::string label;
            for (const auto& text : card_texts_from_mask(cards)) {
                label += text;
            }
            return label;
        }

        /**
         * Labels of every edge, indexed [node][action_index]. Terminal nodes have none.
         * A player node whose replayed actions do not match its edges (which would mean
         * a lowering change) falls back to index labels rather than failing the solve.
         */
        template <std::size_t N>
        [[nodiscard]] inline std::vector<std::vector<std::string>> make_edge_labels(
            const cfr::game_graph& graph,
            const cfr::holdem_betting_graph_config<N>& config,
            const cfr::chance_event_table* chance_events,
            const utility gross_pot)
        {
            std::vector<std::vector<std::string>> labels(graph.node_count);
            utility initial_committed = 0.0;
            for (const auto committed : config.initial_committed) {
                initial_committed += committed;
            }
            struct pending {
                uint32_t node_id;
                cfr::betting_state<N> state;
            };
            std::vector<pending> stack;
            stack.push_back(pending{graph.root_node, cfr::make_initial_betting_state(config)});
            while (!stack.empty()) {
                auto [node_id, state] = std::move(stack.back());
                stack.pop_back();
                const auto edges = graph.out_edges(node_id);
                auto& node_labels = labels[node_id];
                if (graph.node_types[node_id] == cfr::node_kind::chance) {
                    const auto next_street = static_cast<cfr::solver::holdem_street>(static_cast<uint8_t>(state.street) + 1u);
                    const auto* event = chance_events != nullptr ? chance_events->event_for_node(node_id) : nullptr;
                    node_labels.assign(edges.size(), std::string{});
                    for (const auto& edge : edges) {
                        if (event != nullptr) {
                            for (const auto& outcome : chance_events->event_outcomes(*event)) {
                                if (outcome.action_index == edge.action_index) {
                                    node_labels[edge.action_index] = dealt_cards_label(outcome.cards);
                                    break;
                                }
                            }
                        }
                        stack.push_back(pending{
                            edge.child_node,
                            cfr::detail::make_next_street_state(state, next_street, config.root_actor)});
                    }
                    continue;
                }
                if (!graph.is_player_node(node_id)) {
                    continue;
                }
                const auto actions = cfr::legal_betting_actions(state, config.abstraction);
                node_labels.reserve(edges.size());
                if (actions.size() != edges.size()) {
                    for (uint32_t action_index = 0; action_index < edges.size(); ++action_index) {
                        node_labels.push_back(std::to_string(action_index));
                    }
                    continue;
                }
                utility committed = 0.0;
                for (const auto chips : state.committed) {
                    committed += chips;
                }
                const auto pot = gross_pot + (committed - initial_committed);
                for (const auto& action : actions) {
                    node_labels.push_back(action_label(action, pot));
                }
                make_labels_unique(node_labels);
                for (const auto& edge : edges) {
                    auto child = cfr::apply_betting_action(state, actions[edge.action_index], config.abstraction);
                    if (child) {
                        stack.push_back(pending{edge.child_node, std::move(*child)});
                    }
                }
            }
            return labels;
        }

        // ---------------------------------------------------------------------
        // Node locks.
        //
        // A lock fixes some actions' probabilities at a player node and leaves the rest
        // free. With locked mass S and residual r = 1 - S, the played strategy is p_a on
        // locked actions and r * (regret matching over the free actions) on free ones.
        // Free regrets are updated against the free-only value, scaled by r; locked
        // regrets never change. See doc/holdem/plans/node_locking_plan.md.
        // ---------------------------------------------------------------------

        /** One lock's constraint at one node. */
        struct lock_constraint {
            std::vector<float> probability;  /**< Fixed probability per action (0 for free actions). */
            std::vector<uint8_t> locked;     /**< 1 for locked actions. */
            float residual = 1.0f;           /**< Probability left for the free actions. */
            uint32_t free_count = 0;
            uint32_t lock_index = 0;         /**< Position in solve_spot::locks. */
            bool hand_scoped = false;        /**< True when the lock names hands. */
        };

        /** Immutable lock view the solver consults; a null pointer means no locks. */
        struct lock_runtime {
            static constexpr int32_t no_lock = -1;

            std::vector<lock_constraint> constraints;
            /** Per infoset: empty when unlocked, otherwise one constraint index per combo. */
            std::vector<std::vector<int32_t>> by_infoset;
            /** Per node: the lock indices applied there, ascending. */
            std::vector<std::vector<uint32_t>> lock_indices_by_node;

            [[nodiscard]] bool infoset_locked(const uint32_t infoset_id) const noexcept
            {
                return !by_infoset[infoset_id].empty();
            }

            [[nodiscard]] const lock_constraint* find(
                const uint32_t infoset_id,
                const combination_index combo) const noexcept
            {
                const auto& by_combo = by_infoset[infoset_id];
                if (by_combo.empty() || by_combo[combo] == no_lock) {
                    return nullptr;
                }
                return &constraints[static_cast<std::size_t>(by_combo[combo])];
            }
        };

        /**
         * Writes the constrained strategy for one hand: locked actions take their fixed
         * probability and free actions share the residual in proportion to `weights`'
         * positive parts, or uniformly when none is positive. `weights` may alias `out`.
         */
        inline void apply_lock_constraint(
            const lock_constraint& constraint,
            const std::span<const float> weights,
            const std::span<float> out) noexcept
        {
            float free_positive = 0.0f;
            for (std::size_t action_index = 0; action_index < weights.size(); ++action_index) {
                if (constraint.locked[action_index] == 0u) {
                    free_positive += std::max(weights[action_index], 0.0f);
                }
            }
            const auto uniform = constraint.free_count == 0u
                ? 0.0f
                : constraint.residual / static_cast<float>(constraint.free_count);
            for (std::size_t action_index = 0; action_index < out.size(); ++action_index) {
                if (constraint.locked[action_index] != 0u) {
                    out[action_index] = constraint.probability[action_index];
                } else {
                    out[action_index] = free_positive > 0.0f
                        ? constraint.residual * (std::max(weights[action_index], 0.0f) / free_positive)
                        : uniform;
                }
            }
        }

        [[nodiscard]] inline std::string join_labels(const std::vector<std::string>& labels)
        {
            std::string out;
            for (const auto& label : labels) {
                out += out.empty() ? label : ", " + label;
            }
            return out;
        }

        /** The player nodes a lock's path reaches, or an error naming the failing step. */
        [[nodiscard]] inline std::expected<std::vector<uint32_t>, cli_error> resolve_lock_path(
            const cfr::game_graph& graph,
            const std::vector<std::vector<std::string>>& labels,
            const node_lock& lock,
            const std::size_t lock_index,
            const bool card_isomorphism)
        {
            std::vector<uint32_t> frontier{graph.root_node};
            for (std::size_t step = 0; step < lock.path.size(); ++step) {
                const auto& token = lock.path[step];
                const auto step_text = "path step " + std::to_string(step + 1u) + " ('" + token + "')";
                std::vector<uint32_t> next;
                for (const auto node_id : frontier) {
                    const auto edges = graph.out_edges(node_id);
                    if (graph.is_terminal(node_id)) {
                        return std::unexpected(lock_error(lock_index, step_text + " goes past the end of the hand."));
                    }
                    const bool chance = graph.node_types[node_id] == cfr::node_kind::chance;
                    if (chance && token == node_lock_any_card) {
                        for (const auto& edge : edges) {
                            next.push_back(edge.child_node);
                        }
                        continue;
                    }
                    if (!chance && token == node_lock_any_card) {
                        return std::unexpected(lock_error(lock_index,
                            step_text + " is a player decision; '*' only matches a dealt card. Actions here: "
                                + join_labels(labels[node_id]) + "."));
                    }
                    const auto& node_labels = labels[node_id];
                    const auto found = std::ranges::find(node_labels, token);
                    if (found == node_labels.end()) {
                        if (chance) {
                            return std::unexpected(lock_error(lock_index, step_text + " is not a card dealt here"
                                + std::string{card_isomorphism
                                    ? " (with card isomorphism a card names its suit class; use the class's representative card or '*')."
                                    : "; name the dealt card or use '*'."}));
                        }
                        return std::unexpected(lock_error(lock_index,
                            step_text + " is not an action here. Actions: " + join_labels(node_labels) + "."));
                    }
                    const auto action_index = static_cast<uint32_t>(std::distance(node_labels.begin(), found));
                    for (const auto& edge : edges) {
                        if (edge.action_index == action_index) {
                            next.push_back(edge.child_node);
                            break;
                        }
                    }
                }
                frontier = std::move(next);
            }
            for (const auto node_id : frontier) {
                if (graph.is_terminal(node_id)) {
                    return std::unexpected(lock_error(lock_index, "the path ends at the end of the hand, not at a decision."));
                }
                if (graph.node_types[node_id] == cfr::node_kind::chance) {
                    return std::unexpected(lock_error(lock_index,
                        "the path ends where a card is dealt; add the card or '*' and the next decision."));
                }
            }
            std::ranges::sort(frontier);
            frontier.erase(std::unique(frontier.begin(), frontier.end()), frontier.end());
            return frontier;
        }

        /**
         * Resolves the spot's locks against the lowered tree. Returns no runtime when the
         * spot has no locks, so the solver's no-lock path is unchanged.
         */
        template <std::size_t N>
        [[nodiscard]] inline std::expected<std::optional<lock_runtime>, cli_error> resolve_node_locks(
            const struct solve_spot& spot,
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const std::vector<std::vector<std::string>>& labels,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const bool card_isomorphism)
        {
            if (spot.locks.empty()) {
                return std::optional<lock_runtime>{};
            }
            lock_runtime runtime{};
            runtime.by_infoset.resize(graph.infoset_count);
            runtime.lock_indices_by_node.resize(graph.node_count);
            std::vector<uint8_t> range_locked(graph.infoset_count, 0u);

            // Whole-range locks first, so a hand lock can override them whatever the order.
            std::vector<std::size_t> order(spot.locks.size());
            std::iota(order.begin(), order.end(), std::size_t{0});
            std::ranges::stable_sort(order, [&](const std::size_t lhs, const std::size_t rhs) {
                return spot.locks[lhs].hands.empty() && !spot.locks[rhs].hands.empty();
            });

            for (const auto lock_index : order) {
                const auto& lock = spot.locks[lock_index];
                const bool hand_scoped = !lock.hands.empty();
                if (hand_scoped && card_isomorphism) {
                    return std::unexpected(lock_error(lock_index,
                        "a lock on specific hands breaks suit symmetry, so it cannot be solved with card isomorphism. "
                        "Turn card isomorphism off or lock the whole range."));
                }
                auto targets = resolve_lock_path(graph, labels, lock, lock_index, card_isomorphism);
                if (!targets) {
                    return std::unexpected(targets.error());
                }

                std::vector<uint8_t> hand_selected;
                if (hand_scoped) {
                    const auto parsed = parse_range(lock.hands);
                    if (!parsed.ok()) {
                        return std::unexpected(lock_error(lock_index,
                            "invalid hands at position " + std::to_string(parsed.error.position) + ": "
                                + range_parse_error_text(parsed.error.code) + "."));
                    }
                    hand_selected.assign(combination_count, 0u);
                    for (combination_index combo = 0; combo < combination_count; ++combo) {
                        hand_selected[combo] = parsed.range.weights[combo] > 0.0f ? 1u : 0u;
                    }
                }

                for (const auto node_id : *targets) {
                    const auto infoset_id = graph.infoset_id[node_id];
                    const auto actor = annotations.actor_by_node[node_id];
                    const auto& node_labels = labels[node_id];
                    const auto action_count = static_cast<uint32_t>(node_labels.size());

                    lock_constraint constraint{};
                    constraint.probability.assign(action_count, 0.0f);
                    constraint.locked.assign(action_count, 0u);
                    constraint.lock_index = static_cast<uint32_t>(lock_index);
                    constraint.hand_scoped = hand_scoped;
                    double locked_mass = 0.0;
                    for (const auto& action : lock.actions) {
                        const auto found = std::ranges::find(node_labels, action.action);
                        if (found == node_labels.end()) {
                            return std::unexpected(lock_error(lock_index,
                                "'" + action.action + "' is not an action at the locked node. Actions: "
                                    + join_labels(node_labels) + "."));
                        }
                        const auto action_index = static_cast<std::size_t>(std::distance(node_labels.begin(), found));
                        constraint.locked[action_index] = 1u;
                        constraint.probability[action_index] = static_cast<float>(action.frequency);
                        locked_mass += action.frequency;
                    }
                    const auto locked_count = static_cast<uint32_t>(std::ranges::count(constraint.locked, uint8_t{1}));
                    constraint.free_count = action_count - locked_count;
                    if (constraint.free_count == 0u) {
                        if (std::fabs(locked_mass - 1.0) > 1.0e-6) {
                            return std::unexpected(lock_error(lock_index,
                                "it locks every action, so the frequencies must add up to 1."));
                        }
                        for (auto& probability : constraint.probability) {
                            probability = static_cast<float>(static_cast<double>(probability) / locked_mass);
                        }
                        constraint.residual = 0.0f;
                    } else {
                        constraint.residual = static_cast<float>(std::max(0.0, 1.0 - locked_mass));
                    }

                    auto& by_combo = runtime.by_infoset[infoset_id];
                    if (by_combo.empty()) {
                        by_combo.assign(combination_count, lock_runtime::no_lock);
                    }
                    const auto constraint_index = static_cast<int32_t>(runtime.constraints.size());
                    bool applied = false;
                    if (!hand_scoped) {
                        if (range_locked[infoset_id] != 0u) {
                            return std::unexpected(lock_error(lock_index,
                                "another whole-range lock already applies at this node."));
                        }
                        range_locked[infoset_id] = 1u;
                        for (const auto combo : active_combos[actor]) {
                            by_combo[combo] = constraint_index;
                        }
                        applied = !active_combos[actor].empty();
                    } else {
                        for (const auto combo : active_combos[actor]) {
                            if (hand_selected[combo] == 0u) {
                                continue;
                            }
                            const auto existing = by_combo[combo];
                            if (existing != lock_runtime::no_lock
                                && runtime.constraints[static_cast<std::size_t>(existing)].hand_scoped) {
                                return std::unexpected(lock_error(lock_index,
                                    "its hands overlap lock "
                                        + std::to_string(runtime.constraints[static_cast<std::size_t>(existing)].lock_index + 1u)
                                        + " at the same node (" + hand_text_from_combo(combo) + ")."));
                            }
                            by_combo[combo] = constraint_index;
                            applied = true;
                        }
                        if (!applied) {
                            return std::unexpected(lock_error(lock_index,
                                "its hands match nothing in " + spot.players[actor] + "'s range."));
                        }
                    }
                    runtime.constraints.push_back(std::move(constraint));
                    auto& node_locks = runtime.lock_indices_by_node[node_id];
                    node_locks.push_back(static_cast<uint32_t>(lock_index));
                    std::ranges::sort(node_locks);
                }
            }
            return std::optional<lock_runtime>{std::move(runtime)};
        }

        [[nodiscard]] inline uint64_t hash_node_locks(const std::vector<node_lock>& locks)
        {
            if (locks.empty()) {
                return 0u;
            }
            cfr::solver::compatibility_hasher hash;
            hash.add_u64(locks.size());
            for (const auto& lock : locks) {
                hash.add_u64(lock.path.size());
                for (const auto& step : lock.path) {
                    for (const char c : step) {
                        hash.add_u64(static_cast<uint64_t>(static_cast<unsigned char>(c)));
                    }
                    hash.add_u64(step.size());
                }
                // Hands by the combos they name, so equivalent range texts hash alike.
                std::vector<combination_index> combos;
                if (!lock.hands.empty()) {
                    const auto parsed = parse_range(lock.hands);
                    if (parsed.ok()) {
                        for (combination_index combo = 0; combo < combination_count; ++combo) {
                            if (parsed.range.weights[combo] > 0.0f) {
                                combos.push_back(combo);
                            }
                        }
                    }
                }
                hash.add_u64(lock.hands.empty() ? 0u : 1u);
                hash.add_u64(combos.size());
                for (const auto combo : combos) {
                    hash.add_u64(combo);
                }
                hash.add_u64(lock.actions.size());
                for (const auto& action : lock.actions) {
                    for (const char c : action.action) {
                        hash.add_u64(static_cast<uint64_t>(static_cast<unsigned char>(c)));
                    }
                    hash.add_u64(action.action.size());
                    hash.add_u64(static_cast<uint64_t>(std::llround(action.frequency / node_lock_frequency_step)));
                }
            }
            return hash.value;
        }

        /**
         * Mutable per-solve state backing dynamic action pruning. @ref action_active
         * carries the active action set per infoset (preserving the original action
         * indexing so persisted strategies stay aligned), @ref node_live marks nodes
         * still reachable through active edges, and @ref representative_node maps each
         * player infoset to a node that realizes it so reconsideration can read the
         * infoset actor and reach. A null @c pruning_runtime pointer means pruning is
         * off and the solver behaves exactly like the exact Step 2/3 solver.
         */
        struct pruning_runtime {
            static constexpr uint32_t no_node = std::numeric_limits<uint32_t>::max();

            dynamic_pruning_policy policy;
            std::vector<std::vector<uint8_t>> action_active;
            std::vector<uint8_t> node_live;
            std::vector<uint32_t> representative_node;

            [[nodiscard]] uint32_t reconsider_interval() const noexcept
            {
                return std::max<uint32_t>(1u, policy.reconsider_interval);
            }

            [[nodiscard]] uint32_t minimum_active_actions() const noexcept
            {
                return std::max<uint32_t>(1u, policy.minimum_active_actions);
            }
        };

        [[nodiscard]] inline pruning_runtime make_pruning_runtime(
            const cfr::game_graph& graph,
            const cfr::action_table_layout& layout,
            const dynamic_pruning_policy& policy)
        {
            pruning_runtime state{};
            state.policy = policy;
            state.action_active.resize(graph.infoset_count);
            for (uint32_t infoset = 0; infoset < graph.infoset_count; ++infoset) {
                state.action_active[infoset].assign(layout.action_count(infoset), 1u);
            }
            state.node_live.assign(graph.node_count, 1u);
            state.representative_node.assign(graph.infoset_count, pruning_runtime::no_node);
            for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
                if (!graph.is_player_node(node_id)) {
                    continue;
                }
                const auto infoset_id = graph.infoset_id[node_id];
                if (state.representative_node[infoset_id] == pruning_runtime::no_node) {
                    state.representative_node[infoset_id] = node_id;
                }
            }
            return state;
        }

        inline void recompute_node_live(const cfr::game_graph& graph, pruning_runtime& state)
        {
            std::ranges::fill(state.node_live, 0u);
            state.node_live[graph.root_node] = 1u;
            std::vector<uint32_t> stack{graph.root_node};
            while (!stack.empty()) {
                const auto node_id = stack.back();
                stack.pop_back();
                const bool player = graph.is_player_node(node_id);
                const auto infoset_id = player ? graph.infoset_id[node_id] : 0u;
                for (const auto& edge : graph.out_edges(node_id)) {
                    if (player && state.action_active[infoset_id][edge.action_index] == 0u) {
                        continue;
                    }
                    if (state.node_live[edge.child_node] != 0u) {
                        continue;
                    }
                    state.node_live[edge.child_node] = 1u;
                    stack.push_back(edge.child_node);
                }
            }
        }

        /**
         * Recompute each infoset's active action set from the current reach-weighted
         * positive regret. An action stays active when its share of the infoset's total
         * reach-weighted positive regret is at least @c prune_threshold; the highest
         * scoring actions are always kept so at least @c minimum_active_actions remain.
         * Frozen (previously pruned) actions can reactivate here because their retained
         * regret participates in the score without traversing their subtree.
         */
        template <std::size_t N>
        inline void reconsider_active_actions(
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const combo_action_table& regrets,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const std::vector<std::array<reach_vector, N>>& node_reach,
            pruning_runtime& state,
            const lock_runtime* locks = nullptr)
        {
            const auto min_active = state.minimum_active_actions();
            for (uint32_t infoset_id = 0; infoset_id < graph.infoset_count; ++infoset_id) {
                auto& active = state.action_active[infoset_id];
                const auto action_n = static_cast<uint32_t>(active.size());
                const auto node_id = state.representative_node[infoset_id];
                // A locked node keeps every action: pruning one would break its constraint.
                if (node_id == pruning_runtime::no_node || action_n <= min_active
                    || (locks != nullptr && locks->infoset_locked(infoset_id))) {
                    std::ranges::fill(active, 1u);
                    continue;
                }
                const auto actor = annotations.actor_by_node[node_id];
                std::vector<double> score(action_n, 0.0);
                double total = 0.0;
                for (const auto combo : active_combos[actor]) {
                    const auto reach = std::max(0.0f, node_reach[node_id][actor][combo]);
                    if (reach <= 0.0f) {
                        continue;
                    }
                    for (uint32_t action_index = 0; action_index < action_n; ++action_index) {
                        const auto contribution = static_cast<double>(reach)
                            * static_cast<double>(std::max(0.0f, regrets.value(combo, infoset_id, action_index)));
                        score[action_index] += contribution;
                        total += contribution;
                    }
                }
                if (total <= 0.0) {
                    std::ranges::fill(active, 1u);
                    continue;
                }
                const auto cutoff = state.policy.prune_threshold * total;
                std::ranges::fill(active, 0u);
                uint32_t active_count = 0;
                for (uint32_t action_index = 0; action_index < action_n; ++action_index) {
                    if (score[action_index] >= cutoff) {
                        active[action_index] = 1u;
                        ++active_count;
                    }
                }
                if (active_count < min_active) {
                    std::vector<uint32_t> order(action_n);
                    std::iota(order.begin(), order.end(), 0u);
                    std::ranges::sort(order, [&](const uint32_t lhs, const uint32_t rhs) {
                        return score[lhs] > score[rhs];
                    });
                    for (uint32_t rank = 0; rank < min_active; ++rank) {
                        active[order[rank]] = 1u;
                    }
                }
            }
        }

        template <std::size_t N>
        inline void normalize_combo_action_table(
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const combo_action_table& source,
            combo_action_table& destination,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const pruning_runtime* pruning = nullptr,
            const lock_runtime* locks = nullptr)
        {
            std::ranges::fill(destination.values, 0.0f);
            std::vector<uint8_t> seen_infosets(graph.infoset_count, 0u);
            for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
                if (!graph.is_player_node(node_id)) {
                    continue;
                }
                const auto infoset_id = graph.infoset_id[node_id];
                if (seen_infosets[infoset_id] != 0u) {
                    continue;
                }
                seen_infosets[infoset_id] = 1u;
                const auto actor = annotations.actor_by_node[node_id];
                if (locks != nullptr && locks->infoset_locked(infoset_id)) {
                    // Locked hands take their constraint; the rest regret-match as usual.
                    // Locked nodes are never pruned, so every action is active here.
                    const auto uniform = 1.0f / static_cast<float>(std::max<uint32_t>(1u, graph.action_count(node_id)));
                    for (const auto combo : active_combos[actor]) {
                        const auto src = source.combo_infoset(combo, infoset_id);
                        auto dst = destination.combo_infoset(combo, infoset_id);
                        if (const auto* constraint = locks->find(infoset_id, combo); constraint != nullptr) {
                            apply_lock_constraint(*constraint, src, dst);
                            continue;
                        }
                        float positive_sum = 0.0f;
                        for (const auto value : src) {
                            positive_sum += std::max(value, 0.0f);
                        }
                        for (std::size_t action_index = 0; action_index < dst.size(); ++action_index) {
                            dst[action_index] = positive_sum > 0.0f
                                ? std::max(src[action_index], 0.0f) / positive_sum
                                : uniform;
                        }
                    }
                    continue;
                }
                const auto* active = pruning ? &pruning->action_active[infoset_id] : nullptr;
                uint32_t active_count = graph.action_count(node_id);
                if (active != nullptr) {
                    active_count = 0u;
                    for (const auto flag : *active) {
                        active_count += flag;
                    }
                }
                const auto uniform = active_count == 0u ? 0.0f : 1.0f / static_cast<float>(active_count);
                for (const auto combo : active_combos[actor]) {
                    const auto src = source.combo_infoset(combo, infoset_id);
                    auto dst = destination.combo_infoset(combo, infoset_id);
                    float positive_sum = 0.0f;
                    for (std::size_t action_index = 0; action_index < src.size(); ++action_index) {
                        if (active != nullptr && (*active)[action_index] == 0u) {
                            continue;
                        }
                        positive_sum += std::max(src[action_index], 0.0f);
                    }
                    for (std::size_t action_index = 0; action_index < dst.size(); ++action_index) {
                        if (active != nullptr && (*active)[action_index] == 0u) {
                            dst[action_index] = 0.0f;
                            continue;
                        }
                        dst[action_index] = positive_sum > 0.0f
                            ? std::max(src[action_index], 0.0f) / positive_sum
                            : uniform;
                    }
                }
            }
        }

        template <std::size_t N>
        inline void build_node_reach_vectors(
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const combo_action_table& strategy_profile,
            const std::array<reach_vector, N>& root_reach,
            const std::array<std::vector<combination_index>, N>& active_combos,
            std::vector<std::array<reach_vector, N>>& node_reach,
            const pruning_runtime* pruning = nullptr)
        {
            node_reach.assign(graph.node_count, {});
            node_reach[graph.root_node] = root_reach;
            std::vector<uint32_t> stack{graph.root_node};
            while (!stack.empty()) {
                const auto node_id = stack.back();
                stack.pop_back();
                const auto& parent_reach = node_reach[node_id];
                const auto kind = graph.node_types[node_id];
                if (kind == cfr::node_kind::terminal) {
                    continue;
                }
                const auto actor = kind == cfr::node_kind::chance ? cfr::solver::INVALID_PLAYER : annotations.actor_by_node[node_id];
                const auto infoset_id = kind == cfr::node_kind::chance ? cfr::game_graph::INVALID_INFOSET : graph.infoset_id[node_id];
                const auto* active = (pruning && kind != cfr::node_kind::chance) ? &pruning->action_active[infoset_id] : nullptr;
                for (const auto& edge : graph.out_edges(node_id)) {
                    if (active != nullptr && (*active)[edge.action_index] == 0u) {
                        continue;
                    }
                    auto child_reach = parent_reach;
                    if (kind != cfr::node_kind::chance) {
                        for (const auto combo : active_combos[actor]) {
                            child_reach[actor][combo] *= strategy_profile.value(combo, infoset_id, edge.action_index);
                        }
                    }
                    node_reach[edge.child_node] = child_reach;
                    stack.push_back(edge.child_node);
                }
            }
        }

        // Graph nodes that are terminal leaves, in ascending id order. Terminal
        // evaluation is the dominant per-iteration cost and every leaf is independent,
        // so the evaluators below run it as one parallel batch over this list.
        [[nodiscard]] inline std::vector<uint32_t> terminal_node_ids(const cfr::game_graph& graph)
        {
            std::vector<uint32_t> terminals;
            for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
                if (graph.node_types[node_id] == cfr::node_kind::terminal) {
                    terminals.push_back(node_id);
                }
            }
            return terminals;
        }

        // Backs up a non-terminal node from its children, which precede it in the
        // graph's post-order. Chance children are weighted by `chance_probability`.
        // The updating player's own nodes fold in its strategy (or take the best child
        // for a best response). Every other player's node sums its children, because
        // that player's strategy is already in the reach the leaves were evaluated with.
        template <typename ChanceProbability>
        inline void backup_combo_node(
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const combo_action_table& strategy_profile,
            const std::vector<combination_index>& updating_combos,
            const uint8_t updating_player,
            const uint32_t node_id,
            std::vector<reach_vector>& node_values,
            const bool best_response,
            const lock_runtime* locks,
            ChanceProbability&& chance_probability)
        {
            auto& values = node_values[node_id];
            const auto edges = graph.out_edges(node_id);
            if (graph.node_types[node_id] == cfr::node_kind::chance) {
                for (const auto& edge : edges) {
                    const auto probability = chance_probability(edge);
                    for (const auto combo : updating_combos) {
                        values[combo] += probability * node_values[edge.child_node][combo];
                    }
                }
                return;
            }

            if (annotations.actor_by_node[node_id] != updating_player) {
                for (const auto& edge : edges) {
                    for (const auto combo : updating_combos) {
                        values[combo] += node_values[edge.child_node][combo];
                    }
                }
                return;
            }

            const auto infoset_id = graph.infoset_id[node_id];
            for (const auto combo : updating_combos) {
                const auto* constraint = (best_response && locks != nullptr) ? locks->find(infoset_id, combo) : nullptr;
                if (constraint != nullptr) {
                    // A locked hand's best response keeps the locked frequencies and
                    // puts the residual on its best free action.
                    float total = 0.0f;
                    float best_free = -std::numeric_limits<float>::infinity();
                    for (const auto& edge : edges) {
                        const auto child_value = node_values[edge.child_node][combo];
                        if (constraint->locked[edge.action_index] != 0u) {
                            total += constraint->probability[edge.action_index] * child_value;
                        } else {
                            best_free = std::max(best_free, child_value);
                        }
                    }
                    if (constraint->free_count > 0u && constraint->residual > 0.0f) {
                        total += constraint->residual * best_free;
                    }
                    values[combo] = total;
                } else if (best_response) {
                    float best = -std::numeric_limits<float>::infinity();
                    for (const auto& edge : edges) {
                        best = std::max(best, node_values[edge.child_node][combo]);
                    }
                    values[combo] = edges.empty() ? 0.0f : best;
                } else {
                    float total = 0.0f;
                    for (const auto& edge : edges) {
                        total += strategy_profile.value(combo, infoset_id, edge.action_index)
                            * node_values[edge.child_node][combo];
                    }
                    values[combo] = total;
                }
            }
        }

        // Counterfactual values for the updating player over a single-street river
        // betting tree, for any player count. Terminal leaves are evaluated in parallel
        // (each writes only its own node's values, so results do not depend on the
        // worker count); the backup then runs in post-order.
        template <std::size_t N>
        inline void evaluate_combo_profile(
            const cfr::holdem_betting_graph<N>& lowered,
            const combo_action_table& strategy_profile,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const std::vector<std::array<reach_vector, N>>& node_reach,
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& base_indices,
            const std::vector<uint32_t>& terminal_nodes,
            const uint8_t updating_player,
            std::vector<reach_vector>& node_values,
            cfr::scheduler::worker_pool& pool,
            const bool best_response = false,
            const lock_runtime* locks = nullptr)
        {
            node_values.assign(lowered.graph.node_count, {});
            const terminal_engine<N> engine{};
            pool.run(terminal_nodes.size(), [&](uint32_t, const uint64_t task_index) {
                const auto node_id = terminal_nodes[task_index];
                const auto& terminal_state = lowered.terminal_states[lowered.terminal_leaves[node_id].terminal_state_id];
                // The updating player's leaf values must not depend on its own reach,
                // so it keeps its root range; every other seat uses its reach here.
                if constexpr (N == 2) {
                    // Heads-up: pass the base index by reference to the two-index kernel
                    // rather than copying it into a heap-allocated reach array per leaf.
                    const auto opponent = static_cast<uint8_t>(1u - updating_player);
                    const auto opponent_index = make_river_reach_index(cache, node_reach[node_id][opponent]);
                    const auto values = updating_player == 0u
                        ? engine.evaluate_terminal_values(cache, base_indices[0], opponent_index, terminal_state)
                        : engine.evaluate_terminal_values(cache, opponent_index, base_indices[1], terminal_state);
                    for (const auto combo : active_combos[updating_player]) {
                        node_values[node_id][combo] = values[updating_player][combo];
                    }
                    return;
                }
                auto reach = std::make_unique<std::array<river_reach_index, N>>();
                for (std::size_t seat = 0; seat < N; ++seat) {
                    (*reach)[seat] = seat == updating_player
                        ? base_indices[seat]
                        : make_river_reach_index(cache, node_reach[node_id][seat]);
                }
                const auto values = engine.evaluate_terminal_values(cache, *reach, terminal_state);
                for (const auto combo : active_combos[updating_player]) {
                    node_values[node_id][combo] = values[updating_player][combo];
                }
            });

            for (uint32_t node_id = 0; node_id < lowered.graph.node_count; ++node_id) {
                if (lowered.graph.node_types[node_id] == cfr::node_kind::terminal) {
                    continue;
                }
                backup_combo_node(
                    lowered.graph, lowered.annotations, strategy_profile, active_combos[updating_player],
                    updating_player, node_id, node_values, best_response, locks,
                    [](const cfr::edge&) { return 1.0f; });
            }
        }

        // Blocker-aware opponent mass for a fold terminal on any street. Aggregates the
        // opponent's path reach over combos that are not blocked by the (partial or full)
        // board, so a hero combo's compatible mass follows the same inclusion-exclusion
        // the river fold kernel uses, but without a five-card terminal cache.
        struct fold_opponent_aggregate {
            double total = 0.0;
            std::array<double, 52> mass_by_card{};
        };

        [[nodiscard]] inline fold_opponent_aggregate accumulate_fold_opponent_mass(
            const reach_vector& opponent_reach,
            const std::vector<combination_index>& opponent_active,
            const card_mask board_mask)
        {
            fold_opponent_aggregate aggregate{};
            for (const auto combo : opponent_active) {
                const auto weight = opponent_reach[combo];
                if (weight <= 0.0f) {
                    continue;
                }
                const auto mask = combination_mask(combo);
                if ((mask & board_mask) != 0) {
                    continue;
                }
                aggregate.total += static_cast<double>(weight);
                const auto [first, second] = extract_combo_cards(mask);
                aggregate.mass_by_card[first] += static_cast<double>(weight);
                aggregate.mass_by_card[second] += static_cast<double>(weight);
            }
            return aggregate;
        }

        using combo_mass_array = std::array<double, combination_count>;

        // Reach mass of opponent hands compatible with each of `seat`'s hands at a node
        // whose public board is `board_mask`: the normaliser that turns a counterfactual
        // value into an EV in chips. Heads-up this is exact, using the same blocker
        // inclusion-exclusion as the fold kernel. Multiway it is the product of each
        // opponent's compatible mass, which ignores card collisions between opponents and
        // so slightly overstates the joint mass when their ranges share cards.
        template <std::size_t N>
        [[nodiscard]] inline combo_mass_array opponent_reach_masses(
            const std::array<reach_vector, N>& reach,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const card_mask board_mask,
            const uint8_t seat)
        {
            combo_mass_array masses{};
            for (const auto combo : active_combos[seat]) {
                if ((combination_mask(combo) & board_mask) == 0) {
                    masses[combo] = 1.0;
                }
            }
            for (uint8_t opponent = 0; opponent < N; ++opponent) {
                if (opponent == seat) {
                    continue;
                }
                const auto aggregate = accumulate_fold_opponent_mass(reach[opponent], active_combos[opponent], board_mask);
                for (const auto combo : active_combos[seat]) {
                    if (masses[combo] == 0.0) {
                        continue;
                    }
                    const auto [first, second] = extract_combo_cards(combination_mask(combo));
                    const auto self_weight = static_cast<double>(std::max(0.0f, reach[opponent][combo]));
                    masses[combo] *= clamp_compatible_mass(
                        aggregate.total - aggregate.mass_by_card[first] - aggregate.mass_by_card[second] + self_weight);
                }
            }
            return masses;
        }

        // Public board at every node of a multi-street public game.
        template <std::size_t N>
        [[nodiscard]] inline std::vector<card_mask> multi_street_board_by_node(const cfr::holdem_public_game_graph<N>& game)
        {
            std::vector<card_mask> boards(game.graph.node_count, 0u);
            for (uint32_t node_id = 0; node_id < game.graph.node_count; ++node_id) {
                const auto state_id = game.annotations.state_by_node[node_id].public_state_id;
                if (state_id < game.public_states.states.size()) {
                    boards[node_id] = game.public_states.states[state_id].board_cards;
                }
            }
            return boards;
        }

        // EV in chips from a counterfactual value and its opponent reach mass. A hand
        // with no compatible opponent hand has no defined EV and reports 0.
        [[nodiscard]] inline double ev_from_counterfactual_value(const double counterfactual_value, const double opponent_mass) noexcept
        {
            return opponent_mass > minimum_ev_reach_mass ? counterfactual_value / opponent_mass : 0.0;
        }

        // Counterfactual values for the updating player over the exact multi-street
        // public game. Terminal leaves (evaluated in parallel) dispatch to the per-river
        // terminal cache (showdown) or a blocker-aware fold evaluation (any street);
        // chance nodes weight each child by its outcome probability.
        template <std::size_t N>
        inline void evaluate_multi_street_combo_profile(
            const cfr::holdem_public_game_graph<N>& lowered,
            const combo_action_table& strategy_profile,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const std::vector<std::array<reach_vector, N>>& node_reach,
            const cfr::runout_terminal_table& terminal_table,
            const std::vector<std::array<river_reach_index, N>>& river_base_index_by_state,
            const std::vector<uint32_t>& terminal_nodes,
            const uint8_t updating_player,
            std::vector<reach_vector>& node_values,
            cfr::scheduler::worker_pool& pool,
            const pruning_runtime* pruning = nullptr,
            const bool best_response = false,
            const lock_runtime* locks = nullptr)
        {
            static_assert(N == 2, "multi-street public-game CFR is heads-up only");
            node_values.assign(lowered.graph.node_count, {});
            const terminal_engine<N> engine{};
            const auto opponent = static_cast<uint8_t>(1u - updating_player);
            pool.run(terminal_nodes.size(), [&](uint32_t, const uint64_t task_index) {
                const auto node_id = terminal_nodes[task_index];
                if (pruning != nullptr && pruning->node_live[node_id] == 0u) {
                    return;
                }
                const auto state_id = lowered.annotations.state_by_node[node_id].public_state_id;
                const auto& terminal_state = lowered.terminal_states[lowered.terminal_leaves[node_id].terminal_state_id];
                if (terminal_state.kind == terminal_state_kind::showdown) {
                    const auto* entry = terminal_table.find(state_id);
                    assert(entry != nullptr);
                    const auto& base_index = river_base_index_by_state[state_id][updating_player];
                    const auto opponent_index = make_river_reach_index(entry->cache, node_reach[node_id][opponent]);
                    const auto values = updating_player == 0u
                        ? engine.evaluate_terminal_values(entry->cache, base_index, opponent_index, terminal_state)
                        : engine.evaluate_terminal_values(entry->cache, opponent_index, base_index, terminal_state);
                    for (const auto combo : active_combos[updating_player]) {
                        node_values[node_id][combo] = values[updating_player][combo];
                    }
                    return;
                }

                const auto board_mask = lowered.public_states.states[state_id].board_cards;
                const auto payoff = ::zeta::holdem::detail::fold_payoff(terminal_state);
                const auto aggregate = accumulate_fold_opponent_mass(
                    node_reach[node_id][opponent], active_combos[opponent], board_mask);
                for (const auto combo : active_combos[updating_player]) {
                    const auto mask = combination_mask(combo);
                    if ((mask & board_mask) != 0) {
                        continue;
                    }
                    const auto [first, second] = extract_combo_cards(mask);
                    const auto self_weight = std::max(0.0f, node_reach[node_id][opponent][combo]);
                    const auto compatible = clamp_compatible_mass(
                        aggregate.total - aggregate.mass_by_card[first] - aggregate.mass_by_card[second]
                            + static_cast<double>(self_weight));
                    node_values[node_id][combo] = static_cast<float>(compatible * payoff[updating_player]);
                }
            });

            for (uint32_t node_id = 0; node_id < lowered.graph.node_count; ++node_id) {
                if (lowered.graph.node_types[node_id] == cfr::node_kind::terminal
                    || (pruning != nullptr && pruning->node_live[node_id] == 0u)) {
                    continue;
                }
                // Public outcome probabilities ignore the hole cards; rescale so each
                // outcome compatible with a hand pair carries its true conditional weight.
                const auto* event = lowered.chance_events.event_for_node(node_id);
                const auto private_scale = event == nullptr
                    ? 1.0
                    : cfr::private_hand_chance_scale(*event, static_cast<uint32_t>(N));
                backup_combo_node(
                    lowered.graph, lowered.annotations, strategy_profile, active_combos[updating_player],
                    updating_player, node_id, node_values, best_response, locks,
                    [&](const cfr::edge& edge) {
                        return static_cast<float>(private_scale * lowered.chance_events.probability_for_edge(node_id, edge));
                    });
            }
        }

        template <std::size_t N>
        inline void update_combo_cfr_tables(
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const combo_action_table& current_strategy,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const uint8_t updating_player,
            const std::vector<std::array<reach_vector, N>>& node_reach,
            const std::vector<reach_vector>& node_values,
            combo_action_table& regrets,
            combo_action_table& strategy_sums,
            const pruning_runtime* pruning = nullptr,
            const lock_runtime* locks = nullptr)
        {
            for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
                if (!graph.is_player_node(node_id)) {
                    continue;
                }
                if (pruning != nullptr && pruning->node_live[node_id] == 0u) {
                    continue;
                }
                const auto actor = annotations.actor_by_node[node_id];
                if (actor != updating_player) {
                    continue;
                }
                const auto infoset_id = graph.infoset_id[node_id];
                const auto* active = pruning ? &pruning->action_active[infoset_id] : nullptr;
                const auto edges = graph.out_edges(node_id);
                for (const auto combo : active_combos[updating_player]) {
                    const auto node_value = node_values[node_id][combo];
                    const auto path_weight = node_reach[node_id][updating_player][combo];
                    if (const auto* constraint = locks != nullptr ? locks->find(infoset_id, combo) : nullptr;
                        constraint != nullptr) {
                        // Free actions regret against the free-only value, scaled by the
                        // residual: r * (V_a - V_free) = r * V_a - sum_free sigma_b * V_b.
                        // Locked actions never accumulate regret; with no residual nothing does.
                        float free_value = 0.0f;
                        for (const auto& edge : edges) {
                            const auto action_probability = current_strategy.value(combo, infoset_id, edge.action_index);
                            strategy_sums.value(combo, infoset_id, edge.action_index) += path_weight * action_probability;
                            if (constraint->locked[edge.action_index] == 0u) {
                                free_value += action_probability * node_values[edge.child_node][combo];
                            }
                        }
                        if (constraint->free_count == 0u || constraint->residual <= 0.0f) {
                            continue;
                        }
                        for (const auto& edge : edges) {
                            if (constraint->locked[edge.action_index] != 0u) {
                                continue;
                            }
                            auto& regret = regrets.value(combo, infoset_id, edge.action_index);
                            regret = std::max(0.0f,
                                regret + constraint->residual * node_values[edge.child_node][combo] - free_value);
                        }
                        continue;
                    }
                    for (const auto& edge : edges) {
                        if (active != nullptr && (*active)[edge.action_index] == 0u) {
                            continue;
                        }
                        const auto action_probability = current_strategy.value(combo, infoset_id, edge.action_index);
                        strategy_sums.value(combo, infoset_id, edge.action_index) += path_weight * action_probability;
                        regrets.value(combo, infoset_id, edge.action_index) = std::max(
                            0.0f,
                            regrets.value(combo, infoset_id, edge.action_index)
                                + (node_values[edge.child_node][combo] - node_value));
                    }
                }
            }
        }

        // ---------------------------------------------------------------------
        // Robust convergence and exploitability reporting.
        //
        // For supported heads-up abstractions the solver measures an exact
        // best-response exploitability (half of NashConv) by reusing the vectorized
        // value backup with a max over the exploiter's actions. Multiway spots, where
        // an exact best response is expensive, report a normalized average-regret
        // metric instead. Reproducible content hashes and human-readable warnings are
        // always populated so a caller can tell whether a result is trustworthy.
        // ---------------------------------------------------------------------

        inline void hash_add_string(cfr::solver::compatibility_hasher& hash, const std::string_view text) noexcept
        {
            for (const char c : text) {
                hash.add_u64(static_cast<uint64_t>(static_cast<unsigned char>(c)));
            }
            hash.add_u64(text.size());
        }

        [[nodiscard]] inline uint64_t hash_reach_vectors(const std::span<const reach_vector> reach) noexcept
        {
            cfr::solver::compatibility_hasher hash;
            hash.add_u64(reach.size());
            for (const auto& vector : reach) {
                for (combination_index combo = 0; combo < combination_count; ++combo) {
                    hash.add_u64(static_cast<uint64_t>(std::bit_cast<uint32_t>(vector[combo])));
                }
            }
            return hash.value;
        }

        [[nodiscard]] inline uint64_t hash_board(const board public_board) noexcept
        {
            cfr::solver::compatibility_hasher hash;
            hash.add_u64(static_cast<uint64_t>(public_board.mask));
            return hash.value;
        }

        [[nodiscard]] inline uint64_t hash_betting_policy(const cfr::betting_abstraction_policy& policy) noexcept
        {
            cfr::solver::compatibility_hasher hash;
            hash.add_u64(policy.fixed_pot_fractions.size());
            for (const auto fraction : policy.fixed_pot_fractions) {
                hash.add_u64(std::bit_cast<uint64_t>(static_cast<double>(fraction)));
            }
            hash.add_u64(std::bit_cast<uint64_t>(static_cast<double>(policy.all_in_threshold)));
            hash.add_u64(policy.max_raises);
            hash.add_u64(std::bit_cast<uint64_t>(static_cast<double>(policy.min_bet_increment)));
            for (const auto& raises : policy.max_raises_by_street) {
                hash.add_u64(raises.has_value() ? 1u : 0u);
                hash.add_u64(raises.value_or(0u));
            }
            for (const auto& threshold : policy.all_in_threshold_by_street) {
                hash.add_u64(threshold.has_value() ? 1u : 0u);
                hash.add_u64(std::bit_cast<uint64_t>(static_cast<double>(threshold.value_or(0.0))));
            }
            for (const auto& street_sizes : policy.street_actor_sizes) {
                hash.add_u64(street_sizes.size());
                for (const auto& actor_sizes : street_sizes) {
                    hash.add_u64(actor_sizes.fractions.size());
                    for (const auto fraction : actor_sizes.fractions) {
                        hash.add_u64(std::bit_cast<uint64_t>(static_cast<double>(fraction)));
                    }
                    hash.add_u64(actor_sizes.raise_multiples.size());
                    for (const auto multiple : actor_sizes.raise_multiples) {
                        hash.add_u64(std::bit_cast<uint64_t>(static_cast<double>(multiple)));
                    }
                }
            }
            return hash.value;
        }

        template <std::size_t N>
        [[nodiscard]] inline uint64_t hash_spot_configuration(
            const struct solve_spot& spot,
            const cfr::holdem_betting_graph_config<N>& config) noexcept
        {
            cfr::solver::compatibility_hasher hash;
            hash.add_u64(hash_betting_policy(config.abstraction));
            hash.add_u64(std::bit_cast<uint64_t>(static_cast<double>(spot.gross_pot)));
            hash.add_u64(std::bit_cast<uint64_t>(static_cast<double>(spot.rake)));
            for (const auto stack : config.initial_stacks) {
                hash.add_u64(std::bit_cast<uint64_t>(static_cast<double>(stack)));
            }
            for (const auto committed : config.initial_committed) {
                hash.add_u64(std::bit_cast<uint64_t>(static_cast<double>(committed)));
            }
            hash.add_u64(config.root_actor);
            hash.add_u64(static_cast<uint64_t>(config.max_history));
            hash.add_enum(config.street);
            hash.add_u64(config.public_state_id);
            return hash.value;
        }

        [[nodiscard]] inline uint64_t hash_solver_configuration(
            const struct solve_spot& spot,
            const solve_runtime_options& runtime,
            const uint64_t requested_iterations) noexcept
        {
            cfr::solver::compatibility_hasher hash;
            hash_add_string(hash, "cfr+");
            hash.add_u64(requested_iterations);
            hash.add_u64(runtime.enable_card_isomorphism ? 1u : 0u);
            hash.add_u64(runtime.allow_lossy_card_isomorphism ? 1u : 0u);
            hash.add_u64(runtime.pruning.enabled ? 1u : 0u);
            hash.add_u64(std::bit_cast<uint64_t>(runtime.pruning.prune_threshold));
            hash.add_u64(runtime.pruning.minimum_active_actions);
            hash.add_u64(runtime.pruning.reconsider_interval);
            return hash.value;
        }

        template <std::size_t N>
        [[nodiscard]] inline solve_hashes make_solve_hashes(
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const struct solve_spot& spot,
            const cfr::holdem_betting_graph_config<N>& config,
            const std::array<reach_vector, N>& reach_vectors,
            const board public_board,
            const solve_runtime_options& runtime,
            const uint64_t requested_iterations)
        {
            solve_hashes hashes{};
            const auto view = cfr::solver::make_solver_graph_view<N>(graph, annotations);
            hashes.tree_hash = cfr::solver::hash_solver_graph_metadata(view);
            hashes.range_hash = hash_reach_vectors(std::span<const reach_vector>{reach_vectors.data(), N});
            hashes.board_hash = hash_board(public_board);
            hashes.betting_policy_hash = hash_spot_configuration(spot, config);
            hashes.solver_config_hash = hash_solver_configuration(spot, runtime, requested_iterations);
            hashes.locks_hash = hash_node_locks(spot.locks);

            cfr::solver::compatibility_hasher combined;
            combined.add_u64(hashes.tree_hash);
            combined.add_u64(hashes.range_hash);
            combined.add_u64(hashes.board_hash);
            combined.add_u64(hashes.betting_policy_hash);
            combined.add_u64(hashes.solver_config_hash);
            // Only when present, so a solve without locks keeps its earlier hash.
            if (!spot.locks.empty()) {
                combined.add_u64(hashes.locks_hash);
            }
            hashes.solve_hash = combined.value;
            return hashes;
        }

        [[nodiscard]] inline std::vector<std::string> build_solve_warnings(
            const solve_runtime_options& runtime,
            const bool exploitability_available,
            const std::size_t lock_count = 0)
        {
            std::vector<std::string> warnings;
            warnings.push_back(
                "Strategy is an equilibrium only within the configured betting abstraction "
                "(bet sizings and max raises); it is not the full no-limit game.");
            if (lock_count > 0) {
                warnings.push_back(
                    std::to_string(lock_count) + (lock_count == 1 ? " node lock" : " node locks")
                    + " fixed part of the strategy. The free play is a best response within the locked game, "
                      "and exploitability is measured in that game, not the unlocked one.");
            }
            if (!exploitability_available) {
                warnings.push_back(
                    "Exact best-response exploitability is only computed heads-up; "
                    "reporting a normalized average-regret metric instead.");
            }
            if (runtime.enable_card_isomorphism && runtime.allow_lossy_card_isomorphism) {
                warnings.push_back(
                    "Card isomorphism was applied to ranges that may not be suit-symmetric; "
                    "the solution is approximate.");
            }
            if (runtime.pruning.is_active()) {
                warnings.push_back(
                    "Dynamic action pruning is enabled; the solution is approximate within "
                    "the configured strategy/EV tolerance.");
            }
            return warnings;
        }

        // Result of a single heads-up exploitability measurement. @ref exploitability is
        // half of NashConv (the mean of the two seats' best-response gaps), expressed in
        // the same EV (chip) units the per-hand EV uses.
        struct hu_exploitability {
            double exploitability = 0.0;
            double nash_conv = 0.0;
            std::array<double, 2> seat_gap{};
        };

        // Root range EV of `seat` in chips: the sum of weight times counterfactual value
        // over the sum of weight times opponent reach mass, so exploitability is reported
        // in the same units as the per-hand and range EVs in the artifact.
        [[nodiscard]] inline double root_range_ev(
            const reach_vector& root_values,
            const reach_vector& own_reach,
            const std::vector<combination_index>& active,
            const combo_mass_array& opponent_masses) noexcept
        {
            double weighted_value = 0.0;
            double matchup_mass = 0.0;
            for (const auto combo : active) {
                const auto weight = static_cast<double>(std::max(0.0f, own_reach[combo]));
                weighted_value += weight * static_cast<double>(root_values[combo]);
                matchup_mass += weight * opponent_masses[combo];
            }
            return ev_from_counterfactual_value(weighted_value, matchup_mass);
        }

        // Heads-up exploitability of the (average) strategy. For each seat, the best
        // response value (max over the seat's own actions) minus the strategy value is
        // that seat's gap; both gaps are non-negative because a max dominates the
        // strategy-weighted average at every decision node. `evaluate(seat, best_response,
        // values)` fills the seat's counterfactual values at every node.
        template <typename Evaluate>
        [[nodiscard]] inline hu_exploitability measure_heads_up_exploitability(
            const uint32_t root_node,
            const card_mask root_board,
            const std::array<reach_vector, 2>& reach_vectors,
            const std::array<std::vector<combination_index>, 2>& active_combos,
            std::vector<reach_vector>& value_scratch,
            Evaluate&& evaluate)
        {
            hu_exploitability out{};
            for (uint8_t seat = 0; seat < 2; ++seat) {
                const auto masses = opponent_reach_masses<2>(reach_vectors, active_combos, root_board, seat);
                evaluate(seat, false, value_scratch);
                const auto profile_value = root_range_ev(value_scratch[root_node], reach_vectors[seat], active_combos[seat], masses);
                evaluate(seat, true, value_scratch);
                const auto best_response_value = root_range_ev(value_scratch[root_node], reach_vectors[seat], active_combos[seat], masses);
                const auto gap = std::max(0.0, best_response_value - profile_value);
                out.seat_gap[seat] = gap;
                out.nash_conv += gap;
            }
            out.exploitability = out.nash_conv / 2.0;
            return out;
        }

        // Exploitability over the single-street river graph (heads-up only).
        [[nodiscard]] inline hu_exploitability measure_river_exploitability(
            const cfr::holdem_betting_graph<2>& lowered,
            const combo_action_table& average_strategy,
            const std::array<reach_vector, 2>& reach_vectors,
            const std::array<std::vector<combination_index>, 2>& active_combos,
            const river_terminal_cache& cache,
            const std::array<river_reach_index, 2>& base_indices,
            const std::vector<uint32_t>& terminal_nodes,
            std::vector<std::array<reach_vector, 2>>& node_reach_scratch,
            std::vector<reach_vector>& value_scratch,
            cfr::scheduler::worker_pool& pool,
            const lock_runtime* locks = nullptr)
        {
            build_node_reach_vectors(
                lowered.graph, lowered.annotations, average_strategy, reach_vectors, active_combos, node_reach_scratch);
            return measure_heads_up_exploitability(
                lowered.graph.root_node, cache.river_board.mask, reach_vectors, active_combos, value_scratch,
                [&](const uint8_t seat, const bool best_response, std::vector<reach_vector>& values) {
                    evaluate_combo_profile(
                        lowered, average_strategy, active_combos, node_reach_scratch, cache, base_indices,
                        terminal_nodes, seat, values, pool, best_response, locks);
                });
        }

        // Exploitability over the exact multi-street public game (heads-up only).
        // Pruning is intentionally ignored so the best response can exploit actions the
        // pruned solver stopped traversing.
        [[nodiscard]] inline hu_exploitability measure_multi_street_exploitability(
            const cfr::holdem_public_game_graph<2>& lowered,
            const combo_action_table& average_strategy,
            const std::array<reach_vector, 2>& reach_vectors,
            const std::array<std::vector<combination_index>, 2>& active_combos,
            const cfr::runout_terminal_table& terminal_table,
            const std::vector<std::array<river_reach_index, 2>>& river_base_index_by_state,
            const std::vector<uint32_t>& terminal_nodes,
            std::vector<std::array<reach_vector, 2>>& node_reach_scratch,
            std::vector<reach_vector>& value_scratch,
            cfr::scheduler::worker_pool& pool,
            const lock_runtime* locks = nullptr)
        {
            build_node_reach_vectors(
                lowered.graph, lowered.annotations, average_strategy, reach_vectors, active_combos, node_reach_scratch);
            const auto root_state = lowered.annotations.state_by_node[lowered.graph.root_node].public_state_id;
            return measure_heads_up_exploitability(
                lowered.graph.root_node, lowered.public_states.states[root_state].board_cards, reach_vectors,
                active_combos, value_scratch,
                [&](const uint8_t seat, const bool best_response, std::vector<reach_vector>& values) {
                    evaluate_multi_street_combo_profile(
                        lowered, average_strategy, active_combos, node_reach_scratch, terminal_table,
                        river_base_index_by_state, terminal_nodes, seat, values, pool, nullptr, best_response, locks);
                });
        }

        // Mean over player infosets of the maximum positive regret, divided by the
        // number of iterations. This is a cheap, always-available convergence proxy that
        // tends to zero as CFR+ converges, used directly for multiway spots and as a
        // secondary signal heads-up.
        template <std::size_t N>
        [[nodiscard]] inline double normalized_regret_from_combo_table(
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const combo_action_table& regrets,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const uint64_t iterations)
        {
            if (iterations == 0) {
                return 0.0;
            }
            std::vector<uint8_t> seen_infosets(graph.infoset_count, 0u);
            double sum = 0.0;
            uint64_t count = 0;
            for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
                if (!graph.is_player_node(node_id)) {
                    continue;
                }
                const auto infoset_id = graph.infoset_id[node_id];
                if (seen_infosets[infoset_id] != 0u) {
                    continue;
                }
                seen_infosets[infoset_id] = 1u;
                const auto actor = annotations.actor_by_node[node_id];
                for (const auto combo : active_combos[actor]) {
                    const auto span = regrets.combo_infoset(combo, infoset_id);
                    float best = 0.0f;
                    for (const auto value : span) {
                        best = std::max(best, value);
                    }
                    sum += static_cast<double>(best);
                    ++count;
                }
            }
            return count == 0 ? 0.0 : sum / (static_cast<double>(count) * static_cast<double>(iterations));
        }

        // Resolve the iteration cadence at which exploitability is measured. A positive
        // measurement_interval samples the convergence curve on that cadence; a positive
        // target with no explicit interval still needs a cadence to detect the stop
        // condition, defaulting to a coarse fraction of the run.
        [[nodiscard]] inline uint64_t resolve_measurement_interval(
            const convergence_options& options,
            const uint64_t iterations) noexcept
        {
            if (options.measurement_interval > 0) {
                return options.measurement_interval;
            }
            if (options.target_exploitability > 0.0) {
                return std::max<uint64_t>(1, iterations / 64);
            }
            return 0;
        }

        [[nodiscard]] inline std::vector<action_strategy> aggregate_root_strategy_for_actor(
            const combo_action_table& average_strategy,
            const uint32_t infoset_id,
            const std::vector<std::string>& root_labels,
            const reach_vector& actor_reach)
        {
            std::vector<action_strategy> root_strategy;
            root_strategy.reserve(root_labels.size());
            double total_weight = 0.0;
            for (combination_index combo = 0; combo < combination_count; ++combo) {
                total_weight += std::max(0.0f, actor_reach[combo]);
            }
            const auto uniform = root_labels.empty() ? 0.0 : 1.0 / static_cast<double>(root_labels.size());
            for (std::size_t action_index = 0; action_index < root_labels.size(); ++action_index) {
                double weighted_total = 0.0;
                for (combination_index combo = 0; combo < combination_count; ++combo) {
                    const auto weight = std::max(0.0f, actor_reach[combo]);
                    if (weight <= 0.0f) {
                        continue;
                    }
                    weighted_total += static_cast<double>(weight)
                        * static_cast<double>(average_strategy.value(combo, infoset_id, static_cast<uint32_t>(action_index)));
                }
                root_strategy.push_back(action_strategy{
                    .action = root_labels[action_index],
                    .frequency = total_weight > 0.0 ? weighted_total / total_weight : uniform
                });
            }
            return root_strategy;
        }

        [[nodiscard]] inline hand_strategy make_hand_strategy_row(
            const combination_index combo,
            std::vector<action_strategy> strategy,
            const double range_weight,
            const double reach_probability,
            const double counterfactual_value,
            const double opponent_mass)
        {
            const bool defined = opponent_mass > minimum_ev_reach_mass;
            return hand_strategy{
                .combination_index = combo,
                .hand = hand_text_from_combo(combo),
                .strategy = std::move(strategy),
                .range_weight = range_weight,
                .reach_probability = reach_probability,
                .ev = defined ? counterfactual_value / opponent_mass : 0.0,
                .counterfactual_value = defined ? counterfactual_value : 0.0,
                .opponent_reach_mass = defined ? opponent_mass : 0.0
            };
        }

        template <std::size_t N>
        [[nodiscard]] inline std::vector<solved_node_seat_value> compute_node_seat_values(
            const uint32_t node_id,
            const card_mask board_mask,
            const std::array<reach_vector, N>& base_reach,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const std::vector<std::array<reach_vector, N>>& node_reach,
            const std::array<std::vector<reach_vector>, N>& node_values_by_seat)
        {
            std::vector<solved_node_seat_value> out;
            out.reserve(N);
            for (uint8_t seat = 0; seat < N; ++seat) {
                const auto masses = opponent_reach_masses<N>(node_reach[node_id], active_combos, board_mask, seat);
                double range_reach_mass = 0.0;
                double matchup_reach_mass = 0.0;
                double reach_weighted_value = 0.0;
                double counterfactual_value = 0.0;
                for (const auto combo : active_combos[seat]) {
                    const auto base_weight = std::max(0.0f, base_reach[seat][combo]);
                    const auto path_weight = std::max(0.0f, node_reach[node_id][seat][combo]);
                    const auto profile_value = static_cast<double>(node_values_by_seat[seat][node_id][combo]);
                    range_reach_mass += static_cast<double>(path_weight);
                    matchup_reach_mass += static_cast<double>(path_weight) * masses[combo];
                    reach_weighted_value += static_cast<double>(path_weight) * profile_value;
                    counterfactual_value += static_cast<double>(base_weight) * profile_value;
                }
                out.push_back(solved_node_seat_value{
                    .seat = seat,
                    .range_reach_mass = range_reach_mass,
                    .matchup_reach_mass = matchup_reach_mass,
                    .reach_weighted_value = reach_weighted_value,
                    .conditional_range_ev = ev_from_counterfactual_value(reach_weighted_value, matchup_reach_mass),
                    .counterfactual_value = counterfactual_value
                });
            }
            return out;
        }

        template <std::size_t N>
        [[nodiscard]] inline std::vector<hand_strategy> compute_node_strategy_rows(
            const uint32_t node_id,
            const card_mask board_mask,
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const combo_action_table& average_strategy,
            const std::array<reach_vector, N>& base_reach,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const std::vector<std::array<reach_vector, N>>& node_reach,
            const std::array<std::vector<reach_vector>, N>& node_values_by_seat,
            const uint8_t hero_seat,
            const std::vector<std::string>& action_labels,
            const lock_runtime* locks)
        {
            const auto is_player = graph.is_player_node(node_id);
            const auto seat = is_player ? annotations.actor_by_node[node_id] : hero_seat;
            std::vector<hand_strategy> rows;
            rows.reserve(active_combos[seat].size());
            const auto infoset_id = is_player ? graph.infoset_id[node_id] : cfr::game_graph::INVALID_INFOSET;
            const auto action_count = is_player ? average_strategy.action_count(infoset_id) : 0u;
            const auto masses = opponent_reach_masses<N>(node_reach[node_id], active_combos, board_mask, seat);

            for (const auto combo : active_combos[seat]) {
                std::vector<action_strategy> combo_strategy;
                combo_strategy.reserve(action_count);
                if (is_player) {
                    for (uint32_t action_index = 0; action_index < action_count; ++action_index) {
                        combo_strategy.push_back(action_strategy{
                            .action = action_index < action_labels.size()
                                ? action_labels[action_index]
                                : std::to_string(action_index),
                            .frequency = average_strategy.value(combo, infoset_id, action_index)
                        });
                    }
                }
                const auto range_weight = std::max(0.0f, base_reach[seat][combo]);
                const auto path_weight = std::max(0.0f, node_reach[node_id][seat][combo]);
                auto row = make_hand_strategy_row(
                    combo,
                    std::move(combo_strategy),
                    range_weight,
                    range_weight > 0.0 ? path_weight / range_weight : 0.0,
                    node_values_by_seat[seat][node_id][combo],
                    masses[combo]);
                row.locked = is_player && locks != nullptr && locks->find(infoset_id, combo) != nullptr;
                rows.push_back(std::move(row));
            }
            return rows;
        }

        [[nodiscard]] inline std::vector<category_summary_item> compute_category_summaries(
            const std::vector<hand_strategy>& strategy_rows)
        {
            struct category_accumulator {
                std::string category_name;
                double mass = 0.0;
                double matchup_mass = 0.0;
                double weighted_counterfactual_value = 0.0;
                std::vector<double> weighted_action_mass;
            };

            std::vector<category_accumulator> accumulators;
            double total_mass = 0.0;
            for (const auto& row : strategy_rows) {
                const auto row_mass = row.range_weight * row.reach_probability;
                if (row_mass <= 0.0) {
                    continue;
                }
                total_mass += row_mass;
                const auto category = hand_class_text_from_combo(row.combination_index);
                auto it = std::find_if(accumulators.begin(), accumulators.end(), [&](const category_accumulator& candidate) {
                    return candidate.category_name == category;
                });
                if (it == accumulators.end()) {
                    accumulators.push_back(category_accumulator{
                        .category_name = category,
                        .mass = 0.0,
                        .matchup_mass = 0.0,
                        .weighted_counterfactual_value = 0.0,
                        .weighted_action_mass = std::vector<double>(row.strategy.size(), 0.0)
                    });
                    it = std::prev(accumulators.end());
                } else if (it->weighted_action_mass.size() < row.strategy.size()) {
                    it->weighted_action_mass.resize(row.strategy.size(), 0.0);
                }
                it->mass += row_mass;
                it->matchup_mass += row_mass * row.opponent_reach_mass;
                it->weighted_counterfactual_value += row_mass * row.counterfactual_value;
                for (std::size_t action_index = 0; action_index < row.strategy.size(); ++action_index) {
                    it->weighted_action_mass[action_index] += row_mass * row.strategy[action_index].frequency;
                }
            }

            std::vector<category_summary_item> summaries;
            summaries.reserve(accumulators.size());
            for (const auto& accumulator : accumulators) {
                std::vector<category_summary_action_frequency> action_frequencies;
                action_frequencies.reserve(accumulator.weighted_action_mass.size());
                for (std::size_t action_index = 0; action_index < accumulator.weighted_action_mass.size(); ++action_index) {
                    action_frequencies.push_back(category_summary_action_frequency{
                        .action_index = static_cast<uint16_t>(action_index),
                        .frequency = accumulator.mass > 0.0
                            ? accumulator.weighted_action_mass[action_index] / accumulator.mass
                            : 0.0
                    });
                }
                summaries.push_back(category_summary_item{
                    .category_name = accumulator.category_name,
                    .frequency = total_mass > 0.0 ? accumulator.mass / total_mass : 0.0,
                    .range_weight = accumulator.mass,
                    .average_ev = ev_from_counterfactual_value(accumulator.weighted_counterfactual_value, accumulator.matchup_mass),
                    .action_frequencies = std::move(action_frequencies)
                });
            }
            return summaries;
        }

        template <std::size_t N>
        inline void populate_solved_node_profiles(
            solve_artifact& artifact,
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const combo_action_table& average_strategy,
            const std::array<reach_vector, N>& base_reach,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const std::vector<std::array<reach_vector, N>>& node_reach,
            const std::array<std::vector<reach_vector>, N>& node_values_by_seat,
            const std::vector<card_mask>& board_by_node,
            const std::vector<std::vector<std::string>>& labels,
            const lock_runtime* locks)
        {
            const auto hero_seat = static_cast<uint8_t>(std::min<std::size_t>(artifact.hero_seat, N - 1u));
            for (auto& node : artifact.solved_nodes) {
                if (locks != nullptr) {
                    node.lock_indices = locks->lock_indices_by_node[node.node_id];
                }
                const auto board_mask = board_by_node[node.node_id];
                node.seat_values = compute_node_seat_values(
                    node.node_id,
                    board_mask,
                    base_reach,
                    active_combos,
                    node_reach,
                    node_values_by_seat);
                node.strategy_rows = compute_node_strategy_rows(
                    node.node_id,
                    board_mask,
                    graph,
                    annotations,
                    average_strategy,
                    base_reach,
                    active_combos,
                    node_reach,
                    node_values_by_seat,
                    hero_seat,
                    labels[node.node_id],
                    locks);
                node.category_summary_derivation_version = 1;
                node.category_summaries = compute_category_summaries(node.strategy_rows);
            }
        }

        inline void append_solved_graph_payload(
            solve_artifact& artifact,
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const cfr::public_state_registry* public_states,
            const cfr::chance_event_table* chance_events,
            const cfr::runout_registry* runouts,
            const std::vector<std::vector<action_strategy>>& node_strategies)
        {
            if (public_states != nullptr) {
                artifact.public_states.reserve(public_states->states.size());
                for (const auto& state : public_states->states) {
                    artifact.public_states.push_back(solve_artifact_public_state{
                        .id = state.id,
                        .street = street_text(state.street),
                        .board = card_texts_from_mask(state.board_cards),
                        .parent_state_id = state.parent_state_id,
                        .chance_event_id_from_parent = state.chance_event_id_from_parent,
                        .chance_outcome_id_from_parent = state.chance_outcome_id_from_parent,
                        .is_root_state = state.is_root_state,
                        .is_terminal_river_state = state.is_terminal_river_state
                    });
                }
            }

            if (chance_events != nullptr) {
                artifact.chance_events.reserve(chance_events->events.size());
                for (uint32_t event_id = 0; event_id < chance_events->events.size(); ++event_id) {
                    const auto& event = chance_events->events[event_id];
                    solve_artifact_chance_event persisted{
                        .id = event_id,
                        .node_id = event.node_id,
                        .kind = chance_kind_text(event.kind),
                        .board = card_texts_from_mask(event.board_cards)
                    };
                    for (const auto& outcome : chance_events->event_outcomes(event)) {
                        persisted.outcomes.push_back(solved_node_action{
                            .action = "deal",
                            .child_node_id = outcome.child_node,
                            .action_index = outcome.action_index,
                            .probability = outcome.probability,
                            .chance_event_id = event_id,
                            .board_partition_id = outcome.board_partition_id,
                            .chance_outcome_id = outcome.outcome_id,
                            .dealt_cards = card_texts_from_mask(outcome.cards)
                        });
                    }
                    artifact.chance_events.push_back(std::move(persisted));
                }
            }

            if (runouts != nullptr) {
                artifact.runouts.reserve(runouts->runouts.size());
                for (const auto& runout : runouts->runouts) {
                    artifact.runouts.push_back(solve_artifact_runout{
                        .id = runout.id,
                        .root_public_state_id = runout.root_public_state_id,
                        .river_public_state_id = runout.river_public_state_id,
                        .dealt_turn = card_texts_from_mask(runout.dealt_turn),
                        .dealt_river = card_texts_from_mask(runout.dealt_river)
                    });
                }
            }

            artifact.solved_nodes.reserve(graph.node_count);
            for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
                solved_node persisted{};
                persisted.node_id = node_id;
                persisted.parent_node_id = cfr::game_graph::INVALID_NODE;
                persisted.kind = graph.is_terminal(node_id)
                    ? "terminal"
                    : (graph.node_types[node_id] == cfr::node_kind::chance ? "chance" : "player");
                persisted.public_state_id = annotations.state_by_node[node_id].public_state_id;
                persisted.acting_seat = annotations.actor_by_node[node_id];
                persisted.terminal = graph.is_terminal(node_id);
                if (public_states != nullptr && persisted.public_state_id < public_states->states.size()) {
                    persisted.board = card_texts_from_mask(public_states->states[persisted.public_state_id].board_cards);
                }
                for (uint32_t parent = 0; parent < graph.node_count && persisted.parent_node_id == cfr::game_graph::INVALID_NODE; ++parent) {
                    for (const auto& edge : graph.out_edges(parent)) {
                        if (edge.child_node == node_id) {
                            persisted.parent_node_id = parent;
                            break;
                        }
                    }
                }

                if (graph.is_player_node(node_id)) {
                    const auto edges = graph.out_edges(node_id);
                    const auto* strategy = node_id < node_strategies.size() ? &node_strategies[node_id] : nullptr;
                    for (uint32_t action_index = 0; action_index < edges.size(); ++action_index) {
                        persisted.actions.push_back(solved_node_action{
                            .action = (strategy != nullptr && action_index < strategy->size())
                                ? (*strategy)[action_index].action
                                : std::to_string(action_index),
                            .child_node_id = edges[action_index].child_node,
                            .action_index = static_cast<uint16_t>(action_index)
                        });
                    }
                    if (strategy != nullptr) {
                        persisted.range_action_frequencies = *strategy;
                    }
                } else if (graph.node_types[node_id] == cfr::node_kind::chance && chance_events != nullptr) {
                    if (const auto* event = chance_events->event_for_node(node_id); event != nullptr) {
                        for (const auto& outcome : chance_events->event_outcomes(*event)) {
                            persisted.actions.push_back(solved_node_action{
                                .action = "deal",
                                .child_node_id = outcome.child_node,
                                .action_index = outcome.action_index,
                                .probability = outcome.probability,
                                .chance_event_id = annotations.chance_event_id_by_node[node_id],
                                .board_partition_id = outcome.board_partition_id,
                                .chance_outcome_id = outcome.outcome_id,
                                .dealt_cards = card_texts_from_mask(outcome.cards)
                            });
                        }
                    }
                }

                artifact.solved_nodes.push_back(std::move(persisted));
            }
            artifact.root_node_id = graph.root_node;
        }

        // Fraction of detected available memory the solver may plan to use when no
        // explicit budget is configured. Leaves headroom for the OS and other work.
        inline constexpr double default_memory_budget_fraction = 0.8;
        inline constexpr uint64_t fallback_memory_budget_bytes = uint64_t{4} << 30; // 4 GiB

        /** Worker threads for terminal evaluation: the runtime setting, clamped to [1, 64]. */
        [[nodiscard]] inline uint32_t resolve_worker_count(const solve_runtime_options& runtime) noexcept
        {
            return std::clamp<uint32_t>(runtime.worker_threads, 1u, 64u);
        }

        [[nodiscard]] inline uint64_t resolve_memory_budget_bytes(const solve_runtime_options& runtime) noexcept
        {
            if (runtime.memory_budget_bytes != 0u) {
                return runtime.memory_budget_bytes;
            }
            const auto available = detected_available_memory_bytes();
            if (available == 0u) {
                return fallback_memory_budget_bytes;
            }
            return static_cast<uint64_t>(static_cast<double>(available) * default_memory_budget_fraction);
        }

        // Concrete, actionable remedy for the dimension that dominates an over-budget
        // multi-street footprint. Named so the CLI error tells the user what to change.
        [[nodiscard]] inline std::string memory_reduction_hint(
            const cfr::multi_street_memory_component dominant)
        {
            switch (dominant) {
                case cfr::multi_street_memory_component::regret:
                case cfr::multi_street_memory_component::strategy_sum:
                case cfr::multi_street_memory_component::working:
                    return "reduce the number of bet sizes or lower max_raises per street (fewer actions per infoset), or narrow the ranges";
                case cfr::multi_street_memory_component::reach:
                case cfr::multi_street_memory_component::value:
                case cfr::multi_street_memory_component::graph:
                case cfr::multi_street_memory_component::infoset:
                    return "lower max_raises per street, use fewer bet sizes, enable card isomorphism, or restrict the solve to the turn";
                case cfr::multi_street_memory_component::terminal_cache:
                    return "enable card isomorphism or restrict the solve to the turn (fewer distinct river boards)";
                case cfr::multi_street_memory_component::chance:
                    return "enable card isomorphism to collapse suit-isomorphic board runouts";
                case cfr::multi_street_memory_component::worker:
                case cfr::multi_street_memory_component::extraction:
                    return "reduce the number of bet sizes or lower max_raises per street";
            }
            return "reduce the abstraction size";
        }

        [[nodiscard]] inline cli_error over_budget_error(
            const cfr::multi_street_memory_estimate& estimate,
            const uint64_t budget_bytes)
        {
            const auto to_mib = [](const uint64_t bytes) {
                return static_cast<double>(bytes) / (1024.0 * 1024.0);
            };
            std::ostringstream message;
            message.setf(std::ios::fixed);
            message.precision(1);
            message << "Estimated solver memory " << to_mib(estimate.total_bytes)
                << " MiB exceeds the budget of " << to_mib(budget_bytes)
                << " MiB. The dominant cost is the '" << cfr::to_string(estimate.dominant)
                << "' dimension. To fit within budget, "
                << memory_reduction_hint(estimate.dominant) << ".";
            return cli_error{cli_error_kind::solver, message.str()};
        }

        // Verify the ranges are invariant under exactly the suit permutations the
        // isomorphism reduction uses to collapse outcomes (the permutation-scoped
        // symmetry the lossless guarantee requires). For every chance event board, the
        // canonicalizer's member permutations are replayed and each active combo's
        // weight is checked against its induced private-combo image in both ranges.
        template <std::size_t N>
        [[nodiscard]] inline bool ranges_are_suit_symmetric_for_isomorphism(
            const cfr::holdem_public_game_graph<N>& public_game,
            const std::array<reach_vector, N>& reach_vectors,
            const cfr::public_card_isomorphism_mode mode)
        {
            if (mode != cfr::public_card_isomorphism_mode::suit_isomorphic) {
                return true;
            }
            constexpr float tolerance = 1e-4f;
            for (const auto& event : public_game.chance_events.events) {
                auto raw = cfr::enumerate_public_card_outcomes(event.board_cards, event.dead_cards, 1);
                const auto canonical = cfr::canonicalize_public_card_outcomes(
                    raw, event.board_cards, event.dead_cards, mode);
                for (const auto& outcome : canonical) {
                    for (const auto& member : outcome.members) {
                        if (cfr::is_identity_suit_permutation(member.permutation)) {
                            continue;
                        }
                        for (combination_index combo = 0; combo < combination_count; ++combo) {
                            const auto image = cfr::apply_suit_permutation_to_combo(combo, member.permutation);
                            if (image >= combination_count) {
                                return false;
                            }
                            for (std::size_t seat = 0; seat < N; ++seat) {
                                if (std::fabs(reach_vectors[seat][combo] - reach_vectors[seat][image]) > tolerance) {
                                    return false;
                                }
                            }
                        }
                    }
                }
            }
            return true;
        }

        // Runs the unified heads-up CFR+ solve over the exact multi-street public game
        // (flop/turn). CFR+ traverses the interleaved betting/chance tree across every
        // street; terminal leaves dispatch to the per-river terminal cache (showdown) or a
        // blocker-aware fold evaluation (any street), keeping the expensive hand evaluation
        // outside the regret-matching hot loop. Heads-up only; higher seat counts are
        // rejected because the exact terminal kernels are defined for two players.
        // Root summary of a solve. `root_strategy` is the root actor's range-level
        // action mix. Hero rows carry the hero's root EV and, when the hero acts at the
        // root, its per-hand action frequencies; otherwise `strategy` stays empty
        // because the hero has no decision there (per-node rows cover later decisions).
        template <std::size_t N>
        inline void append_root_summary(
            solve_artifact& artifact,
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const combo_action_table& average_strategy,
            const std::vector<std::string>& root_labels,
            const std::array<reach_vector, N>& reach_vectors,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const reach_vector& hero_root_values,
            const card_mask root_board,
            const uint8_t hero,
            const lock_runtime* locks)
        {
            const auto root_infoset = graph.infoset_id[graph.root_node];
            const auto root_actor = annotations.actor_by_node[graph.root_node];
            artifact.root_strategy = aggregate_root_strategy_for_actor(
                average_strategy, root_infoset, root_labels, reach_vectors[root_actor]);

            const auto masses = opponent_reach_masses<N>(reach_vectors, active_combos, root_board, hero);
            for (const auto combo : active_combos[hero]) {
                std::vector<action_strategy> combo_strategy;
                if (hero == root_actor) {
                    combo_strategy.reserve(root_labels.size());
                    for (std::size_t action_index = 0; action_index < root_labels.size(); ++action_index) {
                        combo_strategy.push_back(action_strategy{
                            .action = artifact.root_strategy[action_index].action,
                            .frequency = average_strategy.value(combo, root_infoset, static_cast<uint32_t>(action_index))
                        });
                    }
                }
                auto row = make_hand_strategy_row(
                    combo,
                    std::move(combo_strategy),
                    reach_vectors[hero][combo],
                    reach_vectors[hero][combo] > 0.0f ? 1.0 : 0.0,
                    hero_root_values[combo],
                    masses[combo]);
                row.locked = hero == root_actor && locks != nullptr && locks->find(root_infoset, combo) != nullptr;
                artifact.strategy.push_back(std::move(row));
            }
        }

        // Range-level action mix at every player node, weighted by the acting seat's
        // reach, with each action under its label.
        template <std::size_t N>
        [[nodiscard]] inline std::vector<std::vector<action_strategy>> make_node_strategies(
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const combo_action_table& average_strategy,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const std::vector<std::array<reach_vector, N>>& node_reach,
            const std::vector<std::vector<std::string>>& labels)
        {
            std::vector<std::vector<action_strategy>> node_strategies(graph.node_count);
            for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
                if (!graph.is_player_node(node_id)) {
                    continue;
                }
                const auto actor = annotations.actor_by_node[node_id];
                const auto infoset_id = graph.infoset_id[node_id];
                const auto edges = graph.out_edges(node_id);
                std::vector<double> weighted(edges.size(), 0.0);
                double total = 0.0;
                for (const auto combo : active_combos[actor]) {
                    const auto weight = std::max(0.0f, node_reach[node_id][actor][combo]);
                    if (weight <= 0.0f) {
                        continue;
                    }
                    total += static_cast<double>(weight);
                    for (uint32_t action_index = 0; action_index < edges.size(); ++action_index) {
                        weighted[action_index] += static_cast<double>(weight)
                            * static_cast<double>(average_strategy.value(combo, infoset_id, action_index));
                    }
                }
                auto& node_strategy = node_strategies[node_id];
                node_strategy.reserve(edges.size());
                for (uint32_t action_index = 0; action_index < edges.size(); ++action_index) {
                    node_strategy.push_back(action_strategy{
                        .action = action_index < labels[node_id].size()
                            ? labels[node_id][action_index]
                            : std::to_string(action_index),
                        .frequency = total > 0.0
                            ? weighted[action_index] / total
                            : (edges.empty() ? 0.0 : 1.0 / static_cast<double>(edges.size()))
                    });
                }
            }
            return node_strategies;
        }

        template <std::size_t N>
        [[nodiscard]] inline std::expected<void, cli_error> solve_multi_street_public_game(
            const struct solve_spot& spot,
            const uint64_t iterations,
            const solve_runtime_options& runtime,
            const cfr::holdem_public_game_graph<N>& public_game,
            const cfr::action_table_layout& layout,
            const cfr::holdem_betting_graph_config<N>& config,
            const std::array<reach_vector, N>& reach_vectors,
            solve_output& output)
        {
            if constexpr (N != 2) {
                (void)spot;
                (void)iterations;
                (void)runtime;
                (void)public_game;
                (void)layout;
                (void)config;
                (void)reach_vectors;
                (void)output;
                return std::unexpected(cli_error{
                    cli_error_kind::solver,
                    "Flop and turn solving currently supports heads-up play (2 seats) only."
                });
            } else {
                if (solve_cancel_requested(runtime)) {
                    return std::unexpected(solve_cancelled_error());
                }

                const auto& graph = public_game.graph;
                const auto& annotations = public_game.annotations;

                const auto active_combos = active_combos_by_player(reach_vectors);
                const auto labels = make_edge_labels<N>(graph, config, &public_game.chance_events, spot.gross_pot);
                auto resolved_locks = resolve_node_locks<N>(
                    spot, graph, annotations, labels, active_combos, runtime.enable_card_isomorphism);
                if (!resolved_locks) {
                    return std::unexpected(resolved_locks.error());
                }
                const lock_runtime* locks = resolved_locks->has_value() ? &**resolved_locks : nullptr;

                combo_action_table regrets(layout);
                combo_action_table strategy_sums(layout);
                combo_action_table current_strategy(layout);
                combo_action_table average_strategy(layout);
                const auto terminal_nodes = terminal_node_ids(graph);
                cfr::scheduler::worker_pool pool{resolve_worker_count(runtime)};
                stop_pool_on_cancel(pool, runtime);

                // Materialize one river terminal cache per distinct river board and, for
                // each seat, the range-weighted reach index into that cache. These stay
                // outside the CFR loop so showdown leaves reuse the vectorized river
                // terminal evaluation instead of rebuilding equity every iteration.
                const auto terminal_table =
                    cfr::make_runout_terminal_table(public_game.public_states.states);
                std::vector<std::array<river_reach_index, N>> river_base_index_by_state(
                    public_game.public_states.states.size());
                for (const auto& state : public_game.public_states.states) {
                    if (!state.is_terminal_river_state) {
                        continue;
                    }
                    const auto* entry = terminal_table.find(state.id);
                    if (entry == nullptr) {
                        return std::unexpected(cli_error{
                            cli_error_kind::solver,
                            "River public state is missing its materialized terminal cache."
                        });
                    }
                    for (std::size_t seat = 0; seat < N; ++seat) {
                        river_base_index_by_state[state.id][seat] =
                            make_river_reach_index(entry->cache, reach_vectors[seat]);
                    }
                }

                auto initial_state = cfr::make_initial_betting_state(config);
                const auto root_actions = cfr::legal_betting_actions(initial_state, config.abstraction);
                const auto root_infoset = graph.infoset_id[graph.root_node];
                if (!graph.is_player_node(graph.root_node)
                    || root_actions.empty()
                    || root_actions.size() != layout.action_count(root_infoset)) {
                    return std::unexpected(cli_error{
                        cli_error_kind::solver,
                        "Root action shape does not match strategy table shape."
                    });
                }

                const auto iter_begin = std::chrono::steady_clock::now();
                const auto progress_batch = std::max<uint64_t>(1, runtime.progress_batch_iterations);
                std::vector<std::array<reach_vector, N>> node_reach;
                std::vector<reach_vector> node_values;
                std::optional<pruning_runtime> pruning_state;
                if (runtime.pruning.is_active()) {
                    pruning_state = make_pruning_runtime(graph, layout, runtime.pruning);
                }
                const pruning_runtime* pruning = pruning_state ? &*pruning_state : nullptr;
                const bool measure = runtime.convergence.measure_exploitability;
                const auto measurement_interval = resolve_measurement_interval(runtime.convergence, iterations);
                combo_action_table measure_average(layout);
                std::vector<std::array<reach_vector, N>> measure_reach;
                std::vector<reach_vector> measure_values;
                hu_exploitability final_exploitability{};
                bool have_exploitability = false;
                std::vector<convergence_sample> convergence_curve;
                uint64_t completed_iterations = iterations;
                bool reached_target = false;
                for (uint64_t i = 0; i < iterations; ++i) {
                    if (solve_stopped(runtime, pool)) {
                        return std::unexpected(solve_cancelled_error());
                    }
                    if (pruning_state && i > 0 && (i % pruning_state->reconsider_interval()) == 0) {
                        normalize_combo_action_table(graph, annotations, regrets, current_strategy, active_combos, pruning, locks);
                        build_node_reach_vectors(graph, annotations, current_strategy, reach_vectors, active_combos, node_reach, pruning);
                        reconsider_active_actions(graph, annotations, regrets, active_combos, node_reach, *pruning_state, locks);
                        recompute_node_live(graph, *pruning_state);
                    }
                    for (uint8_t updating_player = 0; updating_player < N; ++updating_player) {
                        if ((i % progress_batch) == 0) {
                            emit_progress(
                                runtime,
                                solve_progress_stage::cfr,
                                i,
                                iterations,
                                updating_player,
                                static_cast<uint8_t>(N),
                                std::chrono::duration<double, std::milli>(
                                    std::chrono::steady_clock::now() - iter_begin).count(),
                                "Multi-street CFR+ player update.");
                        }
                        normalize_combo_action_table(graph, annotations, regrets, current_strategy, active_combos, pruning, locks);
                        build_node_reach_vectors(graph, annotations, current_strategy, reach_vectors, active_combos, node_reach, pruning);
                        evaluate_multi_street_combo_profile(
                            public_game,
                            current_strategy,
                            active_combos,
                            node_reach,
                            terminal_table,
                            river_base_index_by_state,
                            terminal_nodes,
                            updating_player,
                            node_values,
                            pool,
                            pruning);
                        update_combo_cfr_tables(
                            graph,
                            annotations,
                            current_strategy,
                            active_combos,
                            updating_player,
                            node_reach,
                            node_values,
                            regrets,
                            strategy_sums,
                            pruning,
                            locks);
                    }
                    const uint64_t done = i + 1;
                    const bool at_final = done == iterations;
                    const bool at_check = measurement_interval > 0 && (done % measurement_interval == 0);
                    if (measure && (at_check || at_final)) {
                        normalize_combo_action_table(graph, annotations, strategy_sums, measure_average, active_combos, nullptr, locks);
                        final_exploitability = measure_multi_street_exploitability(
                            public_game, measure_average, reach_vectors, active_combos, terminal_table,
                            river_base_index_by_state, terminal_nodes, measure_reach, measure_values, pool, locks);
                        have_exploitability = true;
                        if (at_check && convergence_curve.size() < runtime.convergence.max_curve_samples) {
                            convergence_curve.push_back(convergence_sample{
                                .iteration = done,
                                .metric = final_exploitability.exploitability,
                                .elapsed_ms = std::chrono::duration<double, std::milli>(
                                    std::chrono::steady_clock::now() - iter_begin).count()
                            });
                        }
                        if (runtime.convergence.target_exploitability > 0.0
                            && final_exploitability.exploitability <= runtime.convergence.target_exploitability) {
                            completed_iterations = done;
                            reached_target = true;
                            break;
                        }
                    }
                    if (((i + 1) % progress_batch) == 0 || (i + 1) == iterations) {
                        emit_progress(
                            runtime,
                            solve_progress_stage::cfr,
                            i + 1,
                            iterations,
                            static_cast<uint8_t>(N - 1),
                            static_cast<uint8_t>(N),
                            std::chrono::duration<double, std::milli>(
                                std::chrono::steady_clock::now() - iter_begin).count(),
                            "Multi-street CFR+ iteration batch complete.");
                    }
                }
                output.timing.cfr_iterations_ms = std::chrono::duration<double, std::milli>(
                    std::chrono::steady_clock::now() - iter_begin).count();

                if (solve_stopped(runtime, pool)) {
                    return std::unexpected(solve_cancelled_error());
                }

                const auto extraction_begin = std::chrono::steady_clock::now();
                emit_progress(
                    runtime,
                    solve_progress_stage::extraction,
                    iterations,
                    iterations,
                    0,
                    static_cast<uint8_t>(N),
                    0.0,
                    "Extracting combo strategies.");

                normalize_combo_action_table(graph, annotations, strategy_sums, average_strategy, active_combos, pruning, locks);
                build_node_reach_vectors(graph, annotations, average_strategy, reach_vectors, active_combos, node_reach, pruning);
                const auto hero = static_cast<uint8_t>(spot.hero_seat);
                std::array<std::vector<reach_vector>, N> node_values_by_seat{};
                for (uint8_t seat = 0; seat < N; ++seat) {
                    evaluate_multi_street_combo_profile(
                        public_game,
                        average_strategy,
                        active_combos,
                        node_reach,
                        terminal_table,
                        river_base_index_by_state,
                        terminal_nodes,
                        seat,
                        node_values_by_seat[seat],
                        pool,
                        pruning);
                }
                if (solve_stopped(runtime, pool)) {
                    return std::unexpected(solve_cancelled_error());
                }

                solve_artifact artifact{};
                artifact.players.assign(spot.players.begin(), std::next(spot.players.begin(), N));
                artifact.board = spot.board;
                artifact.street = spot.street;
                artifact.hero_seat = spot.hero_seat;
                artifact.solver.algorithm = "cfr+";
                artifact.solver.iterations = completed_iterations;
                artifact.solver.timestamp = runtime.timestamp_utc.empty() ? now_utc_iso8601() : runtime.timestamp_utc;
                artifact.solver.git_revision = runtime.git_revision;
                {
                    const auto root_state_id = annotations.state_by_node[graph.root_node].public_state_id;
                    const board public_board{
                        .mask = public_game.public_states.states[root_state_id].board_cards};
                    artifact.solver.hashes = make_solve_hashes<N>(
                        graph, annotations, spot, config, reach_vectors, public_board, runtime, iterations);
                    artifact.solver.warnings = build_solve_warnings(
                        runtime, /*exploitability_available=*/true, spot.locks.size());
                    artifact.solver.locks = spot.locks;
                    convergence_report report{};
                    report.exploitability_available = have_exploitability;
                    if (have_exploitability) {
                        report.exploitability = final_exploitability.exploitability;
                        report.exploitability_pot_fraction = spot.gross_pot > 0.0
                            ? final_exploitability.exploitability / spot.gross_pot : 0.0;
                        report.nash_conv = final_exploitability.nash_conv;
                        report.best_response_gap.assign(
                            final_exploitability.seat_gap.begin(),
                            std::next(final_exploitability.seat_gap.begin(), N));
                    }
                    report.normalized_regret = normalized_regret_from_combo_table(
                        graph, annotations, regrets, active_combos, completed_iterations);
                    report.target_exploitability = runtime.convergence.target_exploitability;
                    report.reached_target = reached_target;
                    report.curve = std::move(convergence_curve);
                    artifact.solver.convergence = std::move(report);
                }
                const auto board_by_node = multi_street_board_by_node(public_game);
                append_root_summary<N>(
                    artifact,
                    graph,
                    annotations,
                    average_strategy,
                    labels[graph.root_node],
                    reach_vectors,
                    active_combos,
                    node_values_by_seat[hero][graph.root_node],
                    board_by_node[graph.root_node],
                    hero,
                    locks);

                append_solved_graph_payload(
                    artifact,
                    graph,
                    annotations,
                    &public_game.public_states,
                    &public_game.chance_events,
                    &public_game.runouts,
                    make_node_strategies<N>(graph, annotations, average_strategy, active_combos, node_reach, labels));
                populate_solved_node_profiles(
                    artifact,
                    graph,
                    annotations,
                    average_strategy,
                    reach_vectors,
                    active_combos,
                    node_reach,
                    node_values_by_seat,
                    board_by_node,
                    labels,
                    locks);
                output.artifact = std::move(artifact);
                output.timing.extraction_ms = std::chrono::duration<double, std::milli>(
                    std::chrono::steady_clock::now() - extraction_begin).count();
                emit_progress(
                    runtime,
                    solve_progress_stage::extraction,
                    iterations,
                    iterations,
                    0,
                    static_cast<uint8_t>(N),
                    output.timing.extraction_ms,
                    "Combo extraction complete.");

                if (auto validation = validate_artifact(output.artifact); !validation) {
                    return std::unexpected(validation.error());
                }
                return {};
            }
        }

        // Single-street river CFR+ for any player count and hero seat. Exact
        // best-response exploitability is measured heads-up; multiway solves report
        // the normalized average regret instead.
        template <std::size_t N>
        [[nodiscard]] inline std::expected<void, cli_error> solve_river(
            const struct solve_spot& spot,
            const uint64_t iterations,
            const solve_runtime_options& runtime,
            const cfr::holdem_betting_graph<N>& lowered,
            const cfr::action_table_layout& layout,
            const cfr::holdem_betting_graph_config<N>& config,
            const board public_board,
            const std::array<reach_vector, N>& reach_vectors,
            solve_output& output)
        {
            const auto active_combos = active_combos_by_player(reach_vectors);
            const auto labels = make_edge_labels<N>(lowered.graph, config, nullptr, spot.gross_pot);
            auto resolved_locks = resolve_node_locks<N>(
                spot, lowered.graph, lowered.annotations, labels, active_combos, /*card_isomorphism=*/false);
            if (!resolved_locks) {
                return std::unexpected(resolved_locks.error());
            }
            const lock_runtime* locks = resolved_locks->has_value() ? &**resolved_locks : nullptr;

            combo_action_table regrets(layout);
            combo_action_table strategy_sums(layout);
            combo_action_table current_strategy(layout);
            combo_action_table average_strategy(layout);
            const auto terminal_nodes = terminal_node_ids(lowered.graph);
            cfr::scheduler::worker_pool pool{resolve_worker_count(runtime)};
            stop_pool_on_cancel(pool, runtime);
            const auto cache = make_river_terminal_cache(public_board);
            std::array<river_reach_index, N> base_indices{};
            for (std::size_t seat = 0; seat < N; ++seat) {
                base_indices[seat] = make_river_reach_index(cache, reach_vectors[seat]);
            }

            auto initial_state = cfr::make_initial_betting_state(config);
            const auto root_actions = cfr::legal_betting_actions(initial_state, config.abstraction);
            const auto root_infoset = lowered.graph.infoset_id[lowered.graph.root_node];
            if (!lowered.graph.is_player_node(lowered.graph.root_node)
                || root_actions.empty()
                || root_actions.size() != layout.action_count(root_infoset)) {
                return std::unexpected(cli_error{cli_error_kind::solver, "Root action shape does not match strategy table shape."});
            }

            const auto iter_begin = std::chrono::steady_clock::now();
            const auto progress_batch = std::max<uint64_t>(1, runtime.progress_batch_iterations);
            std::vector<std::array<reach_vector, N>> node_reach;
            std::vector<reach_vector> node_values;
            constexpr bool exploitability_supported = N == 2;
            const bool measure = exploitability_supported && runtime.convergence.measure_exploitability;
            const auto measurement_interval = resolve_measurement_interval(runtime.convergence, iterations);
            combo_action_table measure_average(layout);
            std::vector<std::array<reach_vector, N>> measure_reach;
            std::vector<reach_vector> measure_values;
            hu_exploitability final_exploitability{};
            bool have_exploitability = false;
            std::vector<convergence_sample> convergence_curve;
            uint64_t completed_iterations = iterations;
            bool reached_target = false;
            for (uint64_t i = 0; i < iterations; ++i) {
                if (solve_stopped(runtime, pool)) {
                    return std::unexpected(solve_cancelled_error());
                }
                for (uint8_t updating_player = 0; updating_player < N; ++updating_player) {
                    if ((i % progress_batch) == 0) {
                        emit_progress(
                            runtime,
                            solve_progress_stage::cfr,
                            i,
                            iterations,
                            updating_player,
                            static_cast<uint8_t>(N),
                            std::chrono::duration<double, std::milli>(
                                std::chrono::steady_clock::now() - iter_begin).count(),
                            "River CFR+ player update.");
                    }
                    normalize_combo_action_table(lowered.graph, lowered.annotations, regrets, current_strategy, active_combos, nullptr, locks);
                    build_node_reach_vectors(lowered.graph, lowered.annotations, current_strategy, reach_vectors, active_combos, node_reach);
                    evaluate_combo_profile(
                        lowered,
                        current_strategy,
                        active_combos,
                        node_reach,
                        cache,
                        base_indices,
                        terminal_nodes,
                        updating_player,
                        node_values,
                        pool);
                    update_combo_cfr_tables(
                        lowered.graph,
                        lowered.annotations,
                        current_strategy,
                        active_combos,
                        updating_player,
                        node_reach,
                        node_values,
                        regrets,
                        strategy_sums,
                        nullptr,
                        locks);
                }
                const uint64_t done = i + 1;
                const bool at_final = done == iterations;
                const bool at_check = measurement_interval > 0 && (done % measurement_interval == 0);
                if constexpr (exploitability_supported) {
                    if (measure && (at_check || at_final)) {
                        normalize_combo_action_table(
                            lowered.graph, lowered.annotations, strategy_sums, measure_average, active_combos, nullptr, locks);
                        final_exploitability = measure_river_exploitability(
                            lowered, measure_average, reach_vectors, active_combos, cache, base_indices,
                            terminal_nodes, measure_reach, measure_values, pool, locks);
                        have_exploitability = true;
                        if (at_check && convergence_curve.size() < runtime.convergence.max_curve_samples) {
                            convergence_curve.push_back(convergence_sample{
                                .iteration = done,
                                .metric = final_exploitability.exploitability,
                                .elapsed_ms = std::chrono::duration<double, std::milli>(
                                    std::chrono::steady_clock::now() - iter_begin).count()
                            });
                        }
                        if (runtime.convergence.target_exploitability > 0.0
                            && final_exploitability.exploitability <= runtime.convergence.target_exploitability) {
                            completed_iterations = done;
                            reached_target = true;
                            break;
                        }
                    }
                }
                if (((i + 1) % progress_batch) == 0 || (i + 1) == iterations) {
                    emit_progress(
                        runtime,
                        solve_progress_stage::cfr,
                        i + 1,
                        iterations,
                        static_cast<uint8_t>(N - 1),
                        static_cast<uint8_t>(N),
                        std::chrono::duration<double, std::milli>(
                            std::chrono::steady_clock::now() - iter_begin).count(),
                        "River CFR+ iteration batch complete.");
                }
            }
            output.timing.cfr_iterations_ms = std::chrono::duration<double, std::milli>(
                std::chrono::steady_clock::now() - iter_begin).count();

            if (solve_stopped(runtime, pool)) {
                return std::unexpected(solve_cancelled_error());
            }

            const auto extraction_begin = std::chrono::steady_clock::now();
            emit_progress(
                runtime,
                solve_progress_stage::extraction,
                iterations,
                iterations,
                0,
                static_cast<uint8_t>(N),
                0.0,
                "Extracting combo strategies.");

            normalize_combo_action_table(
                lowered.graph, lowered.annotations, strategy_sums, average_strategy, active_combos, nullptr, locks);
            build_node_reach_vectors(lowered.graph, lowered.annotations, average_strategy, reach_vectors, active_combos, node_reach);
            const auto hero = static_cast<uint8_t>(spot.hero_seat);
            std::array<std::vector<reach_vector>, N> node_values_by_seat{};
            for (uint8_t seat = 0; seat < N; ++seat) {
                evaluate_combo_profile(
                    lowered,
                    average_strategy,
                    active_combos,
                    node_reach,
                    cache,
                    base_indices,
                    terminal_nodes,
                    seat,
                    node_values_by_seat[seat],
                    pool);
            }
            if (solve_stopped(runtime, pool)) {
                return std::unexpected(solve_cancelled_error());
            }

            solve_artifact artifact{};
            artifact.players.assign(spot.players.begin(), std::next(spot.players.begin(), N));
            artifact.board = spot.board;
            artifact.street = spot.street;
            artifact.hero_seat = spot.hero_seat;
            artifact.solver.algorithm = "cfr+";
            artifact.solver.iterations = completed_iterations;
            artifact.solver.timestamp = runtime.timestamp_utc.empty() ? now_utc_iso8601() : runtime.timestamp_utc;
            artifact.solver.git_revision = runtime.git_revision;
            {
                artifact.solver.hashes = make_solve_hashes<N>(
                    lowered.graph, lowered.annotations, spot, config, reach_vectors, public_board, runtime, iterations);
                artifact.solver.warnings = build_solve_warnings(runtime, exploitability_supported, spot.locks.size());
                artifact.solver.locks = spot.locks;
                convergence_report report{};
                report.exploitability_available = have_exploitability;
                if (have_exploitability) {
                    report.exploitability = final_exploitability.exploitability;
                    report.exploitability_pot_fraction = spot.gross_pot > 0.0
                        ? final_exploitability.exploitability / spot.gross_pot : 0.0;
                    report.nash_conv = final_exploitability.nash_conv;
                    report.best_response_gap.assign(
                        final_exploitability.seat_gap.begin(),
                        std::next(final_exploitability.seat_gap.begin(), N));
                }
                report.normalized_regret = normalized_regret_from_combo_table(
                    lowered.graph, lowered.annotations, regrets, active_combos, completed_iterations);
                report.target_exploitability = runtime.convergence.target_exploitability;
                report.reached_target = reached_target;
                report.curve = std::move(convergence_curve);
                artifact.solver.convergence = std::move(report);
            }
            append_root_summary<N>(
                artifact,
                lowered.graph,
                lowered.annotations,
                average_strategy,
                labels[lowered.graph.root_node],
                reach_vectors,
                active_combos,
                node_values_by_seat[hero][lowered.graph.root_node],
                public_board.mask,
                hero,
                locks);
            append_solved_graph_payload(
                artifact,
                lowered.graph,
                lowered.annotations,
                nullptr,
                nullptr,
                nullptr,
                make_node_strategies<N>(
                    lowered.graph, lowered.annotations, average_strategy, active_combos, node_reach, labels));
            populate_solved_node_profiles(
                artifact,
                lowered.graph,
                lowered.annotations,
                average_strategy,
                reach_vectors,
                active_combos,
                node_reach,
                node_values_by_seat,
                std::vector<card_mask>(lowered.graph.node_count, public_board.mask),
                labels,
                locks);
            output.artifact = std::move(artifact);
            output.timing.extraction_ms = std::chrono::duration<double, std::milli>(
                std::chrono::steady_clock::now() - extraction_begin).count();
            emit_progress(
                runtime,
                solve_progress_stage::extraction,
                iterations,
                iterations,
                0,
                static_cast<uint8_t>(N),
                output.timing.extraction_ms,
                "Combo extraction complete.");

            if (auto validation = validate_artifact(output.artifact); !validation) {
                return std::unexpected(validation.error());
            }
            return {};
        }

        template <std::size_t N>
        [[nodiscard]] inline std::expected<solve_output, cli_error> solve_spot_impl(
            const struct solve_spot& spot,
            const uint64_t iterations,
            const solve_runtime_options& runtime)
        {
            if (solve_cancel_requested(runtime)) {
                return std::unexpected(solve_cancelled_error());
            }
            auto parsed_street = parse_holdem_street(spot.street);
            if (!parsed_street) {
                return std::unexpected(parsed_street.error());
            }
            const auto street = *parsed_street;
            auto board_result = board_from_cards(spot.board, street);
            if (!board_result) {
                return std::unexpected(board_result.error());
            }
            const auto public_board = *board_result;

            std::array<hand_range, N> ranges{};
            for (std::size_t seat = 0; seat < N; ++seat) {
                const auto label = "seat_" + std::to_string(seat);
                if (auto parse = parse_range_checked(spot.ranges[seat], ranges[seat], label.c_str()); !parse) {
                    return std::unexpected(parse.error());
                }
                ranges[seat].remove_dead(public_board.mask);
            }

            std::array<reach_vector, N> reach_vectors{};
            for (std::size_t seat = 0; seat < N; ++seat) {
                reach_vectors[seat] = make_reach_vector(ranges[seat]);
                bool has_live_combo = false;
                for (combination_index combo = 0; combo < combination_count; ++combo) {
                    if (reach_vectors[seat][combo] > 0.0f) {
                        has_live_combo = true;
                        break;
                    }
                }
                if (!has_live_combo) {
                    return std::unexpected(cli_error{
                        cli_error_kind::invalid_spot,
                        "Board blockers removed all combos from player range: " + spot.players[seat]
                    });
                }
            }

            std::array<utility, N> initial_stacks{};
            std::array<utility, N> initial_committed{};
            for (std::size_t seat = 0; seat < N; ++seat) {
                initial_stacks[seat] = spot.stacks[seat];
                initial_committed[seat] = spot.contributions[seat];
            }

            solve_output output{};
            const auto graph_begin = std::chrono::steady_clock::now();

            cfr::holdem_betting_graph_config<N> config{};
            config.street = street;
            config.initial_stacks = initial_stacks;
            config.initial_committed = initial_committed;
            config.root_actor = spot.root_actor;
            config.abstraction = resolve_spot_betting_policy(spot);
            config.max_history = spot.max_history;
            config.public_state_id = spot.public_state_id;

            if (street != cfr::solver::holdem_street::river) {
                const auto isomorphism_mode = runtime.enable_card_isomorphism
                    ? cfr::public_card_isomorphism_mode::suit_isomorphic
                    : cfr::public_card_isomorphism_mode::exact;

                cfr::holdem_public_game_config<N> public_config{};
                public_config.street = street;
                public_config.board_cards = public_board.mask;
                public_config.dead_cards = 0u;
                public_config.initial_stacks = initial_stacks;
                public_config.initial_committed = initial_committed;
                public_config.root_actor = spot.root_actor;
                public_config.abstraction = config.abstraction;
                public_config.max_history = spot.max_history;
                public_config.public_state_id = spot.public_state_id;
                public_config.isomorphism = isomorphism_mode;

                // Pre-build budget check: estimate the footprint from the graph shape
                // without materialising the graph, and refuse to allocate anything if it
                // exceeds the resolved memory budget.
                auto estimate = cfr::estimate_multi_street_public_game_memory(public_config, resolve_worker_count(runtime));
                if (!estimate) {
                    return std::unexpected(cli_error{
                        cli_error_kind::solver,
                        "Failed to estimate multi-street solver memory footprint."
                    });
                }
                const auto budget_bytes = resolve_memory_budget_bytes(runtime);
                if (estimate->total_bytes > budget_bytes) {
                    return std::unexpected(over_budget_error(*estimate, budget_bytes));
                }

                auto lowered_public = cfr::lower_multi_street_public_game(public_config);
                if (!lowered_public) {
                    return std::unexpected(cli_error{
                        cli_error_kind::solver,
                        "Failed to lower the multi-street public game graph from the input spot."
                    });
                }
                if (runtime.enable_card_isomorphism && !runtime.allow_lossy_card_isomorphism
                    && !ranges_are_suit_symmetric_for_isomorphism(*lowered_public, reach_vectors, isomorphism_mode)) {
                    return std::unexpected(cli_error{
                        cli_error_kind::solver,
                        "Card isomorphism is lossless only for suit-symmetric ranges; these ranges are not "
                        "symmetric under the collapsed suit permutations. Enable allow_lossy_card_isomorphism "
                        "to accept the approximation, or solve without card isomorphism."
                    });
                }
                auto public_layout = cfr::make_action_table_layout(lowered_public->graph);
                if (!public_layout) {
                    return std::unexpected(cli_error{
                        cli_error_kind::solver,
                        "Failed to build CFR action layout for the multi-street public game."
                    });
                }
                output.timing.graph_build_ms = std::chrono::duration<double, std::milli>(
                    std::chrono::steady_clock::now() - graph_begin).count();
                emit_progress(
                    runtime,
                    solve_progress_stage::graph_build,
                    0,
                    iterations,
                    0,
                    static_cast<uint8_t>(N),
                    output.timing.graph_build_ms,
                    "Public game graph built.");
                if (auto solved = solve_multi_street_public_game<N>(
                        spot,
                        iterations,
                        runtime,
                        *lowered_public,
                        *public_layout,
                        config,
                        reach_vectors,
                        output);
                    !solved) {
                    return std::unexpected(solved.error());
                }
                return output;
            }

            auto lowered_result = cfr::lower_betting_tree_to_graph(config);
            if (!lowered_result) {
                return std::unexpected(cli_error{
                    cli_error_kind::solver,
                    "Failed to lower betting tree to CFR graph."
                });
            }
            const auto& lowered = *lowered_result;
            auto layout_result = cfr::make_action_table_layout(lowered.graph);
            if (!layout_result) {
                return std::unexpected(cli_error{
                    cli_error_kind::solver,
                    "Failed to build CFR action layout."
                });
            }
            output.timing.graph_build_ms = std::chrono::duration<double, std::milli>(
                std::chrono::steady_clock::now() - graph_begin).count();
            emit_progress(
                runtime,
                solve_progress_stage::graph_build,
                0,
                iterations,
                0,
                static_cast<uint8_t>(N),
                output.timing.graph_build_ms,
                "Graph built.");
            if (auto solved = solve_river<N>(
                    spot,
                    iterations,
                    runtime,
                    lowered,
                    *layout_result,
                    config,
                    public_board,
                    reach_vectors,
                    output);
                !solved) {
                return std::unexpected(solved.error());
            }
            return output;
        }
    }

}
