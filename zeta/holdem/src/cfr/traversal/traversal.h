#pragma once

#include "cfr/graph/graph.h"
#include "cfr/tables/delta_buffer.h"
#include "cfr/tables/regret_table.h"
#include "cfr/tables/table_layout.h"
#include "terminal/terminal.h"

#include <algorithm>
#include <cassert>
#include <array>
#include <concepts>
#include <cstdint>
#include <expected>
#include <ostream>
#include <span>
#include <type_traits>
#include <vector>

namespace zeta::holdem::cfr::traversal {

    inline constexpr uint32_t DEFAULT_STACK_MARGIN = 1;

    enum class traversal_phase : uint8_t {
        enter = 0,
        visit_children = 1,
        exit = 2
    };

    [[nodiscard]] constexpr const char* to_string(const traversal_phase phase) noexcept
    {
        using enum traversal_phase;
        switch (phase) {
            case enter:          return "traversal_phase::enter";
            case visit_children: return "traversal_phase::visit_children";
            case exit:           return "traversal_phase::exit";
        }
        return "traversal_phase::unknown";
    }

    struct traversal_frame {
        uint32_t node_id = game_graph::INVALID_NODE;
        uint32_t next_edge_offset = 0;
        float reach_oop = 0.0f;
        float reach_ip = 0.0f;
        float chance_weight = 0.0f;
        float accumulated_utility = 0.0f;
        traversal_phase phase = traversal_phase::enter;
        uint8_t reserved[3]{};
    };

    static_assert(std::is_trivially_copyable_v<traversal_frame>);
    static_assert(sizeof(traversal_frame) <= 32);

    struct traversal_scope {
        uint32_t root_node = game_graph::INVALID_NODE;
        uint32_t begin_node = 0;
        uint32_t end_node = game_graph::INVALID_NODE;
    };

    struct traversal_config {
        traversal_scope scope{};
        float initial_reach_oop = 1.0f;
        float initial_reach_ip = 1.0f;
        float initial_chance_weight = 1.0f;
    };

    struct traversal_diagnostics {
        uint64_t nodes_visited = 0;
        uint64_t edges_scanned = 0;
        uint64_t terminal_nodes = 0;
        uint64_t player_nodes = 0;
        uint64_t player_chance_nodes = 0;
        uint64_t chance_nodes = 0;
        uint32_t max_stack_depth = 0;
        uint32_t max_action_count = 0;
        uint32_t local_delta_entries_touched = 0;
    };

    struct traversal_result {
        float root_utility = 0.0f;
        traversal_diagnostics diagnostics{};
        uint32_t root_node = game_graph::INVALID_NODE;
        uint32_t scope_begin_node = 0;
        uint32_t scope_end_node = 0;
    };

    enum class traversal_error_kind : uint8_t {
        empty_graph,
        invalid_root,
        invalid_scope,
        unbound_worker_context,
        stack_capacity_exceeded,
        scratch_capacity_exceeded,
        table_layout_mismatch,
        invalid_terminal_context
    };

    struct traversal_error {
        traversal_error_kind kind{};
        uint32_t node_id = game_graph::INVALID_NODE;
        uint32_t required_capacity = 0;
        uint32_t available_capacity = 0;
    };

    [[nodiscard]] constexpr const char* to_string(const traversal_error_kind kind) noexcept
    {
        using enum traversal_error_kind;
        switch (kind) {
            case empty_graph:             return "traversal_error_kind::empty_graph";
            case invalid_root:            return "traversal_error_kind::invalid_root";
            case invalid_scope:           return "traversal_error_kind::invalid_scope";
            case unbound_worker_context:  return "traversal_error_kind::unbound_worker_context";
            case stack_capacity_exceeded: return "traversal_error_kind::stack_capacity_exceeded";
            case scratch_capacity_exceeded: return "traversal_error_kind::scratch_capacity_exceeded";
            case table_layout_mismatch:   return "traversal_error_kind::table_layout_mismatch";
            case invalid_terminal_context: return "traversal_error_kind::invalid_terminal_context";
        }
        return "traversal_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const traversal_error_kind kind)
    {
        return os << to_string(kind);
    }

    struct worker_input_views {
        const game_graph* graph = nullptr;
        const regret_table* regrets = nullptr;
        const ::zeta::holdem::river_terminal_cache* river_cache = nullptr;
        std::span<const ::zeta::holdem::river_reach_index> river_reach_indices{};

        [[nodiscard]] bool has_graph_tables() const noexcept
        {
            return graph != nullptr && regrets != nullptr;
        }

        [[nodiscard]] bool has_river_terminal_views() const noexcept
        {
            return river_cache != nullptr && !river_reach_indices.empty();
        }
    };

    struct alignas(64) worker_context {
        worker_input_views inputs{};
        std::vector<traversal_frame> stack;
        std::vector<float> node_utility;
        std::vector<float> edge_probability;
        table_delta_buffer delta_buffer;
        traversal_diagnostics diagnostics{};

        [[nodiscard]] uint32_t stack_capacity() const noexcept
        {
            return static_cast<uint32_t>(stack.size());
        }

        [[nodiscard]] uint32_t node_utility_capacity() const noexcept
        {
            return static_cast<uint32_t>(node_utility.size());
        }

        [[nodiscard]] uint32_t edge_probability_capacity() const noexcept
        {
            return static_cast<uint32_t>(edge_probability.size());
        }
    };

    struct default_terminal_policy {
        [[nodiscard]] float operator()(
            const uint32_t,
            const traversal_frame& frame) const noexcept
        {
            return frame.chance_weight * (frame.reach_oop - frame.reach_ip);
        }
    };

    struct table_terminal_policy {
        std::span<const float> terminal_utility_by_node;

        [[nodiscard]] float operator()(
            const uint32_t node_id,
            const traversal_frame& frame) const noexcept
        {
            assert(node_id < terminal_utility_by_node.size());
            return terminal_utility_by_node[node_id] * frame.chance_weight * (frame.reach_oop - frame.reach_ip);
        }
    };

    /**
     * Terminal metadata indexed by graph node id.
     */
    struct river_terminal_leaf {
        uint32_t terminal_state_id = game_graph::INVALID_NODE;
    };

    /**
     * River terminal policy that converts terminal evaluator combo values into a scalar leaf utility.
     */
    struct river_terminal_leaf_policy {
        const ::zeta::holdem::river_terminal_cache* river_cache = nullptr;
        std::span<const ::zeta::holdem::river_reach_index> reach_indices{};
        std::span<const river_terminal_leaf> terminal_leaves{};
        std::span<const ::zeta::holdem::terminal_state<2>> terminal_states{};
        ::zeta::holdem::heads_up_player perspective = ::zeta::holdem::heads_up_player::oop;
        ::zeta::holdem::combination_index combo = 0;

        [[nodiscard]] std::expected<void, traversal_error> validate(const game_graph& graph) const noexcept
        {
            if (river_cache == nullptr
                || reach_indices.size() < 2u
                || terminal_leaves.size() < graph.node_count
                || terminal_states.empty()) {
                return std::unexpected(traversal_error{traversal_error_kind::invalid_terminal_context});
            }

            for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
                if (graph.node_types[node_id] != node_kind::terminal) {
                    continue;
                }
                if (terminal_leaves[node_id].terminal_state_id >= terminal_states.size()) {
                    return std::unexpected(traversal_error{traversal_error_kind::invalid_terminal_context, node_id});
                }
            }

            return {};
        }

        [[nodiscard]] float operator()(
            const uint32_t node_id,
            const traversal_frame& frame) const noexcept
        {
            assert(river_cache != nullptr);
            assert(reach_indices.size() >= 2u);
            assert(node_id < terminal_leaves.size());
            assert(terminal_leaves[node_id].terminal_state_id < terminal_states.size());

            const auto& terminal = terminal_leaves[node_id];
            const auto& state = terminal_states[terminal.terminal_state_id];
            const ::zeta::holdem::terminal_engine<2> engine{};
            const auto values = engine.evaluate_terminal_values(
                *river_cache,
                reach_indices[0],
                reach_indices[1],
                state);

            const auto own_reach = perspective == ::zeta::holdem::heads_up_player::oop
                ? frame.reach_oop
                : frame.reach_ip;
            return frame.chance_weight * own_reach * values[perspective][combo];
        }
    };

    [[nodiscard]] inline traversal_scope whole_graph_scope(const game_graph& graph) noexcept
    {
        return traversal_scope{graph.root_node, 0u, graph.node_count};
    }

    [[nodiscard]] inline std::expected<void, table_layout_error> prepare_worker_context(
        worker_context& worker,
        const game_graph& graph,
        const regret_table& regrets,
        const ::zeta::holdem::river_terminal_cache* river_cache = nullptr,
        const std::span<const ::zeta::holdem::river_reach_index> river_reach_indices = {},
        const uint32_t stack_margin = DEFAULT_STACK_MARGIN)
    {
        worker.inputs = worker_input_views{
            .graph = &graph,
            .regrets = &regrets,
            .river_cache = river_cache,
            .river_reach_indices = river_reach_indices
        };
        worker.stack.resize(static_cast<size_t>(graph.max_depth) + 1u + stack_margin);
        worker.node_utility.resize(graph.node_count);
        worker.edge_probability.resize(graph.edges.size());
        worker.diagnostics = {};

        if (auto result = worker.delta_buffer.reset_layout(regrets.action_offsets); !result) {
            return std::unexpected(result.error());
        }
        worker.delta_buffer.reserve_sparse_entries(graph.infoset_count, regrets.value_count());
        return {};
    }

    namespace detail {

        [[nodiscard]] inline std::unexpected<traversal_error> traversal_failure(
            const traversal_error_kind kind,
            const uint32_t node_id = game_graph::INVALID_NODE,
            const uint32_t required_capacity = 0,
            const uint32_t available_capacity = 0) noexcept
        {
            return std::unexpected(traversal_error{kind, node_id, required_capacity, available_capacity});
        }

        [[nodiscard]] inline bool is_player_like(const node_kind kind) noexcept
        {
            return kind == node_kind::player || kind == node_kind::player_chance;
        }

        [[nodiscard]] inline traversal_scope normalize_scope(
            const game_graph& graph,
            traversal_scope scope) noexcept
        {
            if (scope.root_node == game_graph::INVALID_NODE) {
                scope.root_node = graph.root_node;
            }
            if (scope.end_node == game_graph::INVALID_NODE) {
                scope.end_node = graph.node_count;
            }
            return scope;
        }

        inline void record_node_entry(
            const game_graph& graph,
            worker_context& worker,
            const uint32_t node_id) noexcept
        {
            const auto kind = graph.node_types[node_id];
            const auto action_count = graph.action_count(node_id);

            auto& diagnostics = worker.diagnostics;
            ++diagnostics.nodes_visited;
            diagnostics.max_action_count = std::max(diagnostics.max_action_count, action_count);

            using enum node_kind;
            switch (kind) {
                case player_chance:
                    ++diagnostics.player_chance_nodes;
                    break;
                case player:
                    ++diagnostics.player_nodes;
                    break;
                case chance:
                    ++diagnostics.chance_nodes;
                    break;
                case terminal:
                    ++diagnostics.terminal_nodes;
                    break;
            }
        }

        inline void prepare_player_probabilities(
            const game_graph& graph,
            const regret_table& regrets,
            worker_context& worker,
            const traversal_frame& frame)
        {
            const auto node_id = frame.node_id;
            const auto begin = graph.row_offsets[node_id];
            const auto end = graph.row_offsets[node_id + 1u];
            const auto action_count = end - begin;
            if (action_count == 0u) {
                return;
            }

            const auto infoset_id = graph.infoset_id[node_id];
            const auto infoset_regrets = regrets.infoset_regrets(infoset_id);

            float positive_sum = 0.0f;
            for (uint32_t edge_offset = begin; edge_offset < end; ++edge_offset) {
                const auto action_index = graph.edges[edge_offset].action_index;
                positive_sum += std::max(infoset_regrets[action_index], 0.0f);
            }

            auto strategy_deltas = worker.delta_buffer.strategy_deltas(infoset_id);
            const auto reach_scale = frame.chance_weight * (frame.reach_oop + frame.reach_ip);
            const auto uniform_probability = 1.0f / static_cast<float>(action_count);

            for (uint32_t edge_offset = begin; edge_offset < end; ++edge_offset) {
                const auto action_index = graph.edges[edge_offset].action_index;
                const auto probability = positive_sum > 0.0f
                    ? std::max(infoset_regrets[action_index], 0.0f) / positive_sum
                    : uniform_probability;
                worker.edge_probability[edge_offset] = probability;
                strategy_deltas[action_index] += reach_scale * probability;
            }
        }

        [[nodiscard]] inline traversal_frame make_child_frame(
            const game_graph& graph,
            const worker_context& worker,
            const traversal_frame& parent,
            const edge child_edge,
            const uint32_t edge_offset) noexcept
        {
            const auto kind = graph.node_types[parent.node_id];
            const auto action_count = graph.row_offsets[parent.node_id + 1u] - graph.row_offsets[parent.node_id];

            auto child = traversal_frame{};
            child.node_id = child_edge.child_node;
            child.next_edge_offset = graph.row_offsets[child.node_id];
            child.reach_oop = parent.reach_oop;
            child.reach_ip = parent.reach_ip;
            child.chance_weight = parent.chance_weight;

            if (is_player_like(kind)) {
                const auto probability = worker.edge_probability[edge_offset];
                child.reach_oop *= probability;
                child.reach_ip *= probability;
            } else if (kind == node_kind::chance && action_count > 0u) {
                child.chance_weight *= 1.0f / static_cast<float>(action_count);
            }

            return child;
        }

        template <typename TerminalPolicy>
        concept validates_terminal_context = requires(const TerminalPolicy& policy, const game_graph& graph) {
            { policy.validate(graph) } -> std::same_as<std::expected<void, traversal_error>>;
        };
    }

    template <typename TerminalPolicy>
    [[nodiscard]] std::expected<traversal_result, traversal_error> traverse_game_tree(
        const game_graph& graph,
        const regret_table& regrets,
        worker_context& worker,
        TerminalPolicy&& terminal_policy,
        traversal_config config = {})
    {
        if (graph.node_count == 0u) {
            return detail::traversal_failure(traversal_error_kind::empty_graph);
        }

        const auto scope = detail::normalize_scope(graph, config.scope);
        if (scope.root_node >= graph.node_count) {
            return detail::traversal_failure(traversal_error_kind::invalid_root, scope.root_node);
        }
        if (scope.begin_node > scope.end_node
            || scope.end_node > graph.node_count
            || scope.root_node < scope.begin_node
            || scope.root_node >= scope.end_node) {
            return detail::traversal_failure(traversal_error_kind::invalid_scope, scope.root_node);
        }

        const auto required_stack = static_cast<uint32_t>(graph.max_depth) + 1u;
        if (worker.stack_capacity() < required_stack) {
            return detail::traversal_failure(
                traversal_error_kind::stack_capacity_exceeded,
                scope.root_node,
                required_stack,
                worker.stack_capacity());
        }
        const auto required_edge_scratch = static_cast<uint32_t>(graph.edges.size());
        if (worker.node_utility_capacity() < graph.node_count
            || worker.edge_probability_capacity() < required_edge_scratch) {
            return detail::traversal_failure(
                traversal_error_kind::scratch_capacity_exceeded,
                scope.root_node,
                std::max(graph.node_count, required_edge_scratch),
                std::min(worker.node_utility_capacity(), worker.edge_probability_capacity()));
        }
        if (!same_action_offsets(regrets.action_offsets, worker.delta_buffer.action_offsets())) {
            return detail::traversal_failure(traversal_error_kind::table_layout_mismatch, scope.root_node);
        }
        if constexpr (detail::validates_terminal_context<std::remove_cvref_t<TerminalPolicy>>) {
            if (auto terminal_result = terminal_policy.validate(graph); !terminal_result) {
                return std::unexpected(terminal_result.error());
            }
        }

        worker.diagnostics = {};
        worker.delta_buffer.clear();
        std::fill_n(worker.node_utility.data(), graph.node_count, 0.0f);

        auto* stack = worker.stack.data();
        uint32_t stack_size = 1;
        float root_utility = 0.0f;

        stack[0] = traversal_frame{
            .node_id = scope.root_node,
            .next_edge_offset = graph.row_offsets[scope.root_node],
            .reach_oop = config.initial_reach_oop,
            .reach_ip = config.initial_reach_ip,
            .chance_weight = config.initial_chance_weight,
            .accumulated_utility = 0.0f,
            .phase = traversal_phase::enter,
            .reserved = {}
        };
        worker.diagnostics.max_stack_depth = 1;

        while (stack_size > 0u) {
            auto& frame = stack[stack_size - 1u];

            using enum traversal_phase;
            switch (frame.phase) {
                case enter: {
                    detail::record_node_entry(graph, worker, frame.node_id);
                    frame.next_edge_offset = graph.row_offsets[frame.node_id];
                    frame.accumulated_utility = 0.0f;

                    const auto kind = graph.node_types[frame.node_id];
                    if (kind == node_kind::terminal) {
                        frame.accumulated_utility = terminal_policy(frame.node_id, frame);
                        frame.phase = exit;
                    } else {
                        if (detail::is_player_like(kind)) {
                            detail::prepare_player_probabilities(graph, regrets, worker, frame);
                        }
                        frame.phase = visit_children;
                    }
                    break;
                }

                case visit_children: {
                    const auto end = graph.row_offsets[frame.node_id + 1u];
                    if (frame.next_edge_offset == end) {
                        frame.phase = exit;
                        break;
                    }

                    if (stack_size == worker.stack_capacity()) {
                        return detail::traversal_failure(
                            traversal_error_kind::stack_capacity_exceeded,
                            frame.node_id,
                            stack_size + 1u,
                            worker.stack_capacity());
                    }

                    const auto edge_offset = frame.next_edge_offset++;
                    const auto child_edge = graph.edges[edge_offset];
                    ++worker.diagnostics.edges_scanned;

                    stack[stack_size] = detail::make_child_frame(graph, worker, frame, child_edge, edge_offset);
                    ++stack_size;
                    worker.diagnostics.max_stack_depth = std::max(worker.diagnostics.max_stack_depth, stack_size);
                    break;
                }

                case exit: {
                    const auto node_id = frame.node_id;
                    const auto utility = frame.accumulated_utility;
                    worker.node_utility[node_id] = utility;

                    --stack_size;
                    if (stack_size == 0u) {
                        root_utility = utility;
                    } else {
                        stack[stack_size - 1u].accumulated_utility += utility;
                    }
                    break;
                }
            }
        }

        worker.diagnostics.local_delta_entries_touched = worker.delta_buffer.entry_count();

        return traversal_result{
            .root_utility = root_utility,
            .diagnostics = worker.diagnostics,
            .root_node = scope.root_node,
            .scope_begin_node = scope.begin_node,
            .scope_end_node = scope.end_node
        };
    }

    template <typename TerminalPolicy>
    [[nodiscard]] std::expected<traversal_result, traversal_error> traverse_game_tree(
        worker_context& worker,
        TerminalPolicy&& terminal_policy,
        traversal_config config = {})
    {
        if (!worker.inputs.has_graph_tables()) {
            return detail::traversal_failure(traversal_error_kind::unbound_worker_context);
        }

        return traverse_game_tree(
            *worker.inputs.graph,
            *worker.inputs.regrets,
            worker,
            static_cast<TerminalPolicy&&>(terminal_policy),
            config);
    }

    [[nodiscard]] inline std::expected<traversal_result, traversal_error> traverse_game_tree(
        worker_context& worker,
        traversal_config config = {})
    {
        return traverse_game_tree(worker, default_terminal_policy{}, config);
    }

}
