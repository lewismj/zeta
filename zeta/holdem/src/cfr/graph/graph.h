#pragma once

#include <cstdint>
#include <vector>
#include <span>
#include <cassert>
#include <ostream>

namespace zeta::holdem::cfr {

    /**
     * Immutable compressed sparse row (CSR) graph representation.
     * Optimized for cache-friendly traversal and multithread-safe reads.
     */

    struct edge {
        uint32_t child_node;
        uint16_t action_index;
    };

    enum class node_kind : uint8_t {
        player_chance = 0,  /**< Player node; chance must act to determine infoset. */
        player = 1,         /**< Player information set node. */
        chance = 2,         /**< Chance node. */
        terminal = 3        /**< Terminal node. */
    };

    enum class graph_build_error_kind : uint8_t {
        already_finalized,
        empty_graph,
        root_out_of_range,
        depth_overflow,
        disconnected_tree,
        uninitialized_infoset,
        invalid_graph
    };

    struct graph_build_error {
        graph_build_error_kind kind{};
        uint32_t node_id = 0;
        uint32_t related_node_id = 0;
    };

    [[nodiscard]] constexpr const char* to_string(const graph_build_error_kind kind) noexcept
    {
        using enum graph_build_error_kind;
        switch (kind) {
            case already_finalized:        return "graph_build_error_kind::already_finalized";
            case empty_graph:              return "graph_build_error_kind::empty_graph";
            case root_out_of_range:        return "graph_build_error_kind::root_out_of_range";
            case depth_overflow:           return "graph_build_error_kind::depth_overflow";
            case disconnected_tree:        return "graph_build_error_kind::disconnected_tree";
            case uninitialized_infoset:    return "graph_build_error_kind::uninitialized_infoset";
            case invalid_graph:            return "graph_build_error_kind::invalid_graph";
        }
        return "graph_build_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const graph_build_error_kind kind)
    {
        return os << to_string(kind);
    }

    /**
     * Immutable infoset game tree with CSR topology.
     * 
     * CSR storage (compressed sparse row):
     *   - row_offsets[node] = starting index in edges array
     *   - row_offsets[node+1] = ending index in edges array
     *   - Each edge contains child node and action index
     * 
     * Node Ordering:
     *   - DFS post-order (NOT pre-order)
     *   - Post-order processes: left subtree -> right subtree -> node itself
     *   - Example: tree with A->[B,C], B->[D,E], C->[] produces order: D,E,B,C,A
     *   - Important: Parent does NOT immediately precede children in post-order
     *   - This ordering is chosen for:
     *     (1) Efficient bottom-up traversals (compute on children before parent)
     *     (2) Compatible with iterative DFS without recursion
     * Structural assumptions (validated during construction):
     *   - Single-rooted tree (exactly one root, all nodes reachable)
     *   - Each non-root node has exactly one parent (tree property, not DAG)
     *   - No cycles (implied by connected tree + one parent per non-root)
     *   - Action indices contiguous 0..degree-1 per node (no gaps, no duplicates)
     *   - No duplicate edges (same source and destination)
     *   - Fully immutable after construction (thread-safe reads only)
     */
    struct game_graph {
        static constexpr uint32_t INVALID_NODE = ~0u;
        static constexpr uint32_t INVALID_INFOSET = ~0u;

        /** Immutable CSR topology. */
        std::vector<uint32_t> row_offsets;      /**< Size: node_count + 1. */
        std::vector<edge> edges;                /**< Flattened adjacency list. */
        std::vector<node_kind> node_types;      /**< Node type per node_id. */
        std::vector<uint32_t> infoset_id;       /**< Infoset mapping per node; INVALID_INFOSET for non-player nodes. */
        std::vector<uint16_t> node_depth;       /**< Depth of each node in tree. */
        std::vector<uint32_t> subtree_size;     /**< Actual subtree size, including self. */

        /** Metadata. */
        uint32_t node_count = 0;
        uint32_t root_node = 0;       /**< Root node ID in the final DFS post-order numbering. */
        uint32_t terminal_count = 0;
        uint32_t infoset_count = 0;
        uint16_t max_depth = 0;

        /**
         * Get outgoing edges for a node.
         */
        [[nodiscard]] std::span<const edge> out_edges(const uint32_t node_id) const noexcept
        {
            assert(node_id < node_count);
            const auto begin = row_offsets[node_id];
            const auto end = row_offsets[node_id + 1];
            return {edges.data() + begin, end - begin};
        }

        /**
         * Get action count for a node.
         */
        [[nodiscard]] uint32_t action_count(const uint32_t node_id) const noexcept
        {
            const auto edges_span = out_edges(node_id);
            return static_cast<uint32_t>(edges_span.size());
        }

        /**
         * Check if node is terminal.
         */
        [[nodiscard]] bool is_terminal(const uint32_t node_id) const noexcept
        {
            assert(node_id < node_count);
            return node_types[node_id] == node_kind::terminal;
        }

        /**
         * Check if the node is a player node (or player-chance).
         */
        [[nodiscard]] bool is_player_node(const uint32_t node_id) const noexcept
        {
            assert(node_id < node_count);
            const auto k = node_types[node_id];
            return k == cfr::node_kind::player || k == cfr::node_kind::player_chance;
        }

        /**
         * Check if node is a chance node.
         */
        [[nodiscard]] bool is_chance_node(const uint32_t node_id) const noexcept
        {
            assert(node_id < node_count);
            return cfr::node_kind::chance == node_types[node_id];
        }

    };

}
