#pragma once

#include "cfr/graph/graph.h"
#include <expected>
#include <vector>

namespace zeta::holdem::cfr {

    /**
     * Builder for game_graph from a mutable game tree representation.
     *
     * This is a factory pattern to construct an immutable graph
     * from mutable tree data in a single pass.
     *
     * The builder reorders the graph into DFS post-order during build(),
     * ensuring all runtime algorithms naturally work with locality-preserving ordering.
     */
    class graph_builder {
    public:
        explicit graph_builder(const uint32_t expected_nodes = 1000) :
            root_(0)
        {
            edges_by_node_.reserve(expected_nodes);
            node_types_.reserve(expected_nodes);
            infoset_ids_.reserve(expected_nodes);
        }

        /**
         * Set the root node ID for DFS traversal.
         * Default is 0. Must be called before build() if using non-zero root.
         */
        void set_root(const uint32_t root_node) noexcept
        {
            if (finalized_) {
                record_error_(graph_build_error_kind::already_finalized);
                return;
            }
            root_ = root_node;
        }

        /**
         * Add a node with a given kind.
         * Returns the assigned node_id (before DFS reordering).
         * Returns game_graph::INVALID_NODE if the builder has already been finalized.
         *
         * For player nodes, must call set_infoset_id() before build().
         */
        [[nodiscard]] uint32_t add_node(node_kind kind);

        /**
         * Add an edge from source_node to dest_node with action_index.
         * Must be called after add_node() for both endpoints.
         *
         * Note: action_index must be in the range [0, degree-1] and contiguous.
         */
        void add_edge(uint32_t source_node, uint32_t dest_node, uint16_t action_index);

        /**
         * Set infoset_id for a player node. O(1) operation.
         * Must be called before build() for all player nodes.
         * Uses pre-DFS-reordering node IDs.
         */
        void set_infoset_id(uint32_t node_id, uint32_t infoset_id) noexcept;

        /**
         * Build the immutable game_graph.
         *
         * Performs:
         * 1. DFS traversal from root_ to establish ordering
         * 2. Reorder all node arrays into DFS post-order
         * 3. Reconstruct CSR arrays in new order
         * 4. Compute depths, subtree sizes
         * 5. Validate tree properties, action numbering
         *
         * Returns the immutable graph or a typed build error. Construction is the only
         * fallible phase; runtime graph reads remain allocation-free and noexcept.
         */
        std::expected<game_graph, graph_build_error> build();

    private:
        std::vector<std::vector<edge>> edges_by_node_;  /**< Edges organized per node. */
        std::vector<node_kind> node_types_;
        std::vector<uint32_t> infoset_ids_;             /**< Per-node allocation. */
        uint32_t root_ = 0;                             /**< Root node for DFS traversal. */
        bool finalized_ = false;
        bool has_pending_error_ = false;
        graph_build_error pending_error_{};

        void record_error_(
            graph_build_error_kind kind,
            uint32_t node_id = 0,
            uint32_t related_node_id = 0) noexcept;
    };

}
