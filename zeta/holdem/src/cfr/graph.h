#pragma once

#include <cstdint>
#include <vector>
#include <span>
#include <cassert>
#include <algorithm>
#include <expected>
#include <limits>
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

    struct graph_partition {
        uint32_t begin_node;        /**< Inclusive: first node in DFS order for this partition. */
        uint32_t end_node;          /**< Exclusive: last node + 1 in DFS order for this partition. */
        uint32_t node_count;        /**< Number of nodes in partition. */
        uint32_t terminal_count;    /**< Terminal nodes in partition. */
        uint32_t action_count;      /**< Total action count in partition. */
        uint16_t min_depth;
        uint16_t max_depth;
        uint64_t estimated_work;    /**< Heuristic cost metric for scheduling. */
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
        invalid_graph,
        invalid_graph_partitions
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
            case invalid_graph_partitions: return "graph_build_error_kind::invalid_graph_partitions";
        }
        return "graph_build_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const graph_build_error_kind kind)
    {
        return os << to_string(kind);
    }

    /**
     * Immutable infoset game tree with CSR topology and scheduling metadata.
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
     *   - WARNING: Greedy partitioning by work accumulation CAN split subtrees
     * 
     * Structural assumptions (validated during construction):
     *   - Single-rooted tree (exactly one root, all nodes reachable)
     *   - Each non-root node has exactly one parent (tree property, not DAG)
     *   - No cycles (implied by connected tree + one parent per non-root)
     *   - Action indices contiguous 0..degree-1 per node (no gaps, no duplicates)
     *   - No duplicate edges (same source and destination)
     *   - Fully immutable after construction (thread-safe reads only)
     */
    struct game_graph {
        static constexpr uint32_t INVALID_INFOSET = ~0u;

        /** Immutable CSR topology. */
        std::vector<uint32_t> row_offsets;      /**< Size: node_count + 1. */
        std::vector<edge> edges;                /**< Flattened adjacency list. */
        std::vector<node_kind> node_types;      /**< Node type per node_id. */
        std::vector<uint32_t> infoset_id;       /**< Infoset mapping per node; INVALID_INFOSET for non-player nodes. */
        std::vector<uint16_t> node_depth;       /**< Depth of each node in tree. */
        std::vector<uint32_t> subtree_size;     /**< Actual subtree size, including self. */

        /** Immutable scheduling metadata. */
        std::vector<graph_partition> partitions;

        /** Metadata. */
        uint32_t node_count = 0;
        uint32_t root_node = 0;       /**< Root node after DFS post-order reordering. */
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

        /**
         * Compute balance metric for partitions.
         * Returns coefficient of variation (stddev/mean) of estimated_work.
         * Lower is better (0.0 = perfect balance).
         */
        [[nodiscard]] double partition_balance_metric() const noexcept;
    };

    namespace graph_validation {
        [[nodiscard]] bool validate(const game_graph& graph) noexcept;
        [[nodiscard]] std::expected<void, graph_build_error> validate_all(const game_graph& graph) noexcept;
        [[nodiscard]] std::expected<void, graph_build_error> validate_structure(const game_graph& graph) noexcept;
        [[nodiscard]] std::expected<void, graph_build_error> validate_metadata(const game_graph& graph) noexcept;
        [[nodiscard]] std::expected<void, graph_build_error> validate_infosets(const game_graph& graph) noexcept;
        [[nodiscard]] std::expected<void, graph_build_error> validate_partitions(const game_graph& graph) noexcept;
    }

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
         * 6. Partition for work distribution
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

        /**
         * Compute DFS post-order, depths, and subtree sizes via single tree traversal.
         * Returns mapping from original node IDs to DFS-reordered node IDs.
         */
        struct dfs_result {
            std::vector<uint16_t> depth;
            std::vector<uint32_t> subtree_size;
            std::vector<uint32_t> dfs_order;  /**< dfs_order[old_id] = new_id. */
            std::vector<uint32_t> inverse_order;  /**< inverse_order[new_id] = old_id. */
            uint16_t max_depth;
        };
        [[nodiscard]] std::expected<dfs_result, graph_build_error> compute_tree_metadata_() const;
        void sort_edges_by_action_();
        void build_node_arrays_(game_graph& graph, const dfs_result& metadata) const;
        void build_csr_(game_graph& graph, const dfs_result& metadata) const;
        void compute_graph_counts_(game_graph& graph) const noexcept;
        [[nodiscard]] static std::expected<void, graph_build_error> validate_complete_(const game_graph& graph) noexcept;
    };

    /**
     * Partition strategy: divide work across threads while balancing load.
     * 
     * ARCHITECTURAL NOTE: Partitioning is a scheduling policy, separate from
     * graph topology. In production, consider moving this to a separate scheduler
     * module that consumes the graph and produces partition plans. This would allow:
     *   - Multiple partition strategies (DFS chunks, NUMA-aware, GPU, work-stealing)
     *   - Experimentation with scheduling algorithms without modifying the graph
     *   - Reusing the same graph for different parallel strategies
     * 
     * Current behavior:
     *   - Greedy work-based partitioning in DFS order
     *   - Attempts to balance estimated_work across partitions
     *   - NOTE: Does NOT guarantee subtree locality (can split subtrees)
     * 
     * Work Heuristic (provisional):
     *   - Formula: actions * 2^depth (using bitshift for speed)
     *   - Cheap to compute but not accurate for CFR cost
     *   - Actual CFR cost depends on reach probability, strategy updates, etc.
     *   - Should eventually be replaced with dynamic measurement or better model
     */
    struct partition_strategy {
        uint32_t target_partition_count = 8;
    };

    /**
     * Compute partitions for a game graph using given strategy.
     * Partitions preserve DFS order, but the greedy work balancer can split subtrees.
     */
    std::vector<graph_partition> compute_partitions(
        const game_graph& graph,
        const partition_strategy& strategy);

}
