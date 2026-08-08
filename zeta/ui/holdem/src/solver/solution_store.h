#pragma once

#include "cli/solve_cli.h"

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace zeta::holdem::ui::solver {

    inline constexpr uint32_t current_solution_schema_version = 2;
    inline constexpr uint8_t invalid_solution_seat = 255;

    enum class solution_store_error_kind : uint8_t {
        parse,
        invalid_solution
    };

    struct solution_store_error {
        solution_store_error_kind kind = solution_store_error_kind::parse;
        std::string message;
    };

    enum class solution_compatibility_mode : uint8_t {
        root_only_artifact,
        action_tree
    };

    struct solution_action_summary {
        std::string action;
        double frequency = 0.0;
        double average_ev = 0.0;
    };

    struct solution_seat_ev {
        uint8_t seat = invalid_solution_seat;
        double ev = 0.0;
    };

    struct solution_table_state {
        double pot = 0.0;
        std::vector<double> stacks;
        std::vector<double> commitments;
    };

    struct solution_node {
        std::string node_id;
        std::vector<std::string> path;
        uint8_t acting_seat = invalid_solution_seat;
        bool terminal = false;
        bool truncated = false;
        std::vector<std::string> legal_actions;
        std::vector<solution_action_summary> average_strategy;
        std::vector<solution_seat_ev> seat_evs;
        solution_table_state table_state;
        std::vector<std::string> children;
    };

    struct solution_source_summary {
        std::string game = "holdem";
        std::string street = "river";
        std::vector<std::string> players;
        std::vector<std::string> board;
        uint8_t hero_seat = 0;
        uint8_t root_actor = 0;
        cli::solver_metadata solver;
    };

    struct solution_store {
        uint32_t schema_version = current_solution_schema_version;
        solution_compatibility_mode compatibility_mode = solution_compatibility_mode::root_only_artifact;
        std::string root_node_id = "root";
        solution_source_summary source;
        std::vector<solution_node> nodes;
        std::vector<std::string> diagnostics;
    };

    /**
     * Builds an honest root-only compatibility store from a legacy root artifact.
     */
    [[nodiscard]] solution_store make_root_only_solution_store(
        const struct cli::solve_spot& spot,
        const cli::solve_artifact& artifact);

    /**
     * Builds a solution store with betting-tree nodes and root strategy extracted from the artifact.
     */
    [[nodiscard]] solution_store make_action_tree_solution_store(
        const struct cli::solve_spot& spot,
        const cli::solve_artifact& artifact);

    /**
     * Parses a serialized solution store payload.
     */
    [[nodiscard]] std::expected<solution_store, solution_store_error> parse_solution_store_json(std::string_view text);

    /**
     * Serializes a solution store payload.
     */
    [[nodiscard]] std::string serialize_solution_store_json(const solution_store& store);

    /**
     * Finds a node by stable node id.
     */
    [[nodiscard]] const solution_node* find_solution_node(const solution_store& store, std::string_view node_id) noexcept;

    /**
     * Finds the root node when it is present.
     */
    [[nodiscard]] const solution_node* root_solution_node(const solution_store& store) noexcept;

    [[nodiscard]] std::string_view to_string(solution_compatibility_mode mode) noexcept;

}
