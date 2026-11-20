#pragma once

#include "board.h"
#include "cfr/betting/betting.h"
#include "range_parser.h"

#include <algorithm>
#include <array>
#include <atomic>
#include <bit>
#include <cassert>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <expected>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iomanip>
#include <ios>
#include <iterator>
#include <limits>
#include <memory>
#include <numeric>
#include <optional>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace zeta::holdem::cli {

    inline constexpr std::size_t cli_min_players = 2;
    inline constexpr std::size_t cli_max_players = 7;

    enum class cli_error_kind : uint8_t {
        io,
        parse,
        invalid_spot,
        invalid_artifact,
        solver,
        cancelled
    };

    struct cli_error {
        cli_error_kind kind = cli_error_kind::parse;
        std::string message;
    };

    struct action_strategy {
        std::string action;
        double frequency = 0.0;
    };

    struct hand_strategy {
        ::zeta::holdem::combination_index combination_index =
            std::numeric_limits<::zeta::holdem::combination_index>::max();
        std::string hand;
        std::vector<action_strategy> strategy;
        double range_weight = 0.0;
        double reach_probability = 0.0;
        double ev = 0.0;                    /**< Expected value in chips: counterfactual_value / opponent_reach_mass (0 when no opponent hand is compatible). */
        double counterfactual_value = 0.0;  /**< Sum over compatible opponent hands of their reach times the payoff. */
        double opponent_reach_mass = 0.0;   /**< Reach mass of opponent hands compatible with this hand and the board. */
        bool locked = false;                /**< True when a node lock constrains this hand's play at the node. */
    };

    /** One constrained action of a node lock. */
    struct node_lock_action {
        std::string action;      /**< Action label at the locked node, e.g. "check" or "bet_75". */
        double frequency = 0.0;  /**< Fixed probability, rounded to node_lock_frequency_step. */
    };

    /**
     * Fixes some of a seat's action frequencies at one decision point, or at one per
     * runout when the path deals "*", and leaves the other actions free for the
     * solver. See doc/holdem/plans/node_locking_plan.md.
     */
    struct node_lock {
        std::vector<std::string> path;          /**< Action labels from the root; chance steps are a dealt card or "*". */
        std::string hands;                      /**< Range text naming the locked hands; empty locks the whole range. */
        std::vector<node_lock_action> actions;  /**< Constrained actions, sorted by label. */
    };

    /** Lock frequencies are rounded to this step, so the JSON text cannot change a solve. */
    inline constexpr double node_lock_frequency_step = 1.0e-6;
    /** Path step that matches every card a chance node deals. */
    inline constexpr std::string_view node_lock_any_card = "*";

    struct solved_node_seat_value {
        uint8_t seat = 0;
        double range_reach_mass = 0.0;      /**< Sum of the seat's path reach over its hands. */
        double matchup_reach_mass = 0.0;    /**< Sum of path reach times opponent_reach_mass: the mass of compatible matchups. */
        double reach_weighted_value = 0.0;  /**< Sum of path reach times the hand's counterfactual value. */
        double conditional_range_ev = 0.0;  /**< Range EV in chips: reach_weighted_value / matchup_reach_mass. */
        double counterfactual_value = 0.0;  /**< Sum of root range weight times the hand's counterfactual value. */
    };

    struct category_summary_action_frequency {
        uint16_t action_index = 0;
        double frequency = 0.0;
    };

    struct category_summary_item {
        std::string category_name;
        double frequency = 0.0;
        double range_weight = 0.0;
        double average_ev = 0.0;
        std::vector<category_summary_action_frequency> action_frequencies;
    };

    /**
     * One measurement of solve quality captured during the CFR loop. @ref iteration
     * is the number of completed CFR iterations at the sample point; @ref metric is
     * the primary convergence metric at that point (heads-up exploitability in EV
     * units when available, otherwise the normalized average regret); @ref elapsed_ms
     * is wall-clock time since the CFR loop started.
     */
    struct convergence_sample {
        uint64_t iteration = 0;
        double metric = 0.0;
        double elapsed_ms = 0.0;
    };

    /**
     * User-facing confidence signals for a completed solve. For supported heads-up
     * abstractions the solver computes an exact best-response exploitability
     * (@ref exploitability, half of NashConv, in EV units); for multiway spots where
     * an exact best response is expensive it reports @ref normalized_regret instead
     * and leaves @ref exploitability_available false. The @ref curve records the
     * primary metric over time, and @ref reached_target reflects whether a quality
     * stop condition fired.
     */
    struct convergence_report {
        bool exploitability_available = false;   /**< True when a best-response exploitability was computed. */
        double exploitability = 0.0;             /**< Half of NashConv, in EV (chip) units. */
        double exploitability_pot_fraction = 0.0;/**< @ref exploitability divided by the gross pot. */
        double nash_conv = 0.0;                  /**< Sum of per-seat best-response gaps, in EV units. */
        std::vector<double> best_response_gap;   /**< Per-seat best-response improvement over the average strategy. */
        double normalized_regret = 0.0;          /**< Mean max positive regret per infoset, divided by iterations. */
        double target_exploitability = 0.0;      /**< Requested stop threshold (0 => none), in EV units. */
        bool reached_target = false;             /**< True when the solve stopped early on the quality threshold. */
        std::vector<convergence_sample> curve;   /**< Primary metric sampled over the CFR loop. */
    };

    /**
     * Reproducible content hashes identifying the exact inputs a solve consumed, so
     * two solves can be compared for equivalence without replaying them. Every hash
     * is a stable FNV-1a digest; @ref solve_hash combines the components.
     */
    struct solve_hashes {
        uint64_t tree_hash = 0;           /**< Lowered game-graph topology and metadata. */
        uint64_t range_hash = 0;          /**< Per-combo reach weights of every seat's range. */
        uint64_t board_hash = 0;          /**< Public board cards. */
        uint64_t betting_policy_hash = 0; /**< Betting abstraction and stack/pot configuration. */
        uint64_t solver_config_hash = 0;  /**< Algorithm identity and solver knobs. */
        uint64_t locks_hash = 0;          /**< Canonical node locks; 0 when the solve has none. */
        uint64_t solve_hash = 0;          /**< Combined digest of all components (locks only when present). */
    };

    struct solver_metadata {
        std::string algorithm = "cfr+";
        uint64_t iterations = 0;
        std::string timestamp;
        std::string git_revision = "unknown";
        solve_hashes hashes{};
        convergence_report convergence{};
        std::vector<std::string> warnings;
        std::vector<node_lock> locks;     /**< Node locks the solve honoured; solved_node::lock_indices index this. */
    };

    inline constexpr uint32_t current_artifact_schema_version = 7;
    /** Oldest artifact schema still read; it predates node locks and loads with none. */
    inline constexpr uint32_t oldest_readable_artifact_schema_version = 6;

    /** Opponent (or matchup) reach mass at or below which an EV is undefined and reported as 0. */
    inline constexpr double minimum_ev_reach_mass = 1.0e-12;
    inline constexpr uint32_t current_extraction_version = 1;

    enum class solve_artifact_export_mode : uint8_t {
        summary,
        standard,
        full
    };

    struct solved_node_action {
        std::string action;
        uint32_t child_node_id = cfr::game_graph::INVALID_NODE;
        uint16_t action_index = 0;
        float probability = 0.0f;
        uint32_t chance_event_id = cfr::INVALID_CHANCE_EVENT;
        uint32_t board_partition_id = cfr::INVALID_BOARD_PARTITION;
        uint32_t chance_outcome_id = cfr::INVALID_CHANCE_OUTCOME_ID;
        std::vector<std::string> dealt_cards;
    };

    struct solved_node {
        uint32_t node_id = cfr::game_graph::INVALID_NODE;
        std::string kind;
        uint32_t public_state_id = cfr::INVALID_PUBLIC_STATE_ID;
        uint32_t parent_node_id = cfr::game_graph::INVALID_NODE;
        uint8_t acting_seat = cfr::solver::INVALID_PLAYER;
        bool terminal = false;
        std::vector<std::string> board;
        std::vector<solved_node_action> actions;
        std::vector<action_strategy> range_action_frequencies;
        std::vector<solved_node_seat_value> seat_values;
        uint32_t category_summary_derivation_version = 1;
        std::vector<category_summary_item> category_summaries;
        std::vector<hand_strategy> strategy_rows;
        std::vector<uint32_t> lock_indices;  /**< Indices into solver.locks of the locks applied at this node. */
    };

    struct solve_artifact_public_state {
        uint32_t id = cfr::INVALID_PUBLIC_STATE_ID;
        std::string street;
        std::vector<std::string> board;
        uint32_t parent_state_id = cfr::INVALID_PUBLIC_STATE_ID;
        uint32_t chance_event_id_from_parent = cfr::INVALID_CHANCE_EVENT;
        uint32_t chance_outcome_id_from_parent = cfr::INVALID_CHANCE_OUTCOME_ID;
        bool is_root_state = false;
        bool is_terminal_river_state = false;
    };

    struct solve_artifact_chance_event {
        uint32_t id = cfr::INVALID_CHANCE_EVENT;
        uint32_t node_id = cfr::game_graph::INVALID_NODE;
        std::string kind;
        std::vector<std::string> board;
        std::vector<solved_node_action> outcomes;
    };

    struct solve_artifact_runout {
        uint32_t id = cfr::INVALID_RUNOUT_ID;
        uint32_t root_public_state_id = cfr::INVALID_PUBLIC_STATE_ID;
        uint32_t river_public_state_id = cfr::INVALID_PUBLIC_STATE_ID;
        std::vector<std::string> dealt_turn;
        std::vector<std::string> dealt_river;
    };

    struct solve_artifact {
        uint32_t schema_version = current_artifact_schema_version;
        uint32_t extraction_version = current_extraction_version;
        std::string game = "holdem";
        std::string street = "river";
        std::vector<std::string> players{"BTN", "BB"};
        std::vector<std::string> board{};
        uint8_t hero_seat = 0;
        solver_metadata solver{};
        std::vector<action_strategy> root_strategy;
        std::vector<hand_strategy> strategy;
        std::vector<solve_artifact_public_state> public_states;
        std::vector<solve_artifact_chance_event> chance_events;
        std::vector<solve_artifact_runout> runouts;
        std::vector<solved_node> solved_nodes;
        uint32_t root_node_id = cfr::game_graph::INVALID_NODE;
    };

    struct solve_spot {
        std::vector<std::string> players{"BTN", "BB"};
        std::string street = "river";
        std::vector<std::string> board{};
        std::vector<std::string> ranges{"AA", "AA"};
        double gross_pot = 100.0;
        double rake = 0.0;
        std::vector<utility> contributions{50.0, 50.0};
        std::vector<utility> stacks{100.0, 100.0};
        double bet_fraction = 0.75;
        cfr::betting_abstraction_policy betting_policy{
            .fixed_pot_fractions = {0.75},
            .max_raises = 1
        };
        uint16_t max_history = 8;
        uint32_t public_state_id = 0;
        uint8_t root_actor = 0;
        uint8_t hero_seat = 0;
        std::vector<node_lock> locks;
    };

    [[nodiscard]] inline cfr::betting_abstraction_policy resolve_spot_betting_policy(const solve_spot& spot)
    {
        auto policy = spot.betting_policy;
        if (policy.fixed_pot_fractions.empty()) {
            policy.fixed_pot_fractions = {spot.bet_fraction > 0.0 ? spot.bet_fraction : 0.75};
            policy.max_raises = 1;
        }
        if (!policy.fixed_pot_fractions.empty()) {
            policy.max_raises = std::max<uint16_t>(1u, policy.max_raises);
        }
        return policy;
    }

    enum class solve_progress_stage : uint8_t {
        graph_build,
        cfr,
        extraction
    };

    struct solve_progress_event {
        solve_progress_stage stage = solve_progress_stage::graph_build;
        uint64_t iterations_completed = 0;
        uint64_t total_iterations = 0;
        uint8_t updating_player = 0;
        uint8_t player_count = 0;
        double elapsed_ms = 0.0;
        std::string message;
    };

    using solve_progress_callback = std::function<void(const solve_progress_event&)>;
    using solve_cancellation_callback = std::function<bool()>;

    /**
     * Opt-in dynamic action pruning for the multi-street CFR+ solver. When
     * @ref enabled is false the solver is bit-identical to the exact Step 2/3
     * solver. When enabled, each infoset maintains an active action set: actions
     * whose reach-weighted positive-regret share falls below @ref prune_threshold
     * are deactivated (their subtree is skipped and their regret frozen), subject
     * to keeping at least @ref minimum_active_actions active. Every
     * @ref reconsider_interval iterations the full action set is reconsidered so
     * that a frozen action can be reactivated once its regret share recovers.
     */
    struct dynamic_pruning_policy {
        bool enabled = false;
        double prune_threshold = 0.0;         /**< Reach-weighted positive-regret share below which an action is pruned. */
        uint32_t minimum_active_actions = 1;  /**< Never prune below this many active actions per infoset. */
        uint32_t reconsider_interval = 64;    /**< Iterations between prune/reactivate reconsideration passes. */

        [[nodiscard]] bool is_active() const noexcept
        {
            return enabled && prune_threshold > 0.0;
        }
    };

    /**
     * Opt-in convergence/exploitability reporting knobs for the CFR solver. When
     * @ref measure_exploitability is false the solver still records reproducible
     * hashes and any applicable warnings but skips the (non-trivial) best-response
     * passes. When enabled, exploitability is measured for supported heads-up
     * abstractions; @ref measurement_interval > 0 additionally records a convergence
     * curve every that-many iterations, and a positive @ref target_exploitability
     * stops the solve early once the measured exploitability drops to or below it.
     */
    struct convergence_options {
        bool measure_exploitability = false; /**< Compute best-response exploitability (heads-up) at the end and on the curve. */
        uint64_t measurement_interval = 0;   /**< Iterations between convergence-curve samples; 0 => final measurement only. */
        double target_exploitability = 0.0;  /**< Stop early once exploitability <= this EV-unit threshold; 0 => run all iterations. */
        uint32_t max_curve_samples = 128;    /**< Cap on retained convergence-curve samples. */
    };

    struct solve_runtime_options {
        std::string timestamp_utc;
        std::string git_revision = "unknown";
        uint64_t progress_batch_iterations = 1;
        uint32_t worker_threads = 1;
        uint64_t memory_budget_bytes = 0;          /**< 0 => derive from detected available system memory. */
        bool enable_card_isomorphism = false;      /**< Collapse suit-isomorphic turn/river boards. */
        bool allow_lossy_card_isomorphism = false; /**< Permit isomorphism when ranges are not suit-symmetric. */
        dynamic_pruning_policy pruning;            /**< Opt-in approximate dynamic action pruning (default off). */
        convergence_options convergence;           /**< Opt-in convergence / exploitability reporting (default off). */
        /** Called on the solving thread at iteration batches and stage changes. */
        solve_progress_callback progress_callback;
        /**
         * Polled once per iteration and before each batch of terminal leaves, from the
         * solving thread and from worker threads, so it must be thread-safe.
         */
        solve_cancellation_callback cancellation_requested;
    };

    struct solve_timing {
        double graph_build_ms = 0.0;
        double cfr_iterations_ms = 0.0;
        double extraction_ms = 0.0;
    };

    struct solve_output {
        solve_artifact artifact;
        solve_timing timing;
    };

    namespace detail {

        /**
         * Available physical memory in bytes as reported by the OS, or 0 if it cannot
         * be determined. Defined in solve_cli.cpp to keep platform headers out of the
         * widely included solver header.
         */
        [[nodiscard]] uint64_t detected_available_memory_bytes() noexcept;

        [[nodiscard]] inline int parse_rank_char(const char c) noexcept
        {
            const char upper = static_cast<char>(std::toupper(static_cast<unsigned char>(c)));
            if (upper >= '2' && upper <= '9') {
                return upper - '2';
            }
            switch (upper) {
                case 'T': return 8;
                case 'J': return 9;
                case 'Q': return 10;
                case 'K': return 11;
                case 'A': return 12;
                default: return -1;
            }
        }

        [[nodiscard]] inline int parse_suit_char(const char c) noexcept
        {
            switch (static_cast<char>(std::tolower(static_cast<unsigned char>(c)))) {
                case 's': return 0;
                case 'h': return 1;
                case 'd': return 2;
                case 'c': return 3;
                default: return -1;
            }
        }

        [[nodiscard]] inline std::expected<card, cli_error> parse_card_text(const std::string_view text)
        {
            if (text.size() != 2) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Card must have length 2."});
            }
            const int rank = parse_rank_char(text[0]);
            const int suit = parse_suit_char(text[1]);
            if (rank < 0 || suit < 0) {
                return std::unexpected(cli_error{cli_error_kind::parse, "Invalid card text: " + std::string{text}});
            }
            return static_cast<card>(suit * 13 + rank);
        }

        [[nodiscard]] inline char rank_char(const uint8_t rank)
        {
            static constexpr std::array<char, 13> chars{'2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A'};
            return chars[rank];
        }

        [[nodiscard]] inline char suit_char(const uint8_t suit)
        {
            static constexpr std::array<char, 4> chars{'s', 'h', 'd', 'c'};
            return chars[suit];
        }

        [[nodiscard]] inline std::string card_text_from_id(const uint8_t id)
        {
            const auto rank = static_cast<uint8_t>(id % 13);
            const auto suit = static_cast<uint8_t>(id / 13);
            std::string out;
            out.push_back(rank_char(rank));
            out.push_back(suit_char(suit));
            return out;
        }

        [[nodiscard]] inline std::vector<std::string> card_texts_from_mask(card_mask mask)
        {
            std::vector<std::string> cards;
            for (uint8_t id = 0; id < 52; ++id) {
                if ((mask & (card_mask{1} << id)) != 0u) {
                    cards.push_back(card_text_from_id(id));
                }
            }
            return cards;
        }

        [[nodiscard]] inline std::string hand_text_from_combo(const combination_index combo)
        {
            const auto mask = combination_mask(combo);
            std::array<uint8_t, 2> cards{};
            std::size_t count = 0;
            for (uint8_t id = 0; id < 52; ++id) {
                if ((mask & (card_mask{1} << id)) != 0) {
                    assert(count < cards.size());
                    cards[count++] = id;
                }
            }
            assert(count == 2u);

            auto first = cards[0];
            auto second = cards[1];
            const auto first_rank = first % 13;
            const auto second_rank = second % 13;
            if (first_rank < second_rank || (first_rank == second_rank && first > second)) {
                std::swap(first, second);
            }
            return card_text_from_id(first) + card_text_from_id(second);
        }

        [[nodiscard]] inline std::string hand_class_text_from_combo(const combination_index combo)
        {
            const auto mask = combination_mask(combo);
            std::array<uint8_t, 2> cards{};
            std::size_t count = 0;
            for (uint8_t id = 0; id < 52; ++id) {
                if ((mask & (card_mask{1} << id)) != 0) {
                    assert(count < cards.size());
                    cards[count++] = id;
                }
            }
            assert(count == 2u);

            const auto first = cards[0];
            const auto second = cards[1];
            const auto first_rank = static_cast<uint8_t>(first % 13);
            const auto second_rank = static_cast<uint8_t>(second % 13);
            const auto first_suit = static_cast<uint8_t>(first / 13);
            const auto second_suit = static_cast<uint8_t>(second / 13);
            if (first_rank == second_rank) {
                std::string out;
                out.push_back(rank_char(first_rank));
                out.push_back(rank_char(first_rank));
                return out;
            }

            const auto high_rank = std::max(first_rank, second_rank);
            const auto low_rank = std::min(first_rank, second_rank);
            std::string out;
            out.push_back(rank_char(high_rank));
            out.push_back(rank_char(low_rank));
            out.push_back(first_suit == second_suit ? 's' : 'o');
            return out;
        }

        [[nodiscard]] inline std::expected<cfr::solver::holdem_street, cli_error> parse_holdem_street(const std::string_view text)
        {
            if (text == "flop") {
                return cfr::solver::holdem_street::flop;
            }
            if (text == "turn") {
                return cfr::solver::holdem_street::turn;
            }
            if (text == "river") {
                return cfr::solver::holdem_street::river;
            }
            return std::unexpected(cli_error{cli_error_kind::parse, "street must be one of: flop, turn, river."});
        }

        [[nodiscard]] inline std::size_t board_size_for_street(const cfr::solver::holdem_street street)
        {
            switch (street) {
                case cfr::solver::holdem_street::flop: return 3u;
                case cfr::solver::holdem_street::turn: return 4u;
                case cfr::solver::holdem_street::river: return 5u;
                case cfr::solver::holdem_street::invalid:
                case cfr::solver::holdem_street::preflop:
                    return 0u;
                default: return 0u;
            }
        }

        [[nodiscard]] inline std::expected<board, cli_error> board_from_cards(
            const std::vector<std::string>& cards,
            const cfr::solver::holdem_street street)
        {
            const auto expected_size = board_size_for_street(street);
            if (cards.size() != expected_size) {
                return std::unexpected(cli_error{
                    cli_error_kind::invalid_spot,
                    "Board card count does not match street."
                });
            }
            board river{};
            std::set<card> seen;
            for (const auto& text : cards) {
                auto parsed = parse_card_text(text);
                if (!parsed) {
                    return std::unexpected(parsed.error());
                }
                if (seen.contains(*parsed)) {
                    return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Duplicate board card: " + text});
                }
                seen.insert(*parsed);
                river.add(card_mask{1} << *parsed);
            }
            if (river.size() != 5) {
                if (street == cfr::solver::holdem_street::flop) {
                    if (river.size() != 3) {
                        return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Flop board must contain exactly 3 unique cards."});
                    }
                } else if (street == cfr::solver::holdem_street::turn) {
                    if (river.size() != 4) {
                        return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Turn board must contain exactly 4 unique cards."});
                    }
                } else {
                    return std::unexpected(cli_error{cli_error_kind::invalid_spot, "River board must contain exactly 5 unique cards."});
                }
            }
            return river;
        }

        /** The environment variable's value, or nullopt when it is not set. */
        [[nodiscard]] inline std::optional<std::string> environment_variable(const char* name)
        {
#if defined(_MSC_VER)
            char* value = nullptr;
            std::size_t size = 0;
            if (_dupenv_s(&value, &size, name) != 0 || value == nullptr) {
                return std::nullopt;
            }
            std::string out{value};
            std::free(value);
            return out;
#else
            const char* value = std::getenv(name);
            return value == nullptr ? std::nullopt : std::optional<std::string>{value};
#endif
        }

        [[nodiscard]] inline std::string now_utc_iso8601()
        {
            const auto now = std::chrono::system_clock::now();
            const auto time = std::chrono::system_clock::to_time_t(now);
            std::tm tm_utc{};
#if defined(_WIN32)
            gmtime_s(&tm_utc, &time);
#else
            gmtime_r(&time, &tm_utc);
#endif
            std::ostringstream os;
            os << std::put_time(&tm_utc, "%Y-%m-%dT%H:%M:%SZ");
            return os.str();
        }

        [[nodiscard]] inline std::string action_label(
            const cfr::betting_action& action,
            const utility gross_pot)
        {
            using enum cfr::betting_action_kind;
            switch (action.kind) {
                case fold: return "fold";
                case check: return "check";
                case call: return "call";
                case all_in: return "all_in";
                case bet:
                case raise: {
                    const auto safe_pot = std::max(1.0, gross_pot);
                    const auto pct = static_cast<int>(std::llround((action.amount / safe_pot) * 100.0));
                    return (action.kind == bet ? "bet_" : "raise_") + std::to_string(pct);
                }
            }
            return "action";
        }

        [[nodiscard]] inline std::string street_text(const cfr::solver::holdem_street street)
        {
            switch (street) {
                case cfr::solver::holdem_street::flop: return "flop";
                case cfr::solver::holdem_street::turn: return "turn";
                case cfr::solver::holdem_street::river: return "river";
                default: return "invalid";
            }
        }

        [[nodiscard]] inline std::string chance_kind_text(const cfr::public_chance_event_kind kind)
        {
            switch (kind) {
                case cfr::public_chance_event_kind::flop: return "flop";
                case cfr::public_chance_event_kind::turn: return "turn";
                case cfr::public_chance_event_kind::river: return "river";
                default: return "none";
            }
        }

        [[nodiscard]] inline std::expected<void, cli_error> parse_range_checked(
            const std::string_view text,
            hand_range& out,
            const char* label)
        {
            const auto parsed = parse_range(text);
            if (!parsed.ok()) {
                return std::unexpected(cli_error{
                    cli_error_kind::invalid_spot,
                    std::string{"Invalid "} + label + " range at position " + std::to_string(parsed.error.position)
                        + ": " + range_parse_error_text(parsed.error.code)
                });
            }
            out = parsed.range;
            return {};
        }

        [[nodiscard]] inline bool solve_cancel_requested(const solve_runtime_options& runtime)
        {
            return runtime.cancellation_requested && runtime.cancellation_requested();
        }

        /** A lock error, naming the lock by its 1-based position in the spot. */
        [[nodiscard]] inline cli_error lock_error(const std::size_t lock_index, const std::string& message)
        {
            return cli_error{cli_error_kind::invalid_spot,
                "Lock " + std::to_string(lock_index + 1u) + ": " + message};
        }

        [[nodiscard]] inline cli_error solve_cancelled_error()
        {
            return cli_error{cli_error_kind::cancelled, "Solve cancelled."};
        }

        inline void emit_progress(
            const solve_runtime_options& runtime,
            const solve_progress_stage stage,
            const uint64_t iterations_completed,
            const uint64_t total_iterations,
            const uint8_t updating_player,
            const uint8_t player_count,
            const double elapsed_ms,
            std::string message)
        {
            if (!runtime.progress_callback) {
                return;
            }
            runtime.progress_callback(solve_progress_event{
                .stage = stage,
                .iterations_completed = iterations_completed,
                .total_iterations = total_iterations,
                .updating_player = updating_player,
                .player_count = player_count,
                .elapsed_ms = elapsed_ms,
                .message = std::move(message)
            });
        }
    }

    [[nodiscard]] inline std::expected<std::string, cli_error> read_file_text(const std::filesystem::path& path)
    {
        std::ifstream input(path, std::ios::binary);
        if (!input) {
            return std::unexpected(cli_error{
                cli_error_kind::io,
                "Failed to open file: " + path.string()
            });
        }
        std::ostringstream buffer;
        buffer << input.rdbuf();
        if (!input.good() && !input.eof()) {
            return std::unexpected(cli_error{
                cli_error_kind::io,
                "Failed to read file: " + path.string()
            });
        }
        return buffer.str();
    }

    [[nodiscard]] inline std::expected<void, cli_error> write_file_text(
        const std::filesystem::path& path,
        const std::string_view text)
    {
        std::ofstream output(path, std::ios::binary | std::ios::trunc);
        if (!output) {
            return std::unexpected(cli_error{
                cli_error_kind::io,
                "Failed to open output file: " + path.string()
            });
        }
        output.write(text.data(), static_cast<std::streamsize>(text.size()));
        if (!output) {
            return std::unexpected(cli_error{
                cli_error_kind::io,
                "Failed to write output file: " + path.string()
            });
        }
        return {};
    }

    /** A spot amount that cannot be solved as written; `field` names the spot JSON field. */
    struct spot_amount_issue {
        std::string field;
        std::string message;
    };

    /**
     * Checks the spot's chip amounts: every number finite, stacks and contributions
     * non-negative, a positive pot, rake within it, and contributions adding up to
     * gross_pot - rake. The solver pays out exactly the contributions plus what is bet
     * in the tree, so a pot that disagrees with them would be solved as a different game.
     * Expects the per-seat arrays to be the player count; check sizes first.
     */
    [[nodiscard]] std::vector<spot_amount_issue> validate_spot_amounts(const solve_spot& spot);

    /**
     * Checks each lock's fields on their own (the tree is not needed) and puts it in
     * canonical form: frequencies rounded to node_lock_frequency_step, actions sorted
     * by label, chance-step cards in canonical text. Errors name the lock by its
     * 1-based position. The path itself is checked against the tree when solving.
     */
    [[nodiscard]] std::expected<void, cli_error> normalize_node_locks(std::vector<node_lock>& locks);

    [[nodiscard]] std::expected<solve_spot, cli_error> parse_spot_json(std::string_view json);
    [[nodiscard]] std::expected<solve_runtime_options, cli_error> parse_spot_runtime_options(std::string_view json);
    [[nodiscard]] std::string serialize_spot_json(const solve_spot& spot);
    [[nodiscard]] std::expected<solve_artifact, cli_error> parse_artifact_json(std::string_view json);
    [[nodiscard]] std::string serialize_artifact_json(
        const solve_artifact& artifact,
        solve_artifact_export_mode mode = solve_artifact_export_mode::standard);

    [[nodiscard]] inline std::expected<void, cli_error> validate_artifact(const solve_artifact& artifact)
    {
        const auto validate_action_distribution = [](std::span<const action_strategy> actions, const std::string& label) -> std::expected<void, cli_error> {
            if (actions.empty()) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, label + " must contain at least one action."});
            }

            double sum = 0.0;
            for (const auto& action : actions) {
                if (!std::isfinite(action.frequency) || action.frequency < 0.0 || action.frequency > 1.0) {
                    return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Action frequency out of [0,1] for " + label});
                }
                sum += action.frequency;
            }
            if (std::abs(sum - 1.0) > 1.0e-3) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Action frequencies must sum to 1 for " + label});
            }
            return {};
        };

        if (artifact.schema_version != current_artifact_schema_version) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact,
                "Unsupported schema_version. Only version " + std::to_string(current_artifact_schema_version) + " is accepted."});
        }
        for (const auto& node : artifact.solved_nodes) {
            for (const auto index : node.lock_indices) {
                if (index >= artifact.solver.locks.size()) {
                    return std::unexpected(cli_error{cli_error_kind::invalid_artifact,
                        "solved_nodes.lock_indices refers to a lock that solver.locks does not contain."});
                }
            }
        }
        if (artifact.extraction_version != current_extraction_version) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact,
                "Unsupported extraction_version. Only version " + std::to_string(current_extraction_version) + " is accepted."});
        }
        if (artifact.game != "holdem") {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "game must be \"holdem\"."});
        }
        auto parsed_street = detail::parse_holdem_street(artifact.street);
        if (!parsed_street) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, parsed_street.error().message});
        }
        if (artifact.players.size() < cli_min_players || artifact.players.size() > cli_max_players) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "players must contain between 2 and 7 labels."});
        }
        if (artifact.hero_seat >= artifact.players.size()) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "hero_seat is out of range."});
        }
        auto board_result = detail::board_from_cards(artifact.board, *parsed_street);
        if (!board_result) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, board_result.error().message});
        }
        if (!artifact.root_strategy.empty()) {
            if (auto result = validate_action_distribution(artifact.root_strategy, "root_strategy"); !result) {
                return result;
            }
        }

        std::set<uint32_t> public_state_ids;
        for (const auto& state : artifact.public_states) {
            if (state.id == cfr::INVALID_PUBLIC_STATE_ID) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "public_states contains an invalid id."});
            }
            if (!public_state_ids.insert(state.id).second) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "public_states contains a duplicate id."});
            }
        }

        std::set<uint32_t> chance_event_ids;
        for (const auto& event : artifact.chance_events) {
            if (event.id == cfr::INVALID_CHANCE_EVENT) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "chance_events contains an invalid id."});
            }
            if (!chance_event_ids.insert(event.id).second) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "chance_events contains a duplicate id."});
            }
        }

        std::set<uint32_t> runout_ids;
        for (const auto& runout : artifact.runouts) {
            if (runout.id == cfr::INVALID_RUNOUT_ID) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "runouts contains an invalid id."});
            }
            if (!runout_ids.insert(runout.id).second) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "runouts contains a duplicate id."});
            }
        }

        std::set<uint32_t> solved_node_ids;
        for (const auto& node : artifact.solved_nodes) {
            if (node.node_id == cfr::game_graph::INVALID_NODE) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "solved_nodes contains an invalid node id."});
            }
            if (!solved_node_ids.insert(node.node_id).second) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "solved_nodes contains a duplicate node id."});
            }
            if (!node.range_action_frequencies.empty()) {
                if (auto result = validate_action_distribution(node.range_action_frequencies, "solved node range_action_frequencies"); !result) {
                    return result;
                }
            }
            if (node.seat_values.empty()) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "solved_nodes.seat_values must not be empty."});
            }
            std::set<uint8_t> solved_node_seats;
            for (const auto& seat_value : node.seat_values) {
                if (seat_value.seat >= artifact.players.size()) {
                    return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "solved_nodes.seat_values contains an out-of-range seat."});
                }
                if (!solved_node_seats.insert(seat_value.seat).second) {
                    return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "solved_nodes.seat_values contains duplicate seat entries."});
                }
                if (!std::isfinite(seat_value.range_reach_mass) || seat_value.range_reach_mass < 0.0) {
                    return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "solved_nodes.seat_values.range_reach_mass must be finite and non-negative."});
                }
                if (!std::isfinite(seat_value.matchup_reach_mass) || seat_value.matchup_reach_mass < 0.0) {
                    return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "solved_nodes.seat_values.matchup_reach_mass must be finite and non-negative."});
                }
                if (!std::isfinite(seat_value.reach_weighted_value)
                    || !std::isfinite(seat_value.conditional_range_ev)
                    || !std::isfinite(seat_value.counterfactual_value)) {
                    return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "solved_nodes.seat_values metrics must be finite."});
                }
                const auto reconstructed = seat_value.matchup_reach_mass > minimum_ev_reach_mass
                    ? seat_value.reach_weighted_value / seat_value.matchup_reach_mass
                    : 0.0;
                if (std::abs(reconstructed - seat_value.conditional_range_ev) > 1.0e-5 * std::max(1.0, std::abs(reconstructed))) {
                    return std::unexpected(cli_error{
                        cli_error_kind::invalid_artifact,
                        "solved_nodes.seat_values.conditional_range_ev must equal reach_weighted_value / matchup_reach_mass."
                    });
                }
            }
            if (node.category_summary_derivation_version != 1u) {
                return std::unexpected(cli_error{
                    cli_error_kind::invalid_artifact,
                    "solved_nodes.derived.category_summaries.derivation_version must be 1."
                });
            }
            for (const auto& item : node.category_summaries) {
                if (item.category_name.empty()) {
                    return std::unexpected(cli_error{
                        cli_error_kind::invalid_artifact,
                        "solved_nodes.derived.category_summaries.items.category_name must not be empty."
                    });
                }
                if (!std::isfinite(item.frequency) || item.frequency < 0.0 || item.frequency > 1.0) {
                    return std::unexpected(cli_error{
                        cli_error_kind::invalid_artifact,
                        "solved_nodes.derived.category_summaries.items.frequency must be finite and in [0,1]."
                    });
                }
                if (!std::isfinite(item.range_weight) || item.range_weight < 0.0 || !std::isfinite(item.average_ev)) {
                    return std::unexpected(cli_error{
                        cli_error_kind::invalid_artifact,
                        "solved_nodes.derived.category_summaries.items metrics must be finite and non-negative where applicable."
                    });
                }
                for (const auto& action : item.action_frequencies) {
                    if (!std::isfinite(action.frequency) || action.frequency < 0.0 || action.frequency > 1.0) {
                        return std::unexpected(cli_error{
                            cli_error_kind::invalid_artifact,
                            "solved_nodes.derived.category_summaries.items.action_frequencies frequency must be finite and in [0,1]."
                        });
                    }
                }
            }
            for (const auto& row : node.strategy_rows) {
                if (!row.strategy.empty()) {
                    if (auto result = validate_action_distribution(row.strategy, "solved node combo strategy"); !result) {
                        return result;
                    }
                }
                if (!std::isfinite(row.range_weight) || row.range_weight < 0.0) {
                    return std::unexpected(cli_error{
                        cli_error_kind::invalid_artifact,
                        "solved_nodes.strategy_rows.range_weight must be finite and non-negative."
                    });
                }
                if (!std::isfinite(row.reach_probability) || row.reach_probability < 0.0 || row.reach_probability > 1.0) {
                    return std::unexpected(cli_error{
                        cli_error_kind::invalid_artifact,
                        "solved_nodes.strategy_rows.reach_probability must be finite and in [0,1]."
                    });
                }
            }
        }

        if (!artifact.solved_nodes.empty()) {
            if (artifact.root_node_id == cfr::game_graph::INVALID_NODE
                || solved_node_ids.find(artifact.root_node_id) == solved_node_ids.end()) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact,
                    "root_node_id must reference an existing solved node."});
            }
            const auto root_it = std::ranges::find_if(artifact.solved_nodes, [&](const auto& node) {
                return node.node_id == artifact.root_node_id;
            });
            if (root_it->parent_node_id != cfr::game_graph::INVALID_NODE) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact,
                    "root_node_id must reference the parentless root node."});
            }
        }

        std::set<std::string> seen_hands;
        for (const auto& row : artifact.strategy) {
            if (row.strategy.empty() && artifact.root_strategy.empty()) {
                return std::unexpected(cli_error{
                    cli_error_kind::invalid_artifact,
                    "Strategy row must contain at least one action when root_strategy is absent."
                });
            }
            if (!seen_hands.insert(row.hand).second) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Duplicate hand entry: " + row.hand});
            }
            const auto parsed_hand = parse_range(row.hand);
            if (!parsed_hand.ok()) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Invalid hand text: " + row.hand});
            }
            std::size_t non_zero = 0;
            combination_index parsed_combo = 0;
            for (combination_index combo = 0; combo < combination_count; ++combo) {
                const auto weight = parsed_hand.range.weights[combo];
                if (weight != 0.0f) {
                    parsed_combo = combo;
                    ++non_zero;
                }
            }
            if (non_zero != 1u) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Hand must represent exactly one combo: " + row.hand});
            }
            if (row.combination_index >= combination_count || row.combination_index != parsed_combo) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "combination_index does not match hand: " + row.hand});
            }

            if (!row.strategy.empty()) {
                if (auto result = validate_action_distribution(row.strategy, "hand: " + row.hand); !result) {
                    return result;
                }
            }
            if (!std::isfinite(row.range_weight) || row.range_weight < 0.0) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "range_weight must be finite and non-negative for hand: " + row.hand});
            }
            if (!std::isfinite(row.reach_probability) || row.reach_probability < 0.0 || row.reach_probability > 1.0) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "reach_probability must be finite and in [0,1] for hand: " + row.hand});
            }
            if (!std::isfinite(row.ev) || !std::isfinite(row.counterfactual_value)) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "EV must be finite for hand: " + row.hand});
            }
            if (!std::isfinite(row.opponent_reach_mass) || row.opponent_reach_mass < 0.0) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "opponent_reach_mass must be finite and non-negative for hand: " + row.hand});
            }
            const auto expected_cfv = row.ev * row.opponent_reach_mass;
            if (std::abs(expected_cfv - row.counterfactual_value) > 1.0e-6 * std::max(1.0, std::abs(row.counterfactual_value))) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "ev * opponent_reach_mass must equal counterfactual_value for hand: " + row.hand});
            }
        }

        return {};
    }

    /**
     * Solves the spot: validates it, resolves its node locks, lowers the game tree and
     * runs combo-vectorised CFR+ (cli/combo_cfr.h). Fails with invalid_spot for bad
     * input, solver when the tree cannot be built or does not fit the memory budget, and
     * cancelled when runtime.cancellation_requested fires.
     */
    [[nodiscard]] std::expected<solve_output, cli_error> solve_spot(
        const struct solve_spot& spot,
        uint64_t iterations,
        const solve_runtime_options& runtime = {});

    /**
     * The lock path that reaches `node_id` in a solved artifact: each player step's
     * action label and each chance step's dealt card. Empty for the root; nullopt when
     * the node is not in the artifact.
     */
    [[nodiscard]] std::optional<std::vector<std::string>> node_lock_path(
        const solve_artifact& artifact,
        uint32_t node_id);

    /** Text table of the root strategy and per-hand EVs, as `zeta-solve dump` prints it. */
    [[nodiscard]] std::string format_dump(const solve_artifact& artifact);

}
