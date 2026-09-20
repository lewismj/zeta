#pragma once

#include "board.h"
#include "cfr/betting/betting.h"
#include "cfr/scheduler/scheduler.h"
#include "cfr/solver/iteration.h"
#include "range_parser.h"
#include "terminal.h"

#include <algorithm>
#include <array>
#include <atomic>
#include <cassert>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <fstream>
#include <functional>
#include <iomanip>
#include <ios>
#include <iterator>
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
        std::string hand;
        std::vector<action_strategy> strategy;
        double ev = 0.0;
    };

    struct solver_metadata {
        std::string algorithm = "cfr+";
        uint64_t iterations = 0;
        std::string timestamp;
        std::string git_revision = "unknown";
    };

    inline constexpr uint32_t current_artifact_schema_version = 3;

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
        std::vector<action_strategy> strategy;
        std::vector<hand_strategy> strategy_rows;
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
        uint16_t samples_per_combo = 64;
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

    struct solve_runtime_options {
        std::string timestamp_utc;
        std::string git_revision = "unknown";
        uint64_t progress_batch_iterations = 1;
        uint32_t worker_threads = 1;
        solve_progress_callback progress_callback;
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

        [[nodiscard]] inline std::vector<card> board_cards_from_mask(card_mask mask)
        {
            std::vector<card> cards;
            cards.reserve(5);
            for (uint8_t id = 0; id < 52; ++id) {
                if ((mask & (card_mask{1} << id)) != 0) {
                    cards.push_back(id);
                }
            }
            return cards;
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
                    std::string{"Invalid "} + label + " range."
                });
            }
            out = parsed.range;
            return {};
        }

        [[nodiscard]] inline bool solve_cancel_requested(const solve_runtime_options& runtime)
        {
            return runtime.cancellation_requested && runtime.cancellation_requested();
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

    [[nodiscard]] std::expected<solve_spot, cli_error> parse_spot_json(std::string_view json);
    [[nodiscard]] std::string serialize_spot_json(const solve_spot& spot);
    [[nodiscard]] std::expected<solve_artifact, cli_error> parse_artifact_json(std::string_view json);
    [[nodiscard]] std::string serialize_artifact_json(const solve_artifact& artifact);

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
            if (!node.strategy.empty()) {
                if (auto result = validate_action_distribution(node.strategy, "solved node strategy"); !result) {
                    return result;
                }
            }
            for (const auto& row : node.strategy_rows) {
                if (!row.strategy.empty()) {
                    if (auto result = validate_action_distribution(row.strategy, "solved node combo strategy"); !result) {
                        return result;
                    }
                }
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
            for (const auto weight : parsed_hand.range.weights) {
                if (weight != 0.0f) {
                    ++non_zero;
                }
            }
            if (non_zero != 1u) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Hand must represent exactly one combo: " + row.hand});
            }

            if (!row.strategy.empty()) {
                if (auto result = validate_action_distribution(row.strategy, "hand: " + row.hand); !result) {
                    return result;
                }
            }
            if (!std::isfinite(row.ev)) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "EV must be finite for hand: " + row.hand});
            }
        }

        return {};
    }

    namespace detail {

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

        template <std::size_t N>
        inline void normalize_combo_action_table(
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const combo_action_table& source,
            combo_action_table& destination,
            const std::array<std::vector<combination_index>, N>& active_combos)
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
                const auto uniform = graph.action_count(node_id) == 0u ? 0.0f : 1.0f / static_cast<float>(graph.action_count(node_id));
                for (const auto combo : active_combos[actor]) {
                    const auto src = source.combo_infoset(combo, infoset_id);
                    auto dst = destination.combo_infoset(combo, infoset_id);
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
            }
        }

        template <std::size_t N>
        inline void build_node_reach_vectors(
            const cfr::game_graph& graph,
            const cfr::solver::solver_graph_annotations& annotations,
            const combo_action_table& strategy_profile,
            const std::array<reach_vector, N>& root_reach,
            const std::array<std::vector<combination_index>, N>& active_combos,
            std::vector<std::array<reach_vector, N>>& node_reach)
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
                for (const auto& edge : graph.out_edges(node_id)) {
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

        template <std::size_t N>
        inline void evaluate_combo_profile(
            const cfr::holdem_betting_graph<N>& lowered,
            const combo_action_table& strategy_profile,
            const std::array<reach_vector, N>& base_reach,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const std::vector<std::array<reach_vector, N>>& node_reach,
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& base_indices,
            const uint8_t updating_player,
            std::vector<reach_vector>& node_values,
            const uint16_t samples_per_combo)
        {
            node_values.assign(lowered.graph.node_count, {});
            terminal_workspace<N> workspace{};
            const terminal_engine<N> engine{};
            for (uint32_t node_id = 0; node_id < lowered.graph.node_count; ++node_id) {
                const auto kind = lowered.graph.node_types[node_id];
                if (kind == cfr::node_kind::terminal) {
                    const auto terminal_state_id = lowered.terminal_leaves[node_id].terminal_state_id;
                    terminal_values<N> values{};
                    if constexpr (N == 2) {
                        const auto opponent = static_cast<uint8_t>(1u - updating_player);
                        const auto opponent_index = make_river_reach_index(cache, node_reach[node_id][opponent]);
                        if (updating_player == 0u) {
                            values = engine.evaluate_terminal_values(
                                cache,
                                base_indices[0],
                                opponent_index,
                                lowered.terminal_states[terminal_state_id],
                                samples_per_combo);
                        } else {
                            values = engine.evaluate_terminal_values(
                                cache,
                                opponent_index,
                                base_indices[1],
                                lowered.terminal_states[terminal_state_id],
                                samples_per_combo);
                        }
                    } else {
                        auto terminal_reach = node_reach[node_id];
                        terminal_reach[updating_player] = base_reach[updating_player];
                        workspace.materialize(cache, terminal_reach);
                        values = engine.evaluate_terminal_values(
                            cache,
                            workspace.reach,
                            lowered.terminal_states[terminal_state_id],
                            samples_per_combo);
                    }
                    for (const auto combo : active_combos[updating_player]) {
                        node_values[node_id][combo] = values[updating_player][combo];
                    }
                    continue;
                }

                const auto edges = lowered.graph.out_edges(node_id);
                if (kind == cfr::node_kind::chance) {
                    for (const auto& edge : edges) {
                        for (const auto combo : active_combos[updating_player]) {
                            node_values[node_id][combo] += node_values[edge.child_node][combo];
                        }
                    }
                    continue;
                }

                const auto actor = lowered.annotations.actor_by_node[node_id];
                const auto infoset_id = lowered.graph.infoset_id[node_id];
                if (actor == updating_player) {
                    for (const auto combo : active_combos[updating_player]) {
                        float total = 0.0f;
                        for (const auto& edge : edges) {
                            total += strategy_profile.value(combo, infoset_id, edge.action_index)
                                * node_values[edge.child_node][combo];
                        }
                        node_values[node_id][combo] = total;
                    }
                } else {
                    for (const auto& edge : edges) {
                        for (const auto combo : active_combos[updating_player]) {
                            node_values[node_id][combo] += node_values[edge.child_node][combo];
                        }
                    }
                }
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

        // Back up counterfactual values for the updating player over the exact
        // multi-street public game. Terminal leaves dispatch to the per-river terminal
        // cache (showdown) or a blocker-aware fold evaluation (any street); chance nodes
        // weight each child by its outcome probability; player nodes fold in the current
        // strategy for the updating player and sum for the opponent.
        template <std::size_t N>
        inline void evaluate_multi_street_combo_profile(
            const cfr::holdem_public_game_graph<N>& lowered,
            const combo_action_table& strategy_profile,
            const std::array<std::vector<combination_index>, N>& active_combos,
            const std::vector<std::array<reach_vector, N>>& node_reach,
            const cfr::runout_terminal_table& terminal_table,
            const std::vector<std::array<river_reach_index, N>>& river_base_index_by_state,
            const uint8_t updating_player,
            std::vector<reach_vector>& node_values,
            const uint16_t samples_per_combo)
        {
            static_assert(N == 2, "multi-street public-game CFR is heads-up only");
            node_values.assign(lowered.graph.node_count, {});
            const terminal_engine<N> engine{};
            const auto opponent = static_cast<uint8_t>(1u - updating_player);
            for (uint32_t node_id = 0; node_id < lowered.graph.node_count; ++node_id) {
                const auto kind = lowered.graph.node_types[node_id];
                if (kind == cfr::node_kind::terminal) {
                    const auto state_id = lowered.annotations.state_by_node[node_id].public_state_id;
                    const auto terminal_state_id = lowered.terminal_leaves[node_id].terminal_state_id;
                    const auto& terminal_state = lowered.terminal_states[terminal_state_id];
                    if (terminal_state.kind == terminal_state_kind::showdown) {
                        const auto* entry = terminal_table.find(state_id);
                        assert(entry != nullptr);
                        const auto opponent_index = make_river_reach_index(entry->cache, node_reach[node_id][opponent]);
                        const auto values = updating_player == 0u
                            ? engine.evaluate_terminal_values(
                                entry->cache, river_base_index_by_state[state_id][0], opponent_index,
                                terminal_state, samples_per_combo)
                            : engine.evaluate_terminal_values(
                                entry->cache, opponent_index, river_base_index_by_state[state_id][1],
                                terminal_state, samples_per_combo);
                        for (const auto combo : active_combos[updating_player]) {
                            node_values[node_id][combo] = values[updating_player][combo];
                        }
                    } else {
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
                            node_values[node_id][combo] = static_cast<float>(
                                compatible * payoff[updating_player]);
                        }
                    }
                    continue;
                }

                const auto edges = lowered.graph.out_edges(node_id);
                if (kind == cfr::node_kind::chance) {
                    for (const auto& edge : edges) {
                        const auto probability = lowered.chance_events.probability_for_edge(node_id, edge);
                        for (const auto combo : active_combos[updating_player]) {
                            node_values[node_id][combo] += probability * node_values[edge.child_node][combo];
                        }
                    }
                    continue;
                }

                const auto actor = lowered.annotations.actor_by_node[node_id];
                const auto infoset_id = lowered.graph.infoset_id[node_id];
                if (actor == updating_player) {
                    for (const auto combo : active_combos[updating_player]) {
                        float total = 0.0f;
                        for (const auto& edge : edges) {
                            total += strategy_profile.value(combo, infoset_id, edge.action_index)
                                * node_values[edge.child_node][combo];
                        }
                        node_values[node_id][combo] = total;
                    }
                } else {
                    for (const auto& edge : edges) {
                        for (const auto combo : active_combos[updating_player]) {
                            node_values[node_id][combo] += node_values[edge.child_node][combo];
                        }
                    }
                }
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
            combo_action_table& strategy_sums)
        {
            for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
                if (!graph.is_player_node(node_id)) {
                    continue;
                }
                const auto actor = annotations.actor_by_node[node_id];
                if (actor != updating_player) {
                    continue;
                }
                const auto infoset_id = graph.infoset_id[node_id];
                const auto edges = graph.out_edges(node_id);
                for (const auto combo : active_combos[updating_player]) {
                    const auto node_value = node_values[node_id][combo];
                    const auto path_weight = node_reach[node_id][updating_player][combo];
                    for (const auto& edge : edges) {
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

        [[nodiscard]] inline std::vector<action_strategy> aggregate_root_strategy_for_actor(
            const combo_action_table& average_strategy,
            const uint32_t infoset_id,
            const std::vector<cfr::betting_action>& root_actions,
            const reach_vector& actor_reach,
            const utility gross_pot)
        {
            std::vector<action_strategy> root_strategy;
            root_strategy.reserve(root_actions.size());
            double total_weight = 0.0;
            for (combination_index combo = 0; combo < combination_count; ++combo) {
                total_weight += std::max(0.0f, actor_reach[combo]);
            }
            const auto uniform = root_actions.empty() ? 0.0 : 1.0 / static_cast<double>(root_actions.size());
            for (std::size_t action_index = 0; action_index < root_actions.size(); ++action_index) {
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
                    .action = action_label(root_actions[action_index], gross_pot),
                    .frequency = total_weight > 0.0 ? weighted_total / total_weight : uniform
                });
            }
            return root_strategy;
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
                        persisted.strategy = *strategy;
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
        }

        // Runs the unified heads-up CFR+ solve over the exact multi-street public game
        // (flop/turn). CFR+ traverses the interleaved betting/chance tree across every
        // street; terminal leaves dispatch to the per-river terminal cache (showdown) or a
        // blocker-aware fold evaluation (any street), keeping the expensive hand evaluation
        // outside the regret-matching hot loop. Heads-up only; higher seat counts are
        // rejected because the exact terminal kernels are defined for two players.
        template <std::size_t N>
        [[nodiscard]] inline std::expected<void, cli_error> solve_multi_street_public_game(
            const solve_spot& spot,
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

                combo_action_table regrets(layout);
                combo_action_table strategy_sums(layout);
                combo_action_table current_strategy(layout);
                combo_action_table average_strategy(layout);
                const auto active_combos = active_combos_by_player(reach_vectors);

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
                for (uint64_t i = 0; i < iterations; ++i) {
                    if (solve_cancel_requested(runtime)) {
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
                                "Multi-street CFR+ player update.");
                        }
                        normalize_combo_action_table(graph, annotations, regrets, current_strategy, active_combos);
                        build_node_reach_vectors(graph, annotations, current_strategy, reach_vectors, active_combos, node_reach);
                        evaluate_multi_street_combo_profile(
                            public_game,
                            current_strategy,
                            active_combos,
                            node_reach,
                            terminal_table,
                            river_base_index_by_state,
                            updating_player,
                            node_values,
                            spot.samples_per_combo);
                        update_combo_cfr_tables(
                            graph,
                            annotations,
                            current_strategy,
                            active_combos,
                            updating_player,
                            node_reach,
                            node_values,
                            regrets,
                            strategy_sums);
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

                if (solve_cancel_requested(runtime)) {
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

                normalize_combo_action_table(graph, annotations, strategy_sums, average_strategy, active_combos);
                build_node_reach_vectors(graph, annotations, average_strategy, reach_vectors, active_combos, node_reach);
                const auto hero = static_cast<uint8_t>(spot.hero_seat);
                evaluate_multi_street_combo_profile(
                    public_game,
                    average_strategy,
                    active_combos,
                    node_reach,
                    terminal_table,
                    river_base_index_by_state,
                    hero,
                    node_values,
                    spot.samples_per_combo);

                solve_artifact artifact{};
                artifact.players.assign(spot.players.begin(), std::next(spot.players.begin(), N));
                artifact.board = spot.board;
                artifact.street = spot.street;
                artifact.hero_seat = spot.hero_seat;
                artifact.solver.algorithm = "cfr+";
                artifact.solver.iterations = iterations;
                artifact.solver.timestamp = runtime.timestamp_utc.empty() ? now_utc_iso8601() : runtime.timestamp_utc;
                artifact.solver.git_revision = runtime.git_revision;
                artifact.root_strategy = aggregate_root_strategy_for_actor(
                    average_strategy,
                    root_infoset,
                    root_actions,
                    reach_vectors[hero],
                    spot.gross_pot);

                for (const auto combo : active_combos[hero]) {
                    std::vector<action_strategy> combo_strategy;
                    combo_strategy.reserve(root_actions.size());
                    for (std::size_t action_index = 0; action_index < root_actions.size(); ++action_index) {
                        combo_strategy.push_back(action_strategy{
                            .action = artifact.root_strategy[action_index].action,
                            .frequency = average_strategy.value(combo, root_infoset, static_cast<uint32_t>(action_index))
                        });
                    }
                    artifact.strategy.push_back(hand_strategy{
                        .hand = hand_text_from_combo(combo),
                        .strategy = std::move(combo_strategy),
                        .ev = node_values[graph.root_node][combo]
                    });
                }

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
                            .action = (node_id == graph.root_node && action_index < artifact.root_strategy.size())
                                ? artifact.root_strategy[action_index].action
                                : std::to_string(action_index),
                            .frequency = total > 0.0
                                ? weighted[action_index] / total
                                : (edges.empty() ? 0.0 : 1.0 / static_cast<double>(edges.size()))
                        });
                    }
                }

                append_solved_graph_payload(
                    artifact,
                    graph,
                    annotations,
                    &public_game.public_states,
                    &public_game.chance_events,
                    &public_game.runouts,
                    node_strategies);
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

        template <std::size_t N>
        [[nodiscard]] inline std::expected<bool, cli_error> solve_vectorized_root_actor_river(
            const solve_spot& spot,
            const uint64_t iterations,
            const solve_runtime_options& runtime,
            const cfr::holdem_betting_graph<N>& lowered,
            const cfr::action_table_layout& layout,
            const cfr::holdem_betting_graph_config<N>& config,
            const board public_board,
            const std::array<reach_vector, N>& reach_vectors,
            solve_output& output)
        {
            if constexpr (N != 2) {
                (void)spot;
                (void)iterations;
                (void)runtime;
                (void)lowered;
                (void)layout;
                (void)config;
                (void)public_board;
                (void)reach_vectors;
                (void)output;
                return false;
            }

            if (spot.hero_seat != spot.root_actor) {
                return false;
            }
            if (spot.street != "river") {
                return false;
            }

            combo_action_table regrets(layout);
            combo_action_table strategy_sums(layout);
            combo_action_table current_strategy(layout);
            combo_action_table average_strategy(layout);
            const auto active_combos = active_combos_by_player(reach_vectors);
            const auto cache = make_river_terminal_cache(public_board);
            std::array<river_reach_index, N> base_indices{};
            for (std::size_t seat = 0; seat < N; ++seat) {
                base_indices[seat] = make_river_reach_index(cache, reach_vectors[seat]);
            }

            auto initial_state = cfr::make_initial_betting_state(config);
            const auto root_actions = cfr::legal_betting_actions(initial_state, config.abstraction);
            const auto root_infoset = lowered.graph.infoset_id[lowered.graph.root_node];
            if (root_actions.empty() || root_actions.size() != layout.action_count(root_infoset)) {
                return std::unexpected(cli_error{cli_error_kind::solver, "Root action shape does not match strategy table shape."});
            }

            const auto iter_begin = std::chrono::steady_clock::now();
            const auto progress_batch = std::max<uint64_t>(1, runtime.progress_batch_iterations);
            std::vector<std::array<reach_vector, N>> node_reach;
            std::vector<reach_vector> node_values;
            for (uint64_t i = 0; i < iterations; ++i) {
                if (solve_cancel_requested(runtime)) {
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
                            "Vectorized CFR player update.");
                    }
                    normalize_combo_action_table(lowered.graph, lowered.annotations, regrets, current_strategy, active_combos);
                    build_node_reach_vectors(lowered.graph, lowered.annotations, current_strategy, reach_vectors, active_combos, node_reach);
                    evaluate_combo_profile(
                        lowered,
                        current_strategy,
                        reach_vectors,
                        active_combos,
                        node_reach,
                        cache,
                        base_indices,
                        updating_player,
                        node_values,
                        spot.samples_per_combo);
                    update_combo_cfr_tables(
                        lowered.graph,
                        lowered.annotations,
                        current_strategy,
                        active_combos,
                        updating_player,
                        node_reach,
                        node_values,
                        regrets,
                        strategy_sums);
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
                        "Vectorized CFR iteration batch complete.");
                }
            }
            output.timing.cfr_iterations_ms = std::chrono::duration<double, std::milli>(
                std::chrono::steady_clock::now() - iter_begin).count();

            if (solve_cancel_requested(runtime)) {
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

            normalize_combo_action_table(lowered.graph, lowered.annotations, strategy_sums, average_strategy, active_combos);
            build_node_reach_vectors(lowered.graph, lowered.annotations, average_strategy, reach_vectors, active_combos, node_reach);
            const auto hero = static_cast<uint8_t>(spot.hero_seat);
            evaluate_combo_profile(
                lowered,
                average_strategy,
                reach_vectors,
                active_combos,
                node_reach,
                cache,
                base_indices,
                hero,
                node_values,
                spot.samples_per_combo);

            solve_artifact artifact{};
            artifact.players.assign(spot.players.begin(), std::next(spot.players.begin(), N));
            artifact.board = spot.board;
            artifact.street = spot.street;
            artifact.hero_seat = spot.hero_seat;
            artifact.solver.algorithm = "cfr+";
            artifact.solver.iterations = iterations;
            artifact.solver.timestamp = runtime.timestamp_utc.empty() ? now_utc_iso8601() : runtime.timestamp_utc;
            artifact.solver.git_revision = runtime.git_revision;
            artifact.root_strategy = aggregate_root_strategy_for_actor(
                average_strategy,
                root_infoset,
                root_actions,
                reach_vectors[hero],
                spot.gross_pot);

            for (const auto combo : active_combos[hero]) {
                std::vector<action_strategy> combo_strategy;
                combo_strategy.reserve(root_actions.size());
                for (std::size_t action_index = 0; action_index < root_actions.size(); ++action_index) {
                    combo_strategy.push_back(action_strategy{
                        .action = artifact.root_strategy[action_index].action,
                        .frequency = average_strategy.value(combo, root_infoset, static_cast<uint32_t>(action_index))
                    });
                }
                artifact.strategy.push_back(hand_strategy{
                    .hand = hand_text_from_combo(combo),
                    .strategy = std::move(combo_strategy),
                    .ev = node_values[lowered.graph.root_node][combo]
                });
            }
            std::vector<std::vector<action_strategy>> node_strategies(lowered.graph.node_count);
            for (uint32_t node_id = 0; node_id < lowered.graph.node_count; ++node_id) {
                if (!lowered.graph.is_player_node(node_id)) {
                    continue;
                }
                const auto actor = lowered.annotations.actor_by_node[node_id];
                const auto infoset_id = lowered.graph.infoset_id[node_id];
                const auto edges = lowered.graph.out_edges(node_id);
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
                        .action = (node_id == lowered.graph.root_node && action_index < artifact.root_strategy.size())
                            ? artifact.root_strategy[action_index].action
                            : std::to_string(action_index),
                        .frequency = total > 0.0
                            ? weighted[action_index] / total
                            : (edges.empty() ? 0.0 : 1.0 / static_cast<double>(edges.size()))
                    });
                }
            }
            append_solved_graph_payload(
                artifact,
                lowered.graph,
                lowered.annotations,
                nullptr,
                nullptr,
                nullptr,
                node_strategies);
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
            return true;
        }

        template <std::size_t N>
        [[nodiscard]] inline std::expected<solve_output, cli_error> solve_spot_impl(
            const solve_spot& spot,
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
                auto lowered_public = cfr::lower_multi_street_public_game(public_config);
                if (!lowered_public) {
                    return std::unexpected(cli_error{
                        cli_error_kind::solver,
                        "Failed to lower the multi-street public game graph from the input spot."
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
            cfr::holdem_betting_graph<N> lowered_storage = std::move(*lowered_result);
            cfr::game_graph* lowered_graph = &lowered_storage.graph;
            cfr::solver::solver_graph_annotations* lowered_annotations = &lowered_storage.annotations;
            std::span<const cfr::solver::cfr_terminal_leaf> lowered_terminal_leaves{lowered_storage.terminal_leaves};
            std::span<const ::zeta::holdem::terminal_state<N>> lowered_terminal_states{lowered_storage.terminal_states.states};
            auto layout_result = cfr::make_action_table_layout(*lowered_graph);
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
            if (auto vectorized = solve_vectorized_root_actor_river(
                    spot,
                    iterations,
                    runtime,
                    lowered_storage,
                    *layout_result,
                    config,
                    public_board,
                    reach_vectors,
                    output);
                !vectorized) {
                return std::unexpected(vectorized.error());
            } else if (*vectorized) {
                return output;
            }
            cfr::regret_table regrets(*layout_result);
            cfr::strategy_sum_table strategy_sums(*layout_result);
            auto context = cfr::solver::make_cfr_solver_context<N>(
                *lowered_graph,
                *lowered_annotations,
                *layout_result,
                regrets,
                strategy_sums);

            if (solve_cancel_requested(runtime)) {
                return std::unexpected(solve_cancelled_error());
            }

            const auto iter_begin = std::chrono::steady_clock::now();
            std::vector<cfr::traversal::worker_context> workers{
                std::clamp<uint32_t>(runtime.worker_threads, 1u, 64u)};
            const auto progress_batch = std::max<uint64_t>(1, runtime.progress_batch_iterations);
            for (uint64_t i = 0; i < iterations; ++i) {
                if (solve_cancel_requested(runtime)) {
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
                            "CFR player update.");
                    }
                    auto terminal_state = terminal_context<N>{};
                    terminal_state.gross_pot = spot.gross_pot;
                    terminal_state.rake = spot.rake;
                    for (std::size_t seat = 0; seat < N; ++seat) {
                        terminal_state.contribution[seat] = spot.contributions[seat];
                    }
                    std::array<combination_index, N> combo_by_player{};
                    for (std::size_t seat = 0; seat < N; ++seat) {
                        combo_by_player[seat] = 0;
                    }
                    const auto cache = make_river_terminal_cache(public_board);
                    std::array<reach_vector, N> full_reach = reach_vectors;
                    std::array<river_reach_index, N> reach_indices{};
                    for (std::size_t seat = 0; seat < N; ++seat) {
                        reach_indices[seat] = make_river_reach_index(cache, full_reach[seat]);
                    }
                    context.terminal_provider = cfr::solver::make_terminal_state_provider<N>(
                        cache,
                        reach_indices,
                        lowered_terminal_leaves,
                        lowered_terminal_states,
                        combo_by_player,
                        spot.samples_per_combo);
                    auto result = cfr::solver::run_cfr_iteration(
                        context,
                        cfr::solver::iteration_config{
                            .variant = cfr::solver::cfr_variant::cfr_plus,
                            .iteration = i,
                            .updating_player = updating_player
                        },
                        std::span<cfr::traversal::worker_context>{workers});
                    if (!result) {
                        return std::unexpected(cli_error{
                            cli_error_kind::solver,
                            "CFR iteration failed for player update."
                        });
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
                        "CFR iteration batch complete.");
                }
            }
            output.timing.cfr_iterations_ms = std::chrono::duration<double, std::milli>(
                std::chrono::steady_clock::now() - iter_begin).count();

            if (solve_cancel_requested(runtime)) {
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
                "Extracting root strategy.");
            const auto root_infoset = lowered_graph->infoset_id[lowered_graph->root_node];
            const auto root_sums = strategy_sums.infoset_sums(root_infoset);
            if (root_sums.empty()) {
                return std::unexpected(cli_error{cli_error_kind::solver, "Root infoset has no actions."});
            }

            auto initial_state = cfr::make_initial_betting_state(config);
            const auto root_actions = cfr::legal_betting_actions(initial_state, config.abstraction);
            if (root_actions.size() != root_sums.size()) {
                return std::unexpected(cli_error{cli_error_kind::solver, "Root action shape does not match strategy table shape."});
            }

            double sum = 0.0;
            for (const auto value : root_sums) {
                if (value > 0.0f) {
                    sum += value;
                }
            }
            std::vector<action_strategy> root_strategy;
            root_strategy.reserve(root_sums.size());
            const auto uniform = 1.0 / static_cast<double>(root_sums.size());
            for (std::size_t action_index = 0; action_index < root_sums.size(); ++action_index) {
                const auto frequency = sum > 0.0
                    ? static_cast<double>(std::max(root_sums[action_index], 0.0f)) / sum
                    : uniform;
                root_strategy.push_back(action_strategy{
                    .action = action_label(root_actions[action_index], spot.gross_pot),
                    .frequency = frequency
                });
            }

            terminal_context<N> terminal{};
            terminal.gross_pot = spot.gross_pot;
            terminal.rake = spot.rake;
            for (std::size_t seat = 0; seat < N; ++seat) {
                terminal.contribution[seat] = spot.contributions[seat];
            }

            const auto cache = make_river_terminal_cache(public_board);
            std::array<reach_vector, N> hero_reach{};
            std::array<river_reach_index, N> hero_indices{};
            for (std::size_t seat = 0; seat < N; ++seat) {
                hero_reach[seat] = reach_vectors[seat];
                hero_indices[seat] = make_river_reach_index(cache, hero_reach[seat]);
            }
            terminal_values<N> showdown_values{};
            if constexpr (N == 2) {
                showdown_values = evaluate_showdown(cache, hero_indices[0], hero_indices[1], terminal).values;
            } else {
                showdown_values = evaluate_showdown_values_multiplayer_sampled(
                    cache,
                    hero_indices,
                    terminal,
                    spot.samples_per_combo);
            }

            solve_artifact artifact{};
            artifact.players.assign(spot.players.begin(), std::next(spot.players.begin(), N));
            artifact.board = spot.board;
            artifact.street = spot.street;
            artifact.hero_seat = spot.hero_seat;
            artifact.solver.algorithm = "cfr+";
            artifact.solver.iterations = iterations;
            artifact.solver.timestamp = runtime.timestamp_utc.empty() ? now_utc_iso8601() : runtime.timestamp_utc;
            artifact.solver.git_revision = runtime.git_revision;

            const auto hero = static_cast<std::size_t>(spot.hero_seat);
            for (combination_index combo = 0; combo < combination_count; ++combo) {
                if (reach_vectors[hero][combo] <= 0.0f) {
                    continue;
                }
                artifact.strategy.push_back(hand_strategy{
                    .hand = hand_text_from_combo(combo),
                    .ev = showdown_values[hero][combo]
                });
            }
            artifact.root_strategy = std::move(root_strategy);
            std::vector<std::vector<action_strategy>> node_strategies(lowered_graph->node_count);
            for (uint32_t node_id = 0; node_id < lowered_graph->node_count; ++node_id) {
                if (!lowered_graph->is_player_node(node_id)) {
                    continue;
                }
                const auto infoset_id = lowered_graph->infoset_id[node_id];
                const auto sums = strategy_sums.infoset_sums(infoset_id);
                double positive = 0.0;
                for (const auto value : sums) {
                    positive += std::max(0.0f, value);
                }
                auto& node_strategy = node_strategies[node_id];
                node_strategy.reserve(sums.size());
                for (uint32_t action_index = 0; action_index < sums.size(); ++action_index) {
                    node_strategy.push_back(action_strategy{
                        .action = (node_id == lowered_graph->root_node && action_index < artifact.root_strategy.size())
                            ? artifact.root_strategy[action_index].action
                            : std::to_string(action_index),
                        .frequency = positive > 0.0
                            ? static_cast<double>(std::max(0.0f, sums[action_index])) / positive
                            : (sums.empty() ? 0.0 : 1.0 / static_cast<double>(sums.size()))
                    });
                }
            }
            append_solved_graph_payload(
                artifact,
                *lowered_graph,
                *lowered_annotations,
                nullptr,
                nullptr,
                nullptr,
                node_strategies);
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
                "Extraction complete.");

            if (auto validation = validate_artifact(output.artifact); !validation) {
                return std::unexpected(validation.error());
            }
            return output;
        }
    }

    [[nodiscard]] inline std::expected<solve_output, cli_error> solve_spot(
        const solve_spot& spot,
        const uint64_t iterations,
        const solve_runtime_options& runtime = {})
    {
        if (spot.players.size() < cli_min_players || spot.players.size() > cli_max_players) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Player count must be between 2 and 7."});
        }
        if (spot.ranges.size() != spot.players.size()) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Ranges array must match player count."});
        }
        if (spot.stacks.size() != spot.players.size()) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Stacks array must match player count."});
        }
        if (spot.contributions.size() != spot.players.size()) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Contributions array must match player count."});
        }
        if (spot.root_actor >= spot.players.size()) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "root_actor is out of range."});
        }
        if (spot.hero_seat >= spot.players.size()) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "hero_seat is out of range."});
        }
        auto street = detail::parse_holdem_street(spot.street);
        if (!street) {
            return std::unexpected(street.error());
        }
        if (spot.board.size() != detail::board_size_for_street(*street)) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Board card count must match street."});
        }

        switch (spot.players.size()) {
            case 2: return detail::solve_spot_impl<2>(spot, iterations, runtime);
            case 3: return detail::solve_spot_impl<3>(spot, iterations, runtime);
            case 4: return detail::solve_spot_impl<4>(spot, iterations, runtime);
            case 5: return detail::solve_spot_impl<5>(spot, iterations, runtime);
            case 6: return detail::solve_spot_impl<6>(spot, iterations, runtime);
            default:
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Unsupported player count."});
        }
    }

    [[nodiscard]] inline std::string format_dump(const solve_artifact& artifact)
    {
        std::vector<std::string> actions;
        const auto* header_strategy = !artifact.root_strategy.empty()
            ? &artifact.root_strategy
            : (artifact.strategy.empty() ? nullptr : &artifact.strategy.front().strategy);
        if (header_strategy != nullptr) {
            actions.reserve(header_strategy->size());
            for (const auto& action : *header_strategy) {
                actions.push_back(action.action);
            }
        }

        if (!artifact.root_strategy.empty()) {
            std::ostringstream os;
            os << "Root Strategy\n";
            os << "------------\n";
            for (const auto& action : artifact.root_strategy) {
                const auto pct = static_cast<int>(std::llround(action.frequency * 100.0));
                os << std::left << std::setw(12) << action.action << (std::to_string(pct) + "%") << "\n";
            }
            os << "\n";
            os << std::left << std::setw(8) << "Hand" << std::setw(10) << "EV" << "\n";
            os << std::string(18, '-') << "\n";
            for (const auto& row : artifact.strategy) {
                os << std::left << std::setw(8) << row.hand
                   << std::showpos << std::fixed << std::setprecision(2) << std::setw(10) << row.ev
                   << std::noshowpos << "\n";
            }
            return os.str();
        }

        const int hand_width = 8;
        const int action_width = 10;
        const int ev_width = 10;
        std::ostringstream os;
        os << std::left << std::setw(hand_width) << "Hand";
        for (const auto& action : actions) {
            os << std::setw(action_width) << action;
        }
        os << std::setw(ev_width) << "EV" << "\n";
        os << std::string(hand_width + static_cast<int>(actions.size()) * action_width + ev_width, '-') << "\n";

        for (const auto& row : artifact.strategy) {
            os << std::left << std::setw(hand_width) << row.hand;
            for (const auto& action_name : actions) {
                double frequency = 0.0;
                for (const auto& action : row.strategy) {
                    if (action.action == action_name) {
                        frequency = action.frequency;
                        break;
                    }
                }
                const auto pct = static_cast<int>(std::llround(frequency * 100.0));
                os << std::setw(action_width) << (std::to_string(pct) + "%");
            }
            os << std::showpos << std::fixed << std::setprecision(2) << std::setw(ev_width) << row.ev << std::noshowpos << "\n";
        }
        return os.str();
    }

}
