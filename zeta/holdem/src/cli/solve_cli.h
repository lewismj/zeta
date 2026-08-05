#pragma once

#include "board.h"
#include "cfr/betting/betting.h"
#include "cfr/solver/iteration.h"
#include "range_parser.h"
#include "terminal.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <cctype>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <expected>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <ios>
#include <iterator>
#include <set>
#include <sstream>
#include <string>
#include <string_view>
#include <vector>

namespace zeta::holdem::cli {

    inline constexpr std::size_t cli_min_players = 2;
    inline constexpr std::size_t cli_max_players = 6;

    enum class cli_error_kind : uint8_t {
        io,
        parse,
        invalid_spot,
        invalid_artifact,
        solver
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

    struct solve_artifact {
        uint32_t schema_version = 1;
        std::string game = "holdem";
        std::string street = "river";
        std::vector<std::string> players{"BTN", "BB"};
        std::vector<std::string> board{};
        uint8_t hero_seat = 0;
        solver_metadata solver{};
        std::vector<hand_strategy> strategy;
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
        uint16_t max_history = 8;
        uint32_t public_state_id = 0;
        uint8_t root_actor = 0;
        uint8_t hero_seat = 0;
        uint16_t samples_per_combo = 64;
    };

    struct solve_runtime_options {
        std::string timestamp_utc;
        std::string git_revision = "unknown";
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

        [[nodiscard]] inline std::vector<board> enumerate_river_runouts(
            const board& partial_board,
            const std::array<combination_index, cli_max_players>& combo_by_player,
            const std::size_t player_count)
        {
            std::vector<board> runouts;
            const auto base_cards = board_cards_from_mask(partial_board.mask);
            std::set<card> dead_cards;
            for (const auto c : base_cards) {
                dead_cards.insert(c);
            }
            for (std::size_t seat = 0; seat < player_count; ++seat) {
                const auto [first, second] = extract_combo_cards(combination_mask(combo_by_player[seat]));
                dead_cards.insert(first);
                dead_cards.insert(second);
            }

            std::vector<card> available;
            available.reserve(52);
            for (uint8_t id = 0; id < 52; ++id) {
                if (!dead_cards.contains(id)) {
                    available.push_back(id);
                }
            }

            const auto missing = 5 - static_cast<int>(base_cards.size());
            if (missing <= 0) {
                runouts.push_back(partial_board);
                return runouts;
            }

            if (missing == 1) {
                runouts.reserve(available.size());
                for (const auto c : available) {
                    board river = partial_board;
                    river.add(card_mask{1} << c);
                    runouts.push_back(river);
                }
                return runouts;
            }

            if (missing == 2) {
                runouts.reserve((available.size() * (available.size() - 1)) / 2);
                for (std::size_t i = 0; i < available.size(); ++i) {
                    for (std::size_t j = i + 1; j < available.size(); ++j) {
                        board river = partial_board;
                        river.add((card_mask{1} << available[i]) | (card_mask{1} << available[j]));
                        runouts.push_back(river);
                    }
                }
            }

            return runouts;
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

        template <std::size_t N>
        [[nodiscard]] inline bool choose_combo_set_recursive(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& reach_indices,
            std::array<combination_index, N>& chosen,
            const std::size_t seat,
            const card_mask used_cards)
        {
            if (seat == N) {
                return true;
            }
            const auto& reach = reach_indices[seat];
            for (uint16_t offset = 0; offset < reach.active_count; ++offset) {
                const auto combo = reach.active_indices[offset];
                const auto combo_mask = cache.masks[combo];
                if ((combo_mask & used_cards) != 0) {
                    continue;
                }
                chosen[seat] = combo;
                if (choose_combo_set_recursive(cache, reach_indices, chosen, seat + 1, used_cards | combo_mask)) {
                    return true;
                }
            }
            return false;
        }

        template <std::size_t N>
        [[nodiscard]] inline std::expected<std::array<combination_index, N>, cli_error> choose_combo_set(
            const river_terminal_cache& cache,
            const std::array<river_reach_index, N>& reach_indices)
        {
            std::array<combination_index, N> chosen{};
            if (!choose_combo_set_recursive(cache, reach_indices, chosen, 0u, card_mask{0})) {
                return std::unexpected(cli_error{cli_error_kind::solver, "No disjoint live combo set found across all players."});
            }
            return chosen;
        }

        template <std::size_t N>
        [[nodiscard]] inline bool choose_combo_set_from_ranges_recursive(
            const std::array<reach_vector, N>& reach_vectors,
            std::array<combination_index, N>& chosen,
            const std::size_t seat,
            const card_mask used_cards)
        {
            if (seat == N) {
                return true;
            }
            for (combination_index combo = 0; combo < combination_count; ++combo) {
                if (reach_vectors[seat][combo] <= 0.0f) {
                    continue;
                }
                const auto combo_mask = combination_masks[combo];
                if ((combo_mask & used_cards) != 0) {
                    continue;
                }
                chosen[seat] = combo;
                if (choose_combo_set_from_ranges_recursive(reach_vectors, chosen, seat + 1, used_cards | combo_mask)) {
                    return true;
                }
            }
            return false;
        }

        template <std::size_t N>
        [[nodiscard]] inline std::expected<std::array<combination_index, N>, cli_error> choose_combo_set_from_ranges(
            const std::array<reach_vector, N>& reach_vectors,
            const card_mask board_mask)
        {
            std::array<combination_index, N> chosen{};
            if (!choose_combo_set_from_ranges_recursive(reach_vectors, chosen, 0u, board_mask)) {
                return std::unexpected(cli_error{cli_error_kind::solver, "No disjoint live combo set found across all players."});
            }
            return chosen;
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
        if (artifact.schema_version != 1u) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Unsupported schema_version."});
        }
        if (artifact.game != "holdem") {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "game must be \"holdem\"."});
        }
        auto parsed_street = detail::parse_holdem_street(artifact.street);
        if (!parsed_street) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, parsed_street.error().message});
        }
        if (artifact.players.size() < cli_min_players || artifact.players.size() > cli_max_players) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "players must contain between 2 and 6 labels."});
        }
        if (artifact.hero_seat >= artifact.players.size()) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "hero_seat is out of range."});
        }
        auto board_result = detail::board_from_cards(artifact.board, *parsed_street);
        if (!board_result) {
            return std::unexpected(cli_error{cli_error_kind::invalid_artifact, board_result.error().message});
        }

        std::set<std::string> seen_hands;
        for (const auto& row : artifact.strategy) {
            if (row.strategy.empty()) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Strategy row must contain at least one action."});
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

            double sum = 0.0;
            for (const auto& action : row.strategy) {
                if (!std::isfinite(action.frequency) || action.frequency < 0.0 || action.frequency > 1.0) {
                    return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Action frequency out of [0,1] for hand: " + row.hand});
                }
                sum += action.frequency;
            }
            if (std::abs(sum - 1.0) > 1.0e-3) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "Action frequencies must sum to 1 for hand: " + row.hand});
            }
            if (!std::isfinite(row.ev)) {
                return std::unexpected(cli_error{cli_error_kind::invalid_artifact, "EV must be finite for hand: " + row.hand});
            }
        }

        return {};
    }

    namespace detail {

        template <std::size_t N>
        [[nodiscard]] inline std::expected<solve_output, cli_error> solve_spot_impl(
            const solve_spot& spot,
            const uint64_t iterations,
            const solve_runtime_options& runtime)
        {
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
            config.abstraction.fixed_pot_fractions = {spot.bet_fraction};
            config.abstraction.geometric_size_count = 1;
            config.abstraction.stack_ratio_buckets = {spot.bet_fraction};
            config.abstraction.max_raises_per_street = 1;
            config.max_history = spot.max_history;
            config.public_state_id = spot.public_state_id;

            auto lowered = cfr::lower_betting_tree_to_graph(config);
            if (!lowered) {
                return std::unexpected(cli_error{
                    cli_error_kind::solver,
                    "Failed to lower betting tree to CFR graph."
                });
            }
            auto layout_result = cfr::make_action_table_layout(lowered->graph);
            if (!layout_result) {
                return std::unexpected(cli_error{
                    cli_error_kind::solver,
                    "Failed to build CFR action layout."
                });
            }
            cfr::regret_table regrets(*layout_result);
            cfr::strategy_sum_table strategy_sums(*layout_result);
            auto context = cfr::solver::make_cfr_solver_context<N>(
                lowered->graph,
                lowered->annotations,
                *layout_result,
                regrets,
                strategy_sums);

            auto combos = choose_combo_set_from_ranges(reach_vectors, public_board.mask);
            if (!combos) {
                return std::unexpected(combos.error());
            }

            std::array<combination_index, cli_max_players> combo_fixed{};
            for (std::size_t seat = 0; seat < N; ++seat) {
                combo_fixed[seat] = (*combos)[seat];
            }

            std::array<std::vector<float>, N> fixed_terminal_utility{};
            if (street != cfr::solver::holdem_street::river) {
                const auto runouts = enumerate_river_runouts(public_board, combo_fixed, N);
                if (runouts.empty()) {
                    return std::unexpected(cli_error{cli_error_kind::solver, "No valid river runouts available for this spot."});
                }
                for (auto& by_node : fixed_terminal_utility) {
                    by_node.assign(lowered->graph.node_count, 0.0f);
                }
                std::size_t valid_runout_count = 0u;
                for (const auto& river_board : runouts) {
                    const auto runout_cache = make_river_terminal_cache(river_board);
                    std::array<reach_vector, N> runout_reach{};
                    std::array<river_reach_index, N> runout_indices{};
                    bool valid_runout = true;
                    for (std::size_t seat = 0; seat < N; ++seat) {
                        runout_reach[seat][(*combos)[seat]] = 1.0f;
                        runout_indices[seat] = make_river_reach_index(runout_cache, runout_reach[seat]);
                        if (runout_indices[seat].active_count == 0u) {
                            valid_runout = false;
                            break;
                        }
                    }
                    if (!valid_runout) {
                        continue;
                    }
                    ++valid_runout_count;
                    const terminal_engine<N> engine{};
                    for (uint32_t node_id = 0; node_id < lowered->graph.node_count; ++node_id) {
                        if (lowered->graph.node_types[node_id] != cfr::node_kind::terminal) {
                            continue;
                        }
                        const auto terminal_state_id = lowered->terminal_leaves[node_id].terminal_state_id;
                        const auto values = engine.evaluate_terminal_values(
                            runout_cache,
                            runout_indices,
                            lowered->terminal_states[terminal_state_id],
                            spot.samples_per_combo);
                        for (std::size_t seat = 0; seat < N; ++seat) {
                            fixed_terminal_utility[seat][node_id] += values[seat][(*combos)[seat]];
                        }
                    }
                }
                const auto divisor = static_cast<float>(valid_runout_count);
                if (divisor <= 0.0f) {
                    return std::unexpected(cli_error{cli_error_kind::solver, "No valid river runouts survived filtering."});
                }
                for (auto& by_node : fixed_terminal_utility) {
                    for (auto& value : by_node) {
                        value /= divisor;
                    }
                }
            } else {
                const auto cache = make_river_terminal_cache(public_board);
                std::array<reach_vector, N> singleton_reach{};
                std::array<river_reach_index, N> singleton_indices{};
                for (std::size_t seat = 0; seat < N; ++seat) {
                    singleton_reach[seat][(*combos)[seat]] = 1.0f;
                    singleton_indices[seat] = make_river_reach_index(cache, singleton_reach[seat]);
                }
                const terminal_engine<N> engine{};
                for (auto& by_node : fixed_terminal_utility) {
                    by_node.assign(lowered->graph.node_count, 0.0f);
                }
                for (uint32_t node_id = 0; node_id < lowered->graph.node_count; ++node_id) {
                    if (lowered->graph.node_types[node_id] != cfr::node_kind::terminal) {
                        continue;
                    }
                    const auto terminal_state_id = lowered->terminal_leaves[node_id].terminal_state_id;
                    const auto values = engine.evaluate_terminal_values(
                        cache,
                        singleton_indices,
                        lowered->terminal_states[terminal_state_id],
                        spot.samples_per_combo);
                    for (std::size_t seat = 0; seat < N; ++seat) {
                        fixed_terminal_utility[seat][node_id] = values[seat][(*combos)[seat]];
                    }
                }
            }

            output.timing.graph_build_ms = std::chrono::duration<double, std::milli>(
                std::chrono::steady_clock::now() - graph_begin).count();

            const auto iter_begin = std::chrono::steady_clock::now();
            std::array<cfr::traversal::worker_context, 1> workers{};
            for (uint64_t i = 0; i < iterations; ++i) {
                for (uint8_t updating_player = 0; updating_player < N; ++updating_player) {
                    context.terminal_provider = cfr::solver::make_fixed_terminal_provider<N>(
                        std::span<const float>{fixed_terminal_utility[updating_player]});
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
            }
            output.timing.cfr_iterations_ms = std::chrono::duration<double, std::milli>(
                std::chrono::steady_clock::now() - iter_begin).count();

            const auto extraction_begin = std::chrono::steady_clock::now();
            const auto root_infoset = lowered->graph.infoset_id[lowered->graph.root_node];
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

            terminal_values<N> showdown_values{};
            if (street == cfr::solver::holdem_street::river) {
                const auto cache = make_river_terminal_cache(public_board);
                std::array<reach_vector, N> hero_reach{};
                std::array<river_reach_index, N> hero_indices{};
                for (std::size_t seat = 0; seat < N; ++seat) {
                    hero_reach[seat] = reach_vectors[seat];
                    hero_indices[seat] = make_river_reach_index(cache, hero_reach[seat]);
                }
                if constexpr (N == 2) {
                    showdown_values = evaluate_showdown(cache, hero_indices[0], hero_indices[1], terminal).values;
                } else {
                    showdown_values = evaluate_showdown_values_multiplayer_sampled(
                        cache,
                        hero_indices,
                        terminal,
                        spot.samples_per_combo);
                }
            } else {
                const auto runouts = enumerate_river_runouts(public_board, combo_fixed, N);
                if (runouts.empty()) {
                    return std::unexpected(cli_error{cli_error_kind::solver, "No valid river runouts available for EV extraction."});
                }
                for (const auto& river_board : runouts) {
                    const auto cache = make_river_terminal_cache(river_board);
                    std::array<reach_vector, N> runout_reach{};
                    std::array<river_reach_index, N> runout_indices{};
                    for (std::size_t seat = 0; seat < N; ++seat) {
                        runout_reach[seat] = reach_vectors[seat];
                        runout_indices[seat] = make_river_reach_index(cache, runout_reach[seat]);
                    }
                    terminal_values<N> values{};
                    if constexpr (N == 2) {
                        values = evaluate_showdown(cache, runout_indices[0], runout_indices[1], terminal).values;
                    } else {
                        values = evaluate_showdown_values_multiplayer_sampled(
                            cache,
                            runout_indices,
                            terminal,
                            spot.samples_per_combo);
                    }
                    for (std::size_t seat = 0; seat < N; ++seat) {
                        for (combination_index combo = 0; combo < combination_count; ++combo) {
                            showdown_values[seat][combo] += values[seat][combo];
                        }
                    }
                }
                const auto denom = static_cast<float>(runouts.size());
                for (std::size_t seat = 0; seat < N; ++seat) {
                    for (combination_index combo = 0; combo < combination_count; ++combo) {
                        showdown_values[seat][combo] = showdown_values[seat][combo] / denom;
                    }
                }
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
                    .strategy = root_strategy,
                    .ev = showdown_values[hero][combo]
                });
            }
            output.artifact = std::move(artifact);

            output.timing.extraction_ms = std::chrono::duration<double, std::milli>(
                std::chrono::steady_clock::now() - extraction_begin).count();

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
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Player count must be between 2 and 6."});
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
        if (!artifact.strategy.empty()) {
            actions.reserve(artifact.strategy.front().strategy.size());
            for (const auto& action : artifact.strategy.front().strategy) {
                actions.push_back(action.action);
            }
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
