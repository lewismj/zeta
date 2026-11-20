#include "cli/combo_cfr.h"

namespace zeta::holdem::cli {

    std::expected<solve_output, cli_error> solve_spot(
        const struct solve_spot& spot,
        const uint64_t iterations,
        const solve_runtime_options& runtime)
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
        // Spots built in code skip parse_spot_json, so the amounts are checked here too.
        if (const auto issues = validate_spot_amounts(spot); !issues.empty()) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, issues.front().message});
        }
        auto street = detail::parse_holdem_street(spot.street);
        if (!street) {
            return std::unexpected(street.error());
        }
        if (spot.board.size() != detail::board_size_for_street(*street)) {
            return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Board card count must match street."});
        }
        // Spots built in code skip parse_spot_json, so their locks are normalised here.
        std::optional<struct solve_spot> normalized;
        const struct solve_spot* source = &spot;
        if (!spot.locks.empty()) {
            normalized = spot;
            if (auto locks = normalize_node_locks(normalized->locks); !locks) {
                return std::unexpected(locks.error());
            }
            source = &*normalized;
        }

        switch (source->players.size()) {
            case 2: return detail::solve_spot_impl<2>(*source, iterations, runtime);
            case 3: return detail::solve_spot_impl<3>(*source, iterations, runtime);
            case 4: return detail::solve_spot_impl<4>(*source, iterations, runtime);
            case 5: return detail::solve_spot_impl<5>(*source, iterations, runtime);
            case 6: return detail::solve_spot_impl<6>(*source, iterations, runtime);
            case 7: return detail::solve_spot_impl<7>(*source, iterations, runtime);
            default:
                return std::unexpected(cli_error{cli_error_kind::invalid_spot, "Unsupported player count."});
        }
    }

}
