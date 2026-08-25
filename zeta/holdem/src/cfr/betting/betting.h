#pragma once

#include "cfr/graph/builder.h"
#include "cfr/graph/validation.h"
#include "cfr/solver/infoset_planning.h"
#include "cfr/solver/iteration.h"
#include "terminal/terminal_types.h"

#include <boost/json.hpp>

#include <algorithm>
#include <array>
#include <bit>
#include <cmath>
#include <expected>
#include <limits>
#include <optional>
#include <ostream>
#include <span>
#include <vector>

namespace zeta::holdem::cfr {

    enum class betting_action_kind : uint8_t {
        fold = 0,
        check = 1,
        call = 2,
        bet = 3,
        raise = 4,
        all_in = 5
    };

    struct betting_action {
        betting_action_kind kind = betting_action_kind::check;
        utility amount = 0.0;      /**< Chips added by the acting player. */
        utility target_bet = 0.0;  /**< Actor's committed amount after the action. */
    };

    struct betting_action_record {
        uint8_t actor = solver::INVALID_PLAYER;
        betting_action action{};
    };

    enum class betting_validation_error_kind : uint8_t {
        invalid_actor,
        invalid_stack,
        invalid_commitment,
        invalid_current_bet,
        invalid_terminal_state,
        illegal_action,
        memory_plan_failed,
        graph_build_failed
    };

    struct betting_validation_error {
        betting_validation_error_kind kind{};
        uint32_t state_id = 0;
        uint32_t node_id = game_graph::INVALID_NODE;
        solver::cfr_memory_plan_error memory_plan_error{};
        graph_build_error graph_error{};
    };

    [[nodiscard]] constexpr const char* to_string(const betting_validation_error_kind kind) noexcept
    {
        using enum betting_validation_error_kind;
        switch (kind) {
            case invalid_actor:         return "betting_validation_error_kind::invalid_actor";
            case invalid_stack:         return "betting_validation_error_kind::invalid_stack";
            case invalid_commitment:    return "betting_validation_error_kind::invalid_commitment";
            case invalid_current_bet:   return "betting_validation_error_kind::invalid_current_bet";
            case invalid_terminal_state:return "betting_validation_error_kind::invalid_terminal_state";
            case illegal_action:        return "betting_validation_error_kind::illegal_action";
            case memory_plan_failed:    return "betting_validation_error_kind::memory_plan_failed";
            case graph_build_failed:    return "betting_validation_error_kind::graph_build_failed";
        }
        return "betting_validation_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const betting_validation_error_kind kind)
    {
        return os << to_string(kind);
    }

    template <std::size_t N>
    struct betting_state {
        solver::holdem_street street = solver::holdem_street::river;
        uint8_t actor = 0;
        std::array<utility, N> stacks{};
        std::array<utility, N> committed{};
        folded_mask<N> folded{};
        player_mask<N> all_in{};
        utility current_bet = 0.0;
        /** Minimum increment established by the last full bet or raise. */
        utility last_raise_increment = 0.0;
        uint16_t raise_count = 0;
        std::vector<betting_action_record> action_history{};
        std::vector<pot_layer<N>> pot_layers{};
        terminal_state_kind terminal_kind = terminal_state_kind::none;
        std::array<bool, N> acted_since_aggression{};

        [[nodiscard]] bool terminal() const noexcept
        {
            return terminal_kind != terminal_state_kind::none;
        }
    };

    struct actor_size_set {
        std::vector<double> fractions{};
        std::vector<double> raise_multiples{};
    };

    enum class betting_policy_error_kind : uint8_t {
        negative_fraction,
        zero_max_raises,
        invalid_all_in_threshold,
        invalid_min_bet_increment,
        raise_multiple_too_small,
        invalid_geometric_size_count
    };

    struct betting_policy_error {
        betting_policy_error_kind kind{};
        uint8_t street = 0;
        uint8_t actor = 0;
    };

    [[nodiscard]] constexpr const char* to_string(const betting_policy_error_kind kind) noexcept
    {
        using enum betting_policy_error_kind;
        switch (kind) {
            case negative_fraction:            return "betting_policy_error_kind::negative_fraction";
            case zero_max_raises:              return "betting_policy_error_kind::zero_max_raises";
            case invalid_all_in_threshold:     return "betting_policy_error_kind::invalid_all_in_threshold";
            case invalid_min_bet_increment:    return "betting_policy_error_kind::invalid_min_bet_increment";
            case raise_multiple_too_small:     return "betting_policy_error_kind::raise_multiple_too_small";
            case invalid_geometric_size_count: return "betting_policy_error_kind::invalid_geometric_size_count";
        }
        return "betting_policy_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const betting_policy_error_kind kind)
    {
        return os << to_string(kind);
    }

    struct betting_abstraction_policy {
        std::array<std::vector<actor_size_set>, 5> street_actor_sizes{}; 
        std::vector<double> fixed_pot_fractions{0.5, 1.0};
        double all_in_threshold = 0.95;
        std::array<std::optional<uint16_t>, 5> max_raises_by_street{};
        std::array<std::optional<double>, 5> all_in_threshold_by_street{};
        uint16_t max_raises = 2;
        utility min_bet_increment = 1.0;

        [[nodiscard]] const actor_size_set& sizes_for_actor(
            const solver::holdem_street street,
            const uint8_t actor) const noexcept
        {
            const auto street_index = static_cast<std::size_t>(street);
            if (street_index < street_actor_sizes.size() && actor < street_actor_sizes[street_index].size()) {
                const auto& actor_sizes = street_actor_sizes[street_index][actor];
                if (!actor_sizes.fractions.empty() || !actor_sizes.raise_multiples.empty()) {
                    return actor_sizes;
                }
            }

            thread_local actor_size_set fallback{};
            fallback.fractions = fixed_pot_fractions;
            fallback.raise_multiples.clear();
            return fallback;
        }

        [[nodiscard]] uint16_t max_raises_for_street(const solver::holdem_street street) const noexcept
        {
            const auto street_index = static_cast<std::size_t>(street);
            if (street_index < max_raises_by_street.size() && max_raises_by_street[street_index].has_value()) {
                return *max_raises_by_street[street_index];
            }
            return max_raises;
        }

        [[nodiscard]] double all_in_threshold_for_street(const solver::holdem_street street) const noexcept
        {
            const auto street_index = static_cast<std::size_t>(street);
            if (street_index < all_in_threshold_by_street.size() && all_in_threshold_by_street[street_index].has_value()) {
                return *all_in_threshold_by_street[street_index];
            }
            return all_in_threshold;
        }
    };

    template <std::size_t N>
    struct holdem_betting_graph_config {
        solver::holdem_street street = solver::holdem_street::river;
        std::array<utility, N> initial_stacks{};
        std::array<utility, N> initial_committed{};
        uint8_t root_actor = 0;
        betting_abstraction_policy abstraction{};
        uint16_t max_history = 16;
        uint32_t public_state_id = 0;
        solver::cfr_memory_plan_options memory_plan_options{};
        solver::cfr_memory_plan_limits memory_plan_limits{};
    };

    template <std::size_t N>
    struct holdem_betting_graph {
        game_graph graph{};
        solver::solver_graph_annotations annotations{};
        terminal_state_table<N> terminal_states{};
        std::vector<solver::cfr_terminal_leaf> terminal_leaves{};
        std::vector<solver::solver_node_state_metadata> rich_state_metadata{};
        uint64_t deterministic_hash = 0;
        uint64_t config_hash = 0;
    };

    namespace detail {

        [[nodiscard]] inline uint64_t hash_combine(const uint64_t value, const uint64_t input) noexcept
        {
            uint64_t hash = value;
            for (uint32_t shift = 0; shift < 64; shift += 8) {
                hash ^= (input >> shift) & 0xffu;
                hash *= solver::compatibility_hasher::PRIME;
            }
            return hash;
        }

        [[nodiscard]] inline uint64_t hash_utility(uint64_t hash, const utility value) noexcept
        {
            return hash_combine(hash, std::bit_cast<uint64_t>(value));
        }

        template <std::size_t N>
        [[nodiscard]] uint8_t next_actor(const betting_state<N>& state) noexcept
        {
            for (std::size_t offset = 1; offset <= N; ++offset) {
                const auto candidate = static_cast<uint8_t>((state.actor + offset) % N);
                if (!state.folded[candidate] && !state.all_in[candidate]) {
                    return candidate;
                }
            }
            return solver::INVALID_PLAYER;
        }

        template <std::size_t N>
        [[nodiscard]] uint32_t active_count(const betting_state<N>& state) noexcept
        {
            uint32_t count = 0;
            for (std::size_t seat = 0; seat < N; ++seat) {
                if (!state.folded[seat]) {
                    ++count;
                }
            }
            return count;
        }

        template <std::size_t N>
        [[nodiscard]] bool betting_round_complete(const betting_state<N>& state) noexcept
        {
            for (std::size_t seat = 0; seat < N; ++seat) {
                if (state.folded[seat] || state.all_in[seat]) {
                    continue;
                }
                if (!state.acted_since_aggression[seat] || state.committed[seat] < state.current_bet) {
                    return false;
                }
            }
            return true;
        }

        template <std::size_t N>
        [[nodiscard]] std::vector<pot_layer<N>> make_pot_layers(const betting_state<N>& state)
        {
            std::vector<utility> levels;
            levels.reserve(N);
            for (const auto contribution : state.committed) {
                if (contribution > 0.0) {
                    levels.push_back(contribution);
                }
            }
            std::sort(levels.begin(), levels.end());
            levels.erase(std::unique(levels.begin(), levels.end()), levels.end());

            std::vector<pot_layer<N>> layers;
            utility previous = 0.0;
            for (const auto level : levels) {
                pot_layer<N> layer{};
                const auto slice = level - previous;
                if (slice <= 0.0) {
                    continue;
                }
                uint32_t contributors = 0;
                for (std::size_t seat = 0; seat < N; ++seat) {
                    if (state.committed[seat] >= level) {
                        layer.contributors_mask.set(seat);
                        ++contributors;
                        if (!state.folded[seat]) {
                            layer.eligible_mask.set(seat);
                        }
                    }
                }
                layer.amount = slice * static_cast<utility>(contributors);
                layers.push_back(layer);
                previous = level;
            }
            return layers;
        }

        template <std::size_t N>
        [[nodiscard]] terminal_state<N> make_terminal_state_from_betting(const betting_state<N>& state)
        {
            terminal_context<N> context{};
            for (std::size_t seat = 0; seat < N; ++seat) {
                context.contribution[seat] = state.committed[seat];
                context.gross_pot += state.committed[seat];
            }

            terminal_state<N> terminal{};
            terminal.kind = state.terminal_kind;
            terminal.context = context;
            terminal.folded = state.folded;
            terminal.all_in_eligible_mask = state.all_in;
            terminal.pot_layers = make_pot_layers(state);
            for (std::size_t seat = 0; seat < N; ++seat) {
                if (!state.folded[seat]) {
                    terminal.active_eligible_mask.set(seat);
                }
            }
            return terminal;
        }

        template <std::size_t N>
        void settle_if_terminal(betting_state<N>& state) noexcept
        {
            if (active_count(state) <= 1u) {
                state.terminal_kind = terminal_state_kind::fold;
                state.actor = solver::INVALID_PLAYER;
                return;
            }
            if (betting_round_complete(state) || next_actor(state) == solver::INVALID_PLAYER) {
                state.terminal_kind = terminal_state_kind::showdown;
                state.actor = solver::INVALID_PLAYER;
            }
        }

        [[nodiscard]] inline bool contains_action_kind(
            const std::span<const betting_action> actions,
            const betting_action_kind kind,
            const utility target_bet) noexcept
        {
            return std::ranges::any_of(actions, [kind, target_bet](const betting_action& action) {
                return action.kind == kind && action.target_bet == target_bet;
            });
        }

        [[nodiscard]] inline std::expected<void, betting_policy_error> validate_fraction_values(
            const std::vector<double>& values,
            const uint8_t street,
            const uint8_t actor) noexcept
        {
            for (const auto value : values) {
                if (!std::isfinite(value) || value < 0.0) {
                    return std::unexpected(betting_policy_error{
                        betting_policy_error_kind::negative_fraction,
                        street,
                        actor
                    });
                }
            }
            return {};
        }

        [[nodiscard]] inline std::expected<void, betting_policy_error> validate_raise_multiple_values(
            const std::vector<double>& values,
            const uint8_t street,
            const uint8_t actor) noexcept
        {
            for (const auto value : values) {
                if (!std::isfinite(value) || value <= 1.0) {
                    return std::unexpected(betting_policy_error{
                        betting_policy_error_kind::raise_multiple_too_small,
                        street,
                        actor
                    });
                }
            }
            return {};
        }

        template <std::size_t N>
        /** Full-raise minimum: previous full increment, or the opening minimum. */
        [[nodiscard]] utility required_raise_increment(
            const betting_state<N>& state,
            const betting_abstraction_policy& policy) noexcept
        {
            return state.last_raise_increment > 0.0 ? state.last_raise_increment : policy.min_bet_increment;
        }

        /** A short all-in does not count as a full raise. */
        template <std::size_t N>
        [[nodiscard]] bool is_full_raise(
            const betting_state<N>& state,
            const utility new_current_bet,
            const betting_abstraction_policy& policy) noexcept
        {
            const auto increment = new_current_bet - state.current_bet;
            return increment >= required_raise_increment(state, policy);
        }

        /** Raise legality follows the reopen rule, not just size availability. */
        template <std::size_t N>
        [[nodiscard]] bool can_raise(
            const betting_state<N>& state,
            const uint8_t actor,
            const betting_abstraction_policy& policy) noexcept
        {
            if (actor >= N || state.folded[actor] || state.all_in[actor]) {
                return false;
            }
            const auto to_call = std::max<utility>(0.0, state.current_bet - state.committed[actor]);
            return state.stacks[actor] > to_call
                && state.raise_count < policy.max_raises_for_street(state.street)
                && !state.acted_since_aggression[actor];
        }

        template <std::size_t N>
        [[nodiscard]] utility live_pot(const betting_state<N>& state) noexcept
        {
            utility pot = 0.0;
            for (const auto contribution : state.committed) {
                pot += contribution;
            }
            return pot;
        }
    }

    [[nodiscard]] inline std::expected<void, betting_policy_error> validate_betting_abstraction_policy(
        const betting_abstraction_policy& policy) noexcept
    {
        if (!std::isfinite(policy.min_bet_increment) || policy.min_bet_increment <= 0.0) {
            return std::unexpected(betting_policy_error{
                betting_policy_error_kind::invalid_min_bet_increment
            });
        }

        if (policy.max_raises == 0u) {
            return std::unexpected(betting_policy_error{
                betting_policy_error_kind::zero_max_raises
            });
        }

        if (!std::isfinite(policy.all_in_threshold) || policy.all_in_threshold <= 0.0 || policy.all_in_threshold > 1.0) {
            return std::unexpected(betting_policy_error{
                betting_policy_error_kind::invalid_all_in_threshold
            });
        }

        for (std::size_t street = 0; street < policy.max_raises_by_street.size(); ++street) {
            if (policy.max_raises_by_street[street].has_value() && *policy.max_raises_by_street[street] == 0u) {
                return std::unexpected(betting_policy_error{
                    betting_policy_error_kind::zero_max_raises,
                    static_cast<uint8_t>(street)
                });
            }
        }

        for (std::size_t street = 0; street < policy.all_in_threshold_by_street.size(); ++street) {
            if (!policy.all_in_threshold_by_street[street].has_value()) {
                continue;
            }
            const auto threshold = *policy.all_in_threshold_by_street[street];
            if (!std::isfinite(threshold) || threshold <= 0.0 || threshold > 1.0) {
                return std::unexpected(betting_policy_error{
                    betting_policy_error_kind::invalid_all_in_threshold,
                    static_cast<uint8_t>(street)
                });
            }
        }

        if (auto result = detail::validate_fraction_values(policy.fixed_pot_fractions, 0, 0); !result) {
            return result;
        }

        for (std::size_t street = 0; street < policy.street_actor_sizes.size(); ++street) {
            const auto& actor_sizes = policy.street_actor_sizes[street];
            for (std::size_t actor = 0; actor < actor_sizes.size(); ++actor) {
                if (auto result = detail::validate_fraction_values(
                        actor_sizes[actor].fractions,
                        static_cast<uint8_t>(street),
                        static_cast<uint8_t>(actor)); !result) {
                    return result;
                }
                if (auto result = detail::validate_raise_multiple_values(
                        actor_sizes[actor].raise_multiples,
                        static_cast<uint8_t>(street),
                        static_cast<uint8_t>(actor)); !result) {
                    return result;
                }
            }
        }

        return {};
    }

    namespace {
        namespace json = boost::json;

        [[nodiscard]] inline std::expected<double, std::string> parse_json_number(
            const json::value& value,
            const char* key)
        {
            if (value.is_double()) {
                const auto number = value.as_double();
                if (!std::isfinite(number)) {
                    return std::unexpected(std::string("Invalid finite numeric value for '") + key + "'.");
                }
                return number;
            }
            if (value.is_int64()) {
                return static_cast<double>(value.as_int64());
            }
            if (value.is_uint64()) {
                return static_cast<double>(value.as_uint64());
            }
            return std::unexpected(std::string("Field '") + key + "' must be numeric.");
        }

        [[nodiscard]] inline std::expected<std::vector<double>, std::string> parse_json_double_array(
            const json::value& value,
            const char* key)
        {
            if (!value.is_array()) {
                return std::unexpected(std::string("Field '") + key + "' must be an array.");
            }
            std::vector<double> numbers;
            numbers.reserve(value.as_array().size());
            for (const auto& item : value.as_array()) {
                auto parsed = parse_json_number(item, key);
                if (!parsed) {
                    return std::unexpected(parsed.error());
                }
                numbers.push_back(*parsed);
            }
            return numbers;
        }

        [[nodiscard]] inline std::expected<uint16_t, std::string> parse_json_u16(
            const json::value& value,
            const char* key)
        {
            if (value.is_uint64()) {
                const auto parsed = value.as_uint64();
                if (parsed > static_cast<uint64_t>(std::numeric_limits<uint16_t>::max())) {
                    return std::unexpected(std::string("Field '") + key + "' is out of range.");
                }
                return static_cast<uint16_t>(parsed);
            }
            if (value.is_int64()) {
                const auto parsed = value.as_int64();
                if (parsed < 0 || parsed > static_cast<int64_t>(std::numeric_limits<uint16_t>::max())) {
                    return std::unexpected(std::string("Field '") + key + "' is out of range.");
                }
                return static_cast<uint16_t>(parsed);
            }
            return std::unexpected(std::string("Field '") + key + "' must be an unsigned integer.");
        }

        [[nodiscard]] inline json::value to_json(const std::vector<double>& values)
        {
            json::array array;
            array.reserve(values.size());
            for (const auto value : values) {
                array.emplace_back(value);
            }
            return json::value{std::move(array)};
        }

        [[nodiscard]] inline json::value to_json(const actor_size_set& sizes)
        {
            json::object object;
            object["fractions"] = to_json(sizes.fractions);
            object["raise_multiples"] = to_json(sizes.raise_multiples);
            return json::value{std::move(object)};
        }

        [[nodiscard]] inline std::expected<actor_size_set, std::string> parse_actor_size_set(
            const json::object& object,
            const char* key)
        {
            actor_size_set sizes{};
            const auto* fractions = object.if_contains("fractions");
            if (fractions != nullptr) {
                auto parsed = parse_json_double_array(*fractions, key);
                if (!parsed) {
                    return std::unexpected(parsed.error());
                }
                sizes.fractions = std::move(*parsed);
            }
            const auto* raise_multiples = object.if_contains("raise_multiples");
            if (raise_multiples != nullptr) {
                auto parsed = parse_json_double_array(*raise_multiples, key);
                if (!parsed) {
                    return std::unexpected(parsed.error());
                }
                sizes.raise_multiples = std::move(*parsed);
            }
            return sizes;
        }

        [[nodiscard]] inline const char* street_key_for_index(const std::size_t index) noexcept
        {
            switch (index) {
                case 1: return "preflop";
                case 2: return "flop";
                case 3: return "turn";
                case 4: return "river";
                default: return nullptr;
            }
        }

        [[nodiscard]] inline std::size_t street_index_for_key(const std::string_view key) noexcept
        {
            if (key == "preflop") {
                return 1u;
            }
            if (key == "flop") {
                return 2u;
            }
            if (key == "turn") {
                return 3u;
            }
            if (key == "river") {
                return 4u;
            }
            return 0u;
        }

        [[nodiscard]] inline json::value to_json(const betting_abstraction_policy& policy)
        {
            json::object root;
            root["schema_version"] = 1;
            root["min_bet_increment"] = policy.min_bet_increment;
            root["max_raises"] = static_cast<int64_t>(policy.max_raises);
            root["all_in_threshold"] = policy.all_in_threshold;

            json::object max_raises_by_street;
            for (std::size_t street = 1; street < policy.max_raises_by_street.size(); ++street) {
                if (!policy.max_raises_by_street[street].has_value()) {
                    continue;
                }
                const auto* key = street_key_for_index(street);
                if (key != nullptr) {
                    max_raises_by_street[key] = static_cast<int64_t>(*policy.max_raises_by_street[street]);
                }
            }
            if (!max_raises_by_street.empty()) {
                root["max_raises_by_street"] = std::move(max_raises_by_street);
            }

            json::object all_in_threshold_by_street;
            for (std::size_t street = 1; street < policy.all_in_threshold_by_street.size(); ++street) {
                if (!policy.all_in_threshold_by_street[street].has_value()) {
                    continue;
                }
                const auto* key = street_key_for_index(street);
                if (key != nullptr) {
                    all_in_threshold_by_street[key] = *policy.all_in_threshold_by_street[street];
                }
            }
            if (!all_in_threshold_by_street.empty()) {
                root["all_in_threshold_by_street"] = std::move(all_in_threshold_by_street);
            }

            json::object street_actor_sizes;
            for (std::size_t street = 1; street < policy.street_actor_sizes.size(); ++street) {
                const auto* key = street_key_for_index(street);
                if (key == nullptr || policy.street_actor_sizes[street].empty()) {
                    continue;
                }
                json::array actors;
                actors.reserve(policy.street_actor_sizes[street].size());
                for (const auto& actor_sizes : policy.street_actor_sizes[street]) {
                    actors.emplace_back(to_json(actor_sizes));
                }
                street_actor_sizes[key] = std::move(actors);
            }
            if (!street_actor_sizes.empty()) {
                root["street_actor_sizes"] = std::move(street_actor_sizes);
            }

            root["fixed_pot_fractions"] = to_json(policy.fixed_pot_fractions);
            return json::value{std::move(root)};
        }
    }

    [[nodiscard]] inline std::string serialize_betting_abstraction_policy(const betting_abstraction_policy& policy)
    {
        return boost::json::serialize(to_json(policy));
    }

    [[nodiscard]] inline std::expected<betting_abstraction_policy, std::string> deserialize_betting_abstraction_policy(
        std::string_view json_text)
    {
        boost::system::error_code ec;
        const auto value = boost::json::parse(json_text, ec);
        if (ec) {
            return std::unexpected(std::string{"Invalid betting abstraction policy JSON: "} + ec.message());
        }
        if (!value.is_object()) {
            return std::unexpected("betting abstraction policy JSON must be an object.");
        }

        const auto& object = value.as_object();
        betting_abstraction_policy policy{};

        if (const auto* min_value = object.if_contains("min_bet_increment")) {
            auto parsed = parse_json_number(*min_value, "min_bet_increment");
            if (!parsed) {
                return std::unexpected(parsed.error());
            }
            policy.min_bet_increment = *parsed;
        }

        if (const auto* max_value = object.if_contains("max_raises")) {
            auto parsed = parse_json_u16(*max_value, "max_raises");
            if (!parsed) {
                return std::unexpected(parsed.error());
            }
            policy.max_raises = *parsed;
        }

        if (const auto* threshold_value = object.if_contains("all_in_threshold")) {
            auto parsed = parse_json_number(*threshold_value, "all_in_threshold");
            if (!parsed) {
                return std::unexpected(parsed.error());
            }
            policy.all_in_threshold = *parsed;
        }

        if (const auto* max_by_street = object.if_contains("max_raises_by_street"); max_by_street != nullptr) {
            if (!max_by_street->is_object()) {
                return std::unexpected("Field 'max_raises_by_street' must be an object.");
            }
            for (const auto& [key, value] : max_by_street->as_object()) {
                const auto street = street_index_for_key(key);
                if (street == 0u) {
                    continue;
                }
                auto parsed = parse_json_u16(value, "max_raises_by_street");
                if (!parsed) {
                    return std::unexpected(parsed.error());
                }
                policy.max_raises_by_street[street] = *parsed;
            }
        }

        if (const auto* threshold_by_street = object.if_contains("all_in_threshold_by_street"); threshold_by_street != nullptr) {
            if (!threshold_by_street->is_object()) {
                return std::unexpected("Field 'all_in_threshold_by_street' must be an object.");
            }
            for (const auto& [key, value] : threshold_by_street->as_object()) {
                const auto street = street_index_for_key(key);
                if (street == 0u) {
                    continue;
                }
                auto parsed = parse_json_number(value, "all_in_threshold_by_street");
                if (!parsed) {
                    return std::unexpected(parsed.error());
                }
                policy.all_in_threshold_by_street[street] = *parsed;
            }
        }

        if (const auto* actor_sizes = object.if_contains("street_actor_sizes"); actor_sizes != nullptr) {
            if (!actor_sizes->is_object()) {
                return std::unexpected("Field 'street_actor_sizes' must be an object.");
            }
            for (const auto& [key, value] : actor_sizes->as_object()) {
                const auto street = street_index_for_key(key);
                if (street == 0u) {
                    continue;
                }
                if (!value.is_array()) {
                    return std::unexpected("Field 'street_actor_sizes' entries must be arrays.");
                }
                std::vector<actor_size_set> sets;
                sets.reserve(value.as_array().size());
                for (const auto& actor_value : value.as_array()) {
                    if (!actor_value.is_object()) {
                        return std::unexpected("Each actor size entry must be an object.");
                    }
                    auto parsed = parse_actor_size_set(actor_value.as_object(), key.data());
                    if (!parsed) {
                        return std::unexpected(parsed.error());
                    }
                    sets.push_back(std::move(*parsed));
                }
                policy.street_actor_sizes[street] = std::move(sets);
            }
        }

        if (const auto* fractions = object.if_contains("fixed_pot_fractions"); fractions != nullptr) {
            auto parsed = parse_json_double_array(*fractions, "fixed_pot_fractions");
            if (!parsed) {
                return std::unexpected(parsed.error());
            }
            policy.fixed_pot_fractions = std::move(*parsed);
        }

        if (auto result = validate_betting_abstraction_policy(policy); !result) {
            return std::unexpected(std::string{"Invalid betting abstraction policy: "} + std::string{to_string(result.error().kind)});
        }
        return policy;
    }

    [[nodiscard]] inline std::expected<betting_abstraction_policy, betting_policy_error>
    make_single_size_policy(const double fraction = 0.75, const uint16_t max_raises = 2)
    {
        betting_abstraction_policy policy{};
        policy.fixed_pot_fractions = {fraction};
        policy.max_raises = max_raises;
        if (auto result = validate_betting_abstraction_policy(policy); !result) {
            return std::unexpected(result.error());
        }
        return policy;
    }

    [[nodiscard]] inline std::expected<betting_abstraction_policy, betting_policy_error>
    make_multi_size_policy(const std::vector<double> fractions = {0.33, 0.67, 1.0}, const uint16_t max_raises = 2)
    {
        betting_abstraction_policy policy{};
        policy.fixed_pot_fractions = fractions;
        policy.max_raises = max_raises;
        if (auto result = validate_betting_abstraction_policy(policy); !result) {
            return std::unexpected(result.error());
        }
        return policy;
    }

    [[nodiscard]] inline std::expected<betting_abstraction_policy, betting_policy_error>
    make_geometric_policy(const double min_fraction = 0.33, const uint16_t size_count = 3, const uint16_t max_raises = 2)
    {
        if (size_count == 0u) {
            return std::unexpected(betting_policy_error{betting_policy_error_kind::invalid_geometric_size_count});
        }
        if (!std::isfinite(min_fraction) || min_fraction <= 0.0 || min_fraction > 1.0) {
            return std::unexpected(betting_policy_error{betting_policy_error_kind::negative_fraction});
        }

        betting_abstraction_policy policy{};
        policy.max_raises = max_raises;
        policy.fixed_pot_fractions.clear();
        if (size_count == 1u) {
            policy.fixed_pot_fractions.push_back(min_fraction);
        } else {
            policy.fixed_pot_fractions.reserve(size_count);
            for (uint16_t index = 0; index < size_count; ++index) {
                const auto progress = static_cast<double>(index) / static_cast<double>(size_count - 1u);
                const auto ratio = std::pow(1.0 / min_fraction, progress);
                auto value = min_fraction * ratio;
                if (index + 1u == size_count) {
                    value = 1.0;
                }
                policy.fixed_pot_fractions.push_back(value);
            }
        }
        if (auto result = validate_betting_abstraction_policy(policy); !result) {
            return std::unexpected(result.error());
        }
        return policy;
    }

    [[nodiscard]] inline std::expected<betting_abstraction_policy, betting_policy_error>
    make_overbet_policy(
        const std::vector<double> base_fractions = {0.5, 1.0},
        const std::vector<double> overbet_fractions = {1.5, 2.0},
        const uint16_t max_raises = 2)
    {
        betting_abstraction_policy policy{};
        policy.fixed_pot_fractions = base_fractions;
        policy.fixed_pot_fractions.insert(policy.fixed_pot_fractions.end(), overbet_fractions.begin(), overbet_fractions.end());
        policy.max_raises = max_raises;
        if (auto result = validate_betting_abstraction_policy(policy); !result) {
            return std::unexpected(result.error());
        }
        return policy;
    }

    [[nodiscard]] inline std::expected<betting_abstraction_policy, betting_policy_error>
    make_all_in_inclusive_policy(const std::vector<double> fractions = {0.5, 1.0}, const uint16_t max_raises = 2)
    {
        betting_abstraction_policy policy{};
        policy.fixed_pot_fractions = fractions;
        policy.all_in_threshold = 1.0;
        policy.max_raises = max_raises;
        if (auto result = validate_betting_abstraction_policy(policy); !result) {
            return std::unexpected(result.error());
        }
        return policy;
    }

    [[nodiscard]] inline std::expected<betting_abstraction_policy, betting_policy_error>
    make_actor_policy(
        const std::array<std::vector<actor_size_set>, 5> actor_sizes,
        const uint16_t max_raises = 2)
    {
        betting_abstraction_policy policy{};
        policy.street_actor_sizes = actor_sizes;
        policy.max_raises = max_raises;
        if (auto result = validate_betting_abstraction_policy(policy); !result) {
            return std::unexpected(result.error());
        }
        return policy;
    }

    template <std::size_t N>
    [[nodiscard]] std::expected<void, betting_validation_error> validate_betting_state(
        const betting_state<N>& state,
        const uint32_t state_id = 0) noexcept
    {
        if (state.terminal()) {
            if (state.actor != solver::INVALID_PLAYER) {
                return std::unexpected(betting_validation_error{betting_validation_error_kind::invalid_terminal_state, state_id});
            }
        } else if (state.actor >= N || state.folded[state.actor] || state.all_in[state.actor]) {
            return std::unexpected(betting_validation_error{betting_validation_error_kind::invalid_actor, state_id});
        }

        utility max_committed = 0.0;
        for (std::size_t seat = 0; seat < N; ++seat) {
            if (state.stacks[seat] < 0.0) {
                return std::unexpected(betting_validation_error{betting_validation_error_kind::invalid_stack, state_id});
            }
            if (state.committed[seat] < 0.0) {
                return std::unexpected(betting_validation_error{betting_validation_error_kind::invalid_commitment, state_id});
            }
            max_committed = std::max(max_committed, state.committed[seat]);
        }
        if (state.current_bet < 0.0 || state.current_bet < max_committed) {
            return std::unexpected(betting_validation_error{betting_validation_error_kind::invalid_current_bet, state_id});
        }
        return {};
    }

    template <std::size_t N>
    [[nodiscard]] std::vector<betting_action> legal_betting_actions(
        const betting_state<N>& state,
        const betting_abstraction_policy& policy)
    {
        std::vector<betting_action> actions;
        if (state.terminal() || state.actor >= N || state.folded[state.actor] || state.all_in[state.actor]) {
            return actions;
        }

        const auto actor = state.actor;
        const auto to_call = std::max<utility>(0.0, state.current_bet - state.committed[actor]);
        const auto stack = state.stacks[actor];
        if (to_call > 0.0) {
            actions.push_back(betting_action{
                .kind = betting_action_kind::fold,
                .amount = 0.0,
                .target_bet = state.committed[actor]
            });
            actions.push_back(betting_action{
                .kind = betting_action_kind::call,
                .amount = std::min(stack, to_call),
                .target_bet = state.committed[actor] + std::min(stack, to_call)
            });
        } else {
            actions.push_back(betting_action{
                .kind = betting_action_kind::check,
                .amount = 0.0,
                .target_bet = state.committed[actor]
            });
        }

        const auto can_aggress = detail::can_raise(state, actor, policy);
        if (can_aggress) {
            const auto pot = detail::live_pot(state);
            const auto required_increment = detail::required_raise_increment(state, policy);
            const auto& sizes = policy.sizes_for_actor(state.street, actor);
            const auto all_in_target = state.committed[actor] + stack;
            const auto all_in_snap_target = all_in_target * static_cast<utility>(policy.all_in_threshold_for_street(state.street));

            auto add_target_action = [&](const utility raw_target) {
                auto target = std::min(raw_target, all_in_target);
                if (state.current_bet == 0.0) {
                    target = std::max(target, policy.min_bet_increment);
                }
                if (target <= state.current_bet || target <= state.committed[actor]) {
                    return;
                }

                const bool snaps_to_all_in = target >= all_in_snap_target;
                if (!snaps_to_all_in) {
                    const auto increment = target - state.current_bet;
                    if (increment < required_increment) {
                        return;
                    }
                }

                const auto kind = snaps_to_all_in
                    ? betting_action_kind::all_in
                    : (state.current_bet == 0.0 ? betting_action_kind::bet : betting_action_kind::raise);
                const auto final_target = snaps_to_all_in ? all_in_target : target;
                if (!detail::contains_action_kind(actions, kind, final_target)) {
                    actions.push_back(betting_action{
                        .kind = kind,
                        .amount = final_target - state.committed[actor],
                        .target_bet = final_target
                    });
                }
            };

            for (const auto fraction : sizes.fractions) {
                if (!std::isfinite(fraction) || fraction <= 0.0) {
                    continue;
                }
                add_target_action(state.current_bet + pot * static_cast<utility>(fraction));
            }
            for (const auto multiple : sizes.raise_multiples) {
                if (!std::isfinite(multiple) || multiple <= 1.0 || state.current_bet <= 0.0) {
                    continue;
                }
                add_target_action(state.current_bet * static_cast<utility>(multiple));
            }
        }

        if (stack > 0.0) {
            const auto target = state.committed[actor] + stack;
            if (!detail::contains_action_kind(actions, betting_action_kind::all_in, target)) {
                actions.push_back(betting_action{
                    .kind = betting_action_kind::all_in,
                    .amount = stack,
                    .target_bet = target
                });
            }
        }

        return actions;
    }

    template <std::size_t N>
    [[nodiscard]] std::expected<betting_state<N>, betting_validation_error> apply_betting_action(
        const betting_state<N>& state,
        const betting_action& action,
        const betting_abstraction_policy& policy,
        const uint32_t state_id = 0)
    {
        if (auto result = validate_betting_state(state, state_id); !result) {
            return std::unexpected(result.error());
        }

        const auto legal = legal_betting_actions(state, policy);
        bool matched = false;
        for (const auto& candidate : legal) {
            if (candidate.kind == action.kind && candidate.target_bet == action.target_bet) {
                matched = true;
                break;
            }
        }
        if (!matched) {
            return std::unexpected(betting_validation_error{betting_validation_error_kind::illegal_action, state_id});
        }

        auto next = state;
        const auto actor = static_cast<std::size_t>(state.actor);
        next.action_history.push_back(betting_action_record{state.actor, action});

        if (action.kind == betting_action_kind::fold) {
            next.folded.set_folded(actor, true);
            next.acted_since_aggression[actor] = true;
        } else {
            const auto chips = std::min(next.stacks[actor], std::max<utility>(0.0, action.target_bet - next.committed[actor]));
            next.stacks[actor] -= chips;
            next.committed[actor] += chips;
            if (next.stacks[actor] == 0.0 || action.kind == betting_action_kind::all_in) {
                next.all_in.set(actor);
            }
            next.acted_since_aggression[actor] = true;
            const auto new_current_bet = next.committed[actor];
            const auto full_raise = detail::is_full_raise(state, new_current_bet, policy);
            if (action.kind == betting_action_kind::bet
                || action.kind == betting_action_kind::raise
                || action.kind == betting_action_kind::all_in) {
                next.current_bet = new_current_bet;
                if (full_raise) {
                    next.last_raise_increment = new_current_bet - state.current_bet;
                    if (state.current_bet > 0.0) {
                        ++next.raise_count;
                    }
                    next.acted_since_aggression.fill(false);
                    next.acted_since_aggression[actor] = true;
                }
            }
        }

        detail::settle_if_terminal(next);
        if (!next.terminal()) {
            next.actor = detail::next_actor(next);
        }
        next.pot_layers = detail::make_pot_layers(next);
        return next;
    }

    template <std::size_t N>
    [[nodiscard]] betting_state<N> make_initial_betting_state(const holdem_betting_graph_config<N>& config)
    {
        betting_state<N> state{};
        state.street = config.street;
        state.actor = config.root_actor;
        state.stacks = config.initial_stacks;
        state.committed = config.initial_committed;
        state.current_bet = *std::max_element(state.committed.begin(), state.committed.end());
        state.last_raise_increment = 0.0;
        state.pot_layers = detail::make_pot_layers(state);
        return state;
    }

    template <std::size_t N>
    [[nodiscard]] std::expected<solver::cfr_memory_estimate, solver::cfr_memory_plan_error> estimate_betting_graph_memory(
        const holdem_betting_graph_config<N>& config) noexcept
    {
        const auto& street_sizes = config.abstraction.sizes_for_actor(config.street, config.root_actor);
        uint64_t max_actions_per_state = 3u;
        if (!solver::checked_add(max_actions_per_state, street_sizes.fractions.size())
            || !solver::checked_add(max_actions_per_state, street_sizes.raise_multiples.size())) {
            return std::unexpected(solver::cfr_memory_plan_error{solver::cfr_memory_plan_error_kind::estimate_overflow});
        }
        max_actions_per_state = std::max<uint64_t>(max_actions_per_state, 1u);

        uint64_t node_count = 1u;
        uint64_t nodes_at_depth = 1u;
        for (uint16_t depth = 0; depth < config.max_history; ++depth) {
            if (!solver::checked_mul(nodes_at_depth, max_actions_per_state, nodes_at_depth)
                || !solver::checked_add(node_count, nodes_at_depth)) {
                return std::unexpected(solver::cfr_memory_plan_error{solver::cfr_memory_plan_error_kind::estimate_overflow});
            }
        }

        const auto edge_count = node_count == 0u ? 0u : node_count - 1u;
        return solver::estimate_cfr_memory(
            solver::cfr_memory_shape{
                .node_count = node_count,
                .edge_count = edge_count,
                .infoset_count = node_count,
                .action_value_count = edge_count,
                .max_depth = config.max_history
            },
            config.memory_plan_options,
            config.memory_plan_limits);
    }

    [[nodiscard]] inline uint64_t hash_betting_abstraction_policy(
        const betting_abstraction_policy& policy) noexcept
    {
        solver::compatibility_hasher hash;
        hash.add_u64(policy.fixed_pot_fractions.size());
        for (const auto fraction : policy.fixed_pot_fractions) {
            hash.add_u64(std::bit_cast<uint64_t>(fraction));
        }
        hash.add_u64(std::bit_cast<uint64_t>(policy.all_in_threshold));
        hash.add_u64(policy.max_raises);
        hash.add_u64(std::bit_cast<uint64_t>(policy.min_bet_increment));

        for (std::size_t street = 0; street < policy.max_raises_by_street.size(); ++street) {
            const auto& value = policy.max_raises_by_street[street];
            hash.add_u64(value.has_value() ? 1u : 0u);
            if (value.has_value()) {
                hash.add_u64(*value);
            }
        }

        for (std::size_t street = 0; street < policy.all_in_threshold_by_street.size(); ++street) {
            const auto& value = policy.all_in_threshold_by_street[street];
            hash.add_u64(value.has_value() ? 1u : 0u);
            if (value.has_value()) {
                hash.add_u64(std::bit_cast<uint64_t>(*value));
            }
        }

        for (std::size_t street = 0; street < policy.street_actor_sizes.size(); ++street) {
            const auto& actor_sizes = policy.street_actor_sizes[street];
            hash.add_u64(actor_sizes.size());
            for (const auto& actor_size : actor_sizes) {
                hash.add_u64(actor_size.fractions.size());
                for (const auto fraction : actor_size.fractions) {
                    hash.add_u64(std::bit_cast<uint64_t>(fraction));
                }
                hash.add_u64(actor_size.raise_multiples.size());
                for (const auto multiple : actor_size.raise_multiples) {
                    hash.add_u64(std::bit_cast<uint64_t>(multiple));
                }
            }
        }

        return hash.value;
    }

    template <std::size_t N>
    [[nodiscard]] uint64_t hash_betting_graph_config(
        const holdem_betting_graph_config<N>& config) noexcept
    {
        solver::compatibility_hasher hash;
        hash.add_u64(N);
        hash.add_enum(config.street);
        hash.add_u64(config.root_actor);
        hash.add_u64(config.max_history);
        hash.add_u64(config.public_state_id);
        for (const auto stack : config.initial_stacks) {
            hash.add_u64(std::bit_cast<uint64_t>(stack));
        }
        for (const auto committed : config.initial_committed) {
            hash.add_u64(std::bit_cast<uint64_t>(committed));
        }
        hash.add_u64(hash_betting_abstraction_policy(config.abstraction));
        return hash.value;
    }

    template <std::size_t N>
    [[nodiscard]] uint64_t hash_betting_graph(const holdem_betting_graph<N>& lowered) noexcept
    {
        uint64_t hash = solver::compatibility_hasher::OFFSET;
        const auto& graph = lowered.graph;
        hash = detail::hash_combine(hash, N);
        hash = detail::hash_combine(hash, graph.node_count);
        hash = detail::hash_combine(hash, graph.root_node);
        hash = detail::hash_combine(hash, graph.edges.size());
        hash = detail::hash_combine(hash, graph.infoset_count);
        hash = detail::hash_combine(hash, graph.terminal_count);
        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            hash = detail::hash_combine(hash, static_cast<uint64_t>(graph.node_types[node_id]));
            hash = detail::hash_combine(hash, graph.infoset_id[node_id]);
            hash = detail::hash_combine(hash, lowered.annotations.actor_by_node[node_id]);
            hash = detail::hash_combine(hash, lowered.annotations.terminal_leaf_id_by_node[node_id]);
            hash = detail::hash_combine(hash, static_cast<uint64_t>(lowered.annotations.state_by_node[node_id].street));
            hash = detail::hash_combine(hash, lowered.annotations.state_by_node[node_id].public_state_id);
            hash = detail::hash_combine(hash, lowered.annotations.state_by_node[node_id].betting_state_id);
        }
        for (const auto& edge : graph.edges) {
            hash = detail::hash_combine(hash, edge.child_node);
            hash = detail::hash_combine(hash, edge.action_index);
        }
        for (const auto& state : lowered.terminal_states.states) {
            hash = detail::hash_combine(hash, static_cast<uint64_t>(state.kind));
            for (const auto contribution : state.context.contribution) {
                hash = detail::hash_utility(hash, contribution);
            }
            for (const auto& layer : state.pot_layers) {
                hash = detail::hash_utility(hash, layer.amount);
                for (std::size_t seat = 0; seat < N; ++seat) {
                    hash = detail::hash_combine(hash, layer.eligible_mask[seat] ? 1u : 0u);
                    hash = detail::hash_combine(hash, layer.contributors_mask[seat] ? 1u : 0u);
                }
            }
            for (std::size_t seat = 0; seat < N; ++seat) {
                hash = detail::hash_combine(hash, state.folded[seat] ? 1u : 0u);
                hash = detail::hash_combine(hash, state.all_in_eligible_mask[seat] ? 1u : 0u);
                hash = detail::hash_combine(hash, state.active_eligible_mask[seat] ? 1u : 0u);
            }
            hash = detail::hash_utility(hash, state.context.rake);
            hash = detail::hash_combine(hash, state.variant_payload_id);
        }
        return hash;
    }

    template <std::size_t N>
    [[nodiscard]] std::expected<holdem_betting_graph<N>, betting_validation_error> lower_betting_tree_to_graph(
        const holdem_betting_graph_config<N>& config)
    {
        auto root_state = make_initial_betting_state(config);
        if (auto result = validate_betting_state(root_state); !result) {
            return std::unexpected(result.error());
        }

        if (auto memory_result = estimate_betting_graph_memory(config); !memory_result) {
            return std::unexpected(betting_validation_error{
                .kind = betting_validation_error_kind::memory_plan_failed,
                .memory_plan_error = memory_result.error()
            });
        }

        graph_builder builder;
        std::vector<uint8_t> actor_by_infoset;
        std::vector<terminal_state<N>> terminal_states_in_leaf_order;
        uint32_t next_infoset_id = 0;

        auto add_state = [&](const betting_state<N>& state) {
            const auto old_node = builder.add_node(state.terminal() ? node_kind::terminal : node_kind::player);
            if (!state.terminal()) {
                builder.set_infoset_id(old_node, next_infoset_id);
                actor_by_infoset.push_back(state.actor);
                ++next_infoset_id;
            } else {
                terminal_states_in_leaf_order.push_back(detail::make_terminal_state_from_betting(state));
            }
            return old_node;
        };

        auto root = add_state(root_state);
        builder.set_root(root);

        auto expand = [&](auto&& self, const uint32_t parent_node, const betting_state<N>& state) -> std::expected<void, betting_validation_error> {
            if (state.terminal()) {
                return {};
            }
            if (state.action_history.size() >= config.max_history) {
                return std::unexpected(betting_validation_error{betting_validation_error_kind::invalid_terminal_state});
            }
            const auto actions = legal_betting_actions(state, config.abstraction);
            for (uint16_t action_index = 0; action_index < static_cast<uint16_t>(actions.size()); ++action_index) {
                auto child = apply_betting_action(state, actions[action_index], config.abstraction);
                if (!child) {
                    return std::unexpected(child.error());
                }
                const auto child_node = add_state(*child);
                builder.add_edge(parent_node, child_node, action_index);
                if (auto result = self(self, child_node, *child); !result) {
                    return result;
                }
            }
            return {};
        };

        [[maybe_unused]] const auto abstraction_id = hash_betting_abstraction_policy(config.abstraction);
        const auto config_hash = hash_betting_graph_config(config);

        if (auto result = expand(expand, root, root_state); !result) {
            return std::unexpected(result.error());
        }

        auto graph_result = builder.build();
        if (!graph_result) {
            return std::unexpected(betting_validation_error{
                .kind = betting_validation_error_kind::graph_build_failed,
                .graph_error = graph_result.error()
            });
        }

        holdem_betting_graph<N> lowered;
        lowered.graph = std::move(*graph_result);
        lowered.annotations.actor_by_node.assign(lowered.graph.node_count, solver::INVALID_PLAYER);
        lowered.annotations.chance_event_id_by_node.assign(lowered.graph.node_count, solver::INVALID_METADATA_ID);
        lowered.annotations.terminal_leaf_id_by_node.assign(lowered.graph.node_count, solver::INVALID_METADATA_ID);
        lowered.annotations.state_by_node.assign(lowered.graph.node_count, {});
        lowered.annotations.betting_tree_config_hash = config_hash;
        lowered.annotations.betting_history_abstraction_id = abstraction_id;
        lowered.terminal_leaves.assign(lowered.graph.node_count, solver::cfr_terminal_leaf{});
        lowered.rich_state_metadata.assign(lowered.graph.node_count, {});

        uint32_t terminal_state_id = 0;
        for (uint32_t node_id = 0; node_id < lowered.graph.node_count; ++node_id) {
            lowered.annotations.state_by_node[node_id] = solver::solver_node_state_metadata{
                .street = config.street,
                .public_state_id = config.public_state_id,
                .betting_state_id = node_id
            };
            lowered.rich_state_metadata[node_id] = lowered.annotations.state_by_node[node_id];
            if (lowered.graph.is_player_node(node_id)) {
                lowered.annotations.actor_by_node[node_id] = actor_by_infoset[lowered.graph.infoset_id[node_id]];
            }
            if (lowered.graph.is_terminal(node_id)) {
                lowered.annotations.terminal_leaf_id_by_node[node_id] = terminal_state_id;
                lowered.terminal_leaves[node_id].terminal_state_id = terminal_state_id;
                lowered.terminal_states.states.push_back(terminal_states_in_leaf_order[terminal_state_id]);
                ++terminal_state_id;
            }
        }

        if (auto result = graph_validation::validate_all(lowered.graph); !result) {
            return std::unexpected(betting_validation_error{
                .kind = betting_validation_error_kind::graph_build_failed,
                .node_id = result.error().node_id,
                .graph_error = result.error()
            });
        }
        if (auto result = validate_solver_graph_view(make_solver_graph_view<N>(lowered.graph, lowered.annotations)); !result) {
            return std::unexpected(betting_validation_error{
                .kind = betting_validation_error_kind::invalid_terminal_state,
                .node_id = result.error().node_id
            });
        }

        lowered.deterministic_hash = hash_betting_graph(lowered);
        lowered.config_hash = config_hash;
        return lowered;
    }

}
