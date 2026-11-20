#include "cfr/betting/betting.h"

#include <boost/json.hpp>

namespace zeta::holdem::cfr {

    namespace detail {

        bool checked_add(uint64_t& total, const uint64_t value) noexcept
        {
            if (value > std::numeric_limits<uint64_t>::max() - total) {
                return false;
            }
            total += value;
            return true;
        }

        bool checked_mul(const uint64_t lhs, const uint64_t rhs, uint64_t& result) noexcept
        {
            if (lhs != 0u && rhs > std::numeric_limits<uint64_t>::max() / lhs) {
                return false;
            }
            result = lhs * rhs;
            return true;
        }

        bool contains_action_kind(
            const std::span<const betting_action> actions,
            const betting_action_kind kind,
            const utility target_bet) noexcept
        {
            return std::ranges::any_of(actions, [kind, target_bet](const betting_action& action) {
                return action.kind == kind && action.target_bet == target_bet;
            });
        }

        std::expected<void, betting_policy_error> validate_fraction_values(
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

        std::expected<void, betting_policy_error> validate_raise_multiple_values(
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

    }

    std::expected<void, betting_policy_error> validate_betting_abstraction_policy(
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

    std::string serialize_betting_abstraction_policy(const betting_abstraction_policy& policy)
    {
        return boost::json::serialize(to_json(policy));
    }

    std::expected<betting_abstraction_policy, std::string> deserialize_betting_abstraction_policy(
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
            for (const auto& [key, street_value] : max_by_street->as_object()) {
                const auto street = street_index_for_key(key);
                if (street == 0u) {
                    continue;
                }
                auto parsed = parse_json_u16(street_value, "max_raises_by_street");
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
            for (const auto& [key, street_value] : threshold_by_street->as_object()) {
                const auto street = street_index_for_key(key);
                if (street == 0u) {
                    continue;
                }
                auto parsed = parse_json_number(street_value, "all_in_threshold_by_street");
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
            for (const auto& [key, street_value] : actor_sizes->as_object()) {
                const auto street = street_index_for_key(key);
                if (street == 0u) {
                    continue;
                }
                if (!street_value.is_array()) {
                    return std::unexpected("Field 'street_actor_sizes' entries must be arrays.");
                }
                std::vector<actor_size_set> sets;
                sets.reserve(street_value.as_array().size());
                for (const auto& actor_value : street_value.as_array()) {
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

    uint64_t hash_betting_abstraction_policy(
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

}
