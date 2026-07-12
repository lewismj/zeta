#pragma once

#include <compare>
#include <cstdint>
#include <ostream>

#include <core.h>

namespace zeta::holdem {

    enum class hand_category : uint8_t {
        high_card,
        pair,
        two_pair,
        trips,
        straight,
        flush,
        full_house,
        quads,
        straight_flush
    };

    struct hand_rank {
        uint32_t value{};

        constexpr auto operator<=>(const hand_rank&) const = default;
    };

    inline_always const char* to_string(const hand_category category) noexcept {
        switch (category) {
            case hand_category::high_card: return "high_card";
            case hand_category::pair: return "pair";
            case hand_category::two_pair: return "two_pair";
            case hand_category::trips: return "trips";
            case hand_category::straight: return "straight";
            case hand_category::flush: return "flush";
            case hand_category::full_house: return "full_house";
            case hand_category::quads: return "quads";
            case hand_category::straight_flush: return "straight_flush";
        }
        return "unknown";
    }

    inline_always std::ostream& operator<<(std::ostream& os, const hand_category category) {
        return os << to_string(category);
    }

    inline_always std::ostream& operator<<(std::ostream& os, const hand_rank rank) {
        return os << "hand_rank{value=" << rank.value << ", category="
                  << static_cast<hand_category>(rank.value >> 24) << "}";
    }

}
