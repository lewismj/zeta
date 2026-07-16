#pragma once

#include "terminal/reach_index.h"

namespace zeta::holdem {
    template <std::size_t N>
    struct terminal_workspace {
        std::array<river_reach_index, N> reach{};

        /**
         * Materialize ranges into reach indices for the given board.
         * Call this once per board before evaluating multiple nodes on that board.
         */
        void materialize(
            const river_terminal_cache& cache,
            const std::array<reach_vector, N>& ranges
        ) noexcept {
            for (std::size_t seat = 0; seat < N; ++seat) {
                reach[seat] = make_river_reach_index(cache, ranges[seat]);
            }
        }
    };
}
