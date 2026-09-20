#pragma once

#include "cfr/chance/chance.h"
#include "terminal/reach_index.h"

#include <expected>
#include <string>

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

        [[nodiscard]] std::expected<std::span<const river_reach_index>, std::string> get_or_materialize_reach_indices(
           const cfr::public_state_id public_state_id,
           const cfr::runout_terminal_table& terminals,
           const std::array<reach_vector, N>& ranges
        ) noexcept {
           const auto* terminal_entry = terminals.find(public_state_id);
           if (terminal_entry == nullptr) {
               return std::unexpected(std::string("public_state_id has no terminal cache"));
           }
           materialize(terminal_entry->cache, ranges);
           return std::span<const river_reach_index>{reach};
        }
    };

    template <std::size_t N>
    struct runout_terminal_worker_cache {
        cfr::public_state_id current_public_state = cfr::INVALID_PUBLIC_STATE_ID;
        terminal_workspace<N> workspace{};
        std::array<river_reach_index, N>* current_reach_indices = nullptr;

        [[nodiscard]] std::expected<std::span<const river_reach_index>, std::string> get_or_materialize_reach_indices(
           const cfr::public_state_id public_state_id,
           const cfr::runout_terminal_table& terminals,
           const std::array<reach_vector, N>& ranges
        ) noexcept {
           const auto* terminal_entry = terminals.find(public_state_id);
           if (terminal_entry == nullptr) {
               return std::unexpected(std::string("public_state_id has no terminal cache"));
           }
           if (public_state_id != current_public_state) {
               current_public_state = public_state_id;
               current_reach_indices = &workspace.reach;
               workspace.materialize(terminal_entry->cache, ranges);
           }
           return std::span<const river_reach_index>{*current_reach_indices};
        }
    };
}
