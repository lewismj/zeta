#pragma once

#include "board.h"
#include "cfr/graph/graph.h"
#include "cfr/solver/metadata.h"
#include "terminal/reach_index.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <bit>
#include <cmath>
#include <cstdint>
#include <expected>
#include <limits>
#include <ostream>
#include <span>
#include <unordered_set>
#include <vector>

namespace zeta::holdem::cfr {

    using public_state_id = uint32_t;
    using chance_event_id = uint32_t;
    using chance_outcome_id = uint32_t;
    using runout_id = uint32_t;
    using board_partition_id = uint32_t;

    inline constexpr public_state_id INVALID_PUBLIC_STATE_ID = std::numeric_limits<public_state_id>::max();
    inline constexpr chance_event_id INVALID_CHANCE_EVENT = std::numeric_limits<chance_event_id>::max();
    inline constexpr chance_outcome_id INVALID_CHANCE_OUTCOME_ID = std::numeric_limits<chance_outcome_id>::max();
    inline constexpr runout_id INVALID_RUNOUT_ID = std::numeric_limits<runout_id>::max();
    inline constexpr board_partition_id INVALID_BOARD_PARTITION = std::numeric_limits<board_partition_id>::max();

    enum class public_chance_event_kind : uint8_t {
        none = 0,
        flop = 1,
        turn = 2,
        river = 3
    };

    [[nodiscard]] constexpr const char* to_string(const public_chance_event_kind kind) noexcept
    {
        using enum public_chance_event_kind;
        switch (kind) {
            case none:  return "public_chance_event_kind::none";
            case flop:  return "public_chance_event_kind::flop";
            case turn:  return "public_chance_event_kind::turn";
            case river: return "public_chance_event_kind::river";
        }
        return "public_chance_event_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const public_chance_event_kind kind)
    {
        return os << to_string(kind);
    }

    [[nodiscard]] constexpr uint8_t public_chance_cards_to_deal(const public_chance_event_kind kind) noexcept
    {
        using enum public_chance_event_kind;
        switch (kind) {
            case flop:  return 3;
            case turn:
            case river: return 1;
            case none:  return 0;
        }
        return 0;
    }

    /**
     * One enumerated chance result aligned to a chance-node child action.
     */
    struct chance_outcome {
        uint32_t child_node = game_graph::INVALID_NODE;
        uint16_t action_index = 0;
        float probability = 0.0f;
        uint32_t board_partition_id = INVALID_BOARD_PARTITION;
        chance_outcome_id outcome_id = INVALID_CHANCE_OUTCOME_ID;
        card_mask cards = 0;
        card_mask dead_cards = 0;
        bool legal = true;
    };

    struct public_board_state {
        public_state_id id = INVALID_PUBLIC_STATE_ID;
        solver::holdem_street street = solver::holdem_street::invalid;
        card_mask board_cards = 0;
        public_state_id parent_state_id = INVALID_PUBLIC_STATE_ID;
        chance_event_id chance_event_id_from_parent = INVALID_CHANCE_EVENT;
        chance_outcome_id chance_outcome_id_from_parent = INVALID_CHANCE_OUTCOME_ID;
        bool is_root_state = false;
        bool is_terminal_river_state = false;
    };

    struct public_state_registry {
        std::vector<public_board_state> states;
        std::vector<public_state_id> state_id_by_node;
        public_state_id root_public_state_id = INVALID_PUBLIC_STATE_ID;
    };

    struct complete_runout {
        runout_id id = INVALID_RUNOUT_ID;
        public_state_id root_public_state_id = INVALID_PUBLIC_STATE_ID;
        public_state_id river_public_state_id = INVALID_PUBLIC_STATE_ID;
        card_mask dealt_turn = 0;
        card_mask dealt_river = 0;
    };

    struct runout_registry {
        std::vector<complete_runout> runouts;
        std::vector<public_state_id> river_public_state_by_runout;
    };

    struct runout_terminal_entry {
        public_state_id public_state = INVALID_PUBLIC_STATE_ID;
        board river_board{};
        river_terminal_cache cache{};
    };

    struct runout_terminal_table {
        std::vector<runout_terminal_entry> entries;
        std::vector<public_state_id> entry_id_by_public_state;

        [[nodiscard]] const runout_terminal_entry* find(const public_state_id state_id) const noexcept
        {
            if (state_id == INVALID_PUBLIC_STATE_ID || state_id >= entry_id_by_public_state.size()) {
                return nullptr;
            }
            const auto entry_index = entry_id_by_public_state[state_id];
            if (entry_index == INVALID_PUBLIC_STATE_ID || entry_index >= entries.size()) {
                return nullptr;
            }
            return &entries[entry_index];
        }
    };

    /**
     * Contiguous outcome slice for one chance node.
     */
    struct chance_event {
        uint32_t node_id = game_graph::INVALID_NODE;
        uint32_t first_outcome = 0;
        uint32_t outcome_count = 0;
        public_chance_event_kind kind = public_chance_event_kind::none;
        card_mask board_cards = 0;
        card_mask dead_cards = 0;

        [[nodiscard]] uint32_t end_outcome() const noexcept
        {
            return first_outcome + outcome_count;
        }
    };

    /**
     * Enumerated chance storage indexed both by event id and graph node id.
     */
    struct chance_event_table {
        std::vector<chance_event> events;
        std::vector<chance_outcome> outcomes;
        std::vector<uint32_t> event_id_by_node;

        [[nodiscard]] std::span<const chance_outcome> event_outcomes(const chance_event& event) const noexcept
        {
            assert(event.end_outcome() <= outcomes.size());
            return event.outcome_count == 0u
                ? std::span<const chance_outcome>{}
                : std::span<const chance_outcome>{outcomes.data() + event.first_outcome, event.outcome_count};
        }

        [[nodiscard]] const chance_event* event_for_node(const uint32_t node_id) const noexcept
        {
            if (node_id >= event_id_by_node.size()) {
                return nullptr;
            }
            const auto event_id = event_id_by_node[node_id];
            return event_id == INVALID_CHANCE_EVENT || event_id >= events.size() ? nullptr : &events[event_id];
        }

        [[nodiscard]] float probability_for_edge(
            const uint32_t node_id,
            const edge child_edge) const noexcept
        {
            const auto* event = event_for_node(node_id);
            if (event == nullptr) {
                return 0.0f;
            }
            for (const auto& outcome : event_outcomes(*event)) {
                if (outcome.child_node == child_edge.child_node
                    && outcome.action_index == child_edge.action_index) {
                    return outcome.probability;
                }
            }
            return 0.0f;
        }
    };

    enum class chance_table_error_kind : uint8_t {
        side_array_size_mismatch,
        missing_chance_event,
        unexpected_chance_event,
        invalid_event_node,
        invalid_outcome_slice,
        outcome_count_mismatch,
        child_alignment_mismatch,
        invalid_probability_sum,
        dead_card_collision,
        duplicate_board_card,
        invalid_outcome_cards,
        invalid_board_partition,
        illegal_outcome
    };

    struct chance_table_error {
        chance_table_error_kind kind{};
        uint32_t node_id = game_graph::INVALID_NODE;
        uint32_t event_id = INVALID_CHANCE_EVENT;
        uint32_t outcome_index = std::numeric_limits<uint32_t>::max();
    };

    enum class public_state_registry_error_kind : uint8_t {
        invalid_state_id,
        invalid_parent_state,
        invalid_street_metadata,
        duplicate_state_id,
        duplicate_public_state_path,
        missing_root_state,
        state_mapping_mismatch,
        invalid_chance_links
    };

    struct public_state_registry_error {
        public_state_registry_error_kind kind{};
        public_state_id state_id = INVALID_PUBLIC_STATE_ID;
        uint32_t node_id = game_graph::INVALID_NODE;
    };

    [[nodiscard]] constexpr const char* to_string(const public_state_registry_error_kind kind) noexcept
    {
        using enum public_state_registry_error_kind;
        switch (kind) {
            case invalid_state_id: return "public_state_registry_error_kind::invalid_state_id";
            case invalid_parent_state: return "public_state_registry_error_kind::invalid_parent_state";
            case invalid_street_metadata: return "public_state_registry_error_kind::invalid_street_metadata";
            case duplicate_state_id: return "public_state_registry_error_kind::duplicate_state_id";
            case duplicate_public_state_path: return "public_state_registry_error_kind::duplicate_public_state_path";
            case missing_root_state: return "public_state_registry_error_kind::missing_root_state";
            case state_mapping_mismatch: return "public_state_registry_error_kind::state_mapping_mismatch";
            case invalid_chance_links: return "public_state_registry_error_kind::invalid_chance_links";
        }
        return "public_state_registry_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const public_state_registry_error_kind kind)
    {
        return os << to_string(kind);
    }

    enum class runout_registry_error_kind : uint8_t {
        invalid_runout_id,
        duplicate_runout_id,
        invalid_root_state,
        invalid_river_state,
        duplicate_river_state,
        invalid_river_path,
        invalid_board_cards
    };

    struct runout_registry_error {
        runout_registry_error_kind kind{};
        runout_id runout_id = INVALID_RUNOUT_ID;
        public_state_id state_id = INVALID_PUBLIC_STATE_ID;
    };

    [[nodiscard]] constexpr const char* to_string(const runout_registry_error_kind kind) noexcept
    {
        using enum runout_registry_error_kind;
        switch (kind) {
            case invalid_runout_id: return "runout_registry_error_kind::invalid_runout_id";
            case duplicate_runout_id: return "runout_registry_error_kind::duplicate_runout_id";
            case invalid_root_state: return "runout_registry_error_kind::invalid_root_state";
            case invalid_river_state: return "runout_registry_error_kind::invalid_river_state";
            case duplicate_river_state: return "runout_registry_error_kind::duplicate_river_state";
            case invalid_river_path: return "runout_registry_error_kind::invalid_river_path";
            case invalid_board_cards: return "runout_registry_error_kind::invalid_board_cards";
        }
        return "runout_registry_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const runout_registry_error_kind kind)
    {
        return os << to_string(kind);
    }

    [[nodiscard]] constexpr const char* to_string(const chance_table_error_kind kind) noexcept
    {
        using enum chance_table_error_kind;
        switch (kind) {
            case side_array_size_mismatch: return "chance_table_error_kind::side_array_size_mismatch";
            case missing_chance_event:     return "chance_table_error_kind::missing_chance_event";
            case unexpected_chance_event:  return "chance_table_error_kind::unexpected_chance_event";
            case invalid_event_node:       return "chance_table_error_kind::invalid_event_node";
            case invalid_outcome_slice:    return "chance_table_error_kind::invalid_outcome_slice";
            case outcome_count_mismatch:   return "chance_table_error_kind::outcome_count_mismatch";
            case child_alignment_mismatch: return "chance_table_error_kind::child_alignment_mismatch";
            case invalid_probability_sum:  return "chance_table_error_kind::invalid_probability_sum";
            case dead_card_collision:      return "chance_table_error_kind::dead_card_collision";
            case duplicate_board_card:     return "chance_table_error_kind::duplicate_board_card";
            case invalid_outcome_cards:    return "chance_table_error_kind::invalid_outcome_cards";
            case invalid_board_partition:  return "chance_table_error_kind::invalid_board_partition";
            case illegal_outcome:          return "chance_table_error_kind::illegal_outcome";
        }
        return "chance_table_error_kind::unknown";
    }

    inline std::ostream& operator<<(std::ostream& os, const chance_table_error_kind kind)
    {
        return os << to_string(kind);
    }

    namespace detail {

        [[nodiscard]] inline bool chance_probabilities_sum_to_one(
            const std::span<const chance_outcome> outcomes) noexcept
        {
            double sum = 0.0;
            for (const auto& outcome : outcomes) {
                if (!(outcome.probability >= 0.0f) || !std::isfinite(outcome.probability)) {
                    return false;
                }
                sum += static_cast<double>(outcome.probability);
            }
            return std::abs(sum - 1.0) <= 1.0e-5;
        }

        [[nodiscard]] inline uint32_t card_count(const card_mask cards) noexcept
        {
            return static_cast<uint32_t>(std::popcount(static_cast<uint64_t>(cards)));
        }

        [[nodiscard]] inline uint32_t combination_count(uint32_t n, const uint8_t k) noexcept
        {
            if (k == 0u || k > n) {
                return k == 0u ? 1u : 0u;
            }

            uint64_t result = 1;
            for (uint8_t i = 1; i <= k; ++i) {
                result = (result * static_cast<uint64_t>(n - k + i)) / i;
            }
            return static_cast<uint32_t>(result);
        }

        [[nodiscard]] inline uint32_t public_chance_outcome_count(
            const public_chance_event_kind kind,
            const card_mask board_cards,
            const card_mask dead_cards) noexcept
        {
            const auto cards_to_deal = public_chance_cards_to_deal(kind);
            if (cards_to_deal == 0u || (board_cards & dead_cards) != 0u) {
                return 0;
            }

            const auto blocked_count = card_count(board_cards | dead_cards);
            const auto live_count = static_cast<uint32_t>(zeta::num_cards<zeta::default_deck>) - blocked_count;
            return combination_count(live_count, cards_to_deal);
        }
    }

    [[nodiscard]] inline std::expected<void, public_state_registry_error> validate_public_state_registry(
       const public_state_registry& registry,
       const uint32_t node_count = 0) noexcept
    {
       if (registry.root_public_state_id != INVALID_PUBLIC_STATE_ID
           && registry.root_public_state_id >= registry.states.size()) {
           return std::unexpected(public_state_registry_error{
               public_state_registry_error_kind::missing_root_state,
               registry.root_public_state_id
           });
       }

       std::unordered_set<uint64_t> seen_paths;
       seen_paths.reserve(registry.states.size());

       for (std::size_t index = 0; index < registry.states.size(); ++index) {
           const auto& state = registry.states[index];
           if (state.id == INVALID_PUBLIC_STATE_ID || state.id != static_cast<public_state_id>(index)) {
               return std::unexpected(public_state_registry_error{
                   public_state_registry_error_kind::invalid_state_id,
                   state.id,
                   static_cast<uint32_t>(index)
               });
           }
           if (state.street == solver::holdem_street::invalid) {
               return std::unexpected(public_state_registry_error{
                   public_state_registry_error_kind::invalid_street_metadata,
                   state.id,
                   static_cast<uint32_t>(index)
               });
           }
           if (!state.is_root_state && state.parent_state_id == INVALID_PUBLIC_STATE_ID) {
               return std::unexpected(public_state_registry_error{
                   public_state_registry_error_kind::invalid_parent_state,
                   state.id,
                   static_cast<uint32_t>(index)
               });
           }
           if (state.is_root_state && state.parent_state_id != INVALID_PUBLIC_STATE_ID) {
               return std::unexpected(public_state_registry_error{
                   public_state_registry_error_kind::invalid_parent_state,
                   state.id,
                   static_cast<uint32_t>(index)
               });
           }
           if (!state.is_root_state && state.parent_state_id >= registry.states.size()) {
               return std::unexpected(public_state_registry_error{
                   public_state_registry_error_kind::invalid_parent_state,
                   state.id,
                   static_cast<uint32_t>(index)
               });
           }
           if (state.chance_event_id_from_parent != INVALID_CHANCE_EVENT && state.is_root_state) {
               return std::unexpected(public_state_registry_error{
                   public_state_registry_error_kind::invalid_chance_links,
                   state.id,
                   static_cast<uint32_t>(index)
               });
           }
           if (state.chance_outcome_id_from_parent != INVALID_CHANCE_OUTCOME_ID && state.is_root_state) {
               return std::unexpected(public_state_registry_error{
                   public_state_registry_error_kind::invalid_chance_links,
                   state.id,
                   static_cast<uint32_t>(index)
               });
           }

           // Public-state identity is the board: each reachable board is registered
           // once and shared by every betting line and dealing order that reaches it,
           // so its stored (chance-event, chance-outcome) link is the first arrival.
           // Every chance event is unique to one end-of-round node, so a repeated
           // (event, outcome) pair means a board was mistakenly registered twice.
           if (!state.is_root_state) {
               const auto path_key = (static_cast<uint64_t>(state.chance_event_id_from_parent) << 32)
                   | static_cast<uint64_t>(state.chance_outcome_id_from_parent);
               if (!seen_paths.insert(path_key).second) {
                   return std::unexpected(public_state_registry_error{
                       public_state_registry_error_kind::duplicate_public_state_path,
                       state.id,
                       static_cast<uint32_t>(index)
                   });
               }
           }
       }

       if (registry.root_public_state_id == INVALID_PUBLIC_STATE_ID && !registry.states.empty()) {
           return std::unexpected(public_state_registry_error{
               public_state_registry_error_kind::missing_root_state,
               registry.states.front().id,
               0
           });
       }
       if (node_count != 0u && registry.state_id_by_node.size() != node_count) {
           return std::unexpected(public_state_registry_error{
               public_state_registry_error_kind::state_mapping_mismatch,
               registry.root_public_state_id,
               node_count
           });
       }
       for (uint32_t node_id = 0; node_id < static_cast<uint32_t>(registry.state_id_by_node.size()); ++node_id) {
           const auto state_id = registry.state_id_by_node[node_id];
           if (state_id == INVALID_PUBLIC_STATE_ID || state_id >= registry.states.size()) {
               return std::unexpected(public_state_registry_error{
                   public_state_registry_error_kind::state_mapping_mismatch,
                   state_id,
                   node_id
               });
           }
       }

       return {};
    }

    [[nodiscard]] inline std::expected<void, runout_registry_error> validate_runout_registry(
       const runout_registry& registry) noexcept
    {
       std::vector<public_state_id> seen_river_states;
       seen_river_states.reserve(registry.runouts.size());

       for (const auto& runout : registry.runouts) {
           if (runout.id == INVALID_RUNOUT_ID || runout.id >= registry.runouts.size()) {
               return std::unexpected(runout_registry_error{
                   runout_registry_error_kind::invalid_runout_id,
                   runout.id,
                   runout.river_public_state_id
               });
           }
           if (runout.root_public_state_id == INVALID_PUBLIC_STATE_ID) {
               return std::unexpected(runout_registry_error{
                   runout_registry_error_kind::invalid_root_state,
                   runout.id,
                   runout.root_public_state_id
               });
           }
           if (runout.river_public_state_id == INVALID_PUBLIC_STATE_ID) {
               return std::unexpected(runout_registry_error{
                   runout_registry_error_kind::invalid_river_state,
                   runout.id,
                   runout.river_public_state_id
               });
           }
           if (registry.river_public_state_by_runout.size() <= runout.id) {
               return std::unexpected(runout_registry_error{
                   runout_registry_error_kind::invalid_river_path,
                   runout.id,
                   runout.river_public_state_id
               });
           }
           if (registry.river_public_state_by_runout[runout.id] != runout.river_public_state_id) {
               return std::unexpected(runout_registry_error{
                   runout_registry_error_kind::invalid_river_path,
                   runout.id,
                   runout.river_public_state_id
               });
           }
           if (runout.dealt_turn != 0u && runout.dealt_river != 0u
               && ((runout.dealt_turn & runout.dealt_river) != 0u)) {
               return std::unexpected(runout_registry_error{
                   runout_registry_error_kind::invalid_board_cards,
                   runout.id,
                   runout.river_public_state_id
               });
           }
           for (const auto river_state_id : seen_river_states) {
               if (river_state_id == runout.river_public_state_id) {
                   return std::unexpected(runout_registry_error{
                       runout_registry_error_kind::duplicate_river_state,
                       runout.id,
                       runout.river_public_state_id
                   });
               }
           }
           seen_river_states.push_back(runout.river_public_state_id);
       }

       return {};
    }

    [[nodiscard]] inline public_state_registry make_public_state_registry(
       std::span<const public_board_state> states,
       const public_state_id root_public_state_id = INVALID_PUBLIC_STATE_ID)
    {
       public_state_registry registry;
       registry.states.assign(states.begin(), states.end());
       registry.root_public_state_id = root_public_state_id;
       registry.state_id_by_node.resize(registry.states.size());
       for (std::size_t index = 0; index < registry.states.size(); ++index) {
           registry.states[index].id = static_cast<public_state_id>(index);
           registry.state_id_by_node[index] = static_cast<public_state_id>(index);
       }
       if (root_public_state_id != INVALID_PUBLIC_STATE_ID && root_public_state_id < registry.states.size()) {
           registry.root_public_state_id = root_public_state_id;
       }
       return registry;
    }

    [[nodiscard]] inline runout_registry make_runout_registry(std::span<const complete_runout> runouts)
    {
       runout_registry registry;
       registry.runouts.assign(runouts.begin(), runouts.end());
       registry.river_public_state_by_runout.resize(registry.runouts.size(), INVALID_PUBLIC_STATE_ID);
       for (std::size_t index = 0; index < registry.runouts.size(); ++index) {
           registry.runouts[index].id = static_cast<runout_id>(index);
           registry.river_public_state_by_runout[index] = registry.runouts[index].river_public_state_id;
       }
       return registry;
    }

    [[nodiscard]] inline runout_terminal_table make_runout_terminal_table(std::span<const public_board_state> states)
    {
       runout_terminal_table table;
       public_state_id max_state_id = 0;
       for (const auto& state : states) {
           if (state.id != INVALID_PUBLIC_STATE_ID) {
               max_state_id = std::max(max_state_id, state.id);
           }
       }
       const auto mapping_size = std::max<std::size_t>(states.size(), static_cast<std::size_t>(max_state_id) + 1u);
       table.entry_id_by_public_state.assign(mapping_size, INVALID_PUBLIC_STATE_ID);
       for (const auto& state : states) {
           if (!state.is_terminal_river_state || state.board_cards == 0u) {
               continue;
           }
           const auto entry_index = static_cast<public_state_id>(table.entries.size());
           const auto river_board = board{state.board_cards};
           table.entries.push_back(runout_terminal_entry{
               .public_state = state.id,
               .river_board = river_board,
               .cache = make_river_terminal_cache(river_board)
           });
           table.entry_id_by_public_state[state.id] = entry_index;
       }
       return table;
    }

    /**
     * Validate enumerated chance storage against graph child ordering and blocker metadata.
     */
    [[nodiscard]] inline std::expected<void, chance_table_error> validate_chance_event_table(
        const game_graph& graph,
        const chance_event_table& table) noexcept
    {
        if (table.event_id_by_node.size() != graph.node_count) {
            return std::unexpected(chance_table_error{chance_table_error_kind::side_array_size_mismatch});
        }

        for (uint32_t event_id = 0; event_id < static_cast<uint32_t>(table.events.size()); ++event_id) {
            const auto& event = table.events[event_id];
            if (event.node_id >= graph.node_count || graph.node_types[event.node_id] != node_kind::chance) {
                return std::unexpected(chance_table_error{
                    chance_table_error_kind::invalid_event_node,
                    event.node_id,
                    event_id
                });
            }
            if (event.end_outcome() > table.outcomes.size()) {
                return std::unexpected(chance_table_error{
                    chance_table_error_kind::invalid_outcome_slice,
                    event.node_id,
                    event_id
                });
            }
            if (table.event_id_by_node[event.node_id] != event_id) {
                return std::unexpected(chance_table_error{
                    chance_table_error_kind::invalid_event_node,
                    event.node_id,
                    event_id
                });
            }
            if ((event.board_cards & event.dead_cards) != 0u) {
                return std::unexpected(chance_table_error{
                    chance_table_error_kind::dead_card_collision,
                    event.node_id,
                    event_id
                });
            }

            const auto edges = graph.out_edges(event.node_id);
            if (event.outcome_count != edges.size()) {
                return std::unexpected(chance_table_error{
                    chance_table_error_kind::outcome_count_mismatch,
                    event.node_id,
                    event_id
                });
            }
            if (event.kind != public_chance_event_kind::none
                && event.outcome_count != detail::public_chance_outcome_count(
                    event.kind,
                    event.board_cards,
                    event.dead_cards)) {
                return std::unexpected(chance_table_error{
                    chance_table_error_kind::outcome_count_mismatch,
                    event.node_id,
                    event_id
                });
            }

            const auto outcomes = table.event_outcomes(event);
            if (!detail::chance_probabilities_sum_to_one(outcomes)) {
                return std::unexpected(chance_table_error{
                    chance_table_error_kind::invalid_probability_sum,
                    event.node_id,
                    event_id
                });
            }

            for (uint32_t local_index = 0; local_index < event.outcome_count; ++local_index) {
                const auto& outcome = outcomes[local_index];
                const auto& edge = edges[local_index];
                const auto outcome_index = event.first_outcome + local_index;
                if (outcome.child_node != edge.child_node || outcome.action_index != edge.action_index) {
                    return std::unexpected(chance_table_error{
                        chance_table_error_kind::child_alignment_mismatch,
                        event.node_id,
                        event_id,
                        outcome_index
                    });
                }
                if (!outcome.legal) {
                    return std::unexpected(chance_table_error{
                        chance_table_error_kind::illegal_outcome,
                        event.node_id,
                        event_id,
                        outcome_index
                    });
                }
                if (outcome.board_partition_id == INVALID_BOARD_PARTITION) {
                    return std::unexpected(chance_table_error{
                        chance_table_error_kind::invalid_board_partition,
                        event.node_id,
                        event_id,
                        outcome_index
                    });
                }
                if ((outcome.cards & (outcome.dead_cards | event.dead_cards)) != 0u) {
                    return std::unexpected(chance_table_error{
                        chance_table_error_kind::dead_card_collision,
                        event.node_id,
                        event_id,
                        outcome_index
                    });
                }
                if ((outcome.cards & event.board_cards) != 0u) {
                    return std::unexpected(chance_table_error{
                        chance_table_error_kind::duplicate_board_card,
                        event.node_id,
                        event_id,
                        outcome_index
                    });
                }
                if (event.kind != public_chance_event_kind::none
                    && detail::card_count(outcome.cards) != public_chance_cards_to_deal(event.kind)) {
                    return std::unexpected(chance_table_error{
                        chance_table_error_kind::invalid_outcome_cards,
                        event.node_id,
                        event_id,
                        outcome_index
                    });
                }
            }
        }

        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            const auto event_id = table.event_id_by_node[node_id];
            if (graph.node_types[node_id] == node_kind::chance) {
                if (event_id == INVALID_CHANCE_EVENT || event_id >= table.events.size()) {
                    return std::unexpected(chance_table_error{
                        chance_table_error_kind::missing_chance_event,
                        node_id,
                        event_id
                    });
                }
            } else if (event_id != INVALID_CHANCE_EVENT) {
                return std::unexpected(chance_table_error{
                    chance_table_error_kind::unexpected_chance_event,
                    node_id,
                    event_id
                });
            }
        }

        return {};
    }

    /**
     * Build uniform enumerated outcomes for every graph chance node.
     */
    [[nodiscard]] inline chance_event_table make_uniform_chance_event_table(const game_graph& graph)
    {
        chance_event_table table;
        table.event_id_by_node.assign(graph.node_count, INVALID_CHANCE_EVENT);

        for (uint32_t node_id = 0; node_id < graph.node_count; ++node_id) {
            if (graph.node_types[node_id] != node_kind::chance) {
                continue;
            }

            const auto edges = graph.out_edges(node_id);
            const auto first_outcome = static_cast<uint32_t>(table.outcomes.size());
            const auto event_id = static_cast<uint32_t>(table.events.size());
            table.event_id_by_node[node_id] = event_id;
            table.events.push_back(chance_event{
                .node_id = node_id,
                .first_outcome = first_outcome,
                .outcome_count = static_cast<uint32_t>(edges.size())
            });

            const auto probability = edges.empty() ? 0.0f : 1.0f / static_cast<float>(edges.size());
            for (const auto& child_edge : edges) {
                table.outcomes.push_back(chance_outcome{
                    .child_node = child_edge.child_node,
                    .action_index = child_edge.action_index,
                    .probability = probability,
                    .board_partition_id = child_edge.action_index,
                    .outcome_id = static_cast<chance_outcome_id>(table.outcomes.size())
                });
            }
        }

        return table;
    }

    struct public_card_chance_event_config {
        uint32_t node_id = game_graph::INVALID_NODE;
        public_chance_event_kind kind = public_chance_event_kind::none;
        card_mask board_cards = 0;
        card_mask dead_cards = 0;
        uint32_t board_partition_base = 0;
    };

    /**
     * Enumerate blocker-safe public-card outcomes with equal probabilities.
     */
    [[nodiscard]] inline std::vector<chance_outcome> enumerate_public_card_outcomes(
        const card_mask board_cards,
        const card_mask dead_cards,
        const uint8_t cards_to_deal)
    {
        assert(cards_to_deal >= 1u && cards_to_deal <= 3u);
        assert((board_cards & dead_cards) == 0u);

        const auto blocked = board_cards | dead_cards;
        std::vector<card_mask> card_sets;

        if (cards_to_deal == 1u) {
            for (uint8_t card_id = 0; card_id < zeta::num_cards<zeta::default_deck>; ++card_id) {
                const auto card_bit = card_mask{1} << card_id;
                if ((blocked & card_bit) == 0u) {
                    card_sets.push_back(card_bit);
                }
            }
        } else if (cards_to_deal == 2u) {
            for (uint8_t first = 0; first < zeta::num_cards<zeta::default_deck>; ++first) {
                const auto first_bit = card_mask{1} << first;
                if ((blocked & first_bit) != 0u) {
                    continue;
                }
                for (uint8_t second = first + 1u; second < zeta::num_cards<zeta::default_deck>; ++second) {
                    const auto second_bit = card_mask{1} << second;
                    if ((blocked & second_bit) == 0u) {
                        card_sets.push_back(first_bit | second_bit);
                    }
                }
            }
        } else {
            for (uint8_t first = 0; first < zeta::num_cards<zeta::default_deck>; ++first) {
                const auto first_bit = card_mask{1} << first;
                if ((blocked & first_bit) != 0u) {
                    continue;
                }
                for (uint8_t second = first + 1u; second < zeta::num_cards<zeta::default_deck>; ++second) {
                    const auto second_bit = card_mask{1} << second;
                    if ((blocked & second_bit) != 0u) {
                        continue;
                    }
                    for (uint8_t third = second + 1u; third < zeta::num_cards<zeta::default_deck>; ++third) {
                        const auto third_bit = card_mask{1} << third;
                        if ((blocked & third_bit) == 0u) {
                            card_sets.push_back(first_bit | second_bit | third_bit);
                        }
                    }
                }
            }
        }

        std::vector<chance_outcome> outcomes;
        outcomes.reserve(card_sets.size());
        const auto probability = card_sets.empty() ? 0.0f : 1.0f / static_cast<float>(card_sets.size());
        for (uint32_t i = 0; i < static_cast<uint32_t>(card_sets.size()); ++i) {
            outcomes.push_back(chance_outcome{
                .action_index = static_cast<uint16_t>(i),
                .probability = probability,
                .board_partition_id = i,
                .outcome_id = i,
                .cards = card_sets[i],
                .dead_cards = dead_cards
            });
        }
        return outcomes;
    }

    [[nodiscard]] inline std::vector<chance_outcome> enumerate_public_card_outcomes(
        const public_chance_event_kind kind,
        const card_mask board_cards,
        const card_mask dead_cards)
    {
        const auto cards_to_deal = public_chance_cards_to_deal(kind);
        return cards_to_deal == 0u
            ? std::vector<chance_outcome>{}
            : enumerate_public_card_outcomes(board_cards, dead_cards, cards_to_deal);
    }

    [[nodiscard]] inline std::vector<chance_outcome> enumerate_flop_outcomes(const card_mask dead_cards)
    {
        return enumerate_public_card_outcomes(0, dead_cards, 3);
    }

    [[nodiscard]] inline std::vector<chance_outcome> enumerate_turn_outcomes(
        const card_mask flop_cards,
        const card_mask dead_cards)
    {
        return enumerate_public_card_outcomes(flop_cards, dead_cards, 1);
    }

    [[nodiscard]] inline std::vector<chance_outcome> enumerate_river_outcomes(
        const card_mask turn_board_cards,
        const card_mask dead_cards)
    {
        return enumerate_public_card_outcomes(turn_board_cards, dead_cards, 1);
    }

    /**
     * Suit relabeling used by public-card isomorphism. Entry s carries the canonical
     * suit that deck suit s maps to. Suits occupy contiguous 13-bit rank lanes in a
     * card_mask (spades=0, hearts=1, diamonds=2, clubs=3), so a permutation simply
     * relabels those lanes.
     */
    using suit_permutation = std::array<uint8_t, 4>;

    [[nodiscard]] inline constexpr suit_permutation identity_suit_permutation() noexcept
    {
        return suit_permutation{0, 1, 2, 3};
    }

    [[nodiscard]] inline constexpr bool is_identity_suit_permutation(const suit_permutation& permutation) noexcept
    {
        return permutation[0] == 0u && permutation[1] == 1u
            && permutation[2] == 2u && permutation[3] == 3u;
    }

    /** Relabel the suit of every card in the mask according to the permutation. */
    [[nodiscard]] inline card_mask apply_suit_permutation(
        const card_mask mask,
        const suit_permutation& permutation) noexcept
    {
        constexpr uint64_t rank_lane = 0x1FFFull; // 13 rank bits per suit lane
        uint64_t result = 0;
        for (uint8_t suit = 0; suit < 4u; ++suit) {
            const auto lane = (static_cast<uint64_t>(mask) >> (static_cast<uint64_t>(suit) * 13u)) & rank_lane;
            result |= lane << (static_cast<uint64_t>(permutation[suit]) * 13u);
        }
        return static_cast<card_mask>(result);
    }

    /**
     * One canonical public-card outcome.
     *
     * Under exact enumeration this is a 1:1 image of a raw chance outcome. The type
     * also carries everything a suit-isomorphic reduction needs: the combined
     * probability of every collapsed member, the public-card suit permutation that
     * maps a member's board onto the representative, and (through that permutation)
     * the induced private-combo relabeling. This keeps the isomorphism contract
     * fully expressed even while exact enumeration is the only active lowering path.
     */
    struct canonical_public_outcome {
        card_mask representative_cards = 0;                       /**< Dealt cards of the representative. */
        float probability = 0.0f;                                /**< Summed probability of collapsed members. */
        suit_permutation public_permutation = identity_suit_permutation();
        std::vector<chance_outcome_id> members{};                /**< Raw outcome ids folded into this one. */
    };

    enum class public_card_isomorphism_mode : uint8_t {
        exact = 0 /**< No collapsing: one canonical outcome per raw outcome. */
    };

    /**
     * Canonicalize enumerated public-card outcomes.
     *
     * Exact mode performs no reduction: every raw outcome becomes its own canonical
     * outcome with the identity suit permutation and its own probability. This is the
     * correctness baseline the multi-street lowering builds on; a suit-isomorphic
     * collapsing mode is introduced later without reshaping this contract.
     */
    [[nodiscard]] inline std::vector<canonical_public_outcome> canonicalize_public_card_outcomes(
        const std::span<const chance_outcome> outcomes,
        const public_card_isomorphism_mode mode = public_card_isomorphism_mode::exact)
    {
        std::vector<canonical_public_outcome> canonical;
        switch (mode) {
            case public_card_isomorphism_mode::exact:
                canonical.reserve(outcomes.size());
                for (const auto& outcome : outcomes) {
                    canonical.push_back(canonical_public_outcome{
                        .representative_cards = outcome.cards,
                        .probability = outcome.probability,
                        .public_permutation = identity_suit_permutation(),
                        .members = {outcome.outcome_id}
                    });
                }
                break;
        }
        return canonical;
    }

    [[nodiscard]] inline std::expected<chance_event_table, chance_table_error> make_public_card_chance_event_table(
        const game_graph& graph,
        const std::span<const public_card_chance_event_config> configs)
    {
        chance_event_table table;
        table.event_id_by_node.assign(graph.node_count, INVALID_CHANCE_EVENT);

        for (const auto& config : configs) {
            if (config.node_id >= graph.node_count
                || graph.node_types[config.node_id] != node_kind::chance
                || config.kind == public_chance_event_kind::none) {
                return std::unexpected(chance_table_error{
                    chance_table_error_kind::invalid_event_node,
                    config.node_id
                });
            }
            if (table.event_id_by_node[config.node_id] != INVALID_CHANCE_EVENT) {
                return std::unexpected(chance_table_error{
                    chance_table_error_kind::unexpected_chance_event,
                    config.node_id,
                    table.event_id_by_node[config.node_id]
                });
            }
            if ((config.board_cards & config.dead_cards) != 0u) {
                return std::unexpected(chance_table_error{
                    chance_table_error_kind::dead_card_collision,
                    config.node_id
                });
            }

            auto outcomes = enumerate_public_card_outcomes(config.kind, config.board_cards, config.dead_cards);
            const auto edges = graph.out_edges(config.node_id);
            if (outcomes.size() != edges.size()) {
                return std::unexpected(chance_table_error{
                    chance_table_error_kind::outcome_count_mismatch,
                    config.node_id
                });
            }

            const auto event_id = static_cast<uint32_t>(table.events.size());
            const auto first_outcome = static_cast<uint32_t>(table.outcomes.size());
            table.event_id_by_node[config.node_id] = event_id;
            table.events.push_back(chance_event{
                .node_id = config.node_id,
                .first_outcome = first_outcome,
                .outcome_count = static_cast<uint32_t>(outcomes.size()),
                .kind = config.kind,
                .board_cards = config.board_cards,
                .dead_cards = config.dead_cards
            });

            for (uint32_t local_index = 0; local_index < static_cast<uint32_t>(outcomes.size()); ++local_index) {
                auto& outcome = outcomes[local_index];
                outcome.child_node = edges[local_index].child_node;
                outcome.action_index = edges[local_index].action_index;
                outcome.board_partition_id += config.board_partition_base;
                outcome.outcome_id = first_outcome + local_index;
                outcome.dead_cards = config.dead_cards;
                table.outcomes.push_back(outcome);
            }
        }

        if (auto result = validate_chance_event_table(graph, table); !result) {
            return std::unexpected(result.error());
        }
        return table;
    }

    /**
     * Number of board partitions referenced by enumerated chance outcomes.
     */
    [[nodiscard]] inline uint32_t chance_board_partition_count(const chance_event_table& table) noexcept
    {
        uint32_t partition_count = 0;
        for (const auto& outcome : table.outcomes) {
            if (outcome.board_partition_id != INVALID_BOARD_PARTITION) {
                partition_count = std::max(partition_count, outcome.board_partition_id + 1u);
            }
        }
        return partition_count;
    }

}
