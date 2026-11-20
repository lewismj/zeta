#include "cfr/chance/chance.h"

namespace zeta::holdem::cfr {

    namespace detail {

        bool chance_probabilities_sum_to_one(
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

        uint32_t card_count(const card_mask cards) noexcept
        {
            return static_cast<uint32_t>(std::popcount(static_cast<uint64_t>(cards)));
        }

        uint32_t combination_count(uint32_t n, const uint8_t k) noexcept
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

        uint32_t public_chance_outcome_count(
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

    std::expected<void, public_state_registry_error> validate_public_state_registry(
       const public_state_registry& registry,
       const uint32_t node_count) noexcept
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

    std::expected<void, runout_registry_error> validate_runout_registry(
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

    runout_terminal_table make_runout_terminal_table(std::span<const public_board_state> states)
    {
       return make_runout_terminal_table(states, [](const std::size_t count, auto&& build) {
           for (std::size_t index = 0; index < count; ++index) {
               build(index);
           }
       });
    }

    std::expected<void, chance_table_error> validate_chance_event_table(
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
            if (event.kind != public_chance_event_kind::none) {
                const auto full_outcome_count = detail::public_chance_outcome_count(
                    event.kind,
                    event.board_cards,
                    event.dead_cards);
                // A canonical (suit-isomorphism-collapsed) event carries one outcome per
                // equivalence class, so it must never exceed - and generally is fewer than
                // - the full enumeration count. A non-canonical event must enumerate every
                // legal card exactly.
                const bool count_ok = event.is_canonical
                    ? (event.outcome_count > 0u && event.outcome_count <= full_outcome_count)
                    : (event.outcome_count == full_outcome_count);
                if (!count_ok) {
                    return std::unexpected(chance_table_error{
                        chance_table_error_kind::outcome_count_mismatch,
                        event.node_id,
                        event_id
                    });
                }
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

    std::vector<chance_outcome> enumerate_public_card_outcomes(
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

    card_mask apply_suit_permutation(
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

    const std::array<suit_permutation, 24>& all_suit_permutations() noexcept
    {
        static const std::array<suit_permutation, 24> permutations = [] {
            std::array<suit_permutation, 24> out{};
            std::array<uint8_t, 4> base{0u, 1u, 2u, 3u};
            std::size_t index = 0;
            do {
                out[index++] = suit_permutation{base[0], base[1], base[2], base[3]};
            } while (std::next_permutation(base.begin(), base.end()));
            return out;
        }();
        return permutations;
    }

    combination_index apply_suit_permutation_to_combo(
        const combination_index combo,
        const suit_permutation& permutation) noexcept
    {
        static const auto index_by_mask = [] {
            std::unordered_map<card_mask, combination_index> table;
            table.reserve(combination_count * 2u);
            for (combination_index i = 0; i < static_cast<combination_index>(combination_count); ++i) {
                table.emplace(combination_masks[i], i);
            }
            return table;
        }();
        const auto permuted = apply_suit_permutation(combination_masks[combo], permutation);
        const auto found = index_by_mask.find(permuted);
        return found == index_by_mask.end()
            ? static_cast<combination_index>(combination_count)
            : found->second;
    }

    std::vector<canonical_public_outcome> canonicalize_public_card_outcomes(
        const std::span<const chance_outcome> outcomes,
        const card_mask board,
        const card_mask dead_cards,
        const public_card_isomorphism_mode mode)
    {
        std::vector<canonical_public_outcome> canonical;
        if (mode == public_card_isomorphism_mode::exact) {
            canonical.reserve(outcomes.size());
            for (const auto& outcome : outcomes) {
                canonical.push_back(canonical_public_outcome{
                    .representative_cards = outcome.cards,
                    .probability = outcome.probability,
                    .members = {canonical_outcome_member{
                        .outcome_id = outcome.outcome_id,
                        .cards = outcome.cards,
                        .permutation = identity_suit_permutation()
                    }}
                });
            }
            return canonical;
        }

        std::vector<suit_permutation> stabilizer;
        for (const auto& permutation : all_suit_permutations()) {
            if (apply_suit_permutation(board, permutation) == board
                && apply_suit_permutation(dead_cards, permutation) == dead_cards) {
                stabilizer.push_back(permutation);
            }
        }

        std::unordered_map<card_mask, std::size_t> index_by_representative;
        canonical.reserve(outcomes.size());
        for (const auto& outcome : outcomes) {
            card_mask representative = outcome.cards;
            suit_permutation to_representative = identity_suit_permutation();
            for (const auto& permutation : stabilizer) {
                const auto mapped = apply_suit_permutation(outcome.cards, permutation);
                if (mapped < representative) {
                    representative = mapped;
                    to_representative = permutation;
                }
            }
            canonical_outcome_member member{
                .outcome_id = outcome.outcome_id,
                .cards = outcome.cards,
                .permutation = to_representative
            };
            const auto found = index_by_representative.find(representative);
            if (found == index_by_representative.end()) {
                index_by_representative.emplace(representative, canonical.size());
                canonical.push_back(canonical_public_outcome{
                    .representative_cards = representative,
                    .probability = outcome.probability,
                    .members = {member}
                });
            } else {
                auto& entry = canonical[found->second];
                entry.probability += outcome.probability;
                entry.members.push_back(member);
            }
        }
        return canonical;
    }

}
