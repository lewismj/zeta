#pragma once

#include "board.h"
#include "eval.h"

#include <algorithm>
#include <array>
#include <bit>
#include <cstdint>

namespace zeta::holdem {

    enum class pair_source : uint8_t {
        none,
        hole_pair,
        hole_board_pair,
        board_only_pair
    };

    enum class pair_position : uint8_t {
        none,
        top_pair,
        middle_pair,
        bottom_pair,
        pocket_pair_below_board,
        overpair,
        underpair
    };

    enum class kicker_quality : uint8_t {
        none,
        top,
        strong,
        medium,
        weak
    };

    enum class draw_flags : uint16_t {
        none = 0,
        open_ended_straight_draw = 1u << 0,
        gutshot_straight_draw = 1u << 1,
        backdoor_straight_draw = 1u << 2,
        flush_draw = 1u << 3
    };

    enum class blocker_flags : uint16_t {
        none = 0,
        nut_flush_blocker = 1u << 0,
        second_nut_blocker = 1u << 1
    };

    struct straight_draw_info {
        uint16_t window_mask = 0;
        uint16_t immediate_completion_ranks = 0;
        bool backdoor_straight = false;
        draw_flags draws = draw_flags::none;
    };

    struct hand_category_classification {
        hand_category made_hand_tier = hand_category::high_card;
        pair_source source = pair_source::none;
        pair_position pair_pos = pair_position::none;
        kicker_quality kicker = kicker_quality::none;
        draw_flags draws = draw_flags::none;
        blocker_flags blockers = blocker_flags::none;
    };

    static_assert(sizeof(hand_category_classification) == 8);

    namespace detail {
        [[nodiscard]] constexpr uint16_t rank_bit(const int rank) noexcept
        {
            return static_cast<uint16_t>(1u << rank);
        }

        [[nodiscard]] inline uint16_t ranks_mask(const card_mask mask) noexcept
        {
            uint16_t bits = 0;
            for (int rank = 0; rank < 13; ++rank) {
                for (int suit = 0; suit < 4; ++suit) {
                    if ((mask & (card_mask{1} << (suit * 13 + rank))) != 0) {
                        bits = static_cast<uint16_t>(bits | rank_bit(rank));
                        break;
                    }
                }
            }
            return bits;
        }

        [[nodiscard]] inline bool board_rank_pairs(const board& public_board) noexcept
        {
            return std::popcount(ranks_mask(public_board.mask)) != public_board.size();
        }

        [[nodiscard]] inline int highest_rank(const uint16_t bits) noexcept
        {
            for (int rank = 12; rank >= 0; --rank) {
                if ((bits & rank_bit(rank)) != 0) {
                    return rank;
                }
            }
            return -1;
        }

        [[nodiscard]] inline int lowest_rank(const uint16_t bits) noexcept
        {
            for (int rank = 0; rank < 13; ++rank) {
                if ((bits & rank_bit(rank)) != 0) {
                    return rank;
                }
            }
            return -1;
        }

        [[nodiscard]] inline std::array<uint8_t, 13> rank_counts(const card_mask mask) noexcept
        {
            std::array<uint8_t, 13> counts{};
            for (int rank = 0; rank < 13; ++rank) {
                for (int suit = 0; suit < 4; ++suit) {
                    if ((mask & (card_mask{1} << (suit * 13 + rank))) != 0) {
                        ++counts[rank];
                    }
                }
            }
            return counts;
        }

        [[nodiscard]] inline int nth_board_rank_desc(const uint16_t bits, const int target_index) noexcept
        {
            int index = 0;
            for (int rank = 12; rank >= 0; --rank) {
                if ((bits & rank_bit(rank)) != 0) {
                    if (index == target_index) {
                        return rank;
                    }
                    ++index;
                }
            }
            return -1;
        }

        [[nodiscard]] inline pair_source classify_pair_source(const card_mask hole, const board& public_board) noexcept
        {
            const auto hole_bits = ranks_mask(hole);
            const auto board_bits = ranks_mask(public_board.mask);
            const bool pocket_pair = std::popcount(hole_bits) == 1;
            const bool board_pair = board_rank_pairs(public_board);
            if (pocket_pair && !board_pair) {
                return pair_source::hole_pair;
            }
            if ((hole_bits & board_bits) != 0) {
                return pair_source::hole_board_pair;
            }
            if (!pocket_pair && board_pair) {
                return pair_source::board_only_pair;
            }
            return pair_source::none;
        }

        [[nodiscard]] inline pair_position classify_pair_position(const card_mask hole, const board& public_board) noexcept
        {
            const auto hole_mask = ranks_mask(hole);
            const auto board_mask = ranks_mask(public_board.mask);

            if (std::popcount(hole_mask) == 1) {
                const int hole_rank = highest_rank(hole_mask);
                const int highest_board = highest_rank(board_mask);
                const int lowest_board = lowest_rank(board_mask);

                if (hole_rank > highest_board) {
                    return pair_position::overpair;
                }
                if (hole_rank < lowest_board) {
                    return pair_position::pocket_pair_below_board;
                }
                if (hole_rank < highest_board && hole_rank > lowest_board) {
                    return pair_position::underpair;
                }
                return pair_position::none;
            }

            const auto matched = static_cast<uint16_t>(hole_mask & board_mask);
            if (matched == 0) {
                return pair_position::none;
            }
            if ((matched & rank_bit(nth_board_rank_desc(board_mask, 0))) != 0) {
                return pair_position::top_pair;
            }
            if ((matched & rank_bit(nth_board_rank_desc(board_mask, 1))) != 0) {
                return pair_position::middle_pair;
            }
            return pair_position::bottom_pair;
        }

        [[nodiscard]] inline kicker_quality classify_kicker(const hand_category tier, const card_mask hole, const board& public_board) noexcept
        {
            if (tier != hand_category::pair && tier != hand_category::two_pair && tier != hand_category::trips && tier != hand_category::high_card) {
                return kicker_quality::none;
            }
            const auto counts = rank_counts(hole | public_board.mask);
            int primary_group = -1;
            int secondary_group = -1;
            if (tier == hand_category::high_card) {
                for (int rank = 12; rank >= 0; --rank) {
                    if (counts[rank] != 0) {
                        primary_group = rank;
                        break;
                    }
                }
            } else if (tier == hand_category::pair || tier == hand_category::trips) {
                const uint8_t needed = tier == hand_category::pair ? 2 : 3;
                for (int rank = 12; rank >= 0; --rank) {
                    if (counts[rank] >= needed) {
                        primary_group = rank;
                        break;
                    }
                }
            } else if (tier == hand_category::two_pair) {
                for (int rank = 12; rank >= 0; --rank) {
                    if (counts[rank] >= 2) {
                        if (primary_group < 0) {
                            primary_group = rank;
                        } else {
                            secondary_group = rank;
                            break;
                        }
                    }
                }
            }

            int kicker_rank = -1;
            int kicker_ordinal = 0;
            for (int rank = 12; rank >= 0; --rank) {
                if (rank == primary_group || rank == secondary_group) {
                    continue;
                }
                if (counts[rank] != 0 && kicker_rank < 0) {
                    kicker_rank = rank;
                }
                if (kicker_rank == rank) {
                    break;
                }
                ++kicker_ordinal;
            }
            if (kicker_rank < 0) {
                return kicker_quality::none;
            }
            if (kicker_ordinal == 0) {
                return kicker_quality::top;
            }
            if (kicker_ordinal == 1) {
                return kicker_quality::strong;
            }
            if (kicker_ordinal <= 3) {
                return kicker_quality::medium;
            }
            return kicker_quality::weak;
        }

        [[nodiscard]] inline blocker_flags classify_blockers(const card_mask hole, const board& public_board, const hand_category tier) noexcept
        {
            if (tier == hand_category::flush || tier == hand_category::straight_flush) {
                return blocker_flags::none;
            }
            const int required_board_suit_count = public_board.board_street() == street::flop ? 2 : 3;
            for (int suit = 0; suit < 4; ++suit) {
                int board_cards = 0;
                int nut_rank = -1;
                int second_nut_rank = -1;
                for (int rank = 12; rank >= 0; --rank) {
                    const auto bit = card_mask{1} << (suit * 13 + rank);
                    if ((public_board.mask & bit) != 0) {
                        ++board_cards;
                    } else {
                        if (nut_rank < 0) {
                            nut_rank = rank;
                        } else if (second_nut_rank < 0) {
                            second_nut_rank = rank;
                        }
                    }
                }
                if (board_cards < required_board_suit_count) {
                    continue;
                }
                if (nut_rank >= 0 && (hole & (card_mask{1} << (suit * 13 + nut_rank))) != 0) {
                    return blocker_flags::nut_flush_blocker;
                }
                if (second_nut_rank >= 0 && (hole & (card_mask{1} << (suit * 13 + second_nut_rank))) != 0) {
                    return blocker_flags::second_nut_blocker;
                }
            }
            return blocker_flags::none;
        }

        [[nodiscard]] inline draw_flags classify_flush_draws(const card_mask hole, const board& public_board, const hand_category tier) noexcept
        {
            if (tier == hand_category::flush || tier == hand_category::straight_flush) {
                return draw_flags::none;
            }
            for (int suit = 0; suit < 4; ++suit) {
                int suited_cards = 0;
                for (int rank = 0; rank < 13; ++rank) {
                    const auto bit = card_mask{1} << (suit * 13 + rank);
                    if (((hole | public_board.mask) & bit) != 0) {
                        ++suited_cards;
                    }
                }
                if (suited_cards == 4) {
                    return draw_flags::flush_draw;
                }
            }
            return draw_flags::none;
        }

        [[nodiscard]] inline bool has_draw_rank(const uint16_t bits, const int rank) noexcept
        {
            return (bits & rank_bit(rank)) != 0;
        }

        [[nodiscard]] inline bool has_window(const uint16_t bits, const int window) noexcept
        {
            const int start = window == 0 ? 12 : window - 1;
            for (int offset = 0; offset < 5; ++offset) {
                const int rank = (start + offset) % 13;
                if (!has_draw_rank(bits, rank)) {
                    return false;
                }
            }
            return true;
        }

        [[nodiscard]] inline hand_category classify_made_hand(const card_mask cards) noexcept
        {
            const auto counts = rank_counts(cards);
            const auto bits = ranks_mask(cards);
            bool straight = false;
            for (int window = 0; window < 10 && !straight; ++window) {
                straight = has_window(bits, window);
            }

            bool flush = false;
            bool straight_flush = false;
            for (int suit = 0; suit < 4; ++suit) {
                int suited_count = 0;
                uint16_t suited_bits = 0;
                for (int rank = 0; rank < 13; ++rank) {
                    const auto bit = card_mask{1} << (suit * 13 + rank);
                    if ((cards & bit) != 0) {
                        ++suited_count;
                        suited_bits = static_cast<uint16_t>(suited_bits | rank_bit(rank));
                    }
                }
                if (suited_count >= 5) {
                    flush = true;
                    for (int window = 0; window < 10 && !straight_flush; ++window) {
                        straight_flush = has_window(suited_bits, window);
                    }
                }
            }
            if (straight_flush) {
                return hand_category::straight_flush;
            }

            int pairs = 0;
            bool trips = false;
            for (const auto count : counts) {
                if (count >= 4) {
                    return hand_category::quads;
                }
                if (count >= 3) {
                    trips = true;
                } else if (count >= 2) {
                    ++pairs;
                }
            }
            if (trips && pairs > 0) {
                return hand_category::full_house;
            }
            if (flush) {
                return hand_category::flush;
            }
            if (straight) {
                return hand_category::straight;
            }
            if (trips) {
                return hand_category::trips;
            }
            if (pairs >= 2) {
                return hand_category::two_pair;
            }
            if (pairs == 1) {
                return hand_category::pair;
            }
            return hand_category::high_card;
        }

        [[nodiscard]] inline bool has_live_rank_card(const card_mask cards, const int rank) noexcept
        {
            for (int suit = 0; suit < 4; ++suit) {
                if ((cards & (card_mask{1} << (suit * 13 + rank))) == 0) {
                    return true;
                }
            }
            return false;
        }
    }

    [[nodiscard]] inline straight_draw_info evaluate_straight_draw(card_mask cards, street current_street) noexcept
    {
        straight_draw_info info{};
        const auto bits = detail::ranks_mask(cards);
        uint16_t backdoor_missing_pairs = 0;
        for (int window = 0; window < 10; ++window) {
            if (detail::has_window(bits, window)) {
                info.window_mask = static_cast<uint16_t>(info.window_mask | detail::rank_bit(window));
                continue;
            }
            int missing = 0;
            int missing_rank = -1;
            std::array<int, 2> missing_ranks{-1, -1};
            for (int offset = 0; offset < 5; ++offset) {
                const int rank = (window == 0 ? 12 : window - 1 + offset) % 13;
                if (!detail::has_draw_rank(bits, rank)) {
                    if (missing < 2) {
                        missing_ranks[missing] = rank;
                    }
                    ++missing;
                    missing_rank = rank;
                }
            }
            if (missing == 1) {
                info.immediate_completion_ranks = static_cast<uint16_t>(info.immediate_completion_ranks | detail::rank_bit(missing_rank));
            } else if (missing == 2 && current_street == street::flop
                && detail::has_live_rank_card(cards, missing_ranks[0])
                && detail::has_live_rank_card(cards, missing_ranks[1])) {
                info.backdoor_straight = true;
                backdoor_missing_pairs = static_cast<uint16_t>(backdoor_missing_pairs | detail::rank_bit(window));
            }
        }
        if (info.window_mask != 0) {
            info.immediate_completion_ranks = 0;
            info.backdoor_straight = false;
            info.draws = draw_flags::none;
            return info;
        }
        if (std::popcount(info.immediate_completion_ranks) >= 2) {
            info.draws = static_cast<draw_flags>(static_cast<uint16_t>(info.draws) | static_cast<uint16_t>(draw_flags::open_ended_straight_draw));
        } else if (info.immediate_completion_ranks != 0) {
            info.draws = static_cast<draw_flags>(static_cast<uint16_t>(info.draws) | static_cast<uint16_t>(draw_flags::gutshot_straight_draw));
        }
        if (backdoor_missing_pairs != 0) {
            info.draws = static_cast<draw_flags>(static_cast<uint16_t>(info.draws) | static_cast<uint16_t>(draw_flags::backdoor_straight_draw));
        }
        return info;
    }

    [[nodiscard]] inline hand_category_classification categorize_hand(card_mask hole, const board& public_board) noexcept
    {
        const auto combined = hole | public_board.mask;
        hand_category_classification out{};
        out.made_hand_tier = detail::classify_made_hand(combined);
        if (out.made_hand_tier == hand_category::pair) {
            out.source = detail::classify_pair_source(hole, public_board);
            out.pair_pos = detail::classify_pair_position(hole, public_board);
        }
        out.kicker = detail::classify_kicker(out.made_hand_tier, hole, public_board);
        const auto straight_draws = evaluate_straight_draw(combined, public_board.board_street()).draws;
        out.draws = static_cast<draw_flags>(static_cast<uint16_t>(straight_draws) | static_cast<uint16_t>(detail::classify_flush_draws(hole, public_board, out.made_hand_tier)));
        out.blockers = detail::classify_blockers(hole, public_board, out.made_hand_tier);
        return out;
    }

}
