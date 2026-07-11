#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <string_view>

#include "range.h"

namespace zeta::holdem {

    enum class range_parse_error_code : uint8_t {
        none,
        expected_rank,
        expected_suit,
        expected_term,
        expected_comma,
        invalid_exact_combo,
        invalid_plus,
        invalid_range,
        invalid_weight
    };

    struct range_parse_error {
        range_parse_error_code code = range_parse_error_code::none;
        std::size_t position = 0;
    };

    struct range_parse_result {
        hand_range range;
        range_parse_error error;

        [[nodiscard]] constexpr bool ok() const noexcept {
            return error.code == range_parse_error_code::none;
        }
    };

    namespace detail {

        enum class hand_class_kind : uint8_t {
            pair,
            non_pair,
            exact_combo
        };

        enum class suit_mode : uint8_t {
            both,
            suited,
            offsuit
        };

        struct hand_class {
            hand_class_kind kind{};
            uint8_t high_rank{};
            uint8_t low_rank{};
            uint8_t first_suit{};
            uint8_t second_suit{};
            suit_mode mode = suit_mode::both;
        };

        [[nodiscard]] constexpr bool is_space(const char c) noexcept {
            return c == ' ' || c == '\t' || c == '\n' || c == '\r';
        }

        [[nodiscard]] constexpr char upper_ascii(const char c) noexcept {
            return c >= 'a' && c <= 'z' ? static_cast<char>(c - ('a' - 'A')) : c;
        }

        [[nodiscard]] constexpr int parse_rank_char(const char c) noexcept {
            const char upper = upper_ascii(c);
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

        [[nodiscard]] constexpr int parse_suit_char(const char c) noexcept {
            switch (upper_ascii(c)) {
                case 'S': return static_cast<int>(suit::spades);
                case 'H': return static_cast<int>(suit::hearts);
                case 'D': return static_cast<int>(suit::diamonds);
                case 'C': return static_cast<int>(suit::clubs);
                default: return -1;
            }
        }

        // Matches make_combination_masks(): high rank descending, then first suit,
        // same-rank suit pairs, then lower ranks by descending rank and suit.
        [[nodiscard]] constexpr std::array<combination_index, 13> make_combo_rank_bases() noexcept {
            std::array<combination_index, 13> bases{};
            combination_index base = 0;
            for (int rank = 12; rank >= 0; --rank) {
                bases[rank] = base;
                base = static_cast<combination_index>(base + 6 + 16 * rank);
            }
            return bases;
        }

        inline constexpr auto combo_rank_bases = make_combo_rank_bases();

        [[nodiscard]] inline_always combination_index combo_index_from_cards(
            uint8_t first_rank,
            uint8_t first_suit,
            uint8_t second_rank,
            uint8_t second_suit
        ) noexcept {
            if (first_rank < second_rank || (first_rank == second_rank && first_suit > second_suit)) {
                const uint8_t rank = first_rank;
                const uint8_t suit = first_suit;
                first_rank = second_rank;
                first_suit = second_suit;
                second_rank = rank;
                second_suit = suit;
            }

            auto index = combo_rank_bases[first_rank];
            for (uint8_t suit_index = 0; suit_index < first_suit; ++suit_index) {
                index = static_cast<combination_index>(index + 3 - suit_index + 4 * first_rank);
            }

            if (first_rank == second_rank) {
                return static_cast<combination_index>(index + second_suit - first_suit - 1);
            }

            return static_cast<combination_index>(
                index + 3 - first_suit + 4 * (first_rank - second_rank - 1) + second_suit
            );
        }

        inline_always void set_combo_by_cards(
            hand_range& out,
            const uint8_t first_rank,
            const uint8_t first_suit,
            const uint8_t second_rank,
            const uint8_t second_suit,
            const combo_weight weight
        ) noexcept {
            out[combo_index_from_cards(first_rank, first_suit, second_rank, second_suit)] = weight;
        }

        inline_hint void set_pair(hand_range& out, const uint8_t rank_index, const combo_weight weight) noexcept {
            for (uint8_t first_suit = 0; first_suit < 3; ++first_suit) {
                for (uint8_t second_suit = static_cast<uint8_t>(first_suit + 1); second_suit < 4; ++second_suit) {
                    set_combo_by_cards(out, rank_index, first_suit, rank_index, second_suit, weight);
                }
            }
        }

        inline_hint void set_suited(hand_range& out, const uint8_t high_rank, const uint8_t low_rank, const combo_weight weight) noexcept {
            for (uint8_t suit_index = 0; suit_index < 4; ++suit_index) {
                set_combo_by_cards(out, high_rank, suit_index, low_rank, suit_index, weight);
            }
        }

        inline_hint void set_offsuit(hand_range& out, const uint8_t high_rank, const uint8_t low_rank, const combo_weight weight) noexcept {
            for (uint8_t high_suit = 0; high_suit < 4; ++high_suit) {
                for (uint8_t low_suit = 0; low_suit < 4; ++low_suit) {
                    if (high_suit != low_suit) {
                        set_combo_by_cards(out, high_rank, high_suit, low_rank, low_suit, weight);
                    }
                }
            }
        }

        inline_hint void set_non_pair(
            hand_range& out,
            const uint8_t high_rank,
            const uint8_t low_rank,
            const suit_mode mode,
            const combo_weight weight
        ) noexcept {
            if (mode != suit_mode::offsuit) {
                set_suited(out, high_rank, low_rank, weight);
            }
            if (mode != suit_mode::suited) {
                set_offsuit(out, high_rank, low_rank, weight);
            }
        }

        inline_hint void expand_class(hand_range& out, const hand_class hand, const combo_weight weight) noexcept {
            switch (hand.kind) {
                case hand_class_kind::pair:
                    set_pair(out, hand.high_rank, weight);
                    return;
                case hand_class_kind::non_pair:
                    set_non_pair(out, hand.high_rank, hand.low_rank, hand.mode, weight);
                    return;
                case hand_class_kind::exact_combo:
                    set_combo_by_cards(out, hand.high_rank, hand.first_suit, hand.low_rank, hand.second_suit, weight);
                    return;
            }
        }

        class range_parser {
        public:
            explicit constexpr range_parser(const std::string_view input) noexcept
                : input_(input) {}

            [[nodiscard]] range_parse_result parse() noexcept {
                range_parse_result result{};
                skip_ws();
                if (eof()) {
                    fail(result, range_parse_error_code::expected_term);
                    return result;
                }

                while (!eof()) {
                    if (!parse_term(result)) {
                        return result;
                    }

                    skip_ws();
                    if (eof()) {
                        return result;
                    }
                    if (!match(',')) {
                        fail(result, range_parse_error_code::expected_comma);
                        return result;
                    }
                    skip_ws();
                    if (eof()) {
                        fail(result, range_parse_error_code::expected_term);
                        return result;
                    }
                }

                return result;
            }

        private:
            std::string_view input_;
            std::size_t pos_ = 0;

            [[nodiscard]] constexpr bool eof() const noexcept {
                return pos_ >= input_.size();
            }

            [[nodiscard]] constexpr char peek() const noexcept {
                return eof() ? '\0' : input_[pos_];
            }

            constexpr char consume() noexcept {
                return eof() ? '\0' : input_[pos_++];
            }

            [[nodiscard]] constexpr bool match(const char c) noexcept {
                if (peek() == c) {
                    ++pos_;
                    return true;
                }
                return false;
            }

            constexpr void skip_ws() noexcept {
                while (!eof() && is_space(peek())) {
                    ++pos_;
                }
            }

            void fail(range_parse_result& result, const range_parse_error_code code) const noexcept {
                result.error = range_parse_error{.code = code, .position = pos_};
            }

            [[nodiscard]] bool parse_rank(uint8_t& rank_out, range_parse_result& result) noexcept {
                if (eof()) {
                    fail(result, range_parse_error_code::expected_rank);
                    return false;
                }
                const int rank = parse_rank_char(consume());
                if (rank < 0) {
                    fail(result, range_parse_error_code::expected_rank);
                    return false;
                }
                rank_out = static_cast<uint8_t>(rank);
                return true;
            }

            [[nodiscard]] bool parse_suit(uint8_t& suit_out, range_parse_result& result) noexcept {
                if (eof()) {
                    fail(result, range_parse_error_code::expected_suit);
                    return false;
                }
                const int suit_index = parse_suit_char(consume());
                if (suit_index < 0) {
                    fail(result, range_parse_error_code::expected_suit);
                    return false;
                }
                suit_out = static_cast<uint8_t>(suit_index);
                return true;
            }

            [[nodiscard]] bool parse_weight(combo_weight& weight_out, range_parse_result& result) noexcept {
                skip_ws();
                if (!match(':')) {
                    weight_out = 1.0f;
                    return true;
                }
                skip_ws();
                const std::size_t start = pos_;

                combo_weight integer = 0.0f;
                bool any_digit = false;
                while (!eof() && peek() >= '0' && peek() <= '9') {
                    any_digit = true;
                    integer = integer * 10.0f + static_cast<combo_weight>(consume() - '0');
                }

                combo_weight fractional = 0.0f;
                combo_weight scale = 1.0f;
                if (match('.')) {
                    while (!eof() && peek() >= '0' && peek() <= '9') {
                        any_digit = true;
                        scale *= 0.1f;
                        fractional += static_cast<combo_weight>(consume() - '0') * scale;
                    }
                }

                if (!any_digit || pos_ == start) {
                    fail(result, range_parse_error_code::invalid_weight);
                    return false;
                }

                weight_out = integer + fractional;
                skip_ws();
                const char c = peek();
                if (c != '\0' && c != ',') {
                    fail(result, range_parse_error_code::invalid_weight);
                    return false;
                }
                return true;
            }

            [[nodiscard]] bool parse_class(hand_class& hand_out, range_parse_result& result) noexcept {
                uint8_t first_rank = 0;
                if (!parse_rank(first_rank, result)) {
                    return false;
                }

                const int first_suit = parse_suit_char(peek());
                if (first_suit >= 0) {
                    consume();
                    uint8_t second_rank = 0;
                    uint8_t second_suit = 0;
                    if (!parse_rank(second_rank, result) || !parse_suit(second_suit, result)) {
                        return false;
                    }
                    if (first_rank == second_rank && static_cast<uint8_t>(first_suit) == second_suit) {
                        fail(result, range_parse_error_code::invalid_exact_combo);
                        return false;
                    }
                    hand_out = hand_class{
                        .kind = hand_class_kind::exact_combo,
                        .high_rank = first_rank,
                        .low_rank = second_rank,
                        .first_suit = static_cast<uint8_t>(first_suit),
                        .second_suit = second_suit,
                        .mode = suit_mode::both
                    };
                    return true;
                }

                uint8_t second_rank = 0;
                if (!parse_rank(second_rank, result)) {
                    return false;
                }
                if (first_rank == second_rank) {
                    hand_out = hand_class{
                        .kind = hand_class_kind::pair,
                        .high_rank = first_rank,
                        .low_rank = second_rank,
                        .first_suit = 0,
                        .second_suit = 0,
                        .mode = suit_mode::both
                    };
                    return true;
                }

                suit_mode mode = suit_mode::both;
                if (upper_ascii(peek()) == 'S') {
                    consume();
                    mode = suit_mode::suited;
                } else if (upper_ascii(peek()) == 'O') {
                    consume();
                    mode = suit_mode::offsuit;
                }

                const uint8_t high_rank = first_rank > second_rank ? first_rank : second_rank;
                const uint8_t low_rank = first_rank > second_rank ? second_rank : first_rank;
                hand_out = hand_class{
                    .kind = hand_class_kind::non_pair,
                    .high_rank = high_rank,
                    .low_rank = low_rank,
                    .first_suit = 0,
                    .second_suit = 0,
                    .mode = mode
                };
                return true;
            }

            [[nodiscard]] bool parse_term(range_parse_result& result) noexcept {
                hand_class first{};
                if (!parse_class(first, result)) {
                    return false;
                }

                bool plus = false;
                bool range = false;
                hand_class second{};
                skip_ws();
                if (match('+')) {
                    plus = true;
                } else if (match('-')) {
                    range = true;
                    skip_ws();
                    if (!parse_class(second, result)) {
                        return false;
                    }
                }

                combo_weight weight = 1.0f;
                if (!parse_weight(weight, result)) {
                    return false;
                }

                if (plus) {
                    return expand_plus(result, first, weight);
                }
                if (range) {
                    return expand_range(result, first, second, weight);
                }
                expand_class(result.range, first, weight);
                return true;
            }

            [[nodiscard]] bool expand_plus(range_parse_result& result, const hand_class hand, const combo_weight weight) noexcept {
                if (hand.kind == hand_class_kind::exact_combo) {
                    fail(result, range_parse_error_code::invalid_plus);
                    return false;
                }
                if (hand.kind == hand_class_kind::pair) {
                    for (uint8_t rank = hand.high_rank; rank <= 12; ++rank) {
                        set_pair(result.range, rank, weight);
                    }
                    return true;
                }
                for (uint8_t low = hand.low_rank; low < hand.high_rank; ++low) {
                    set_non_pair(result.range, hand.high_rank, low, hand.mode, weight);
                }
                return true;
            }

            [[nodiscard]] bool expand_range(
                range_parse_result& result,
                const hand_class first,
                const hand_class second,
                const combo_weight weight
            ) noexcept {
                if (first.kind != second.kind || first.kind == hand_class_kind::exact_combo) {
                    fail(result, range_parse_error_code::invalid_range);
                    return false;
                }

                if (first.kind == hand_class_kind::pair) {
                    const uint8_t lo = first.high_rank < second.high_rank ? first.high_rank : second.high_rank;
                    const uint8_t hi = first.high_rank < second.high_rank ? second.high_rank : first.high_rank;
                    for (uint8_t rank = lo; rank <= hi; ++rank) {
                        set_pair(result.range, rank, weight);
                    }
                    return true;
                }

                if (first.high_rank != second.high_rank || first.mode != second.mode) {
                    fail(result, range_parse_error_code::invalid_range);
                    return false;
                }
                const uint8_t lo = first.low_rank < second.low_rank ? first.low_rank : second.low_rank;
                const uint8_t hi = first.low_rank < second.low_rank ? second.low_rank : first.low_rank;
                for (uint8_t low = lo; low <= hi; ++low) {
                    if (low != first.high_rank) {
                        set_non_pair(result.range, first.high_rank, low, first.mode, weight);
                    }
                }
                return true;
            }
        };
    }

    [[nodiscard]] inline_hint range_parse_result parse_range(const std::string_view input) noexcept {
        return detail::range_parser{input}.parse();
    }

}
