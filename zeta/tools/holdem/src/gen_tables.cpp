#include <cstdint>
#include <filesystem>
#include <fstream>
#include <iostream>
#include <array>
#include <bit>
#include <string_view>
#include <unordered_map>
#include <vector>

#include "tables.h"

namespace zeta::holdem::lookup {
    const std::vector<non_flush_entry>& rank_table() noexcept;
}

namespace {
    [[nodiscard]] uint64_t key_from_counts(const std::array<uint8_t, 13>& counts) noexcept {
        uint16_t ones = 0;
        uint16_t twos = 0;
        uint16_t threes = 0;
        uint16_t fours = 0;

        for (int r = 0; r < 13; ++r) {
            const auto bit = static_cast<uint16_t>(1u << r);
            const uint8_t c = counts[r];
            if (c >= 1) ones = static_cast<uint16_t>(ones | bit);
            if (c >= 2) twos = static_cast<uint16_t>(twos | bit);
            if (c >= 3) threes = static_cast<uint16_t>(threes | bit);
            if (c >= 4) fours = static_cast<uint16_t>(fours | bit);
        }

        return static_cast<uint64_t>(ones)
            | (static_cast<uint64_t>(twos) << 13)
            | (static_cast<uint64_t>(threes) << 26)
            | (static_cast<uint64_t>(fours) << 39);
    }

    [[nodiscard]] uint64_t key_from_suits(const std::array<uint16_t, 4>& suits) noexcept {
        const auto s0 = suits[0];
        const auto s1 = suits[1];
        const auto s2 = suits[2];
        const auto s3 = suits[3];

        const auto ones = static_cast<uint16_t>(s0 | s1 | s2 | s3);
        const auto twos = static_cast<uint16_t>((s0 & s1) | (s0 & s2) | (s0 & s3)
                                                 | (s1 & s2) | (s1 & s3) | (s2 & s3));
        const auto threes = static_cast<uint16_t>((s0 & s1 & s2) | (s0 & s1 & s3)
                                                   | (s0 & s2 & s3) | (s1 & s2 & s3));
        const auto fours = static_cast<uint16_t>(s0 & s1 & s2 & s3);

        return static_cast<uint64_t>(ones)
            | (static_cast<uint64_t>(twos) << 13)
            | (static_cast<uint64_t>(threes) << 26)
            | (static_cast<uint64_t>(fours) << 39);
    }

    [[nodiscard]] bool is_flush(const std::array<uint16_t, 4>& suits) noexcept {
        for (const auto mask : suits) {
            if (std::popcount(mask) >= 5) {
                return true;
            }
        }
        return false;
    }

    [[nodiscard]] bool validate_canonical_from_cards(const std::vector<zeta::holdem::lookup::non_flush_entry>& rank) {
        std::unordered_map<uint64_t, uint32_t> canonical_rank;
        canonical_rank.reserve(rank.size());
        for (const auto& entry : rank) {
            canonical_rank.emplace(entry.key, entry.rank.value);
        }

        std::unordered_map<uint64_t, uint32_t> seen;
        seen.reserve(rank.size());

        std::size_t non_flush_hands = 0;
        std::size_t key_mismatch_count = 0;
        std::size_t missing_rank_count = 0;
        std::size_t rank_conflict_count = 0;

        std::array<int, 7> cards{0, 1, 2, 3, 4, 5, 6};
        while (true) {
            std::array<uint8_t, 13> counts{};
            std::array<uint16_t, 4> suits{};
            for (const int card : cards) {
                const int suit = card / 13;
                const int rank_index = card % 13;
                ++counts[rank_index];
                suits[suit] = static_cast<uint16_t>(suits[suit] | (static_cast<uint16_t>(1u) << rank_index));
            }

            if (!is_flush(suits)) {
                ++non_flush_hands;

                const uint64_t count_key = key_from_counts(counts);
                const uint64_t suit_key = key_from_suits(suits);
                if (count_key != suit_key) {
                    ++key_mismatch_count;
                } else {
                    const auto it_rank = canonical_rank.find(suit_key);
                    if (it_rank == canonical_rank.end()) {
                        ++missing_rank_count;
                    } else {
                        const auto [it_seen, inserted] = seen.emplace(suit_key, it_rank->second);
                        if (!inserted && it_seen->second != it_rank->second) {
                            ++rank_conflict_count;
                        }
                    }
                }
            }

            int i = 6;
            while (i >= 0 && cards[i] == (52 - 7 + i)) {
                --i;
            }
            if (i < 0) {
                break;
            }

            ++cards[i];
            for (int j = i + 1; j < 7; ++j) {
                cards[j] = cards[j - 1] + 1;
            }
        }

        std::cout << "canonical validation: non-flush hands=" << non_flush_hands
                  << ", key mismatches=" << key_mismatch_count
                  << ", missing keys=" << missing_rank_count
                  << ", rank conflicts=" << rank_conflict_count
                  << ", unique keys seen=" << seen.size() << "\n";

        return key_mismatch_count == 0
            && missing_rank_count == 0
            && rank_conflict_count == 0
            && seen.size() == rank.size();
    }

    template<typename Table>
    void emit_chunk_table(std::ofstream& os, const char* type_name, const char* table_name, const Table& table) {
        os << "const " << type_name << " " << table_name << " = {\n";
        for (std::size_t remaining = 0; remaining < table.size(); ++remaining) {
            os << "        std::array<uint32_t, " << table[remaining].size() << ">{";
            for (std::size_t code = 0; code < table[remaining].size(); ++code) {
                if (code != 0) {
                    os << ", ";
                }
                os << table[remaining][code] << "u";
            }
            os << "}";
            if (remaining + 1 != table.size()) {
                os << ",";
            }
            os << "\n";
        }
        os << "    };\n\n";
    }
}

int main(int argc, char** argv) {
#ifdef ZETA_HOLDEM_TABLES_OUTPUT
    std::filesystem::path out = ZETA_HOLDEM_TABLES_OUTPUT;
#else
    std::filesystem::path out = "tables.generated.cpp";
#endif

    bool validate_canonical = false;
    for (int i = 1; i < argc; ++i) {
        const std::string_view arg{argv[i]};
        if (arg == "--validate-canonical") {
            validate_canonical = true;
        } else {
            out = argv[i];
        }
    }

    std::ofstream os(out, std::ios::binary | std::ios::trunc);
    if (!os) {
        std::cerr << "Failed to open output file: " << out << "\n";
        return 1;
    }

    const auto& flush = zeta::holdem::lookup::flush_table;
    const auto& rank = zeta::holdem::lookup::rank_table();
    const auto chunk0 = zeta::holdem::lookup::build_quinary_chunk_table<0, 4>();
    const auto chunk1 = zeta::holdem::lookup::build_quinary_chunk_table<4, 4>();
    const auto chunk2 = zeta::holdem::lookup::build_quinary_chunk_table<8, 5>();

    if (validate_canonical && !validate_canonical_from_cards(rank)) {
        std::cerr << "Canonical non-flush key validation failed.\n";
        return 2;
    }

    std::array<zeta::holdem::hand_rank, zeta::holdem::lookup::non_flush_quinary_table_size> non_flush_dense{};
    std::vector<uint8_t> filled(non_flush_dense.size(), 0);
    for (const auto& entry : rank) {
        const auto index = zeta::holdem::lookup::quinary_index_from_key(entry.key);
        if (index >= non_flush_dense.size()) {
            std::cerr << "Quinary index out of range: " << index << "\n";
            return 3;
        }
        if (filled[index] != 0 && non_flush_dense[index].value != entry.rank.value) {
            std::cerr << "Conflicting rank for quinary index: " << index << "\n";
            return 4;
        }
        filled[index] = 1;
        non_flush_dense[index] = entry.rank;
    }

    std::size_t filled_count = 0;
    for (const auto is_filled : filled) {
        filled_count += is_filled;
    }
    if (filled_count != non_flush_dense.size()) {
        std::cerr << "Dense quinary table has " << filled_count << " filled entries, expected "
                  << non_flush_dense.size() << ".\n";
        return 5;
    }

    os << "// Generated by zeta-gen-holdem-tables\n";
    os << "// flush entries: " << flush.size() << "\n";
    os << "// non-flush entries: " << rank.size() << "\n";
    os << "// non-flush quinary slots: " << non_flush_dense.size() << "\n";
    os << "// non-flush quinary filled slots: " << filled_count << "\n";
    os << "// non-flush indexing: restricted quinary perfect index\n";

    os << "#include <array>\n";
    os << "#include \"tables.h\"\n\n";
    os << "namespace zeta::holdem::lookup {\n";

    os << "const std::array<hand_rank, (1u << 13)> flush_table = {\n";
    for (std::size_t i = 0; i < flush.size(); ++i) {
        os << "        hand_rank{" << flush[i].value << "}";
        if (i + 1 != flush.size()) os << ",";
        os << "\n";
    }
    os << "    };\n\n";

    emit_chunk_table(os, "quinary_chunk4_table", "quinary_chunk0", chunk0);
    emit_chunk_table(os, "quinary_chunk4_table", "quinary_chunk1", chunk1);
    emit_chunk_table(os, "quinary_chunk5_table", "quinary_chunk2", chunk2);

    os << "const std::array<hand_rank, non_flush_quinary_table_size> non_flush_table = {\n";
    for (std::size_t i = 0; i < non_flush_dense.size(); ++i) {
        os << "        hand_rank{" << non_flush_dense[i].value << "}";
        if (i + 1 != non_flush_dense.size()) os << ",";
        os << "\n";
    }
    os << "    };\n\n";

    os << "}\n";

    std::cout << "Generated " << out
              << " with " << flush.size() << " flush entries, "
              << rank.size() << " non-flush entries, "
              << non_flush_dense.size() << " quinary slots.\n";

    return 0;
}
