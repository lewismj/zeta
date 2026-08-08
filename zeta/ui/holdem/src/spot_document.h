#pragma once

#include "cli/solve_cli.h"
#include "solver/solution_store.h"

#include <cstdint>
#include <expected>
#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace zeta::holdem::ui {

    using spot = struct cli::solve_spot;
    using solve_artifact = struct cli::solve_artifact;

    enum class document_error_kind : uint8_t {
        io,
        parse,
        invalid_document
    };

    struct document_error {
        document_error_kind kind = document_error_kind::parse;
        std::string message;
    };

    struct solve_history_entry {
        std::string timestamp_utc;
        uint64_t iterations = 0;
        std::string outcome;
    };

    struct spot_document_metadata {
        std::string created_utc;
        std::string updated_utc;
        std::string last_solve_summary;
        std::vector<std::string> tags;
    };

    /**
     * Owns the editable spot, optional artifact, persistence metadata, and dirty state for one UI tab.
     */
    class spot_document {
    public:
        [[nodiscard]] static spot_document create_new();
        [[nodiscard]] static std::expected<spot_document, document_error> load(const std::filesystem::path& path);
        [[nodiscard]] static std::expected<spot_document, document_error> parse_json(std::string_view json);

        [[nodiscard]] const spot& current_spot() const noexcept;
        [[nodiscard]] const std::optional<solve_artifact>& artifact() const noexcept;
        [[nodiscard]] const std::optional<solver::solution_store>& solution() const noexcept;
        [[nodiscard]] const spot_document_metadata& metadata() const noexcept;
        [[nodiscard]] const std::vector<solve_history_entry>& recent_history() const noexcept;
        [[nodiscard]] const std::filesystem::path& file_path() const noexcept;
        [[nodiscard]] bool is_dirty() const noexcept;

        void replace_spot(spot next_spot);
        void replace_artifact(std::optional<solve_artifact> next_artifact);
        void replace_solution(std::optional<solver::solution_store> next_solution);
        void update_metadata(spot_document_metadata next_metadata);
        void add_history(solve_history_entry entry);
        void set_file_path(std::filesystem::path path);
        void mark_dirty() noexcept;
        void clear_dirty() noexcept;

        [[nodiscard]] std::string serialize_json() const;
        [[nodiscard]] std::expected<void, document_error> save() const;
        [[nodiscard]] std::expected<void, document_error> save_as(const std::filesystem::path& path);

    private:
        spot spot_{};
        std::optional<solve_artifact> artifact_{};
        std::optional<solver::solution_store> solution_{};
        spot_document_metadata metadata_{};
        std::vector<solve_history_entry> recent_history_{};
        std::filesystem::path file_path_{};
        bool dirty_ = false;
    };

}
