#include "spot_document.h"

#include "document/document_json.h"

#include <utility>

namespace zeta::holdem::ui {

    namespace {

        [[nodiscard]] document_error from_cli_error(const cli::cli_error& error)
        {
            switch (error.kind) {
                case cli::cli_error_kind::io:
                    return document_error{document_error_kind::io, error.message};
                case cli::cli_error_kind::parse:
                    return document_error{document_error_kind::parse, error.message};
                case cli::cli_error_kind::invalid_spot:
                case cli::cli_error_kind::invalid_artifact:
                case cli::cli_error_kind::solver:
                case cli::cli_error_kind::cancelled:
                    return document_error{document_error_kind::invalid_document, error.message};
            }
            return document_error{document_error_kind::invalid_document, error.message};
        }

    }

    spot_document spot_document::create_new()
    {
        spot_document document;
        const auto now = cli::detail::now_utc_iso8601();
        document.metadata_.created_utc = now;
        document.metadata_.updated_utc = now;
        document.dirty_ = false;
        return document;
    }

    std::expected<spot_document, document_error> spot_document::load(const std::filesystem::path& path)
    {
        auto text = cli::read_file_text(path);
        if (!text) {
            return std::unexpected(from_cli_error(text.error()));
        }

        auto parsed = parse_json(*text);
        if (!parsed) {
            return std::unexpected(parsed.error());
        }
        auto document = std::move(*parsed);
        document.file_path_ = path;
        document.dirty_ = false;
        return document;
    }

    std::expected<spot_document, document_error> spot_document::parse_json(const std::string_view json)
    {
        spot_document parsed_document = create_new();
        auto parsed = ui::document::parse_document_json(json);
        if (!parsed) {
            return std::unexpected(parsed.error());
        }
        parsed_document.spot_ = std::move(parsed->spot);
        parsed_document.artifact_ = std::move(parsed->artifact);
        parsed_document.solution_ = std::move(parsed->solution);
        if (!parsed_document.solution_ && parsed_document.artifact_) {
            parsed_document.solution_ = solver::make_root_only_solution_store(
                parsed_document.spot_,
                *parsed_document.artifact_);
        }
        if (parsed->metadata) {
            parsed_document.metadata_ = std::move(*parsed->metadata);
        }
        parsed_document.recent_history_ = std::move(parsed->recent_history);
        parsed_document.dirty_ = false;
        return parsed_document;
    }

    const spot& spot_document::current_spot() const noexcept
    {
        return spot_;
    }

    const std::optional<solve_artifact>& spot_document::artifact() const noexcept
    {
        return artifact_;
    }

    const std::optional<solver::solution_store>& spot_document::solution() const noexcept
    {
        return solution_;
    }

    const spot_document_metadata& spot_document::metadata() const noexcept
    {
        return metadata_;
    }

    const std::vector<solve_history_entry>& spot_document::recent_history() const noexcept
    {
        return recent_history_;
    }

    const std::filesystem::path& spot_document::file_path() const noexcept
    {
        return file_path_;
    }

    bool spot_document::is_dirty() const noexcept
    {
        return dirty_;
    }

    void spot_document::replace_spot(spot next_spot)
    {
        spot_ = std::move(next_spot);
        metadata_.updated_utc = cli::detail::now_utc_iso8601();
        dirty_ = true;
    }

    void spot_document::replace_artifact(std::optional<solve_artifact> next_artifact)
    {
        artifact_ = std::move(next_artifact);
        solution_ = artifact_ ? std::optional{solver::make_root_only_solution_store(spot_, *artifact_)} : std::nullopt;
        metadata_.updated_utc = cli::detail::now_utc_iso8601();
        dirty_ = true;
    }

    void spot_document::replace_solution(std::optional<solver::solution_store> next_solution)
    {
        solution_ = std::move(next_solution);
        metadata_.updated_utc = cli::detail::now_utc_iso8601();
        dirty_ = true;
    }

    void spot_document::update_metadata(spot_document_metadata next_metadata)
    {
        metadata_ = std::move(next_metadata);
        dirty_ = true;
    }

    void spot_document::add_history(solve_history_entry entry)
    {
        recent_history_.push_back(std::move(entry));
        metadata_.updated_utc = cli::detail::now_utc_iso8601();
        dirty_ = true;
    }

    void spot_document::set_file_path(std::filesystem::path path)
    {
        file_path_ = std::move(path);
    }

    void spot_document::mark_dirty() noexcept
    {
        dirty_ = true;
    }

    void spot_document::clear_dirty() noexcept
    {
        dirty_ = false;
    }

    std::string spot_document::serialize_json() const
    {
        return ui::document::serialize_document_json(ui::document::document_json_payload{
            .spot = spot_,
            .artifact = artifact_,
            .solution = solution_,
            .metadata = metadata_,
            .recent_history = recent_history_
        });
    }

    std::expected<void, document_error> spot_document::save() const
    {
        if (file_path_.empty()) {
            return std::unexpected(document_error{document_error_kind::io, "Document has no file path."});
        }
        auto write = cli::write_file_text(file_path_, serialize_json());
        if (!write) {
            return std::unexpected(from_cli_error(write.error()));
        }
        return {};
    }

    std::expected<void, document_error> spot_document::save_as(const std::filesystem::path& path)
    {
        file_path_ = path;
        auto write = cli::write_file_text(file_path_, serialize_json());
        if (!write) {
            return std::unexpected(from_cli_error(write.error()));
        }
        dirty_ = false;
        return {};
    }

}
