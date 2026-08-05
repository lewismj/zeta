#include "spot_document.h"

#include <boost/json.hpp>

#include <utility>

namespace zeta::holdem::ui {

    namespace {

        namespace json = boost::json;

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
                    return document_error{document_error_kind::invalid_document, error.message};
            }
            return document_error{document_error_kind::invalid_document, error.message};
        }

        [[nodiscard]] const json::value* find_value(const json::object& object, const std::string_view key)
        {
            return object.if_contains(json::string_view{key.data(), key.size()});
        }

        [[nodiscard]] std::string json_string(const json::value& value)
        {
            const auto& string = value.as_string();
            return std::string{string.data(), string.size()};
        }

        [[nodiscard]] json::array string_array_json(const std::vector<std::string>& values)
        {
            json::array out;
            out.reserve(values.size());
            for (const auto& value : values) {
                out.emplace_back(value);
            }
            return out;
        }

        void parse_metadata(const json::object& object, spot_document_metadata& metadata)
        {
            if (const auto* value = find_value(object, "created_utc"); value != nullptr && value->is_string()) {
                metadata.created_utc = json_string(*value);
            }
            if (const auto* value = find_value(object, "updated_utc"); value != nullptr && value->is_string()) {
                metadata.updated_utc = json_string(*value);
            }
            if (const auto* value = find_value(object, "last_solve_summary"); value != nullptr && value->is_string()) {
                metadata.last_solve_summary = json_string(*value);
            }
            const auto* tags = find_value(object, "tags");
            if (tags != nullptr && tags->is_array()) {
                metadata.tags.clear();
                metadata.tags.reserve(tags->as_array().size());
                for (const auto& tag : tags->as_array()) {
                    if (tag.is_string()) {
                        metadata.tags.push_back(json_string(tag));
                    }
                }
            }
        }

        void parse_history(const json::object& object, std::vector<solve_history_entry>& history)
        {
            const auto* value = find_value(object, "recent_history");
            if (value == nullptr || !value->is_array()) {
                return;
            }
            for (const auto& entry_value : value->as_array()) {
                if (!entry_value.is_object()) {
                    continue;
                }
                const auto& entry = entry_value.as_object();
                const auto* timestamp = find_value(entry, "timestamp_utc");
                const auto* iterations = find_value(entry, "iterations");
                const auto* outcome = find_value(entry, "outcome");
                if (timestamp == nullptr || !timestamp->is_string()
                    || iterations == nullptr || (!iterations->is_uint64() && !iterations->is_int64())
                    || outcome == nullptr || !outcome->is_string()) {
                    continue;
                }
                if (iterations->is_int64() && iterations->as_int64() < 0) {
                    continue;
                }
                history.push_back(solve_history_entry{
                    .timestamp_utc = json_string(*timestamp),
                    .iterations = iterations->is_uint64()
                        ? iterations->as_uint64()
                        : static_cast<uint64_t>(iterations->as_int64()),
                    .outcome = json_string(*outcome)
                });
            }
        }

        [[nodiscard]] json::value parse_serialized_json(const std::string& text)
        {
            boost::system::error_code ec;
            auto value = json::parse(text, ec);
            return ec ? json::value{nullptr} : std::move(value);
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
        spot_document document = create_new();
        boost::system::error_code ec;
        auto value = json::parse(json, ec);
        if (ec || !value.is_object()) {
            auto parsed_spot = cli::parse_spot_json(json);
            if (!parsed_spot) {
                return std::unexpected(from_cli_error(parsed_spot.error()));
            }
            document.spot_ = std::move(*parsed_spot);
            document.dirty_ = false;
            return document;
        }

        const auto& root = value.as_object();
        if (find_value(root, "document_schema_version") == nullptr) {
            auto parsed_spot = cli::parse_spot_json(json);
            if (!parsed_spot) {
                return std::unexpected(from_cli_error(parsed_spot.error()));
            }
            document.spot_ = std::move(*parsed_spot);
            document.dirty_ = false;
            return document;
        }

        const auto* spot_value = find_value(root, "spot");
        if (spot_value == nullptr || !spot_value->is_object()) {
            return std::unexpected(document_error{document_error_kind::parse, "Missing spot object."});
        }
        auto parsed_spot = cli::parse_spot_json(json::serialize(*spot_value));
        if (!parsed_spot) {
            return std::unexpected(from_cli_error(parsed_spot.error()));
        }
        document.spot_ = std::move(*parsed_spot);

        if (const auto* metadata = find_value(root, "metadata"); metadata != nullptr && metadata->is_object()) {
            parse_metadata(metadata->as_object(), document.metadata_);
        }
        if (const auto* artifact = find_value(root, "artifact"); artifact != nullptr && artifact->is_object()) {
            auto parsed_artifact = cli::parse_artifact_json(json::serialize(*artifact));
            if (!parsed_artifact) {
                return std::unexpected(from_cli_error(parsed_artifact.error()));
            }
            document.artifact_ = std::move(*parsed_artifact);
        }
        parse_history(root, document.recent_history_);

        document.dirty_ = false;
        return document;
    }

    const spot& spot_document::current_spot() const noexcept
    {
        return spot_;
    }

    const std::optional<solve_artifact>& spot_document::artifact() const noexcept
    {
        return artifact_;
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
        json::object metadata;
        metadata["created_utc"] = metadata_.created_utc;
        metadata["updated_utc"] = metadata_.updated_utc;
        metadata["last_solve_summary"] = metadata_.last_solve_summary;
        metadata["tags"] = string_array_json(metadata_.tags);

        json::array history;
        history.reserve(recent_history_.size());
        for (const auto& entry : recent_history_) {
            json::object entry_object;
            entry_object["timestamp_utc"] = entry.timestamp_utc;
            entry_object["iterations"] = entry.iterations;
            entry_object["outcome"] = entry.outcome;
            history.emplace_back(std::move(entry_object));
        }

        json::object root;
        root["document_schema_version"] = 1;
        root["metadata"] = std::move(metadata);
        root["spot"] = parse_serialized_json(cli::serialize_spot_json(spot_));
        root["artifact"] = artifact_ ? parse_serialized_json(cli::serialize_artifact_json(*artifact_)) : json::value{nullptr};
        root["recent_history"] = std::move(history);
        return json::serialize(root);
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
