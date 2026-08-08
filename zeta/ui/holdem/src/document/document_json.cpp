#include "document_json.h"

#include <boost/json.hpp>

#include <utility>

namespace zeta::holdem::ui::document {

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

        [[nodiscard]] std::string key_name(const std::string_view key)
        {
            return std::string{key};
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

        [[nodiscard]] std::expected<std::string, document_error> optional_string(
            const json::object& object,
            const std::string_view key,
            std::string fallback)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return fallback;
            }
            if (!value->is_string()) {
                return std::unexpected(document_error{
                    document_error_kind::parse,
                    key_name(key) + " must be a string."
                });
            }
            return json_string(*value);
        }

        [[nodiscard]] std::expected<uint64_t, document_error> history_iterations(const json::value& value)
        {
            if (value.is_uint64()) {
                return value.as_uint64();
            }
            if (value.is_int64() && value.as_int64() >= 0) {
                return static_cast<uint64_t>(value.as_int64());
            }
            return std::unexpected(document_error{
                document_error_kind::parse,
                "History iterations must be a non-negative integer."
            });
        }

        [[nodiscard]] std::expected<std::vector<std::string>, document_error> optional_string_array(
            const json::object& object,
            const std::string_view key)
        {
            const auto* value = find_value(object, key);
            if (value == nullptr) {
                return std::vector<std::string>{};
            }
            if (!value->is_array()) {
                return std::unexpected(document_error{
                    document_error_kind::parse,
                    key_name(key) + " must be an array."
                });
            }
            std::vector<std::string> out;
            out.reserve(value->as_array().size());
            for (const auto& element : value->as_array()) {
                if (!element.is_string()) {
                    return std::unexpected(document_error{
                        document_error_kind::parse,
                        key_name(key) + " entries must be strings."
                    });
                }
                out.push_back(json_string(element));
            }
            return out;
        }

        [[nodiscard]] std::expected<spot_document_metadata, document_error> parse_metadata(const json::object& object)
        {
            spot_document_metadata metadata{};
            auto created = optional_string(object, "created_utc", metadata.created_utc);
            auto updated = optional_string(object, "updated_utc", metadata.updated_utc);
            auto summary = optional_string(object, "last_solve_summary", metadata.last_solve_summary);
            auto tags = optional_string_array(object, "tags");
            if (!created) {
                return std::unexpected(created.error());
            }
            if (!updated) {
                return std::unexpected(updated.error());
            }
            if (!summary) {
                return std::unexpected(summary.error());
            }
            if (!tags) {
                return std::unexpected(tags.error());
            }
            metadata.created_utc = std::move(*created);
            metadata.updated_utc = std::move(*updated);
            metadata.last_solve_summary = std::move(*summary);
            metadata.tags = std::move(*tags);
            return metadata;
        }

        [[nodiscard]] std::expected<std::vector<solve_history_entry>, document_error> parse_history(
            const json::object& object)
        {
            std::vector<solve_history_entry> history;
            const auto* value = find_value(object, "recent_history");
            if (value == nullptr) {
                return history;
            }
            if (!value->is_array()) {
                return std::unexpected(document_error{
                    document_error_kind::parse,
                    "recent_history must be an array."
                });
            }
            history.reserve(value->as_array().size());
            for (const auto& entry_value : value->as_array()) {
                if (!entry_value.is_object()) {
                    return std::unexpected(document_error{
                        document_error_kind::parse,
                        "History entries must be objects."
                    });
                }
                const auto& entry = entry_value.as_object();
                const auto* timestamp = find_value(entry, "timestamp_utc");
                const auto* iterations = find_value(entry, "iterations");
                const auto* outcome = find_value(entry, "outcome");
                if (timestamp == nullptr || !timestamp->is_string()
                    || iterations == nullptr
                    || outcome == nullptr || !outcome->is_string()) {
                    return std::unexpected(document_error{
                        document_error_kind::parse,
                        "History entries require timestamp_utc, iterations, and outcome."
                    });
                }
                auto parsed_iterations = history_iterations(*iterations);
                if (!parsed_iterations) {
                    return std::unexpected(parsed_iterations.error());
                }
                history.push_back(solve_history_entry{
                    .timestamp_utc = json_string(*timestamp),
                    .iterations = *parsed_iterations,
                    .outcome = json_string(*outcome)
                });
            }
            return history;
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

        [[nodiscard]] json::value parse_serialized_json(const std::string& text)
        {
            boost::system::error_code ec;
            auto value = json::parse(text, ec);
            return ec ? json::value{nullptr} : std::move(value);
        }

    }

    std::expected<document_json_payload, document_error> parse_document_json(const std::string_view text)
    {
        document_json_payload payload{};

        boost::system::error_code ec;
        auto value = json::parse(text, ec);
        if (ec || !value.is_object()) {
            auto parsed_spot = cli::parse_spot_json(text);
            if (!parsed_spot) {
                return std::unexpected(from_cli_error(parsed_spot.error()));
            }
            payload.spot = std::move(*parsed_spot);
            return payload;
        }

        const auto& root = value.as_object();
        if (find_value(root, "document_schema_version") == nullptr) {
            auto parsed_spot = cli::parse_spot_json(text);
            if (!parsed_spot) {
                return std::unexpected(from_cli_error(parsed_spot.error()));
            }
            payload.spot = std::move(*parsed_spot);
            return payload;
        }

        const auto* spot_value = find_value(root, "spot");
        if (spot_value == nullptr || !spot_value->is_object()) {
            return std::unexpected(document_error{document_error_kind::parse, "Missing spot object."});
        }
        auto parsed_spot = cli::parse_spot_json(json::serialize(*spot_value));
        if (!parsed_spot) {
            return std::unexpected(from_cli_error(parsed_spot.error()));
        }
        payload.spot = std::move(*parsed_spot);

        if (const auto* metadata = find_value(root, "metadata"); metadata != nullptr) {
            if (!metadata->is_object()) {
                return std::unexpected(document_error{
                    document_error_kind::parse,
                    "metadata must be an object."
                });
            }
            auto parsed_metadata = parse_metadata(metadata->as_object());
            if (!parsed_metadata) {
                return std::unexpected(parsed_metadata.error());
            }
            payload.metadata = std::move(*parsed_metadata);
        }

        if (const auto* artifact = find_value(root, "artifact"); artifact != nullptr && !artifact->is_null()) {
            if (!artifact->is_object()) {
                return std::unexpected(document_error{
                    document_error_kind::parse,
                    "artifact must be an object or null."
                });
            }
            auto parsed_artifact = cli::parse_artifact_json(json::serialize(*artifact));
            if (!parsed_artifact) {
                return std::unexpected(from_cli_error(parsed_artifact.error()));
            }
            payload.artifact = std::move(*parsed_artifact);
        }

        auto history = parse_history(root);
        if (!history) {
            return std::unexpected(history.error());
        }
        payload.recent_history = std::move(*history);
        return payload;
    }

    std::string serialize_document_json(const document_json_payload& payload)
    {
        const auto metadata_source = payload.metadata.value_or(spot_document_metadata{});
        json::object metadata;
        metadata["created_utc"] = metadata_source.created_utc;
        metadata["updated_utc"] = metadata_source.updated_utc;
        metadata["last_solve_summary"] = metadata_source.last_solve_summary;
        metadata["tags"] = string_array_json(metadata_source.tags);

        json::array history;
        history.reserve(payload.recent_history.size());
        for (const auto& entry : payload.recent_history) {
            json::object entry_object;
            entry_object["timestamp_utc"] = entry.timestamp_utc;
            entry_object["iterations"] = entry.iterations;
            entry_object["outcome"] = entry.outcome;
            history.emplace_back(std::move(entry_object));
        }

        json::object root;
        root["document_schema_version"] = 1;
        root["metadata"] = std::move(metadata);
        root["spot"] = parse_serialized_json(cli::serialize_spot_json(payload.spot));
        root["artifact"] = payload.artifact ? parse_serialized_json(cli::serialize_artifact_json(*payload.artifact)) : json::value{nullptr};
        root["recent_history"] = std::move(history);
        return json::serialize(root);
    }

}
