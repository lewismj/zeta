#pragma once

#include "../spot_document.h"

#include <expected>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace zeta::holdem::ui::document {

    struct document_json_payload {
        spot spot{};
        std::optional<solve_artifact> artifact{};
        std::optional<spot_document_metadata> metadata{};
        std::vector<solve_history_entry> recent_history{};
    };

    /**
     * Parses either a UI document envelope or a legacy bare solver spot JSON object.
     */
    [[nodiscard]] std::expected<document_json_payload, document_error> parse_document_json(std::string_view text);

    /**
     * Serializes a UI document envelope that keeps spot input, artifact, metadata, and run history together.
     */
    [[nodiscard]] std::string serialize_document_json(const document_json_payload& payload);

}
