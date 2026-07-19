#pragma once

#include "cfr/graph/graph.h"
#include <expected>

namespace zeta::holdem::cfr::graph_validation {

    [[nodiscard]] bool validate(const game_graph& graph) noexcept;
    [[nodiscard]] std::expected<void, graph_build_error> validate_all(const game_graph& graph) noexcept;
    [[nodiscard]] std::expected<void, graph_build_error> validate_structure(const game_graph& graph) noexcept;
    [[nodiscard]] std::expected<void, graph_build_error> validate_metadata(const game_graph& graph) noexcept;
    [[nodiscard]] std::expected<void, graph_build_error> validate_infosets(const game_graph& graph) noexcept;

}
