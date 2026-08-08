#pragma once

#include "cli/solve_cli.h"
#include "spot_document.h"
#include "viewmodels/strategy_view_model.h"

#include <QImage>
#include <QWidget>

#include <expected>
#include <filesystem>
#include <optional>
#include <string>
#include <string_view>
#include <vector>

namespace zeta::holdem::ui::study {

    struct study_record {
        std::filesystem::path path;
        std::string title;
        std::vector<std::string> tags;
        bool pinned = false;
        std::string updated_utc;
    };

    struct action_frequency_delta {
        std::string action;
        double before = 0.0;
        double after = 0.0;
        double delta = 0.0;
    };

    struct hand_ev_delta {
        std::string hand;
        double before = 0.0;
        double after = 0.0;
        double delta = 0.0;
    };

    struct strategy_run_comparison {
        std::vector<action_frequency_delta> action_deltas;
        std::vector<hand_ev_delta> ev_deltas;
        std::size_t changed_best_action_count = 0;
        std::vector<std::string> settings_differences;
    };

    /**
     * Filters recent or pinned study records by tag and free-text query.
     */
    [[nodiscard]] std::vector<study_record> filter_studies(
        const std::vector<study_record>& studies,
        std::string_view query,
        std::optional<std::string_view> required_tag);

    /**
     * Exports root strategy rows as stable CSV.
     */
    [[nodiscard]] std::string export_strategy_csv(const solve_artifact& artifact);

    /**
     * Exports the visible hand table model as stable CSV.
     */
    [[nodiscard]] std::string export_hand_table_csv(const viewmodels::strategy_view_model& model);

    /**
     * Builds a concise, copyable textual study summary.
     */
    [[nodiscard]] std::string make_share_summary(
        const spot& source,
        const solve_artifact& artifact,
        const viewmodels::strategy_view_model& model);

    /**
     * Compares two solved runs of the same spot.
     */
    [[nodiscard]] std::expected<strategy_run_comparison, std::string> compare_strategy_runs(
        const spot& before_spot,
        const solve_artifact& before,
        const spot& after_spot,
        const solve_artifact& after);

    /**
     * Captures the current widget view into an image.
     */
    [[nodiscard]] QImage capture_widget_image(QWidget& widget);

}
