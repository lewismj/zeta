#pragma once

#include "../spot_document.h"

#include <atomic>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>

namespace zeta::holdem::ui::solver {

    enum class solver_session_terminal_state : uint8_t {
        completed,
        failed,
        cancelled_before_start
    };

    struct solver_session_request {
        spot spot_snapshot{};
        uint64_t iterations = 100;
        cli::solve_runtime_options runtime{};
    };

    struct solver_session_metadata {
        std::string started_utc;
        std::string finished_utc;
        std::string git_revision = "unknown";
    };

    struct solver_session_result {
        spot spot_snapshot{};
        uint64_t iterations = 0;
        solver_session_metadata metadata{};
        solver_session_terminal_state terminal_state = solver_session_terminal_state::failed;
        cli::solve_timing timing{};
        std::optional<solve_artifact> artifact{};
        std::string error_message;
    };

    /**
     * Runs a single solver invocation from an immutable spot snapshot.
     */
    class solver_session {
    public:
        explicit solver_session(solver_session_request request);

        [[nodiscard]] const solver_session_request& request() const noexcept;
        void cancel_before_start() noexcept;
        [[nodiscard]] solver_session_result run();

    private:
        solver_session_request request_;
        std::atomic_bool cancel_before_start_ = false;
    };

    [[nodiscard]] const char* to_string(solver_session_terminal_state state) noexcept;

}
