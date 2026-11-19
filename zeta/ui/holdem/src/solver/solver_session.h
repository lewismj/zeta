#pragma once

#include "../spot_document.h"

#include <atomic>
#include <cstdint>
#include <expected>
#include <mutex>
#include <optional>
#include <string>

namespace zeta::holdem::ui::solver {

    enum class solver_session_terminal_state : uint8_t {
        completed,
        failed,
        cancelled_before_start,
        cancelled
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
     *
     * run() blocks on its calling thread; cancel() and latest_progress() are safe to
     * call from any other thread while it runs. A cancelled solve stops at the next
     * iteration or batch of terminal leaves and produces no artifact.
     */
    class solver_session {
    public:
        explicit solver_session(solver_session_request request);

        [[nodiscard]] const solver_session_request& request() const noexcept;

        /** Asks the solve to stop: before it starts, or at its next cancellation point. */
        void cancel() noexcept;
        [[nodiscard]] bool cancel_requested() const noexcept;

        /** The most recent progress event the solver reported, if any. */
        [[nodiscard]] std::optional<cli::solve_progress_event> latest_progress() const;

        [[nodiscard]] solver_session_result run();

    private:
        solver_session_request request_;
        std::atomic_bool cancel_requested_ = false;
        mutable std::mutex progress_mutex_;
        std::optional<cli::solve_progress_event> latest_progress_;
    };

    [[nodiscard]] const char* to_string(solver_session_terminal_state state) noexcept;

}
