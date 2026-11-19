#include "solver_session.h"

#include <utility>

namespace zeta::holdem::ui::solver {

    solver_session::solver_session(solver_session_request request)
        : request_(std::move(request))
    {
    }

    const solver_session_request& solver_session::request() const noexcept
    {
        return request_;
    }

    void solver_session::cancel() noexcept
    {
        cancel_requested_.store(true, std::memory_order_relaxed);
    }

    bool solver_session::cancel_requested() const noexcept
    {
        return cancel_requested_.load(std::memory_order_relaxed);
    }

    std::optional<cli::solve_progress_event> solver_session::latest_progress() const
    {
        const std::lock_guard lock{progress_mutex_};
        return latest_progress_;
    }

    solver_session_result solver_session::run()
    {
        solver_session_result result;
        result.spot_snapshot = request_.spot_snapshot;
        result.iterations = request_.iterations;
        result.metadata.started_utc = cli::detail::now_utc_iso8601();
        result.metadata.git_revision = request_.runtime.git_revision;

        if (cancel_requested()) {
            result.terminal_state = solver_session_terminal_state::cancelled_before_start;
            result.metadata.finished_utc = cli::detail::now_utc_iso8601();
            return result;
        }

        auto runtime = request_.runtime;
        if (runtime.timestamp_utc.empty()) {
            runtime.timestamp_utc = result.metadata.started_utc;
        }
        // The session's own cancel and progress hooks wrap any the request brought.
        runtime.cancellation_requested = [this, requested = std::move(runtime.cancellation_requested)] {
            return cancel_requested() || (requested && requested());
        };
        runtime.progress_callback = [this, forward = std::move(runtime.progress_callback)](const cli::solve_progress_event& event) {
            {
                const std::lock_guard lock{progress_mutex_};
                latest_progress_ = event;
            }
            if (forward) {
                forward(event);
            }
        };

        auto solved = cli::solve_spot(request_.spot_snapshot, request_.iterations, runtime);
        result.metadata.finished_utc = cli::detail::now_utc_iso8601();
        if (!solved) {
            result.terminal_state = solved.error().kind == cli::cli_error_kind::cancelled
                ? solver_session_terminal_state::cancelled
                : solver_session_terminal_state::failed;
            if (result.terminal_state == solver_session_terminal_state::failed) {
                result.error_message = solved.error().message;
            }
            return result;
        }

        result.terminal_state = solver_session_terminal_state::completed;
        result.timing = solved->timing;
        result.artifact = std::move(solved->artifact);
        return result;
    }

    const char* to_string(const solver_session_terminal_state state) noexcept
    {
        switch (state) {
            case solver_session_terminal_state::completed: return "completed";
            case solver_session_terminal_state::failed: return "failed";
            case solver_session_terminal_state::cancelled_before_start: return "cancelled-before-start";
            case solver_session_terminal_state::cancelled: return "cancelled";
        }
        return "unknown";
    }

}
