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

    void solver_session::cancel_before_start() noexcept
    {
        cancel_before_start_.store(true, std::memory_order_relaxed);
    }

    solver_session_result solver_session::run()
    {
        solver_session_result result;
        result.spot_snapshot = request_.spot_snapshot;
        result.iterations = request_.iterations;
        result.metadata.started_utc = cli::detail::now_utc_iso8601();
        result.metadata.git_revision = request_.runtime.git_revision;

        if (cancel_before_start_.load(std::memory_order_relaxed)) {
            result.terminal_state = solver_session_terminal_state::cancelled_before_start;
            result.metadata.finished_utc = cli::detail::now_utc_iso8601();
            return result;
        }

        auto runtime = request_.runtime;
        if (runtime.timestamp_utc.empty()) {
            runtime.timestamp_utc = result.metadata.started_utc;
        }

        auto solved = cli::solve_spot(request_.spot_snapshot, request_.iterations, runtime);
        result.metadata.finished_utc = cli::detail::now_utc_iso8601();
        if (!solved) {
            result.terminal_state = solver_session_terminal_state::failed;
            result.error_message = solved.error().message;
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
        }
        return "unknown";
    }

}
