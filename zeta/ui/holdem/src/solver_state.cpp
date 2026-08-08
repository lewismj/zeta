#include "solver_state.h"

namespace zeta::holdem::ui {

    namespace {

        [[nodiscard]] bool is_allowed_transition(const solver_state from, const solver_state to) noexcept
        {
            switch (from) {
                case solver_state::idle:
                    return to == solver_state::validating || to == solver_state::starting;
                case solver_state::validating:
                    return to == solver_state::idle || to == solver_state::failed || to == solver_state::starting;
                case solver_state::starting:
                    return to == solver_state::running || to == solver_state::completed || to == solver_state::failed || to == solver_state::cancelling;
                case solver_state::running:
                    return to == solver_state::cancelling || to == solver_state::completed || to == solver_state::failed;
                case solver_state::cancelling:
                    return to == solver_state::idle || to == solver_state::completed || to == solver_state::failed;
                case solver_state::completed:
                case solver_state::failed:
                    return to == solver_state::idle || to == solver_state::validating || to == solver_state::starting;
            }
            return false;
        }

    }

    solver_state solver_state_machine::state() const noexcept
    {
        return state_;
    }

    std::expected<void, std::string> solver_state_machine::transition_to(const solver_state next_state)
    {
        if (next_state == state_) {
            return {};
        }
        if (!is_allowed_transition(state_, next_state)) {
            return std::unexpected(
                std::string{"Invalid solver state transition from "}
                + to_string(state_)
                + " to "
                + to_string(next_state)
                + ".");
        }
        state_ = next_state;
        return {};
    }

    solver_controls solver_state_machine::controls() const noexcept
    {
        return controls_for_state(state_);
    }

    const char* to_string(const solver_state state) noexcept
    {
        switch (state) {
            case solver_state::idle: return "Idle";
            case solver_state::validating: return "Validating";
            case solver_state::starting: return "Starting";
            case solver_state::running: return "Running";
            case solver_state::cancelling: return "Cancelling";
            case solver_state::completed: return "Completed";
            case solver_state::failed: return "Failed";
        }
        return "Unknown";
    }

    solver_controls controls_for_state(const solver_state state) noexcept
    {
        switch (state) {
            case solver_state::idle:
            case solver_state::completed:
            case solver_state::failed:
                return solver_controls{
                    .validate_enabled = true,
                    .solve_enabled = true,
                    .cancel_enabled = false
                };
            case solver_state::validating:
            case solver_state::starting:
                return solver_controls{
                    .validate_enabled = false,
                    .solve_enabled = false,
                    .cancel_enabled = true
                };
            case solver_state::running:
            case solver_state::cancelling:
                return solver_controls{
                    .validate_enabled = false,
                    .solve_enabled = false,
                    .cancel_enabled = state == solver_state::running
                };
        }
        return solver_controls{};
    }

}
