#pragma once

#include <cstdint>
#include <expected>
#include <string>

namespace zeta::holdem::ui {

    enum class solver_state : uint8_t {
        idle,
        validating,
        starting,
        running,
        cancelling,
        completed,
        failed
    };

    struct solver_controls {
        bool validate_enabled = true;
        bool solve_enabled = true;
        bool cancel_enabled = false;
    };

    /**
     * Tracks solver lifecycle transitions and derives button enablement from the current state.
     */
    class solver_state_machine {
    public:
        [[nodiscard]] solver_state state() const noexcept;
        [[nodiscard]] std::expected<void, std::string> transition_to(solver_state next_state);
        [[nodiscard]] solver_controls controls() const noexcept;

    private:
        solver_state state_ = solver_state::idle;
    };

    [[nodiscard]] const char* to_string(solver_state state) noexcept;
    [[nodiscard]] solver_controls controls_for_state(solver_state state) noexcept;

}
