#pragma once

#include <algorithm>
#include <thread>

namespace zeta::holdem::ui {

    inline constexpr int min_solver_iterations = 1;
    inline constexpr int max_solver_iterations = 1'000'000;
    inline constexpr int min_worker_threads = 1;
    inline constexpr int max_worker_threads = 64;

    [[nodiscard]] inline int available_worker_threads() noexcept
    {
        const auto hardware_threads = std::thread::hardware_concurrency();
        if (hardware_threads == 0) {
            return max_worker_threads;
        }
        return std::clamp(static_cast<int>(hardware_threads), min_worker_threads, max_worker_threads);
    }

}
