#include "viewmodels/lock_view_model.h"

#include <algorithm>
#include <cctype>
#include <cmath>
#include <sstream>

namespace zeta::holdem::ui::viewmodels {

    namespace {

        [[nodiscard]] std::string trimmed(const std::string& text)
        {
            const auto begin = text.find_first_not_of(" \t\r\n");
            if (begin == std::string::npos) {
                return {};
            }
            const auto end = text.find_last_not_of(" \t\r\n");
            return text.substr(begin, end - begin + 1u);
        }

        [[nodiscard]] std::string percent_text(const double frequency)
        {
            std::ostringstream out;
            const auto percent = frequency * 100.0;
            if (std::fabs(percent - std::round(percent)) < 0.05) {
                out << static_cast<long long>(std::llround(percent)) << '%';
            } else {
                out.setf(std::ios::fixed);
                out.precision(1);
                out << percent << '%';
            }
            return out.str();
        }

    }

    std::string display_lock_step(const std::string& step)
    {
        if (step == cli::node_lock_any_card) {
            return "Any card";
        }
        if (step == "all_in") {
            return "All In";
        }
        if (step == "fold") {
            return "Fold";
        }
        if (step == "check") {
            return "Check";
        }
        if (step == "call") {
            return "Call";
        }
        if (step.starts_with("bet_")) {
            return "Bet " + step.substr(4u);
        }
        if (step.starts_with("raise_")) {
            return "Raise " + step.substr(6u);
        }
        return step;
    }

    std::string format_lock_path(const std::vector<std::string>& path)
    {
        if (path.empty()) {
            return "Root";
        }
        std::string out;
        for (const auto& step : path) {
            out += out.empty() ? display_lock_step(step) : " / " + display_lock_step(step);
        }
        return out;
    }

    std::string format_lock_hands(const cli::node_lock& lock)
    {
        return lock.hands.empty() ? std::string{"All hands"} : lock.hands;
    }

    std::string format_lock_actions(const cli::node_lock& lock)
    {
        std::string out;
        for (const auto& action : lock.actions) {
            const auto part = display_lock_step(action.action) + " " + percent_text(action.frequency);
            out += out.empty() ? part : ", " + part;
        }
        return out;
    }

    std::expected<cli::node_lock, std::string> make_node_lock(
        const std::vector<std::string>& path,
        const std::string& hands,
        const std::vector<lock_action_entry>& entries)
    {
        cli::node_lock lock{.path = path, .hands = trimmed(hands), .actions = {}};
        double total = 0.0;
        std::size_t locked_count = 0;
        for (const auto& entry : entries) {
            if (!entry.locked) {
                continue;
            }
            if (!std::isfinite(entry.percent) || entry.percent < 0.0 || entry.percent > 100.0) {
                return std::unexpected("The frequency of " + display_lock_step(entry.action) + " must be between 0% and 100%.");
            }
            ++locked_count;
            total += entry.percent;
            lock.actions.push_back(cli::node_lock_action{.action = entry.action, .frequency = entry.percent / 100.0});
        }
        if (locked_count == 0) {
            return std::unexpected(std::string{"Lock at least one action."});
        }
        if (total > 100.0 + 1.0e-6) {
            return std::unexpected("The locked frequencies add up to " + percent_text(total / 100.0) + ", more than 100%.");
        }
        if (locked_count == entries.size() && std::fabs(total - 100.0) > 1.0e-4) {
            return std::unexpected("Every action is locked, so the frequencies must add up to 100% (they add up to "
                + percent_text(total / 100.0) + ").");
        }
        std::vector<cli::node_lock> normalized{lock};
        if (auto result = cli::normalize_node_locks(normalized); !result) {
            // Drop the "Lock 1: " prefix the solver uses to number locks in a spot.
            auto message = result.error().message;
            if (const auto colon = message.find(": "); message.starts_with("Lock ") && colon != std::string::npos) {
                message = message.substr(colon + 2u);
            }
            if (!message.empty()) {
                message[0] = static_cast<char>(std::toupper(static_cast<unsigned char>(message[0])));
            }
            return std::unexpected(message);
        }
        return normalized.front();
    }

    bool lock_path_matches(const std::vector<std::string>& lock_path, const std::vector<std::string>& node_path)
    {
        if (lock_path.size() != node_path.size()) {
            return false;
        }
        for (std::size_t index = 0; index < lock_path.size(); ++index) {
            if (lock_path[index] != node_path[index] && lock_path[index] != cli::node_lock_any_card) {
                return false;
            }
        }
        return true;
    }

    std::vector<std::size_t> locks_at_node(
        const std::vector<cli::node_lock>& locks,
        const std::vector<std::string>& node_path)
    {
        std::vector<std::size_t> out;
        for (std::size_t index = 0; index < locks.size(); ++index) {
            if (lock_path_matches(locks[index].path, node_path)) {
                out.push_back(index);
            }
        }
        return out;
    }

    std::vector<cli::node_lock> with_lock(std::vector<cli::node_lock> locks, cli::node_lock lock)
    {
        const auto existing = std::ranges::find_if(locks, [&lock](const cli::node_lock& candidate) {
            return candidate.path == lock.path && candidate.hands == lock.hands;
        });
        if (existing != locks.end()) {
            *existing = std::move(lock);
        } else {
            locks.push_back(std::move(lock));
        }
        return locks;
    }

    std::vector<cli::node_lock> without_locks_at(
        std::vector<cli::node_lock> locks,
        const std::vector<std::string>& path)
    {
        std::erase_if(locks, [&path](const cli::node_lock& lock) {
            return lock.path == path;
        });
        return locks;
    }

}
