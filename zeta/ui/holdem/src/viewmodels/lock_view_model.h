#pragma once

#include "../spot_document.h"

#include <expected>
#include <string>
#include <vector>

namespace zeta::holdem::ui::viewmodels {

    /** One action row of the lock editor. */
    struct lock_action_entry {
        std::string action;    ///< Action label at the node.
        bool locked = false;   ///< Whether the frequency is fixed.
        double percent = 0.0;  ///< Fixed frequency in percent, used when locked.
    };

    /** A lock action or path step for display: "check" -> "Check", "bet_75" -> "Bet 75", "*" -> "Any card". */
    [[nodiscard]] std::string display_lock_step(const std::string& step);

    /** "Root" for the root, otherwise the displayed steps joined by " / ". */
    [[nodiscard]] std::string format_lock_path(const std::vector<std::string>& path);

    /** "All hands" or the lock's hands. */
    [[nodiscard]] std::string format_lock_hands(const cli::node_lock& lock);

    /** The constrained actions, e.g. "Fold 20%, Call 70%"; unlisted actions are free. */
    [[nodiscard]] std::string format_lock_actions(const cli::node_lock& lock);

    /**
     * Builds a lock from the editor rows. Fails with a message the editor can show when no
     * action is locked, a frequency is outside 0-100%, the locked frequencies add up to more
     * than 100% (or not to 100% when every action is locked), or the hands do not parse.
     */
    [[nodiscard]] std::expected<cli::node_lock, std::string> make_node_lock(
        const std::vector<std::string>& path,
        const std::string& hands,
        const std::vector<lock_action_entry>& entries);

    /** True when a lock path reaches the node path; "*" matches any dealt card. */
    [[nodiscard]] bool lock_path_matches(const std::vector<std::string>& lock_path, const std::vector<std::string>& node_path);

    /** Indices of the locks whose path reaches the node path. */
    [[nodiscard]] std::vector<std::size_t> locks_at_node(
        const std::vector<cli::node_lock>& locks,
        const std::vector<std::string>& node_path);

    /** Adds the lock, replacing one with the same path and hands. */
    [[nodiscard]] std::vector<cli::node_lock> with_lock(std::vector<cli::node_lock> locks, cli::node_lock lock);

    /** Removes every lock whose path is exactly `path` (wildcard locks that also reach it stay). */
    [[nodiscard]] std::vector<cli::node_lock> without_locks_at(
        std::vector<cli::node_lock> locks,
        const std::vector<std::string>& path);

}
