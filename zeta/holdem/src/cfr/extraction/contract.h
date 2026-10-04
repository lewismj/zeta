#pragma once

#include "board.h"

#include <cmath>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <optional>
#include <ostream>
#include <span>
#include <string_view>

namespace zeta::holdem::cfr::extraction {

    /**
     * Mathematical Contract & Conditioning Specifications
     * 
     * Core Architectural Boundaries:
     *   CFR State != Extraction Result != Artifact != UI Model
     * 
     * 1. Extensive-Form Information Set vs. Strategy Decision Context:
     *    - Standard Extensive-Form Infoset:
     *        I(S, h) = acting_seat + public_state + betting_history + player's private cards (h)
     *        Opponent private cards and hidden chance events are strictly excluded.
     *    - Range Strategy Decision Context (S):
     *        S = public_state + betting_history + acting_seat (identified by strategy_context_id)
     *        Owns the vectorized range strategy across all legal private holdings h in H_legal(S).
     * 
     * 2. Q -> V -> A Canonical Chain:
     *    - Q_profile(n, h, a): Local one-step action intervention at node n, frozen average profile thereafter.
     *    - V_profile(n, h) = Sum_a sigma_avg(a | I, h) * Q_profile(n, h, a).
     *    - A_profile(n, h, a) = Q_profile(n, h, a) - V_profile(n, h) [Profile Advantage, never termed regret].
     *    - Strategy-Weighted Advantage Identity: Sum_a sigma_avg(a | I, h) * A_profile(n, h, a) = 0.
     * 
     * 3. Precision Rule:
     *    - range_reach_weight is derived on demand by promoting range_weight and reach_probability to double
     *      prior to multiplication.
     *    - All reach-weighted aggregations accumulate strictly in double.
     */

    using strategy_context_id = uint32_t;
    using combo_local_index = uint32_t;
    using action_index = uint16_t;
    using combination_index = ::zeta::holdem::combination_index;

    inline constexpr strategy_context_id INVALID_STRATEGY_CONTEXT_ID = std::numeric_limits<strategy_context_id>::max();
    inline constexpr combo_local_index INVALID_COMBO_LOCAL_INDEX = std::numeric_limits<combo_local_index>::max();
    inline constexpr action_index INVALID_ACTION_INDEX = std::numeric_limits<action_index>::max();

    inline constexpr uint32_t CURRENT_SCHEMA_VERSION = 4;
    inline constexpr uint32_t CURRENT_EXTRACTION_VERSION = 1;

    inline constexpr double DEFAULT_ADVANTAGE_IDENTITY_TOLERANCE = 1e-5;
    inline constexpr double DEFAULT_STRATEGY_NORMALIZATION_TOLERANCE = 1e-5;
    inline constexpr double DEFAULT_EQUITY_TOLERANCE = 1e-6;
    inline constexpr double DEFAULT_ZERO_SUM_TOLERANCE = 1e-5;

    /**
     * Multi-dimensional quality and solve enums.
     */

    enum class solve_mode : uint8_t {
        normal = 0,
        preview = 1
    };

    enum class termination_reason : uint8_t {
        iteration_limit = 0,        /**< Configured iteration budget exhausted (normal schedule completion). */
        exploitability_target = 1,  /**< Converged to target exploitability. */
        user_interrupted = 2        /**< Early cancellation requested. */
    };

    enum class convergence_status : uint8_t {
        target_met = 0,             /**< Convergence criterion verified (exploitability <= target). */
        iteration_limited = 1,      /**< Reached iteration ceiling without meeting target exploitability. */
        not_evaluated = 2           /**< Exploitability was not evaluated. */
    };

    enum class evaluation_method : uint8_t {
        exact = 0,                  /**< Exact mathematical evaluation of the represented game graph. */
        sampled = 1                 /**< Derived via Monte Carlo rollouts or lossy sampling. */
    };

    enum class abstraction_mode : uint8_t {
        exact = 0,
        suit_isomorphic = 1,
        range_abstracted = 2,
        mixed = 3
    };

    enum class artifact_export_mode : uint8_t {
        summary = 0,                /**< High-level topology, public states, seat values, range action frequencies. */
        standard = 1,               /**< Summary + strategy, profile values, equities, and classifications. */
        full = 2                    /**< Standard + complete diagnostic tables and chance/terminal combo surfaces. */
    };

    enum class solve_status : uint8_t {
        iteration_limited = 0,
        converged = 1,
        not_evaluated = 2
    };

    [[nodiscard]] constexpr const char* to_string(const solve_mode mode) noexcept
    {
        using enum solve_mode;
        switch (mode) {
            case normal:  return "normal";
            case preview: return "preview";
        }
        return "unknown";
    }

    [[nodiscard]] constexpr const char* to_string(const termination_reason reason) noexcept
    {
        using enum termination_reason;
        switch (reason) {
            case iteration_limit:       return "iteration_limit";
            case exploitability_target: return "exploitability_target";
            case user_interrupted:      return "user_interrupted";
        }
        return "unknown";
    }

    [[nodiscard]] constexpr const char* to_string(const convergence_status status) noexcept
    {
        using enum convergence_status;
        switch (status) {
            case target_met:        return "target_met";
            case iteration_limited: return "iteration_limited";
            case not_evaluated:     return "not_evaluated";
        }
        return "unknown";
    }

    [[nodiscard]] constexpr const char* to_string(const evaluation_method method) noexcept
    {
        using enum evaluation_method;
        switch (method) {
            case exact:   return "exact";
            case sampled: return "sampled";
        }
        return "unknown";
    }

    [[nodiscard]] constexpr const char* to_string(const abstraction_mode mode) noexcept
    {
        using enum abstraction_mode;
        switch (mode) {
            case exact:            return "exact";
            case suit_isomorphic:  return "suit_isomorphic";
            case range_abstracted: return "range_abstracted";
            case mixed:            return "mixed";
        }
        return "unknown";
    }

    [[nodiscard]] constexpr const char* to_string(const artifact_export_mode mode) noexcept
    {
        using enum artifact_export_mode;
        switch (mode) {
            case summary:  return "summary";
            case standard: return "standard";
            case full:     return "full";
        }
        return "unknown";
    }

    [[nodiscard]] constexpr const char* to_string(const solve_status status) noexcept
    {
        using enum solve_status;
        switch (status) {
            case iteration_limited: return "iteration_limited";
            case converged:         return "converged";
            case not_evaluated:     return "not_evaluated";
        }
        return "unknown";
    }

    [[nodiscard]] constexpr std::optional<solve_mode> parse_solve_mode(const std::string_view sv) noexcept
    {
        if (sv == "normal")  return solve_mode::normal;
        if (sv == "preview") return solve_mode::preview;
        return std::nullopt;
    }

    [[nodiscard]] constexpr std::optional<termination_reason> parse_termination_reason(const std::string_view sv) noexcept
    {
        if (sv == "iteration_limit")       return termination_reason::iteration_limit;
        if (sv == "exploitability_target") return termination_reason::exploitability_target;
        if (sv == "user_interrupted")      return termination_reason::user_interrupted;
        return std::nullopt;
    }

    [[nodiscard]] constexpr std::optional<convergence_status> parse_convergence_status(const std::string_view sv) noexcept
    {
        if (sv == "target_met")        return convergence_status::target_met;
        if (sv == "iteration_limited") return convergence_status::iteration_limited;
        if (sv == "not_evaluated")     return convergence_status::not_evaluated;
        return std::nullopt;
    }

    [[nodiscard]] constexpr std::optional<evaluation_method> parse_evaluation_method(const std::string_view sv) noexcept
    {
        if (sv == "exact")   return evaluation_method::exact;
        if (sv == "sampled") return evaluation_method::sampled;
        return std::nullopt;
    }

    [[nodiscard]] constexpr std::optional<abstraction_mode> parse_abstraction_mode(const std::string_view sv) noexcept
    {
        if (sv == "exact")            return abstraction_mode::exact;
        if (sv == "suit_isomorphic")  return abstraction_mode::suit_isomorphic;
        if (sv == "range_abstracted") return abstraction_mode::range_abstracted;
        if (sv == "mixed")            return abstraction_mode::mixed;
        return std::nullopt;
    }

    [[nodiscard]] constexpr std::optional<artifact_export_mode> parse_artifact_export_mode(const std::string_view sv) noexcept
    {
        if (sv == "summary")  return artifact_export_mode::summary;
        if (sv == "standard") return artifact_export_mode::standard;
        if (sv == "full")     return artifact_export_mode::full;
        return std::nullopt;
    }

    [[nodiscard]] constexpr std::optional<solve_status> parse_solve_status(const std::string_view sv) noexcept
    {
        if (sv == "iteration_limited") return solve_status::iteration_limited;
        if (sv == "converged")         return solve_status::converged;
        if (sv == "not_evaluated")     return solve_status::not_evaluated;
        return std::nullopt;
    }

    inline std::ostream& operator<<(std::ostream& os, const solve_mode mode)
    {
        return os << to_string(mode);
    }

    inline std::ostream& operator<<(std::ostream& os, const termination_reason reason)
    {
        return os << to_string(reason);
    }

    inline std::ostream& operator<<(std::ostream& os, const convergence_status status)
    {
        return os << to_string(status);
    }

    inline std::ostream& operator<<(std::ostream& os, const evaluation_method method)
    {
        return os << to_string(method);
    }

    inline std::ostream& operator<<(std::ostream& os, const abstraction_mode mode)
    {
        return os << to_string(mode);
    }

    inline std::ostream& operator<<(std::ostream& os, const artifact_export_mode mode)
    {
        return os << to_string(mode);
    }

    inline std::ostream& operator<<(std::ostream& os, const solve_status status)
    {
        return os << to_string(status);
    }

    /**
     * Reach representation for a private combo holding at a tree node.
     */
    struct combo_reach {
        float range_weight = 1.0f;       /**< Initial preflop range weight w_0(h). */
        float reach_probability = 1.0f;  /**< Conditional path probability pi_i(n | h). */

        /**
         * Derived remaining range mass: w_0(h) * pi_i(n | h) promoted to double.
         */
        [[nodiscard]] constexpr double range_reach_weight() const noexcept
        {
            return static_cast<double>(range_weight) * static_cast<double>(reach_probability);
        }
    };

    /**
     * Evaluated action payoff and deviation advantage for a private combo at a player node.
     */
    struct action_profile_evaluation {
        double q_profile = 0.0;          /**< Q_profile(n, h, a): payoff under local action intervention a. */
        double profile_advantage = 0.0;  /**< A_profile(n, h, a) = Q - V: advantage over average profile (never called regret). */
    };

    /**
     * Node-level evaluated reach mass and strategic values for a specific seat.
     */
    struct seat_reach_and_value {
        uint8_t seat = 0;
        double range_reach_mass = 0.0;       /**< W_i(n) = Sum_h w_0(h) * pi_i(n | h) (hero range mass). */
        double reach_weighted_value = 0.0;   /**< Sum_h range_reach_weight(n, h) * V_i(n, h). */
        double conditional_range_ev = 0.0;   /**< reach_weighted_value / range_reach_mass. */
        double counterfactual_value = 0.0;   /**< Sum_h w_0(h) * Pi_-i(n | h) * pi_c(n) * V_i(n, h). */
    };

    /**
     * Mathematical Contract Functions & Formulations
     */

    /**
     * Derived remaining range mass for combo h of player i conditional on player i's path to node n:
     *   range_reach_weight(n, h) = static_cast<double>(w_0(h)) * static_cast<double>(pi_i(n | h))
     */
    [[nodiscard]] constexpr double compute_range_reach_weight(
        const float range_weight,
        const float reach_probability) noexcept
    {
        return static_cast<double>(range_weight) * static_cast<double>(reach_probability);
    }

    /**
     * Joint realization probability mass incorporating hero, opponent, and chance reach:
     *   joint_reach_mass(n, h) = w_0(h) * pi_i(n | h) * Pi_-i(n | h) * pi_c(n)
     */
    [[nodiscard]] constexpr double compute_joint_reach_mass(
        const float range_weight,
        const float reach_probability,
        const float opponent_reach,
        const double chance_reach) noexcept
    {
        return static_cast<double>(range_weight) *
               static_cast<double>(reach_probability) *
               static_cast<double>(opponent_reach) *
               chance_reach;
    }

    /**
     * Profile combo value under average strategy sigma_avg:
     *   V_profile(n, h) = Sum_a sigma_avg(a | I, h) * Q_profile(n, h, a)
     */
    [[nodiscard]] inline double compute_combo_profile_value(
        const std::span<const float> strategy,
        const std::span<const double> q_profile) noexcept
    {
        const std::size_t n = std::min(strategy.size(), q_profile.size());
        double v = 0.0;
        for (std::size_t a = 0; a < n; ++a) {
            v += static_cast<double>(strategy[a]) * q_profile[a];
        }
        return v;
    }

    /**
     * Profile deviation advantage:
     *   A_profile(n, h, a) = Q_profile(n, h, a) - V_profile(n, h)
     */
    [[nodiscard]] constexpr double compute_profile_advantage(
        const double q_profile,
        const double v_profile) noexcept
    {
        return q_profile - v_profile;
    }

    /**
     * Strategy normalization check: Sum_a sigma_avg(a | I, h) == 1.0.
     */
    [[nodiscard]] inline bool verify_strategy_normalized(
        const std::span<const float> strategy,
        const double tolerance = DEFAULT_STRATEGY_NORMALIZATION_TOLERANCE) noexcept
    {
        if (strategy.empty()) {
            return false;
        }
        double sum = 0.0;
        for (const float p : strategy) {
            if (p < -static_cast<float>(tolerance)) {
                return false;
            }
            sum += static_cast<double>(p);
        }
        return std::abs(sum - 1.0) <= tolerance;
    }

    /**
     * Strategy-Weighted Advantage Identity check:
     *   Sum_a sigma_avg(a | I, h) * A_profile(n, h, a) == 0.
     */
    [[nodiscard]] inline bool verify_strategy_weighted_advantage_identity(
        const std::span<const float> strategy,
        const std::span<const double> advantages,
        const double tolerance = DEFAULT_ADVANTAGE_IDENTITY_TOLERANCE) noexcept
    {
        if (strategy.size() != advantages.size() || strategy.empty()) {
            return false;
        }
        double sum = 0.0;
        for (std::size_t a = 0; a < strategy.size(); ++a) {
            sum += static_cast<double>(strategy[a]) * advantages[a];
        }
        return std::abs(sum) <= tolerance;
    }

    /**
     * Total remaining active range mass of player i reaching node n:
     *   range_reach_mass(n) = W_i(n) = Sum_h w_0(h) * pi_i(n | h)
     */
    [[nodiscard]] inline double compute_range_reach_mass(
        const std::span<const float> range_weights,
        const std::span<const float> reach_probabilities) noexcept
    {
        const std::size_t n = std::min(range_weights.size(), reach_probabilities.size());
        double total_mass = 0.0;
        for (std::size_t i = 0; i < n; ++i) {
            total_mass += static_cast<double>(range_weights[i]) * static_cast<double>(reach_probabilities[i]);
        }
        return total_mass;
    }

    /**
     * Unnormalized EV mass carried by reaching range:
     *   reach_weighted_ev(n) = Sum_h range_reach_weight(n, h) * V_i(n, h)
     */
    [[nodiscard]] inline double compute_reach_weighted_ev(
        const std::span<const float> range_weights,
        const std::span<const float> reach_probabilities,
        const std::span<const double> combo_values) noexcept
    {
        const std::size_t n = std::min({range_weights.size(), reach_probabilities.size(), combo_values.size()});
        double ev_mass = 0.0;
        for (std::size_t i = 0; i < n; ++i) {
            const double w = static_cast<double>(range_weights[i]) * static_cast<double>(reach_probabilities[i]);
            ev_mass += w * combo_values[i];
        }
        return ev_mass;
    }

    /**
     * Normalized strategic EV conditional on reaching node n:
     *   conditional_range_ev(n) = reach_weighted_ev(n) / range_reach_mass(n)
     */
    [[nodiscard]] constexpr double compute_conditional_range_ev(
        const double reach_weighted_ev,
        const double range_reach_mass) noexcept
    {
        return (range_reach_mass > 0.0) ? (reach_weighted_ev / range_reach_mass) : 0.0;
    }

    /**
     * CFR-style counterfactual value for Heads-Up (2-player):
     *   CFV_i(n) = Sum_h w_{0,i}(h) * pi_-i(n | h) * pi_c(n) * V_i(n, h)
     */
    [[nodiscard]] inline double compute_counterfactual_value(
        const std::span<const float> range_weights,
        const std::span<const float> opponent_reaches,
        const double chance_reach,
        const std::span<const double> combo_values) noexcept
    {
        const std::size_t n = std::min({range_weights.size(), opponent_reaches.size(), combo_values.size()});
        double cfv = 0.0;
        for (std::size_t i = 0; i < n; ++i) {
            const double w_opp = static_cast<double>(range_weights[i]) *
                                static_cast<double>(opponent_reaches[i]) *
                                chance_reach;
            cfv += w_opp * combo_values[i];
        }
        return cfv;
    }

    /**
     * CFR-style counterfactual value for Multiway (N-player):
     *   CFV_i(n) = Sum_h w_{0,i}(h) * Pi_-i(n | h) * pi_c(n) * V_i(n, h)
     */
    [[nodiscard]] inline double compute_counterfactual_value_multiway(
        const std::span<const float> range_weights,
        const std::span<const double> joint_opponent_reaches,
        const double chance_reach,
        const std::span<const double> combo_values) noexcept
    {
        const std::size_t n = std::min({range_weights.size(), joint_opponent_reaches.size(), combo_values.size()});
        double cfv = 0.0;
        for (std::size_t i = 0; i < n; ++i) {
            const double w_opp = static_cast<double>(range_weights[i]) *
                                joint_opponent_reaches[i] *
                                chance_reach;
            cfv += w_opp * combo_values[i];
        }
        return cfv;
    }

    [[nodiscard]] constexpr double math_abs(const double v) noexcept
    {
        return (v < 0.0) ? -v : v;
    }

    /**
     * Zero-sum payoff conservation check for Heads-Up (2-player):
     *   V_1(n) + V_2(n) == 0 (subject to rake and zero-sum payoff conventions).
     */
    [[nodiscard]] constexpr bool verify_heads_up_zero_sum(
        const double ev1,
        const double ev2,
        const double tolerance = DEFAULT_ZERO_SUM_TOLERANCE) noexcept
    {
        return math_abs(ev1 + ev2) <= tolerance;
    }

    /**
     * Showdown equity range check: Equity in [0, 1].
     */
    [[nodiscard]] constexpr bool verify_showdown_equity_bounds(
        const double equity,
        const double tolerance = DEFAULT_EQUITY_TOLERANCE) noexcept
    {
        return equity >= -tolerance && equity <= 1.0 + tolerance;
    }

    /**
     * Heads-Up showdown equity conservation: E_1(n) + E_2(n) == 1.0.
     */
    [[nodiscard]] constexpr bool verify_heads_up_equity_conservation(
        const double equity1,
        const double equity2,
        const double tolerance = DEFAULT_EQUITY_TOLERANCE) noexcept
    {
        return math_abs(equity1 + equity2 - 1.0) <= tolerance;
    }

    /**
     * Multiway showdown equity conservation: Sum_i E_i(n) == 1.0 with fractional ties.
     */
    [[nodiscard]] inline bool verify_multiway_equity_conservation(
        const std::span<const double> equities,
        const double tolerance = DEFAULT_EQUITY_TOLERANCE) noexcept
    {
        double sum = 0.0;
        for (const double e : equities) {
            sum += e;
        }
        return std::abs(sum - 1.0) <= tolerance;
    }

    /**
     * Derived category summary math reductions (Section 2.10):
     */

    /**
     * Category Reach Frequency:
     *   F(C) = Sum_{h in C} range_reach_weight(n, h) / range_reach_mass(n)
     */
    [[nodiscard]] constexpr double compute_category_reach_frequency(
        const double category_reach_weight_sum,
        const double range_reach_mass) noexcept
    {
        return (range_reach_mass > 0.0) ? (category_reach_weight_sum / range_reach_mass) : 0.0;
    }

    /**
     * Category-Conditional Action Frequency:
     *   F(a | C) = Sum_{h in C} range_reach_weight(n, h) * sigma_avg(a | I, h) / Sum_{h in C} range_reach_weight(n, h)
     */
    [[nodiscard]] constexpr double compute_category_conditional_action_frequency(
        const double category_action_reach_weight_sum,
        const double category_reach_weight_sum) noexcept
    {
        return (category_reach_weight_sum > 0.0) ? (category_action_reach_weight_sum / category_reach_weight_sum) : 0.0;
    }

    /**
     * Category Average Strategic EV:
     *   EV_avg(C) = Sum_{h in C} range_reach_weight(n, h) * V(n, h) / Sum_{h in C} range_reach_weight(n, h)
     */
    [[nodiscard]] constexpr double compute_category_average_ev(
        const double category_reach_weighted_ev_sum,
        const double category_reach_weight_sum) noexcept
    {
        return (category_reach_weight_sum > 0.0) ? (category_reach_weighted_ev_sum / category_reach_weight_sum) : 0.0;
    }

    /**
     * Category Average Showdown Equity:
     *   Equity_avg(C) = Sum_{h in C} range_reach_weight(n, h) * Equity(n, h) / Sum_{h in C} range_reach_weight(n, h)
     */
    [[nodiscard]] constexpr double compute_category_average_equity(
        const double category_reach_weighted_equity_sum,
        const double category_reach_weight_sum) noexcept
    {
        return (category_reach_weight_sum > 0.0) ? (category_reach_weighted_equity_sum / category_reach_weight_sum) : 0.0;
    }

}
