#pragma once

#include "cfr/extraction/contract.h"
#include "eval/categorizer.h"

#include <stdexcept>
#include <span>
#include <utility>
#include <vector>

namespace zeta::holdem::cfr::extraction {

    struct strategy_surface_entry {
        float average_strategy = 0.0f;
    };

    struct regret_surface_entry {
        double cumulative_regret = 0.0;
    };

    struct combo_reach_entry {
        float range_weight = 0.0f;
        float reach_probability = 0.0f;
    };

    struct combo_value_entry {
        double combo_profile_value = 0.0;
    };

    struct action_value_entry {
        double q_profile = 0.0;
        double profile_advantage = 0.0;
    };

    struct seat_value {
        double range_reach_mass = 0.0;
        double reach_weighted_value = 0.0;
        double conditional_range_ev = 0.0;
        double counterfactual_value = 0.0;
    };

    struct strategy_surface_record {
        uint32_t strategy_begin = 0;
        uint32_t combo_count = 0;
        uint16_t action_count = 0;
    };

    struct node_record {
        uint32_t node_id = 0;
        strategy_context_id strategy_context_id = INVALID_STRATEGY_CONTEXT_ID;
        uint32_t public_state_id = 0;
        uint32_t combo_begin = 0;
        uint32_t combo_count = 0;
        uint32_t action_val_begin = 0;
        uint16_t action_count = 0;
        uint32_t seat_value_begin = 0;
        uint16_t seat_value_count = 0;
    };

    inline constexpr std::size_t two_action_node_footprint_budget_bytes = 74'880;
    inline constexpr std::size_t canonical_combo_domain_count = ::zeta::holdem::combination_count;
    inline constexpr std::size_t postflop_memory_budget_combo_count = 1'100;

    [[nodiscard]] constexpr std::size_t node_footprint_bytes(
        const std::size_t combo_count,
        const std::size_t action_count,
        const std::size_t seat_count = 1u) noexcept
    {
        return sizeof(node_record)
            + sizeof(strategy_surface_record)
            + combo_count * action_count * sizeof(strategy_surface_entry)
            + combo_count * sizeof(combo_reach_entry)
            + combo_count * sizeof(combo_value_entry)
            + combo_count * action_count * sizeof(action_value_entry)
            + seat_count * sizeof(seat_value)
            + combo_count * sizeof(float)
            + combo_count * sizeof(hand_category_classification);
    }

    static_assert(node_footprint_bytes(postflop_memory_budget_combo_count, 2u) <= two_action_node_footprint_budget_bytes);

    class strategy_view {
    public:
        strategy_view(std::span<const strategy_surface_entry> entries, uint32_t combo_count, uint16_t action_count) noexcept
            : entries_(entries), combo_count_(combo_count), action_count_(action_count) {}

        [[nodiscard]] float frequency(uint32_t combo_local_index, action_index action) const noexcept
        {
            return entries_[combo_local_index * action_count_ + action].average_strategy;
        }

        [[nodiscard]] uint32_t combo_count() const noexcept { return combo_count_; }
        [[nodiscard]] uint16_t action_count() const noexcept { return action_count_; }

        [[nodiscard]] std::span<const strategy_surface_entry> entries() const noexcept
        {
            return entries_;
        }

    private:
        std::span<const strategy_surface_entry> entries_;
        uint32_t combo_count_ = 0;
        uint16_t action_count_ = 0;
    };

    class value_view {
    public:
        value_view(
            std::span<const combo_reach_entry> reaches,
            std::span<const combo_value_entry> combo_vals,
            std::span<const action_value_entry> action_vals,
            const seat_value& seat_val,
            uint16_t action_count) noexcept
            : reaches_(reaches), combo_vals_(combo_vals), action_vals_(action_vals), seat_val_(seat_val), action_count_(action_count) {}

        [[nodiscard]] double range_reach_mass() const noexcept { return seat_val_.range_reach_mass; }
        [[nodiscard]] double reach_weighted_ev() const noexcept { return seat_val_.reach_weighted_value; }
        [[nodiscard]] double conditional_range_ev() const noexcept { return seat_val_.conditional_range_ev; }
        [[nodiscard]] double counterfactual_value() const noexcept { return seat_val_.counterfactual_value; }
        [[nodiscard]] double combo_ev(uint32_t combo_local_index) const noexcept { return combo_vals_[combo_local_index].combo_profile_value; }
        [[nodiscard]] double q_value(uint32_t combo_local_index, action_index action) const noexcept { return action_vals_[combo_local_index * action_count_ + action].q_profile; }
        [[nodiscard]] double profile_advantage(uint32_t combo_local_index, action_index action) const noexcept { return action_vals_[combo_local_index * action_count_ + action].profile_advantage; }
        [[nodiscard]] float reach_probability(uint32_t combo_local_index) const noexcept { return reaches_[combo_local_index].reach_probability; }
        [[nodiscard]] float range_weight(uint32_t combo_local_index) const noexcept { return reaches_[combo_local_index].range_weight; }
        [[nodiscard]] double range_reach_weight(uint32_t combo_local_index) const noexcept
        {
            return static_cast<double>(reaches_[combo_local_index].range_weight) * static_cast<double>(reaches_[combo_local_index].reach_probability);
        }

    private:
        std::span<const combo_reach_entry> reaches_;
        std::span<const combo_value_entry> combo_vals_;
        std::span<const action_value_entry> action_vals_;
        seat_value seat_val_{};
        uint16_t action_count_ = 0;
    };

    class equity_view {
    public:
        explicit equity_view(std::span<const float> equities) noexcept : equities_(equities) {}
        [[nodiscard]] double showdown_equity(uint32_t combo_local_index) const noexcept
        {
            return static_cast<double>(equities_[combo_local_index]);
        }

    private:
        std::span<const float> equities_;
    };

    class category_view {
    public:
        category_view(
            std::span<const hand_category_classification> categories,
            std::span<const combo_reach_entry> reaches,
            std::span<const combo_value_entry> values,
            std::span<const strategy_surface_entry> strategies,
            uint16_t action_count) noexcept
            : categories_(categories), reaches_(reaches), values_(values), strategies_(strategies), action_count_(action_count) {}

        [[nodiscard]] hand_category_classification classification(uint32_t combo_local_index) const noexcept
        {
            return categories_[combo_local_index];
        }

        [[nodiscard]] double range_reach_weight(uint32_t combo_local_index) const noexcept
        {
            return static_cast<double>(reaches_[combo_local_index].range_weight)
                * static_cast<double>(reaches_[combo_local_index].reach_probability);
        }

        [[nodiscard]] double combo_ev(uint32_t combo_local_index) const noexcept
        {
            return values_[combo_local_index].combo_profile_value;
        }

        [[nodiscard]] float frequency(uint32_t combo_local_index, action_index action) const noexcept
        {
            return strategies_[combo_local_index * action_count_ + action].average_strategy;
        }

    private:
        std::span<const hand_category_classification> categories_;
        std::span<const combo_reach_entry> reaches_;
        std::span<const combo_value_entry> values_;
        std::span<const strategy_surface_entry> strategies_;
        uint16_t action_count_ = 0;
    };

    class node_view {
    public:
        node_view(
            const node_record& record,
            strategy_view strategy,
            value_view values,
            equity_view equity,
            category_view categories) noexcept
            : record_(record), strategy_(strategy), values_(values), equity_(equity), categories_(categories) {}

        [[nodiscard]] const node_record& record() const noexcept { return record_; }
        [[nodiscard]] uint32_t node_id() const noexcept { return record_.node_id; }
        [[nodiscard]] uint32_t strategy_context_id() const noexcept { return record_.strategy_context_id; }
        [[nodiscard]] uint32_t public_state_id() const noexcept { return record_.public_state_id; }
        [[nodiscard]] uint32_t combo_begin() const noexcept { return record_.combo_begin; }
        [[nodiscard]] uint32_t combo_count() const noexcept { return record_.combo_count; }
        [[nodiscard]] uint16_t action_count() const noexcept { return record_.action_count; }
        [[nodiscard]] strategy_view strategy() const noexcept { return strategy_; }
        [[nodiscard]] value_view values() const noexcept { return values_; }
        [[nodiscard]] equity_view equity() const noexcept { return equity_; }
        [[nodiscard]] category_view categories() const noexcept { return categories_; }

    private:
        node_record record_{};
        strategy_view strategy_;
        value_view values_;
        equity_view equity_;
        category_view categories_;
    };

    class result_store {
    public:
        result_store() = default;

        result_store(
            std::vector<node_record> nodes,
            std::vector<strategy_surface_record> strategy_surfaces,
            std::vector<strategy_surface_entry> strategy_entries,
            std::vector<combo_reach_entry> combo_reaches,
            std::vector<combo_value_entry> combo_values,
            std::vector<action_value_entry> action_values,
            std::vector<seat_value> seat_values,
            std::vector<float> equities,
            std::vector<hand_category_classification> categories)
            : nodes_(std::move(nodes)),
              strategy_surfaces_(std::move(strategy_surfaces)),
              strategy_entries_(std::move(strategy_entries)),
              combo_reaches_(std::move(combo_reaches)),
              combo_values_(std::move(combo_values)),
              action_values_(std::move(action_values)),
              seat_values_(std::move(seat_values)),
              equities_(std::move(equities)),
              categories_(std::move(categories))
        {
            validate_invariants();
        }

        [[nodiscard]] node_view node(uint32_t node_id) const;
        [[nodiscard]] strategy_view context_strategy(uint32_t strategy_context_id) const;
        [[nodiscard]] value_view node_values(uint32_t node_id) const;
        [[nodiscard]] equity_view node_equity(uint32_t node_id) const;
        [[nodiscard]] category_view node_categories(uint32_t node_id) const;
        [[nodiscard]] uint32_t node_strategy_context_id(uint32_t node_id) const;
        [[nodiscard]] std::size_t node_count() const noexcept { return nodes_.size(); }
        [[nodiscard]] std::size_t strategy_context_count() const noexcept { return strategy_surfaces_.size(); }

    private:
        void validate_invariants() const;

        std::vector<node_record> nodes_{};
        std::vector<strategy_surface_record> strategy_surfaces_{};
        std::vector<strategy_surface_entry> strategy_entries_{};
        std::vector<combo_reach_entry> combo_reaches_{};
        std::vector<combo_value_entry> combo_values_{};
        std::vector<action_value_entry> action_values_{};
        std::vector<seat_value> seat_values_{};
        std::vector<float> equities_{};
        std::vector<hand_category_classification> categories_{};
    };

    inline void result_store::validate_invariants() const
    {
        for (uint32_t node_id = 0; node_id < nodes_.size(); ++node_id) {
            const auto& record = nodes_[node_id];
            if (record.node_id != node_id) {
                throw std::invalid_argument{"result_store node ids must be dense and zero based"};
            }
            if (record.strategy_context_id >= strategy_surfaces_.size()) {
                throw std::invalid_argument{"result_store node references an invalid strategy context"};
            }
            if (record.combo_count == 0u || record.action_count == 0u || record.seat_value_count == 0u) {
                throw std::invalid_argument{"result_store node dimensions must be non-zero"};
            }
            if (static_cast<std::size_t>(record.combo_begin) + record.combo_count > combo_reaches_.size()
                || static_cast<std::size_t>(record.combo_begin) + record.combo_count > combo_values_.size()
                || static_cast<std::size_t>(record.combo_begin) + record.combo_count > equities_.size()
                || static_cast<std::size_t>(record.combo_begin) + record.combo_count > categories_.size()) {
                throw std::invalid_argument{"result_store combo-domain surfaces must share the node combo offset"};
            }
            if (static_cast<std::size_t>(record.action_val_begin) + record.combo_count * record.action_count > action_values_.size()) {
                throw std::invalid_argument{"result_store action-value surface is out of range"};
            }
            if (static_cast<std::size_t>(record.seat_value_begin) + record.seat_value_count > seat_values_.size()) {
                throw std::invalid_argument{"result_store seat-value surface is out of range"};
            }

            const auto& strategy = strategy_surfaces_[record.strategy_context_id];
            if (strategy.combo_count != record.combo_count || strategy.action_count != record.action_count) {
                throw std::invalid_argument{"result_store strategy surface shape must alias node combo/action shape"};
            }
            if (static_cast<std::size_t>(strategy.strategy_begin) + strategy.combo_count * strategy.action_count > strategy_entries_.size()) {
                throw std::invalid_argument{"result_store strategy surface is out of range"};
            }
        }
    }

    inline node_view result_store::node(uint32_t node_id) const
    {
        const auto& record = nodes_.at(node_id);
        const auto& strategy_record = strategy_surfaces_.at(record.strategy_context_id);
        const auto strategy = strategy_view{
            std::span<const strategy_surface_entry>{strategy_entries_}.subspan(strategy_record.strategy_begin, strategy_record.combo_count * strategy_record.action_count),
            strategy_record.combo_count,
            strategy_record.action_count
        };
        const auto values = value_view{
            std::span<const combo_reach_entry>{combo_reaches_}.subspan(record.combo_begin, record.combo_count),
            std::span<const combo_value_entry>{combo_values_}.subspan(record.combo_begin, record.combo_count),
            std::span<const action_value_entry>{action_values_}.subspan(record.action_val_begin, record.combo_count * record.action_count),
            seat_values_.at(record.seat_value_begin),
            record.action_count
        };
        const auto equity = equity_view{
            std::span<const float>{equities_}.subspan(record.combo_begin, record.combo_count)
        };
        const auto categories = category_view{
            std::span<const hand_category_classification>{categories_}.subspan(record.combo_begin, record.combo_count),
            std::span<const combo_reach_entry>{combo_reaches_}.subspan(record.combo_begin, record.combo_count),
            std::span<const combo_value_entry>{combo_values_}.subspan(record.combo_begin, record.combo_count),
            std::span<const strategy_surface_entry>{strategy_entries_}.subspan(strategy_record.strategy_begin, strategy_record.combo_count * strategy_record.action_count),
            record.action_count
        };
        return node_view{record, strategy, values, equity, categories};
    }

    inline strategy_view result_store::context_strategy(uint32_t strategy_context_id) const
    {
        const auto& record = strategy_surfaces_.at(strategy_context_id);
        return strategy_view{
            std::span<const strategy_surface_entry>{strategy_entries_}.subspan(record.strategy_begin, record.combo_count * record.action_count),
            record.combo_count,
            record.action_count
        };
    }

    inline value_view result_store::node_values(uint32_t node_id) const
    {
        const auto& record = nodes_.at(node_id);
        return value_view{
            std::span<const combo_reach_entry>{combo_reaches_}.subspan(record.combo_begin, record.combo_count),
            std::span<const combo_value_entry>{combo_values_}.subspan(record.combo_begin, record.combo_count),
            std::span<const action_value_entry>{action_values_}.subspan(record.action_val_begin, record.combo_count * record.action_count),
            seat_values_.at(record.seat_value_begin),
            record.action_count
        };
    }

    inline equity_view result_store::node_equity(uint32_t node_id) const
    {
        const auto& record = nodes_.at(node_id);
        return equity_view{std::span<const float>{equities_}.subspan(record.combo_begin, record.combo_count)};
    }

    inline category_view result_store::node_categories(uint32_t node_id) const
    {
        const auto& record = nodes_.at(node_id);
        const auto& strategy_record = strategy_surfaces_.at(record.strategy_context_id);
        return category_view{
            std::span<const hand_category_classification>{categories_}.subspan(record.combo_begin, record.combo_count),
            std::span<const combo_reach_entry>{combo_reaches_}.subspan(record.combo_begin, record.combo_count),
            std::span<const combo_value_entry>{combo_values_}.subspan(record.combo_begin, record.combo_count),
            std::span<const strategy_surface_entry>{strategy_entries_}.subspan(strategy_record.strategy_begin, strategy_record.combo_count * strategy_record.action_count),
            record.action_count
        };
    }

    inline uint32_t result_store::node_strategy_context_id(uint32_t node_id) const
    {
        return nodes_.at(node_id).strategy_context_id;
    }

}
