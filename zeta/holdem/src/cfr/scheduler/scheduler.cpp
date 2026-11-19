#include "scheduler.h"

#include <algorithm>
#include <utility>

namespace zeta::holdem::cfr::scheduler {

    worker_pool::worker_pool(const uint32_t worker_count)
    {
        const auto helpers = worker_count > 1u ? worker_count - 1u : 0u;
        threads_.reserve(helpers);
        for (uint32_t worker_id = 1; worker_id <= helpers; ++worker_id) {
            threads_.emplace_back([this, worker_id] { worker_main(worker_id); });
        }
    }

    worker_pool::~worker_pool()
    {
        {
            const std::lock_guard lock{mutex_};
            stopping_ = true;
        }
        start_.notify_all();
        for (auto& thread : threads_) {
            thread.join();
        }
    }

    uint32_t worker_pool::worker_count() const noexcept
    {
        return static_cast<uint32_t>(threads_.size()) + 1u;
    }

    void worker_pool::set_stop_condition(stop_condition condition)
    {
        stop_condition_ = std::move(condition);
    }

    bool worker_pool::stopped() const noexcept
    {
        return stopped_.load(std::memory_order_acquire);
    }

    bool worker_pool::should_stop()
    {
        if (stopped_.load(std::memory_order_acquire)) {
            return true;
        }
        if (stop_condition_ && stop_condition_()) {
            stopped_.store(true, std::memory_order_release);
            return true;
        }
        return false;
    }

    void worker_pool::run(const uint64_t task_count, const task& body, const uint32_t chunk_size)
    {
        if (task_count == 0u || should_stop()) {
            return;
        }
        if (threads_.empty() || task_count == 1u) {
            for (uint64_t task_index = 0; task_index < task_count; ++task_index) {
                if (task_index > 0u && should_stop()) {
                    return;
                }
                body(0u, task_index);
            }
            return;
        }

        {
            const std::lock_guard lock{mutex_};
            body_ = &body;
            task_count_ = task_count;
            chunk_size_ = std::max<uint32_t>(chunk_size, 1u);
            next_task_.store(0, std::memory_order_relaxed);
            failed_.store(false, std::memory_order_relaxed);
            error_ = nullptr;
            busy_ = static_cast<uint32_t>(threads_.size());
            ++generation_;
        }
        start_.notify_all();
        drain(0u);

        std::exception_ptr error;
        {
            std::unique_lock lock{mutex_};
            done_.wait(lock, [this] { return busy_ == 0u; });
            body_ = nullptr;
            error = std::exchange(error_, nullptr);
        }
        if (error) {
            std::rethrow_exception(error);
        }
    }

    void worker_pool::worker_main(const uint32_t worker_id)
    {
        uint64_t seen_generation = 0;
        for (;;) {
            {
                std::unique_lock lock{mutex_};
                start_.wait(lock, [&] { return stopping_ || generation_ != seen_generation; });
                if (stopping_) {
                    return;
                }
                seen_generation = generation_;
            }
            drain(worker_id);
            {
                const std::lock_guard lock{mutex_};
                if (--busy_ == 0u) {
                    done_.notify_one();
                }
            }
        }
    }

    void worker_pool::drain(const uint32_t worker_id) noexcept
    {
        try {
            while (!failed_.load(std::memory_order_relaxed) && !should_stop()) {
                const auto begin = next_task_.fetch_add(chunk_size_, std::memory_order_relaxed);
                if (begin >= task_count_) {
                    return;
                }
                const auto end = std::min<uint64_t>(begin + chunk_size_, task_count_);
                for (auto task_index = begin; task_index < end; ++task_index) {
                    (*body_)(worker_id, task_index);
                }
            }
        } catch (...) {
            const std::lock_guard lock{mutex_};
            if (!error_) {
                error_ = std::current_exception();
            }
            failed_.store(true, std::memory_order_relaxed);
        }
    }

}
