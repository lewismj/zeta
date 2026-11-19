#pragma once

#include <atomic>
#include <condition_variable>
#include <cstdint>
#include <exception>
#include <functional>
#include <mutex>
#include <thread>
#include <vector>

namespace zeta::holdem::cfr::scheduler {

    /**
     * Fixed set of worker threads that run batches of independent tasks.
     *
     * run() hands out task indices in chunks from a shared counter, so tasks of
     * uneven cost balance across workers. The calling thread works too, so a pool of
     * N workers starts N - 1 threads, and a pool of one runs everything inline. Each
     * task receives its worker id (0 .. worker_count() - 1) to index per-worker
     * scratch; tasks must not otherwise share mutable state. The first exception a
     * task throws stops the batch and is rethrown from run() once every worker has
     * finished.
     *
     * An optional stop condition is checked before each chunk. Once it returns true the
     * pool stops handing out tasks, the batch returns with some tasks not run, and
     * stopped() stays true: every later run() returns at once. A caller that sets a
     * stop condition must check stopped() before using a batch's results.
     */
    class worker_pool {
    public:
        using task = std::function<void(uint32_t worker_id, uint64_t task_index)>;
        using stop_condition = std::function<bool()>;

        explicit worker_pool(uint32_t worker_count);
        ~worker_pool();

        worker_pool(const worker_pool&) = delete;
        worker_pool& operator=(const worker_pool&) = delete;

        [[nodiscard]] uint32_t worker_count() const noexcept;

        /** Sets the stop condition. It is called from every worker thread, so it must be thread-safe. */
        void set_stop_condition(stop_condition condition);

        /** Whether the stop condition has fired. Once true, it stays true. */
        [[nodiscard]] bool stopped() const noexcept;

        /**
         * Runs body(worker_id, i) for every i in [0, task_count) and returns when all
         * have finished, or when the stop condition fires.
         */
        void run(uint64_t task_count, const task& body, uint32_t chunk_size = 1);

    private:
        void worker_main(uint32_t worker_id);
        void drain(uint32_t worker_id) noexcept;
        [[nodiscard]] bool should_stop();

        std::vector<std::thread> threads_;
        std::mutex mutex_;
        std::condition_variable start_;
        std::condition_variable done_;
        const task* body_ = nullptr;
        uint64_t task_count_ = 0;
        uint32_t chunk_size_ = 1;
        uint64_t generation_ = 0;
        uint32_t busy_ = 0;
        bool stopping_ = false;
        std::exception_ptr error_;
        std::atomic<uint64_t> next_task_{0};
        std::atomic<bool> failed_{false};
        stop_condition stop_condition_;
        std::atomic<bool> stopped_{false};
    };

}
