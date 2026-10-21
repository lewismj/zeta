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
     */
    class worker_pool {
    public:
        using task = std::function<void(uint32_t worker_id, uint64_t task_index)>;

        explicit worker_pool(uint32_t worker_count);
        ~worker_pool();

        worker_pool(const worker_pool&) = delete;
        worker_pool& operator=(const worker_pool&) = delete;

        [[nodiscard]] uint32_t worker_count() const noexcept;

        /** Runs body(worker_id, i) for every i in [0, task_count) and returns when all have finished. */
        void run(uint64_t task_count, const task& body, uint32_t chunk_size = 1);

    private:
        void worker_main(uint32_t worker_id);
        void drain(uint32_t worker_id) noexcept;

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
    };

}
