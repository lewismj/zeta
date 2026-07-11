project(zeta_benchmark_holdem LANGUAGES CXX)

add_executable(zeta-bench-holdem-probes
    src/bench_probes.cpp
)

target_link_libraries(zeta-bench-holdem-probes
    PRIVATE zeta-holdem
)

if(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_link_libraries(zeta-bench-holdem-probes PRIVATE stdc++ m)
endif()

set(ZETA_BENCH_HOLDEM_MAX_RANDOM_NS "80" CACHE STRING "Maximum random-corpus ns/eval for zeta-bench-holdem-regression")
set(ZETA_BENCH_HOLDEM_MAX_ADVERSARIAL_NS "80" CACHE STRING "Maximum adversarial-corpus ns/eval for zeta-bench-holdem-regression")
set(ZETA_BENCH_HOLDEM_MAX_INDEX_NS "60" CACHE STRING "Maximum isolated quinary-index ns/op for zeta-bench-holdem-regression")

add_custom_target(zeta-bench-holdem-regression
    COMMAND $<TARGET_FILE:zeta-bench-holdem-probes>
        --quick
        --max-random-ns ${ZETA_BENCH_HOLDEM_MAX_RANDOM_NS}
        --max-adversarial-ns ${ZETA_BENCH_HOLDEM_MAX_ADVERSARIAL_NS}
        --max-index-ns ${ZETA_BENCH_HOLDEM_MAX_INDEX_NS}
    DEPENDS zeta-bench-holdem-probes
    USES_TERMINAL
    VERBATIM
    COMMENT "Running Hold'em evaluator benchmark regression smoke check"
)

find_package(benchmark 1.9.5 CONFIG QUIET)
if(NOT benchmark_FOUND)
    include(FetchContent)
    set(BENCHMARK_ENABLE_TESTING OFF CACHE BOOL "" FORCE)
    set(BENCHMARK_ENABLE_GTEST_TESTS OFF CACHE BOOL "" FORCE)
    set(BENCHMARK_ENABLE_INSTALL OFF CACHE BOOL "" FORCE)
    if(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
        set(HAVE_STD_REGEX TRUE CACHE BOOL "" FORCE)
        set(HAVE_STEADY_CLOCK TRUE CACHE BOOL "" FORCE)
    endif()
    FetchContent_Declare(
        google_benchmark
        GIT_REPOSITORY https://github.com/google/benchmark.git
        GIT_TAG v1.9.5
        GIT_SHALLOW TRUE
    )
    FetchContent_MakeAvailable(google_benchmark)
endif()

if(NOT TARGET benchmark::benchmark)
    message(FATAL_ERROR "Google Benchmark 1.9.5 is required but benchmark::benchmark was not provided.")
endif()

add_executable(zeta-bench-holdem-evaluator
    src/evaluator_benchmark.cpp
)

add_executable(zeta-bench-holdem-range-parser
    src/range_parser_benchmark.cpp
)

target_link_libraries(zeta-bench-holdem-evaluator
    PRIVATE
    zeta-holdem
    benchmark::benchmark
)

target_link_libraries(zeta-bench-holdem-range-parser
    PRIVATE
    zeta-holdem
    benchmark::benchmark
)

if(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_link_libraries(zeta-bench-holdem-evaluator PRIVATE stdc++ m)
    target_link_libraries(zeta-bench-holdem-range-parser PRIVATE stdc++ m)
endif()

add_custom_target(zeta-bench-holdem-evaluator-json
    COMMAND $<TARGET_FILE:zeta-bench-holdem-evaluator>
        --benchmark_out=${CMAKE_CURRENT_BINARY_DIR}/holdem-evaluator-benchmark.json
        --benchmark_out_format=json
    DEPENDS zeta-bench-holdem-evaluator
    USES_TERMINAL
    VERBATIM
    COMMENT "Running Google Benchmark Hold'em evaluator suite and writing JSON baseline"
)

add_custom_target(zeta-bench-holdem-range-parser-json
    COMMAND $<TARGET_FILE:zeta-bench-holdem-range-parser>
        --benchmark_out=${CMAKE_CURRENT_BINARY_DIR}/holdem-range-parser-benchmark.json
        --benchmark_out_format=json
    DEPENDS zeta-bench-holdem-range-parser
    USES_TERMINAL
    VERBATIM
    COMMENT "Running Google Benchmark Hold'em range parser suite and writing JSON baseline"
)
