project(zeta_benchmark_holdem LANGUAGES CXX)

if(NOT TARGET benchmark::benchmark)
    message(FATAL_ERROR "Google Benchmark 1.9.5 is required but benchmark::benchmark was not provided.")
endif()

add_executable(zeta-bench-holdem-evaluator
    src/evaluator_benchmark.cpp
)

add_executable(zeta-bench-holdem-range-parser
    src/range_parser_benchmark.cpp
)

add_executable(zeta-bench-holdem-terminal
    src/terminal_benchmark.cpp
)

add_executable(zeta-bench-holdem-cfr
    src/cfr_benchmark.cpp
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

target_link_libraries(zeta-bench-holdem-terminal
    PRIVATE
    zeta-holdem
    benchmark::benchmark
)

target_link_libraries(zeta-bench-holdem-cfr
    PRIVATE
    zeta-holdem
    benchmark::benchmark
)

if(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_link_libraries(zeta-bench-holdem-evaluator PRIVATE stdc++ m)
    target_link_libraries(zeta-bench-holdem-range-parser PRIVATE stdc++ m)
    target_link_libraries(zeta-bench-holdem-terminal PRIVATE stdc++ m)
    target_link_libraries(zeta-bench-holdem-cfr PRIVATE stdc++ m)
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

add_custom_target(zeta-bench-holdem-terminal-json
    COMMAND $<TARGET_FILE:zeta-bench-holdem-terminal>
        --benchmark_out=${CMAKE_CURRENT_BINARY_DIR}/holdem-terminal-benchmark.json
        --benchmark_out_format=json
    DEPENDS zeta-bench-holdem-terminal
    USES_TERMINAL
    VERBATIM
    COMMENT "Running Google Benchmark Hold'em terminal suite and writing JSON baseline"
)

add_custom_target(zeta-bench-holdem-cfr-json
    COMMAND $<TARGET_FILE:zeta-bench-holdem-cfr>
        --benchmark_out=${CMAKE_CURRENT_BINARY_DIR}/holdem-cfr-benchmark.json
        --benchmark_out_format=json
    DEPENDS zeta-bench-holdem-cfr
    USES_TERMINAL
    VERBATIM
    COMMENT "Running Google Benchmark Hold'em CFR suite and writing JSON baseline"
)

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    find_program(PERF_EXECUTABLE perf)
    if(PERF_EXECUTABLE)
        add_custom_target(zeta-bench-holdem-cfr-perf
            COMMAND ${PERF_EXECUTABLE} stat -e cache-misses,LLC-load-misses,cycles,instructions
                $<TARGET_FILE:zeta-bench-holdem-cfr>
                --benchmark_filter=BM_CFRIteration
            DEPENDS zeta-bench-holdem-cfr
            USES_TERMINAL
            VERBATIM
            COMMENT "Profiling Hold'em CFR iteration benchmarks with perf stat"
        )
    endif()
endif()
