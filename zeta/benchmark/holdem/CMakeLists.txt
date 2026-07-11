project(zeta_benchmark_holdem LANGUAGES CXX)

set(_zeta_bench_holdem_sources
    ${CMAKE_CURRENT_LIST_DIR}/src/bench_probes.cpp
)

add_executable(zeta-bench-holdem-probes
    ${_zeta_bench_holdem_sources}
)

target_link_libraries(zeta-bench-holdem-probes
    PRIVATE zeta-holdem
)

if(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_link_libraries(zeta-bench-holdem-probes PRIVATE stdc++ m)
endif()
