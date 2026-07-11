project(zero)

add_subdirectory(core)
add_subdirectory(holdem)

option(ZETA_BUILD_TOOLS "Build Zeta tool targets" ON)
if(ZETA_BUILD_TOOLS)
    add_subdirectory(tools)
endif()

option(ZETA_BUILD_BENCHMARKS "Build Zeta benchmark targets" ON)
if(ZETA_BUILD_BENCHMARKS)
    add_subdirectory(benchmark)
endif()

#add_subdirectory(core_py)
if(ZETA_BUILD_TESTS)
    add_subdirectory(test)
endif()
