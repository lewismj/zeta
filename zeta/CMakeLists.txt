project(zero)

add_subdirectory(core)
add_subdirectory(holdem)

if(ZETA_BUILD_TOOLS)
    add_subdirectory(tools)
endif()

add_subdirectory(ui)

if(ZETA_BUILD_BENCHMARKS)
    add_subdirectory(benchmark)
endif()

#add_subdirectory(core_py)
if(ZETA_BUILD_TESTS)
    add_subdirectory(test)
endif()
