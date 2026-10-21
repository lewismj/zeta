project(zeta_holdem LANGUAGES CXX)

add_library(zeta-holdem STATIC
    src/board.h
    src/range.h
    src/range_parser.h
    src/tables.h
    src/tables.generated.cpp
    src/eval/eval.h
    src/eval/evaluator.h
    src/terminal/terminal.h
    src/terminal/terminal_types.h
    src/terminal/engine.h
    src/terminal/fold.h
    src/terminal/reach_index.h
    src/terminal/showdown.h
    src/cfr/chance/chance.h
    src/cfr/betting/betting.h
    src/cfr/graph/graph.h
    src/cfr/graph/graph.cpp
    src/cfr/graph/builder.h
    src/cfr/graph/builder.cpp
    src/cfr/graph/validation.h
    src/cfr/graph/validation.cpp
    src/cfr/tables/table_layout.h
    src/cfr/solver/metadata.h
    src/cfr/scheduler/scheduler.h
    src/cfr/scheduler/scheduler.cpp
    src/cli/solve_cli.h
    src/cli/solve_cli.cpp
)
add_library(zeta_holdem ALIAS zeta-holdem)

target_include_directories(zeta-holdem
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/src
)

find_package(Threads REQUIRED)

target_link_libraries(zeta-holdem
    PUBLIC zeta-core Boost::json Threads::Threads
)

if(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_link_libraries(zeta-holdem PUBLIC stdc++)
endif()

if (NOT MSVC)
    target_compile_options(zeta-holdem PRIVATE -Wswitch-enum)
endif()

install(TARGETS zeta-holdem
        LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
        PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR})
