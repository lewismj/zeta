project(zeta_holdem LANGUAGES CXX)

add_library(zeta-holdem STATIC
    src/board.h
    src/eval.h
    src/evaluator.h
    src/range.h
    src/range_parser.h
    src/tables.h
    src/terminal.h
    src/tables.generated.cpp
)
add_library(zeta_holdem ALIAS zeta-holdem)

target_include_directories(zeta-holdem
    PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/src
)

target_link_libraries(zeta-holdem
    PUBLIC zeta-core
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