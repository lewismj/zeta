project(zeta_tools_holdem LANGUAGES CXX)

add_executable(zeta-gen-holdem-tables
    src/gen_tables.cpp
    ../../holdem/src/tables.cpp
)

target_include_directories(zeta-gen-holdem-tables PRIVATE
    ${CMAKE_SOURCE_DIR}/zeta/holdem/src
    ${CMAKE_SOURCE_DIR}/zeta/core/include
    ${CMAKE_SOURCE_DIR}/zeta/core/src
)

target_compile_definitions(zeta-gen-holdem-tables PRIVATE
    ZETA_HOLDEM_TABLES_OUTPUT="${CMAKE_SOURCE_DIR}/zeta/holdem/src/tables.generated.cpp"
    ZETA_HOLDEM_ENABLE_RANK_TABLE
)

if(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_link_libraries(zeta-gen-holdem-tables PRIVATE stdc++ m)
endif()
