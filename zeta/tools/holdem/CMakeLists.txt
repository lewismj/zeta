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
)

set(_zeta_holdem_generated_tables "${CMAKE_SOURCE_DIR}/zeta/holdem/src/tables.generated.cpp")
set(_zeta_holdem_generated_tables_check "${CMAKE_CURRENT_BINARY_DIR}/tables.generated.check.cpp")

add_custom_target(zeta-generate-holdem-tables
    COMMAND zeta-gen-holdem-tables "${_zeta_holdem_generated_tables}"
    DEPENDS zeta-gen-holdem-tables
    USES_TERMINAL
    COMMENT "Regenerating Hold'em lookup tables"
)

add_custom_target(zeta-check-holdem-tables
    COMMAND ${CMAKE_COMMAND} -E make_directory "${CMAKE_CURRENT_BINARY_DIR}"
    COMMAND zeta-gen-holdem-tables "${_zeta_holdem_generated_tables_check}"
    COMMAND ${CMAKE_COMMAND} -E compare_files "${_zeta_holdem_generated_tables}" "${_zeta_holdem_generated_tables_check}"
    DEPENDS zeta-gen-holdem-tables
    USES_TERMINAL
    COMMENT "Checking generated Hold'em lookup tables are up to date"
)

if(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_link_libraries(zeta-gen-holdem-tables PRIVATE stdc++ m)
endif()
