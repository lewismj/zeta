set(CMAKE_AUTORCC ON)

set(ZETA_TEST_SOURCES
    src/test_repr.cpp
    src/test_holdem.cpp
    src/test_holdem_cli.cpp
    src/test_holdem_locks.cpp
    src/test_cfr_graph.cpp
)
if(ZETA_BUILD_UI)
    list(APPEND ZETA_TEST_SOURCES
        src/test_holdem_ui.cpp
        src/test_holdem_ui_locks.cpp
    )
endif()

add_executable(zeta_tests
    src/test_main.cpp
    ${ZETA_TEST_SOURCES}
)
target_link_libraries(zeta_tests PRIVATE zeta_core zeta-holdem Boost::unit_test_framework Boost::json)
if(ZETA_BUILD_UI)
    # The UI tests link the app's own UI library; only the resources are added here.
    target_sources(zeta_tests PRIVATE ../ui/holdem/resources/holdem.qrc)
    target_link_libraries(zeta_tests PRIVATE zeta-ui-holdem-lib)
endif()
target_compile_definitions(zeta_tests PRIVATE BOOST_TEST_DYN_LINK)
target_compile_features(zeta_tests PRIVATE cxx_std_23)

if(MSVC)
    target_link_options(zeta_tests PRIVATE "$<$<CONFIG:Debug>:/STACK:8388608>")
endif()

if(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_link_libraries(zeta_tests PRIVATE stdc++ m)
endif()

function(zeta_register_boost_test_cases target)
    set(_options)
    set(_one_value_args)
    set(_multi_value_args SOURCES LABELS)
    cmake_parse_arguments(ZETA_BOOST_TEST "${_options}" "${_one_value_args}" "${_multi_value_args}" ${ARGN})

    set(_registered_tests)
    foreach(_source IN LISTS ZETA_BOOST_TEST_SOURCES)
        set(_suite_stack)
        file(STRINGS "${CMAKE_CURRENT_SOURCE_DIR}/${_source}" _source_lines
            REGEX "BOOST_(AUTO_TEST_SUITE|AUTO_TEST_SUITE_END|AUTO_TEST_CASE|FIXTURE_TEST_CASE|DATA_TEST_CASE)"
        )

        foreach(_line IN LISTS _source_lines)
            if(_line MATCHES "BOOST_AUTO_TEST_SUITE[ \t]*\\([ \t]*([A-Za-z_][A-Za-z0-9_]*)")
                list(APPEND _suite_stack "${CMAKE_MATCH_1}")
            elseif(_line MATCHES "BOOST_AUTO_TEST_SUITE_END")
                list(LENGTH _suite_stack _suite_depth)
                if(_suite_depth GREATER 0)
                    list(POP_BACK _suite_stack)
                endif()
            elseif(_line MATCHES "BOOST_(AUTO|FIXTURE|DATA)_TEST_CASE[ \t]*\\([ \t]*([A-Za-z_][A-Za-z0-9_]*)")
                set(_test_case "${CMAKE_MATCH_2}")
                if(_suite_stack)
                    string(JOIN "/" _suite_path ${_suite_stack})
                    set(_boost_test_filter "${_suite_path}/${_test_case}")
                else()
                    set(_boost_test_filter "${_test_case}")
                endif()

                string(REPLACE "/" "." _ctest_test_name "${_boost_test_filter}")
                set(_ctest_test_name "${target}.${_ctest_test_name}")
                add_test(
                    NAME "${_ctest_test_name}"
                    COMMAND $<TARGET_FILE:${target}> "--run_test=${_boost_test_filter}"
                )
                set_tests_properties("${_ctest_test_name}" PROPERTIES
                    LABELS "${ZETA_BOOST_TEST_LABELS}"
                    WORKING_DIRECTORY $<TARGET_FILE_DIR:${target}>
                )
                list(APPEND _registered_tests "${_ctest_test_name}")
            endif()
        endforeach()
    endforeach()

    if(NOT _registered_tests)
        message(FATAL_ERROR "No Boost.Test cases were registered for ${target}")
    endif()
endfunction()

zeta_register_boost_test_cases(zeta_tests
    SOURCES ${ZETA_TEST_SOURCES}
    LABELS "core;holdem;cfr"
)

add_custom_target(zeta_test
    COMMAND ${CMAKE_CTEST_COMMAND} -C $<CONFIG> --output-on-failure --test-dir ${CMAKE_CURRENT_BINARY_DIR}
    DEPENDS zeta_tests
    USES_TERMINAL
    COMMENT "Running all tests in zeta/test"
)

if(TARGET zeta-check-holdem-tables)
    add_dependencies(zeta_test zeta-check-holdem-tables)
endif()

if(WIN32)
    if(NOT DEFINED BOOST_ROOT)
        if(DEFINED ENV{BOOST_ROOT})
            set(BOOST_ROOT "$ENV{BOOST_ROOT}")
        elseif(Boost_INCLUDE_DIRS)
            list(GET Boost_INCLUDE_DIRS 0 _boost_inc)
            get_filename_component(BOOST_ROOT "${_boost_inc}" DIRECTORY)
        elseif(DEFINED Boost_DIR AND Boost_DIR)
            get_filename_component(_zeta_boost_share "${Boost_DIR}" DIRECTORY)
            get_filename_component(BOOST_ROOT "${_zeta_boost_share}" DIRECTORY)
        else()
            message(FATAL_ERROR "BOOST_ROOT not set.")
        endif()
    endif()

    if(ZETA_BUILD_UI)
        zeta_deploy_runtime(zeta_tests QT_PLATFORM qoffscreen BOOST_ROOT "${BOOST_ROOT}")
    else()
        zeta_deploy_runtime(zeta_tests BOOST_ROOT "${BOOST_ROOT}")
    endif()
endif()

target_link_libraries(zeta_tests PRIVATE zeta_warnings)
