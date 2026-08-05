add_executable(zeta_tests
    src/test_main.cpp
    src/test_repr.cpp
    src/test_holdem.cpp
    src/test_holdem_cli.cpp
    src/test_holdem_ui.cpp
    ../ui/holdem/src/solver_state.cpp
    ../ui/holdem/src/spot_document.cpp
    src/test_cfr_graph.cpp
)
target_include_directories(zeta_tests PRIVATE ${CMAKE_SOURCE_DIR}/zeta/ui/holdem/src)
target_link_libraries(zeta_tests PRIVATE zeta_core zeta-holdem Boost::unit_test_framework Boost::json)
target_compile_definitions(zeta_tests PRIVATE BOOST_TEST_DYN_LINK)
target_compile_features(zeta_tests PRIVATE cxx_std_23)

if(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_link_libraries(zeta_tests PRIVATE stdc++ m)
endif()

add_test(
    NAME zeta_tests
    COMMAND $<TARGET_FILE:zeta_tests>
)
set_tests_properties(zeta_tests PROPERTIES
    LABELS "core;holdem;cfr"
    WORKING_DIRECTORY $<TARGET_FILE_DIR:zeta_tests>
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

    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(BOOST_DLL_PATH "${BOOST_ROOT}/debug/bin")
    else()
        set(BOOST_DLL_PATH "${BOOST_ROOT}/bin")
    endif()
    message(STATUS "Looking for Boost DLLs in: ${BOOST_DLL_PATH}")
    file(GLOB BOOST_TEST_DLL "${BOOST_DLL_PATH}/boost_unit_test_framework*.dll")

    foreach(dll IN LISTS BOOST_TEST_DLL)
        add_custom_command(TARGET zeta_tests POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${dll}"
            $<TARGET_FILE_DIR:zeta_tests>
            COMMENT "Copying Boost.Test DLL to output directory"
        )
    endforeach()
endif()
