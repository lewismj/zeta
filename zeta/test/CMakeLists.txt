set(CMAKE_AUTORCC ON)

set(ZETA_TEST_SOURCES
    src/test_repr.cpp
    src/test_holdem.cpp
    src/test_holdem_cli.cpp
    src/test_holdem_ui.cpp
    src/test_cfr_graph.cpp
)

add_executable(zeta_tests
    src/test_main.cpp
    ${ZETA_TEST_SOURCES}
    ../ui/holdem/src/app/app_settings.cpp
    ../ui/holdem/src/document/document_json.cpp
    ../ui/holdem/src/main_window.cpp
    ../ui/holdem/src/solver/solution_store.cpp
    ../ui/holdem/src/solver/solver_session.cpp
    ../ui/holdem/src/study/study_workflow.cpp
    ../ui/holdem/src/solver_state.cpp
    ../ui/holdem/src/spot_document.cpp
    ../ui/holdem/src/theme/theme_registry.cpp
    ../ui/holdem/src/theme/theme_styles.cpp
    ../ui/holdem/src/viewmodels/range_view_model.cpp
    ../ui/holdem/src/viewmodels/spot_view_model.cpp
    ../ui/holdem/src/viewmodels/strategy_view_model.cpp
    ../ui/holdem/src/widgets/range_editor.cpp
    ../ui/holdem/src/widgets/spot_builder.cpp
    ../ui/holdem/src/widgets/strategy_explorer.cpp
    ../ui/holdem/src/widgets/table_state_view.cpp
    ../ui/holdem/resources/holdem.qrc
)
target_include_directories(zeta_tests PRIVATE ${CMAKE_SOURCE_DIR}/zeta/ui/holdem/src)
target_link_libraries(zeta_tests PRIVATE zeta_core zeta-holdem Boost::unit_test_framework Boost::json Qt6::Widgets)
if(TARGET Qt6::Svg)
    target_link_libraries(zeta_tests PRIVATE Qt6::Svg)
endif()
target_compile_definitions(zeta_tests PRIVATE BOOST_TEST_DYN_LINK)
target_compile_features(zeta_tests PRIVATE cxx_std_23)

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
    target_link_libraries(zeta_tests PRIVATE dwmapi)

    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(ZETA_TEST_QT_RUNTIME_BIN "${Qt6_DIR}/../../debug/bin")
        set(ZETA_QOFFSCREEN_PLUGIN_NAMES qoffscreend.dll qoffscreen.dll)
        set(ZETA_TEST_QSVGICON_PLUGIN_NAMES qsvgicond.dll qsvgicon.dll)
        set(ZETA_TEST_QSVG_PLUGIN_NAMES qsvgd.dll qsvg.dll)
        set(ZETA_TEST_QT_PLUGIN_HINT_ROOTS
            "${Qt6_DIR}/../../debug/Qt6/plugins"
            "${Qt6_DIR}/../../../debug/Qt6/plugins"
        )
    else()
        set(ZETA_TEST_QT_RUNTIME_BIN "${Qt6_DIR}/../../bin")
        set(ZETA_QOFFSCREEN_PLUGIN_NAMES qoffscreen.dll qoffscreend.dll)
        set(ZETA_TEST_QSVGICON_PLUGIN_NAMES qsvgicon.dll qsvgicond.dll)
        set(ZETA_TEST_QSVG_PLUGIN_NAMES qsvg.dll qsvgd.dll)
        set(ZETA_TEST_QT_PLUGIN_HINT_ROOTS
            "${Qt6_DIR}/../../Qt6/plugins"
            "${Qt6_DIR}/../../../Qt6/plugins"
        )
    endif()
    cmake_path(NORMAL_PATH ZETA_TEST_QT_RUNTIME_BIN)
    foreach(_zeta_test_qt_plugin_hint_root IN LISTS ZETA_TEST_QT_PLUGIN_HINT_ROOTS)
        cmake_path(NORMAL_PATH _zeta_test_qt_plugin_hint_root)
        list(APPEND ZETA_TEST_QT_PLATFORM_PLUGIN_HINTS "${_zeta_test_qt_plugin_hint_root}/platforms")
        list(APPEND ZETA_TEST_QT_ICON_PLUGIN_HINTS "${_zeta_test_qt_plugin_hint_root}/iconengines")
        list(APPEND ZETA_TEST_QT_IMAGE_PLUGIN_HINTS "${_zeta_test_qt_plugin_hint_root}/imageformats")
    endforeach()

    file(GLOB ZETA_TEST_QT_RUNTIME_DLL_PATHS "${ZETA_TEST_QT_RUNTIME_BIN}/*.dll")
    if(NOT ZETA_TEST_QT_RUNTIME_DLL_PATHS)
        message(FATAL_ERROR "No Qt test runtime dependencies found in: ${ZETA_TEST_QT_RUNTIME_BIN}")
    endif()

    unset(ZETA_QOFFSCREEN_PLUGIN CACHE)
    unset(ZETA_TEST_QSVGICON_PLUGIN CACHE)
    unset(ZETA_TEST_QSVG_PLUGIN CACHE)
    find_file(ZETA_QOFFSCREEN_PLUGIN
        NAMES ${ZETA_QOFFSCREEN_PLUGIN_NAMES}
        HINTS ${ZETA_TEST_QT_PLATFORM_PLUGIN_HINTS}
        REQUIRED
    )
    find_file(ZETA_TEST_QSVGICON_PLUGIN
        NAMES ${ZETA_TEST_QSVGICON_PLUGIN_NAMES}
        HINTS ${ZETA_TEST_QT_ICON_PLUGIN_HINTS}
        REQUIRED
    )
    find_file(ZETA_TEST_QSVG_PLUGIN
        NAMES ${ZETA_TEST_QSVG_PLUGIN_NAMES}
        HINTS ${ZETA_TEST_QT_IMAGE_PLUGIN_HINTS}
        REQUIRED
    )

    add_custom_command(TARGET zeta_tests POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $<TARGET_RUNTIME_DLLS:zeta_tests>
            ${ZETA_TEST_QT_RUNTIME_DLL_PATHS}
            $<TARGET_FILE_DIR:zeta_tests>
        COMMAND ${CMAKE_COMMAND} -E make_directory
            $<TARGET_FILE_DIR:zeta_tests>/platforms
        COMMAND ${CMAKE_COMMAND} -E make_directory
            $<TARGET_FILE_DIR:zeta_tests>/iconengines
        COMMAND ${CMAKE_COMMAND} -E make_directory
            $<TARGET_FILE_DIR:zeta_tests>/imageformats
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${ZETA_QOFFSCREEN_PLUGIN}"
            $<TARGET_FILE_DIR:zeta_tests>/platforms
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${ZETA_TEST_QSVGICON_PLUGIN}"
            $<TARGET_FILE_DIR:zeta_tests>/iconengines
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${ZETA_TEST_QSVG_PLUGIN}"
            $<TARGET_FILE_DIR:zeta_tests>/imageformats
        COMMAND_EXPAND_LISTS
        COMMENT "Copying Qt runtime dependencies for zeta_tests"
    )

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
    file(GLOB BOOST_RUNTIME_DLLS "${BOOST_DLL_PATH}/boost_*.dll")

    if(BOOST_RUNTIME_DLLS)
        add_custom_command(TARGET zeta_tests POST_BUILD
            COMMAND ${CMAKE_COMMAND} -E copy_if_different
                ${BOOST_RUNTIME_DLLS}
                $<TARGET_FILE_DIR:zeta_tests>
            COMMAND_EXPAND_LISTS
            COMMENT "Copying Boost runtime DLLs to zeta_tests output directory"
        )
    endif()
endif()
