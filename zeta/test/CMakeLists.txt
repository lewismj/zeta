add_executable(zeta_tests
    src/test_main.cpp
    src/test_repr.cpp
    src/test_holdem.cpp
    src/test_holdem_cli.cpp
    src/test_holdem_ui.cpp
    ../ui/holdem/src/app/app_settings.cpp
    ../ui/holdem/src/document/document_json.cpp
    ../ui/holdem/src/solver/solver_session.cpp
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
    src/test_cfr_graph.cpp
)
target_include_directories(zeta_tests PRIVATE ${CMAKE_SOURCE_DIR}/zeta/ui/holdem/src)
target_link_libraries(zeta_tests PRIVATE zeta_core zeta-holdem Boost::unit_test_framework Boost::json Qt6::Widgets)
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
    set(ZETA_TEST_QT_RUNTIME_BIN "${Qt6_DIR}/../../bin")
    cmake_path(NORMAL_PATH ZETA_TEST_QT_RUNTIME_BIN)

    set(ZETA_TEST_QT_RUNTIME_DLLS
        brotlicommon.dll
        brotlidec.dll
        bz2.dll
        double-conversion.dll
        freetype.dll
        harfbuzz.dll
        icudt78.dll
        icuin78.dll
        icuuc78.dll
        libpng16.dll
        md4c.dll
        pcre2-16.dll
        Qt6Core.dll
        Qt6Gui.dll
        Qt6Widgets.dll
        zlib1.dll
        zstd.dll
    )

    set(ZETA_TEST_QT_RUNTIME_DLL_PATHS "")
    foreach(_zeta_test_runtime_dll IN LISTS ZETA_TEST_QT_RUNTIME_DLLS)
        set(_zeta_test_runtime_dll_path "${ZETA_TEST_QT_RUNTIME_BIN}/${_zeta_test_runtime_dll}")
        if(NOT EXISTS "${_zeta_test_runtime_dll_path}")
            message(FATAL_ERROR "Required test runtime dependency not found: ${_zeta_test_runtime_dll_path}")
        endif()
        list(APPEND ZETA_TEST_QT_RUNTIME_DLL_PATHS "${_zeta_test_runtime_dll_path}")
    endforeach()

    find_file(ZETA_QOFFSCREEN_PLUGIN
        NAMES qoffscreen.dll
        HINTS
            "${Qt6_DIR}/../../Qt6/plugins/platforms"
            "${Qt6_DIR}/../../../Qt6/plugins/platforms"
        REQUIRED
    )

    add_custom_command(TARGET zeta_tests POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $<TARGET_RUNTIME_DLLS:zeta_tests>
            ${ZETA_TEST_QT_RUNTIME_DLL_PATHS}
            $<TARGET_FILE_DIR:zeta_tests>
        COMMAND ${CMAKE_COMMAND} -E make_directory
            $<TARGET_FILE_DIR:zeta_tests>/platforms
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${ZETA_QOFFSCREEN_PLUGIN}"
            $<TARGET_FILE_DIR:zeta_tests>/platforms
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
