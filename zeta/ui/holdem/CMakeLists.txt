project(zeta_ui_holdem LANGUAGES CXX)

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

add_executable(zeta-ui-holdem
    src/main.cpp
    src/main_window.h
    src/main_window.cpp
    src/app/app_settings.h
    src/app/app_settings.cpp
    src/document/document_json.h
    src/document/document_json.cpp
    src/solver/solution_store.h
    src/solver/solution_store.cpp
    src/solver/solver_session.h
    src/solver/solver_session.cpp
    src/study/study_workflow.h
    src/study/study_workflow.cpp
    src/solver_state.h
    src/solver_state.cpp
    src/spot_document.h
    src/spot_document.cpp
    src/theme/theme.h
    src/theme/theme_registry.h
    src/theme/theme_registry.cpp
    src/theme/theme_styles.h
    src/theme/theme_styles.cpp
    src/viewmodels/range_view_model.h
    src/viewmodels/range_view_model.cpp
    src/viewmodels/spot_view_model.h
    src/viewmodels/spot_view_model.cpp
    src/viewmodels/strategy_view_model.h
    src/viewmodels/strategy_view_model.cpp
    src/widgets/range_editor.h
    src/widgets/range_editor.cpp
    src/widgets/spot_builder.h
    src/widgets/spot_builder.cpp
    src/widgets/strategy_explorer.h
    src/widgets/strategy_explorer.cpp
    src/widgets/table_state_view.h
    src/widgets/table_state_view.cpp
    resources/holdem.qrc
)

target_link_libraries(zeta-ui-holdem
    PRIVATE
    zeta-holdem
    Boost::json
    Qt6::Widgets
)

target_include_directories(zeta-ui-holdem PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/src)
target_compile_features(zeta-ui-holdem PRIVATE cxx_std_23)

add_custom_command(TARGET zeta-ui-holdem POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_directory
        "${CMAKE_CURRENT_SOURCE_DIR}/resources/styles"
        $<TARGET_FILE_DIR:zeta-ui-holdem>/styles
    COMMENT "Deploying editable zeta-ui-holdem styles"
    VERBATIM
)

if(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_link_libraries(zeta-ui-holdem PRIVATE m)
endif()

if(WIN32)
    target_link_libraries(zeta-ui-holdem PRIVATE dwmapi)

    if(CMAKE_BUILD_TYPE STREQUAL "Debug")
        set(ZETA_QT_RUNTIME_BIN "${Qt6_DIR}/../../debug/bin")
        set(ZETA_QWINDOWS_PLUGIN_NAMES qwindowsd.dll qwindows.dll)
        set(ZETA_QSVGICON_PLUGIN_NAMES qsvgicond.dll qsvgicon.dll)
        set(ZETA_QSVG_PLUGIN_NAMES qsvgd.dll qsvg.dll)
        set(ZETA_QT_PLUGIN_HINT_ROOTS
            "${Qt6_DIR}/../../debug/Qt6/plugins"
            "${Qt6_DIR}/../../../debug/Qt6/plugins"
        )
    else()
        set(ZETA_QT_RUNTIME_BIN "${Qt6_DIR}/../../bin")
        set(ZETA_QWINDOWS_PLUGIN_NAMES qwindows.dll qwindowsd.dll)
        set(ZETA_QSVGICON_PLUGIN_NAMES qsvgicon.dll qsvgicond.dll)
        set(ZETA_QSVG_PLUGIN_NAMES qsvg.dll qsvgd.dll)
        set(ZETA_QT_PLUGIN_HINT_ROOTS
            "${Qt6_DIR}/../../Qt6/plugins"
            "${Qt6_DIR}/../../../Qt6/plugins"
        )
    endif()
    cmake_path(NORMAL_PATH ZETA_QT_RUNTIME_BIN)
    foreach(_zeta_qt_plugin_hint_root IN LISTS ZETA_QT_PLUGIN_HINT_ROOTS)
        cmake_path(NORMAL_PATH _zeta_qt_plugin_hint_root)
        list(APPEND ZETA_QT_PLATFORM_PLUGIN_HINTS "${_zeta_qt_plugin_hint_root}/platforms")
        list(APPEND ZETA_QT_ICON_PLUGIN_HINTS "${_zeta_qt_plugin_hint_root}/iconengines")
        list(APPEND ZETA_QT_IMAGE_PLUGIN_HINTS "${_zeta_qt_plugin_hint_root}/imageformats")
    endforeach()

    unset(ZETA_QWINDOWS_PLUGIN CACHE)
    unset(ZETA_QSVGICON_PLUGIN CACHE)
    unset(ZETA_QSVG_PLUGIN CACHE)
    find_file(ZETA_QWINDOWS_PLUGIN
        NAMES ${ZETA_QWINDOWS_PLUGIN_NAMES}
        HINTS ${ZETA_QT_PLATFORM_PLUGIN_HINTS}
        REQUIRED
    )
    find_file(ZETA_QSVGICON_PLUGIN
        NAMES ${ZETA_QSVGICON_PLUGIN_NAMES}
        HINTS ${ZETA_QT_ICON_PLUGIN_HINTS}
        REQUIRED
    )
    find_file(ZETA_QSVG_PLUGIN
        NAMES ${ZETA_QSVG_PLUGIN_NAMES}
        HINTS ${ZETA_QT_IMAGE_PLUGIN_HINTS}
        REQUIRED
    )

    file(GLOB ZETA_HOLDEM_RUNTIME_DLL_PATHS "${ZETA_QT_RUNTIME_BIN}/*.dll")
    if(NOT ZETA_HOLDEM_RUNTIME_DLL_PATHS)
        message(FATAL_ERROR "No Qt runtime dependencies found in: ${ZETA_QT_RUNTIME_BIN}")
    endif()

    add_custom_command(TARGET zeta-ui-holdem POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            $<TARGET_RUNTIME_DLLS:zeta-ui-holdem>
            ${ZETA_HOLDEM_RUNTIME_DLL_PATHS}
            $<TARGET_FILE_DIR:zeta-ui-holdem>
        COMMAND ${CMAKE_COMMAND} -E make_directory
            $<TARGET_FILE_DIR:zeta-ui-holdem>/platforms
        COMMAND ${CMAKE_COMMAND} -E make_directory
            $<TARGET_FILE_DIR:zeta-ui-holdem>/iconengines
        COMMAND ${CMAKE_COMMAND} -E make_directory
            $<TARGET_FILE_DIR:zeta-ui-holdem>/imageformats
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${ZETA_QWINDOWS_PLUGIN}"
            $<TARGET_FILE_DIR:zeta-ui-holdem>/platforms
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${ZETA_QSVGICON_PLUGIN}"
            $<TARGET_FILE_DIR:zeta-ui-holdem>/iconengines
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${ZETA_QSVG_PLUGIN}"
            $<TARGET_FILE_DIR:zeta-ui-holdem>/imageformats
        COMMENT "Deploying Qt runtime dependencies for zeta-ui-holdem"
        COMMAND_EXPAND_LISTS
        VERBATIM
    )
endif()
