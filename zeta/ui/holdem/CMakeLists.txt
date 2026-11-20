project(zeta_ui_holdem LANGUAGES CXX)

set(CMAKE_AUTOMOC ON)
set(CMAKE_AUTORCC ON)

# Everything but main(): the app and the tests link the same compiled UI code.
add_library(zeta-ui-holdem-lib STATIC
    src/main_window.h
    src/main_window.cpp
    src/main_window_shell.cpp
    src/main_window_documents.cpp
    src/main_window_solver.cpp
    src/main_window_settings_dialog.cpp
    src/main_window_constants.h
    src/main_window_dialogs.h
    src/main_window_dialogs.cpp
    src/main_window_visuals.h
    src/main_window_visuals.cpp
    src/spot_summary_helpers.h
    src/spot_summary_helpers.cpp
    src/configuration_dialog.h
    src/configuration_dialog.cpp
    src/document_workspace_widget.h
    src/document_workspace_widget.cpp
    src/app/app_settings.h
    src/app/app_settings.cpp
    src/document/document_json.h
    src/document/document_json.cpp
    src/solver/solution_store.h
    src/solver/solution_store.cpp
    src/solver/solver_session.h
    src/solver/solver_session.cpp
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
    src/viewmodels/lock_view_model.h
    src/viewmodels/lock_view_model.cpp
    src/widgets/range_editor.h
    src/widgets/range_editor.cpp
    src/widgets/spot_json_editor.h
    src/widgets/spot_json_editor.cpp
    src/widgets/spot_builder.h
    src/widgets/spot_builder.cpp
    src/widgets/node_lock_dialog.h
    src/widgets/node_lock_dialog.cpp
    src/widgets/strategy_explorer.h
    src/widgets/strategy_explorer.cpp
    src/widgets/table_state_view.h
    src/widgets/table_state_view.cpp
)
target_include_directories(zeta-ui-holdem-lib PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/src)
target_link_libraries(zeta-ui-holdem-lib
    PUBLIC
    zeta-holdem
    Boost::json
    Qt6::Widgets
)
if(TARGET Qt6::Svg)
    target_link_libraries(zeta-ui-holdem-lib PUBLIC Qt6::Svg)
endif()
target_compile_features(zeta-ui-holdem-lib PUBLIC cxx_std_23)
if(WIN32)
    target_link_libraries(zeta-ui-holdem-lib PUBLIC dwmapi)
endif()
if(UNIX AND CMAKE_CXX_COMPILER_ID STREQUAL "Clang")
    target_link_libraries(zeta-ui-holdem-lib PUBLIC m)
endif()

# Resources stay with each executable: a static library's Qt resources would need
# Q_INIT_RESOURCE in every main().
add_executable(zeta-ui-holdem
    src/main.cpp
    resources/holdem.qrc
)
target_link_libraries(zeta-ui-holdem PRIVATE zeta-ui-holdem-lib)

add_custom_command(TARGET zeta-ui-holdem POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_directory
        "${CMAKE_CURRENT_SOURCE_DIR}/resources/styles"
        $<TARGET_FILE_DIR:zeta-ui-holdem>/styles
    COMMENT "Deploying editable zeta-ui-holdem styles"
    VERBATIM
)

zeta_deploy_runtime(zeta-ui-holdem QT_PLATFORM qwindows)

target_link_libraries(zeta-ui-holdem-lib PRIVATE zeta_warnings)
target_link_libraries(zeta-ui-holdem PRIVATE zeta_warnings)
