project(zeta_ui_holdem LANGUAGES CXX)

set(CMAKE_AUTOMOC ON)

add_executable(zeta-ui-holdem
    src/main.cpp
    src/main_window.h
    src/main_window.cpp
    src/solver_state.h
    src/solver_state.cpp
    src/spot_document.h
    src/spot_document.cpp
)

target_link_libraries(zeta-ui-holdem
    PRIVATE
    zeta-holdem
    Boost::json
    Qt6::Widgets
)

target_compile_features(zeta-ui-holdem PRIVATE cxx_std_23)

if(WIN32)
    set(ZETA_QT_RUNTIME_BIN "${Qt6_DIR}/../../bin")
    cmake_path(NORMAL_PATH ZETA_QT_RUNTIME_BIN)

    find_file(ZETA_QWINDOWS_PLUGIN
        NAMES qwindows.dll qwindowsd.dll
        HINTS
            "${Qt6_DIR}/../../Qt6/plugins/platforms"
            "${Qt6_DIR}/../../../Qt6/plugins/platforms"
        REQUIRED
    )

    set(ZETA_HOLDEM_RUNTIME_DLLS
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

    set(ZETA_HOLDEM_RUNTIME_DLL_PATHS "")
    foreach(_zeta_runtime_dll IN LISTS ZETA_HOLDEM_RUNTIME_DLLS)
        set(_zeta_runtime_dll_path "${ZETA_QT_RUNTIME_BIN}/${_zeta_runtime_dll}")
        if(NOT EXISTS "${_zeta_runtime_dll_path}")
            message(FATAL_ERROR "Required runtime dependency not found: ${_zeta_runtime_dll_path}")
        endif()
        list(APPEND ZETA_HOLDEM_RUNTIME_DLL_PATHS "${_zeta_runtime_dll_path}")
    endforeach()

    add_custom_command(TARGET zeta-ui-holdem POST_BUILD
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            ${ZETA_HOLDEM_RUNTIME_DLL_PATHS}
            $<TARGET_FILE_DIR:zeta-ui-holdem>
        COMMAND ${CMAKE_COMMAND} -E make_directory
            $<TARGET_FILE_DIR:zeta-ui-holdem>/platforms
        COMMAND ${CMAKE_COMMAND} -E copy_if_different
            "${ZETA_QWINDOWS_PLUGIN}"
            $<TARGET_FILE_DIR:zeta-ui-holdem>/platforms
        COMMENT "Deploying Qt runtime dependencies for zeta-ui-holdem"
        COMMAND_EXPAND_LISTS
        VERBATIM
    )
endif()
