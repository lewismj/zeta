# zeta_deploy_runtime(<target> [QT_PLATFORM <qwindows|qoffscreen>] [BOOST_ROOT <dir>])
#
# After <target> builds on Windows, copies the DLLs it needs next to it, for the
# configuration actually being built: its imported runtime DLLs, Boost's DLLs from
# BOOST_ROOT, and with QT_PLATFORM the Qt runtime plus the platform, SVG icon and SVG
# image plugins. See ZetaDeployRuntime.cmake.
set(ZETA_DEPLOY_RUNTIME_SCRIPT "${CMAKE_CURRENT_LIST_DIR}/ZetaDeployRuntime.cmake")

function(zeta_deploy_runtime target)
    if(NOT WIN32)
        return()
    endif()
    cmake_parse_arguments(ARG "" "QT_PLATFORM;BOOST_ROOT" "" ${ARGN})
    set(_qt_dir "")
    if(ARG_QT_PLATFORM)
        set(_qt_dir "${Qt6_DIR}")
    endif()
    add_custom_command(TARGET ${target} POST_BUILD
        COMMAND "${CMAKE_COMMAND}"
            "-DZETA_CONFIG=$<CONFIG>"
            "-DZETA_DEST=$<TARGET_FILE_DIR:${target}>"
            "-DZETA_QT_DIR=${_qt_dir}"
            "-DZETA_PLATFORM=${ARG_QT_PLATFORM}"
            "-DZETA_RUNTIME_DLLS=$<TARGET_RUNTIME_DLLS:${target}>"
            "-DZETA_BOOST_ROOT=${ARG_BOOST_ROOT}"
            -P "${ZETA_DEPLOY_RUNTIME_SCRIPT}"
        COMMENT "Deploying runtime DLLs for ${target} ($<CONFIG>)"
        VERBATIM
    )
endfunction()
