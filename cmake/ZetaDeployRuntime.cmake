# Copies the Qt (and optionally Boost) runtime next to a built executable.
#
# Run at build time with `cmake -P`, so the configuration is the one actually being
# built ($<CONFIG>), which also works for multi-config generators (Visual Studio,
# Ninja Multi-Config) where CMAKE_BUILD_TYPE is empty at configure time.
#
# Inputs (-D):
#   ZETA_CONFIG        Build configuration, e.g. Debug or Release.
#   ZETA_DEST          Directory of the executable.
#   ZETA_QT_DIR        Optional Qt6_DIR from the configure step (<prefix>/share/Qt6);
#                      empty skips Qt.
#   ZETA_PLATFORM      Qt platform plugin base name (qwindows, qoffscreen) when Qt is deployed.
#   ZETA_RUNTIME_DLLS  Optional list, $<TARGET_RUNTIME_DLLS:target>.
#   ZETA_BOOST_ROOT    Optional vcpkg-style Boost root whose [debug/]bin holds boost_*.dll.

cmake_minimum_required(VERSION 3.28)

foreach(_required IN ITEMS ZETA_CONFIG ZETA_DEST)
    if(NOT DEFINED ${_required} OR "${${_required}}" STREQUAL "")
        message(FATAL_ERROR "ZetaDeployRuntime: ${_required} is not set.")
    endif()
endforeach()

if(ZETA_CONFIG STREQUAL "Debug")
    set(_prefix "debug/")
    set(_suffixes d "")
else()
    set(_prefix "")
    set(_suffixes "" d)
endif()

set(_deploy_qt FALSE)
if(DEFINED ZETA_QT_DIR AND NOT ZETA_QT_DIR STREQUAL "")
    set(_deploy_qt TRUE)
    if(NOT DEFINED ZETA_PLATFORM OR ZETA_PLATFORM STREQUAL "")
        message(FATAL_ERROR "ZetaDeployRuntime: ZETA_PLATFORM is required with ZETA_QT_DIR.")
    endif()
endif()

set(_qt_dlls)
if(_deploy_qt)
    set(_qt_bin "${ZETA_QT_DIR}/../../${_prefix}bin")
    cmake_path(NORMAL_PATH _qt_bin)
    file(GLOB _qt_dlls "${_qt_bin}/*.dll")
    if(NOT _qt_dlls)
        message(FATAL_ERROR "ZetaDeployRuntime: no Qt runtime DLLs in ${_qt_bin}")
    endif()

    set(_plugin_roots
        "${ZETA_QT_DIR}/../../${_prefix}Qt6/plugins"
        "${ZETA_QT_DIR}/../../../${_prefix}Qt6/plugins"
    )

    # Finds <dir>/<base>[d].dll under the plugin roots, preferring this configuration's flavour.
    function(zeta_find_plugin out_var dir base)
        set(_names)
        foreach(_suffix IN LISTS _suffixes)
            list(APPEND _names "${base}${_suffix}.dll")
        endforeach()
        set(_hints)
        foreach(_root IN LISTS _plugin_roots)
            cmake_path(NORMAL_PATH _root)
            list(APPEND _hints "${_root}/${dir}")
        endforeach()
        find_file(_found NAMES ${_names} HINTS ${_hints} NO_DEFAULT_PATH NO_CACHE)
        if(NOT _found)
            message(FATAL_ERROR "ZetaDeployRuntime: ${base} plugin not found under ${_hints}")
        endif()
        set(${out_var} "${_found}" PARENT_SCOPE)
    endfunction()

    zeta_find_plugin(_platform_plugin platforms "${ZETA_PLATFORM}")
    zeta_find_plugin(_svgicon_plugin iconengines qsvgicon)
    zeta_find_plugin(_svg_plugin imageformats qsvg)
endif()

set(_dlls ${ZETA_RUNTIME_DLLS} ${_qt_dlls})
if(DEFINED ZETA_BOOST_ROOT AND NOT ZETA_BOOST_ROOT STREQUAL "")
    file(GLOB _boost_dlls "${ZETA_BOOST_ROOT}/${_prefix}bin/boost_*.dll")
    list(APPEND _dlls ${_boost_dlls})
endif()
list(REMOVE_DUPLICATES _dlls)

if(_dlls)
    execute_process(COMMAND "${CMAKE_COMMAND}" -E copy_if_different ${_dlls} "${ZETA_DEST}" COMMAND_ERROR_IS_FATAL ANY)
endif()
if(NOT _deploy_qt)
    return()
endif()
file(MAKE_DIRECTORY "${ZETA_DEST}/platforms" "${ZETA_DEST}/iconengines" "${ZETA_DEST}/imageformats")
execute_process(COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${_platform_plugin}" "${ZETA_DEST}/platforms" COMMAND_ERROR_IS_FATAL ANY)
execute_process(COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${_svgicon_plugin}" "${ZETA_DEST}/iconengines" COMMAND_ERROR_IS_FATAL ANY)
execute_process(COMMAND "${CMAKE_COMMAND}" -E copy_if_different "${_svg_plugin}" "${ZETA_DEST}/imageformats" COMMAND_ERROR_IS_FATAL ANY)
