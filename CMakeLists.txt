cmake_minimum_required(VERSION 3.28)

project(zeta)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_CXX_COMPILER_ID STREQUAL "GNU")
    set(CMAKE_CXX_STANDARD 23)
    set(CMAKE_CXX_STANDARD_REQUIRED ON)
    set(CMAKE_CXX_EXTENSIONS OFF)
elseif(MSVC)
    set(CMAKE_CXX_STANDARD 23)
    set(CMAKE_CXX_STANDARD_REQUIRED ON) 
endif()


option(ZETA_NATIVE_OPTIMIZATIONS "Build with CPU-native instruction selection/tuning for local benchmarks" OFF)
if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
    add_compile_options("$<$<CONFIG:Release>:-O3>")
elseif(MSVC)
    add_compile_options(
        "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>>:/Oi>"
        "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>>:/Ot>"
        "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>>:/Oy>"
        "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>>:/GL>"
        "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>>:/Gw>"
        "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>>:/Gy>"
    )
    add_link_options(
        "$<$<CONFIG:Release>:/LTCG>"
        "$<$<CONFIG:Release>:/OPT:REF>"
        "$<$<CONFIG:Release>:/OPT:ICF>"
        "$<$<CONFIG:Release>:/STACK:8388608>"
    )
endif()

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    add_compile_options("$<$<COMPILE_LANGUAGE:CXX>:-mpopcnt>")
endif()

if(ZETA_NATIVE_OPTIMIZATIONS AND (CMAKE_CXX_COMPILER_ID MATCHES "Clang" OR CMAKE_CXX_COMPILER_ID STREQUAL "GNU"))
    add_compile_options(
        "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>>:-march=native>"
        "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>>:-mtune=native>"
        "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>>:-fomit-frame-pointer>"
    )
elseif(ZETA_NATIVE_OPTIMIZATIONS AND MSVC)
    add_compile_options(
        "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>>:/arch:AVX2>"
        "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:CXX>>:/favor:INTEL64>"
    )
endif()

option(ZETA_BUILD_TESTS "Build Zeta test targets" ON)
if(ZETA_BUILD_TESTS)
    enable_testing()
endif()

add_subdirectory(zeta)

function(zeta_collect_build_targets directory out_var)
    get_property(_targets DIRECTORY "${directory}" PROPERTY BUILDSYSTEM_TARGETS)
    foreach(_target IN LISTS _targets)
        if(TARGET "${_target}")
            get_target_property(_target_type "${_target}" TYPE)
            get_target_property(_target_source_dir "${_target}" SOURCE_DIR)
            cmake_path(IS_PREFIX CMAKE_SOURCE_DIR "${_target_source_dir}" NORMALIZE _target_is_source_target)
            cmake_path(IS_PREFIX CMAKE_BINARY_DIR "${_target_source_dir}" NORMALIZE _target_is_generated_target)
            if(NOT _target_type STREQUAL "UTILITY"
                AND NOT _target_type STREQUAL "INTERFACE_LIBRARY"
                AND _target_is_source_target
                AND NOT _target_is_generated_target)
                list(APPEND _build_targets "${_target}")
            endif()
        endif()
    endforeach()

    get_property(_subdirectories DIRECTORY "${directory}" PROPERTY SUBDIRECTORIES)
    foreach(_subdirectory IN LISTS _subdirectories)
        zeta_collect_build_targets("${_subdirectory}" _subdirectory_build_targets)
        list(APPEND _build_targets ${_subdirectory_build_targets})
    endforeach()

    set(${out_var} ${_build_targets} PARENT_SCOPE)
endfunction()

zeta_collect_build_targets("${CMAKE_SOURCE_DIR}" ZETA_BUILD_ALL_TARGETS)
list(REMOVE_DUPLICATES ZETA_BUILD_ALL_TARGETS)

add_custom_target(zeta-build-all
    DEPENDS ${ZETA_BUILD_ALL_TARGETS}
    COMMENT "Building all Zeta targets"
)

set(_zeta_clean_objects_script "${CMAKE_BINARY_DIR}/zeta-clean-objects.cmake")
file(GENERATE
    OUTPUT "${_zeta_clean_objects_script}"
    CONTENT [=[
file(GLOB_RECURSE _zeta_object_files
    "${ZETA_BUILD_DIR}/*.o"
    "${ZETA_BUILD_DIR}/*.obj"
)
if(_zeta_object_files)
    file(REMOVE ${_zeta_object_files})
endif()
]=]
)

add_custom_target(zeta-clean
    COMMAND ${CMAKE_COMMAND} -D "ZETA_BUILD_DIR=${CMAKE_BINARY_DIR}" -P "${_zeta_clean_objects_script}"
    COMMENT "Removing object files from the Zeta build tree"
    VERBATIM
)
